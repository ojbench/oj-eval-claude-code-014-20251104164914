#include "Evalvisitor.h"
#include <iostream>

static Value toInt(const Value &v) {
    if (v.type == Value::T_INT) return v;
    return Value::None();
}

std::any EvalVisitor::visitFile_input(Python3Parser::File_inputContext *ctx) {
    // iterate statements
    for (auto s : ctx->stmt()) visit(s);
    return nullptr;
}

std::any EvalVisitor::visitStmt(Python3Parser::StmtContext *ctx) {
    if (ctx->simple_stmt()) return visit(ctx->simple_stmt());
    if (ctx->compound_stmt()) return visit(ctx->compound_stmt());
    return nullptr;
}

std::any EvalVisitor::visitSimple_stmt(Python3Parser::Simple_stmtContext *ctx) {
    return visit(ctx->small_stmt());
}

std::any EvalVisitor::visitSmall_stmt(Python3Parser::Small_stmtContext *ctx) {
    if (ctx->expr_stmt()) return visit(ctx->expr_stmt());
    // ignore flow statements in minimal implementation
    return nullptr;
}

std::any EvalVisitor::visitExpr_stmt(Python3Parser::Expr_stmtContext *ctx) {
    // assignment or expression
    auto lists = ctx->testlist();
    if (ctx->augassign()) {
        // Augmented assignment: NAME op expr
        Python3Parser::TestContext *t = lists[0]->test(0);
        auto ot = t->or_test();
        auto at = ot->and_test(0);
        auto nt = at->not_test(0);
        auto cmp = nt->comparison();
        auto ae = cmp->arith_expr(0);
        auto te = ae->term(0);
        auto fa = te->factor(0);
        auto ae2 = fa->atom_expr();
        auto atom = ae2->atom();
        std::string name = atom->NAME()->getSymbol()->getText();
        Value lhs = env.count(name) ? env[name] : Value::fromInt(BigInt::fromLL(0));
        Value rhs = std::any_cast<Value>(visit(lists.back()));
        auto op = ctx->augassign();
        Value res = lhs;
        if (op->ADD_ASSIGN()) res = VAdd(lhs, rhs);
        else if (op->SUB_ASSIGN()) res = VSub(lhs, rhs);
        else if (op->MULT_ASSIGN()) res = VMul(lhs, rhs);
        else if (op->DIV_ASSIGN()) res = VDivFloat(lhs, rhs);
        else if (op->IDIV_ASSIGN()) res = VDivInt(lhs, rhs);
        else if (op->MOD_ASSIGN()) res = VMod(lhs, rhs);
        env[name] = res;
        return nullptr;
    }
    if (!ctx->ASSIGN().empty()) {
        // Simple assignment: NAME = expr
        auto lhs_list = lists[0];
        auto rhs_list = lists.back();
        Value vals = std::any_cast<Value>(visit(rhs_list));
        Python3Parser::TestContext *t = lhs_list->test(0);
        auto ot = t->or_test();
        auto at = ot->and_test(0);
        auto nt = at->not_test(0);
        auto cmp = nt->comparison();
        auto ae = cmp->arith_expr(0);
        auto te = ae->term(0);
        auto fa = te->factor(0);
        auto ae2 = fa->atom_expr();
        auto atom = ae2->atom();
        std::string name = atom->NAME()->getSymbol()->getText();
        env[name] = vals;
        return nullptr;
    } else {
        // expression statement: evaluate and if it's a call to print, the printing happens in visitAtom_expr
        visit(lists[0]);
        return nullptr;
    }
}

std::any EvalVisitor::visitCompound_stmt(Python3Parser::Compound_stmtContext *ctx) {
    if (ctx->if_stmt()) return visit(ctx->if_stmt());
    if (ctx->while_stmt()) return visit(ctx->while_stmt());
    if (ctx->funcdef()) return nullptr; // functions not implemented in minimal subset
    return nullptr;
}
std::any EvalVisitor::visitIf_stmt(Python3Parser::If_stmtContext *ctx) {
    // if test: suite (elif test: suite)* (else: suite)?
    for (size_t i=0;i<ctx->test().size();++i) {
        Value v = std::any_cast<Value>(visit(ctx->test(i)));
        if (v.truthy()) { visit(ctx->suite(i)); return nullptr; }
    }
    if (ctx->ELSE()) {
        visit(ctx->suite(ctx->suite().size()-1));
    }
    return nullptr;
}
std::any EvalVisitor::visitWhile_stmt(Python3Parser::While_stmtContext *ctx) {
    // while test: suite
    int guard = 0; // avoid infinite loop in malformed input
    while (true) {
        Value v = std::any_cast<Value>(visit(ctx->test()));
        if (!v.truthy()) break;
        visit(ctx->suite());
        if (++guard > 1000000) break;
    }
    return nullptr;
}
std::any EvalVisitor::visitSuite(Python3Parser::SuiteContext *ctx) {
    if (ctx->simple_stmt()) return visit(ctx->simple_stmt());
    for (auto s : ctx->stmt()) visit(s);
    return nullptr;
}

std::any EvalVisitor::visitTest(Python3Parser::TestContext *ctx) { return visit(ctx->or_test()); }
std::any EvalVisitor::visitOr_test(Python3Parser::Or_testContext *ctx) {
    // Evaluate left-to-right with short-circuit, return boolean Value
    Value cur = std::any_cast<Value>(visit(ctx->and_test(0)));
    bool res = cur.truthy();
    for (size_t i=1; i<ctx->and_test().size(); ++i) {
        if (res) break;
        Value v = std::any_cast<Value>(visit(ctx->and_test(i)));
        res = res || v.truthy();
    }
    return Value::fromBool(res);
}
std::any EvalVisitor::visitAnd_test(Python3Parser::And_testContext *ctx) {
    bool res = true;
    for (size_t i=0; i<ctx->not_test().size(); ++i) {
        if (!res) break;
        Value v = std::any_cast<Value>(visit(ctx->not_test(i)));
        res = res && v.truthy();
    }
    return Value::fromBool(res);
}
std::any EvalVisitor::visitNot_test(Python3Parser::Not_testContext *ctx) {
    if (ctx->comparison()) return visit(ctx->comparison());
    // NOT not_test
    Value v = std::any_cast<Value>(visit(ctx->not_test()));
    return Value::fromBool(!v.truthy());
}

static int strCompare(const std::string &a, const std::string &b){ if (a<b) return -1; if (a>b) return 1; return 0; }

std::any EvalVisitor::visitComparison(Python3Parser::ComparisonContext *ctx) {
    // Handle chained comparisons: a op1 b op2 c ... -> all must be true
    size_t n = ctx->arith_expr().size();
    std::vector<Value> vals; vals.reserve(n);
    for (size_t i=0;i<n;++i) vals.push_back(std::any_cast<Value>(visit(ctx->arith_expr(i))));
    auto cmp_op = [&](size_t i)->bool{
        auto op = ctx->comp_op(i);
        const Value &A = vals[i], &B = vals[i+1];
        if (op->EQUALS() || op->NOT_EQ_2()) {
            if (A.type == Value::T_STR || B.type == Value::T_STR) {
                if (A.type != Value::T_STR || B.type != Value::T_STR) return op->NOT_EQ_2() != nullptr; // false for ==, true for !=
                bool eq = (A.s == B.s); return op->EQUALS() ? eq : !eq;
            }
            if (A.type == Value::T_NONE || B.type == Value::T_NONE) {
                bool eq = (A.type == Value::T_NONE && B.type == Value::T_NONE);
                return op->EQUALS() ? eq : !eq;
            }
            if (A.type == Value::T_FLOAT || B.type == Value::T_FLOAT) {
                double a = (A.type == Value::T_FLOAT) ? A.f : (A.type == Value::T_INT ? std::stod(A.i.toString()) : (A.type==Value::T_BOOL ? (A.b?1.0:0.0) : 0.0));
                double b = (B.type == Value::T_FLOAT) ? B.f : (B.type == Value::T_INT ? std::stod(B.i.toString()) : (B.type==Value::T_BOOL ? (B.b?1.0:0.0) : 0.0));
                bool eq = (a == b); return op->EQUALS() ? eq : !eq;
            } else {
                BigInt ai = (A.type == Value::T_INT) ? A.i : BigInt::fromLL(A.type==Value::T_BOOL && A.b ? 1 : 0);
                BigInt bi = (B.type == Value::T_INT) ? B.i : BigInt::fromLL(B.type==Value::T_BOOL && B.b ? 1 : 0);
                bool eq = (cmp(ai, bi) == 0); return op->EQUALS() ? eq : !eq;
            }
        } else {
            if (A.type == Value::T_STR || B.type == Value::T_STR) {
                if (A.type != Value::T_STR || B.type != Value::T_STR) return false;
                int c = strCompare(A.s, B.s);
                if (op->LESS_THAN()) return c < 0;
                if (op->GREATER_THAN()) return c > 0;
                if (op->LT_EQ()) return c <= 0;
                if (op->GT_EQ()) return c >= 0;
                return false;
            }
            if (A.type == Value::T_FLOAT || B.type == Value::T_FLOAT) {
                double a = (A.type == Value::T_FLOAT) ? A.f : (A.type == Value::T_INT ? std::stod(A.i.toString()) : (A.type==Value::T_BOOL ? (A.b?1.0:0.0) : 0.0));
                double b = (B.type == Value::T_FLOAT) ? B.f : (B.type == Value::T_INT ? std::stod(B.i.toString()) : (B.type==Value::T_BOOL ? (B.b?1.0:0.0) : 0.0));
                if (op->LESS_THAN()) return a < b;
                if (op->GREATER_THAN()) return a > b;
                if (op->LT_EQ()) return a <= b;
                if (op->GT_EQ()) return a >= b;
                return false;
            } else {
                BigInt ai = (A.type == Value::T_INT) ? A.i : BigInt::fromLL(A.type==Value::T_BOOL && A.b ? 1 : 0);
                BigInt bi = (B.type == Value::T_INT) ? B.i : BigInt::fromLL(B.type==Value::T_BOOL && B.b ? 1 : 0);
                int c = cmp(ai, bi);
                if (op->LESS_THAN()) return c < 0;
                if (op->GREATER_THAN()) return c > 0;
                if (op->LT_EQ()) return c <= 0;
                if (op->GT_EQ()) return c >= 0;
                return false;
            }
        }
    };
    bool all = true;
    for (size_t i=0; i+1<n && i<ctx->comp_op().size(); ++i) {
        if (!cmp_op(i)) { all=false; break; }
    }
    return Value::fromBool(all);
}

std::any EvalVisitor::visitArith_expr(Python3Parser::Arith_exprContext *ctx) {
    Value cur = std::any_cast<Value>(visit(ctx->term(0)));
    for (size_t i = 1; i < ctx->term().size(); ++i) {
        auto op = ctx->addorsub_op(i-1);
        Value rhs = std::any_cast<Value>(visit(ctx->term(i)));
        if (op->ADD()) cur = VAdd(cur, rhs);
        else cur = VSub(cur, rhs);
    }
    return cur;
}

std::any EvalVisitor::visitAddorsub_op(Python3Parser::Addorsub_opContext *ctx) { return nullptr; }

std::any EvalVisitor::visitTerm(Python3Parser::TermContext *ctx) {
    Value cur = std::any_cast<Value>(visit(ctx->factor(0)));
    for (size_t i = 1; i < ctx->factor().size(); ++i) {
        auto op = ctx->muldivmod_op(i-1);
        Value rhs = std::any_cast<Value>(visit(ctx->factor(i)));
        if (op->STAR()) cur = VMul(cur, rhs);
        else if (op->IDIV()) cur = VDivInt(cur, rhs);
        else if (op->MOD()) cur = VMod(cur, rhs);
        else /* DIV */ cur = VDivFloat(cur, rhs);
    }
    return cur;
}

std::any EvalVisitor::visitMuldivmod_op(Python3Parser::Muldivmod_opContext *ctx) { return nullptr; }

std::any EvalVisitor::visitFactor(Python3Parser::FactorContext *ctx) {
    if (ctx->atom_expr()) {
        return visit(ctx->atom_expr());
    } else {
        // unary + or -
        Value v = std::any_cast<Value>(visit(ctx->factor()));
        if (ctx->MINUS()) {
            if (v.type == Value::T_INT) { BigInt t = v.i; t.neg = !t.neg; return Value::fromInt(t); }
            if (v.type == Value::T_FLOAT) return Value::fromFloat(-v.f);
        }
        return v;
    }
}

std::any EvalVisitor::visitAtom_expr(Python3Parser::Atom_exprContext *ctx) {
    // If atom is a function name then base is None sentinel
    Value base = std::any_cast<Value>(visit(ctx->atom()));
    if (ctx->trailer()) {
        auto tr = ctx->trailer();
        // retrieve function name
        auto atom = ctx->atom();
        std::string fname;
        if (atom->NAME()) fname = atom->NAME()->getSymbol()->getText();
        if (fname == "print") {
            std::vector<Value> args;
            if (tr->arglist()) {
                auto al = tr->arglist();
                for (auto arg : al->argument()) {
                    args.push_back(std::any_cast<Value>(visit(arg->test(0))));
                }
            }
            for (size_t i=0;i<args.size();++i) {
                if (i) std::cout << ' ';
                std::cout << args[i].toString();
            }
            std::cout << '\n';
            return Value::None();
        } else if (fname == "int") {
            Value a = Value::None();
            if (tr->arglist()) a = std::any_cast<Value>(visit(tr->arglist()->argument(0)->test(0)));
            if (a.type == Value::T_INT) return a;
            if (a.type == Value::T_BOOL) return Value::fromInt(a.b?BigInt::fromLL(1):BigInt::fromLL(0));
            if (a.type == Value::T_FLOAT) return Value::fromInt(BigInt::fromLL((long long)a.f));
            if (a.type == Value::T_STR) return Value::fromInt(BigInt::fromString(a.s));
            return Value::fromInt(BigInt::fromLL(0));
        } else if (fname == "float") {
            Value a = Value::None();
            if (tr->arglist()) a = std::any_cast<Value>(visit(tr->arglist()->argument(0)->test(0)));
            if (a.type == Value::T_FLOAT) return a;
            if (a.type == Value::T_INT) return Value::fromFloat(0.0 + std::stod(a.i.toString()));
            if (a.type == Value::T_BOOL) return Value::fromFloat(a.b?1.0:0.0);
            if (a.type == Value::T_STR) return Value::fromFloat(std::stod(a.s));
            return Value::fromFloat(0.0);
        } else if (fname == "str") {
            Value a = Value::None();
            if (tr->arglist()) a = std::any_cast<Value>(visit(tr->arglist()->argument(0)->test(0)));
            if (a.type == Value::T_STR) return a;
            if (a.type == Value::T_INT) return Value::fromStr(a.i.toString());
            if (a.type == Value::T_FLOAT) return Value::fromStr(Value::fromFloat(a.f).toString());
            if (a.type == Value::T_BOOL) return Value::fromStr(a.b?"True":"False");
            return Value::fromStr("None");
        } else if (fname == "bool") {
            Value a = Value::None();
            if (tr->arglist()) a = std::any_cast<Value>(visit(tr->arglist()->argument(0)->test(0)));
            return Value::fromBool(a.truthy());
        }
    }
    return base;
}

std::any EvalVisitor::visitTrailer(Python3Parser::TrailerContext *ctx) { return nullptr; }

std::any EvalVisitor::visitAtom(Python3Parser::AtomContext *ctx) {
    if (ctx->NUMBER()) {
        std::string s = ctx->NUMBER()->getSymbol()->getText();
        if (s.find('.') != std::string::npos) {
            return Value::fromFloat(std::stod(s));
        }
        return Value::fromInt(BigInt::fromString(s));
    } else if (ctx->NONE()) {
        return Value::None();
    } else if (ctx->TRUE()) {
        return Value::fromBool(true);
    } else if (ctx->FALSE()) {
        return Value::fromBool(false);
    } else if (ctx->OPEN_PAREN()) {
        return visit(ctx->test());
    } else if (!ctx->STRING().empty()) {
        // concatenate multiple adjacent strings
        std::string res;
        for (auto s : ctx->STRING()) res += s->getSymbol()->getText().substr(1, s->getSymbol()->getText().size()-2);
        return Value::fromStr(res);
    } else if (ctx->format_string()) {
        return visit(ctx->format_string());
    } else if (ctx->NAME()) {
        std::string name = ctx->NAME()->getSymbol()->getText();
        auto it = env.find(name);
        if (it != env.end()) return it->second;
        return Value::None();
    }
    return Value::None();
}

std::any EvalVisitor::visitTestlist(Python3Parser::TestlistContext *ctx) {
    return visit(ctx->test(0));
}

std::any EvalVisitor::visitArglist(Python3Parser::ArglistContext *ctx) { return nullptr; }

std::any EvalVisitor::visitFormat_string(Python3Parser::Format_stringContext *ctx) {
    // Build string by interleaving literals and evaluated expressions inside braces
    std::string out;
    size_t litN = ctx->FORMAT_STRING_LITERAL().size();
    size_t exprN = ctx->testlist().size();
    size_t i = 0;
    for (; i < litN || i < exprN; ++i) {
        if (i < litN) out += ctx->FORMAT_STRING_LITERAL(i)->getSymbol()->getText();
        if (i < exprN) {
            Value v = std::any_cast<Value>(visit(ctx->testlist(i)));
            out += v.toString();
        }
    }
    return Value::fromStr(out);
}

// helpers
Value EvalVisitor::VAdd(const Value &a, const Value &b) {
    if (a.type == Value::T_INT && b.type == Value::T_INT) return Value::fromInt(a.i + b.i);
    if (a.type == Value::T_FLOAT && b.type == Value::T_FLOAT) return Value::fromFloat(a.f + b.f);
    if (a.type == Value::T_INT && b.type == Value::T_FLOAT) return Value::fromFloat(std::stod(a.i.toString()) + b.f);
    if (a.type == Value::T_FLOAT && b.type == Value::T_INT) return Value::fromFloat(a.f + std::stod(b.i.toString()));
    if (a.type == Value::T_STR && b.type == Value::T_STR) return Value::fromStr(a.s + b.s);
    return Value::None();
}
Value EvalVisitor::VSub(const Value &a, const Value &b) {
    if (a.type == Value::T_INT && b.type == Value::T_INT) return Value::fromInt(a.i - b.i);
    if (a.type == Value::T_FLOAT && b.type == Value::T_FLOAT) return Value::fromFloat(a.f - b.f);
    if (a.type == Value::T_INT && b.type == Value::T_FLOAT) return Value::fromFloat(std::stod(a.i.toString()) - b.f);
    if (a.type == Value::T_FLOAT && b.type == Value::T_INT) return Value::fromFloat(a.f - std::stod(b.i.toString()));
    return Value::None();
}
Value EvalVisitor::VMul(const Value &a, const Value &b) {
    if (a.type == Value::T_INT && b.type == Value::T_INT) return Value::fromInt(a.i * b.i);
    if (a.type == Value::T_FLOAT && b.type == Value::T_FLOAT) return Value::fromFloat(a.f * b.f);
    if (a.type == Value::T_INT && b.type == Value::T_FLOAT) return Value::fromFloat(std::stod(a.i.toString()) * b.f);
    if (a.type == Value::T_FLOAT && b.type == Value::T_INT) return Value::fromFloat(a.f * std::stod(b.i.toString()));
    if (a.type == Value::T_STR && b.type == Value::T_INT) {
        long long times = std::stoll(b.i.toString()); if (times < 0) times = 0;
        std::string out; out.reserve(times * a.s.size());
        for (long long i=0;i<times;++i) out += a.s;
        return Value::fromStr(out);
    }
    return Value::None();
}
Value EvalVisitor::VDivInt(const Value &a, const Value &b) {
    if (a.type == Value::T_INT && b.type == Value::T_INT) return Value::fromInt(divFloor(a.i, b.i));
    return Value::None();
}
Value EvalVisitor::VDivFloat(const Value &a, const Value &b) {
    double x=0.0,y=0.0; bool ok=true;
    if (a.type == Value::T_FLOAT) x=a.f; else if (a.type == Value::T_INT) x = std::stod(a.i.toString()); else ok=false;
    if (b.type == Value::T_FLOAT) y=b.f; else if (b.type == Value::T_INT) y = std::stod(b.i.toString()); else ok=false;
    if (!ok) return Value::None();
    return Value::fromFloat(x / y);
}
Value EvalVisitor::VMod(const Value &a, const Value &b) {
    if (a.type == Value::T_INT && b.type == Value::T_INT) return Value::fromInt(modFloor(a.i, b.i));
    return Value::None();
}
