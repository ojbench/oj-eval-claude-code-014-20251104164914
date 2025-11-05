#pragma once
#ifndef PYTHON_INTERPRETER_EVALVISITOR_H
#define PYTHON_INTERPRETER_EVALVISITOR_H

#include <bits/stdc++.h>
#include "Python3ParserBaseVisitor.h"
#include "Python3Parser.h"

// Minimal BigInt implementation (base 1e9) to pass BigInteger tests
struct BigInt {
    static const int BASE = 1000000000;
    std::vector<int> d; // little-endian blocks
    bool neg = false;

    BigInt() { d = {0}; }
    BigInt(long long v) { *this = fromLL(v); }
    static BigInt fromLL(long long v) {
        BigInt x; x.d.clear(); if (v < 0) { x.neg = true; v = -v; }
        while (v) { x.d.push_back(int(v % BASE)); v /= BASE; }
        if (x.d.empty()) x.d.push_back(0), x.neg=false; return x;
    }
    static BigInt fromString(const std::string &s) {
        BigInt x; x.d.clear(); size_t i = 0; if (!s.empty() && (s[0] == '-' || s[0] == '+')) { x.neg = (s[0]=='-'); i=1; }
        std::vector<int> chunks; for (size_t j = s.size(); j > i; ) {
            size_t k = (j >= i+9 ? j - 9 : i);
            int block = 0;
            for (size_t t = k; t < j; ++t) block = block*10 + (s[t]-'0');
            chunks.push_back(block);
            j = k;
        }
        if (chunks.empty()) chunks.push_back(0), x.neg=false;
        x.d = chunks; // already little-endian
        x.trim();
        return x;
    }
    std::string toString() const {
        if (isZero()) return "0";
        std::string s = (neg?"-":"");
        int n = d.size();
        s += std::to_string(d.back());
        for (int i = n-2; i>=0; --i) {
            std::string t = std::to_string(d[i]);
            s += std::string(9 - t.size(), '0') + t;
        }
        return s;
    }
    bool isZero() const { return d.size()==1 && d[0]==0; }
    void trim() {
        while (d.size() > 1 && d.back() == 0) d.pop_back();
        if (isZero()) neg=false;
    }
    static int cmpAbs(const BigInt &a, const BigInt &b) {
        if (a.d.size() != b.d.size()) return a.d.size() < b.d.size() ? -1 : 1;
        for (int i = int(a.d.size())-1; i>=0; --i) if (a.d[i] != b.d[i]) return a.d[i] < b.d[i] ? -1 : 1;
        return 0;
    }
    friend int cmp(const BigInt &a, const BigInt &b) {
        if (a.neg != b.neg) return a.neg ? -1 : 1;
        int c = cmpAbs(a,b);
        return a.neg ? -c : c;
    }
    static BigInt addAbs(const BigInt &a, const BigInt &b) {
        BigInt r; r.neg=false; r.d.assign(std::max(a.d.size(), b.d.size()), 0);
        long long carry=0; for (size_t i=0;i<r.d.size();i++) {
            long long sum = carry;
            if (i < a.d.size()) sum += a.d[i];
            if (i < b.d.size()) sum += b.d[i];
            r.d[i] = int(sum % BASE);
            carry = sum / BASE;
        }
        if (carry) r.d.push_back(int(carry));
        return r;
    }
    static BigInt subAbs(const BigInt &a, const BigInt &b) { // assumes |a|>=|b|
        BigInt r; r.neg=false; r.d.assign(a.d.size(),0);
        long long carry=0; for (size_t i=0;i<a.d.size();++i) {
            long long diff = (long long)a.d[i] - (i<b.d.size()?b.d[i]:0) - carry;
            if (diff < 0) { diff += BASE; carry=1; } else carry=0;
            r.d[i] = int(diff);
        }
        r.trim();
        return r;
    }
    friend BigInt operator+(const BigInt &a, const BigInt &b) {
        if (a.neg == b.neg) { BigInt r = addAbs(a,b); r.neg = a.neg; r.trim(); return r; }
        int c = cmpAbs(a,b);
        if (c==0) return BigInt::fromLL(0);
        if (c>0) { BigInt r = subAbs(a,b); r.neg = a.neg; return r; }
        BigInt r = subAbs(b,a); r.neg = b.neg; return r;
    }
    friend BigInt operator-(const BigInt &a, const BigInt &b) {
        BigInt nb = b; nb.neg = !b.neg; return a + nb;
    }
    friend BigInt operator*(const BigInt &a, const BigInt &b) {
        BigInt r; r.neg = a.neg ^ b.neg; r.d.assign(a.d.size()+b.d.size(), 0);
        for (size_t i=0;i<a.d.size();++i) {
            long long carry=0; for (size_t j=0;j<b.d.size();++j) {
                long long cur = r.d[i+j] + (long long)a.d[i]*b.d[j] + carry;
                r.d[i+j] = int(cur % BASE);
                carry = cur / BASE;
            }
            size_t pos = i + b.d.size();
            while (carry) {
                long long cur = r.d[pos] + carry;
                if (pos >= r.d.size()) r.d.push_back(0);
                r.d[pos] = int(cur % BASE);
                carry = cur / BASE;
                ++pos;
            }
        }
        r.trim();
        return r;
    }
    static std::pair<BigInt, BigInt> divmodAbs(const BigInt &a, const BigInt &b) { // |b|>0
        BigInt zero = fromLL(0);
        if (cmpAbs(a,b) < 0) return {zero, a};
        int n = a.d.size(), m = b.d.size();
        int norm = BASE / (b.d.back() + 1);
        BigInt A = a * fromLL(norm);
        BigInt B = b * fromLL(norm);
        std::vector<int> q(n - m + 1, 0);
        BigInt rem; rem.d = A.d; rem.neg=false;
        auto get = [&](const std::vector<int> &v, int idx)->long long { return idx>=0 && idx<(int)v.size()? v[idx] : 0; };
        for (int i = n - 1; i >= m - 1; --i) {
            long long r2 = get(rem.d, i) * 1LL * BASE + get(rem.d, i-1);
            long long qt = r2 / B.d.back();
            if (qt >= BASE) qt = BASE-1;
            BigInt t = B * fromLL(qt);
            if (!(t.isZero())) t.d.insert(t.d.begin(), i - (m-1), 0);
            while (cmpAbs(rem, t) < 0) {
                qt -= 1;
                t = B * fromLL(qt);
                if (!(t.isZero())) t.d.insert(t.d.begin(), i - (m-1), 0);
            }
            q[i - (m-1)] = (int)qt;
            rem = subAbs(rem, t);
        }
        BigInt quot; quot.d = q; quot.neg=false; quot.trim();
        BigInt rr = rem;
        if (norm != 1) {
            long long carry = 0;
            for (int i = (int)rr.d.size()-1; i>=0; --i) {
                long long cur = rr.d[i] + carry * BASE;
                rr.d[i] = int(cur / norm);
                carry = cur % norm;
            }
            rr.trim();
        }
        return {quot, rr};
    }
    friend BigInt divFloor(const BigInt &a, const BigInt &b) { // a // b, floor division
        if (b.isZero()) return fromLL(0); // avoid crash
        bool neg = a.neg ^ b.neg;
        auto ra = a.abs(); auto rb = b.abs();
        auto [q, r] = divmodAbs(ra, rb);
        q.neg = neg; q.trim();
        if (!r.isZero() && neg) q = q - fromLL(1);
        return q;
    }
    friend BigInt modFloor(const BigInt &a, const BigInt &b) { // a % b = a - (a // b) * b
        BigInt q = divFloor(a,b);
        BigInt r = a - q * b;
        return r;
    }
    BigInt abs() const { BigInt r=*this; r.neg=false; return r; }
};

// A value variant used by visitor
struct Value {
    enum Type { T_INT, T_FLOAT, T_BOOL, T_STR, T_NONE } type = T_NONE;
    BigInt i;
    double f = 0.0;
    bool b = false;
    std::string s;
    Value() : type(T_NONE) {}
    static Value fromInt(const BigInt &x) { Value v; v.type=T_INT; v.i=x; return v; }
    static Value fromFloat(double x) { Value v; v.type=T_FLOAT; v.f=x; return v; }
    static Value fromBool(bool x) { Value v; v.type=T_BOOL; v.b=x; return v; }
    static Value fromStr(const std::string &x) { Value v; v.type=T_STR; v.s=x; return v; }
    static Value None() { return Value(); }
    std::string toString() const {
        switch (type) {
            case T_INT: return i.toString();
            case T_FLOAT: {
                std::ostringstream oss; oss.setf(std::ios::fixed); oss<<std::setprecision(6)<<f; return oss.str();
            }
            case T_BOOL: return b?"True":"False";
            case T_STR: return s;
            case T_NONE: return "None";
        }
        return "None";
    }
    bool truthy() const {
        switch (type) {
            case T_INT: return cmp(i, BigInt::fromLL(0)) != 0;
            case T_FLOAT: return f != 0.0;
            case T_BOOL: return b;
            case T_STR: return !s.empty();
            case T_NONE: return false;
        }
        return false;
    }
};

class EvalVisitor : public Python3ParserBaseVisitor {
public:
    // environment: variable name -> Value
    std::unordered_map<std::string, Value> env;

    // Program
    std::any visitFile_input(Python3Parser::File_inputContext *ctx) override;

    // Statements
    std::any visitStmt(Python3Parser::StmtContext *ctx) override;
    std::any visitSimple_stmt(Python3Parser::Simple_stmtContext *ctx) override;
    std::any visitSmall_stmt(Python3Parser::Small_stmtContext *ctx) override;
    std::any visitExpr_stmt(Python3Parser::Expr_stmtContext *ctx) override;
    std::any visitCompound_stmt(Python3Parser::Compound_stmtContext *ctx) override;
    std::any visitIf_stmt(Python3Parser::If_stmtContext *ctx) override;
    std::any visitWhile_stmt(Python3Parser::While_stmtContext *ctx) override;
    std::any visitSuite(Python3Parser::SuiteContext *ctx) override;

    // Expressions
    std::any visitTest(Python3Parser::TestContext *ctx) override;
    std::any visitOr_test(Python3Parser::Or_testContext *ctx) override;
    std::any visitAnd_test(Python3Parser::And_testContext *ctx) override;
    std::any visitNot_test(Python3Parser::Not_testContext *ctx) override;
    std::any visitComparison(Python3Parser::ComparisonContext *ctx) override;
    std::any visitArith_expr(Python3Parser::Arith_exprContext *ctx) override;
    std::any visitAddorsub_op(Python3Parser::Addorsub_opContext *ctx) override;
    std::any visitTerm(Python3Parser::TermContext *ctx) override;
    std::any visitMuldivmod_op(Python3Parser::Muldivmod_opContext *ctx) override;
    std::any visitFactor(Python3Parser::FactorContext *ctx) override;
    std::any visitAtom_expr(Python3Parser::Atom_exprContext *ctx) override;
    std::any visitTrailer(Python3Parser::TrailerContext *ctx) override;
    std::any visitAtom(Python3Parser::AtomContext *ctx) override;
    std::any visitFormat_string(Python3Parser::Format_stringContext *ctx) override;
    std::any visitTestlist(Python3Parser::TestlistContext *ctx) override;
    std::any visitArglist(Python3Parser::ArglistContext *ctx) override;

private:
    // helpers
    static Value VAdd(const Value &a, const Value &b);
    static Value VSub(const Value &a, const Value &b);
    static Value VMul(const Value &a, const Value &b);
    static Value VDivInt(const Value &a, const Value &b);
    static Value VDivFloat(const Value &a, const Value &b);
    static Value VMod(const Value &a, const Value &b);
};

#endif//PYTHON_INTERPRETER_EVALVISITOR_H
