cmake_minimum_required(VERSION 3.15)

include_directories(/usr/include/antlr4-runtime/)
include_directories(${PROJECT_SOURCE_DIR}/third_party/antlr4-runtime/include/antlr4-runtime)

file(GLOB PyAntlrSrc *.cpp)

add_library(PyAntlr STATIC ${PyAntlrSrc})
