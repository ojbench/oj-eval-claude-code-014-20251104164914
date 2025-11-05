### YOU CAN'T MODIFY THE CODE BELOW
cmake_minimum_required(VERSION 3.15.2)
project(python_interpreter)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Ofast")

include_directories(/usr/include/antlr4-runtime/)
include_directories(
	${PROJECT_SOURCE_DIR}/generated/
	${PROJECT_SOURCE_DIR}/src/
)

add_subdirectory(${PROJECT_SOURCE_DIR}/generated)
### YOU CAN"T MODIFY THE CODE ABOVE

file(GLOB_RECURSE main_src src/*.cpp)

add_executable(code ${main_src}) # Add all *.cpp file after src/main.cpp, like src/Evalvisitor.cpp did

# Local vendored antlr4 runtime (headers and lib) to avoid apt
include_directories(${PROJECT_SOURCE_DIR}/third_party/antlr4-runtime/include/antlr4-runtime)
# Provide imported target so the existing link line works
add_library(antlr4-runtime STATIC IMPORTED GLOBAL)
set_target_properties(antlr4-runtime PROPERTIES
  IMPORTED_LOCATION "${PROJECT_SOURCE_DIR}/third_party/antlr4-runtime/lib/libantlr4-runtime.a"
  INTERFACE_INCLUDE_DIRECTORIES "${PROJECT_SOURCE_DIR}/third_party/antlr4-runtime/include/antlr4-runtime"
)

### YOU CAN'T MODIFY THE CODE BELOW
target_link_libraries(code PyAntlr)
target_link_libraries(code antlr4-runtime)
### YOU CAN"T MODIFY THE CODE ABOVE
