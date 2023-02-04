cmake_minimum_required(VERSION 3.20)

file(GLOB_RECURSE LIB_SRC "*.cpp")

if (CLANG_TIDY)
    set(CMAKE_CXX_CLANG_TIDY "clang-tidy;-extra-arg=-std=c++2b")
endif()
add_library(${PROJECT_NAME} ${LIB_SRC})
