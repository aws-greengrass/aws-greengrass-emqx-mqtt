# clang-format off
# Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
# SPDX-License-Identifier: Apache-2.0
# clang-format on
cmake_minimum_required(VERSION 3.5)

file(GLOB_RECURSE LIB_SRC "*.cpp")

if (CLANG_TIDY)
    set(CMAKE_CXX_CLANG_TIDY clang-tidy)
endif()
add_library(${PROJECT_NAME} ${LIB_SRC})
