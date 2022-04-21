cmake_minimum_required(VERSION 3.0)

file(GLOB LIB_SRC "*.cpp")

add_library(${PROJECT_NAME} ${LIB_SRC})
