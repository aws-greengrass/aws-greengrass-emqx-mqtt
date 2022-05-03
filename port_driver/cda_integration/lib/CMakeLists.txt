cmake_minimum_required(VERSION 3.20)

file(GLOB LIB_SRC "*.cpp")

add_library(${PROJECT_NAME} ${LIB_SRC})
