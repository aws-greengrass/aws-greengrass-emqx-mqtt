cmake_minimum_required(VERSION 3.20)
project(port_driver_unit_tests)
add_definitions(-DUNIT_TESTS)

if (${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_BINARY_DIR})
    message(FATAL_ERROR "In-source builds not allowed. Please make a new directory (called a build directory) and run CMake from there. You may need to remove CMakeCache.txt and CMakeFiles folder.")
endif ()

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/archive)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# Configure Compiler flags
if (WIN32)
    set(CUSTOM_COMPILER_FLAGS "/W4")
else
    set(CUSTOM_COMPILER_FLAGS "-fno-exceptions -Wall -Werror")
endif ()

# Download and unpack googletest at configure time
configure_file(${CMAKE_CURRENT_LIST_DIR}/CMakeLists-gtest.txt.in
        ${CMAKE_BINARY_DIR}/third_party/googletest/download/CMakeLists.txt)

execute_process(COMMAND ${CMAKE_COMMAND} -G "${CMAKE_GENERATOR}" .
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/third_party/googletest/download)

execute_process(COMMAND ${CMAKE_COMMAND} --build .
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/third_party/googletest/download)

# Prevent GoogleTest from overriding compiler/linker options
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)

# This adds the following targets: gtest, gtest_main, gmock and gmock_main
add_subdirectory(${CMAKE_BINARY_DIR}/third_party/googletest/src
        ${CMAKE_BINARY_DIR}/third_party/googletest/build EXCLUDE_FROM_ALL)

# Add testing target
enable_testing()
set(UNIT_TEST_TARGET_NAME port_driver_unit_tests)

add_executable(${UNIT_TEST_TARGET_NAME} hello_test.cc)
target_link_libraries(${UNIT_TEST_TARGET_NAME} gtest gtest_main gmock gmock_main)

#include(GoogleTest)
#gtest_discover_tests(hello_test)

set_property(TARGET ${UNIT_TEST_TARGET_NAME} APPEND_STRING PROPERTY COMPILE_FLAGS ${CUSTOM_COMPILER_FLAGS})

# Enable 'make test'
add_test(NAME Run-All-Tests COMMAND ${UNIT_TEST_TARGET_NAME})