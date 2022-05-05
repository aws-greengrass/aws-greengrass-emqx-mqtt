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
else ()
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

file(GLOB_RECURSE UNIT_TEST_SOURCES FOLLOW_SYMLINKS ${CMAKE_SOURCE_DIR}/tests/unit/src/*.cpp)
add_executable(${UNIT_TEST_TARGET_NAME} ${UNIT_TEST_SOURCES})
target_link_libraries(${UNIT_TEST_TARGET_NAME} gtest gtest_main gmock gmock_main)
target_link_libraries(${UNIT_TEST_TARGET_NAME} ${PORT_DRIVER_TARGET_NAME})

find_program(GCOV gcov)
find_program(LCOV lcov)
find_program(GENHTML genhtml)

if (GCOV AND LCOV AND GENHTML)
    set(UNIT_TEST_COVERAGE_TARGET_NAME "${UNIT_TEST_TARGET_NAME}-coverage")
    target_link_libraries(${UNIT_TEST_TARGET_NAME} -fprofile-arcs)
    target_link_libraries(${UNIT_TEST_TARGET_NAME} -ftest-coverage)

    set(UNIT_TEST_OUTPUT_DIR_PATH ${CMAKE_BINARY_DIR}/unit_test_results)
    set(UNIT_TEST_OUTPUT_NAME unit_test)

    file(MAKE_DIRECTORY ${UNIT_TEST_OUTPUT_DIR_PATH})

    set(CUSTOM_COMPILER_FLAGS "${CUSTOM_COMPILER_FLAGS} -g -O0 -fprofile-arcs -ftest-coverage")

    set(LCOV_INFO "${UNIT_TEST_OUTPUT_DIR_PATH}/${UNIT_TEST_OUTPUT_NAME}.info")
    set(LCOV_CLEAN "${LCOV_INFO}.clean")

    # Setup target
    add_custom_target(${UNIT_TEST_COVERAGE_TARGET_NAME}
            DEPENDS ${UNIT_TEST_TARGET_NAME}
            WORKING_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
            # Cleanup old lcov runs
            COMMAND ${LCOV} --directory ${UNIT_TEST_OUTPUT_DIR_PATH} --zerocounters --rc lcov_branch_coverage=1

            # Run tests
            COMMAND ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${UNIT_TEST_TARGET_NAME}

            # Capturing lcov counters and generating report
            COMMAND ${LCOV} --rc lcov_branch_coverage=1 --directory ${CMAKE_BINARY_DIR} --capture --output-file ${LCOV_INFO}
            COMMAND ${LCOV} --rc lcov_branch_coverage=1 --remove ${LCOV_INFO} 'tests/*' 'build/*' '/usr/*' '*logging/*' --output-file ${LCOV_CLEAN}
            COMMAND ${GENHTML} -o ${UNIT_TEST_OUTPUT_NAME} ${LCOV_CLEAN}
            COMMAND ${CMAKE_COMMAND} -E remove ${LCOV_INFO} ${LCOV_CLEAN}
            COMMENT "Open ${CMAKE_BINARY_DIR}/${UNIT_TEST_OUTPUT_NAME}/index.html in your browser to view the coverage report."
            )
endif ()

set_property(TARGET ${UNIT_TEST_TARGET_NAME} APPEND_STRING PROPERTY COMPILE_FLAGS ${CUSTOM_COMPILER_FLAGS})

# Enable 'make test'
add_test(NAME Run-All-Tests COMMAND ${UNIT_TEST_TARGET_NAME})
