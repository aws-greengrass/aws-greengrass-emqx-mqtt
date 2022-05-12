cmake_minimum_required(VERSION 3.20)

project(port_driver)
include(cmake_include/clang-format.cmake)

option(BUILD_TESTS "Build the tests." ON)

if (WIN32)
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "$<0:>${CMAKE_BINARY_DIR}/lib") # .dll
    set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>DLL")
else()
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "$<0:>${CMAKE_BINARY_DIR}/bin") # .exe
endif()
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "$<0:>${CMAKE_BINARY_DIR}/lib") # .so and .dylib
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "$<0:>${CMAKE_BINARY_DIR}/liba") # .lib and .a
set(DRIVER_GENERATED_DIR_PATH "$<0:>${CMAKE_BINARY_DIR}/lib")
set(DRIVER_OUTPUT_DIR_PATH "${CMAKE_BINARY_DIR}/driver_lib")
file(MAKE_DIRECTORY ${DRIVER_OUTPUT_DIR_PATH})

execute_process (
    COMMAND erl -noshell -eval "io:format('~s/../usr/include', [code:lib_dir()])" -s erlang halt
    OUTPUT_VARIABLE ERL_INCLUDES
)
message("Erlang include directory found: ${ERL_INCLUDES}")

execute_process(
    COMMAND erl -noshell -eval "io:format('~s/../usr/lib', [code:lib_dir()])" -s erlang halt
    OUTPUT_VARIABLE ERL_LIBS
)
message("Erlang libs directory found: ${ERL_LIBS}")

include_directories("include")
add_subdirectory(cda_integration)

if (CLANG_TIDY)
    set(CMAKE_CXX_CLANG_TIDY clang-tidy)
endif()
if (UNIX)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra -pedantic -std=gnu99 -Wc++-compat -fPIC")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -pedantic -fPIC -Wconversion -Werror -Wno-missing-field-initializers")
endif (UNIX)    

if (WIN32)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /W4")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4")
endif (WIN32)

cmake_path(APPEND CDA_INTEGRATION_INCLUDES "${CMAKE_CURRENT_SOURCE_DIR}" "cda_integration" "include")
include_directories("${CDA_INTEGRATION_INCLUDES}")
include_directories(SYSTEM ${ERL_INCLUDES})

set(PORT_DRIVER_TARGET_NAME "${PROJECT_NAME}")
link_directories("${ERL_LIBS}")
add_library(${PORT_DRIVER_TARGET_NAME} SHARED port_driver.cpp)
set_target_properties(${PORT_DRIVER_TARGET_NAME} PROPERTIES PREFIX "")
set_property(TARGET ${PORT_DRIVER_TARGET_NAME} PROPERTY CXX_STANDARD 23)

find_package(aws-crt-cpp REQUIRED)
target_link_libraries(${PORT_DRIVER_TARGET_NAME} PRIVATE AWS::aws-crt-cpp)

if (APPLE)
    # Erlang has .so hardcoded for Unix, so even on mac we need to use .so instead of .dylib
    set_target_properties(${PORT_DRIVER_TARGET_NAME} PROPERTIES SUFFIX ".so")

    # Avoid linking error on MacOS, though this likely means that the plugin won't actually work!
    # TODO: Make it build properly on MacOS
    target_link_options(${PORT_DRIVER_TARGET_NAME} PRIVATE -undefined dynamic_lookup)
endif()

if (WIN32)
    # Enable to output preprocessed file to a .i file
    # set_target_properties(${PORT_DRIVER_TARGET_NAME} PROPERTIES COMPILE_FLAGS "/P")
    set_target_properties(${PORT_DRIVER_TARGET_NAME} PROPERTIES LINK_FLAGS "/WHOLEARCHIVE:ei")
    target_link_libraries(${PORT_DRIVER_TARGET_NAME} PUBLIC cda_integration ei "ws2_32.lib")
else ()
    target_link_libraries(${PORT_DRIVER_TARGET_NAME} PUBLIC cda_integration ei)
endif ()

add_custom_command(TARGET ${PORT_DRIVER_TARGET_NAME}
    POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy ${DRIVER_GENERATED_DIR_PATH}/$<TARGET_FILE_NAME:${PORT_DRIVER_TARGET_NAME}> ${DRIVER_OUTPUT_DIR_PATH}/$<TARGET_FILE_NAME:${PORT_DRIVER_TARGET_NAME}>
)

if(BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests/unit)
endif()
