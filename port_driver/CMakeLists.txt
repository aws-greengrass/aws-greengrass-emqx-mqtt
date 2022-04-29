cmake_minimum_required(VERSION 3.20)

project(port_driver)

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/liba")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")

add_subdirectory(cda_integration)

if (APPLE)
    # Avoid linking error on MacOS, though this likely means that the plugin won't actually work!
    # TODO: Make it build properly on MacOS
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra -pedantic -std=gnu99 -Wc++-compat -I$(ERL_INCLUDE) -fPIC")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -pedantic -fPIC -undefined dynamic_lookup")
endif (APPLE)

if (UNIX)
    execute_process (
        COMMAND erl -noshell -eval "io:format(\"~s/../usr/include\", [code:lib_dir()])" -s erlang halt
        OUTPUT_VARIABLE ERL_INCLUDES
    )
    message("Erlang include directory found: ${ERL_INCLUDES}")

    execute_process(
        COMMAND erl -noshell -eval "io:format(\"~s/../usr/lib\", [code:lib_dir()])" -s erlang halt
        OUTPUT_VARIABLE ERL_LIBS
    )
    message("Erlang libs directory found: ${ERL_LIBS}")
    

elseif (WIN32)
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
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /W4 /I$(ERL_INCLUDES)")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /W4 /I$(ERL_INCLUDES)")
    set(WIN32_SOCK_LIB "ws2_32.lib")
endif ()

cmake_path(APPEND CDA_INTEGRATION_INCLUDES "${CMAKE_CURRENT_SOURCE_DIR}" "cda_integration" "include")
include_directories("${CDA_INTEGRATION_INCLUDES}" "${ERL_INCLUDES}")

link_directories("${ERL_LIBS}")
add_library(${PROJECT_NAME} SHARED port_driver.c)
set_target_properties(${PROJECT_NAME} PROPERTIES PREFIX "")

if (WIN32)
    set_target_properties(${PROJECT_NAME} PROPERTIES LINK_FLAGS "/WHOLEARCHIVE:ei")
endif ()

target_link_libraries(${PROJECT_NAME} PUBLIC cda_integration ei "${WIN32_SOCK_LIB}")
