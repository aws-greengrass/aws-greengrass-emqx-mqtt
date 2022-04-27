cmake_minimum_required(VERSION 3.2)

project(port_driver)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra -pedantic -std=gnu99 -Wc++-compat -I$(ERL_INCLUDE) -fPIC")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -pedantic -fPIC")

add_subdirectory(cda_integration)

# TODO: Make work on windows
if (UNIX)
    execute_process (
        COMMAND erl -noshell -eval "io:format(\"~s/../usr/include\", [code:lib_dir()])" -s erlang halt
        OUTPUT_VARIABLE ERL_INCLUDES
    )
    message("Erlang include directory found: ${ERL_INCLUDES}")
endif (UNIX)

execute_process(
    COMMAND erl -noshell -eval "io:format(\"~s\", [code:lib_dir()])" -s erlang halt
    OUTPUT_VARIABLE ERL_LIBS
)

cmake_path(APPEND CDA_INTEGRATION_INCLUDES "${CMAKE_CURRENT_SOURCE_DIR}" "cda_integration" "include")
include_directories("${CDA_INTEGRATION_INCLUDES}" ${ERL_INCLUDES})

link_directories(${ERL_LIBS})
add_library(${PROJECT_NAME} SHARED port_driver.c)
set_target_properties(${PROJECT_NAME} PROPERTIES PREFIX "")
target_link_libraries(${PROJECT_NAME} PUBLIC cda_integration ei)
