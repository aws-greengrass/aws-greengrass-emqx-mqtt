cmake_minimum_required(VERSION 3.0)

# Uncomment if you need erl directories for Mac
#execute_process (
#    COMMAND erl -noshell -eval 'io:format("~n~s/../usr/include~n", [[code:lib_dir()]]).' -s erlang halt | tail -n 1
#    OUTPUT_VARIABLE ERL_INCLUDE
#)

project(port_driver)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra -pedantic -std=gnu99 -Wc++-compat -I$(ERL_INCLUDE) -fPIC")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -pedantic -std=c++14 -fPIC")

add_subdirectory(cda_integration)

cmake_path(APPEND CDA_INTEGRATION_INCLUDES "${CMAKE_CURRENT_SOURCE_DIR}" "cda_integration" "include")
include_directories("${CDA_INTEGRATION_INCLUDES}")

add_library(${PROJECT_NAME} SHARED port_driver.c)
set_target_properties(${PROJECT_NAME} PROPERTIES PREFIX "")
target_link_libraries(${PROJECT_NAME} PUBLIC cda_integration ei)