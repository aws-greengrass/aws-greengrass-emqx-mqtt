cmake_minimum_required(VERSION 3.20)

project(cda_integration)

find_package(aws-crt-cpp REQUIRED)
find_package(GreengrassIpc-cpp REQUIRED)

include_directories(include)
add_subdirectory(lib)

target_link_libraries(${PROJECT_NAME} PUBLIC AWS::aws-crt-cpp AWS::EventstreamRpc-cpp AWS::GreengrassIpc-cpp)
