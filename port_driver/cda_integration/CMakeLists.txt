cmake_minimum_required(VERSION 3.20)

project(cda_integration)

find_package(aws-crt-cpp REQUIRED)
find_package(GreengrassIpc-cpp REQUIRED)

include_directories(include)
add_subdirectory(lib)

set_property(TARGET ${PROJECT_NAME} PROPERTY CXX_STANDARD 23)
target_link_libraries(${PROJECT_NAME} PRIVATE AWS::aws-crt-cpp AWS::EventstreamRpc-cpp AWS::GreengrassIpc-cpp)
