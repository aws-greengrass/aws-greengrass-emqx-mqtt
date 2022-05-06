file(GLOB_RECURSE ALL_SOURCE_FILES *.cpp *.h)

add_custom_target(
        clangformat
        COMMAND clang-format
        -style=file
        -i
        ${ALL_SOURCE_FILES}
)
