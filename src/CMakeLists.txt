set(SRC
        stdlib.F90
        stdlib_kinds.F90
        stdlib_ascii.F90
        base/stdlib_base.F90
        )

add_library(stdlib SHARED "${SRC}")

set_target_properties(stdlib PROPERTIES
        LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")
