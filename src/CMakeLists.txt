set(SRC
    stdlib.F90
    stdlib_kinds.F90
    stdlib_ascii.F90
    unittest/stdlib_unittest.F90
    base/stdlib_base.F90
    list/stdlib_list.F90
    stdlib_io.F90
    )

add_library(stdlib SHARED "${SRC}")
