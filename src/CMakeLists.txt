set(SRC
    stdlib.F90
    kinds.F90
    base/__init__.F90
    string/__init__.F90
    array/__init__.F90
    unittest/__init__.F90
    list/__init__.F90
    io.F90
)

add_library(stdlib SHARED "${SRC}")
