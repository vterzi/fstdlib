cmake_minimum_required(VERSION 3.14.0)

project(stdlib
        LANGUAGES Fortran
        VERSION 0.1.0
        )

set(CMAKE_Fortran_STANDARD 2008)

include(cmake/kinds.cmake)

add_subdirectory(src)
