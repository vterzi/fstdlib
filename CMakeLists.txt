cmake_minimum_required(VERSION 3.14.0)

project(stdlib
    LANGUAGES Fortran
    VERSION 0.1.0
    )

set(CMAKE_Fortran_STANDARD 2008)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/include)

include(cmake/kinds.cmake)
find_package(MPI)
# ${MPI_Fortran_FOUND} ${MPI_Fortran_HAVE_F08_MODULE}

add_subdirectory(src)

include(CTest)
add_subdirectory(tests)
