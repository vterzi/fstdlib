function (add_unittest label)
    add_executable("test_${label}" "test_${label}.f90")
    target_link_libraries("test_${label}" PRIVATE stdlib)
    target_include_directories("test_${label}" PRIVATE ${PROJECT_BINARY_DIR}/include)
    add_test(NAME "test_${label}" COMMAND "test_${label}")
endfunction ()

add_unittest(stdlib_list)
