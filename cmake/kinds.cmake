try_run(run_result compile_result
    ${CMAKE_BINARY_DIR}
    ${CMAKE_SOURCE_DIR}/cmake/kinds.f90
    RUN_OUTPUT_VARIABLE output
)

function (add_kind label)
    string(REGEX MATCH "${label} +([0-9]+)" match ${output})
    if (NOT ${match} STREQUAL "")
        add_compile_definitions("_${label}=${CMAKE_MATCH_1}")
    endif ()
endfunction ()

if (${run_result} EQUAL 0)
    add_kind("K1")
    add_kind("K2")
    add_kind("K4")
    add_kind("K8")
    add_kind("K16")
    add_kind("HP")
    add_kind("SP")
    add_kind("DP")
    add_kind("XDP")
    add_kind("QP")
    add_kind("ASCII")
    add_kind("UCS4")
else ()
    message(FATAL_ERROR "Failed to determine available kinds of basic data types")
endif ()
