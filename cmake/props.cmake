try_run(run_result compile_result
    "${PROJECT_BINARY_DIR}"
    "${CMAKE_SOURCE_DIR}/cmake/props.F90"
    RUN_OUTPUT_VARIABLE output
    COMPILE_DEFINITIONS "${compile_definitions}"
)

if("${run_result}" EQUAL 0)
    string(REGEX MATCH "-huge\\(0\\)-1= *(-[0-9]+)" match "${output}")
    set(min_int "${CMAKE_MATCH_1}")
    set(info " *SIZE= *([0-9]+)\n *STR=`\n([^`]+)\n *`")
    set(info "${info}\n( *INF=`\n([^`]+)\n *`\n *NAN=`\n([^`]+)\n *`)?")
    string(REGEX MATCHALL "[A-Z0-9_]+:" list "${output}")
    foreach(item IN LISTS list)
        string(REGEX MATCH "[A-Z0-9_]+" item "${item}")
        string(REGEX MATCH "${item}:\n${info}" match "${output}")
        if(NOT "${match}" STREQUAL "")
            add_compile_definitions("_${item}_SIZE=${CMAKE_MATCH_1}")
            string(LENGTH "${CMAKE_MATCH_2}" len)
            add_compile_definitions("_MAX_LEN_${item}_STR=${len}")
            if(NOT "${CMAKE_MATCH_3}" STREQUAL "")
                set(inf_list "${CMAKE_MATCH_4}")
                set(nan_list "${CMAKE_MATCH_5}")
                string(REGEX MATCH "_[A-Z0-9]+" kind "${item}")
                string(STRIP "${inf_list}" list)
                string(REGEX REPLACE "${min_int}" "-huge(0)-1" list "${list}")
                string(REGEX REPLACE "[ \n]+" "," list "${list}")
                add_compile_definitions(
                    "_${item}_INF=transfer([${list}],0.${kind})"
                )
                string(STRIP "${nan_list}" list)
                string(REGEX REPLACE "${min_int}" "-huge(0)-1" list "${list}")
                string(REGEX REPLACE "[ \n]+" "," list "${list}")
                add_compile_definitions(
                    "_${item}_NAN=transfer([${list}],0.${kind})"
                )
            endif()
        endif()
    endforeach()
else()
    message(FATAL_ERROR
        "Failed to determine properties for kinds of basic data types"
    )
endif()
