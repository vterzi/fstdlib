include(CheckFortranSourceRuns)

check_fortran_source_runs(
        "if (SELECTED_INT_KIND(38) == -1) stop 1; end"
        _K16
)
if(_K16)
    add_definitions(-D_K16)
endif()

check_fortran_source_runs(
        "if (ANY(SELECTED_REAL_KIND(18) == [-1, SELECTED_REAL_KIND(33)])) stop 1; end"
        _XDP
)
if(_XDP)
    add_definitions(-D_XDP)
endif()

check_fortran_source_runs(
        "if (SELECTED_REAL_KIND(33) == -1) stop 1; end"
        _QP
)
if(_QP)
    add_definitions(-D_QP)
endif()

check_fortran_source_runs(
        "if (SELECTED_CHAR_KIND('ISO_10646') == -1) stop 1; end"
        _UCS4
)
if(_UCS4)
    add_definitions(-D_UCS4)
endif()
