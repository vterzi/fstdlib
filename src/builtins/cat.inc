#include "../inc/var.inc"


#define _PROC _CAT3(SA,cat,_LABEL)
pure function _PROC(arg1, arg2) result(res)
    character(len=*), intent(in) :: arg1
    _ARG_TYPE, intent(in) :: arg2
    character(len=:), allocatable :: res

    res = arg1 // character(arg2)
end function _PROC
#undef _PROC


#define _PROC _CAT3(_LABEL,cat,SA)
pure function _PROC(arg1, arg2) result(res)
    _ARG_TYPE, intent(in) :: arg1
    character(len=*), intent(in) :: arg2
    character(len=:), allocatable :: res

    res = character(arg1) // arg2
end function _PROC
#undef _PROC
