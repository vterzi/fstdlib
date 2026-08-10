#include "../inc/var.inc"


#define _PROC _CAT3(_LABEL1,cat,_LABEL2)
pure function _PROC(arg1, arg2) result(res)
    _ARG_TYPE1, intent(in) :: arg1
    _ARG_TYPE2, intent(in) :: arg2
    _RES_TYPE1 :: res

    res = arg1 // character(arg2, kind=res)
end function _PROC
#undef _PROC


#define _PROC _CAT3(_LABEL2,cat,_LABEL1)
pure function _PROC(arg1, arg2) result(res)
    _ARG_TYPE2, intent(in) :: arg1
    _ARG_TYPE1, intent(in) :: arg2
    _RES_TYPE1 :: res

    res = character(arg1, kind=res) // arg2
end function _PROC
#undef _PROC
