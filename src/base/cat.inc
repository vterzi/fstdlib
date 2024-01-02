#define _PROC _CAT(Cat_,_LABEL)
elemental function _PROC(arg1, arg2) result(res)
    _ARG_TYPE, intent(in) :: arg1, arg2
    _TYPE_LEN(LEN(arg1)+LEN(arg2)) :: res

    res = arg1 // arg2
end function _PROC
#undef _PROC
