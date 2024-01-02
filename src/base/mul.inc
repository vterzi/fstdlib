#define _PROC _CAT3(_LABEL1,_Mul_,_LABEL2)
elemental function _PROC(arg1, arg2) result(res)
    _ARG_TYPE1, intent(in) :: arg1
    _ARG_TYPE2, intent(in) :: arg2
    _TYPE_LEN2(arg1*LEN(arg2)) :: res

    res = REPEAT(arg2, arg1)
end function _PROC
#undef _PROC


#define _PROC _CAT3(_LABEL2,_Mul_,_LABEL1)
elemental function _PROC(arg1, arg2) result(res)
    _ARG_TYPE2, intent(in) :: arg1
    _ARG_TYPE1, intent(in) :: arg2
    _TYPE_LEN2(LEN(arg1)*arg2) :: res

    res = REPEAT(arg1, arg2)
end function _PROC
#undef _PROC
