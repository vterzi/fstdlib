! {"id.inc"(_ID1, _ID2)} + () -(_PROC)> ()
! Define repitition functions for the character type using the integer type.
! `_ID1` must be `_INTEGER` and `_ID2` must be `_CHARACTER`.


#define _PROC _CAT3(_LABEL1,_mul_,_LABEL2)
elemental function _PROC(arg1, arg2) result(res)
    _ARG_TYPE1, intent(in) :: arg1
    _ARG_TYPE2, intent(in) :: arg2
    _TYPE_LEN2(arg1*len(arg2)) :: res

    res = repeat(arg2, arg1)
end function _PROC
#undef _PROC


#define _PROC _CAT3(_LABEL2,_mul_,_LABEL1)
elemental function _PROC(arg1, arg2) result(res)
    _ARG_TYPE2, intent(in) :: arg1
    _ARG_TYPE1, intent(in) :: arg2
    _TYPE_LEN2(len(arg1)*arg2) :: res

    res = repeat(arg1, arg2)
end function _PROC
#undef _PROC
