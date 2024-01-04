! {"id.inc"(_ID1, _ID2)} + () -(_PROC)> ()
! Define equality and inequality operators for kinds of the logical type.  `_ID1` and `_ID2` must be `_LOGICAL`.


#define _PROC _CAT3(_LABEL1,_eq_,_LABEL2)
elemental function _PROC(arg1, arg2) result(res)
    _ARG_TYPE1, intent(in) :: arg1
    _ARG_TYPE2, intent(in) :: arg2
    _RES_TYPE0 :: res

    res = arg1 .eqv. arg2
end function _PROC
#undef _PROC


#define _PROC _CAT3(_LABEL1,_ne_,_LABEL2)
elemental function _PROC(arg1, arg2) result(res)
    _ARG_TYPE1, intent(in) :: arg1
    _ARG_TYPE2, intent(in) :: arg2
    _RES_TYPE0 :: res

    res = arg1 .neqv. arg2
end function _PROC
#undef _PROC
