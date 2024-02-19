! {"id.inc"(_ID1, _ID2)} + () -(_PROC)> ()
! Define concatenation functions for the character type with other basic data types.  `_ID1` must be `_CHARACTER`.


#define _PROC _CAT3(_LABEL1,_cat_,_LABEL2)
pure function _PROC(arg1, arg2) result(res)
    _ARG_TYPE1, intent(in) :: arg1
    _ARG_TYPE2, intent(in) :: arg2
    _ALLOC_TYPE1 :: res

    res = arg1 // to_character(arg2)
end function _PROC
#undef _PROC


#define _PROC _CAT3(_LABEL2,_cat_,_LABEL1)
pure function _PROC(arg1, arg2) result(res)
    _ARG_TYPE2, intent(in) :: arg1
    _ARG_TYPE1, intent(in) :: arg2
    _ALLOC_TYPE1 :: res

    res = to_character(arg1) // arg2
end function _PROC
#undef _PROC
