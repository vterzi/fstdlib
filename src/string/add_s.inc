! {"id.inc"(_ID)} + () -(_PROC)> ()
! Define space-separated concatenation functions for the character type.  `_ID`
! must be `_CHARACTER`.


#define _PROC _CAT3(_LABEL,_add_,_LABEL)
elemental function _PROC(arg1, arg2) result(res)
    _ARG_TYPE, intent(in) :: arg1, arg2
    _TYPE_LEN(len(arg1)+1+len(arg2)) :: res

    res = arg1 // ' ' // arg2
end function _PROC
#undef _PROC
