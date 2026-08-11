#define _PROC _CAT3(_LABEL1,cat,_LABEL2)
pure function _PROC(arg1, arg2) result(res)
    character(len=*, kind=_KIND1), intent(in) :: arg1
    _TYPE2(_KIND2), intent(in) :: arg2
    character(len=:, kind=_KIND1), allocatable :: res

    res = arg1 // character(arg2, mold=res)
end function _PROC
#undef _PROC


#define _PROC _CAT3(_LABEL2,cat,_LABEL1)
pure function _PROC(arg1, arg2) result(res)
    _TYPE2(_KIND2), intent(in) :: arg1
    character(len=*, kind=_KIND1), intent(in) :: arg2
    character(len=:, kind=_KIND1), allocatable :: res

    res = character(arg1, mold=res) // arg2
end function _PROC
#undef _PROC
