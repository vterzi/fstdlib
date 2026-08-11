#define _PROC _CAT3(_LABEL1,cat,_LABEL2)
pure function _PROC(arg1, arg2) result(res)
    character(len=*, kind=_KIND1), intent(in) :: arg1
    character(len=*, kind=_KIND2), intent(in) :: arg2
#if _KIND1 > _KIND2
    character(len=(len(arg1) + len(arg2)), kind=_KIND1) :: res

    res = arg1 // character(arg2, mold=res)
#else
    character(len=(len(arg1) + len(arg2)), kind=_KIND2) :: res

    res = character(arg1, mold=res) // arg2
#endif
end function _PROC
#undef _PROC
