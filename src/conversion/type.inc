#define _PROC _CAT3(_TYPE1,_KIND_LABEL1,_LABEL2)
elemental function _PROC(arg, mold) result(res)
    character(len=*, kind=_KIND2), intent(in) :: arg
#if _KIND1 == _DEFAULT_KIND1
    _TYPE1(_KIND1), intent(in), optional :: mold
#else
    _TYPE1(_KIND1), intent(in) :: mold
#endif
    _TYPE1(_KIND1) :: res

    read(arg, *) res
end function _PROC
#undef _PROC
