#define _PROC _CAT3(character,_KIND_LABEL1,_LABEL2)
pure function _PROC(arg, mold, width) result(res)
    _TYPE2(len=*, kind=_KIND2), intent(in) :: arg
#if _KIND1 == _DEFAULT_KIND1
    character(len=*, kind=_KIND1), intent(in), optional :: mold
#else
    character(len=*, kind=_KIND1), intent(in) :: mold
#endif
    integer, intent(in), optional :: width
    character(len=:, kind=_KIND1), allocatable :: res

    integer :: width_

    if (present(width)) then
        width_ = max(0, width)
        allocate(character(len=width_, kind=_KIND1) :: res)
        write(res, "(a" // str(width_) // ")") arg
    else
        res = arg
    end if
end function _PROC
#undef _PROC
