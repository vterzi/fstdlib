#define _PROC _CAT3(character,_KIND_LABEL1,_LABEL2)
pure function _PROC(arg, kind, width) result(res)
    _TYPE2(len=*, kind=_KIND2), intent(in) :: arg
#if _KIND1 == _DEFAULT_KIND1
    character(len=*, kind=_KIND1), intent(in), optional :: kind
#else
    character(len=*, kind=_KIND1), intent(in) :: kind
#endif
    integer, intent(in), optional :: width
    character(len=:, kind=_KIND1), allocatable :: res

    integer :: width_

    if (present(width)) then
        width_ = max(0, width)
        call alloc(res, width_)
        write(res, "(a" // str(width_) // ")") arg
    else
        res = arg
    end if
end function _PROC
#undef _PROC
