#define _PROC _UNARY_OP(alloc)
pure subroutine _PROC(arg, len)
    character(len=:, kind=_KIND), allocatable, intent(out) :: arg
    integer, intent(in) :: len

    allocate(character(len=len, kind=_KIND) :: arg)
end subroutine _PROC
#undef _PROC
