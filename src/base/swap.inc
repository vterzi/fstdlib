#define _PROC _CAT(swap_,_LABEL)
elemental subroutine _PROC(arg1, arg2)
    _ARG_TYPE, intent(inout) :: arg1, arg2

    _VAR_TYPE :: var

    var = arg1
    arg1 = arg2
    arg2 = var
end subroutine _PROC
#undef _PROC
