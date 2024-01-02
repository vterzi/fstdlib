#define _PROC _CAT(ToCharacter_,_LABEL)
pure function _PROC(arg) result(res)
    _ARG_TYPE, intent(in) :: arg
    character(len=:), allocatable :: res

    character(len=MAX_LEN_NUM_STR) :: string

    write(string, *) arg
    res = Strip(string)
end function _PROC
#undef _PROC
