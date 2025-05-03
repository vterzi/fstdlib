! {"id.inc"(_ID)} + () -(_PROC)> ()
! Define conversion functions from other basic data types to the character type.


#define _PROC _CAT(to_character_,_LABEL)
pure function _PROC(arg, fmt) result(res)
    _ARG_TYPE, intent(in) :: arg
    character(len=*), intent(in), optional :: fmt
    character(len=:), allocatable :: res

    character(len=MAX_LEN_NUM_STR) :: string

    if (PRESENT(fmt)) then
        write(string, '(' // fmt // ')') arg
    else
        write(string, *) arg
        string = ADJUSTL(string)
    end if
    res = TRIM(string)
end function _PROC
#undef _PROC
