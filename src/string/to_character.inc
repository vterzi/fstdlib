! {"id.inc"(_ID)} + () -(_PROC)> ()
! Define conversion functions from other basic data types to the character
! type.


#define _PROC _CAT(to_character_,_LABEL)
pure function _PROC(arg, fmt) result(res)
    _ARG_TYPE, intent(in) :: arg
    character(len=*), intent(in), optional :: fmt
    character(len=:), allocatable :: res

    integer :: lo, up
    character(len=MAX_LEN_NUM_STR) :: buffer

    if (present(fmt)) then
        lo = scan(fmt, DIGITS)
        up = verify(fmt(lo:), DIGITS) - 1
        if (up == 0) up = len(fmt)
        allocate(character(len=to_integer(fmt(lo:up))) :: res)
        write(res, '(' // fmt // ')') arg
    else
        write(buffer, *) arg
        res = trim(adjustl(buffer))
    end if
end function _PROC
#undef _PROC
