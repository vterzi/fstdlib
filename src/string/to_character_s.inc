! {"id.inc"(_ID)} + () -(_PROC)> ()
! Define conversion functions from the character type to the character type.


#define _PROC _CAT(to_character_,_LABEL)
pure function _PROC(arg, fmt) result(res)
    _ARG_TYPE, intent(in) :: arg
    character(len=*), intent(in), optional :: fmt
    character(len=:), allocatable :: res

    if (present(fmt)) then
        allocate(character(len=to_integer(fmt(1:))) :: res)
        write(res, '(' // fmt // ')') arg
    else
        res = arg
    end if
end function _PROC
#undef _PROC
