! {"id.inc"(_ID)} + () -(_PROC, _ARR)> ()
! Define functions that determine the decimal exponent of real numbers.

#define _ARR _CAT3(REAL_,_KIND_LABEL,_DEC_POWERS)
#define _PROC _CAT(dec_exponent_,_LABEL)
elemental function _PROC(num) result(res)
    _ARG_TYPE, intent(in) :: num
    integer :: res

    _ELEM_TYPE :: n

    ! `res = floor(log10(abs(num)))` is numerically less stable.
    n = abs(num)
    if (n == 0) then
        res = -huge(res)
    else if (n == _CAT3(REAL_,_KIND_LABEL,_INF)) then
        res = huge(res)
    else
        res = 0
        if (n < 1) then
            do while (n < _ARR(res))
                res = res - 1
            end do
        else
            do while (n >= _ARR(res + 1))
                res = res + 1
            end do
        end if
    end if
end function _PROC
#undef _PROC
#undef _ARR
