#define _PROC _CAT(sort_,_LABEL)
pure subroutine _PROC(array, back)
    _ARG_TYPE, intent(inout) :: array(:)
    logical, intent(in), optional :: back

    logical :: back_

    back_ = .false.
    if (PRESENT(back)) back_ = back

    if (back_) then
        call sort_back(array)
    else
        call sort_fore(array)
    end if

contains
#define _SUBPROC sort_fore
#include "../base/sort_proc.inc"

#define _SUBPROC sort_back
#define _BACK
#include "../base/sort_proc.inc"
end subroutine _PROC
#undef _PROC


#define _PROC _CAT(sorted_,_LABEL)
pure function _PROC(array, back) result(res)
    _ARG_TYPE, intent(in) :: array(:)
    logical, intent(in), optional :: back
    _TYPE_LEN(LEN(array)) :: res(SIZE(array))

    res = array
    call sort(res, back)
end function _PROC
#undef _PROC
