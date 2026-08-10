#include "../inc/init.inc"
#include "../inc/utils.inc"

module pytran_utils
    implicit none

    private
    public :: str

#define _PROC _UNARY_OP(_OP)
#define _TYPE_IDS _CHARACTER
#define _OP alloc
#include "../inc/iface.inc"
#undef _TYPE_IDS
#undef _PROC

contains
    pure function str(arg) result(res)
        integer, intent(in) :: arg
        character(len=:), allocatable :: res

        character(len=_MAX_LEN_INTEGER_STR) :: buf

        write(buf, '(i0)') arg
        res = trim(buf)
    end function str


#define _TYPE_IDS _CHARACTER
#define _FILE "../utils/alloc.inc"
#include "../inc/types.inc"
#undef _FILE
#undef _TYPE_IDS

end module pytran_utils
