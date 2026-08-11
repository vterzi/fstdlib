#include "../inc/init.inc"
#include "../inc/utils.inc"


module pytran_builtins
    use pytran_conversion, only: character

    implicit none

    private

    logical, parameter, public :: &
        False = .false., &
        True = .true.

#define _PROC _BINARY_OP(_OP)
#define _IFACE operator(._OP.)
#define _TYPE_IDS1 _LOGICAL
#define _TYPE_IDS2 _LOGICAL
#define _OP eq
#include "../inc/iface.inc"
#define _OP ne
#include "../inc/iface.inc"
#define _OP lt
#include "../inc/iface.inc"
#define _OP le
#include "../inc/iface.inc"
#define _OP gt
#include "../inc/iface.inc"
#define _OP ge
#include "../inc/iface.inc"
#undef _TYPE_IDS1
#undef _TYPE_IDS2
#undef _PROC

#define _PROC _UNARY_OP(_OP)
#define _TYPE_IDS (_LOGICAL | _INTEGER | _REAL | _COMPLEX | _CHARACTER)
#define _OP assign_optional
#include "../inc/iface.inc"
#define _OP swap
#include "../inc/iface.inc"
#undef _TYPE_IDS
#undef _PROC

    public :: operator(//)
#define _PROC _BINARY_OP(_OP)
#define _TYPE_IDS1 _CHARACTER
#define _TYPE_IDS2 (_LOGICAL | _INTEGER | _REAL | _COMPLEX)
#define _IFACE operator(//)
#define _NO_PUBLIC
#define _OP cat
#include "../inc/iface.inc"
#undef _TYPE_IDS1
#undef _TYPE_IDS2
#define _TYPE_IDS1 (_LOGICAL | _INTEGER | _REAL | _COMPLEX)
#define _TYPE_IDS2 _CHARACTER
#define _IFACE operator(//)
#define _NO_PUBLIC
#define _OP cat
#include "../inc/iface.inc"
#undef _TYPE_IDS1
#undef _TYPE_IDS2
#undef _PROC
#if defined(_ASCII) && defined(_UCS4)
    interface operator(//)
        module procedure :: SA_cat_SU
        module procedure :: SU_cat_SA
    end interface operator(//)
#endif

contains

#define _TYPE_IDS1 _LOGICAL
#define _TYPE_IDS2 _LOGICAL
#define _FILE "../builtins/cmp.inc"
#include "../inc/types.inc"
#undef _FILE
#undef _TYPE_IDS1
#undef _TYPE_IDS2

#define _TYPE_IDS (_LOGICAL | _INTEGER | _REAL | _COMPLEX | _CHARACTER)
#define _FILE "../builtins/assign_optional.inc"
#include "../inc/types.inc"
#undef _FILE
#define _FILE "../builtins/swap.inc"
#include "../inc/types.inc"
#undef _FILE
#undef _TYPE_IDS

#define _TYPE_IDS1 _CHARACTER
#define _TYPE_IDS2 (_LOGICAL | _INTEGER | _REAL | _COMPLEX)
#define _FILE "../builtins/cat.inc"
#include "../inc/types.inc"
#undef _FILE
#undef _TYPE_IDS1
#undef _TYPE_IDS2


#if defined(_ASCII) && defined(_UCS4)
    pure function SA_cat_SU(arg1, arg2) result(res)
        character(len=*, kind=_ASCII), intent(in) :: arg1
        character(len=*, kind=_UCS4), intent(in) :: arg2
        character(len=(len(arg1) + len(arg2)), kind=_UCS4) :: res

        res = character(arg1, mold=res) // arg2
    end function SA_cat_SU


    pure function SU_cat_SA(arg1, arg2) result(res)
        character(len=*, kind=_UCS4), intent(in) :: arg1
        character(len=*, kind=_ASCII), intent(in) :: arg2
        character(len=(len(arg1) + len(arg2)), kind=_UCS4) :: res

        res = arg1 // character(arg2, mold=res)
    end function SU_cat_SA
#endif

end module pytran_builtins
