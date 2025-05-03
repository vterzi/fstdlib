! Basic utilities


module stdlib_base
    use stdlib_kinds

    implicit none

    private
    public :: &
        operator(==), operator(/=), &
        swap, default_assign, &
        getcmd, getcmdarg, getenv

#include "../inc/proc.inc"
#define _DECL_ONE(X) module procedure :: _UNARY(X)
#define _DECL_TWO(X,Y) module procedure :: _BINARY(X,Y)

#define _OP eq
    interface operator(._OP.)
#define _ID1 _LOGICAL
#define _ID2 _LOGICAL
#include "../inc/decls.inc"
    end interface
#undef _OP

#define _OP ne
    interface operator(._OP.)
#define _ID1 _LOGICAL
#define _ID2 _LOGICAL
#include "../inc/decls.inc"
    end interface
#undef _OP

#define _OP default_assign
    interface _OP
#define _ID _LOGICAL
#include "../inc/decls.inc"
#define _ID _INTEGER
#include "../inc/decls.inc"
#define _ID _REAL
#include "../inc/decls.inc"
#define _ID _COMPLEX
#include "../inc/decls.inc"
#define _ID _CHARACTER
#include "../inc/decls.inc"
    end interface _OP
#undef _OP

#define _OP swap
    interface _OP
#define _ID _LOGICAL
#include "../inc/decls.inc"
#define _ID _INTEGER
#include "../inc/decls.inc"
#define _ID _REAL
#include "../inc/decls.inc"
#define _ID _COMPLEX
#include "../inc/decls.inc"
#define _ID _CHARACTER
#include "../inc/decls.inc"
    end interface _OP
#undef _OP

#undef _DECL_ONE
#undef _DECL_TWO

contains
#define _FILE "../base/cmp.inc"
#define _ID1 _LOGICAL
#define _ID2 _LOGICAL
#include "../inc/defs.inc"
#undef _FILE

#define _FILE "../base/default_assign.inc"
#define _ID _LOGICAL
#include "../inc/defs.inc"
#define _ID _INTEGER
#include "../inc/defs.inc"
#define _ID _REAL
#include "../inc/defs.inc"
#define _ID _COMPLEX
#include "../inc/defs.inc"
#define _ID _CHARACTER
#include "../inc/defs.inc"
#undef _FILE

#define _FILE "../base/swap.inc"
#define _ID _LOGICAL
#include "../inc/defs.inc"
#define _ID _INTEGER
#include "../inc/defs.inc"
#define _ID _REAL
#include "../inc/defs.inc"
#define _ID _COMPLEX
#include "../inc/defs.inc"
#define _ID _CHARACTER
#include "../inc/defs.inc"
#undef _FILE


    function getcmd(success) result(val)
        logical, intent(out), optional :: success
        character(len=:), allocatable :: val

        integer :: length, status

        call GET_COMMAND(length=length)
        allocate(character(len=length) :: val)
        call GET_COMMAND(val, status=status)
        if (PRESENT(success)) success = status == 0
    end function getcmd


    function getcmdarg(number, success) result(val)
        integer, intent(in) :: number
        logical, intent(out), optional :: success
        character(len=:), allocatable :: val

        integer :: length, status

        call GET_COMMAND_ARGUMENT(number, length=length)
        allocate(character(len=length) :: val)
        call GET_COMMAND_ARGUMENT(number, val, status=status)
        if (PRESENT(success)) success = status == 0
    end function getcmdarg


    function getenv(name, success) result(val)
        character(len=*), intent(in) :: name
        logical, intent(out), optional :: success
        character(len=:), allocatable :: val

        integer :: length, status

        call GET_ENVIRONMENT_VARIABLE(name, length=length)
        allocate(character(len=length) :: val)
        call GET_ENVIRONMENT_VARIABLE(name, val, status=status)
        if (PRESENT(success)) success = status == 0
    end function getenv
end module stdlib_base
