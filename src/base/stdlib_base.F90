! Basic utilities


module stdlib_base
    use stdlib_kinds
    use stdlib_ascii, only: DIGITS, UPPERCASE, LOWERCASE

    implicit none

    private
    public :: &
        operator(==), operator(/=), operator(//), operator(+), operator(*), &
        swap, sort, sorted, &
        strip, isdigit, isupper, islower, isletter, upper, lower, to_character, &
        getcmd, getcmdarg, getenv

#define _DECL(X) public :: _CAT3(to_,_TYPE_NAME,X)
#define _ID _LOGICAL
#define _DEFAULT
#include "../inc/decl.inc"
#define _ID _INTEGER
#define _DEFAULT
#include "../inc/decl.inc"
#define _ID _REAL
#define _DEFAULT
#include "../inc/decl.inc"
#define _ID _COMPLEX
#define _DEFAULT
#include "../inc/decl.inc"
#undef _DECL

    integer, parameter :: &
        SIMPLE_SORT_SIZE = 5, &
        LETTER_SHIFT = IACHAR(LOWERCASE(:1)) - IACHAR(UPPERCASE(:1)), &
        LEN_BUFFER = 256, &
        MAX_LEN_NUM_STR = 95

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

#define _OP to_character
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

#define _OP cat
    interface operator(//)
#define _ID1 _LOGICAL
#define _ID2 _CHARACTER
#define _DEFAULT_ONLY2
#include "../inc/decls.inc"
#define _ID1 _CHARACTER
#define _ID2 _LOGICAL
#define _DEFAULT_ONLY1
#include "../inc/decls.inc"
#define _ID1 _INTEGER
#define _ID2 _CHARACTER
#define _DEFAULT_ONLY2
#include "../inc/decls.inc"
#define _ID1 _CHARACTER
#define _ID2 _INTEGER
#define _DEFAULT_ONLY1
#include "../inc/decls.inc"
#define _ID1 _REAL
#define _ID2 _CHARACTER
#define _DEFAULT_ONLY2
#include "../inc/decls.inc"
#define _ID1 _CHARACTER
#define _ID2 _REAL
#define _DEFAULT_ONLY1
#include "../inc/decls.inc"
#define _ID1 _COMPLEX
#define _ID2 _CHARACTER
#define _DEFAULT_ONLY2
#include "../inc/decls.inc"
#define _ID1 _CHARACTER
#define _ID2 _COMPLEX
#define _DEFAULT_ONLY1
#include "../inc/decls.inc"
    end interface
#undef _OP

#define _OP join
    interface operator(+)
#define _ID1 _CHARACTER
#define _ID2 _CHARACTER
#define _DEFAULT_ONLY1
#define _DEFAULT_ONLY2
#include "../inc/decls.inc"
#define _ID1 _LOGICAL
#define _ID2 _CHARACTER
#define _DEFAULT_ONLY2
#include "../inc/decls.inc"
#define _ID1 _CHARACTER
#define _ID2 _LOGICAL
#define _DEFAULT_ONLY1
#include "../inc/decls.inc"
#define _ID1 _INTEGER
#define _ID2 _CHARACTER
#define _DEFAULT_ONLY2
#include "../inc/decls.inc"
#define _ID1 _CHARACTER
#define _ID2 _INTEGER
#define _DEFAULT_ONLY1
#include "../inc/decls.inc"
#define _ID1 _REAL
#define _ID2 _CHARACTER
#define _DEFAULT_ONLY2
#include "../inc/decls.inc"
#define _ID1 _CHARACTER
#define _ID2 _REAL
#define _DEFAULT_ONLY1
#include "../inc/decls.inc"
#define _ID1 _COMPLEX
#define _ID2 _CHARACTER
#define _DEFAULT_ONLY2
#include "../inc/decls.inc"
#define _ID1 _CHARACTER
#define _ID2 _COMPLEX
#define _DEFAULT_ONLY1
#include "../inc/decls.inc"
    end interface
#undef _OP

#define _OP mul
    interface operator(*)
#define _ID1 _INTEGER
#define _ID2 _CHARACTER
#include "../inc/decls.inc"
#define _ID1 _CHARACTER
#define _ID2 _INTEGER
#include "../inc/decls.inc"
    end interface
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

#define _OP sort
    interface _OP
#define _ID _INTEGER
#include "../inc/decls.inc"
#define _ID _REAL
#include "../inc/decls.inc"
#define _ID _CHARACTER
#include "../inc/decls.inc"
    end interface _OP
#undef _OP

#define _OP sorted
    interface _OP
#define _ID _INTEGER
#include "../inc/decls.inc"
#define _ID _REAL
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

#define _FILE "../base/obj2char.inc"
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

#define _FILE "../base/char2obj.inc"
#define _ID _LOGICAL
#define _DEFAULT
#include "../inc/defs.inc"
#define _ID _INTEGER
#define _DEFAULT
#include "../inc/defs.inc"
#define _ID _REAL
#define _DEFAULT
#include "../inc/defs.inc"
#define _ID _COMPLEX
#define _DEFAULT
#include "../inc/defs.inc"
#undef _FILE

#define _FILE "../base/cat.inc"
#define _ID1 _CHARACTER
#define _ID2 _LOGICAL
#define _DEFAULT_ONLY1
#include "../inc/defs.inc"
#define _ID1 _CHARACTER
#define _ID2 _INTEGER
#define _DEFAULT_ONLY1
#include "../inc/defs.inc"
#define _ID1 _CHARACTER
#define _ID2 _REAL
#define _DEFAULT_ONLY1
#include "../inc/defs.inc"
#define _ID1 _CHARACTER
#define _ID2 _COMPLEX
#define _DEFAULT_ONLY1
#include "../inc/defs.inc"
#undef _FILE

#define _FILE "../base/join_s.inc"
#define _ID _CHARACTER
#define _DEFAULT_ONLY
#include "../inc/defs.inc"
#undef _FILE

#define _FILE "../base/join.inc"
#define _ID1 _CHARACTER
#define _ID2 _LOGICAL
#define _DEFAULT_ONLY1
#include "../inc/defs.inc"
#define _ID1 _CHARACTER
#define _ID2 _INTEGER
#define _DEFAULT_ONLY1
#include "../inc/defs.inc"
#define _ID1 _CHARACTER
#define _ID2 _REAL
#define _DEFAULT_ONLY1
#include "../inc/defs.inc"
#define _ID1 _CHARACTER
#define _ID2 _COMPLEX
#define _DEFAULT_ONLY1
#include "../inc/defs.inc"
#undef _FILE

#define _FILE "../base/mul.inc"
#define _ID1 _INTEGER
#define _ID2 _CHARACTER
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

#define _FILE "../base/sort.inc"
#define _ID _INTEGER
#include "../inc/defs.inc"
#define _ID _REAL
#include "../inc/defs.inc"
#define _ID _CHARACTER
#include "../inc/defs.inc"
#undef _FILE

    pure function strip(arg) result(res)
        character(len=*), intent(in) :: arg
        character(len=:), allocatable :: res

        res = TRIM(ADJUSTL(arg))
    end function strip


    elemental function isdigit(arg) result(res)
        character(len=1), intent(in) :: arg
        logical :: res

        res = DIGITS(:1) <= arg .and. arg <= DIGITS(LEN(DIGITS):)
    end function isdigit


    elemental function isupper(arg) result(res)
        character(len=1), intent(in) :: arg
        logical :: res

        res = UPPERCASE(:1) <= arg .and. arg <= UPPERCASE(LEN(UPPERCASE):)
    end function isupper


    elemental function islower(arg) result(res)
        character(len=1), intent(in) :: arg
        logical :: res

        res = LOWERCASE(:1) <= arg .and. arg <= LOWERCASE(LEN(LOWERCASE):)
    end function islower


    elemental function isletter(arg) result(res)
        character(len=1), intent(in) :: arg
        logical :: res

        res = isupper(arg) .or. islower(arg)
    end function isletter


    elemental function upper(arg) result(res)
        character(len=*), intent(in) :: arg
        character(len=LEN(arg)) :: res

        integer :: i
        character(len=1) :: symbol

        res = arg
        do i = 1, LEN(res)
            symbol = res(i:i)
            if (islower(symbol)) res(i:i) = ACHAR(IACHAR(symbol) - LETTER_SHIFT)
        end do
    end function upper


    elemental function lower(arg) result(res)
        character(len=*), intent(in) :: arg
        character(len=LEN(arg)) :: res

        integer :: i
        character(len=1) :: symbol

        res = arg
        do i = 1, LEN(res)
            symbol = res(i:i)
            if (isupper(symbol)) res(i:i) = ACHAR(IACHAR(symbol) + LETTER_SHIFT)
        end do
    end function lower


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
