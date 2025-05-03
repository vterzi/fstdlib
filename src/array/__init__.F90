! Array utilities


module stdlib_array
    use stdlib_kinds
    use stdlib_base, only: swap

    implicit none

    private
    public :: sort, sorted

    integer, parameter :: SIMPLE_SORT_SIZE = 5

#include "../inc/proc.inc"
#define _DECL_ONE(X) module procedure :: _UNARY(X)
#define _DECL_TWO(X,Y) module procedure :: _BINARY(X,Y)

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

#define _FILE "../array/sort.inc"
#define _ID _INTEGER
#include "../inc/defs.inc"
#define _ID _REAL
#include "../inc/defs.inc"
#define _ID _CHARACTER
#include "../inc/defs.inc"
#undef _FILE

end module stdlib_array
