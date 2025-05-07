! Math utilities


module stdlib_math
    use stdlib_kinds

    implicit none

    private
    public :: dec_digits

#include "../inc/proc.inc"
#define _DECL_ONE(X) module procedure :: _UNARY(X)
#define _DECL_TWO(X,Y) module procedure :: _BINARY(X,Y)

#define _OP dec_digits
    interface _OP
#define _ID _INTEGER
#include "../inc/decls.inc"
    end interface _OP
#undef _OP

#undef _DECL_ONE
#undef _DECL_TWO

contains

#define _FILE "../math/dec_digits.inc"
#define _ID _INTEGER
#define _DEFAULT
#include "../inc/defs.inc"
#undef _FILE

end module stdlib_math
