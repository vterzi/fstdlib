! () -> (_PASTE, _CAT, _CAT3, _CAT4, _CAT5, _CAT6, _CAT7, _CAT8, _CAT9)
! Define the concatenation macros for up to 9 arguments.  The macro for two arguments is `_CAT(<string_1>,<string_2>)`.  The macros
! for more arguments contain the number of arguments as a suffix in their names.

#undef _PASTE
#undef _CAT
#undef _CAT3
#undef _CAT4
#undef _CAT5
#undef _CAT6
#undef _CAT7
#undef _CAT8
#undef _CAT9

#define _PASTE(X) X
#define _CAT(A,B) _PASTE(A)B
#define _CAT3(A,B,C) _CAT(_CAT(A,B),C)
#define _CAT4(A,B,C,D) _CAT(_CAT3(A,B,C),D)
#define _CAT5(A,B,C,D,E) _CAT(_CAT4(A,B,C,D),E)
#define _CAT6(A,B,C,D,E,F) _CAT(_CAT5(A,B,C,D,E),F)
#define _CAT7(A,B,C,D,E,F,G) _CAT(_CAT6(A,B,C,D,E,F),G)
#define _CAT8(A,B,C,D,E,F,G,H) _CAT(_CAT7(A,B,C,D,E,F,G),H)
#define _CAT9(A,B,C,D,E,F,G,H,I) _CAT(_CAT8(A,B,C,D,E,F,G,H),I)
