! {"id.inc", _OP} + () -> ("cat.inc", _UNARY, _BINARY)
! Define the macros to construct procedure names, `_UNARY(<kind_label>)` and `_BINARY(<kind_label_1>,<kind_label_2>)`, for the
! operator `_OP`.

#include "../inc/cat.inc"

#undef _UNARY
#define _UNARY(X) _CAT4(_OP,_,_TYPE_LABEL,X)
#undef _BINARY
#define _BINARY(X,Y) _CAT7(_TYPE_LABEL1,X,_,_OP,_,_TYPE_LABEL2,Y)
