#include "../inc/cat.inc"

#undef _UNARY
#define _UNARY(X) _CAT4(_OP,_,_TYPE_LABEL,X)
#undef _BINARY
#define _BINARY(X,Y) _CAT7(_TYPE_LABEL1,X,_,_OP,_,_TYPE_LABEL2,Y)
