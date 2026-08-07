#define _(X) X
#define _CAT2(X,Y) _(X)_(_)_(Y)
#define _CAT3(X,Y,Z) _(X)_(_)_(Y)_(_)_(Z)
#ifdef _DIM_LABEL
#  define _LABEL _(_TYPE_LABEL)_(_DIM_LABEL)_(_KIND_LABEL)
#else
#  define _LABEL _(_TYPE_LABEL)_(_KIND_LABEL)
#endif
#ifdef _DIM_LABEL1
#  define _LABEL1 _(_TYPE_LABEL1)_(_DIM_LABEL1)_(_KIND_LABEL1)
#else
#  define _LABEL1 _(_TYPE_LABEL1)_(_KIND_LABEL1)
#endif
#ifdef _DIM_LABEL2
#  define _LABEL2 _(_TYPE_LABEL2)_(_DIM_LABEL2)_(_KIND_LABEL2)
#else
#  define _LABEL2 _(_TYPE_LABEL2)_(_KIND_LABEL2)
#endif
#define _UNARY_OP(X) _CAT2(X,_LABEL)
#define _BINARY_OP(X) _CAT3(_LABEL1,X,_LABEL2)
