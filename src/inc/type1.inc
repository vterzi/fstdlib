#if _TYPE_IDS1 & _LOGICAL
#  define _TYPE_ID1 _LOGICAL
#  define _TYPE1 logical
#  define _TYPE_LABEL1 L
#  define _DEFAULT_KIND1 _LK
#  include "../inc/type2.inc"
#  undef _TYPE_ID1
#  undef _TYPE1
#  undef _TYPE_LABEL1
#  undef _DEFAULT_KIND1
#endif
#if _TYPE_IDS1 & _INTEGER
#  define _TYPE_ID1 _INTEGER
#  define _TYPE1 integer
#  define _TYPE_LABEL1 I
#  define _DEFAULT_KIND1 _IK
#  include "../inc/type2.inc"
#  undef _TYPE_ID1
#  undef _TYPE1
#  undef _TYPE_LABEL1
#  undef _DEFAULT_KIND1
#endif
#if _TYPE_IDS1 & _REAL
#  define _TYPE_ID1 _REAL
#  define _TYPE1 real
#  define _TYPE_LABEL1 R
#  define _DEFAULT_KIND1 _RK
#  include "../inc/type2.inc"
#  undef _TYPE_ID1
#  undef _TYPE1
#  undef _TYPE_LABEL1
#  undef _DEFAULT_KIND1
#endif
#if _TYPE_IDS1 & _COMPLEX
#  define _TYPE_ID1 _COMPLEX
#  define _TYPE1 complex
#  define _TYPE_LABEL1 C
#  define _DEFAULT_KIND1 _CK
#  include "../inc/type2.inc"
#  undef _TYPE_ID1
#  undef _TYPE1
#  undef _TYPE_LABEL1
#  undef _DEFAULT_KIND1
#endif
#if _TYPE_IDS1 & _CHARACTER
#  define _TYPE_ID1 _CHARACTER
#  define _TYPE1 character
#  define _TYPE_LABEL1 S
#  define _DEFAULT_KIND1 _SK
#  include "../inc/type2.inc"
#  undef _TYPE_ID1
#  undef _TYPE1
#  undef _TYPE_LABEL1
#  undef _DEFAULT_KIND1
#endif
