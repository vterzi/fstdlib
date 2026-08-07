#if _TYPE_IDS2 & _LOGICAL
#  define _TYPE_ID2 _LOGICAL
#  define _TYPE2 logical
#  define _TYPE_LABEL2 L
#  define _DEFAULT_KIND2 _LK
#  include "../inc/kind1.inc"
#  undef _TYPE_ID2
#  undef _TYPE2
#  undef _TYPE_LABEL2
#  undef _DEFAULT_KIND2
#endif
#if _TYPE_IDS2 & _INTEGER
#  define _TYPE_ID2 _INTEGER
#  define _TYPE2 integer
#  define _TYPE_LABEL2 I
#  define _DEFAULT_KIND2 _IK
#  include "../inc/kind1.inc"
#  undef _TYPE_ID2
#  undef _TYPE2
#  undef _TYPE_LABEL2
#  undef _DEFAULT_KIND2
#endif
#if _TYPE_IDS2 & _REAL
#  define _TYPE_ID2 _REAL
#  define _TYPE2 real
#  define _TYPE_LABEL2 R
#  define _DEFAULT_KIND2 _RK
#  include "../inc/kind1.inc"
#  undef _TYPE_ID2
#  undef _TYPE2
#  undef _TYPE_LABEL2
#  undef _DEFAULT_KIND2
#endif
#if _TYPE_IDS2 & _COMPLEX
#  define _TYPE_ID2 _COMPLEX
#  define _TYPE2 complex
#  define _TYPE_LABEL2 C
#  define _DEFAULT_KIND2 _CK
#  include "../inc/kind1.inc"
#  undef _TYPE_ID2
#  undef _TYPE2
#  undef _TYPE_LABEL2
#  undef _DEFAULT_KIND2
#endif
#if _TYPE_IDS2 & _CHARACTER
#  define _TYPE_ID2 _CHARACTER
#  define _TYPE2 character
#  define _TYPE_LABEL2 S
#  define _DEFAULT_KIND2 _SK
#  include "../inc/kind1.inc"
#  undef _TYPE_ID2
#  undef _TYPE2
#  undef _TYPE_LABEL2
#  undef _DEFAULT_KIND2
#endif
