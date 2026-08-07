#if _TYPE_IDS & _LOGICAL
#  define _TYPE_ID _LOGICAL
#  define _TYPE logical
#  define _TYPE_LABEL L
#  define _DEFAULT_KIND _LK
#  include "../inc/kind.inc"
#  undef _TYPE_ID
#  undef _TYPE
#  undef _TYPE_LABEL
#  undef _DEFAULT_KIND
#endif
#if _TYPE_IDS & _INTEGER
#  define _TYPE_ID _INTEGER
#  define _TYPE integer
#  define _TYPE_LABEL I
#  define _DEFAULT_KIND _IK
#  include "../inc/kind.inc"
#  undef _TYPE_ID
#  undef _TYPE
#  undef _TYPE_LABEL
#  undef _DEFAULT_KIND
#endif
#if _TYPE_IDS & _REAL
#  define _TYPE_ID _REAL
#  define _TYPE real
#  define _TYPE_LABEL R
#  define _DEFAULT_KIND _RK
#  include "../inc/kind.inc"
#  undef _TYPE_ID
#  undef _TYPE
#  undef _TYPE_LABEL
#  undef _DEFAULT_KIND
#endif
#if _TYPE_IDS & _COMPLEX
#  define _TYPE_ID _COMPLEX
#  define _TYPE complex
#  define _TYPE_LABEL C
#  define _DEFAULT_KIND _CK
#  include "../inc/kind.inc"
#  undef _TYPE_ID
#  undef _TYPE
#  undef _TYPE_LABEL
#  undef _DEFAULT_KIND
#endif
#if _TYPE_IDS & _CHARACTER
#  define _TYPE_ID _CHARACTER
#  define _TYPE character
#  define _TYPE_LABEL S
#  define _DEFAULT_KIND _SK
#  include "../inc/kind.inc"
#  undef _TYPE_ID
#  undef _TYPE
#  undef _TYPE_LABEL
#  undef _DEFAULT_KIND
#endif
