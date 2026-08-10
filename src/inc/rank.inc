#undef _RANK_LABEL
#define _RANK 0
#define _RANK_LABEL
#include "../inc/type.inc"
#undef _RANK
#undef _RANK_LABEL
#define _RANK_LABEL _RANK
#if _MAX_RANK >= 1
#  define _RANK 1
#  include "../inc/type.inc"
#  undef _RANK
#endif
#if _MAX_RANK >= 2
#  define _RANK 2
#  include "../inc/type.inc"
#  undef _RANK
#endif
#if _MAX_RANK >= 3
#  define _RANK 3
#  include "../inc/type.inc"
#  undef _RANK
#endif
#if _MAX_RANK >= 4
#  define _RANK 4
#  include "../inc/type.inc"
#  undef _RANK
#endif
#if _MAX_RANK >= 5
#  define _RANK 5
#  include "../inc/type.inc"
#  undef _RANK
#endif
#if _MAX_RANK >= 6
#  define _RANK 6
#  include "../inc/type.inc"
#  undef _RANK
#endif
#if _MAX_RANK >= 7
#  define _RANK 7
#  include "../inc/type.inc"
#  undef _RANK
#endif
#if _MAX_RANK >= 8
#  define _RANK 8
#  include "../inc/type.inc"
#  undef _RANK
#endif
#if _MAX_RANK >= 9
#  define _RANK 9
#  include "../inc/type.inc"
#  undef _RANK
#endif
#if _MAX_RANK >= 10
#  define _RANK 10
#  include "../inc/type.inc"
#  undef _RANK
#endif
#if _MAX_RANK >= 11
#  define _RANK 11
#  include "../inc/type.inc"
#  undef _RANK
#endif
#if _MAX_RANK >= 12
#  define _RANK 12
#  include "../inc/type.inc"
#  undef _RANK
#endif
#if _MAX_RANK >= 13
#  define _RANK 13
#  include "../inc/type.inc"
#  undef _RANK
#endif
#if _MAX_RANK >= 14
#  define _RANK 14
#  include "../inc/type.inc"
#  undef _RANK
#endif
#if _MAX_RANK >= 15
#  define _RANK 15
#  include "../inc/type.inc"
#  undef _RANK
#endif
#undef _RANK_LABEL
