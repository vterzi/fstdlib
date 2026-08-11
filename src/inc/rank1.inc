#define _RANK1 0
#define _RANK_LABEL1
#include "../inc/rank2.inc"
#undef _RANK1
#undef _RANK_LABEL1
#define _RANK_LABEL1 _RANK1
#if _MAX_RANK >= 1
#  define _RANK1 1
#  include "../inc/rank2.inc"
#  undef _RANK1
#endif
#if _MAX_RANK >= 2
#  define _RANK1 2
#  include "../inc/rank2.inc"
#  undef _RANK1
#endif
#if _MAX_RANK >= 3
#  define _RANK1 3
#  include "../inc/rank2.inc"
#  undef _RANK1
#endif
#if _MAX_RANK >= 4
#  define _RANK1 4
#  include "../inc/rank2.inc"
#  undef _RANK1
#endif
#if _MAX_RANK >= 5
#  define _RANK1 5
#  include "../inc/rank2.inc"
#  undef _RANK1
#endif
#if _MAX_RANK >= 6
#  define _RANK1 6
#  include "../inc/rank2.inc"
#  undef _RANK1
#endif
#if _MAX_RANK >= 7
#  define _RANK1 7
#  include "../inc/rank2.inc"
#  undef _RANK1
#endif
#if _MAX_RANK >= 8
#  define _RANK1 8
#  include "../inc/rank2.inc"
#  undef _RANK1
#endif
#if _MAX_RANK >= 9
#  define _RANK1 9
#  include "../inc/rank2.inc"
#  undef _RANK1
#endif
#if _MAX_RANK >= 10
#  define _RANK1 10
#  include "../inc/rank2.inc"
#  undef _RANK1
#endif
#if _MAX_RANK >= 11
#  define _RANK1 11
#  include "../inc/rank2.inc"
#  undef _RANK1
#endif
#if _MAX_RANK >= 12
#  define _RANK1 12
#  include "../inc/rank2.inc"
#  undef _RANK1
#endif
#if _MAX_RANK >= 13
#  define _RANK1 13
#  include "../inc/rank2.inc"
#  undef _RANK1
#endif
#if _MAX_RANK >= 14
#  define _RANK1 14
#  include "../inc/rank2.inc"
#  undef _RANK1
#endif
#if _MAX_RANK >= 15
#  define _RANK1 15
#  include "../inc/rank2.inc"
#  undef _RANK1
#endif
#undef _RANK_LABEL1
