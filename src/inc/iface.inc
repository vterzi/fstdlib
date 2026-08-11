#ifndef _IFACE
#  define _IFACE _OP
#endif
#ifndef _NO_PUBLIC
public :: _IFACE
#endif
interface _IFACE
#ifdef _SKIP_SAME
#  undef _SKIP_SAME
#  define _FILE "../inc/skipsame.inc"
#  define _SUBFILE "../inc/decl.inc"
#else
#  define _FILE "../inc/decl.inc"
#endif
#include "../inc/types.inc"
#undef _FILE
#undef _SUBFILE
end interface _IFACE
#undef _IFACE
#undef _OP
#undef _NO_PUBLIC
