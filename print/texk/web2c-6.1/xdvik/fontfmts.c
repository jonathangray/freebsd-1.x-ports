#include "config.h"

/***
 ***	Font reading routines are read into this file.
 ***/

#ifdef	USE_PK
#include "pk.c"
#endif

#ifdef	USE_GF
#include "gf.c"
#endif

#ifdef	USE_PXL
#include "pxl.c"
#endif
