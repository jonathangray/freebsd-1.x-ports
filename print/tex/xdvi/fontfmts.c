#include "xdvi.h"

#ifndef	X_NOT_STDC_ENV
#include <stdlib.h>
#endif

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
