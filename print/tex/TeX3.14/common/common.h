/* common.h: Definitions and declarations common both to the change
   files and to web2c itself.  */

#ifndef COMMON_H
#define COMMON_H

/* pltotf et al. use the symbol `index' themselves; we don't want to
   redefine it in those cases (and those programs don't use the other
   string functions, fortunately).  */ 
#ifndef index
#ifndef	BSD
#include <string.h>
#define index strchr
#define rindex strrchr
#else /* BSD */
#include <strings.h>
#endif /* BSD */
#endif /* not index */

extern char *getenv (), *rindex ();
extern double atof ();


/* Global constants.  */
#define true 1
#define false 0

#define TRUE 1
#define FALSE 0


#endif /* not COMMON_H */
