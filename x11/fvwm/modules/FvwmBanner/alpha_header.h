/**************************************************************************/
/* If I do ALL this, I can compile OK with -Wall -Wstrict-prototypes on the
 * alpha's */
#include <sys/types.h>
#include <sys/time.h>


extern int select(int, fd_set *, fd_set *, fd_set *, struct timeval *);

/* stdio things */
extern int mkstemp(char *);

/* string manipulation */
extern size_t strlen(char *);

extern int bzero(char *, int);
extern int gethostname (char *, int);
/**************************************************************************/

