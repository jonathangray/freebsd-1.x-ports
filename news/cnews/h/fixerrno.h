/* attempt to supply a declaration of errno in case <errno.h> doesn't */

#ifndef __STDC__
extern int errno;
#endif
