/* expIcomm.h - private symbols common to both expect program and library

Written by: Don Libes, libes@cme.nist.gov, NIST, 12/3/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.
*/

#ifndef _EXPECT_INT_H
#define _EXPECT_INT_H

#ifndef TRUE
#define FALSE 0
#define TRUE 1
#endif

#ifdef NO_MEMCPY
#define memcpy(x,y,len) bcopy(y,x,len)
#endif

extern int errno;

int		Exp_StringMatch();
int		Exp_StringMatch2();
void		exp_console_set _ANSI_ARGS_((void));

#endif /* _EXPECT_INT_H */
