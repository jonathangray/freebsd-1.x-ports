/* expectcomm.h - public symbols common to both expect.h and expect_tcl.h

Written by: Don Libes, libes@cme.nist.gov, NIST, 12/3/90

Design and implementation of this program was paid for by U.S. tax
dollars.  Therefore it is public domain.  However, the author and NIST
would appreciate credit if this program or parts of it are used.
*/

#ifndef _EXPECT_COMM_H
#define _EXPECT_COMM_H

#include "expect_cf.h"

#include <stdio.h>
#include <setjmp.h>
#include "tcl.h"
#include "tclRegexp.h"

/* support for Standard C and C++ prototypes */
#ifdef __cplusplus
#  define EXP_PROTOTYPES
#  define EXP_VARARGS	...
#else
#  define EXP_VARARGS	, ...
#  ifdef __STDC__
#    define EXP_PROTOTYPES
#  endif /*__STDC__*/
#endif /*__cplusplus*/

#ifdef EXP_PROTOTYPES
#define EXP_PROTO(x)	x
#ifdef EXP_DEFINE_FNS
/* when functions are really being defined, we have to use va_alist as arg */
#define EXP_PROTOV(x)	va_alist
#else
#define EXP_PROTOV(x)	x
#endif /*EXP_DEFINE_FNS*/
#else
#define EXP_PROTO(x)	()
#define EXP_PROTOV(x)	()
#endif /*EXP_PROTOTYPES*/

/* Put double parens around macro args so they all look like a single arg */
/* to preprocessor.  That way, don't need a different macro for functions */
/* with a different number of arguments. */

EXTERN int errno;

#ifdef NO_STDLIB_H
#include "compat/stdlib.h"
#else
#include <stdlib.h>		/* for malloc */
#endif /*NO_STDLIB_H*/

/* common return codes for Expect functions */
/* The library actually only uses TIMEOUT and EOF */
#define EXP_ABEOF	-1	/* abnormal eof in Expect */
				/* when in library, this define is not used. */
				/* Instead "-1" is used literally in the */
				/* usual sense to check errors in system */
				/* calls */
#define EXP_TIMEOUT	-2
#define EXP_TCLERROR	-3
#define EXP_FULLBUFFER	-5
#define EXP_MATCH	-6
#define EXP_NOMATCH	-7
#define EXP_CANTMATCH	EXP_NOMATCH
#define EXP_CANMATCH	-8
#define EXP_DATA_NEW	-9	/* if select says there is new data */
#define EXP_DATA_OLD	-10	/* if we already read data in another cmd */
#define EXP_EOF		-11

/* in the unlikely event that a signal handler forces us to return this */
/* through expect's read() routine, we temporarily convert it to this. */
#define EXP_TCLRET	-20
#define EXP_TCLCNT	-21
#define EXP_TCLBRK	-22
#define EXP_TCLCNTEXP	-23
#define EXP_TCLRETTCL	-24

/* yet more TCL return codes */
/* Tcl does not safely provide a way to define the values of these, so */
/* use ridiculously numbers for safety */
#define EXP_CONTINUE		-101	/* continue expect command itself */
#define EXP_TCL_RETURN		-102	/* converted by interact, interpeter */
					/* from inter_return into TCL_RETURN*/

#define EXP_TIME_INFINITY	-1
#define EXP_SPAWN_ID_BAD	-1

EXTERN int exp_is_debugging;
EXTERN int exp_loguser;
EXTERN int exp_disconnected;		/* proc. disc'd from controlling tty */

EXTERN void (*exp_close_in_child)();	/* procedure to close files in child */
EXTERN void exp_close_tcl_files();	/* deflt proc: close all Tcl's files */

EXTERN void exp_slave_control _ANSI_ARGS_((int,int));

#endif /* _EXPECT_COMM_H */
