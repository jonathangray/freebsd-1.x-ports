/*
** Random stuff needed for nntpxmit
**
** This file also contains a lot of assumptions about what features
** are available on the local system - if something is not working
** to your liking, look them over carefully.
*/

#ifdef MMAP
#include <sys/mman.h>
#include <sys/stat.h>
#endif MMAP

typedef	SIGRET	(*ifunp)();	/* pointer to function that returns */
				/* whatever signal() returns */

#define	dprintf	if (Debug) fprintf

#ifdef TIMEOUT
#undef TIMEOUT
#endif
#define	TIMEOUT	3600		/* seconds to read timeout in sfgets */

#ifndef	TRUE
#define	TRUE	1
#define	FALSE	0
#endif

/* in goodbye() wait (or not) for QUIT response */
#define	WAIT		TRUE
#define	DONT_WAIT	FALSE

/* in lockfd(), blocking, or non_blocking */
#define	BLOCK		FALSE
#define	DONT_BLOCK	TRUE

#ifndef FAIL
#define	FAIL		(-1)
#endif


/* DECNET support is only there if the DECNET compile-time option defined */
#define	T_IP_TCP	1	/* transport is IP/TCP */
#define	T_DECNET	2	/* transport is DECNET */
#define	T_FD		3	/* transport is a descriptor */

/* for syslog, if we compile it in */
#define	L_DEBUG		1
#define	L_INFO		2
#define	L_NOTICE	3
#define	L_WARNING	4


#ifdef	BSD_42			/* look at all these goodies we get! */
#define	FTRUNCATE
#define	RELSIG
#endif	BSD4_2

