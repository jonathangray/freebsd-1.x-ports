/*
 * slurp.h - common definitions for slurp
 *
 * Copyright (C) 1992/93 Stephen Hebditch. All rights reserved.
 * TQM Communications, BCM Box 225, London, WC1N 3XX.
 * steveh@orbital.demon.co.uk  +44 836 825962
 *
 * See README for more information and disclaimers
 *
 * $Id: slurp.h,v 1.1 1993/08/27 02:47:48 alm Exp $
 *
 * $Log: slurp.h,v $
 * Revision 1.1  1993/08/27 02:47:48  alm
 * Initial revision
 *
 * Revision 1.7  1993/06/07  11:17:47  root
 * Removed bcopy/bzero definitions and used ANSI memcpy and memset
 * functions throughout the code instead.
 *
 * Revision 1.6  1993/04/22  18:10:09  root
 * Corrected function prototype of server_time - should have been
 * time_t instead of long.
 * Made <unistd.h> a compilation option.
 *
 * Revision 1.5  1993/03/01  17:57:44  root
 * Minor reshuffle, plus USG definition now SYSV.
 *
 * Revision 1.4  1993/02/14  14:40:55  root
 * Added no_id_load_flag.
 * Modified struct mnode to include used flag and remove msgid.
 * New process_id and set_ntime definitions.
 *
 * Revision 1.3  1992/12/15
 * Added SYS_ERRLIST definitions.
 *
 * Revision 1.1  1992/12/06
 * Added no_time flag.
 *
 * Revision 1.0  1992/10/29
 * Initial coding.
 *
 */

/* Local header files */

#include "conf.h"
#include "nntp.h"


/* Standard header files */

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <ctype.h>
#include <errno.h>

#ifdef SYSLOG
  #ifdef FAKESYSLOG
    #include "fakesyslog.h"
  #else
    #include <syslog.h>
  #endif
#endif

#ifdef UNISTD
  #include <unistd.h>
#endif

#ifdef SYSV
  #include <string.h>
  #include <time.h>
#else
  #include <strings.h>
  #include <sys/time.h>
#endif

#ifdef SYS_ERRLIST
  extern const char *sys_errlist[];
  #define strerror(x) (sys_errlist[x])
#endif


/* Important variables */

extern char *hostname;		/* Name of current NNTP server host */
extern char *pname;			/* Name of this program */
extern int  debug_flag;		/* Write extra debugging output to screen */
extern int  no_time_flag;	/* Don't update slurp.<hostname> */
extern int  no_id_load_flag;/* Don't dump / load message ids */

/* Article counters */

extern int  dupart;			/* Number of duplicate articles */
extern int  misart;			/* Number of missing articles */
extern int  newart;			/* Number of new articles */

extern long totalsize;		/* Total size of articles tranferred */

/* Details for NEWNEWS */

extern char *nn_newsgroups;
extern char *nn_time;
extern char *nn_distributions;

/* Binary tree holding message ids */

struct mnode
	{
	struct mnode *left;
	struct mnode *right;
	char *msgid;
	int used;
	};
                          
extern struct mnode *root;
extern int entries;


/* Slurp function prototypes */

extern void get_articles ();						/* articles.c */
extern void enqueue_batch ();

extern int  open_history ();						/* history.c */
extern void close_history ();
extern int  check_id (char *message_id);

extern void log_ret (const char *fmt, ...);			/* misc.c */
extern void log_sys (const char *fmt, ...);
extern void log_msg (const char *fmt, ...);
extern char *stradd (const char *, const char *);

extern void get_ids ();								/* newnews.c */
extern void process_id (char *msgid);

extern void set_ntime ();							/* slurp.c */

extern int  tcp_open (char *host, char *service);	/* sockets.c */
extern int  server_init (char *hostname);
extern void close_server ();
extern void get_server (char *buf, int size);
extern void put_server (char *buf);

extern int  space (int min_free);					/* space.c */

extern time_t server_time ();						/* time.c */


/* The inevitable... */

#if !defined (TRUE) || ((TRUE) != 1)
  #define TRUE (1)
#endif

#if !defined (FALSE) || ((FALSE) != 0)
  #define FALSE (0)
#endif

#ifndef PATH_MAX
  #define PATH_MAX 1024
#endif

/* END-OF-FILE */
