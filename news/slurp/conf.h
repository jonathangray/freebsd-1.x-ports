/*
 * conf.h - compilation options for slurp
 *
 * Copyright (C) 1992/93 Stephen Hebditch. All rights reserved.
 * TQM Communications, BCM Box 225, London, WC1N 3XX.
 * steveh@orbital.demon.co.uk  +44 836 825962
 *
 * See README for more information and disclaimers
 *
 * $Id: conf.h,v 1.1 1993/08/27 02:47:48 alm Exp $
 *
 * $Log: conf.h,v $
 * Revision 1.1  1993/08/27 02:47:48  alm
 * Initial revision
 *
 * Revision 1.8  1993/08/20  10:36:02  root
 * Removed SIGRET definition as unused.
 *
 * Revision 1.7  1993/06/23  10:22:38  root
 * Removed NOBUFFOUT option.
 * Amended description for DBM, NDBM and DBZ as can now have none
 * defined if don't want to do history lookups.
 * Amended RNEWS description to describe how to send batches via UUCP.
 * Upped MAXCACHE value and improved description.
 *
 * Revision 1.6  1993/04/22  18:28:01  root
 * Added new compilation options for NOBUFFOUT and UNISTD.
 *
 * Revision 1.5  1993/03/01  17:41:09  root
 * Changed USG definition to SYSV, now used solely in slurp.h.
 * Added explanation of defines for space.c.
 *
 * Revision 1.4  1993/02/14  14:36:16  root
 * Removed BATCHARTSMAX, MSGIDMAX.
 * Added INDIR, BATCHNAME.
 * Modified TIMFILE, RNEWS.
 *
 * Revision 1.0  1992/10/29
 * Initial coding.
 *
 */


/* SLURP CONFIGURATION
   =================== */

/* Set these to the location of the slurp configuration files - NEWSLIB
   is usually a good place for them. The hostname will be added to the
   end of TIMFILE. */

#define SYSFILE			"/usr/lib/news/slurp.sys"
#define TIMFILE			"/usr/lib/news/slurp."


/* If SPEEDUP is defined then there will always be one ARTICLE request
   stacked up at the NNTP server before the current article has
   finished being received. Although not strictly conforming to the
   standard this allows a much greater article throughput. */

#define SPEEDUP


/* If defined then the number of characters per second transferred
   during the article request phase will be logged. */

#define SPEEDSTATS


/* MAXCACHE sets the maximum number of articles that may be requested
   in any one session. If this is exceeded then it noted in the log and
   the new time for the next NEWNEWS will not be written. If you are
   transferring a large amount of news then you will probably need to
   increase this value. */

#define MAXCACHE		8192


/* NEWS CONFIGURATION
   ================== */

/* The location of the news history file. */

#define HISTORY_FILE    "/usr/lib/news/history"


/* The database format used by the news history file. For modern C News
   and INN this will almost certainly be DBZ. If none of these are
   defined then history lookups will not take place. */

#define DBZ
#undef	DBM
#undef	NDBM


/* The location of the rnews program - or any other program you wish to
   receive batches on stdin. If defined, slurp will pipe batches of
   articles to this program rather than creating batch files in INDIR */

#define RNEWS		"/usr/local/bin/rnews" /* */
/* #define RNEWS		"compress | uux - -r -glow spooky!rnews" /* */


/* The place where incoming batches will be placed if RNEWS is not
   defined. This is usually /usr/spool/news/in.coming for C News and
   /var/spool/rnews for INN. */

#define INDIR			"/var/spool/news/in.coming"


/* The optimum size of a news batch. Normally this can be left at
   300000L.  If you are using INN and have RNEWS defined, then set this
   to 0 so the pipe to rnews (and the socket to innd) is kept open
   throughout the session. */

#define BATCHSIZEMAX	300000L


/* The location of the spool directory holding news articles. This is
   needed so we can check if there is enough space before starting to
   build up another batch of articles. */

#define SPOOLDIR        "/var/spool/news"


/* If less then MINFREE blocks are available on the disk containing
   SPOOLDIR or less than MINFILES inodes are available then slurp will
   be aborted. If MINFREE is not defined then a space check is not
   carried out. */

#define MINFREE 4000
/* #define MINFILES 1000 /* */
                        

/* UNIX VERSION CONFIGURATION
   ========================== */

/* Define SYSV if you are running a System V derivative and need
   <string.h>, <time.h> & <fcntl.h>. If it is undefined, then you're
   BSDish and need <strings.h> & <sys/time.h>. */

#undef SYSV


/* You may need additional defines to use the correct method of
   determining how much disk space there is on the file system
   containing your news spool. space.c understands defines for SVR4,
   SVR3, sun, hpux, pyr, hp300, __NeXT__, linux, apollo, ultrix,
   __bsdi__ and CMU_MACH. For other systems you may need to amend
   space.c. */

#undef SVR4


/* Define if your system has <unistd.h> */

#define UNISTD


/* Define if your system has extern char sys_errlist[], but no strerror() */

#undef SYS_ERRLIST


/* The facility name which syslog reports errors and stats under. If
   you don't want to use syslog for reporting then undefine and the
   information will be written to stderr instead. */

#define SYSLOG			LOG_NEWS


/* If you don't have syslog then you can use the supplied fakesyslog
   instead. Define FAKESYSLOG to be the name of the file to contain the
   log. If your host supports the BSD fdopen() function and the
   O_APPEND flag to open(), you should define FAKEAPPEND with
   FAKESYSLOG so that multiple copies of nntpd don't trash the log with
   buffered fprintfs. */
   
/* #define FAKESYSLOG      "/usr/lib/news/nntplog" /* */
/* #define FAKEAPPEND /* */


/* STUFF BELOW PROBABLY DOESN'T NEED ALTERING
   ========================================== */

#define COPYSIZE		16384	/* Articles bigger than this size will
                                   be written to a temporary file while
                                   they are being retrieved, rather than
                                   stored in memory */

#define TIMEOUT			60 * 10	/* Max time to wait for a line from the
                                   server. */

#define BATCHNAME		"nntp.XXXXXX"	/* Temporary filename for NNTP
										   batch */

/* END-OF-FILE */
