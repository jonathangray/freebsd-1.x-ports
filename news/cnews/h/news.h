/*
 * definitions unique to all of C news
 * things marked with qqq are subject to being configured by "build"
 */

/*
 * tunable parameters
 * which actually very seldom need to be tuned
 * in particular, don't get alarmed about MAXCOMP, it's not used for
 *  anything where it matters
 */
#define MAXPATH 1024		/* max. length of pwd output */
#define MAXCOMP 14		/* file name component length */
#define MAXHOST 128		/* max. length of this host's name */
#define SPOOLTMP ".tmpXXXXXX"	/* template for NEWSARTS temporary link */


/* STATIC & FORWARD must agree to avoid redeclarations(!) */
#ifndef STATIC
#define STATIC	static		/* "static" when not debugging|profiling */
#endif

/* adapt to compiler limitations */
#ifndef FORWARD
#ifdef pdp11
#define FORWARD			/* "static" except for dmr's 11 compiler */
#else
#define FORWARD static		/* "static" except for dmr's 11 compiler */
#endif
#endif
#ifdef NOVOID
#define void int		/* if your compiler doesn't understand void's */
#endif
#ifdef NOUNSLONG
#define MAXLONG 017777777777L	/* if your compiler lacks "unsigned long" type */
#endif

/* fundamental constants of the implementation */
#define SMALLMEM	/* qqq for PDP-11s, PDP-8s, IBM PCs, etc. */
#define	FASTSTRCHR	/* qqq if string functions are very fast */

/* automatic configuration */
#ifdef pdp11
#ifndef SMALLMEM
#define SMALLMEM
#endif				/* SMALLMEM */
#endif				/* pdp11 */


/* types */
typedef short statust;
typedef char boolean;

/* status bits */
#define ST_OKAY		0	/* nothing wrong */
#define ST_SHORT	(1<<1)	/* article shorter than byte count; truncated? */
#define ST_ACCESS	(1<<2)	/* no access permission */
#define ST_REFUSED	(1<<3)	/* article was deliberately refused - OK */
#define ST_DROPPED	(1<<4)	/* article was accidentally dropped */
#define ST_DISKFULL	(1<<5)	/* disk full - give up */
#define ST_JUNKED	(1<<6)	/* article was accepted, but junked */
#define ST_NEEDATTN	(1<<7)	/* news system needs human attention - give up */

/* newsgroup specific definitions */
#define NGSEP ','		/* separates groups */
#define NGNEG '!'		/* preceding a pattern, negates it */
#define NGDELIM '.'		/* within a group */
#define SNGDELIM "."		/* string of NGDELIM */
#define FNDELIM '/'		/* within a group, on disk */
#define SFNDELIM "/"		/* string of FNDELIM */

/* macros, replacing functions for speed */
#define max(a,b) ((a) > (b)? (a): (b))
#define min(a,b) ((a) < (b)? (a): (b))
#define iswhite(c) ((c) == ' ' || (c) == '\t')
/* STREQ is an optimised strcmp(a,b)==0 */
#define STREQ(a, b) ((a)[0] == (b)[0] && strcmp(a, b) == 0)
/* STREQN is an optimised strncmp(a,b,n)==0; assumes n > 0 */
#define STREQN(a, b, n) ((a)[0] == (b)[0] && strncmp(a, b, n) == 0)
#define STRLEN(s) ((unsigned)sizeof(s) - 1)	/* s must be a char array */
#ifdef FASTSTRCHR
#define STRCHR(src, chr, dest) (dest) = strchr(src, chr)
#else
#define STRCHR(src, chr, dest) \
	for ((dest) = (src); *(dest) != '\0' && *(dest) != (chr); ++(dest)) \
		; \
	if (*(dest) == '\0') \
		(dest) = NULL		/* N.B.: missing semi-colon */
#endif

/* macros, of necessity */
/* nnafree(any **) where "any" is any type; must be a macro */
#define nnafree(mempp) (*(mempp) != 0? (free((char *)*(mempp)), (*(mempp) = 0)): 0)
#ifdef lint
static
nnfree(mempp)		/* If *mempp is non-null, free it and zero it. */
register char **mempp;			/* pointer to malloc'ed ptr. */
{
	if (*mempp != 0) {
		free(*mempp);
		*mempp = 0;
	}
}
#else					/* lint */
#define nnfree nnafree
#endif					/* lint */

#define YES 1
#define NO 0

#define SIZENUL (sizeof(char))		/* size in bytes of an ASCII NUL byte */

#define NOTALLHDRS NO			/* hdrdump flags for "all headers seen?" */
#define ALLHDRS YES

#define DEFEXP "-"			/* default expiry period */

/* imports from news */
extern char *progname;

extern void fclsexec();				/* from ../libos */
extern FILE *fopenexcl();			/* from ../libos */
extern char *getcwd();				/* from ../libos */

extern FILE *fopenclex(), *fopenwclex();	/* from ../libcnews/fopenclex.c */
extern char *gethdr();				/* from ../libcnews/gethdr.c */
extern char *hostname();			/* from ../libcnews/hostname.c */
extern void lockdebug(), newslock(), newsunlock();	/* from ../libcnews/lock.c */
extern void errunlock();			/* from ../libcnews/lock.c */
extern int ltozan(), ltoza();			/* from ../libcnews/ltoza.c */
extern void matchdebug();			/* from ../libcnews/ngmatch.c */
extern boolean ngmatch();			/* from ../libcnews/ngmatch.c */
extern void mkfilenm();				/* from ../libcnews/string.c */
extern char *trim();				/* from ../libcnews/string.c */
extern boolean anyhostin(), hostin();		/* from ../libcnews/string.c */
extern int hopcount();				/* from ../libcnews/string.c */
extern char *skipsp(), *first(), *strsvto();	/* from ../libcnews/string.c */
extern char *sendersite(), *nullify();		/* from ../libcnews/string.c */
extern char *canonpath();			/* from ../libcnews/string.c */
extern void timestamp();			/* from ../libcnews/time.c */

extern void warning(), error();			/* from ../libc */
extern void closeall();				/* from ../libc */
extern void stdfdopen();			/* from ../libc */
extern int nfclose();				/* from ../libc */

#include "alloc.h"				/* ugh */
