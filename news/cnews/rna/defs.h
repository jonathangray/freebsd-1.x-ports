#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#ifdef USG
#include <fcntl.h>
#endif
#include <signal.h>

/* C News header(s) */
#include <config.h>

#define NEWSVERSION	 "B UNSW 1.1 19 Sep 1984, heavily hacked for C News"

/* Things that very well may require local configuration */

/*#define UNSWMAIL 1*/			/* if you have UNSW "mail" which
					   allows "-s subject -i include_file"
					   arguments */
#define MAIL	"/bin/mail"
#if UNSWMAIL
#define FASTMAIL	"/bin/mail"
#else
#define FASTMAIL	MAIL
#endif


/* Things you might want to change */

/* #define MANGRPS	 1		/* if you have mandatory subscriptions
					   tailored per-person (uses
					   getclasses()) */
#define NEWSRC  ".newsrc"		/* name of .newsrc file */
#define	PAGESIZE 24			/* lines on screen */
#define ARTICLES "articles"		/* default place to save articles */
#define BUFLEN	256			/* standard buffer size */

/* Things you probably won't want to change */

#define NEGCHAR	'!'			/* newsgroup negation character	*/
#define NEGS	"!"			/* ditto (string) */
#define BADGRPCHARS "/#!"		/* illegal chars in group name */
#define	NGSEPCHAR ','	/* delimit character in news group line		*/
#define NGSEPS	","	/* ditto */
#define PSEPS "!"	/* separator in Path: */
#define PSEPCHAR '!'	/* ditto */
#define TRUE	1
#define FALSE	0

#ifndef F_SETFD
#ifdef F_SETFL
#define F_SETFD F_SETFL		/* SETFL becomes SETFD (close on exec arg
				   to fcntl) */
#endif
#endif

typedef enum booltype { false = 0, true } bool;
typedef enum applytype { stop, next, nextgroup, searchgroup } applycom;
typedef applycom (*apcmfunc)();
typedef enum pheadtype { printing, passing, making } pheadcom;

/*
 * header structure
 */
typedef struct header {
	/* mandatory fields */
	char	*h_relayversion;
	char	*h_postversion;
	char	*h_from;
	char	*h_date;
	char	*h_newsgroups;
	char	*h_subject;
	char	*h_messageid;
	char	*h_path;
	/* optional fields */
	char	*h_replyto;
	char	*h_sender;
	char	*h_followupto;
	char	*h_datereceived;
	char	*h_expires;
	char	*h_references;
	char	*h_control;
	char	*h_distribution;
	char	*h_organisation;
	char	*h_lines;
	/* any we don't recognise */
	char	*h_others;
} header;

/*
 * internal structure for active file
 */
typedef struct active active;
struct active {
	char	*a_name;
	long	a_seq;
	long	a_low;
	active	*a_next;
};

/*
 * internal struct for newsrc file
 */
typedef struct newsrc newsrc;
struct newsrc {
	char	*n_name;
	bool	n_subscribe;
	long	n_last;
	newsrc	*n_next;
};

/* some of these may not exist any more */
char	*strrchr(), *strchr(), *strcat(), *strcpy(), *strpbrk();
char	*itoa(), *ltoa(), *convg(), *ngsquash(), *ttoa(), *mgets(), *rconvg();
char	*newstr(), *newstr2(), *newstr3(), *newstr4(), *newstr5(), *catstr();
char	*catstr2(), *bsearch(), *mtempnam(), *newstr6();
char	*getunique(), *getretaddr(), *getsubject();
FILE	*fopenl(), *fopenf();
char	*myalloc(), *myrealloc();
long	time(), atol(), atot();
int	strpcmp();
active	*readactive();
char *getenv();

#define NIL(type)	((type *) 0)
#define NEW(type)	((type *) myalloc(sizeof(type)))
#define CMP(a, b)	(*(a) != *(b) ? *(a) - *(b) : strcmp(a, b))
#define CMPN(a, b, n)	(*(a) != *(b) ? *(a) - *(b) : strncmp(a, b, n))

extern char mailvia[];
