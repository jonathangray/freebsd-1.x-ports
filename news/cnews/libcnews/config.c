/*
 * news configuration inquiry
 */

#include <stdio.h>
#include <sys/types.h>
#include "libc.h"
#include "news.h"
#include "config.h"

#ifndef NULL
#define	NULL	0
#endif

#ifndef NEWSCTL
/* =()<#define	NEWSCTL	"@<NEWSCTL>@">()= */
#define	NEWSCTL	"/usr/lib/news"
#endif
#ifndef NEWSPATH
/* =()<#define	NEWSPATH	"@<NEWSPATH>@">()= */
#define	NEWSPATH	"/bin:/usr/bin"
#endif
#ifndef NEWSARTS
/* =()<#define	NEWSARTS	"@<NEWSARTS>@">()= */
#define	NEWSARTS	"/usr/spool/news"
#endif
#ifndef NEWSBIN
/* =()<#define	NEWSBIN	"@<NEWSBIN>@">()= */
#define	NEWSBIN	"/usr/lib/newsbin"
#endif
#ifndef NEWSUMASK
/* =()<#define	NEWSUMASK	@<NEWSUMASK>@>()= */
#define	NEWSUMASK	002
#endif
#ifndef NEWSMASTER
/* =()<#define	NEWSMASTER	"@<NEWSMASTER>@">()= */
#define	NEWSMASTER	"usenet"
#endif
#ifndef NEWSCONFIG
/* =()<#define	NEWSCONFIG	"@<NEWSCONFIG>@">()= */
#define	NEWSCONFIG	"/usr/lib/news/bin/config"
#endif

static char *pwd = NULL;	/* Current directory, NULL means unknown. */
static int dirsset = NO;	/* Have the following been set up? */
static char *arts = NEWSARTS;
static char *bin = NEWSBIN;
static char *ctl = NEWSCTL;
static char *path = NEWSPATH;
static int numask = NEWSUMASK;
static char *nmaster = NEWSMASTER;
static char *nconfig = NEWSCONFIG;
#define	DIRS()	if (!dirsset) setdirs()

extern char *getenv();

/*
 - setdirs - set up stuff from environment, for use by other functions
 *
 * Invokes user-supplied function unprivileged() if non-standard values used.
 */
static void
setdirs()
{
	register char *p;
	register int mask;
	register char *scan;
	register int ns = 0;
#	define	NONSTD(reason)	{ if (!ns) { unprivileged(reason); ns = 1; } }

	if (dirsset)
		return;

	p = getenv("NEWSARTS");
	if (p != NULL && !STREQ(p, arts)) {
		arts = p;
		NONSTD("NEWSARTS");
	}

	p = getenv("NEWSCTL");
	if (p != NULL && !STREQ(p, ctl)) {
		ctl = p;
		NONSTD("NEWSCTL");
	}

	p = getenv("NEWSPATH");
	if (p != NULL && !STREQ(p, path)) {
		path = p;
		NONSTD("NEWSPATH");
	}

	p = getenv("NEWSBIN");
	if (p != NULL && !STREQ(p, bin)) {
		bin = p;
		NONSTD("NEWSBIN");
	}

	p = getenv("NEWSUMASK");
	if (p != NULL) {
		mask = 0;
		for (scan = p; *scan != '\0'; scan++)
			if ('0' <= *scan && *scan <= '7' && mask <= 077)
				mask = (mask << 3) | (*scan - '0');
			else {	/* Garbage, ignore it. */
				mask = numask;
				break;			/* NOTE BREAK OUT */
			}
		if (mask != numask) {
			numask = mask;
			NONSTD("NEWSUMASK");
		}
	}

	p = getenv("NEWSMASTER");
	if (p != NULL && !STREQ(p, nmaster)) {
		nmaster = p;
		NONSTD("NEWSMASTER");
	}

	p = getenv("NEWSCONFIG");
	if (p != NULL && !STREQ(p, nconfig)) {
		nconfig = p;
		NONSTD("NEWSCONFIG");
	}

	dirsset = YES;
}

/*
 - artfile - best pathname for a file in NEWSARTS
 */
char *
artfile(base)
char *base;
{
	static char *artf = NULL;

	DIRS();

	if (base == NULL)	/* he just wants the directory */
		return (arts);

	if (artf != NULL)
		free(artf);	/* toss old returned value */
	if (pwd != NULL && STREQ(pwd, arts))
		artf = strsave(base);
	else
		artf = str3save(arts, SFNDELIM, base);

	return (artf);
}

/*
 - fullartfile - full pathname for a file in NEWSARTS
 */
char *
fullartfile(base)
char *base;
{
	register char *p;
	register char *pwdsave;

	pwdsave = pwd;
	pwd = NULL;		/* fool artfile() into giving full path */
	p = artfile(base);
	pwd = pwdsave;
	return (p);
}

/*
 - ctlfile - full pathname for a file in NEWSCTL
 */
char *
ctlfile(base)
char *base;
{
	static char *ctlf = NULL;

	DIRS();

	if (ctlf != NULL)
		free(ctlf);		/* toss old returned value */

	if (base == NULL) {
		ctlf = NULL;
		return(ctl);
	} else {
		ctlf = str3save(ctl, SFNDELIM, base);
		return(ctlf);
	}
}

/*
 - binfile - full pathname for a file in NEWSBIN
 */
char *
binfile(base)
char *base;
{
	static char *binf = NULL;

	DIRS();

	if (binf != NULL)
		free(binf);		/* toss old returned value */

	if (base == NULL) {
		binf = NULL;
		return(bin);
	} else {
		binf = str3save(bin, SFNDELIM, base);
		return (binf);
	}
}

/*
 - cd - change to a directory, with checking
 */
void
cd(dir)
char *dir;
{
	if (pwd != NULL)
		free(pwd);
	if (chdir(dir) < 0)
		errunlock("cannot chdir(%s)", dir);
	pwd = strsave(dir);
}

/*
 - newspath - search path for normal system commands
 */
char *
newspath()
{
	DIRS();
	return(path);
}

/*
 - newsumask - suitable value of umask for news stuff
 */
int
newsumask()
{
	DIRS();
	return(numask);
}

/*
 - newsmaster - mail address to complain to
 */
char *
newsmaster()
{
	DIRS();
	return(nmaster);
}
