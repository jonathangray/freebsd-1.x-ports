/*
 * newsrc file handling
 */

#include "defs.h"

static char nrcname[]	 = NEWSRC;

static char *rcname;		/* full pathname of .newsrc */
newsrc *rc;			/* internal .newsrc */
char *rcgrps;			/* subscription from .newsrc */
static newsrc *lastrc;		/* last newsrc struct in list */
static int rclineno;		/* current lineno in .newsrc */
static bool sortrc;		/* if we should sort on output */

static newsrc *findnewsrc();

readnewsrc()
{
	register FILE *f;
	static char option[] = "options";
	char word[BUFSIZ], rest[BUFSIZ];	/* getline knows sizes */
	extern char *getenv();

	if ((rcname = getenv("HOME")) == NULL)
		error("No $HOME in environment.");
	rcname = newstr3(rcname, "/", nrcname);
	if ((f = fopen(rcname, "r")) == NULL)
		return;

	rclineno = 0;
	while (getline(f, word, rest))
		if (CMP(word, option) == 0)
			dooptions(rest);
		else
			dorcline(word, rest);
	(void) fclose(f);
}

/*
 * Read a line from f, put first word into w and the rest into r.
 * Discard trailing newline instead of storing it.
 * This is a poor design, as w & r are unchecked for overrun.
 */
static
getline(f, w, r)
register FILE *f;
char *w, *r;
{
	register int c;
	register char *s;
	register int n;

	rclineno++;
	s = w;
	n = BUFSIZ-1;
	while ((c = getc(f)) != EOF && c != ' ' && c != '\t')
		if (n > 0) {
			*s++ = c;			/* stash first word */
			n--;
		}
	*s = '\0';
	if (n <= 0)
		error("%s line %d too long", rcname, rclineno);

	if (c != EOF) {
		s = r;
		n = BUFSIZ-1;
		while ((c = getc(f)) != EOF && c != '\n')
			if (n > 0) {
				*s++ = c;		/* stash the rest */
				n--;
			}
		*s = '\0';
		if (n <= 0)
			error("%s line %d too long", rcname, rclineno);
	}

	if (c != '\n' && c != EOF)
		error("Bad format: %s line %d: %s", rcname, rclineno, w);

	return c != EOF;
}

/*
 * Parse s into words and simulate command line arguments with them.
 */
static
dooptions(s)
char *s;
{
	register char *cp;
	register int argc;
	register char **argv;

	cp = s;
	while (isspace(*cp))
		cp++;
	if (!*cp)
		return;

	argc = 1;
	argv = (char **) myalloc(sizeof(char *));
	argv[argc - 1] = cp;
	while (*cp && (cp = strpbrk(cp, " \t")) != NULL) {
		while (*cp == ' ' || *cp == '\t')
			*cp++ = '\0';
		if (*cp) {
			argc++;
			argv = (char **) myrealloc((char *) argv,
				argc * (int)sizeof(char *));
			argv[argc - 1] = cp;
		}
	}
	if (options(argc, argv, false))
		error("Bad options: %s line %d: %s", rcname, rclineno, s);
	free((char *) argv);
}

/*
 * Parse w & r together as a .newsrc newsgroup line.
 */
static
dorcline(w, r)
char *w, *r;
{
	register char lastw;
	register int len;
	register newsrc	*np;

	len = strlen(w);
	lastw = w[len - 1];			/* save presumed colon or bang */
	w[len - 1] = '\0';			/* nuke presumed colon */
	while (*r == ' ' || *r == '\t')
		r++;				/* skip extra whitespace */

	/* kludges, hacks, etc. for compatibility with other readers */
	if (strncmp(r, "1-", sizeof "1-"-1) == 0)
		r += sizeof "1-"-1;		/* skip usual `1-' */
	if (*r == '\0')				/* rn's: `news.trash: ' */
		r = "0";			/* fake a zero */

	if (lastw != ':' && lastw != NEGCHAR || !isdigit(*r))
		error("Bad line: %s line %d: %s", rcname, rclineno, w);

	np = NEW(newsrc);
	np->n_subscribe = (bool) (lastw == ':');	/* colon or bang? */
	np->n_next = NIL(newsrc);
	np->n_last = atoi(r);			/* stash first number only */
	np->n_name = newstr(w);			/* stash n.g. name */

	if (rc == 0)
		rc = np;
	else
		lastrc->n_next = np;
	lastrc = np;
}

/*
 * for every group in active list, which belongs to the specified subscription
 * list, and has messages to be read, call func
 * if no mention in newsrc file, make new entry
 */
apply(alist, group, func, dolast)
active *alist;
char *group;
applycom (*func)();
bool dolast;
{
	register active *ap;
	register newsrc *np;
	register applycom act;
	register bool donesome;

	donesome = false;
	do {
		act = stop;
		for (ap = alist; ap; ap = ap->a_next) {
			if (ap->a_seq == 0 || ap->a_low > ap->a_seq)
				continue;	/* empty group */
			if (!ngmatch(ap->a_name, group))
				continue;
			if ((np = findnewsrc(ap->a_name)) == NULL) {
				np = NEW(newsrc);
				np->n_name = newstr(ap->a_name);
				np->n_next = NULL;
				np->n_last = 0;
				np->n_subscribe = true;
				if (!rc)
					rc = np;
				else
					lastrc->n_next = np;
				lastrc = np;
			}
			if (!np->n_subscribe)
				continue;
			/*
			 * if we haven't read any news for a while (or at all),
			 * or somehow seq got smaller (active corrupted?),
			 * set last read to oldest available article
			 */
			if (ap->a_low - 1 > np->n_last || ap->a_seq < np->n_last)
				np->n_last = ap->a_low - 1;
			while (np->n_last < ap->a_seq) {
				donesome = true;
				switch (act = (*func)(ap, np, false, false)) {
				case stop:		
					return;
				case next:		
					continue;
				case nextgroup:		
					break;
				case searchgroup:	
					break;
				}
				break;
			}				/* while */
			if (act == searchgroup)
				break;
		}					/* for */
		if (act != searchgroup && dolast && donesome)
			act = (*func)(NIL(active), NIL(newsrc), true, false);
	} while (act == searchgroup);
}

/*
 * find if a newsrc entry exists,
 * taking advantange of the fact that requests should be
 * in the same order
 *
 * detect when the newsrc gets out of order
 * so it can be sorted at the end of the session
 */
static newsrc *
findnewsrc(name)
register char *name;
{
	register newsrc *np, *start;
	register bool found;
	static newsrc *nextp;

	if (!rc)
		return NULL;

	found = false;
	np = nextp ? nextp : rc;
	nextp = start = np;
	do {
		if (CMP(np->n_name, name) == 0) {
			found = true;
			break;
		}
		np = np->n_next;
		if (!np)
			np = rc;
	} while (np != nextp);

	if (!found)
		return NIL(newsrc);
	nextp = np->n_next;
	if (np != start)
		sortrc = true;
	return np;
}

/*
 * rewrite the newsrc file
 */
writenewsrc(alist)
active *alist;
{
	register FILE *f;
	register active	*ap;
	register newsrc	*np;
	register int i;
	extern int usize;

	if (!rc && (!rcgrps || !*rcgrps))
		return;

	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);

	f = fopenf(rcname, "w");
	if (rcgrps && *rcgrps)
		(void) fprintf(f, "options -n %s\n", rcgrps);
	if (sortrc) {
		/*
		 * sort newsrc so next time we use it,
		 * history/newsrc comparisons will be faster
		 */
		for (ap = alist; ap; ap = ap->a_next)
			if (np = findnewsrc(ap->a_name))
				writengline(f, np);
	} else
		for (np = rc; np; np = np->n_next)
			writengline(f, np);
	(void) fclose(f);
}

static
writengline(f, np)		/* write .newsrc n.g. line in normal form on f */
FILE *f;
register newsrc *np;
{
	(void) fprintf(f, "%s%c 1-%d\n", np->n_name,
		(np->n_subscribe? ':': NEGCHAR), np->n_last);
}
