/*
 * ngmatch - newsgroup name matching (faster tree-walking version)
 *
 * ngmatch returns true iff the newsgroup(s) in ngs match
 * the pattern(s) in ngpat, where
 *
 * 	ngpats: { ngpat { "," ngpat }* }?
 *	ngpat: "!"? word { "." word }*
 *	word: { alphanum }+ | "all"
 *
 * Only one group need match for success.
 * Failure to match any pattern with a group is a mismatch of that group only.
 * Failure to match any group against any pattern is a total failure.
 *
 * For each group, note the depth of each match against the patterns,
 * negated or not.  Ignore mismatches.  The deepest match wins at the
 * end:  a deeper negated match is a failure, a deeper plain match is a
 * success; a tie is a failure, but see the description of wildcards.
 *
 * "all" in a pattern is a wildcard that matches exactly one word;
 * it does not cross "." (NGDELIM) delimiters.  A non-wildcard match is
 * considered to have matched slightly deeper than a wildcard match in the
 * same position.  This ensures that !alt.sex.all,alt.sex.bondage matches
 * alt.sex.bondage.
 *
 * The obvious implementation involves (number of groups)*(number of
 * patterns) comparisons (unless you get an early match), which can get
 * quite slow, particular when the number of patterns gets large (e.g. when
 * feeding neurotic or cheap neighbours who want *this* and *this* but not
 * *that* oh except for *this*).  This implementation parses the patterns
 * into a tree with common prefixes merged, then walks that tree for each
 * group, so it should only take about 2*(number of groups) comparisons
 * (due to wildcards).  It's actually a little worse in the worst case due
 * to backtracking due to wildcards that appear in non-leaf nodes.  This is
 * a win for complex sys files and probably isn't much worse for trivial
 * ones.  Each node contains a message telling us whether a match at this
 * node is a plain or negated match (i.e. is a match or not).  Barry Shein
 * contributed to this design.
 *
 * Ken Thompson was right: when in doubt, use brute force.  Attempts to
 * use hashing and sorting in this code have slowed it down.  Sheer, brute
 * linear search is fastest for real, live patterns, since 88% of tree nodes
 * have 0 or 1 children (72% have 0).
 *
 * Essentially all the expense is in parsing, matching is pretty fast.
 * Thus preparsing really pays off; using ngmatch isn't a win.
 */

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include "news.h"
#include "ngmatch.h"

#define truth(bool) ((bool)? "yes": "no")
#define LENBIT(len) ((len) > 15? 1: 1 << (len))

/* tunable parameters */
#define RETAIN 300		/* retain & recycle this many patnodes */

/* fundamental constants */
#define UNKNOWN (YES+1)			/* no message attached to this node */
#define ALL "all"			/* word wildcard */

NGPAT {
	NGPAT	*n_next;		/* both next kid and next free node */
	NGPAT	*n_kids;		/* children of this node */
	char	*n_word;		/* atom name */
	short	n_lenmask;		/* bitmask of lengths of self+sibs */
	char	n_match;		/* ternary message */
	char	n_len;			/* length of n_word */
#ifdef STATS
	char	n_nkids;
#endif
};

/* exports */
char *ngerr = NULL;			/* if non-NULL, contains error string */

/* private */
static boolean debug = NO;
static NGPAT *patreuse = NULL;
static int reusables = 0;
/* this cache really does help; it's been measured. */
#define CACHEWORDS 20	/* currently high water mark is 7 (10 nov 1991) */
			/* (sci.physics.edward.teller.boom.boom.boom) */
static struct wordcache {
	char	*word;
	NGPAT	*node;
	char	len;
} wordcache[CACHEWORDS];
static int cachedepth = 0;
/* a bit ugly, but these save tree nodes and time. */
static NGPAT *lastnp = NULL, *tree = NULL;
#ifdef STATS
#define MAXKIDS 20			/* maximum kids to record in stats */
static long kids[MAXKIDS];
#endif

/* forwards */
FORWARD nginprpat();
FORWARD int treematch();

void
matchdebug(state)
boolean state;
{
	debug = state;
}

STATIC NGPAT *
patalloc()					/* allocate a pattern node */
{
	register NGPAT *np = patreuse;

	if (np == NULL)
		return (NGPAT *)nemalloc(sizeof *np);
	else {
		/* pull the first reusable one off the pile */
		patreuse = np->n_next;
		reusables--;
		return np;
	}
}

STATIC
patfree(np)					/* free a pattern node */
register NGPAT *np;
{
	if (reusables >= RETAIN)		/* compost heap is full? */
		free((char *)np);		/* yup, just pitch this one */
	else {					/* no, just stash for reuse */
		++reusables;
		np->n_next = patreuse;
		patreuse = np;
	}
}


/*
 * search tree from np through np's siblings for word of length len.
 * most words (95%) are not found.
 * using word length as a pre-filter seems to be effective at
 * reducing the number of expensive strcmps because length of words
 * in newsgroup names is fairly evenly distributed across the range 3-8.
 * using the masks of lengths extends this pre-filter to all succeeding
 * nodes without having to examine them all.
 *
 * This function is a profiling hot spot.
 */
STATIC NGPAT *
treesearch(np, word, len)
register NGPAT *np;			/* better not be NULL */
register char *word;
register int len;
{
	register char *npword;
	register char word1stc = word[0];

	do {
		if (len == np->n_len) {
			/* in-line STREQ for greater speed */
			npword = np->n_word;
			if (*npword == word1stc && strcmp(npword, word) == 0)
				return np;
		}
	} while ((np = np->n_next) != NULL);
	return NULL;
}

/*
 * add an atom next to curnp for "word", unless it's already there;
 * return a pointer to the node for "word".
 * keep a cache of (string, tree node) pairs for each level in the tree.
 */
STATIC NGPAT *
addatom(curnp, word, len, level, wcp, cachematchp)
register NGPAT *curnp;			/* may be NULL */
register char *word;
register int len, level;
register struct wordcache *wcp;
register int *cachematchp;
{
	/*
	 * look up this word in the cache (finds 40%).  if absent,
	 * insert it in cache and tree.
	 */
	if (*cachematchp && level < cachedepth && level < CACHEWORDS &&
	    len == wcp->len && STREQ(word, wcp->word))
		return (lastnp = wcp->node);
	else {
		register NGPAT *newnp;
		register int lenmask = LENBIT(len), nxtlenmsk = 0;

		/* only 1.5% of these lookups succeed */
		if (curnp == NULL ||
		    !((nxtlenmsk = curnp->n_lenmask)&lenmask) ||
		    (newnp = treesearch(curnp, word, len)) == NULL) {
			/* the common case */
			if (word[0] == '\0')
				ngerr = "empty word";
			newnp = patalloc();
			newnp->n_match = UNKNOWN;
			newnp->n_word = word;
			newnp->n_len = len;
			newnp->n_lenmask = lenmask;
			newnp->n_kids = NULL;
			/* new node's sibs are this node's kids */
			newnp->n_next = curnp;
			newnp->n_lenmask |= nxtlenmsk;

			/* add new node as kid of previous node, if any */
			if (lastnp != NULL)
				lastnp->n_kids = newnp;
			if (level == 0)
				tree = newnp;
#ifdef STATS
			if (lastnp != NULL)
				lastnp->n_nkids++;
			newnp->n_nkids = 0;
#endif
		}
		*cachematchp = NO;
		if (level < CACHEWORDS) {
			cachedepth = level + 1;
			wcp->word = word;
			wcp->len = len;
			wcp->node = newnp;
		}
		lastnp = newnp;
		return newnp;
	}
}

/*
 * add pattern p to the tree `curnp', if any.  returns resulting tree.
 * modifies p, which must persist until we are done with this tree.
 */
STATIC NGPAT *
addpat(curnp, p)
register NGPAT *curnp;		/* may be NULL for first pattern */
char *p;
{
	register char *word, *period = p;
	register char c;
	register int level = 0;
	register struct wordcache *wcp = wordcache;
	register int leafmatch = YES;
	int cachematching = YES;

	/* consume negation character if any */
	if (*period == NGNEG) {
		period++;
		leafmatch = NO;
	}

	lastnp = NULL;		/* no parent for first atom */
	for (word = period; (c = *period++) != '\0'; )
		/* inline STRCHR(word, NGDELIM, period); */
		if (c == NGDELIM) {
			/* there appears to be another word in the pattern */
			period[-1] = '\0';	/* NOT restored below */
			curnp = addatom(curnp, word, period - 1 - word,
				level++, wcp++, &cachematching)->n_kids;
			word = period;		/* next atom! */
		}
	/* one last pattern */
	curnp = addatom(curnp, word, period - 1 - word, level, wcp,
		&cachematching);
	if (curnp->n_match != UNKNOWN)
		if (curnp->n_match == leafmatch)
			ngerr = "redundant pattern";
		else
			ngerr = "contradictory pattern";
	curnp->n_match = leafmatch;
	return tree;
}

/*
 * parse ngpat into a tree with no redundant nodes.
 * modifies ngpat, which must persist until we are done
 * with the resulting tree.
 *
 * break the list into NUL-terminated patterns.  add them to the parse tree.
 */
NGPAT *
ngparse(ngpat)
char *ngpat;
{
	register char *p, *comma;
	register char c;
	register NGPAT *pat = NULL;

	ngerr = NULL;
	if (ngpat[0] == '\0')
		ngerr = "empty pattern list";
	cachedepth = 0;
	tree = NULL;
	for (p = comma = ngpat; (c = *comma++) != '\0'; )
		/* inline STRCHR(p, NGSEP, comma); */
		if (c == NGSEP) {
			/* there appears to be another pattern in the list */
			comma[-1] = '\0';	/* NOT restored below */
			if (*p == '\0')
				ngerr = "empty pattern";
			pat = addpat(pat, p);
			p = comma;		/* advance to next pattern */
		}
	/* one last pattern to process */
	return addpat(pat, p);
}


struct prhook {
	char	*group;
	FILE	*stream;
	short	first;
};

STATIC
prnode(np, hook)
register NGPAT *np;
register struct prhook *hook;
{
	register char *newgrp, *period;

	if (hook->group == NULL)
		hook->group = strsave("");
	/* append this word */
	if (hook->group[0] == '\0') {
		free(hook->group);
		hook->group = strsave(np->n_word);
	} else {
		newgrp = str3save(hook->group, SNGDELIM, np->n_word);
		free(hook->group);
		hook->group = newgrp;
	}
	/* check for a message at this node & print if found */
	if (np->n_match != UNKNOWN) {
		if (hook->first)
			hook->first = NO;
		else
			(void) putc(NGSEP, hook->stream);
		if (np->n_match == NO)
			(void) putc(NGNEG, hook->stream);
		(void) fputs(hook->group, hook->stream);
	}
	/* recurse on any children */
	nginprpat(np->n_kids, hook);
	/* remove this word */
	period = strrchr(hook->group, NGDELIM);
	if (period == NULL)
		hook->group[0] = '\0';
	else
		*period = '\0';
}

/* print the pattern tree, internal version */
STATIC
nginprpat(np, hook)
register NGPAT *np;
register struct prhook *hook;
{
	for (; np != NULL; np = np->n_next)
		prnode(np, hook);
}

/* print a pattern tree */
ngprint(pat, stream)
NGPAT *pat;
FILE *stream;
{
	static struct prhook hook;

	hook.first = YES;
	hook.stream = stream;
	nginprpat(pat, &hook);
}


#ifndef STATS
/* ARGSUSED */
ngprstats(stream)
FILE *stream;
{
}
#else				/* STATS */
/* collect tree statistics */
STATIC
ngstatpat(np)
register NGPAT *np;
{
	for (; np != NULL; np = np->n_next) {
		if (np->n_nkids >= MAXKIDS - 1)
			kids[MAXKIDS-1]++;
		else
			kids[np->n_nkids]++;
		if (np->n_kids != NULL)
			ngstatpat(np->n_kids);
	}
}

/* print the tree statistics */
ngprstats(stream)
FILE *stream;
{
	register int n;

	(void) fprintf(stream, "kids\t# nodes\n");
	for (n = 0; n < MAXKIDS; n++)
		(void) fprintf(stream, "%d\t%ld\n", n, kids[n]);
}
#endif				/* STATS */


struct matchook {
	char *suffix;
	int match;
};

#define finalscore(defmatch, realscore, nominalscore) \
	((defmatch) == UNKNOWN? realscore: nominalscore)
#define abs(n) ((n) < 0? -(n): (n))

/*
 * match group (ngpfx.sfx) against the patterns in the tree pat;
 * return a score to indicate depth and kind of match.
 * In particular, the first word (ngpfx) should be found in
 * the pat or its siblings, if any.
 */
STATIC int
matscore(pat, defmatch, ngpfx, ngplen, sfx, realscore, nominalscore, incr)
register NGPAT *pat;			/* pattern subtree */
register int defmatch;			/* inherited match value */
char *ngpfx, *sfx;			/* newsgroup prefix & suffix */
register int ngplen;
register int nominalscore, realscore;
int incr;
{
	register NGPAT *wdnp;
	register int ret;

	if (debug) {
		(void) fprintf(stderr, "matscore(");
		ngprint(pat, stderr);
		(void) fprintf(stderr,
		", defmatch %d, ngpfx %s, sfx %s, scores %d %d, incr %d): ",
			defmatch, ngpfx, sfx, realscore, nominalscore, incr);
	}

	/*
	 * any pattern word matches sfx word? succeeds about 33% of the time.
	 * if no match, use the last "known" score.
	 * if a match, try the next tree level.
	 */
	if (!(pat->n_lenmask&LENBIT(ngplen)) ||
	    (wdnp = treesearch(pat, ngpfx, ngplen)) == NULL) {
		ret = finalscore(defmatch, realscore, nominalscore);
		if (debug)
			(void) fprintf(stderr, "missing; returning %d\n", ret);
		return ret;
	} else {
		if (debug)
			(void) fprintf(stderr,
				"found; continuing with treematch\n");
		defmatch = wdnp->n_match;
		if (nominalscore < 0)
			nominalscore = -nominalscore;	/* absolute value */
		nominalscore += incr;
		if (defmatch == NO)
			nominalscore = -nominalscore;
		if (defmatch != UNKNOWN)
			realscore = nominalscore;
		return treematch(wdnp->n_kids, defmatch, sfx,
			realscore, nominalscore);
	}
}

/*
 * match group suffix ngsfx against the patterns in the tree pat.
 * In particular, the first word of ngsfx should be found in the children of
 * pat, if any.  Alternately, if "all" appears in a child, follow that subtree.
 * Matches with a message of "UNKNOWN" don't contribute to the score,
 * they just allow us to keep matching (we need to note our level in
 * the tree and any discount for wildcard matches, in case we later get
 * a match with a non-"UNKNOWN" message).
 */
STATIC int				/* (depth of match) * sign(match) */
treematch(pat, defmatch, ngsfx, realscore, nominalscore)
register NGPAT *pat;			/* pattern subtree; may be NULL */
register int defmatch;			/* inherited match value */
char *ngsfx;				/* newsgroup suffix */
register int realscore, nominalscore;
{
	register char *period, *sfx;
	register int ret, litscore, wildscore, ngplen;

	if (debug) {
		(void) fprintf(stderr, "treematch(");
		ngprint(pat, stderr);
		(void) fprintf(stderr, ", defmatch %d, %s, scores %d %d): ",
			defmatch, ngsfx, realscore, nominalscore);
	}
	/*
	 * if the pattern or group is exhausted,
	 * defmatch==UNKNOWN is a failure.
	 */
	if (pat == NULL || ngsfx == NULL || ngsfx[0] == '\0') {
		ret = finalscore(defmatch, realscore, nominalscore);
		if (debug)
			(void) fprintf(stderr,
				"returning %d due to exhaustion\n", ret);
		return ret;
	}
	if (debug)
		(void) fprintf(stderr, "...\n");

	STRCHR(ngsfx, NGDELIM, period);
	if (period != NULL) {
		ngplen = period - ngsfx;
		*period = '\0';		/* restored below */
		sfx = period + 1;	/* next words */
	} else {
		ngplen = strlen(ngsfx);
		sfx = "";
	}

	litscore =  matscore(pat, defmatch, ngsfx, ngplen, sfx,
		realscore, nominalscore, 20);
	wildscore = matscore(pat, defmatch, ALL, (int)STRLEN(ALL), sfx,
		realscore, nominalscore, 19);

	if (period != NULL) {
		*period++ = NGDELIM;
		if (period[0] == '\0')
			ngerr = "empty word in group suffix";
	}

	/* return deeper match */
	ret = (abs(wildscore) < abs(litscore)? litscore: wildscore);
	if (debug) {
		(void) fprintf(stderr, "treematch(");
		ngprint(pat, stderr);
		(void) fprintf(stderr,
			", defmatch %d, %s, scores %d %d) returns %d\n",
			defmatch, ngsfx, realscore, nominalscore, ret);
	}
	return ret;
}

/*
 * match groups ngs against the patterns in the tree pat, by walking the tree
 * for each group; first unbanged match wins;
 * return the score (0: no match, <0: banged match, >0: unbanged match,
 * greater magnitude indicates a deeper match).
 */
int
ngpatscore(pat, ngs)
register NGPAT *pat;
register char *ngs;
{
	register char *ngp;			/* point at current group */
	register char *ngcomma;
	register int score = 0;			/* no match yet */

	if (ngs[0] == '\0')
		ngerr = "empty group list";
	for (ngp = ngs; score <= 0 && ngp != NULL; ngp = ngcomma) {
		STRCHR(ngp, NGSEP, ngcomma);
		if (ngcomma != NULL)
			*ngcomma = '\0';	/* will be restored below */

		if (ngp[0] == '\0')
			ngerr = "empty group";
		/* match one group (ngp) against all patterns at once */
		score = treematch(pat, UNKNOWN, ngp, 0, 0);

		if (ngcomma != NULL)
			*ngcomma++ = NGSEP;	/* point after the comma */
	}
	return score;
}

/* match groups ngs against the patterns in the tree pat */
boolean
ngpatmat(pat, ngs)
NGPAT *pat;
char *ngs;
{
	return ngpatscore(pat, ngs) > 0;
}

/* compatibility interface; not a very fast way to match */
boolean
ngmatch(ngpat, ngs)
char *ngpat, *ngs;
{
	register NGPAT *pat;
	register boolean match = NO;
	register char *patcopy = strsave(ngpat);

	if (debug)
		(void) putc('\n', stderr);
	pat = ngparse(patcopy);		/* ngparse destroys patcopy */
	if (pat != NULL) {
#ifdef STATS
		ngstatpat(pat);
#endif
#ifdef NGPRINT
		(void) putc('\n', stderr);
		ngprint(pat, stderr);
		(void) putc('\n', stderr);
#endif
		match = ngpatmat(pat, ngs);
		ngfree(pat);
	}
	free(patcopy);
	return match;
}


/* tear down a pattern tree and free the nodes */
ngfree(np)
register NGPAT *np;
{
	register NGPAT *next = np, *kids;

	while ((np = next) != NULL) {
		next = np->n_next;
		if ((kids = np->n_kids) != NULL)
			ngfree(kids);
		patfree(np);
	}
}
