/*
 * Usenet header modification & generation
 *
 * Ideally, headers should never be modified; message text, including
 * headers, should be passed untouched.  Path: and Xref: demand that this
 * rule be violated, and we delete huge obsolete headers to save space.
 *
 * Delete obsolete & large headers and Xref (can't be right),
 * as headers are read.
 * Recognise Newsgroups: and if more than one group, generate Xref: &
 * leave holes for the article numbers - fileart will fill them in.
 * (Make rn look in history instead?)
 *
 * Pile up headers into a static buffer until end of buffer (i.e. next
 * line might not fit) [checked in hdrsave()], end of headers, end of
 * file, or byte count is exhausted [checked in cparttofp].  Then write
 * them to disk.  Prepend hostname! to Path: value, during output.
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include "libc.h"
#include "news.h"
#include "case.h"
#include "fileart.h"
#include "headers.h"
#include "article.h"
#include "hdrint.h"
#include "msgs.h"

/*
 * HDRMEMSIZ is the length of a header-stashing buffer, which is used
 * only during article-header copying.
 * HDRMEMSIZ can be too small if memory is tight & will only hurt performance.
 * Derivation: 630 bytes is a large header (after discarding *-Version:, etc.).
 */
#ifndef HDRMEMSIZ
#ifdef SMALLMEM
#define HDRMEMSIZ 630
#else
#define HDRMEMSIZ 8192			/* # bytes for saving headers in core */
#endif	/* SMALLMEM */
#endif	/* HDRMEMSIZ */

/* private */
static char **hptrs = NULL;	/* saved-headers-ptrs array; allocated once */

/* forwards */
FORWARD void emithdr(), hdrsave();

/*
 * Generate an Xref: header from art->a_files.
 * Turn slashes in art->a_files into colons in Xref:.
 */
void
emitxref(art)
register struct article *art;
{
	register char *slashp, *xrefs;

	if (!art->a_xref) {
		art->a_xref = YES;
		xrefs = strsave(art->a_files);
		for (slashp = xrefs; (slashp = strchr(slashp, FNDELIM)) != NULL; )
			*slashp++ = ':';
		if (fprintf(art->a_artf, "%s %s %s\n",
		    xrefhdr.hdrnm, hostname(), xrefs) == EOF)
			fulldisk(art, spoolnm(art));
		free(xrefs);
	}
}

/*
 * --- header copying starts here ---
 */

/*
 * Copy headers and delete or modify a few.  Assumes hdrparse has been called.
 * Delete obsolete & large headers and Xref.
 * Pile up other headers for later output (Path: is changed during output).
 *
 * art->a_artf may be NULL, and may get set by hdrsave.
 */
STATIC void
hdrmunge(art, buffer, hdrlen, hdrlst)
register struct article *art;
register char *buffer;
int hdrlen;			/* optimisation only */
hdrlist hdrlst;			/* headers of negative utility: snuff 'em */
{
	register struct hdrdef **vhp;

	if (headdebug)
		(void) fputs(buffer, stderr);
	for (vhp = hdrlst; *vhp != NULL; vhp++) {
		register char *hdrnm = (*vhp)->hdrnm;

		if (CISTREQN(buffer, hdrnm, (int)(*vhp)->hdrlen))
			return;			/* don't save this header */
	}
	hdrsave(art, buffer, hdrlen);
}

/*
 * If headers already dumped, just write to art->a_artf.
 * Else if there is room, stash "hdr" away until end of headers is seen
 * (could just wait until Newsgroups: and Control: are seen, if seen)
 * or there is no room left in the header buffer, then open the first
 * article link (on art->a_artf) and dump the saved headers and the current
 * header to it.
 *
 * Copy into art->a_haccum (in future, could read in directly,
 * iff copying is high on the profile).
 *
 * hdrstore is static because it is used repeatedly, it only makes sense
 * to have one active at a time, and there is no memory saving in allocating
 * and deallocating it, particularly since copyart's (header) buffer must
 * coexist with hdrstore.
 */
STATIC void
hdrsave(art, hdr, hdrlen)
register struct article *art;
char *hdr;
register int hdrlen;			/* optimisation only */
{
	if (art->a_artf != NULL) {
		emithdr(art, hdr, hdrlen);
		return;
	}
	if (art->a_haccum == NULL) {
		static char hdrstore[HDRMEMSIZ];

		art->a_haccum = hdrstore;
		art->a_haccum[0] = '\0';
		art->a_hnext = art->a_haccum;
		art->a_hbytesleft = HDRMEMSIZ;
	}
	if (art->a_hbytesleft > hdrlen) {
		/* add new ptr.-to-this-header to tail of saved-hdr-ptr.-list */
		if (art->a_hptrs == NULL) {
			art->a_hpused = 0;
			art->a_hpalloced = MINSHPTRS;
			if (hptrs == NULL)	/* once only */
				hptrs = (char **) nemalloc((unsigned)
					(art->a_hpalloced * sizeof(char *)));
			art->a_hptrs = hptrs;
		}
		while (art->a_hpused >= art->a_hpalloced) {
			art->a_hpalloced += MINSHPTRS;
			art->a_hptrs = hptrs = (char **)
				realloc((char *)art->a_hptrs, (unsigned)
				(art->a_hpalloced * sizeof(char *)));
			if (art->a_hptrs == NULL)
				errunlock("out of memory (for art->a_hptrs)", "");
		}
		art->a_hptrs[art->a_hpused++] = art->a_hnext;

		/* (void) strcat(art->a_haccum, hdr); */
		(void) strcpy(art->a_hnext, hdr);
		art->a_hnext += hdrlen;		/* points at NUL byte */
		art->a_hbytesleft -= hdrlen;
	} else {
		hdrdump(art, NOTALLHDRS);	/* don't file */
		if (art->a_artf != NULL)
			emithdr(art, hdr, hdrlen);
	}
}

/*
 * Change Path: while writing it out, just dump other headers (hdr) verbatim.
 */
STATIC void
emithdr(art, hdr, hdrlen)
register struct article *art;
char *hdr;
register int hdrlen;
{
	if (CISTREQN(hdr, pathhdr.hdrnm, (int)pathhdr.hdrlen)) {
		register char *oldpath, *hostnm = hostname();

		oldpath = skipsp(&hdr[pathhdr.hdrlen]);
		/*
		 * V7 f?printf return 0 or EOF, not a byte count, so it is
		 * not portable to use fprintf's return value as a byte count.
		 */
		if (fprintf(art->a_artf, "%s %s!", pathhdr.hdrnm, hostnm) ==
		    EOF || fputs(oldpath, art->a_artf) == EOF)
			fulldisk(art, spoolnm(art));
		else {
			static unsigned hostlen = 0;

			if (hostlen == 0)
				hostlen = strlen(hostnm);
			art->a_charswritten += pathhdr.hdrlen + STRLEN(" ") +
			    hostlen + STRLEN("!") + strlen(oldpath);
		}
	} else {
		if (fwrite(hdr, hdrlen, 1, art->a_artf) != 1)
			fulldisk(art, spoolnm(art));
		else
			art->a_charswritten += hdrlen;
	}
}

/*
 * Write out saved headers after opening on art->a_artf either a temporary
 * file (using mktemp(3)) or the first article link, based on art->h.h_ngs &
 * nxtartnum(); set a_tmpf to which ever name is opened.
 * Modify Path: value on the way.
 *
 * If all headers were seen, then open the first link, link to the rest,
 * and generate Xref:, else open a temporary name and write the article
 * there (it will get filed later by hdrdump(...,ALLHDRS) or in insart()).
 */
void
hdrdump(art, allhdrsseen)
register struct article *art;
boolean allhdrsseen;		/* all headers seen & hdrdeflt() called? */
{
	if (art->a_filed)
		return;
	if (allhdrsseen)
		fileart(art);			/* set a_tmpf */
	else if (art->a_artf == NULL) {
		nnfree(&art->a_tmpf);
		art->a_tmpf = strsave(SPOOLTMP);
		(void) mktemp(art->a_tmpf);
		art->a_unlink = YES;
		art->a_artf = fopenwclex(art->a_tmpf, "w");
		if (art->a_artf == NULL)
			art->a_status |= ST_DROPPED;
	}
	if (art->a_artf != NULL &&
	    art->a_haccum != NULL && art->a_haccum[0] != '\0') {
		register int i;
		register char **pp;
		register char *nxtln;
		register int saved;

		for (i = 0, pp = art->a_hptrs; i < art->a_hpused; ++i, ++pp) {
			/* last in-core hdr? */
			nxtln = (i >= art->a_hpused-1? art->a_hnext: pp[1]);

			saved = *nxtln;
			*nxtln = '\0';
			emithdr(art, *pp, nxtln - *pp);
	     		*nxtln = saved;			/* restore */
		}
		/* art->a_haccum could be freed and zeroed here, if malloced */
		art->a_haccum[0] = '\0';	/* paranoia */
		art->a_hnext = art->a_haccum;	/* for next article */
		art->a_hpused = 0;		/* trash saved-header-ptr-list */
		/* reduce saved-header-ptr-list to original size */
		if (art->a_hpalloced > MINSHPTRS) {
			art->a_hpalloced = MINSHPTRS;
			art->a_hptrs = hptrs = (char **)
				realloc((char *)art->a_hptrs, (unsigned)
				(art->a_hpalloced * sizeof(char *)));
			if (hptrs == NULL)
				errunlock("can't free a_hptrs memory", "");
		}
	}
}

/*
 * hdrparse remembers this header if it's interesting.
 * hdrmunge needs art->h.h_ngs to be set, so it is called second.
 * hdrmunge saves or writes this header, unless it's deemed a waste of bytes.
 * hdrmunge may call hdrdump(art, NOTALLHDRS).
 * hdrdump counts art->a_charswritten.
 */
void
hdrdigest(art, line, hdrlen)
register struct article *art;
register char *line;
int hdrlen;
{
	register int match;

	match = hdrparse(&art->h, line, reqdhdrs);
	if (!match)
		match = hdrparse(&art->h, line, opthdrs);

	/* duplicate required header and no previous refusal? */
	if (match == YES+1 && !(art->a_status&ST_REFUSED)) {
		register char *hdrnonl = strsave(line);

		trim(hdrnonl);
		decline(art);
		prefuse(art);
		(void) printf("duplicate required header `%s'\n", hdrnonl);
		free(hdrnonl);
	}
	hdrmunge(art, line, hdrlen, hdrvilest);
}
