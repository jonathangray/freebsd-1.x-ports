/*
 * Usenet header parsing and remembering.
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include "libc.h"
#include "news.h"
#include "control.h"
#include "case.h"
#include "fileart.h"
#include "headers.h"
#include "hdrint.h"

#define RFC1036				/* alters hdrparse behaviour */

/*
 * Parse (assumed) RFC822 or 850/1036 header in "line" (ishdr(line) can
 * verify this) into "hdrs" using hdrlst set of keywords by retaining the
 * value of any matching keyword.  Keyword matching is case-insensitive.
 * If the keyword in "line" matches one in hdrlst, store the value in
 * *malloc'ed memory* (N.B.) pointed to by a member of "hdrs".
 * freeheader() will free this memory.
 */
int					/* NO, YES or YES+1 (error) */
hdrparse(hdrs, line, hdrlst)
register struct headers *hdrs;
char *line;
hdrlist hdrlst;				/* headers of positive utility */
{
	register struct hdrdef **hpp;
	register char *hackline = hackhybrid(line);
	register int match = NO;

	for (hpp = hdrlst; *hpp != NULL; hpp++) {
		register char *hdrnm = (*hpp)->hdrnm;

		if (CISTREQN(hackline, hdrnm, (int)(*hpp)->hdrlen) &&
		    (*hpp)->hdroff >= 0		/* paranoia */
#ifdef RFC1036
		    && hackline[(*hpp)->hdrlen] == ' '
#endif
		    ) {
			match = YES;
			break;
		}
	}
	if (match) {
		register char **ptrp = (char **)((char *)hdrs+(*hpp)->hdroff);

		if (*ptrp != NULL && hdrlst == reqdhdrs)
			match = YES+1;		/* duplicate req'd hdr */
		else {
			nnfree(ptrp);	/* free prev. value in this article */
			*ptrp = strsave(skipsp(&hackline[(*hpp)->hdrlen]));
			if (*ptrp != NULL)
				trim(*ptrp);	/* cut trailing \n */
		}
	}
	free(hackline);
	return match;
}

/*
 * default missing header values
 *
 * If strsave ever returns NULL on failure, instead of exiting,
 * then the strsave calls need to check for failure.
 *
 * An article lacking a Message-ID: but possessing an (obsolete)
 * Article-I.D.: gets the transformed Article-I.D. as its Message-ID:.
 *
 * We support control message *backwards* compatibility: if no Control:
 * header exists and the newsgroup matches all.all.ctl, use the Subject:
 * as the control message.  Ugh.
 */
void
hdrdeflt(hdrs)
register struct headers *hdrs;
{
	if (hdrs->h_distr == NULL)
		hdrs->h_distr = strsave(DEFDIST);
	if (hdrs->h_msgid == NULL && hdrs->h_artid != NULL)
		hdrs->h_msgid = str3save("<", hdrs->h_artid, ">");
	if (hdrs->h_expiry == NULL || hdrs->h_expiry[0] == '\0') {
		nnfree(&hdrs->h_expiry);
		hdrs->h_expiry = strsave(DEFEXP);
	}
	hackoldctl(hdrs);				/* NCMP */
}

/*
 * require the headers required by RFC 1036.
 */
char *					/* error string */
hdrreq(hdrs)
register struct headers *hdrs;
{
	register struct hdrdef **hpp;
	static char errbuf[100];

	/* require all required headers and that they have non-empty contents */
	for (hpp = reqdhdrs; *hpp != NULL; hpp++) {
		register char *hdrp = *(char **)((char *)hdrs + (*hpp)->hdroff);

		if (hdrp == NULL || hdrp[0] == '\0') {
			(void) sprintf(errbuf, "%s %s header\n",
				(hdrp == NULL? "no": "empty"), (*hpp)->hdrnm);
			return errbuf;
		}
	}
	return NULL;
}
