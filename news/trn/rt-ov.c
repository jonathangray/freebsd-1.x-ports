/* $Id: rt-ov.c,v 1.6 1994/02/22 01:50:42 nate Exp $
*/
/* The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"
#include "trn.h"
#include "cache.h"
#include "bits.h"
#include "head.h"
#include "ngdata.h"
#include "util.h"
#include "ng.h"
#include "nntp.h"
#include "term.h"
#include "final.h"
#include "hash.h"
#include "rthread.h"
#include "rt-process.h"
#include "rt-util.h"
#include "overview.h"
#include "INTERN.h"
#include "rt-ov.h"

bool
ov_init()
{
#ifdef USE_XOVER
    /* Check if the server is XOVER complient (we're not in a group, BTW) */
    sprintf(ser_line, "XOVER");
    nntp_command(ser_line);
    nntp_check(FALSE);
    if (atoi(ser_line) == NNTP_BAD_COMMAND_VAL)
	return FALSE;
    /* paranoia reins supreme */
    if (*ser_line == NNTP_CLASS_OK) {
	do {
	    nntp_gets(ser_line, sizeof ser_line);
	} while (!NNTP_LIST_END(ser_line));
    }
#endif
    return TRUE;
}

#ifndef OV_XREFS
# ifdef OV_OTHERS_HAS_XREFS
bool ov_files_have_xrefs = TRUE;	/* set once per session */
# else
bool ov_files_have_xrefs = FALSE;	/* set once per session */
# endif
#endif
#ifdef USE_XOVER
ART_NUM ov_next_art;
#else
FILE *ov_in;
#endif

/* Process the data in the group's news-overview file.
*/
bool
ov_data(first, last, cheating)
ART_NUM first, last;
bool_int cheating;
{
    register ARTICLE *ap;
    ART_NUM artnum, an;
    char *line, *last_buf = buf;
    MEM_SIZE last_buflen = LBUFLEN;
    int cachemask;
    bool success = TRUE;
#ifdef USE_XOVER
    ART_NUM real_first = first, real_last = last;

    setspin(cheating? SPIN_BACKGROUND : SPIN_FOREGROUND);
beginning:
    if (ov_opened)
	first = ov_next_art;
    if (last < first)
	goto exit;
    if (last - first > OV_CHUNK_SIZE + OV_CHUNK_SIZE/2 - 1)
	last = first + OV_CHUNK_SIZE - 1;
    sprintf(ser_line, "XOVER %ld-%ld", (long)first, (long)last);
    nntp_command(ser_line);
    if (nntp_check(FALSE) != NNTP_CLASS_OK) {
	success = FALSE;
	goto exit;
    }
#ifdef VERBOSE
    IF(verbose)
	if (!ov_opened)
	    printf("\nGetting overview file."), fflush(stdout);
#endif
    ov_next_art = last+1;

#else /* !USE_XOVER */

    if (!ov_opened) {
	if ((ov_in = fopen(ov_name(ngname), "r")) == Nullfp) {
	    return FALSE;
	}
#ifdef VERBOSE
	IF(verbose)
	    printf("\nReading overview file."), fflush(stdout);
#endif
    }
    setspin(cheating? SPIN_BACKGROUND : SPIN_FOREGROUND);
#endif /* !USE_XOVER */

    ov_opened = TRUE;
    artnum = first-1;
    for (;;) {
#ifdef USE_XOVER
	line = nntp_get_a_line(last_buf, last_buflen);
	if (NNTP_LIST_END(line))
	    break;
#else
	if (!(line = get_a_line(last_buf, last_buflen, ov_in)))
	    break;
#endif
	last_buf = line;
	last_buflen = buflen_last_line_got;
	artnum = atol(line);
	spin(100);
	if (artnum < first)
	    continue;
#ifndef USE_XOVER
	if (artnum > last) {
	    artnum = last;
	    break;
	}
#endif
	if ((ap = ov_parse(line, artnum)) != Nullart) {
#ifndef OV_XREFS
	    if (ov_files_have_xrefs) {
		if (!ap->xrefs)
		    ap->xrefs = nullstr;
	    } else if (ap->xrefs) {
		register ART_UNREAD i;
		register ARTICLE *ap2;
		ap2 = article_ptr(first);
		for (i = artnum-first; i; ap2++, i--)
		    ap2->xrefs = nullstr;
		ov_files_have_xrefs = TRUE;
	    }
#endif
	    if (ThreadedGroup) {
		if (valid_article(ap))
		    thread_article(ap);
	    } else if (!(ap->flags & AF_CACHED))
		cache_article(ap);
	    check_poster(ap);
	}
#ifdef USE_XOVER
	if (int_count) {
	    int_count = 0;
	    success = FALSE;
	}
#else
	if (int_count) {
	    int_count = 0;
	    success = FALSE;
	    break;
	}
	if (cheating) {
	    if (input_pending()) {
		success = FALSE;
		break;
	    }
	    if (curr_artp != sentinel_artp) {
		pushchar('\f' | 0200);
		success = FALSE;
		break;
	    }
	}
#endif /* !USE_XOVER */
    }
    cachemask = (ThreadedGroup? AF_THREADED : AF_CACHED);
    for (an = first, ap = article_ptr(an); an <= artnum; an++, ap++) {
	if (!(ap->flags & cachemask)) {
#ifdef USE_NNTP
	    onemissing(ap);
#else
	    (void) parseheader(an);
#endif
	}
    }
    if (artnum > last_cached && artnum >= first)
	last_cached = artnum;
#ifdef USE_XOVER
    if (int_count || !success) {
	int_count = 0;
	success = FALSE;
    } else if (cheating && curr_artp != sentinel_artp) {
	pushchar('\f' | 0200);
	success = FALSE;
    } else if (last < real_last) {
	if (!cheating || !input_pending()) {
	    last = real_last;
	    goto beginning;
	}
	success = FALSE;
    } else
	ov_next_art = absfirst;
    if (success && real_first <= first_cached) {
	first_cached = real_first;
	cached_all_in_range = TRUE;
    }
  exit:
#else
    if (success && first <= first_cached) {
	first_cached = first;
	cached_all_in_range = TRUE;
    }
    if (!cheating)
	fseek(ov_in, 0L, 0);	/* rewind it for the cheating phase */
#endif
    setspin(SPIN_POP);
    if (last_buf != buf)
	free(last_buf);
    return success;
}

static ARTICLE *
ov_parse(line, artnum)
register char *line;
ART_NUM artnum;
{
    register int nf;
    register ARTICLE *article;
    char *fields[OV_OTHERS+1], *cp;

    article = article_ptr(artnum);
    if (article->flags & AF_THREADED)
	return Nullart;

    if (len_last_line_got > 0 && line[len_last_line_got-1] == '\n')
	line[len_last_line_got-1] = '\0';

    cp = line;
    
    for (nf = 0; ; nf++) {
	fields[nf] = cp;
	if (nf == OV_OTHERS)
	    break;
	if (!(cp = index(cp, '\t'))) {
	    if (nf < OV_OTHERS-1)	/* only "others" field is optional */
		return Nullart;		/* skip this line */
	    break;
	}
	*cp++ = '\0';
    }

    if (!article->subj)
	set_subj_line(article, fields[OV_SUBJ], strlen(fields[OV_SUBJ]));
    if (!article->msgid)
	article->msgid = savestr(fields[OV_MSGID]);
    if (!article->from)
	article->from = savestr(fields[OV_FROM]);
    if (!article->date)
	article->date = parsedate(fields[OV_DATE]);
    references = fields[OV_REFS];

#ifdef OV_XREFS
# ifdef OV_LAX_XREFS
    if (!strncasecmp("xref: ", fields[OV_XREFS], 6))
	article->xrefs = savestr(fields[OV_XREFS]+6);
    else
	article->xrefs = savestr(fields[OV_XREFS]);
# else
    article->xrefs = savestr(fields[OV_XREFS]);
# endif
#else
    /* check the "others" field for an optional xrefs header */
    if (nf == OV_OTHERS && !article->xrefs) {
	register char *fld;
	cp = fields[OV_OTHERS];
	while (cp && *cp) {
	    fld = cp;
	    if ((cp = index(cp, '\t')) != Nullch)
		*cp++ = '\0';
	    if (!strncasecmp("xref: ", fld, 6)) {
		article->xrefs = savestr(fld+6);
		break;
	    }
	}
    }
#endif
    return article;
}

#ifndef USE_XOVER
/* Change a newsgroup name into the name of the overview data file.  We
** subsitute any '.'s in the group name into '/'s, prepend the path, and
** append the '/.overview' or '.ov') on to the end.
*/
static char *
ov_name(group)
char *group;
{
    register char *cp;

    cp = strcpy(buf, overviewdir) + strlen(overviewdir);
    *cp++ = '/';
    strcpy(cp, group);
    while ((cp = index(cp, '.')))
	*cp = '/';
    strcat(buf, OV_FILE_NAME);
    return buf;
}
#endif

void
ov_close()
{
    if (ov_opened) {
#ifndef USE_XOVER
	(void) fclose(ov_in);
#endif
	ov_opened = FALSE;
    }
}
