/* $Id: artsrch.c,v 1.4 1993/12/01 06:37:53 nate Exp $
 */
/* This software is Copyright 1991 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The authors make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"
#include "search.h"
#include "term.h"
#include "util.h"
#include "intrp.h"
#include "cache.h"
#include "bits.h"
#include "kfile.h"
#include "head.h"
#include "final.h"
#include "nntp.h"
#include "ng.h"
#include "ngdata.h"
#include "ngstuff.h"
#include "artio.h"
#include "rthread.h"
#include "rt-select.h"
#include "INTERN.h"
#include "artsrch.h"

void
artsrch_init()
{
#ifdef ARTSEARCH
    init_compex(&sub_compex);
    init_compex(&art_compex);
#endif
}

/* search for an article containing some pattern */

#ifdef ARTSEARCH
int
art_search(patbuf,patbufsiz,get_cmd)
char *patbuf;				/* if patbuf != buf, get_cmd must */
int patbufsiz;
int get_cmd;				/*   be set to FALSE!!! */
{
    char *pattern;			/* unparsed pattern */
    register char cmdchr = *patbuf;	/* what kind of search? */
    register char *s;
    bool backward = cmdchr == '?' || cmdchr == Ctl('p');
					/* direction of search */
    COMPEX *compex;			/* which compiled expression */
    char *cmdlst = Nullch;		/* list of commands to do */
    int normal_return = SRCH_NOTFOUND;	/* assume no commands */
    bool saltaway = FALSE;		/* store in KILL file? */
    char howmuch;		/* search scope: subj/from/Hdr/head/art */
    char *srchhdr;		/* header to search if Hdr scope */
    bool doread;			/* search read articles? */
    bool foldcase = TRUE;		/* fold upper and lower case? */
    int ignorethru = 0;			/* should we ignore the thru line? */

    ART_NUM srchfirst;

    int_count = 0;
    if (cmdchr == '/' || cmdchr == '?') {	/* normal search? */
	if (get_cmd && buf == patbuf)
	    if (!finish_command(FALSE))	/* get rest of command */
		return SRCH_ABORT;
	compex = &art_compex;
	if (patbuf[1]) {
	    howmuch = ARTSCOPE_SUBJECT;
	    srchhdr = Nullch;
	    doread = FALSE;
	}
	else {
	    howmuch = art_howmuch;
	    srchhdr = art_srchhdr;
	    doread = art_doread;
	}
	s = cpytill(buf,patbuf+1,cmdchr);/* ok to cpy buf+1 to buf */
	pattern = buf;
	if (*pattern) {
	    if (*lastpat)
		free(lastpat);
	    lastpat = savestr(pattern);
	}
	if (*s) {			/* modifiers or commands? */
	    for (s++; *s && index("KarcfhHIN",*s); s++) {
		if (*s == 'f')		/* scan from line */
		    howmuch = ARTSCOPE_FROM;
		else if (*s == 'H') {	/* scan a specific header */
		    howmuch = ARTSCOPE_ONEHDR;
		    srchhdr = s + 1;
		    if (!(s = index(srchhdr, ':'))) {
			s = buf + strlen(buf);
			*s++ = ':';
			*s = '\0';
		    }
		    else
			s++;
		    srchhdr = savestr(srchhdr);
		    break;
		} else if (*s == 'h')	/* scan header */
		    howmuch = ARTSCOPE_HEAD;
		else if (*s == 'a')	/* scan article */
		    howmuch = ARTSCOPE_ARTICLE;
		else if (*s == 'r')	/* scan read articles */
		    doread = TRUE;
		else if (*s == 'K')	/* put into KILL file */
		    saltaway = TRUE;
		else if (*s == 'c')	/* make search case sensitive */
		    foldcase = FALSE;
		else if (*s == 'I')	/* ignore the killfile thru line */
		    ignorethru = 1;
		else if (*s == 'N')	/* override ignore if -k was used */
		    ignorethru = -1;
	    }
	}
	while (isspace(*s) || *s == ':')
	    s++;
	if (*s) {
#ifdef OLD_RN_WAY
	    if (*s == 'm' || *s == 'M')
#else
	    if (*s == 'm')
#endif
		doread = TRUE;
	    if (*s == 'k')		/* grandfather clause */
		*s = 'j';
	    cmdlst = savestr(s);
	    normal_return = SRCH_DONE;
	}
	art_howmuch = howmuch;
	if (art_srchhdr != srchhdr) {
	    if (art_srchhdr)
		free(art_srchhdr);
	    art_srchhdr = srchhdr;
	}
	art_doread = doread;
	if (srchahead)
	    srchahead = -1;
    }
    else {
	register char *h;

	howmuch = ARTSCOPE_SUBJECT;	/* just search subjects */
	srchhdr = Nullch;
	doread = (cmdchr == Ctl('p'));
	if (cmdchr == Ctl('n'))
	    normal_return = SRCH_SUBJDONE;
	compex = &sub_compex;
	pattern = patbuf+1;
	strcpy(pattern,": *");
	h = pattern + strlen(pattern);
	interp(h,patbufsiz - (h-patbuf),"%\\s");  /* fetch current subject */
	if (cmdchr == 'k' || cmdchr == 'K' || cmdchr == ','
	 || cmdchr == '+' || cmdchr == '.') {
	    if (cmdchr != 'k')
		saltaway = TRUE;
	    normal_return = SRCH_DONE;
	    if (cmdchr == '+')
		cmdlst = savestr("I:++");
	    else if (cmdchr == '.')
		cmdlst = savestr("I:.");
	    else {
		if (cmdchr == ',')
		    cmdlst = savestr(",");
		mark_as_read();		/* this article has this subject */
	    }
	    if (!*h) {
#ifdef VERBOSE
		IF(verbose)
		    fputs("\nCannot process a null subject.\n",stdout) FLUSH;
		ELSE
#endif
#ifdef TERSE
		    fputs("\nNull subject.\n",stdout) FLUSH;
#endif
		return SRCH_ABORT;
	    }
#ifdef VERBOSE
	    else if (verbose)
		if (cmdchr != '+' && cmdchr != '.')
		    printf("\nMarking subject \"%s\" as read.\n",h) FLUSH;
		else
		    printf("\nSelecting subject \"%s\".\n",h) FLUSH;
#endif
	}
	else if (!srchahead)
	    srchahead = -1;

	{			/* compensate for notesfiles */
	    register int i;
	    for (i = 24; *h && i--; h++)
		if (*h == '\\')
		    h++;
	    *h = '\0';
	}
#ifdef DEBUG
	if (debug) {
	    printf("\npattern = %s\n",pattern) FLUSH;
	}
#endif
    }
    if ((s = compile(compex,pattern,TRUE,foldcase)) != Nullch) {
					/* compile regular expression */
	printf("\n%s\n",s) FLUSH;
	return SRCH_ABORT;
    }
#ifdef KILLFILES
    if (saltaway) {
	char saltbuf[LBUFLEN], *f;

	s = saltbuf;
	f = pattern;
	*s++ = '/';
	while (*f) {
	    if (*f == '/')
		*s++ = '\\';
	    *s++ = *f++;
	}
	*s++ = '/';
	if (doread)
	    *s++ = 'r';
	if (howmuch != ARTSCOPE_SUBJECT) {
	    *s++ = scopestr[howmuch];
	    if (howmuch == ARTSCOPE_ONEHDR) {
		safecpy(s,srchhdr,LBUFLEN-(s-saltbuf));
		s = index(s,':');
		if (!s)
		    s = saltbuf+LBUFLEN-2;
	    }
	}
	*s++ = ':';
	if (!cmdlst)
	    cmdlst = savestr("j");
	safecpy(s,cmdlst,LBUFLEN-(s-saltbuf));
	kf_append(saltbuf);
    }
#endif
    if (cmdlst && index(cmdlst,'='))
	normal_return = SRCH_ERROR;	/* listing subjects is an error? */
    if (get_cmd) {
	fputs("\nSearching...\n",stdout) FLUSH;
					/* give them something to read */
    }
    if (mode == 't') {
	if (!cmdlst)
	    if (sel_mode == SM_ARTICLE)/* set the selector's default command */
		cmdlst = savestr("+");
	    else
		cmdlst = savestr("++");
	if (sel_rereading)
	    doread = TRUE;
	normal_return = SRCH_DONE;
    }
    if (ignorethru == 0 && kill_thru_kludge && cmdlst
     && (*cmdlst == '+' || *cmdlst == '.'))
	ignorethru = 1;
    srchfirst = doread? absfirst
		      : (mode != 'k' || ignorethru > 0)? firstart : killfirst;
    if (backward) {
	if (cmdlst && art <= lastart)
	    art++;			/* include current article */
    }
    else {
	if (art > lastart)
	    art = srchfirst-1;
	else if (cmdlst && art >= absfirst)
	    art--;			/* include current article */
    }
    if (srchahead > 0) {
	if (!backward)
	    art = srchahead - 1;
	srchahead = -1;
    }
    assert(!cmdlst || *cmdlst);
    perform_cnt = 0;
    for (;;) {
	/* check if we're out of articles */
	if (backward? (--art < srchfirst) : (++art > lastart)) {
	    if (cmdlst)
		free(cmdlst);
	    return normal_return;
	}
	if (int_count) {
	    int_count = 0;
	    if (cmdlst)
		free(cmdlst);
	    return SRCH_INTR;
	}
	artp = article_ptr(art);
	if (doread || !(artp->flags & AF_READ)) {
	    if (wanted(compex,art,howmuch)) {
				    /* does the shoe fit? */
		if (cmdlst) {
		    if (perform(cmdlst,TRUE)) {
			if (cmdlst)
			    free(cmdlst);
			return SRCH_INTR;
		    }
		}
		else {
		    if (cmdlst)
			free(cmdlst);
		    return SRCH_FOUND;
		}
	    }
	    else if (!cmdlst && ! (art%50)) {
		printf("...%ld",(long)art);
		fflush(stdout);
	    }
	}
    }
}

/* determine if article fits pattern */
/* returns TRUE if it exists and fits pattern, FALSE otherwise */

bool
wanted(compex, artnum, scope)
COMPEX *compex;
ART_NUM artnum;
char_int scope;
{
    ARTICLE *ap = find_article(artnum);

    if (!ap || (ap->flags & AF_MISSING))
	return FALSE;

    switch (scope) {
    case ARTSCOPE_SUBJECT:
	strcpy(buf,"Subject: ");
	strncpy(buf+9,fetchsubj(artnum,FALSE),256);
#ifdef DEBUG
	if (debug & DEB_SEARCH_AHEAD)
	    printf("%s\n",buf) FLUSH;
#endif
	break;
    case ARTSCOPE_FROM:
	strcpy(buf, "From: ");
	strncpy(buf+6,fetchfrom(artnum,FALSE),256);
	break;
    case ARTSCOPE_ONEHDR:
    {
	int header_num;
	char *s;
	assert(art_srchhdr != Nullch);
	s = index(art_srchhdr,':');
	header_num = set_line_type(art_srchhdr, s);
	if (header_num == SOME_LINE)
	    return FALSE;  /* FIX ME */
	untrim_cache = TRUE;
	strcpy(buf, art_srchhdr);
	sprintf(buf + (s-art_srchhdr), ": %s",
		prefetchlines(artnum,header_num,FALSE));
	untrim_cache = FALSE;
	break;
    }
    default:
	if (!parseheader(artnum))
	    return FALSE;
	/* see if it's in the header */
	if (execute(compex,headbuf) != Nullch)	/* does it match? */
	    return TRUE;			/* say, "Eureka!" */
	if (scope < ARTSCOPE_ARTICLE)
	    return FALSE;
	if (!artopen(artnum))		/* ensure we have the body */
	    return FALSE;
	/* loop through each line of the article */
	while (fgets(buf,LBUFLEN,artfp) != Nullch) {
	    if (execute(compex,buf) != Nullch)	/* does it match? */
		return TRUE;			/* say, "Eureka!" */
	}
	return FALSE;			/* out of article, so no match */
    }
    return execute(compex,buf) != Nullch;
}
#endif

