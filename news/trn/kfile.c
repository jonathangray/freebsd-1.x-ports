/* $Id: kfile.c,v 1.3 1993/08/02 23:52:37 nate Exp $
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
#include "term.h"
#include "util.h"
#include "cache.h"
#include "artsrch.h"
#include "ng.h"
#include "ngdata.h"
#include "intrp.h"
#include "ngstuff.h"
#include "rcstuff.h"
#include "trn.h"
#include "hash.h"
#include "rthread.h"
#include "rt-process.h"
#include "rt-select.h"
#include "INTERN.h"
#include "kfile.h"

extern HASHTABLE *msgid_hash;

static bool exitcmds = FALSE;

void
kfile_init()
{
    ;
}

#ifndef KILLFILES
int
edit_kfile()
{
    notincl("^K");
    return -1;
}

#else /* KILLFILES */

char killglobal[] = KILLGLOBAL;
char killlocal[] = KILLLOCAL;

void
mention(str)
char *str;
{
#ifdef VERBOSE
    IF(verbose) {
#ifdef NOFIREWORKS
	no_sofire();
#endif
	standout();
	fputs(str,stdout);
	un_standout();
	putchar('\n');
    }
    ELSE
#endif
#ifdef TERSE
	putchar('.');
#endif
    fflush(stdout);
}

static bool kill_mentioned;

int
do_kfile(kfp,entering)
FILE *kfp;
int entering;
{
    bool first_time = (entering && !killfirst);
    ART_UNREAD selections = selected_count;
    ART_UNREAD unread = toread[ng];
    int thread_kill_cnt = 0;
    int thread_select_cnt = 0;
    char *cp;

    art = lastart+1;
    killfirst = firstart;
    fseek(kfp,0L,0);			/* rewind file */
    while (fgets(buf,LBUFLEN,kfp) != Nullch) {
	if (*(cp = buf + strlen(buf) - 1) == '\n')
	    *cp = '\0';
	if (strnEQ(buf,"THRU",4)) {
	    killfirst = atol(buf+4)+1;
	    if (killfirst < absfirst)
		killfirst = absfirst;
	    if (killfirst > lastart)
		killfirst = lastart+1;
	    if (entering)
		localkf_changes |= 1;
	    continue;
	}
	if (*buf == 'X') {		/* exit command? */
	    if (entering) {
		exitcmds = TRUE;
		continue;
	    }
	    strcpy(buf,buf+1);
	}
	else if (!entering)
	    continue;

	if (*buf == '&') {
	    mention(buf);
	    switcheroo();
	}
	else if (*buf == '/') {
	    has_normal_kills = TRUE;
	    if (firstart > lastart)
		continue;
	    mention(buf);
	    kill_mentioned = TRUE;
	    switch (art_search(buf, (sizeof buf), FALSE)) {
	    case SRCH_ABORT:
		continue;
	    case SRCH_INTR:
#ifdef VERBOSE
		IF(verbose)
		    printf("\n(Interrupted at article %ld)\n",(long)art)
		      FLUSH;
		ELSE
#endif
#ifdef TERSE
		    printf("\n(Intr at %ld)\n",(long)art) FLUSH;
#endif
		return -1;
	    case SRCH_DONE:
		break;
	    case SRCH_SUBJDONE:
		fputs("\tsubject not found (?)\n",stdout) FLUSH;
		break;
	    case SRCH_NOTFOUND:
		fputs("\tnot found\n",stdout) FLUSH;
		break;
	    case SRCH_FOUND:
		fputs("\tfound\n",stdout) FLUSH;
	    }
	}
	else if (first_time && *buf == '<') {
	    register ARTICLE *ap;
	    cp = index(buf,' ');
	    if (!cp)
		cp = "T,";
	    else
		*cp++ = '\0';
	    if ((ap = get_article(buf)) != Nullart) {
		if ((ap->flags & AF_FAKE) == AF_FAKE) {
		    if (*cp == 'T')
			cp++;
		    switch (*cp) {
		    case '+':
			ap->flags |= AF_AUTOSELECTALL;
			thread_select_cnt++;
			break;
		    case '.':
			ap->flags |= AF_AUTOSELECT;
			thread_select_cnt++;
			break;
		    case 'J':
		    case 'j':
			ap->flags |= AF_AUTOKILLALL;
			thread_kill_cnt++;
			break;
		    case ',':
			ap->flags |= AF_AUTOKILL;
			thread_kill_cnt++;
			break;
		    }
		} else {
		    art = article_num(ap);
		    artp = ap;
		    perform(cp,FALSE);
		    if (ap->flags & (AF_AUTOSELECT|AF_AUTOSELECTALL))
			thread_select_cnt++;
		    else if (ap->flags & (AF_AUTOKILL|AF_AUTOKILLALL))
			thread_kill_cnt++;
		}
	    }
	    art = lastart+1;
	} else if (*buf == '*') {
	    register ARTICLE *ap;
	    register int killmask = AF_READ;
	    switch (buf[1]) {
	    case 'X':
		killmask |= sel_mask;	/* don't kill selected articles */
		/* FALL THROUGH */
	    case 'j':
		for (art = killfirst, ap = article_ptr(killfirst);
		     art <= lastart;
		     art++, ap++
		) {
		    if (!(ap->flags & killmask))
			set_read(ap);
		}
		break;
	    }
	    has_normal_kills = TRUE;
	}
    }
    if (thread_kill_cnt) {
	sprintf(buf,"%ld auto-kill command%s.", (long)thread_kill_cnt,
		thread_kill_cnt == 1? "" : "s");
	mention(buf);
	kill_mentioned = TRUE;
    }
    if (thread_select_cnt) {
	sprintf(buf,"%ld auto-select command%s.", (long)thread_select_cnt,
		thread_select_cnt == 1? "" : "s");
	mention(buf);
	kill_mentioned = TRUE;
    }
    unread -= toread[ng];
    selections -= selected_count;
#ifdef VERBOSE
    IF(verbose && (unread > 0 || selections < 0)) {
	putchar('\n');
	if (unread > 0) {
	    printf("Killed %ld article%s", (long)unread,
		unread == 1? nullstr : "s");
	    if (selections < 0)
		fputs("; ",stdout);
	}
	if (selections < 0)
	    printf("Selected %ld article%s", (long)-selections,
		selections == -1? nullstr : "s");
	fputs(".\n",stdout) FLUSH;
	kill_mentioned = TRUE;
    }
#endif
    return 0;
}

void
kill_unwanted(starting,message,entering)
ART_NUM starting;
char *message;
int entering;
{
    bool intr = FALSE;			/* did we get an interrupt? */
    ART_NUM oldfirst;
    char oldmode = mode;
    bool anytokill = (toread[ng] > 0);

    mode = 'k';
    if ((entering || exitcmds) && (localkfp || globkfp)) {
	exitcmds = FALSE;
	oldfirst = firstart;
	firstart = starting;
	clear();
#ifdef VERBOSE
# ifdef TERSE
	if (message && (verbose || entering))
# else
	if (message)
# endif
#else
	if (message && entering)
#endif
	    fputs(message,stdout) FLUSH;

	kill_mentioned = FALSE;
	if (localkfp)
	    intr = do_kfile(localkfp,entering);
	if (globkfp && !intr)
	    intr = do_kfile(globkfp,entering);
	putchar('\n') FLUSH;
	if (entering && kill_mentioned && novice_delays)
#ifdef VERBOSE
	    IF(verbose)
		get_anything();
	    ELSE
#endif
#ifdef TERSE
		pad(just_a_sec);
#endif
	if (anytokill)			/* if there was anything to kill */
	    forcelast = FALSE;		/* allow for having killed it all */
	firstart = oldfirst;
    }
    if (!entering && localkf_changes && !intr)
	rewrite_kfile(lastart);
    mode = oldmode;
}

static FILE *newkfp;

static void write_thread_commands(data, extra)
HASHDATUM *data;
int extra;
{
    register ARTICLE *ap = (data->dat_ptr? (ARTICLE*)data->dat_ptr
					: article_ptr(data->dat_len));
    register int flags;
    char ch;

    if (flags = (ap->flags & AF_AUTOFLAGS)) {
	if (!(ap->flags & AF_MISSING) || ap->child1) {
	    if (flags & AF_AUTOKILLALL)
		ch = 'J';
	    else if (flags & AF_AUTOKILL)
		ch = ',';
	    else if (flags & AF_AUTOSELECTALL)
		ch = '+';
	    else if (flags & AF_AUTOSELECT)
		ch = '.';
	    fprintf(newkfp,"%s T%c\n", ap->msgid, ch);
	}
    }
}

void
rewrite_kfile(thru)
ART_NUM thru;
{
    bool no_kills = 0, has_star_commands = FALSE;

    if (localkfp) {
	fseek(localkfp,0L,0);		/* rewind current file */
	/* If we're writing ids, we know the file is not null */
	if (localkf_changes > 1)
	    ;
	else if (fgets(buf,LBUFLEN,localkfp) != Nullch
	 && (strnNE(buf,"THRU",4) || fgets(buf,LBUFLEN,localkfp) != Nullch))
	    fseek(localkfp,0L,0);
	else
	    no_kills = 1;
    }
    strcpy(buf,filexp(getval("KILLLOCAL",killlocal)));
    if (!localkfp)
	makedir(buf,MD_FILE);
    UNLINK(buf);			/* to prevent file reuse */
    if (no_kills)
	open_kfile(KF_LOCAL);		/* close file and reset open flag */
    else if (newkfp = fopen(buf,"w")) {
	fprintf(newkfp,"THRU %ld\n",(long)thru);
	while (localkfp && fgets(buf,LBUFLEN,localkfp) != Nullch) {
	    if (strnEQ(buf,"THRU",4))
		continue;
	    /* Write star commands after other kill commands */
	    if (*buf == '*') {
		has_star_commands = TRUE;
		continue;
	    }
	    /* Leave out any outdated thread commands */
	    if (*buf != 'T' && *buf != '<')
		fputs(buf,newkfp);
	}
	if (has_star_commands) {
	    fseek(localkfp,0L,0);			/* rewind file */
	    while (fgets(buf,LBUFLEN,localkfp) != Nullch) {
		if (*buf == '*')
		    fputs(buf,newkfp);
	    }
	}
	/* Append all the still-valid thread commands */
	hashwalk(msgid_hash, write_thread_commands, 0);
	fclose(newkfp);
	open_kfile(KF_LOCAL);		/* and reopen local file */
    }
    else
	printf(cantcreate,buf) FLUSH;
    localkf_changes = 0;
    has_normal_kills = FALSE;
}

/* edit KILL file for newsgroup */

int
edit_kfile()
{
    int r = -1;

    if (in_ng) {
	register SUBJECT *sp;

	if (localkf_changes)
	    rewrite_kfile(lastart);
	for (sp = first_subject; sp; sp = sp->next)
	    clear_subject(sp);
	localkf_changes = 0;
	strcpy(buf,filexp(getval("KILLLOCAL",killlocal)));
    } else
	strcpy(buf,filexp(getval("KILLGLOBAL",killglobal)));
    if ((r = makedir(buf,MD_FILE)) >= 0) {
	sprintf(cmd_buf,"%s %s",
	    filexp(getval("VISUAL",getval("EDITOR",defeditor))),buf);
	printf("\nEditing %s KILL file:\n%s\n",
	    (in_ng?"local":"global"),cmd_buf) FLUSH;
	resetty();			/* make sure tty is friendly */
	r = doshell(sh,cmd_buf);/* invoke the shell */
	noecho();			/* and make terminal */
	crmode();			/*   unfriendly again */
	open_kfile(in_ng);
	if (localkfp) {
	    fseek(localkfp,0L,0);			/* rewind file */
	    has_normal_kills = FALSE;
	    while (fgets(buf,LBUFLEN,localkfp) != Nullch) {
		if (*buf == '/' || *buf == '*')
		    has_normal_kills = TRUE;
		else if (*buf == '<') {
		    register ARTICLE *ap;
		    char *cp = index(buf,' ');
		    if (!cp)
			cp = ",";
		    else
			*cp++ = '\0';
		    if ((ap = get_article(buf)) != Nullart) {
			if (*cp == 'T')
			    cp++;
			switch (*cp) {
			case '+':
			    ap->flags |= AF_AUTOSELECTALL;
			    break;
			case '.':
			    ap->flags |= AF_AUTOSELECT;
			    break;
			case 'J':
			case 'j':
			    ap->flags |= AF_AUTOKILLALL;
			    break;
			case ',':
			    ap->flags |= AF_AUTOKILL;
			    break;
			}
		    }
		}
	    }
	}
    }
    else
	printf("Can't make %s\n",buf) FLUSH;
    return r;
}

void
open_kfile(local)
int local;
{
    char *kname = filexp(local ?
	getval("KILLLOCAL",killlocal) :
	getval("KILLGLOBAL",killglobal)
	);
    
    /* delete the file if it is empty */
    if (stat(kname,&filestat) >= 0 && !filestat.st_size)
	UNLINK(kname);
    if (local) {
	if (localkfp)
	    fclose(localkfp);
	localkfp = fopen(kname,"r");
    }
    else {
	if (globkfp)
	    fclose(globkfp);
	globkfp = fopen(kname,"r");
    }
}

void
kf_append(cmd)
char *cmd;
{
    strcpy(cmd_buf,filexp(getval("KILLLOCAL",killlocal)));
    if (makedir(cmd_buf,MD_FILE) >= 0) {
#ifdef VERBOSE
	IF(verbose)
	    printf("\nDepositing command in %s...",cmd_buf);
	ELSE
#endif
#ifdef TERSE
	    printf("\n--> %s...",cmd_buf);
#endif
	fflush(stdout);
	if (novice_delays)
	    sleep(2);
	if ((tmpfp = fopen(cmd_buf,"a")) != Nullfp) {
	    fseek(tmpfp,0L,2);		/* get to EOF for sure */
	    fprintf(tmpfp,"%s\n",cmd);
	    fclose(tmpfp);
	    fputs("done\n",stdout) FLUSH;
	}
	else
	    printf(cantopen,cmd_buf) FLUSH;
    }
    has_normal_kills = TRUE;
}
#endif /* KILLFILES */
