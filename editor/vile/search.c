/*
 * The functions in this file implement commands that search in the forward
 * and backward directions.
 *  heavily modified by Paul Fox, 1990
 *
 * $Log: search.c,v $
 * Revision 1.1  1994/02/01 03:29:38  jkh
 * Initial revision
 *
 * Revision 1.53  1993/09/06  16:34:38  pgf
 * took out redundant TTbeep() calls, now that all failed motions cause
 * a beep.  (left them in for the "only one occurence" cases, since those
 * return TRUE)
 *
 * Revision 1.52  1993/08/13  16:32:50  pgf
 * tom's 3.58 changes
 *
 * Revision 1.51  1993/07/01  10:47:36  pgf
 * rearranged limit setting in scanner(), so I can understand it
 *
 * Revision 1.50  1993/06/18  15:57:06  pgf
 * tom's 3.49 changes
 *
 * Revision 1.49  1993/05/24  15:21:37  pgf
 * tom's 3.47 changes, part a
 *
 * Revision 1.48  1993/04/20  12:18:32  pgf
 * see tom's 3.43 CHANGES
 *
 * Revision 1.47  1993/04/01  12:53:33  pgf
 * removed redundant includes and declarations
 *
 * Revision 1.46  1993/03/09  19:57:33  pgf
 * fixed pointer/integer mismatch on calls to fsearch/rsearch
 *
 * Revision 1.45  1993/03/05  17:50:54  pgf
 * see CHANGES, 3.35 section
 *
 * Revision 1.44  1993/02/24  10:59:02  pgf
 * see 3.34 changes, in CHANGES file
 *
 * Revision 1.43  1993/02/08  14:53:35  pgf
 * see CHANGES, 3.32 section
 *
 * Revision 1.42  1992/12/14  09:03:25  foxharp
 * lint cleanup, mostly malloc
 *
 * Revision 1.41  1992/12/07  23:31:00  foxharp
 * whoops -- on second though, _don't_ do backslash processing in kbd_string,
 * do it in delins()
 *
 * Revision 1.40  1992/12/05  13:36:14  foxharp
 * tell kbd_string() to do backslash processing on the replacement pattern.
 * some comment revisions, and moved rvstrcpy to file.c, where it's now
 * ifdefed along with the code that calls it
 *
 * Revision 1.39  1992/12/04  09:14:36  foxharp
 * deleted unused assigns
 *
 * Revision 1.38  1992/08/20  23:40:48  foxharp
 * typo fixes -- thanks, eric
 *
 * Revision 1.37  1992/07/13  20:03:54  foxharp
 * the "terse" variable is now a boolean mode
 *
 * Revision 1.36  1992/07/08  09:08:08  foxharp
 * don't mlforce() the "onlyonemsg", and support terse on "Not found"
 *
 * Revision 1.35  1992/07/07  08:34:50  foxharp
 * added not_found_msg(), to that non-wrapping searches give appropriate
 * message.  thanks leora!
 *
 * Revision 1.34  1992/06/03  08:37:48  foxharp
 * stop searching if we've wrapped when we hit the boundary
 *
 * Revision 1.33  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.32  1992/03/19  23:46:24  pgf
 * took out scrsearchword stuff -- macros can do it
 *
 * Revision 1.31  1992/03/07  10:19:17  pgf
 * added Eric Krohn's srcsearchword() functions.  All of this screen search
 * stuff could be done with macros if variables worked right.
 *
 * Revision 1.30  1992/02/17  08:54:07  pgf
 * took out unused var for saber
 *
 * Revision 1.29  1992/01/22  16:58:20  pgf
 * cleaned up nextch(), to get rid of unneeded return value
 *
 * Revision 1.28  1992/01/05  00:06:13  pgf
 * split mlwrite into mlwrite/mlprompt/mlforce to make errors visible more
 * often.  also normalized message appearance somewhat.
 *
 * Revision 1.27  1992/01/03  23:24:25  pgf
 * try not to let curpos.o go negative during scan
 *
 * Revision 1.26  1991/11/16  18:34:31  pgf
 * pass magic mode as flag to regcomp()
 *
 * Revision 1.25  1991/11/12  23:47:05  pgf
 * the header line no longer matches '^.$', and
 * now use lregexec exclusively
 *
 * Revision 1.24  1991/11/08  13:11:49  pgf
 * renamed bsearch to rsearch, for lint/libc cleanliness
 *
 * Revision 1.23  1991/11/04  14:24:23  pgf
 * don't use matchlen anymore
 *
 * Revision 1.22  1991/11/04  14:19:49  pgf
 * cleaned up savematch, and call it correctly
 *
 * Revision 1.21  1991/11/03  17:33:20  pgf
 * use new lregexec() routine to check for patterns in lines
 *
 * Revision 1.20  1991/11/01  14:38:00  pgf
 * saber cleanup
 *
 * Revision 1.19  1991/11/01  14:10:35  pgf
 * matchlen is now part of the regexp struct (mlen), and readpattern now
 * takes a **regexp instead of just a flag to indicate it should compile
 * the pattern
 *
 * Revision 1.18  1991/10/30  14:55:43  pgf
 * renamed thescanner to scanner, setboundry to scanboundry
 *
 * Revision 1.17  1991/10/28  01:01:06  pgf
 * added start offset and end offset to regexec calls
 *
 * Revision 1.16  1991/10/27  01:54:47  pgf
 * switched to regexp from regex, and
 * eliminated the old search-and-replace code that was no longer
 * used, and
 * added the new simple findpat() utility
 *
 * Revision 1.15  1991/10/24  13:05:52  pgf
 * conversion to new regex package -- much faster
 *
 * Revision 1.14  1991/10/23  14:18:56  pgf
 * made character comparisons faster with macro, and
 * took out special cases for the header line -- now it really _does_
 * have a -1 as its first and only character
 *
 * Revision 1.13  1991/09/26  13:10:32  pgf
 * new arg. to kbd_string, to suppress backslash processing
 *
 * Revision 1.12  1991/09/19  13:40:30  pgf
 * MDEXACT is now MDIGNCASE
 *
 * Revision 1.11  1991/08/07  12:35:07  pgf
 * added RCS log messages
 *
 * revision 1.10
 * date: 1991/08/06 15:55:24;
 * fixed null l_text dereference in nextch()
 * 
 * revision 1.9
 * date: 1991/08/06 15:25:24;
 *  global/local values
 * 
 * revision 1.8
 * date: 1991/06/25 19:53:21;
 * massive data structure restructure
 * 
 * revision 1.7
 * date: 1991/06/20 17:20:56;
 * added missing status return in forwsearch()
 * 
 * revision 1.6
 * date: 1991/06/03 12:18:46;
 * ifdef'ed MAGIC for unresolved ref
 * 
 * revision 1.5
 * date: 1991/05/31 11:23:11;
 * cleaned up so forwsearch no longer depends on 3rd or 4th arg.
 * 
 * revision 1.4
 * date: 1990/10/12 19:31:44;
 * added scrsearchpat function
 * 
 * revision 1.3
 * date: 1990/09/25 11:38:22;
 * took out old ifdef BEFORE code
 * 
 * revision 1.2
 * date: 1990/09/24 20:36:24;
 * fixed core dump resulting from not checking return code of thescanner()
 * for ABORT
 * 
 * revision 1.1
 * date: 1990/09/21 10:25:59;
 * initial vile RCS revision
 *
 * original written Aug. 1986 by John M. Gamble, but I (pgf) have since
 * replaced his regex stuff with Henry Spencer's regexp package.
 *
 */

#include	"estruct.h"
#include        "edef.h"

#if	LATTICE
#define	void	int
#endif

int lastdirec;

MARK scanboundpos;

char onlyonemsg[] = "Only one occurrence of pattern";
char notfoundmsg[] = "Not found";
char hitendmsg[] = "Search reached %s without matching pattern";

void
not_found_msg(wrapok, dir)
int wrapok, dir;
{
	if (wrapok || global_b_val(MDTERSE))
		mlforce (notfoundmsg);
	else
		mlforce (hitendmsg, dir == FORWARD ? "BOTTOM":"TOP");
}

int
scrforwsearch(f,n)
int f,n;
{
	return fsearch(f,n,FALSE,TRUE);
}

int
scrbacksearch(f,n)
int f,n;
{
	return rsearch(f,n,FALSE,TRUE);
}

/*
 * forwsearch -- Search forward.  Get a search string from the user, and
 *	search for the string.  If found, reset the "." to be just after
 *	the match string, and (perhaps) repaint the display.
 */

int
forwsearch(f, n)
int f, n;
{
	register int status;
	hst_init('/');
	status = fsearch(f, n, FALSE, FALSE);
	hst_flush();
	return status;
}

/* extra args -- marking if called from globals, and should mark lines, and
	fromscreen, if the searchpattern is on the screen, so we don't need to
	ask for it.  */
int
fsearch(f, n, marking, fromscreen)
int f, n;
int marking, fromscreen;
{
	register int status;
	int wrapok;
	MARK curpos;
	int didmark = FALSE;

	if (f && n < 0)
		return rsearch(f, -n, FALSE, FALSE);

	wrapok = marking || b_val(curwp->w_bufp, MDSWRAP);

	lastdirec = 0;

	/* Ask the user for the text of a pattern.  If the
	 * response is TRUE (responses other than FALSE are
	 * possible), search for the pattern for as long as
	 * n is positive (n == 0 will go through once, which
	 * is just fine).
	 *
	 * If "marking", then we were called to do line marking for the
	 *  global command.
	 */
	if (!marking && (status = readpattern("Search: ", &pat[0],
				&gregexp, lastkey, fromscreen)) != TRUE) {
		return status;
	}
		
	ignorecase = b_val(curwp->w_bufp, MDIGNCASE);
			
	curpos = DOT;
	scanboundry(wrapok,curpos,FORWARD);
	do {
		nextch(&(DOT), FORWARD);
		status = scanner(gregexp, FORWARD, wrapok);
		if (status == ABORT) {
			mlforce("[Aborted]");
			DOT = curpos;
			return status;
		}
		/* if found, mark the line */
		if (status && marking) {
			/* if we were on a match when we started, then
				scanner returns TRUE, even though it's
				on a boundary. quit if we find ourselves
				marking a line twice */
			if (lismarked(l_ref(DOT.l)))
				break;
			lsetmarked(l_ref(DOT.l));
			/* and, so the next nextch gets to next line */
			DOT.o = lLength(DOT.l);
			didmark = TRUE;
		}
	} while ((marking || --n > 0) && status == TRUE);
	
	if (!marking && !status)
		nextch(&(DOT),REVERSE);
		
	if (marking) {  /* restore dot and offset */
		DOT = curpos;
	} else if (status) {
		savematch(DOT,gregexp->mlen);
		if (samepoint(DOT,curpos)) {
			mlwrite(onlyonemsg);
			TTbeep();
		}
	}

	/* Complain if not there.  */
	if ((marking && didmark == FALSE) ||
				(!marking && status == FALSE)) {
		not_found_msg(wrapok,FORWARD);
		return FALSE;
	}
	
	return TRUE;
}

/*
 * forwhunt -- Search forward for a previously acquired search string.
 *	If found, reset the "." to be just after the match string,
 *	and (perhaps) repaint the display.
 */

int
forwhunt(f, n)
int f, n;	/* default flag / numeric argument */
{
	register int status;
	int wrapok;
	MARK curpos;
	
	wrapok = b_val(curwp->w_bufp, MDSWRAP);

	if (n < 0)		/* search backwards */
		return(backhunt(f, -n));

	/* Make sure a pattern exists, or that we didn't switch
	 * into MAGIC mode until after we entered the pattern.
	 */
	if (pat[0] == '\0')
	{
		mlforce("[No pattern set]");
		return FALSE;
	}

	ignorecase = b_val(curwp->w_bufp, MDIGNCASE);
			
	/* Search for the pattern for as long as
	 * n is positive (n == 0 will go through once, which
	 * is just fine).
	 */
	curpos = DOT;
	scanboundry(wrapok,DOT,FORWARD);
	do {
		nextch(&(DOT),FORWARD);
		status = scanner(gregexp, FORWARD, wrapok);
	} while ((--n > 0) && status == TRUE);

	/* Save away the match, or complain if not there.  */
	if (status == TRUE) {
		savematch(DOT,gregexp->mlen);
		if (samepoint(DOT,curpos)) {
			mlwrite(onlyonemsg);
			TTbeep();
		}
	} else if (status == FALSE) {
		nextch(&(DOT),REVERSE);
		not_found_msg(wrapok,FORWARD);
	} else if (status == ABORT) {
		mlforce("[Aborted]");
		DOT = curpos;
		return status;
	}

	return status;
}

/*
 * backsearch -- Reverse search.  Get a search string from the user, and
 *	search, starting at "." and proceeding toward the front of the buffer.
 *	If found "." is left pointing at the first character of the pattern
 *	(the last character that was matched).
 */
int
backsearch(f, n)
int f, n;
{
	register int status;
	hst_init('?');
	status = rsearch(f, n, FALSE, FALSE);
	hst_flush();
	return status;
}

/* ARGSUSED */
int
rsearch(f, n, dummy, fromscreen)
int f, n;	/* default flag / numeric argument */
int dummy, fromscreen;
{
	register int status;
	int wrapok;
	MARK curpos;
	
	if (n < 0)
		return fsearch(f, -n, FALSE, fromscreen);

	wrapok = b_val(curwp->w_bufp, MDSWRAP);

	lastdirec = 1;

	/* Ask the user for the text of a pattern.  If the
	 * response is TRUE (responses other than FALSE are
	 * possible), search for the pattern for as long as
	 * n is positive (n == 0 will go through once, which
	 * is just fine).
	 */
	if ((status = readpattern("Reverse search: ", &pat[0],
				&gregexp, lastkey, fromscreen)) == TRUE) {
		ignorecase = b_val(curwp->w_bufp, MDIGNCASE);
			
		curpos = DOT;
		scanboundry(wrapok,DOT,REVERSE);
		do {
			nextch(&(DOT),REVERSE);
			status = scanner(gregexp, REVERSE, wrapok);
		} while ((--n > 0) && status == TRUE);

		/* Save away the match, or complain if not there.  */
		if (status == TRUE)
			savematch(DOT,gregexp->mlen);
			if (samepoint(DOT,curpos)) {
				mlwrite(onlyonemsg);
				TTbeep();
			}
		else if (status == FALSE) {
			nextch(&(DOT),FORWARD);
			not_found_msg(wrapok,REVERSE);
		} else if (status == ABORT) {
			mlforce("[Aborted]");
			DOT = curpos;
			return status;
		}
	}
	return(status);
}

/*
 * backhunt -- Reverse search for a previously acquired search string,
 *	starting at "." and proceeding toward the front of the buffer.
 *	If found "." is left pointing at the first character of the pattern
 *	(the last character that was matched).
 */
int
backhunt(f, n)
int f, n;	/* default flag / numeric argument */
{
	register int status;
	int wrapok;
	MARK curpos;
	
	wrapok = b_val(curwp->w_bufp, MDSWRAP);

	if (n < 0)
		return(forwhunt(f, -n));

	/* Make sure a pattern exists, or that we didn't switch
	 * into MAGIC mode until after we entered the pattern.
	 */
	if (pat[0] == '\0') {
		mlforce("[No pattern set]");
		return FALSE;
	}

	/* Go search for it for as long as
	 * n is positive (n == 0 will go through once, which
	 * is just fine).
	 */

	ignorecase = b_val(curwp->w_bufp, MDIGNCASE);
			
	curpos = DOT;
	scanboundry(wrapok,DOT,REVERSE);
	do {
		nextch(&(DOT),REVERSE);
		status = scanner(gregexp, REVERSE, wrapok);
	} while ((--n > 0) && status == TRUE);

	/* Save away the match, or complain
	 * if not there.
	 */
	if (status == TRUE) {
		savematch(DOT,gregexp->mlen);
		if (samepoint(DOT, curpos)) {
			mlwrite(onlyonemsg);
			TTbeep();
		}
	} else if (status == FALSE) {
		nextch(&(DOT),FORWARD);
		not_found_msg(wrapok,REVERSE);
	} else if (status == ABORT) {
		mlforce("[Aborted]");
		DOT = curpos;
		return status;
	}

	return(status);
}

/* continue searching in the same direction as previous */
int
consearch(f,n)
int f,n;
{
	if (lastdirec == 0)
		return(forwhunt(f,n));
	else
		return(backhunt(f,n));
}

/* continue searching in the opposite direction as previous */
int
revsearch(f,n)
int f,n;
{
	if (lastdirec == 0)
		return(backhunt(f,n));
	else
		return(forwhunt(f,n));
}


/*
 * scanner -- Search for a pattern in either direction.  If found,
 *	reset the "." to be at the start or just after the match string
 */
int	
scanner(exp, direct, wrapok)
regexp	*exp;	/* the compiled expression */
int	direct;	/* which way to go.*/
int	wrapok;	/* ok to wrap around bottom of buffer? */
{
	MARK curpos;
	int found, wrapped;

	if (!exp) {
		mlforce("BUG: null exp");
		return FALSE;
	}

	/* Setup local scan pointers to global ".".
	 */
	curpos = DOT;

	wrapped = 0;

	/* Scan each character until we hit the scan boundary */
	for(;;) {
		register int startoff, srchlim;

		if (interrupted) return ABORT;

		if (sameline(curpos, scanboundpos)) {
			if (direct == FORWARD) {
				if (wrapped) {
					startoff = curpos.o;
					srchlim = scanboundpos.o;
				} else {
					startoff = curpos.o;
					srchlim = (scanboundpos.o > curpos.o) ?
					  scanboundpos.o : lLength(curpos.l);
				}
			} else {
				if (wrapped) {
					startoff = scanboundpos.o;
					srchlim = lLength(curpos.l);
				} else {
					startoff = 0;
					srchlim = scanboundpos.o + 1;
				}
			}
		} else {
			if (direct == FORWARD) {
				startoff = curpos.o;
				srchlim = lLength(curpos.l);
			} else {
				startoff = 0;
				srchlim = curpos.o+1;
			}
		}
		found = lregexec(exp, l_ref(curpos.l), startoff, srchlim);
		if (found) {
			if (direct == REVERSE) { /* find the last one */
				char *got = exp->startp[0];
				while (lregexec(exp, l_ref(curpos.l),
						got+1-l_ref(curpos.l)->l_text, 
						srchlim)) {
					got = exp->startp[0];
				}
				if (!lregexec(exp, l_ref(curpos.l),
						got-l_ref(curpos.l)->l_text,
						srchlim)) {
					mlforce("BUG: prev. match no good");
					return FALSE;
				}
			}
			DOT.l = curpos.l;
			DOT.o = exp->startp[0] - l_ref(curpos.l)->l_text;
			curwp->w_flag |= WFMOVE; /* flag that we have moved */
			return TRUE;
		} else {
			if (sameline(curpos,scanboundpos) &&
							(!wrapok || wrapped))
				break;
		}
		if (direct == FORWARD) {
			curpos.l = lFORW(curpos.l);
			curpos.o = 0;
		} else {
			curpos.l = lBACK(curpos.l);
			if ((curpos.o = lLength(curpos.l)-1) < 0)
				curpos.o = 0;
		}
		if (is_header_line(curpos, curbp)) {
			wrapped++;
			if (sameline(curpos,scanboundpos) &&
						(!wrapok || wrapped) )
				break;
			if (direct == FORWARD) {
				curpos.l = lFORW(curpos.l);
			} else {
				curpos.l = lBACK(curpos.l);
			}
		}

	}

	return FALSE;	/* We could not find a match.*/
}

void
regerror(s)
char *s;
{
	mlforce("[Bad pattern: %s ]",s);
}


/*
 * eq -- Compare two characters.  The "bc" comes from the buffer, "pc"
 *	from the pattern.  If we are in IGNCASE mode, fold out the case.
 */
#if OPT_EVAL || UNUSED
int	
eq(bc, pc)
register int	bc;
register int	pc;
{
	if (bc == pc)
		return TRUE;
	if (!b_val(curwp->w_bufp, MDIGNCASE))
		return FALSE;
	return nocase_eq(bc,pc);
}
#endif

/* ARGSUSED */
int
scrsearchpat(f,n)
int f,n;
{
	int s;
	s =  readpattern("", pat, &gregexp, 0, TRUE);
	mlwrite("Search pattern is now %s", pat);
	lastdirec = 0;
	return s;
}

/*
 * readpattern -- Read a pattern.  Stash it in apat.  If it is the
 *	search string, re_comp() it.
 *	Apat is not updated if the user types in an empty line.  If
 *	the user typed an empty line, and there is no old pattern, it is
 *	an error.  Display the old pattern, in the style of Jeff Lomicka.
 *	There is some do-it-yourself control expansion.
 *	An alternate termination character is passed in.
 */
int
readpattern(prompt, apat, srchexpp, c, fromscreen)
char	*prompt;
char	*apat;
regexp	**srchexpp;
int	c;
int	fromscreen;
{
	int status;

	/* Read a pattern.  Either we get one,
	 * or we don't, and use the previous pattern.
	 * Then, if it's the search string, make a reversed pattern.
	 */
	if (fromscreen) {
		status = screen_string(apat, NPAT, (CMASK)_ident);
		if (status != TRUE)
			return status;
	} else {
		/* don't expand #, %, :, and never process backslashes
			since they're handled by regexp directly for the
			search pattern, and in delins() for the replacement
			pattern */
		hst_glue(c);
	 	status = kbd_string(prompt, apat, NPAT, c, 0, no_completion);
	}
 	if (status == TRUE) {
		if (srchexpp) {	/* If doing the search string, compile it */
			FreeIfNeeded(*srchexpp);
			*srchexpp = regcomp(pat, b_val(curbp, MDMAGIC));
			if (!*srchexpp)
				return FALSE;
		}
	} else if (status == FALSE && *apat != 0) { /* Old one */
		status = TRUE;
	}

	return status;
}

/*
 * savematch -- We found the pattern?  Let's save it away.
 */

void
savematch(curpos,matchlen)
MARK curpos;		/* last match */
int matchlen;
{
	static	patlen = -1;	/* length of last malloc */

	/* free any existing match string */
	if (patmatch == NULL || patlen < matchlen) {
		FreeIfNeeded(patmatch);
		/* attempt to allocate a new one */
		patmatch = castalloc(char, patlen = matchlen + 20);
		if (patmatch == NULL)
			return;
	}

	(void)strncpy(patmatch, &(l_ref(curpos.l)->l_text[curpos.o]), matchlen);

	/* null terminate the match string */
	patmatch[matchlen] = '\0';
}

void
scanboundry(wrapok,dot,dir)
int wrapok;
MARK dot;
int dir;
{
	if (wrapok) {
		nextch(&dot,dir);
		scanboundpos = dot;
	} else {
		scanboundpos = curbp->b_line;
	}
}

/*
 * nextch -- advance/retreat the given mark
 *  will wrap, and doesn't set motion flags
 */
void
nextch(pdot, dir)
MARK	*pdot;
int	dir;
{
	register LINE	*curline;
	register int	curoff;

	curline = l_ref(pdot->l);
	curoff = pdot->o;
	if (dir == FORWARD) {
		if (curoff == llength(curline)) {
			curline = lforw(curline);	/* skip to next line */
			curoff = 0;
		} else {
			curoff++;
		}
	} else {
		if (curoff == 0) {
			curline = lback(curline);
			curoff = llength(curline);
		} else {
			curoff--;
		}
	}
	pdot->l = l_ptr(curline);
	pdot->o = curoff;
}


/* simple finder -- give it a compiled regex, a direction, and it takes you
	there if it can.  no wrapping allowed  */
int
findpat(f,n,exp,direc)
int f, n;
regexp *exp;
int direc;
{
	int s;
	MARK savepos;

	if (!exp)
		return FALSE;

	if (!f) n = 1;

	s = TRUE;
	scanboundry(FALSE,savepos,0);
	while (s == TRUE && n--) {
		savepos = DOT;
		s = (direc == FORWARD) ? forwchar(TRUE,1) : backchar(TRUE,1);
		if (s == TRUE)
			s = scanner(exp, direc, FALSE);
	}
	if (s != TRUE)
		DOT = savepos;

	return s;
}

