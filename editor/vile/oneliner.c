/*
 *	A few functions that used to operate on single whole lines, mostly
 *	here to support the globals() function.  They now work on regions.
 *	Written (except for delins()) for vile by Paul Fox, (c)1990
 *
 * $Log: oneliner.c,v $
 * Revision 1.1  1994/02/01 03:29:36  jkh
 * Initial revision
 *
 * Revision 1.55  1993/09/10  16:06:49  pgf
 * tom's 3.61 changes
 *
 * Revision 1.54  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.53  1993/08/13  16:32:50  pgf
 * tom's 3.58 changes
 *
 * Revision 1.52  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.51  1993/07/27  18:06:20  pgf
 * see tom's 3.56 CHANGES entry
 *
 * Revision 1.50  1993/07/21  13:08:00  pgf
 * changed name of 'int *confirm' to 'int *confirmp' for clarity
 *
 * Revision 1.49  1993/07/21  13:01:48  pgf
 * fixed some sloppiness i introduced in the confirmation code (testing
 * "confirm" instead of "*confirm".
 *
 * Revision 1.48  1993/07/20  17:15:22  pgf
 * added mlerase calls to clear confirmation prompt when operation done
 *
 * Revision 1.47  1993/07/15  12:00:36  pgf
 * use mlquickask instead of mlreply when confirming substitutions, so that
 * user needn't hit return for each answer.
 *
 * Revision 1.46  1993/07/15  10:37:58  pgf
 * see 3.55 CHANGES
 *
 * Revision 1.45  1993/07/01  16:15:54  pgf
 * tom's 3.51 changes
 *
 * Revision 1.44  1993/06/18  15:57:06  pgf
 * tom's 3.49 changes
 *
 * Revision 1.43  1993/06/02  14:28:47  pgf
 * see tom's 3.48 CHANGES
 *
 * Revision 1.42  1993/05/24  15:21:37  pgf
 * tom's 3.47 changes, part a
 *
 * Revision 1.41  1993/05/04  17:05:14  pgf
 * see tom's CHANGES, 3.45
 *
 * Revision 1.40  1993/04/28  14:34:11  pgf
 * see CHANGES, 3.44 (tom)
 *
 * Revision 1.39  1993/04/01  12:53:33  pgf
 * removed redundant includes and declarations
 *
 * Revision 1.38  1993/03/08  10:53:17  pgf
 * fix for lengthy multiple substitutions:  need to track the growing line
 * length so we don't stop scanning too early
 *
 * Revision 1.37  1993/01/16  10:40:58  foxharp
 * use new macros
 *
 * Revision 1.36  1992/12/29  23:18:00  foxharp
 * don't allow pregion to be used if the p-lines buffer is current, and
 * set the WINDOW list mode, not the BUFFER list mode, so :l works again
 *
 * Revision 1.35  1992/12/28  23:50:00  foxharp
 * fix for s/./=/g bug, from eric krohn
 *
 * Revision 1.34  1992/12/14  08:31:06  foxharp
 * now handle \U \L \u \l \e in the replacement pattern.  thanks to eric krohn.
 *
 * Revision 1.33  1992/12/07  23:32:29  foxharp
 * fix backslash processing in the replacement pattern in delins()
 *
 * Revision 1.32  1992/05/19  09:14:27  foxharp
 * drop cntrl char in comment
 *
 * Revision 1.31  1992/05/19  08:55:44  foxharp
 * more prototype and shadowed decl fixups
 *
 * Revision 1.30  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.29  1992/05/05  17:47:10  pgf
 * clear fulllineregions after using it, in substregion1, and pregion
 *
 * Revision 1.28  1992/03/13  08:12:11  pgf
 * attempt to use args passed after :& command, as in ":&g"
 *
 * Revision 1.27  1992/03/03  08:42:59  pgf
 * skip a char when doing /g if the pattern can or does match nothing
 *
 * Revision 1.26  1992/03/01  18:41:31  pgf
 * /g finds non-overlapping matches, and in addition, must skip a character
 * if a pattern can find nothing
 *
 * Revision 1.25  1992/02/26  21:56:36  pgf
 * fixed inf. loop caused by s/$/string/g
 *
 * Revision 1.24  1992/01/22  16:58:20  pgf
 * substline() now always returns true except on hard failure -- this makes
 * substreg work properly, instead of quitting on the first line not containing
 * a pattern
 *
 * Revision 1.23  1992/01/05  00:06:13  pgf
 * split mlwrite into mlwrite/mlprompt/mlforce to make errors visible more
 * often.  also normalized message appearance somewhat.
 *
 * Revision 1.22  1992/01/03  23:31:49  pgf
 * use new ch_fname() to manipulate filenames, since b_fname is now
 * a malloc'ed sting, to avoid length limits
 *
 * Revision 1.21  1991/12/22  12:03:09  pgf
 * bug fix for global subst, and allow aborts at the "g, p, or n" prompt
 *
 * Revision 1.20  1991/11/16  18:37:27  pgf
 * use memcpy instead of strncpy
 *
 * Revision 1.19  1991/11/04  14:29:22  pgf
 * extra caution in delins
 *
 * Revision 1.18  1991/11/03  17:46:30  pgf
 * removed f,n args from all region functions -- they don't use them,
 * since they're no longer directly called by the user
 *
 * Revision 1.17  1991/11/01  14:38:00  pgf
 * saber cleanup
 *
 * Revision 1.16  1991/11/01  14:10:35  pgf
 * regexps are now relocatable, so the subst commands can keep a copy
 * of their pattern around
 *
 * Revision 1.15  1991/10/31  02:37:02  pgf
 * implement vi-like substagain
 *
 * Revision 1.14  1991/10/30  14:55:43  pgf
 * fixed boundary position in substline, renamed thescanner to scanner
 *
 * Revision 1.13  1991/10/29  14:35:29  pgf
 * implemented the & commands: substagain
 *
 * Revision 1.12  1991/10/27  01:50:16  pgf
 * switched from regex to regexp, and
 * used Spencer's regsub routine as the basis for a new delins()
 * that allows for replacement metachars
 *
 * Revision 1.11  1991/10/24  13:05:52  pgf
 * conversion to new regex package -- much faster
 *
 * Revision 1.10  1991/10/23  14:17:36  pgf
 * initialize nth_occur in all cases
 *
 * Revision 1.9  1991/09/26  13:11:19  pgf
 * LIST mode moved to the window
 *
 * Revision 1.8  1991/09/16  23:48:30  pgf
 * added "nth occurrence" support to s/repl/pat
 *
 * Revision 1.7  1991/09/10  00:55:24  pgf
 * don't re-popup the buffer everytime -- only on first call
 *
 * Revision 1.6  1991/08/07  12:35:07  pgf
 * added RCS log messages
 *
 * revision 1.5
 * date: 1991/08/06 15:24:19;
 *  global/local values
 *
 * revision 1.4
 * date: 1991/06/27 18:33:47;
 * made screen oriented substitutes always act globally across line
 *
 * revision 1.3
 * date: 1991/06/25 19:53:07;
 * massive data structure restructure
 *
 * revision 1.2
 * date: 1991/05/31 11:14:50;
 * turned these into region operators.
 * they're not one-liners anymore
 *
 * revision 1.1
 * date: 1990/09/21 10:25:51;
 * initial vile RCS revision
 */

#include	"estruct.h"
#include	"edef.h"

#define PLIST	0x01

static	int	delins P(( regexp *, char * ));
static	void	showpat P(( regexp *, int ));
static	int	substreg1 P(( int, int ));
static	int	substline P(( regexp *, int, int, int, int * ));

static	int	lines_changed,
		total_changes;

/*
 * put lines in a popup window
 */
int
pregion(flag)
int flag;
{
	register WINDOW *wp;
	register BUFFER *bp;
	register int	status;
	REGION		region;
	static char bname[] = ScratchName(p-lines);
	fast_ptr LINEPTR linep;

	if ((status = get_fl_region(&region)) != TRUE) {
		rls_region();
		return (status);
	}

	linep = region.r_orig.l;		 /* Current line.	 */

	/* first check if we are already here */
	bp = bfind(bname, 0);
	if (bp == NULL) {
		rls_region();
		return FALSE;
	}

	if (bp == curbp
	 || find_b_name(ScratchName(Buffer List)) != 0 /* patch */
		) {
		mlforce("[Can't do that from this buffer.]");
		rls_region();
		return FALSE;
	}

	if (!calledbefore) {		/* fresh start */
		/* bring p-lines up */
		if (popupbuff(bp) != TRUE) {
			rls_region();
			return FALSE;
		}

		bclear(bp);
		calledbefore = TRUE;
	}

	do {
		if (!addline(bp, l_ref(linep)->l_text, lLength(linep)))
			break;	/* out of memory */
		linep = lFORW(linep);
	} while (!same_ptr(linep, region.r_end.l));

	set_bname(bp, bname);
	set_rdonly(bp, "");
	set_b_val(bp,VAL_TAB,tabstop_val(curbp));

	for_each_window(wp) {
		if (wp->w_bufp == bp) {
			make_local_w_val(wp,WMDLIST);
			set_w_val(wp, WMDLIST, ((flag & PLIST) != 0) );
			wp->w_flag |= WFMODE|WFFORCE;
		}
	}
	rls_region();
	return TRUE;
}

int
llineregion()
{
	return pregion(PLIST);
}

int
plineregion()
{
	return pregion(0);
}

static regexp *substexp;

int
substregion()
{
	return substreg1(TRUE,TRUE);
}

int
subst_again_region()
{
	return substreg1(FALSE,TRUE);
}

/* traditional vi & command */
/* ARGSUSED */
int
subst_again(f,n)
int f,n;
{
	int s;
	MARK curpos;

	curpos = DOT;

	/* the region spans just the line */
	MK.l = DOT.l;
	DOT.o = 0;
	MK.o = lLength(MK.l);
	s = substreg1(FALSE,FALSE);
	if (s != TRUE) {
		mlforce("[No match.]");
		DOT = curpos;
		return s;
	}
	swapmark();
	return TRUE;
}

static int
substreg1(needpats, use_opts)
int needpats, use_opts;
{
	int c, status;
	static int printit, globally, nth_occur, confirm;
	REGION region;
	LINEPTR oline;
	int	getopts = FALSE;

	if ((status = get_fl_region(&region)) != TRUE) {
		rls_region();
		return (status);
	}

	if (calledbefore == FALSE && needpats) {
		c = kbd_delimiter();
		if ((status = readpattern("substitute pattern: ", &pat[0],
					&gregexp, c, FALSE)) != TRUE) {
			if (status != ABORT)
				mlforce("[No pattern.]");
			rls_region();
			return FALSE;
		}

		if (gregexp) {
			FreeIfNeeded(substexp);
			substexp = castalloc(regexp,(ALLOC_T)(gregexp->size));
			(void)memcpy((char *)substexp, (char *)gregexp, (SIZE_T)gregexp->size);
		}

		if ((status = readpattern("replacement string: ", &rpat[0], (regexp	**)0, c,
				FALSE)) == ABORT) {
			rls_region();
			return FALSE;
			/* if false, the pattern is null, which is okay... */
		}
		nth_occur = -1;
		confirm = printit = globally = FALSE;
		getopts = (lastkey == c); /* the user may have something to add */

	} else {
		if (!use_opts) {
			nth_occur = -1;
			printit = globally = FALSE;
		} else {
			if (more_named_cmd()) {
				tungetc(lastkey);
				nth_occur = -1;
				printit = globally = FALSE;
				getopts = TRUE;
			}
		}
	}

	if (getopts) {
		char buf[4];
		char *bp = buf;

		buf[0] = EOS;
		status = mlreply(
"(g)lobally, ([1-9])th occurrence on line, (c)onfirm, and/or (p)rint result: ",
			buf, sizeof buf);
		if (status == ABORT) {
			rls_region();
			return FALSE;
		}
		nth_occur = 0;
		while (*bp) {
			if (*bp == 'p' && !printit) {
				printit = TRUE;
			} else if (*bp == 'g' &&
					!globally && !nth_occur) {
				globally = TRUE;
			} else if (isdigit(*bp) &&
					!nth_occur && !globally) {
				nth_occur = *bp - '0';
				globally = TRUE;
			} else if (*bp == 'c' && !confirm) {
				confirm = TRUE;
			} else if (!isspace(*bp)) {
				mlforce("[Unknown action %s]",buf);
				rls_region();
				return FALSE;
			}
			bp++;
		}
		if (!nth_occur)
			nth_occur = -1;
	}

	lines_changed =
	total_changes = 0;
	DOT.l = region.r_orig.l;	    /* Current line.	    */
	do {
		oline = DOT.l;
		if ((status = substline(substexp, nth_occur, printit,
						globally, &confirm)) != TRUE) {
			rls_region();
			return status;
		}
		DOT.l = lFORW(oline);
	} while (!sameline(DOT, region.r_end));
	calledbefore = TRUE;

	rls_region();
	if (do_report(total_changes)) {
		if (globally)
			mlforce("[%d changes on %d lines]", total_changes, lines_changed);
		else
			mlforce("[%d changes]", total_changes);
	}
	return TRUE;
}

/* show the pattern we've matched */
static void
showpat(rp, on)
regexp	*rp;
int	on;
{
	fast_ptr LINEPTR	lp;
	int	row;

	for (lp = curwp->w_line.l, row = curwp->w_toprow;
		!same_ptr(lp, DOT.l);
			lp = lFORW(lp))
		row += line_height(curwp,lp);

	hilite(row,
		offs2col(curwp, lp, DOT.o),
		offs2col(curwp, lp, DOT.o + (rp->endp[0] - rp->startp[0])), on);
}

static int
substline(exp, nth_occur, printit, globally, confirmp)
regexp *exp;
int nth_occur, printit, globally, *confirmp;
{
	int foundit;
	int again = 0;
	register int s;
	register int which_occur = 0;
	int matched_at_eol = FALSE;
	int yes, c, skipped;
	extern MARK scanboundpos;

	/* if the "magic number" hasn't been set yet... */
	if (!exp || UCHAR_AT(exp->program) != REGEXP_MAGIC) {
		mlforce("[No pattern set yet]");
		return FALSE;
	}

	ignorecase = b_val(curwp->w_bufp, MDIGNCASE);

	foundit = FALSE;
	scanboundpos.l = DOT.l;
	DOT.o = 0;
	do {
		scanboundpos.o = lLength(DOT.l);
		s = scanner(exp, FORWARD, FALSE);
		if (s != TRUE)
			break;

		/* found the pattern */
		foundit = TRUE;
		which_occur++;
		if (nth_occur == -1 || which_occur == nth_occur) {
			(void)setmark();
			/* only allow one match at the end of line, to
				prevent loop with s/$/x/g  */
			if (MK.o == lLength(DOT.l)) {
				if (matched_at_eol)
					break;
				matched_at_eol = TRUE;
			}

			/* if we need confirmation, get it */
			skipped = FALSE;
			if (*confirmp) {

				/* force the pattern onto the screen */
				(void)gomark(FALSE, 0);
				if (update(TRUE) == TRUE)
					showpat(exp, TRUE);
				s = mlquickask("Make change [y/n/q/a]?",
					"ynqa\r",&c);

				showpat(exp, FALSE);

				if (s != TRUE)
					c = 'q';

				switch(c) {
				case 'y' :
					yes = TRUE;
					break;
				default:
				case 'n' :
					yes = FALSE;
					(void)update(TRUE);
					skipped = TRUE;
					/* so we don't match this again */
					DOT.o += (exp->endp[0] - exp->startp[0]);
					break;
				case 'q' :
					mlerase();
					return(FALSE);
				case 'a' :
					yes = TRUE;
					*confirmp = FALSE;
					mlerase();
					break;
				}
			} else {
				yes = TRUE;
			}
			if (yes) {
				s = delins(exp, &rpat[0]);
				if (s != TRUE)
					return s;
				if (!again++)
					lines_changed++;
				total_changes++;
			}
			if (*confirmp && !skipped) /* force a screen update */
				(void)update(TRUE);

			if (exp->mlen == 0 && forwchar(TRUE,1) == FALSE)
				break;
			if (nth_occur > 0)
				break;
		} else { /* non-overlapping matches */
			s = forwchar(TRUE, exp->mlen);
			if (s != TRUE)
				return s;
		}
	} while (globally && sameline(scanboundpos,DOT));
	if (foundit && printit) {
		register WINDOW *wp = curwp;
		(void)setmark();
		s = plineregion();
		if (s != TRUE) return s;
		/* back to our buffer */
		swbuffer(wp->w_bufp);
	}
	if (*confirmp)
		mlerase();
	return TRUE;
}

/*
 * delins, modified by pgf from regsub
 *
 *	Copyright (c) 1986 by University of Toronto.
 *	Written by Henry Spencer.  Not derived from licensed software.
 *
 *	Permission is granted to anyone to use this software for any
 *	purpose on any computer system, and to redistribute it freely,
 *	subject to the following restrictions:
 *
 *	1. The author is not responsible for the consequences of use of
 *		this software, no matter how awful, even if they arise
 *		from defects in it.
 *
 *	2. The origin of this software must not be misrepresented, either
 *		by explicit claim or by omission.
 *
 *	3. Altered versions must be plainly marked as such, and must not
 *		be misrepresented as being the original software.
 */

static	char	*buf_delins;
static	UINT	len_delins;

/*
 - delins - perform substitutions after a regexp match
 */
static int
delins(exp, sourc)
regexp *exp;
char *sourc;
{
	register char *src;
	register ALLOC_T dlength;
	register char c;
	register int no;
	int s;
#define NO_CASE	0
#define UPPER_CASE 1
#define LOWER_CASE 2
	int case_next, case_all;

	if (exp == NULL || sourc == NULL) {
		mlforce("BUG: NULL parm to delins");
		return FALSE;
	}
	if (UCHAR_AT(exp->program) != REGEXP_MAGIC) {
		regerror("damaged regexp fed to delins");
		return FALSE;
	}

	dlength = exp->mlen;

	if (buf_delins == NULL || dlength + 1 > len_delins) {
		if (buf_delins)
			free(buf_delins);
		if ((buf_delins = castalloc(char,dlength+1)) == NULL) {
			mlforce("[Out of memory in delins]");
			return FALSE;
		}
		len_delins = dlength + 1;
	}

	(void)memcpy(buf_delins, exp->startp[0], (SIZE_T)dlength);
	buf_delins[dlength] = EOS;

	if (ldelete((long) dlength, FALSE) != TRUE) {
		mlforce("[Error while deleting]");
		return FALSE;
	}
	src = sourc;
	case_next = case_all = NO_CASE;
	while ((c = *src++) != EOS) {
	    no = 0;
	    s = TRUE;
	    switch(c) {
	    case '\\':
		    c = *src++;
		    if (c == EOS)
		    	return TRUE;
		    if (!isdigit(c)) {
			    /* here's where the \U \E \u \l \t etc.
			    special escapes should be implemented */
			    switch (c) {
			    case 'U':
				    case_all = UPPER_CASE;
				    break;
			    case 'L':
				    case_all = LOWER_CASE;
				    break;
			    case 'u':
				    case_next = UPPER_CASE;
				    break;
			    case 'l':
				    case_next = LOWER_CASE;
				    break;
			    case 'E':
			    case 'e':
				    case_all = NO_CASE;
				    break;
			    case 'b':
				    s = linsert(1,'\b');
				    break;
			    case 'f':
				    s = linsert(1,'\f');
				    break;
			    case 'r':
				    s = linsert(1,'\r');
				    break;
			    case 't':
				    s = linsert(1,'\t');
				    break;
			    case 'n':
				    s = lnewline();
				    break;
			    default:
				    s = linsert(1,c);
				    break;
			    }
			    break;
		    }
		    /* else it's a digit --
		    	get pattern number, and fall through */
		    no = c - '0';
	    case '&':
		    if (exp->startp[no] != NULL && exp->endp[no] != NULL) {
			    char *cp;
			    int len;
			    len = (exp->endp[no] - exp->startp[no]);
			    cp = (exp->startp[no] - exp->startp[0]) + buf_delins;
			    while (len--) {
				    if (!*cp) {
					    mlforce( "BUG: mangled replace");
					    return FALSE;
				    }
				    if (*cp == '\n')
					    s = lnewline();
				    else if (case_next == NO_CASE && case_all == NO_CASE)
					    s = linsert(1,*cp);
				    else {
					    int direction = case_next != NO_CASE ? case_next : case_all;
					    case_next = NO_CASE;
					    c = *cp;
					    /* Somewhat convoluted to handle
					       \u\L correctly (upper case first
					       char, lower case remainder).
					       This is the perl model, not the vi model. */
					    if (isupper(c) && (direction == LOWER_CASE))
						    c = tolower(c);
					    if (islower(c) && (direction == UPPER_CASE))
						    c = toupper(c);
					    s = linsert(1,c);
				    }
				    if (s != TRUE)
					    goto nomem;
				    cp++;
			    }
		    }
		    break;

	    case '\n':
		    s = lnewline();
		    break;

	    default:
		    if (case_next || case_all) {
			    int direction = case_next != NO_CASE ? case_next : case_all;
			    case_next = NO_CASE;
			    /* Somewhat convoluted to handle
			       \u\L correctly (upper case first
			       char, lower case remainder).
			       This is the perl model, not the vi model. */
			    if (isupper(c) && (direction == LOWER_CASE))
				    c = tolower(c);
			    if (islower(c) && (direction == UPPER_CASE))
				    c = toupper(c);
		    }
		    s = linsert(1,c);
		    break;
	    }
	    if (s != TRUE) {
	    nomem:
		    mlforce("[Out of memory while inserting]");
		    return FALSE;
	    }
	}
	return TRUE;
}

#if NO_LEAKS
void
onel_leaks()
{
	FreeIfNeeded(substexp);
	FreeIfNeeded(buf_delins);
}
#endif
