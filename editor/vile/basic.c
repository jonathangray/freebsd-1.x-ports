/*
 * The routines in this file move the cursor around on the screen. They
 * compute a new value for the cursor, then adjust ".". The display code
 * always updates the cursor location, so only moves between lines, or
 * functions that adjust the top line in the window and invalidate the
 * framing, are hard.
 *
 * $Log: basic.c,v $
 * Revision 1.1  1994/02/01 03:29:11  jkh
 * Initial revision
 *
 * Revision 1.59  1993/09/10  16:06:49  pgf
 * tom's 3.61 changes
 *
 * Revision 1.58  1993/09/06  16:19:31  pgf
 * eliminated infinite loop in gotobosent()
 *
 * Revision 1.57  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.56  1993/08/13  16:32:50  pgf
 * tom's 3.58 changes
 *
 * Revision 1.55  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.54  1993/07/27  18:06:20  pgf
 * see tom's 3.56 CHANGES entry
 *
 * Revision 1.53  1993/06/02  14:28:47  pgf
 * see tom's 3.48 CHANGES
 *
 * Revision 1.52  1993/05/24  15:21:37  pgf
 * tom's 3.47 changes, part a
 *
 * Revision 1.51  1993/05/11  16:22:22  pgf
 * see tom's CHANGES, 3.46
 *
 * Revision 1.50  1993/05/08  00:23:13  pgf
 * gotomos() and gotoeos() now deal correctly with hitting end of file
 * in window
 *
 * Revision 1.49  1993/04/29  19:14:28  pgf
 * allow goto-named-mark command to be used from command line
 *
 * Revision 1.48  1993/04/28  17:11:22  pgf
 * got rid of NeWS ifdefs
 *
 * Revision 1.47  1993/04/20  12:18:32  pgf
 * see tom's 3.43 CHANGES
 *
 * Revision 1.46  1993/04/01  12:53:33  pgf
 * removed redundant includes and declarations
 *
 * Revision 1.45  1993/03/31  19:30:50  pgf
 * added firstnonwhite() calls to godotplus, so we can publish it as "whole-
 * lines"
 *
 * Revision 1.44  1993/03/18  17:42:20  pgf
 * see 3.38 section of CHANGES
 *
 * Revision 1.43  1993/03/16  10:53:21  pgf
 * see 3.36 section of CHANGES file
 *
 * Revision 1.42  1992/12/14  09:03:25  foxharp
 * lint cleanup, mostly malloc
 *
 * Revision 1.41  1992/12/05  13:12:16  foxharp
 * fix paragraph problem -- i didn't fix all the firstchar() calls before
 *
 * Revision 1.40  1992/12/04  09:08:45  foxharp
 * deleted unused assigns
 *
 * Revision 1.39  1992/11/30  23:06:03  foxharp
 * firstchar/lastchar now return -1 for no non-white chars in line
 *
 * Revision 1.38  1992/08/20  23:40:48  foxharp
 * typo fixes -- thanks, eric
 *
 * Revision 1.37  1992/08/04  20:09:03  foxharp
 * prototype fixups, for xvile
 *
 * Revision 1.36  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.35  1992/03/22  10:54:41  pgf
 * fixed bad bug in gotoline
 *
 * Revision 1.34  1992/03/19  23:30:35  pgf
 * gotoline can now take neg. argument, to count from bottom of
 * buffer. (for finderr)
 *
 * Revision 1.33  1992/03/19  23:05:50  pgf
 * forwpage now sets WFMODE
 *
 * Revision 1.32  1992/03/13  08:12:53  pgf
 * new paragraph behavior wrt blank lines, once again
 *
 * Revision 1.31  1992/03/03  21:57:01  pgf
 * fixed loop at buffer top/bottom in gotoeosent
 *
 * Revision 1.30  1992/02/17  08:49:47  pgf
 * took out unused var for saber
 *
 * Revision 1.29  1992/01/22  17:15:25  pgf
 * minor change to blank-line-skip for backward paragraph motions
 *
 * Revision 1.28  1992/01/22  16:58:20  pgf
 * paragraph motions now treat consecutive blank lines as a single paragraph
 * delimeter (note that this is independent of what the regexp says)
 *
 * Revision 1.27  1992/01/10  08:10:15  pgf
 * don't bother with list mode in next_column(), since this should _always_
 * give the "unlist-moded" column
 *
 * Revision 1.26  1992/01/05  00:06:13  pgf
 * split mlwrite into mlwrite/mlprompt/mlforce to make errors visible more
 * often.  also normalized message appearance somewhat.
 *
 * Revision 1.25  1992/01/03  23:35:47  pgf
 * screen motions (goto[emb]os()) now unconditionally return TRUE, to
 * eliminate oddness when buffer doesn't fill window
 *
 * paragraph and section motions no longer fail at the bottom of the
 * buffer (operators wouldn't work)
 *
 * paragraph motions are treated more vi-like.  in particular, whether
 * a para motion is character or line oriented is determined by where
 * the motion starts and finishes.  thanks to Eric Krohn for this tip.
 * (forward, the motion is line oriented if it moves off the current line
 * and it started at the beginning of the line.  otherwise it's character
 * oriented.  backward, it's similar, but it's line oriented if it starts
 * at the beginning _or_ end of a line.)
 *
 * Revision 1.24  1991/12/24  18:32:28  pgf
 * don't reset lastdot mark if we're moving to a mark and it's on behalf
 * of an opcmd
 *
 * Revision 1.23  1991/11/13  20:09:27  pgf
 * X11 changes, from dave lemke
 *
 * Revision 1.22  1991/11/10  22:28:17  pgf
 * the goto{end,begin}of{para,sec,sentence} motions now return TRUE, whether
 * they really went to a para,sec, or sentence, or went to the beginning or
 * end of the buffer because there were no more paras, secs, or sentences.
 * this makes operators work right.
 *
 * Revision 1.21  1991/11/08  13:08:04  pgf
 * moved firstchar() here, created lastchar(), and
 * eliminated unused atmark()
 *
 * Revision 1.20  1991/11/03  17:33:20  pgf
 * use new lregexec() routine to check for patterns in lines
 *
 * Revision 1.19  1991/11/01  14:38:00  pgf
 * saber cleanup
 *
 * Revision 1.18  1991/11/01  14:10:35  pgf
 * matchlen is now part of a regexp, not global
 *
 * Revision 1.17  1991/10/28  00:57:31  pgf
 * cleaned the sentence motions some more
 *
 * Revision 1.16  1991/10/27  16:09:06  pgf
 * improved the sentence motions
 *
 * Revision 1.15  1991/10/26  00:12:34  pgf
 * section, paragraph, and new sentence motions are all regex based
 *
 * Revision 1.14  1991/09/27  02:48:16  pgf
 * remove unused automatics
 *
 * Revision 1.13  1991/09/26  13:05:45  pgf
 * undid forw/backline optimization, since it causes flags to not be set,
 * and moved LIST mode to window
 *
 * Revision 1.12  1991/09/24  01:04:33  pgf
 * forwline and backline now do nothing if passed 0 arg
 *
 * Revision 1.11  1991/09/19  12:22:57  pgf
 * paragraphs now end at nroff-style section boundaries as well
 *
 * Revision 1.10  1991/08/07  12:35:07  pgf
 * added RCS log messages
 *
 * revision 1.9
 * date: 1991/08/06 15:10:26;
 * bug fix in forwline, and
 * global/local values
 *
 * revision 1.8
 * date: 1991/06/25 19:52:01;
 * massive data structure restructure
 *
 * revision 1.7
 * date: 1991/06/20 17:22:42;
 * fixed write-to-const-string problem in setnmmark
 *
 * revision 1.6
 * date: 1991/06/16 17:33:32;
 * added next_column() routine, along with converting to modulo tab processing
 *
 * revision 1.5
 * date: 1991/06/15 09:08:44;
 * added new forwchar_to_eol, and backchar_to_bol
 *
 * revision 1.4
 * date: 1991/06/03 10:17:45;
 * prompt for mark name in setnmmark if isnamedcmd
 *
 * revision 1.3
 * date: 1991/05/31 10:29:06;
 * fixed "last dot" mark code, and
 * added godotplus() for the operators
 *
 * revision 1.2
 * date: 1990/09/25 11:37:50;
 * took out old ifdef BEFORE code
 *
 * revision 1.1
 * date: 1990/09/21 10:24:42;
 * initial vile RCS revision
 */

#include	"estruct.h"
#include	"edef.h"

static	int	full_pages P((int, int));
static	int	half_pages P((int, int));

/* utility routine for 'forwpage()' and 'backpage()' */
static int
full_pages(f,n)
int	f, n;
{
	if (f == FALSE) {
		n = curwp->w_ntrows - 2;	/* Default scroll.	*/
		if (n <= 0)			/* Don't blow up if the */
			n = 1;			/* window is tiny.	*/
	}
#if	CVMVAS
	else if (n > 0)				/* Convert from pages	*/
		n *= curwp->w_ntrows;		/* to lines.		*/
#endif
	return n;
}

/* utility routine for 'forwhpage()' and 'backhpage()' */
static int
half_pages(f,n)
int	f, n;
{
	if (f == FALSE) {
		n = curwp->w_ntrows / 2;	/* Default scroll.	*/
		if (n <= 0)			/* Forget the overlap	*/
			n = 1;			/* if tiny window.	*/
	}
#if	CVMVAS
	else if (n > 0)				/* Convert from pages	*/
		n *= curwp->w_ntrows/2;		/* to lines.		*/
#endif
	return n;
}

/*
 * Implements the vi "0" command.
 *
 * Move the cursor to the beginning of the current line.
 */
/* ARGSUSED */
int
gotobol(f, n)
int f,n;
{
	DOT.o  = w_left_margin(curwp);
	return mvleftwind(TRUE, -w_val(curwp,WVAL_SIDEWAYS));
}

/*
 * Move the cursor backwards by "n" characters. If "n" is less than zero call
 * "forwchar" to actually do the move. Otherwise compute the new cursor
 * location. Error if you try and move out of the buffer. Set the flag if the
 * line pointer for dot changes.
 */
int
backchar(f, n)
int f;
register int n;
{
	register LINE	*lp;

	if (f == FALSE) n = 1;
	if (n < 0)
		return (forwchar(f, -n));
	while (n--) {
		if (DOT.o == w_left_margin(curwp)) {
			if ((lp=lBack(DOT.l)) == l_ref(curbp->b_line.l))
				return (FALSE);
			DOT.l  = l_ptr(lp);
			DOT.o  = llength(lp);
			curwp->w_flag |= WFMOVE;
		} else
			DOT.o--;
	}
	return (TRUE);
}

/*
 * Implements the vi "h" command.
 *
 * Move the cursor backwards by "n" characters. Stop at beginning of line.
 */
int
backchar_to_bol(f, n)
int f;
register int	n;
{

	if (f == FALSE) n = 1;
	if (n < 0)
		return forwchar_to_eol(f, -n);
	while (n--) {
		if (DOT.o == w_left_margin(curwp))
			return doingopcmd;
		else
			DOT.o--;
	}
	return TRUE;
}

/*
 * Implements the vi "$" command.
 *
 * Move the cursor to the end of the current line. Trivial. No errors.
 */
int
gotoeol(f, n)
int f,n;
{
	if (f == TRUE) {
		if (n > 0)
			--n;
		else if (n < 0)
			++n;
		forwline(f,n);
	}
	DOT.o  = lLength(DOT.l);
	curgoal = HUGE;
	return (TRUE);
}

/*
 * Move the cursor forwards by "n" characters. If "n" is less than zero call
 * "backchar" to actually do the move. Otherwise compute the new cursor
 * location, and move ".". Error if you try and move off the end of the
 * buffer. Set the flag if the line pointer for dot changes.
 */
int
forwchar(f, n)
int f;
register int	n;
{
	if (f == FALSE) n = 1;
	if (n < 0)
		return (backchar(f, -n));
	while (n--) {
		if (is_at_end_of_line(DOT)) {
			if (is_header_line(DOT, curbp) ||
					is_last_line(DOT,curbp))
				return (FALSE);
			DOT.l  = lFORW(DOT.l);
			DOT.o  = w_left_margin(curwp);
			curwp->w_flag |= WFMOVE;
		} else
			DOT.o++;
	}
	return (TRUE);
}

/*
 * Implements the vi "l" command.
 *
 * Move the cursor forwards by "n" characters. Don't go past end-of-line
 *
 * If the flag 'doingopcmd' is set, implements a vi "l"-like motion for
 * internal use.  The end-of-line test is off-by-one from the true "l" command
 * to allow for substitutions at the end of a line.
 */
int
forwchar_to_eol(f, n)
int f;
register int	n;
{
	if (f == FALSE) n = 1;
	if (n < 0)
		return backchar_to_bol(f, -n);
	while (n--) {
		if ((DOT.o + !doingopcmd) >= lLength(DOT.l) + (insertmode != 0))
			return doingopcmd;
		else
			DOT.o++;
	}
	return TRUE;
}

/*
 * Implements the vi "G" command.
 *
 * Move to a particular line (the argument).  Count from bottom of file if
 * argument is negative.
 */
int
gotoline(f, n)
int f,n;
{
	register int status;	/* status return */

	/* get an argument if one doesnt exist */
	if (f == FALSE) {
		return(gotoeob(f,n));
	}

	if (n == 0)		/* if a bogus argument...then leave */
		return(FALSE);

	DOT.o  = w_left_margin(curwp);
	if (n < 0) {
		DOT.l  = lBACK(curbp->b_line.l);
		status = backline(f, -n - 1 );
	} else {
		DOT.l  = lFORW(curbp->b_line.l);
		status = forwline(f, n-1);
	}
	if (status == TRUE)
		(void)firstnonwhite(FALSE,1);
	return(status);
}

/*
 * Goto the beginning of the buffer. Massive adjustment of dot. This is
 * considered to be hard motion; it really isn't if the original value of dot
 * is the same as the new value of dot.
 */
/* ARGSUSED */
int
gotobob(f, n)
int f,n;
{
	DOT.l  = lFORW(curbp->b_line.l);
	DOT.o  = w_left_margin(curwp);
	curwp->w_flag |= WFMOVE;
	return (TRUE);
}

/*
 * Move to the end of the buffer. Dot is always put at the end of the file.
 */
/* ARGSUSED */
int
gotoeob(f, n)
int f,n;
{
	DOT.l  = lBACK(curbp->b_line.l);
	curwp->w_flag |= WFMOVE;
	return firstnonwhite(FALSE,1);
}

/*
 * Implements the vi "H" command.
 *
 * Move to first (or nth) line in window
 */
int
gotobos(f,n)
int f,n;
{
	int	nn = curwp->w_ntrows;
	if (!f || n <= 0)
		n = 1;

	DOT.l = curwp->w_line.l;
	while (--n) {
		if (is_last_line(DOT,curbp))
			break;
		nn -= line_height(curwp, DOT.l);
		DOT.l = lFORW(DOT.l);
	}

	if (nn <= 0)		/* we went past the end of window */
		curwp->w_flag |= WFMOVE;
	return firstnonwhite(FALSE,1);
}

/*
 * Implements the vi "M" command.
 *
 * Move to the middle of lines displayed in window
 */
/* ARGSUSED */
int
gotomos(f,n)
int f,n;
{
	fast_ptr LINEPTR lp, head;
	int	half = (curwp->w_ntrows+1) / 2;

	head = curbp->b_line.l;
	for (n = 0, lp = curwp->w_line.l; !same_ptr(lp,head); lp = lFORW(lp)) {
		if (n < half)
			DOT.l = lp;
		if ((n += line_height(curwp, lp)) >= curwp->w_ntrows)
			break;
	}
	if (n < curwp->w_ntrows) {	/* then we hit eof before eos */
		half = (n+1) / 2;	/* go back up */
		for (n = 0, lp = curwp->w_line.l; !same_ptr(lp,head); lp = lFORW(lp)) {
			DOT.l = lp;
			if ((n += line_height(curwp, lp)) >= half)
				break;
		}
	}

	return firstnonwhite(FALSE,1);
}

/*
 * Implements the vi "L" command.
 *
 * Move to the last (or nth last) line in window
 */
int
gotoeos(f,n)
int f,n;
{
	int nn;
	if (f == FALSE || n <= 0)
		n = 1;

	/* first get to the end */
	DOT.l = curwp->w_line.l;
	nn = curwp->w_ntrows;
	while ((nn -= line_height(curwp,DOT.l)) > 0) {
		if (is_last_line(DOT,curbp))
			break;
		DOT.l = lFORW(DOT.l);
	}
#ifdef WMDLINEWRAP
	/* adjust if we pointed to a line-fragment */
	if (w_val(curwp,WMDLINEWRAP)
	 && nn < 0
	 && !same_ptr(DOT.l, curwp->w_line.l))
		DOT.l = lBACK(DOT.l);
#endif
	/* and then go back up */
	/* (we're either at eos or eof) */
	while (--n) {
		if (same_ptr(DOT.l, curwp->w_line.l))
			break;
		DOT.l = lBACK(DOT.l);
	}
	return firstnonwhite(FALSE,1);
}

/*
 * Implements the vi "j" command.
 *
 * Move forward by full lines. If the number of lines to move is less than
 * zero, call the backward line function to actually do it. The last command
 * controls how the goal column is set. No errors are
 * possible.
 */
int
forwline(f, n)
int f,n;
{
	register LINE	*dlp;

	if (f == FALSE) n = 1;
	if (n < 0)
		return (backline(f, -n));

	/* if we are on the last line as we start....fail the command */
	if (is_last_line(DOT, curbp))
		return(FALSE);

	/* if the last command was not a line move,
	   reset the goal column */
	if (curgoal < 0)
		curgoal = getccol(FALSE);

	/* and move the point down */
	dlp = l_ref(DOT.l);
	while (n-- > 0) {
		register LINE *nlp = lforw(dlp);
		if (nlp == l_ref(curbp->b_line.l))
			break;
		dlp = nlp;
	}

	/* resetting the current position */
	DOT.l  = l_ptr(dlp);
	DOT.o  = getgoal(dlp);
	curwp->w_flag |= WFMOVE;
	return (TRUE);
}

/*
 * Implements the vi "^" command.
 *
 * Move to the first nonwhite character on the current line.  No errors are
 * returned.
 */
/* ARGSUSED */
int
firstnonwhite(f,n)
int f,n;
{
	DOT.o  = firstchar(l_ref(DOT.l));
	if (DOT.o < w_left_margin(curwp)) {
		if (lLength(DOT.l) <= w_left_margin(curwp))
			DOT.o = w_left_margin(curwp);
		else
			DOT.o = lLength(DOT.l) - 1;
	}
	return TRUE;
}

/* ARGSUSED */
#if !SMALLER
int
lastnonwhite(f,n)
int f,n;
{
	DOT.o  = lastchar(l_ref(DOT.l));
	if (DOT.o < w_left_margin(curwp))
		DOT.o = w_left_margin(curwp);
	return TRUE;
}
#endif

/* return the offset of the first non-white character on the line,
	or -1 if there are no non-white characters on the line */
int
firstchar(lp)
LINE *lp;
{
	int off = w_left_margin(curwp);
	while ( off < llength(lp) && isblank(lgetc(lp, off)) )
		off++;
	if (off == llength(lp))
		return -1;
	return off;
}

/* return the offset of the next non-white character on the line,
	or -1 if there are no more non-white characters on the line */
int
nextchar(lp,off)
LINE *lp;
int off;
{
	while (off < llength(lp)) {
		if (!isspace(lgetc(lp,off)))
			return off;
		off++;
	}
	return -1;
}

/* return the offset of the last non-white character on the line
	or -1 if there are no non-white characters on the line */
int
lastchar(lp)
LINE *lp;
{
	int off = llength(lp)-1;
	while ( off >= 0 && isspace(lgetc(lp, off)) )
		off--;
	return off;
}

/*
 * Implements the vi "^M" command.
 *
 * Like 'forwline()', but goes to the first non-white character position.
 */
int
forwbline(f,n)
int f,n;
{
	int s;

	if (f == FALSE) n = 1;
	if ((s = forwline(f,n)) != TRUE)
		return (s);
	return firstnonwhite(FALSE,1);
}

/*
 * Implements the vi "-" command.
 *
 * Like 'backline()', but goes to the first non-white character position.
 */
int
backbline(f,n)
int f,n;
{
	int s;

	if (f == FALSE) n = 1;
	if ((s = backline(f,n)) != TRUE)
		return (s);
	return firstnonwhite(FALSE,1);
}

/*
 * Implements the vi "k" command.
 *
 * This function is like "forwline", but goes backwards.
 */
int
backline(f, n)
int f,n;
{
	register LINE	*dlp;

	if (f == FALSE) n = 1;
	if (n < 0)
		return (forwline(f, -n));

	/* if we are on the first line as we start....fail the command */
	if (is_first_line(DOT, curbp))
		return(FALSE);

	/* if the last command was not note a line move,
	   reset the goal column */
	if (curgoal < 0)
		curgoal = getccol(FALSE);

	/* and move the point up */
	dlp = l_ref(DOT.l);
	while (n-- && lback(dlp) != l_ref(curbp->b_line.l))
		dlp = lback(dlp);

	/* reseting the current position */
	DOT.l  = l_ptr(dlp);
	DOT.o  = getgoal(dlp);
	curwp->w_flag |= WFMOVE;
	return (TRUE);
}

#if	WORDPRO
/*
 * Go to the beginning of the current paragraph.
 */
int
gotobop(f,n)
int f,n;
{
	MARK odot;
	int was_on_empty;
	int fc;

	if (!f) n = 1;

	was_on_empty = (lLength(DOT.l) == 0);
	odot = DOT;

	fc = firstchar(l_ref(DOT.l));
	if (doingopcmd &&
		((fc >= 0 && DOT.o <= fc) || fc < 0) &&
		!is_first_line(DOT,curbp)) {
		backchar(TRUE,DOT.o+1);
		pre_op_dot = DOT;
	}
	while (n) {
		if (findpat(TRUE, 1, b_val_rexp(curbp,VAL_PARAGRAPHS)->reg,
							REVERSE) != TRUE) {
			(void)gotobob(f,n);
		} else if (lLength(DOT.l) == 0) {
			/* special case -- if we found an empty line,
				and it's adjacent to where we started,
				skip all adjacent empty lines, and try again */
			if ( (was_on_empty && lForw(DOT.l) == l_ref(odot.l)) ||
				(n > 0 && llength(lForw(DOT.l)) == 0) ) {
				/* then we haven't really found what we
					wanted.  keep going */
				skipblanksb();
				continue;
			}
		}
		n--;
	}
	if (doingopcmd) {
		fc = firstchar(l_ref(DOT.l));
		if (!sameline(DOT,odot) &&
			(pre_op_dot.o > lastchar(l_ref(pre_op_dot.l))) &&
			((fc >= 0 && DOT.o <= fc) || fc < 0)) {
			fulllineregions = TRUE;
		}
	}
	return TRUE;
}

/*
 * Go to the end of the current paragraph.
 */
int
gotoeop(f,n)
int f,n;
{
	MARK odot;
	int was_at_bol;
	int was_on_empty;
	int fc;

	if (!f) n = 1;

	fc = firstchar(l_ref(DOT.l));
	was_on_empty = (lLength(DOT.l) == 0);
	was_at_bol = ((fc >= 0 && DOT.o <= fc) || fc < 0);
	odot = DOT;

	while (n) {
		if (findpat(TRUE, 1, b_val_rexp(curbp,VAL_PARAGRAPHS)->reg,
						FORWARD) != TRUE) {
			DOT = curbp->b_line;
		} else if (lLength(DOT.l) == 0) {
			/* special case -- if we found an empty line. */
			/* either as the very next line, or at the end of
				our search */
			if ( (was_on_empty && lBack(DOT.l) == l_ref(odot.l)) ||
				(n > 0 && llength(lBack(DOT.l)) == 0) ) {
				/* then we haven't really found what we
					wanted.  keep going */
				skipblanksf();
				continue;
			}
		}
		n--;
	}
	if (doingopcmd) {
		/* if we're now at the beginning of a line and we can back up,
		  do so to avoid eating the newline and leading whitespace */
		fc = firstchar(l_ref(DOT.l));
		if (((fc >= 0 && DOT.o <= fc) || fc < 0) &&
			!is_first_line(DOT,curbp) &&
			!sameline(DOT,odot) ) {
			backchar(TRUE,DOT.o+1);
		}
		/* if we started at the start of line, eat the whole line */
		if (!sameline(DOT,odot) && was_at_bol)
			fulllineregions = TRUE;
	}
	return TRUE;
}

void
skipblanksf()
{
	while (lForw(DOT.l) != l_ref(curbp->b_line.l) && lLength(DOT.l) == 0)
		DOT.l = lFORW(DOT.l);
}

void
skipblanksb()
{
	while (lBack(DOT.l) != l_ref(curbp->b_line.l) && lLength(DOT.l) == 0)
		DOT.l = lBACK(DOT.l);
}

#if STUTTER_SEC_CMD
getstutter()
{
	int this1key;
	if (!clexec) {
		this1key = last1key;
		kbd_seq();
		if (this1key != last1key) {
			TTbeep();
			return FALSE;
		}
	}
	return TRUE;
}
#endif

/*
 * Go to the beginning of the current section (or paragraph if no section
 * marker found).
 */
int
gotobosec(f,n)
int f,n;
{
#if STUTTER_SEC_CMD
	if (!getstutter())
		return FALSE;
#endif
	if (findpat(f, n, b_val_rexp(curbp,VAL_SECTIONS)->reg,
							REVERSE) != TRUE) {
		(void)gotobob(f,n);
	}
	return TRUE;
}

/*
 * Go to the end of the current section (or paragraph if no section marker
 * found).
 */
int
gotoeosec(f,n)
int f,n;
{
#if STUTTER_SEC_CMD
	if (!getstutter())
		return FALSE;
#endif
	if (findpat(f, n, b_val_rexp(curbp,VAL_SECTIONS)->reg,
							FORWARD) != TRUE) {
		DOT = curbp->b_line;
	}
	return TRUE;
}

/*
 * Go to the beginning of the current sentence.
 */
int
gotobosent(f,n)
int f,n;
{
	MARK savepos;
	int looped = 0;
	int extra;
	register regexp *exp;
	register int s;

	savepos = DOT;
	exp = b_val_rexp(curbp,VAL_SENTENCES)->reg;
 top:
	extra = 0;
	if (findpat(f, n, exp, REVERSE) != TRUE) {
		return gotobob(f,n);
	}
	s = forwchar(TRUE, exp->mlen?exp->mlen:1);
	while (s && (is_at_end_of_line(DOT) || isspace(char_at(DOT)))) {
		s = forwchar(TRUE,1);
		extra++;
	}
	if (n == 1 && samepoint(savepos,DOT)) { /* try again */
		if (looped > 10)
			return FALSE;
		s = backchar(TRUE, (exp->mlen?exp->mlen:1) + extra + looped);
		while (s && is_at_end_of_line(DOT))
			s = backchar(TRUE,1);
		looped++;
		goto top;

	}
	return TRUE;
}

/*
 * Go to the end of the current sentence.
 */
int
gotoeosent(f,n)
int f,n;
{
	register regexp *exp;
	register int s;

	exp = b_val_rexp(curbp,VAL_SENTENCES)->reg;
	/* if we're on the end of a sentence now, don't bother scanning
		further, or we'll miss the immediately following sentence */
	if (!(lregexec(exp, l_ref(DOT.l), DOT.o, lLength(DOT.l)) &&
				exp->startp[0] - l_ref(DOT.l)->l_text == DOT.o)) {
		if (findpat(f, n, exp, FORWARD) != TRUE) {
			DOT = curbp->b_line;
			return TRUE;
		}
	}
	s = forwchar(TRUE, exp->mlen?exp->mlen:1);
	while (s && (is_at_end_of_line(DOT) || isspace(char_at(DOT)))) {
		s = forwchar(TRUE,1);
	}
	return TRUE;
}

#endif /* WORDPRO */

/*
 * This routine, given a pointer to a LINE, and the current cursor goal
 * column, return the best choice for the offset. The offset is returned.
 * Used by "C-N" and "C-P".
 */
int
getgoal(dlp)
register LINE	*dlp;
{
	register int	c;
	register int	col;
	register int	newcol;
	register int	dbo;

	col = 0;
	dbo = w_left_margin(curwp);
	while (dbo < llength(dlp)) {
		c = lgetc(dlp, dbo);
		newcol = next_column(c,col);
		if (newcol > curgoal)
			break;
		col = newcol;
		++dbo;
	}
	return (dbo);
}

/* return the next column index, given the current char and column */
int
next_column(c,col)
int c, col;
{
	if (c == '\t')
		return nextab(col);
	else if (!isprint(c))
		return col+2;
	else
		return col+1;
}

/*
 * Implements the vi "^F" command.
 *
 * Scroll forward by a specified number of lines, or by a full page if no
 * argument.
 */
int
forwpage(f, n)
int f;
register int	n;
{
	fast_ptr LINEPTR lp;
	int	status;

	if ((n = full_pages(f,n)) < 0)
		return backpage(f, -n);

	if ((status = !same_ptr(lFORW(DOT.l), buf_head(curbp))) == TRUE) {
		lp = curwp->w_line.l;
		while ((n -= line_height(curwp,lp)) >= 0
		  &&   !same_ptr(lp, buf_head(curbp)))
			lp = lFORW(lp);
		if (n < 0)
			curwp->w_line.l = lp;
		DOT.l  = lp;
		DOT.o  = w_left_margin(curwp);
		curwp->w_flag |= WFHARD|WFMODE;
	}
	return status;
}

/*
 * Implements the vi "^B" command.
 *
 * This command is like "forwpage", but it goes backwards.
 */
int
backpage(f, n)
int f;
register int	n;
{
	fast_ptr LINEPTR lp;
	int	status;

	if ((n = full_pages(f,n)) < 0)
		return forwpage(f, -n);

	lp = curwp->w_line.l;
	if ((status = !same_ptr(lBACK(lp), buf_head(curbp))) == TRUE) {
		while ((n -= line_height(curwp,lp)) >= 0
		  &&   !same_ptr(lBACK(lp), buf_head(curbp)))
			lp = lBACK(lp);
		curwp->w_line.l = lp;
		DOT.l  = lp;
		DOT.o  = w_left_margin(curwp);
		curwp->w_flag |= WFHARD;
	}
	return status;
}

/*
 * Implements the vi "^D" command.
 *
 * Scroll forward by a half-page.  If a repeat count is given, interpret that
 * as the number of half-pages to scroll.
 *
 * Unlike vi, the CVMVAS option causes the repeat-count to be interpreted as
 * half-page, rather than lines.
 */
int
forwhpage(f, n)
int f;
register int	n;
{
	fast_ptr LINEPTR  llp, dlp;
	int	status;

	if ((n = half_pages(f,n)) < 0)
		return backhpage(f, -n);

	llp = curwp->w_line.l;
	dlp = DOT.l;
	if ((status = !same_ptr(lFORW(dlp), buf_head(curbp))) == TRUE) {
		while ((n -= line_height(curwp,dlp)) >= 0
		  &&   !same_ptr(lFORW(dlp), buf_head(curbp))) {
			llp = lFORW(llp);
			dlp = lFORW(dlp);
		}
		curwp->w_line.l = llp;
		DOT.l  = dlp;
		curwp->w_flag |= WFHARD|WFKILLS;
	}
	(void)firstnonwhite(FALSE,1);
	return status;
}

/*
 * Implements the vi "^U" command.
 *
 * This command is like "forwpage", but it goes backwards.  It returns false
 * only if the cursor is on the first line of the buffer.
 *
 * Unlike vi, the CVMVAS option causes the repeat-count to be interpreted as
 * half-pages, rather than lines.
 */
int
backhpage(f, n)
int f;
register int	n;
{
	fast_ptr LINEPTR llp, dlp;
	int	status;

	if ((n = half_pages(f,n)) < 0)
		return forwhpage(f, -n);

	llp = curwp->w_line.l;
	dlp = DOT.l;
	if ((status = !same_ptr(lBACK(dlp), buf_head(curbp))) == TRUE) {
		while ((n -= line_height(curwp,dlp)) >= 0
		  &&   !same_ptr(lBACK(dlp), buf_head(curbp))) {
			llp = lBACK(llp);
			dlp = lBACK(dlp);
		}
		curwp->w_line.l = llp;
		DOT.l  = dlp;
		curwp->w_flag |= WFHARD|WFINS;
	}
	(void)firstnonwhite(FALSE,1);
	return status;
}

/*
 * Implements the vi "m" command.
 *
 * Set the named mark in the current window to the value of "." in the window.
 */
/* ARGSUSED */
int
setnmmark(f,n)
int f,n;
{
	int c,i;

	if (clexec || isnamedcmd) {
		int stat;
		static char cbuf[2];
		if ((stat=mlreply("Set mark: ", cbuf, 2)) != TRUE)
			return stat;
		c = cbuf[0];
	} else {
		c = kbd_key();
	}
	if (c < 'a' || c > 'z') {
		TTbeep();
		mlforce("[Invalid mark name]");
		return FALSE;
	}

	if (curbp->b_nmmarks == NULL) {
		curbp->b_nmmarks = typeallocn(struct MARK,26);
		if (curbp->b_nmmarks == NULL)
			return no_memory("named-marks");
		for (i = 0; i < 26; i++) {
			curbp->b_nmmarks[i] = nullmark;
		}
	}

	curbp->b_nmmarks[c-'a'] = DOT;
	mlwrite("[Mark %c set]",c);
	return TRUE;
}

/* ARGSUSED */
int
golinenmmark(f,n)
int f,n;
{
	int c;
	register int s;

	s = getnmmarkname(&c);
	if (s != TRUE)
		return s;
	s = gonmmark(c);
	if (s != TRUE)
		return s;

	return firstnonwhite(FALSE,1);

}

/* ARGSUSED */
int
goexactnmmark(f,n)
int f,n;
{
	int c;
	register int s;

	s = getnmmarkname(&c);
	if (s != TRUE)
		return s;

	return gonmmark(c);
}

/* get the name of the mark to use.  interactively, "last dot" is
	represented by stuttering the goto-mark command.  from
	the command line, it's always named ' or `.  I suppose
	this is questionable. */
int
getnmmarkname(cp)
int *cp;
{
	int c;
	int this1key;
	int useldmark;

	if (clexec || isnamedcmd) {
		int stat;
		static char cbuf[2];
		if ((stat=mlreply("Goto mark: ", cbuf, 2)) != TRUE)
			return stat;
		c = cbuf[0];
		useldmark = (c == '\'' || c == '`');
	} else {
		this1key = last1key;
		c = kbd_key();
		useldmark = (last1key == this1key);  /* usually '' or `` */
	}

	if (useldmark)
		c = '\'';

	*cp = c;
	return TRUE;
}

int
gonmmark(c)
int c;
{
	register MARK *markp;
	MARK tmark;
	int found;

	if (!islower(c) && c != '\'') {
		TTbeep();
		mlforce("[Invalid mark name]");
		return FALSE;
	}

	markp = NULL;

	if (c == '\'') { /* use the 'last dot' mark */
		markp = &(curwp->w_lastdot);
	} else if (curbp->b_nmmarks != NULL) {
		markp = &(curbp->b_nmmarks[c-'a']);
	}

	found = FALSE;
	/* if we have any named marks, and the one we want isn't null */
	if (markp != NULL && !samepoint((*markp), nullmark)) {
		register LINE *lp;
		for_each_line(lp, curbp) {
			if (l_ref((*markp).l) == lp) {
				found = TRUE;
				break;
			}
		}
	}
	if (!found) {
		TTbeep();
		mlforce("[Mark not set]");
		return (FALSE);
	}

	/* save current dot */
	tmark = DOT;

	/* move to the selected mark */
	DOT = *markp;

	if (!doingopcmd)	/* reset last-dot-mark to old dot */
		curwp->w_lastdot = tmark;

	curwp->w_flag |= WFMOVE;
	return (TRUE);
}

/*
 * Set the mark in the current window to the value of "." in the window. No
 * errors are possible.
 */
int
setmark()
{
	MK = DOT;
	return (TRUE);
}

/* ARGSUSED */
int
gomark(f,n)
int f,n;
{
	DOT = MK;
	curwp->w_flag |= WFMOVE;
	return (TRUE);
}

/* this odd routine puts us at the internal mark, plus an offset of lines */
/*  n == 1 leaves us at mark, n == 2 one line down, etc. */
/*  this is for the use of stuttered commands, and line oriented regions */
int
godotplus(f,n)
int f,n;
{
	int s;
	if (!f || n == 1) {
		return firstnonwhite(FALSE,1);
	}
	if (n < 1)
		return (FALSE);
	s = forwline(TRUE,n-1);
	if (s && is_header_line(DOT, curbp))
		s = backline(FALSE,1);
	if (s == TRUE)
		(void)firstnonwhite(FALSE,1);
	return s;
}

/*
 * Swap the values of "." and "mark" in the current window. This is pretty
 * easy, because all of the hard work gets done by the standard routine
 * that moves the mark about. The only possible error is "no mark".
 */
void
swapmark()
{
	MARK odot;

	if (samepoint(MK, nullmark)) {
		mlforce("BUG: No mark ");
		return;
	}
	odot = DOT;
	DOT = MK;
	MK = odot;
	curwp->w_flag |= WFMOVE;
	return;
}

#if OPT_MOUSE
/*
 * Given row & column from the screen, set the MK value.
 * Note that the resulting position may be past the end-of-buffer.
 */
int
setwmark(row, col)
int row, col;
{
	MARK	save;
	fast_ptr LINEPTR dlp;

	save = DOT;
	if (row == mode_row(curwp)) {
		(void) gotoeos(FALSE,1);
		DOT.l = lFORW(DOT.l);
		DOT.o = w_left_margin(curwp);
	} else {	/* move to the right row */
		row -= curwp->w_toprow;
		dlp = curwp->w_line.l;	/* get pointer to 1st line */
		while ((row -= line_height(curwp,dlp)) >= 0
		  &&   !same_ptr(dlp, curbp->b_line.l))
			dlp = lFORW(dlp);
		DOT.l = dlp;			/* set dot line pointer */

		/* now move the dot over until near the requested column */
#ifdef WMDLINEWRAP
		if (w_val(curwp,WMDLINEWRAP))
			col += term.t_ncol * (row+line_height(curwp,dlp));
#endif
		DOT.o = col2offs(curwp, dlp, col);

		/* don't allow the cursor to be set past end of line unless we
		 * are in insert mode
		 */
		if (DOT.o >= lLength(dlp) && DOT.o > w_left_margin(curwp) && !insertmode)
			DOT.o--;
	}
	MK  = DOT;
	DOT = save;
	return TRUE;
}

/*
 * Given row & column from the screen, set the curwp and DOT values.
 * Note that the resulting position may be past the end-of-buffer.
 */
int
setcursor (row, col)
int	row;
int	col;
{
	register WINDOW *wp0 = curwp;
	register WINDOW *wp1;

	if ((wp1 = row2window(row)) != 0
	 && set_curwp(wp1)
	 && setwmark(row, col)) {
		if (insertmode != FALSE
		 && b_val(wp1->w_bufp, MDVIEW)
		 && b_val(wp1->w_bufp, MDSHOWMODE)) {
			if (b_val(wp0->w_bufp, MDSHOWMODE))
				wp0->w_flag |= WFMODE;
			if (b_val(wp1->w_bufp, MDSHOWMODE))
				wp1->w_flag |= WFMODE;
			insertmode = FALSE;
		}
		return gomark(FALSE,1);
	}

	return FALSE;
}
#endif
