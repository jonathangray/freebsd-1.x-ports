/*
 * The routines in this file implement commands that work word or a
 * paragraph at a time.  There are all sorts of word mode commands.  If I
 * do any sentence mode commands, they are likely to be put in this file. 
 *
 * $Log: word.c,v $
 * Revision 1.1  1994/02/01 03:29:44  jkh
 * Initial revision
 *
 * Revision 1.32  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.31  1993/08/18  20:37:17  pgf
 * as a stop-gap measure, added '>' to the characters that the formatregion
 * code knows about.  there's a note in the buglist about how this should
 * be regexp-able.
 *
 * Revision 1.30  1993/08/13  16:32:50  pgf
 * tom's 3.58 changes
 *
 * Revision 1.29  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.28  1993/06/02  14:28:47  pgf
 * see tom's 3.48 CHANGES
 *
 * Revision 1.27  1993/05/24  15:21:37  pgf
 * tom's 3.47 changes, part a
 *
 * Revision 1.26  1993/04/28  14:34:11  pgf
 * see CHANGES, 3.44 (tom)
 *
 * Revision 1.25  1993/04/01  12:53:33  pgf
 * removed redundant includes and declarations
 *
 * Revision 1.24  1993/03/05  17:50:54  pgf
 * see CHANGES, 3.35 section
 *
 * Revision 1.23  1992/12/23  09:28:15  foxharp
 * fix counts on 'J' command
 *
 * Revision 1.22  1992/12/16  21:19:07  foxharp
 * fixups (from tom dickey) for 'J' command, wrt blank lines and counts
 *
 * Revision 1.21  1992/08/20  23:40:48  foxharp
 * typo fixes -- thanks, eric
 *
 * Revision 1.20  1992/06/12  22:23:42  foxharp
 * changes for separate 'comments' r.e. for formatregion
 *
 * Revision 1.19  1992/06/03  08:40:22  foxharp
 * initialize comment_char in formatregion, to suppress gcc warning
 *
 * Revision 1.18  1992/05/31  22:11:11  foxharp
 * added C and shell comment reformatting
 *
 * Revision 1.17  1992/05/19  08:55:44  foxharp
 * more prototype and shadowed decl fixups
 *
 * Revision 1.16  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.15  1992/03/05  09:19:55  pgf
 * changed some mlwrite() to mlforce(), due to new terse support
 *
 * Revision 1.14  1991/11/08  13:02:46  pgf
 * ifdefed unneeded funcs
 *
 * Revision 1.13  1991/11/03  17:33:20  pgf
 * use new lregexec() routine to check for patterns in lines
 *
 * Revision 1.12  1991/11/01  14:37:22  pgf
 * saber cleanup
 *
 * Revision 1.11  1991/10/28  14:26:45  pgf
 * eliminated TABVAL and fillcol macros -- now use curtabval and the VAL_FILL
 * directly
 *
 * Revision 1.10  1991/10/28  01:01:06  pgf
 * added start offset and end offset to regexec calls
 *
 * Revision 1.9  1991/10/27  01:57:45  pgf
 * changed usage of issecbegin() in formatregion to use regexec instead
 *
 * Revision 1.8  1991/08/09  13:17:52  pgf
 * formatregion now restarts with each fresh paragraph, so you can
 * format an entire file at once, without collapsing it all into a
 * single paragraph
 *
 * Revision 1.7  1991/08/07  12:35:07  pgf
 * added RCS log messages
 *
 * revision 1.6
 * date: 1991/08/06 15:27:52;
 * removed old rdonly check
 * 
 * revision 1.5
 * date: 1991/06/28 10:54:14;
 * suppress trailing space after paragraph reformat
 * 
 * revision 1.4
 * date: 1991/06/25 19:53:45;
 * massive data structure restructure
 * 
 * revision 1.3
 * date: 1991/06/06 13:58:09;
 * added auto-indent mode
 * 
 * revision 1.2
 * date: 1991/03/26 17:02:20;
 * formatting now knows about ! and ? as well as .
 * 
 * revision 1.1
 * date: 1990/09/21 10:26:25;
 * initial vile RCS revision
 */

#include	"estruct.h"
#include	"edef.h"

/* Word wrap on n-spaces. Back-over whatever precedes the point on the current
 * line and stop on the first word-break or the beginning of the line. If we
 * reach the beginning of the line, jump back to the end of the word and start
 * a new line.	Otherwise, break the line at the word-break, eat it, and jump
 * back to the end of the word.
 * Returns TRUE on success, FALSE on errors.
 */
/* ARGSUSED */
int
wrapword(f,n)
int f,n;
{
	register int cnt;	/* size of word wrapped to next line */
	register int c;		/* character temporary */

	/* backup from the <NL> 1 char */
	if (!backchar(FALSE, 1))
		return(FALSE);

	/* back up until we aren't in a word,
	   make sure there is a break in the line */
	cnt = 0;
	while (c = char_at(DOT), !isspace(c)) {
		cnt++;
		if (!backchar(FALSE, 1))
			return(FALSE);
		/* if we make it to the beginning, start a new line */
		if (DOT.o == 0) {
			(void)gotoeol(FALSE, 0);
			return(lnewline());
		}
	}

	/* delete the forward white space */
	if (!ldelete(1L, FALSE))
		return(FALSE);

	/* put in a end of line */
	if (!newline(TRUE,1))
		return FALSE;

	/* and past the first word */
	while (cnt-- > 0) {
		if (forwchar(FALSE, 1) == FALSE)
			return(FALSE);
	}
	return(TRUE);
}


/*
 * Implements the vi "w" command.
 *
 * Move the cursor forward by the specified number of words. All of the motion
 * is done by "forwchar". Error if you try and move beyond the buffer's end.
 *
 * Returns of SORTOFTRUE result if we're doing a non-delete operation.  
 * Whitespace after a word is always included on deletes (and non-operations,
 * of course), but only on intermediate words for other operations, for
 * example.  The last word of non-delete ops does _not_ include its whitespace.
 */
int
forwviword(f, n)
int f,n;
{
	int s;

	if (n < 0)
		return (backword(f, -n));
	setchartype();
	if (forwchar(FALSE, 1) == FALSE)
		return (FALSE);
	while (n--) {
		int any = 0;
		while (((s = isnewviwordf()) == FALSE) || 
				(s == SORTOFTRUE && n != 0)) {
			if (forwchar(FALSE, 1) == FALSE)
				return (any != 0);
			any++;
		}
	}
	return TRUE;
}

/*
 * Implements the vi "W" command.
 *
 * Move the cursor forward by the specified number of words. All of the motion
 * is done by "forwchar". Error if you try and move beyond the buffer's end.
 */
int
forwword(f, n)
int f,n;
{
	int s;

	if (n < 0)
		return (backword(f, -n));
	setchartype();
	if (forwchar(FALSE, 1) == FALSE)
		return (FALSE);
	while (n--) {
		int any = 0;
		while (((s = isnewwordf()) == FALSE) || 
				(s == SORTOFTRUE && n != 0)) {
			if (forwchar(FALSE, 1) == FALSE)
				return (any != 0);
			any++;
		}
	}
	return(TRUE);
}

/*
 * Implements the vi "e" command.
 *
 * Move the cursor forward by the specified number of words. All of the motion
 * is done by "forwchar". Error if you try and move beyond the buffer's end.
 */
int
forwviendw(f, n)
int f,n;
{
	int s = FALSE;
	if (!f)
		n = 1;
	else if (n <= 0)
		return (FALSE);
	if (forwchar(FALSE, 1) == FALSE)
		return (FALSE);
	setchartype();
	while (n--) {
		int	any = 0;
		while ((s = isendviwordf()) == FALSE) {
			if (forwchar(FALSE, 1) == FALSE)
				return (any != 0);
			any++;
		}

	}
	if (s == SORTOFTRUE)
		return TRUE;
	else
		return backchar(FALSE, 1);
}

/*
 * Implements the vi "E" command.
 *
 * Move the cursor forward by the specified number of words. All of the motion
 * is done by "forwchar". Error if you try and move beyond the buffer's end.
 */
int
forwendw(f, n)
int f,n;
{
	int s = FALSE;
	if (!f)
		n = 1;
	else if (n <= 0)
		return (FALSE);
	if (forwchar(FALSE, 1) == FALSE)
		return (FALSE);
	setchartype();
	while (n--) {
		int	any = 0;
		while ((s = isendwordf()) == FALSE) {
			if (forwchar(FALSE, 1) == FALSE)
				return (any != 0);
			any++;
		}

	}
	if (s == SORTOFTRUE)
		return TRUE;
	else
		return backchar(FALSE, 1);
}

/*
 * Implements the vi "b" command.
 *
 * Move the cursor backward by "n" words. All of the details of motion are
 * performed by the "backchar" and "forwchar" routines. Error if you try to
 * move beyond the buffers.
 */
int
backviword(f, n)
int f,n;
{
	if (n < 0)
		return (forwword(f, -n));
	if (backchar(FALSE, 1) == FALSE)
		return (FALSE);
	setchartype();
	while (n--) {
		int	any = 0;
		while (isnewviwordb() == FALSE) {
			any++;
			if (backchar(FALSE, 1) == FALSE)
				return (any != 0);
		}
	}
	return (forwchar(FALSE, 1));
}

/*
 * Implements the vi "B" command.
 *
 * Move the cursor backward by "n" words. All of the details of motion are
 * performed by the "backchar" and "forwchar" routines. Error if you try to
 * move beyond the buffers.
 */
int
backword(f, n)
int f,n;
{
	if (n < 0)
		return (forwword(f, -n));
	if (backchar(FALSE, 1) == FALSE)
		return (FALSE);
	setchartype();
	while (n--) {
		int	any = 0;
		while (isnewwordb() == FALSE) {
			any++;
			if (backchar(FALSE, 1) == FALSE)
				return (any != 0);
		}
	}
	return (forwchar(FALSE, 1));
}

int
joinregion()
{
	register int status;
	register int doto, c;
	LINE	*end;
	REGION	region;
	int	done = FALSE;

	if ((status = getregion(&region)) == TRUE
	 && (status = !is_last_line(region.r_orig, curbp)) == TRUE) {

		DOT = region.r_orig;
		end = l_ref(region.r_end.l);
		fulllineregions = FALSE;

		while (!done) {
			c = EOS;
			status = gotoeol(FALSE,1);
			if (DOT.o > 0)
				c = lGetc(DOT.l, DOT.o-1);
			if (status == TRUE) status = setmark();
			if (status == TRUE) status = forwline(FALSE,1);
			if (status == TRUE) status = firstnonwhite(FALSE,1);

			done = ((l_ref(DOT.l) == end) || (lForw(DOT.l) == end));
			if (status == TRUE) status = killregion();
			if (status != TRUE)
				break;

			doto = DOT.o;
			if (doto == 0)
				;	/* join at column 0 to empty line */
			else if (doto < lLength(DOT.l)) {
				if (lGetc(DOT.l, doto) == ')')
					;	/* join after parentheses */
				else if (lGetc(DOT.l, doto-1) == '.')
					status = linsert(2,' ');
				else if (!isspace(c))
					status = linsert(1,' ');
			}
		}
	}
	rls_region();
	return status;
}

int
joinlines(f,n)
int f,n;
{
	extern CMDFUNC f_godotplus;
	havemotion = &f_godotplus;
	return(operjoin(f,n));
}

int
formatregion()
{
	register int c;			/* current char during scan	*/
	register int wordlen;		/* length of current word	*/
	register int clength;		/* position on line during fill	*/
	register int i;			/* index during word copy	*/
	register int newlen;		/* tentative new line length	*/
	register int finished;		/* Are we at the End-Of-Paragraph? */
	register int firstflag;		/* first word? (needs no space)	*/
	register int is_comment;	/* doing a comment block?	*/
	register int comment_char = -1;	/* # or *, for shell or C	*/
	register int at_nl = TRUE;	/* just saw a newline?		*/
	fast_ptr LINEPTR pastline;	/* pointer to line just past EOP */
	register int sentence;		/* was the last char a period?	*/
	char wbuf[NSTRING];		/* buffer for current word	*/
	int secondindent;
	regexp *expP, *expC;
	int s;
	
	if (!sameline(MK, DOT)) {
		REGION region;

		getregion(&region);
		if (sameline(region.r_orig, MK))
			swapmark();
		rls_region();
	}
	pastline = MK.l;
	if (!same_ptr(pastline, curbp->b_line.l))
		pastline = lFORW(pastline);

	expP = b_val_rexp(curbp,VAL_PARAGRAPHS)->reg;
	expC = b_val_rexp(curbp,VAL_COMMENTS)->reg;
 	finished = FALSE;
 	while (finished != TRUE) {  /* i.e. is FALSE or SORTOFTRUE */
		while (lregexec(expP, l_ref(DOT.l), 0, lLength(DOT.l)) ||
			lregexec(expC, l_ref(DOT.l), 0, lLength(DOT.l)) ) {
			DOT.l = lFORW(DOT.l);
			if (same_ptr(DOT.l, pastline))
				return setmark();
		}

		secondindent = indentlen(l_ref(DOT.l));
		
		/* go forward to get the indent for the second
			and following lines */
		DOT.l = lFORW(DOT.l);

		if (!same_ptr(DOT.l, pastline)) {
			secondindent = indentlen(l_ref(DOT.l));
		}
			
		/* and back where we should be */
		DOT.l = lBACK(DOT.l);
		(void)firstnonwhite(FALSE,1);
		
		clength = indentlen(l_ref(DOT.l));
		wordlen = 0;
		sentence = FALSE;

		is_comment = ( ((c = char_at(DOT)) == '#') ||
				(c == '>') ||
				(c == '*') ||
				((c == '/') &&
				DOT.o+1 < lLength(DOT.l) &&
				 lGetc(DOT.l,DOT.o+1) == '*'));

		if (is_comment)
			comment_char = (c == '#' || c == '>') ? c :'*';

		/* scan through lines, filling words */
		firstflag = TRUE;
		finished = FALSE;
		while (finished == FALSE) { /* i.e. is not TRUE  */
					    /* or SORTOFTRUE */
			if (interrupted) return ABORT;

			/* get the next character */
			if (is_at_end_of_line(DOT)) {
				c = ' ';
				DOT.l = lFORW(DOT.l);
				if (same_ptr(DOT.l, pastline)) {
					finished = TRUE;
				} else if (
				lregexec(expP, l_ref(DOT.l), 0, lLength(DOT.l)) ||
				lregexec(expC, l_ref(DOT.l), 0, lLength(DOT.l))) {
					/* we're at a section break */
					finished = SORTOFTRUE;
				}
				DOT.l = lBACK(DOT.l);
				at_nl = TRUE;
			} else {
				c = char_at(DOT);
				if (at_nl && ( isspace(c) ||
					(is_comment && c == comment_char)))
					c = ' ';
				else
					at_nl = FALSE;
			}
			/* and then delete it */
			if (finished == FALSE) {
				s = ldelete(1L, FALSE);
				if (s != TRUE) return s;
			}

			/* if not a separator, just add it in */
			if (!isblank(c)) {
				/* was it the end of a "sentence"? */
				sentence = (c == '.' || c == '?' || c == '!');
				if (wordlen < NSTRING - 1)
					wbuf[wordlen++] = c;
			} else if (wordlen) {
				/* at a word break with a word waiting */
				/* calculate tentative new length
							with word added */
				newlen = clength + 1 + wordlen;
				if (newlen <= b_val(curbp,VAL_FILL)) {
					/* add word to current line */
					if (!firstflag) {
						/* the space */
						s = linsert(1, ' ');
						if (s != TRUE) return s;
						++clength;
					} 
				} else {
			                if (lnewline() == FALSE ||
					((i=secondindent/curtabval)!=0 &&
			                	   linsert(i, '\t')==FALSE) ||
					((i=secondindent%curtabval)!=0 &&
				                   linsert(i,  ' ')==FALSE)) {
			                        return FALSE;
			                }
					clength = secondindent;
					firstflag = TRUE;
				}
				if (firstflag && is_comment &&
						strncmp("/*",wbuf,2)) {
					s = linsert(1, comment_char);
					if (s != TRUE) return s;
					s = linsert(1, ' ');
					if (s != TRUE) return s;
					clength += 2;
				}
				firstflag = FALSE;

				/* and add the word in in either case */
				for (i=0; i<wordlen; i++) {
					s = linsert(1, wbuf[i]);
					if (s != TRUE) return s;
					++clength;
				}
				if (finished == FALSE && sentence) {
					s = linsert(1, ' ');
					if (s != TRUE) return s;
					++clength;
				}
				wordlen = 0;
			}
		}
		DOT.l = lFORW(DOT.l);
	}
	return setmark();
}


#if	WORDCOUNT
/*	wordcount:	count the # of words in the marked region,
			along with average word sizes, # of chars, etc,
			and report on them.			*/
int
wordcount(f, n)
{
	register LINE *lp;	/* current line to scan */
	register int offset;	/* current char to scan */
	long size;		/* size of region left to count */
	register int ch;	/* current character to scan */
	register int wordflag;	/* are we in a word now? */
	register int lastword;	/* were we just in a word? */
	long nwords;		/* total # of words */
	long nchars;		/* total number of chars */
	int nlines;		/* total number of lines in region */
	int avgch;		/* average number of chars/word */
	int status;		/* status return code */
	REGION region;		/* region to look at */

	/* make sure we have a region to count */
	if ((status = getregion(&region)) != TRUE) {
		rls_region();
		return(status);
	}
	lp     = l_ref(region.r_orig.l);
	offset = region.r_orig.o;
	size   = region.r_size;

	/* count up things */
	lastword = FALSE;
	nchars = 0L;
	nwords = 0L;
	nlines = 0;
	while (size--) {

		/* get the current character */
		if (offset == llength(lp)) {	/* end of line */
			ch = '\n';
			lp = lforw(lp);
			offset = 0;
			++nlines;
		} else {
			ch = lgetc(lp, offset);
			++offset;
		}

		/* and tabulate it */
		if (((wordflag = isident(ch)) != 0) && !lastword)
			++nwords;
		lastword = wordflag;
		++nchars;
	}

	/* and report on the info */
	if (nwords > 0L)
		avgch = (int)((100L * nchars) / nwords);
	else
		avgch = 0;

	mlforce("lines %d, words %D, chars %D, avg chars/word %f",
		nlines, nwords, nchars, avgch);

	rls_region();
	return(TRUE);
}
#endif
