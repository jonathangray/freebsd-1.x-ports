/* vile note, 6/1/91, pgf -- I haven't tried this code in a long time */
/*
 * The functions in this file implement commands that perform incremental
 * searches in the forward and backward directions.  This "ISearch" command
 * is intended to emulate the same command from the original EMACS
 * implementation (ITS).  Contains references to routines internal to
 * SEARCH.C.
 * 
 * REVISION HISTORY:
 * 
 * D. R. Banks 9-May-86 - added ITS EMACSlike ISearch
 * 
 * John M. Gamble 5-Oct-86 - Made iterative search use search.c's scanner()
 * routine. This allowed the elimination of bakscan(). - Put isearch
 * constants into estruct.h - Eliminated the passing of 'status' to
 * scanmore() and checknext(), since there were no circumstances where it
 * ever equalled FALSE.
 * 
 * $Log: isearch.c,v $
 * Revision 1.1  1994/02/01 03:29:28  jkh
 * Initial revision
 *
 * Revision 1.24  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.23  1993/07/27  18:06:20  pgf
 * see tom's 3.56 CHANGES entry
 *
 * Revision 1.22  1993/07/01  16:15:54  pgf
 * tom's 3.51 changes
 *
 * Revision 1.21  1993/06/18  15:57:06  pgf
 * tom's 3.49 changes
 *
 * Revision 1.20  1993/05/04  17:05:14  pgf
 * see tom's CHANGES, 3.45
 *
 * Revision 1.19  1993/04/28  17:11:22  pgf
 * got rid of NeWS ifdefs
 *
 * Revision 1.18  1993/04/01  12:53:33  pgf
 * removed redundant includes and declarations
 *
 * Revision 1.17  1992/12/23  09:20:10  foxharp
 * ifdef of unused code
 *
 * Revision 1.16  1992/05/25  21:07:48  foxharp
 * extern func declarations moved to header
 *
 * Revision 1.15  1992/05/19  08:55:44  foxharp
 * more prototype and shadowed decl fixups
 *
 * Revision 1.14  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.13  1992/03/01  18:41:31  pgf
 * took out the checknext optimization, since we no longer move along
 * the found string quite the way we used to
 *
 * Revision 1.12  1992/01/05  00:06:13  pgf
 * split mlwrite into mlwrite/mlprompt/mlforce to make errors visible more
 * often.  also normalized message appearance somewhat.
 *
 * Revision 1.11  1991/11/16  18:34:31  pgf
 * pass magic mode as flag to regcomp()
 *
 * Revision 1.10  1991/11/01  14:38:00  pgf
 * saber cleanup
 * Revision 1.9  1991/10/30  14:55:43  pgf renamed
 * thescanner to scanner and setboundry to scanboundry
 * 
 * Revision 1.8  1991/10/27  01:46:27  pgf switch to regexp from regex package,
 * and moved expandp to this file
 * 
 * Revision 1.7  1991/10/24  13:05:52  pgf conversion to new regex package --
 * much faster
 * 
 * Revision 1.6  1991/08/07  12:35:07  pgf added RCS log messages
 * 
 * revision 1.5 date: 1991/06/26 09:37:56; renamed an ifdef BEFORE
 * 
 * revision 1.4 date: 1991/06/25 19:52:50; massive data structure restructure
 * 
 * revision 1.3 date: 1991/06/03 10:23:22; cleanup, and made it work with newer
 * scanner (setboundry())
 * 
 * revision 1.2 date: 1990/10/03 16:00:54; make backspace work for everyone
 * 
 * revision 1.1 date: 1990/09/21 10:25:30; initial vile RCS revision
 */

#include	"estruct.h"
#include        "edef.h"

#if	ISRCH

static	int	promptpattern P(( char * ));
static	char *	expandp P(( char *, char *, int ));

/* A couple "own" variables for the command string */

int             cmd_buff[CMDBUFLEN];	/* Save the command args here */
int             cmd_offset;	/* Current offset into command buff */
int             cmd_reexecute = -1;	/* > 0 if re-executing command */


/*
 * Subroutine to do incremental reverse search.  It actually uses the same
 * code as the normal incremental search, as both can go both ways.
 */

int 
risearch(f, n)
	int             f, n;
{
	MARK            curpos;	/* Current point on entry */

	/* remember the initial . on entry: */

	curpos = DOT;		/* Save the current point */

	/* Make sure the search doesn't match where we already are: */

	backchar(TRUE, 1);	/* Back up a character */


	if (!(isearch(f, -n))) {/* Call ISearch backwards */
				/* If error in search: */
		DOT = curpos;	/* Reset the pointer */
		curwp->w_flag |= WFMOVE;	/* Say we've moved */
		(void)update(FALSE);		/* And force an update */
		mlforce("[I-Search failed]");	/* Say we died */
		TTbeep();
		return FALSE;
	} else
		mlerase();	/* If happy, just erase the cmd line */

	return TRUE;
}

/* Again, but for the forward direction */

int
fisearch(f, n)
	int             f, n;
{
	MARK            curpos;	/* current line on entryl */

	/* remember the initial . on entry: */

	curpos = DOT;		/* save current point */

	/* do the search */


	if (!(isearch(f, n))) {	/* Call ISearch forwards */
				/* If error in search: */
		DOT = curpos;	/* reset */
		curwp->w_flag |= WFMOVE;	/* Say we've moved */
		(void)update(FALSE);		/* And force an update */
		mlforce("[I-Search failed]");	/* Say we died */
		TTbeep();
		return FALSE;
	} else
		mlerase();	/* If happy, just erase the cmd line */

	return TRUE;
}

/*
 * Subroutine to do an incremental search.  In general, this works similarly
 * to the older micro-emacs search function, except that the search happens
 * as each character is typed, with the screen and cursor updated with each
 * new search character.
 * 
 * While searching forward, each successive character will leave the cursor at
 * the end of the entire matched string.  Typing a Control-S or Control-X
 * will cause the next occurrence of the string to be searched for (where the
 * next occurrence does NOT overlap the current occurrence).  A Control-R
 * will change to a backwards search, META will terminate the search and
 * Control-G will abort the search.  Rubout will back up to the previous
 * match of the string, or if the starting point is reached first, it will
 * delete the last character from the search string.
 * 
 * While searching backward, each successive character will leave the cursor at
 * the beginning of the matched string.  Typing a Control-R will search
 * backward for the next occurrence of the string.  Control-S or Control-X
 * will revert the search to the forward direction.  In general, the reverse
 * incremental search is just like the forward incremental search inverted.
 * 
 * In all cases, if the search fails, the user will be feeped, and the search
 * will stall until the pattern string is edited back into something that
 * exists (or until the search is aborted).
 */

/* ARGSUSED */
int
isearch(f, n)
int f,n;
{
	int             status;	/* Search status */
	int             col;	/* prompt column */
	register int    cpos;	/* character number in search string */
	register int    c;	/* current input character */
	char            pat_save[NPAT];	/* Saved copy of the old pattern str */
	MARK            curpos, curp;	/* Current point on entry */
	int             init_direction;	/* The initial search direction */

	/* Initialize starting conditions */

	cmd_reexecute = -1;	/* We're not re-executing (yet?) */
	cmd_offset = 0;		/* Start at the beginning of the buff */
	cmd_buff[0] = '\0';	/* Init the command buffer */
	(void)strncpy(pat_save, pat, NPAT); /* Save the old pattern string */
	curpos = DOT;		/* Save the current pointer */
	init_direction = n;	/* Save the initial search direction */

	ignorecase = b_val(curwp->w_bufp, MDIGNCASE);

	scanboundry(FALSE, DOT, FORWARD);	/* keep scanner() finite */

	/* This is a good place to start a re-execution: */

start_over:

	/* ask the user for the text of a pattern */
	col = promptpattern("ISearch: ");	/* Prompt, remember the col */

	cpos = 0;		/* Start afresh */
	status = TRUE;		/* Assume everything's cool */

	/*
	 * Get the first character in the pattern.  If we get an initial
	 * Control-S or Control-R, re-use the old search string and find the
	 * first occurrence
	 */

	c = kcod2key(get_char());	/* Get the first character */
	if ((c == IS_FORWARD) ||
	    (c == IS_REVERSE)) {/* Reuse old search string? */
		for (cpos = 0; pat[cpos] != 0; cpos++)	/* Yup, find the length */
			col = echochar(pat[cpos], col);	/* and re-echo the
							 * string */
		curp = DOT;
		if (c == IS_REVERSE) {	/* forward search? */
			n = -1;	/* No, search in reverse */
			backchar(TRUE, 1);	/* Be defensive about EOB */
		} else {
			n = 1;	/* Yes, search forward */
			forwchar(TRUE, 1);
		}
		--cmd_offset;	/* Back up over the Rubout */
		cmd_buff[--cmd_offset] = '\0';	/* Yes, delete last char */
		status = scanmore(pat, n);	/* Do the search */
		if (status != TRUE)
			DOT = curp;
		c = kcod2key(get_char());	/* Get another character */
	}
	/* Top of the per character loop */

	for (;;) {		/* ISearch per character loop */
		/* Check for special characters first: */
		/* Most cases here change the search */

		if (c == kcod2key(abortc))	/* Want to quit searching? */
			return (TRUE);	/* Quit searching now */

		if (isbackspace(c))
			c = '\b';

		if (c == kcod2key(quotec))	/* quote character? */
			c = kcod2key(get_char());	/* Get the next char */

		switch (c) {	/* dispatch on the input char */
		case IS_REVERSE:	/* If backward search */
		case IS_FORWARD:	/* If forward search */
			curp = DOT;
			if (c == IS_REVERSE) {	/* forward search? */
				n = -1;	/* No, search in reverse */
				backchar(TRUE, 1);	/* Be defensive about
							 * EOB */
			} else {
				n = 1;	/* Yes, search forward */
				forwchar(TRUE, 1);
			}
			status = scanmore(pat, n);	/* Do the search */
			if (status != TRUE)
				DOT = curp;
			c = kcod2key(get_char());	/* Get the next char */
			--cmd_offset;	/* Back up over the Rubout */
			cmd_buff[--cmd_offset] = '\0';	/* Yes, del last char */
			continue;	/* Go continue with the search */

		case '\r':	/* Carriage return */
			c = '\n';	/* Make it a new line */
			break;	/* Make sure we use it */

		case '\t':	/* Generically allowed */
		case '\n':	/* controlled characters */
			break;	/* Make sure we use it */

		case '\b':	/* or if a Rubout: */
			if (cmd_offset <= 1)	/* Anything to delete? */
				return (TRUE);	/* No, just exit */
			--cmd_offset;	/* Back up over the Rubout */
			cmd_buff[--cmd_offset] = '\0';	/* Yes, del last char */
			DOT = curpos;	/* Reset the pointer */
			n = init_direction;	/* Reset the search direction */
			(void)strncpy(pat, pat_save, NPAT);
						/* Restore the old search str */
			cmd_reexecute = 0;	/* Start the whole mess over */
			goto start_over;	/* Let it take care of itself */

			/* Presumably a quasi-normal character comes here */

		default:	/* All other chars */
			if (c < ' ') {	/* Is it printable? *//* Nop
					 * e. */
				tungetc(c);	/* Re-eat the char */
				return (TRUE);	/* And return the last status */
			}
		}		/* Switch */

		/* I guess we got something to search for, so search for it */

		pat[cpos++] = c;/* put the char in the buffer */
		if (cpos >= NPAT) {	/* too many chars in string? *//* Yup
					 * .  Complain about it */
			mlforce("[Search string too long]");
			return (TRUE);	/* Return an error */
		}
		pat[cpos] = 0;	/* null terminate the buffer */
		col = echochar(c, col);	/* Echo the character */
		if (!status) {	/* If we lost last time */
			TTbeep();	/* Feep again */
			TTflush();	/* see that the feep feeps */
		} else /* Otherwise, we must have won */
			status = scanmore(pat, n);   /* or find the next
							      * match */
		c = kcod2key(get_char());	/* Get the next char */
	}			/* for {;;} */
}

/*
 * This hack will search for the next occurrence of <pat> in the buffer,
 * either forward or backward.  It is called with the status of the prior
 * search attempt, so that it knows not to bother if it didn't work last
 * time.  If we can't find any more matches, "point" is left where it was
 * before.  If we do find a match, "point" will be at the end of the matched
 * string for forward searches and at the beginning of the matched string for
 * reverse searches.
 */

int 
scanmore(patrn, dir)		/* search forward or back for a pattern */
	char           *patrn;	/* string to scan for */
	int             dir;	/* direction to search */
{
	int             sts;	/* search status */

	if (gregexp)
		free((char *)gregexp);
	gregexp = regcomp(patrn, b_val(curbp, MDMAGIC));
	if (!gregexp)
		return FALSE;

	ignorecase = b_val(curwp->w_bufp, MDIGNCASE);

	sts = scanner(gregexp, (dir < 0) ? REVERSE : FORWARD, FALSE);

	if (!sts) {
		TTbeep();	/* Feep if search fails */
		TTflush();	/* see that the feep feeps */
	}
	return (sts);		/* else, don't even try */
}

/*
 * The following is a worker subroutine used by the reverse search.  It
 * compares the pattern string with the characters at "." for equality. If
 * any characters mismatch, it will return FALSE.
 * 
 * This isn't used for forward searches, because forward searches leave "." at
 * the end of the search string (instead of in front), so all that needs to
 * be done is match the last char input.
 */

#ifdef	UNUSED
int 
match_pat(patrn)		/* See if the pattern string matches string
				 * at "." */
	char           *patrn;	/* String to match to buffer */
{
	register int    i;	/* Generic loop index/offset */
	register int    buffchar;	/* character at current position */
	MARK            curpos;

	/* setup the local scan pointer to current "." */
	curpos = DOT;		/* Get the current point */

	/* top of per character compare loop: */
	for (i = 0; i < strlen(patrn); i++) {	/* Loop for all characters in
						 * patrn */
		if (is_at_end_of_line(curpos)) {	/* If at end of line */
			curpos.l = lforw(curpos.l);	/* Skip to the next line */
			curpos.o = 0;	/* Start at the beginning of the line */
			if (is_header_line(curpos, curbp))
				return (FALSE);	/* Abort if at end of buffer */
			buffchar = '\n';	/* And say the next char is
						 * NL */
		} else {
			buffchar = char_at(curpos);	/* Get the next char */
			curpos.o++;
		}
		if (!eq(buffchar, patrn[i]))	/* Is it what we're looking
						 * for? */
			return (FALSE);	/* Nope, just punt it then */
	}
	return (TRUE);		/* Everything matched? Let's celebrate */
}
#endif	/* UNUSED */

/* Routine to prompt for I-Search string. */

static int 
promptpattern(prompt)
	char           *prompt;
{
	static	char	fmt[] = "%s [%s]: ";
	char            tpat[NPAT],
			temp[NPAT];

	(void)lsprintf(tpat, fmt, prompt,
		expandp(temp, pat, (int)(NPAT-sizeof(fmt)-strlen(prompt))));

	/* check to see if we are executing a command line */
	if (!clexec) {
		mlforce("%s", tpat);
	}
	return (strlen(tpat));
}

/*
 * expandp -- Expand control key sequences for output.
 */
static char *
expandp(deststr, srcstr, maxlength)
	char           *deststr;	/* destination of expanded string */
	char           *srcstr;		/* string to expand */
	int             maxlength;	/* maximum chars in destination */
{
	char	*base = deststr;
	UCHAR   c;	/* current char to translate */

	/*
	 * Scan through the string.
	 */
	while ((c = *srcstr++) != 0) {
		if (c == '\n') {/* it's a newline */
			*deststr++ = '<';
			*deststr++ = 'N';
			*deststr++ = 'L';
			*deststr++ = '>';
			maxlength -= 4;
		} else if (!isprint(c)) {	/* control character */
			*deststr++ = '^';
			*deststr++ = toalpha(c);
			maxlength -= 2;
		} else {	/* any other character */
			*deststr++ = c;
			maxlength--;
		}

		/* check for maxlength */
		if (maxlength < 4) {
			*deststr++ = '$';
			break;
		}
	}
	*deststr = '\0';
	return base;
}

/* routine to echo i-search characters */

int 
echochar(c, col)
	int             c;	/* character to be echoed */
	int             col;	/* column to be echoed in */
{
	movecursor(term.t_nrow, col);	/* Position the cursor */
	if (!isprint(c)) {	/* control char */
		TTputc('^');	/* Yes, output prefix */
		TTputc(toalpha(c));	/* Make it "^X" */
		col++;		/* Count this char */
	} else {
		TTputc(c);	/* Otherwise, output raw char */
	}
	TTflush();		/* Flush the output */
	return (++col);		/* return the new column no */
}

/*
 * Routine to get the next character from the input stream.  If we're reading
 * from the real terminal, force a screen update before we get the char.
 * Otherwise, we must be re-executing the command string, so just return the
 * next character.
 */

int 
get_char()
{
	int             c;	/* A place to get a character */

	/* See if we're re-executing: */

	if (cmd_reexecute >= 0)	/* Is there an offset? */
		if ((c = cmd_buff[cmd_reexecute++]) != 0)
			return (c);	/* Yes, return any character */

	/* We're not re-executing (or aren't any more).  Try for a real char */

	cmd_reexecute = -1;	/* Say we're in real mode again */
	(void)update(FALSE);	/* Pretty up the screen */
	if (cmd_offset >= CMDBUFLEN - 1) {	/* If we're getting too big
						 * ... */
		mlforce("[Command too long]");	/* Complain loudly and
						 * bitterly */
		return (abortc);/* And force a quit */
	}
	c = kbd_key();		/* Get the next character */
	cmd_buff[cmd_offset++] = c;	/* Save the char for next time */
	cmd_buff[cmd_offset] = '\0';	/* And terminate the buffer */
	return (c);		/* Return the character */
}

#endif
