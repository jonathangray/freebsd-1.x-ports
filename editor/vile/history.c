/*
 *	history.c
 *
 *	Manage command-history buffer
 *
 * Notes:
 *	This module manages an invisible, non-volatile buffer "[History]".
 *	Each keyboard command to vile is logged in this buffer.  The leading
 *	':' is not included, but all other characters are preserved so that the
 *	command can be replayed.
 *
 *	Each interactive call on 'kbd_reply()' logs the resulting string, and
 *	the "glue" character (nominally, the end-of-line character), so that
 *	successive calls can be spliced together. On completion of the command,
 *	the composed command (in 'MyText') is flushed into the history-buffer.
 *
 *	The procedure 'edithistory()' is invoked from 'kbd_reply()' to provide
 *	access to the history.  In particular, it presents a scrollable list of
 *	strings based on a match of the command to that point.  For example, if
 *	the commands
 *
 *		:set ts=8
 *		:set ab
 *		:set ai
 *
 *	were entered, then in response to ":set", the user would see the
 *	strings "ai", "ab" and "ts".
 *
 *	Scrolling is accomplished by either arrow keys, or by an escaped set of
 *	commands (a la 'ksh').
 *
 *	Note that this implementation is a compromise.  Ideally, the command
 *	interpreter for ':' would be able to scroll through the entire list of
 *	commands from any point, moving forward and backward through its
 *	internal state of range, command, arguments.  As implemented, it is not
 *	so general.  The user can backspace from the command state into the
 *	range state, but not from the arguments.  Also, the history scrolling
 *	is not really useful in the range state, so it is disabled there.  If
 *	the command interpreter were able to easily go back and forth in its
 *	state, then it would be simple to implement an 'expert' mode, in which
 *	prompting would be suppressed.
 *
 * To do:
 *	Add logic to quote arguments that should be strings, to make them
 *	easier to parse back for scrolling, etc.
 *
 *	Integrate this with the "!!" response to the ^X-! and !-commands.
 *
 *	Modify the matching logic so that file commands (i.e., ":e", ":w",
 *	etc.) are equivalent when matching for the argument.  Currently, the
 *	history scrolling will show only arguments for identical command names.
 *
 *	Modify the matching logic so that search commands (i.e., "/" and "?")
 *	are equivalent when matching for the argument.  Note also that these do
 *	not (yet) correspond to :-commands.  Before implementing, probably will
 *	have to make TESTC a settable mode.
 *
 *	Make the display updating work for more than simply erasing/printing
 *	the entire response.  This is adequate for scrolling, but won't support
 *	inline editing.
 *
 *	Implement other ksh-style inline command editing.
 *
 *	Allow left/right scrolling of input lines (when they get too long).
 *
 * $Log: history.c,v $
 * Revision 1.1  1994/02/01 03:29:24  jkh
 * Initial revision
 *
 * Revision 1.9  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.8  1993/07/27  18:06:20  pgf
 * see tom's 3.56 CHANGES entry
 *
 * Revision 1.7  1993/07/01  16:15:54  pgf
 * tom's 3.51 changes
 *
 * Revision 1.6  1993/05/24  15:21:37  pgf
 * tom's 3.47 changes, part a
 *
 * Revision 1.5  1993/04/20  12:18:32  pgf
 * see tom's 3.43 CHANGES
 *
 * Revision 1.4  1993/04/01  13:06:31  pgf
 * turbo C support (mostly prototypes for static)
 *
 * Revision 1.3  1993/03/16  10:53:21  pgf
 * see 3.36 section of CHANGES file
 *
 * Revision 1.2  1993/03/05  17:50:54  pgf
 * see CHANGES, 3.35 section
 *
 * Revision 1.1  1993/02/24  10:58:31  pgf
 * Initial revision
 *
 */

#include "estruct.h"
#include "edef.h"

#if	OPT_HISTORY

#define	tb_args(p)	tb_values(p), (int)tb_length(p)
#define	lp_args(p)	p->l_text, llength(p)

typedef	struct	{
	char *	buffer;
	int *	position;
	int	(*endfunc) P(( char *, int, int, int ));
	int	eolchar;
	int	options;
	} HST;

/*--------------------------------------------------------------------------*/
static	BUFFER *makeMyBuff P(( void ));
static	void	stopMyBuff P(( void ));
static	int	willGlue P(( void ));
static	int	willExtend P(( char *, int ));
static	int	sameLine P(( LINE *, char *, int ));
static	int	parseArg P(( HST *, LINE * ));
static	int	hst_macroize P(( char *, char * ));
static	LINE *	hst_find P(( HST *, BUFFER *, LINE *, int ));
static	void	hst_display P(( HST *, char *, int ));
static	void	display_LINE P(( HST *, LINE * ));
static	void	display_TBUFF P(( HST *, TBUFF * ));
static	LINE *	hst_scroll P(( LINE *, HST * ));

static	char	*MyBuff = ScratchName(History);
static	TBUFF	*MyText,	/* current command to display */
		*MyArgs;	/* ...buffer to hold extended-args */
static	int	MyGlue,		/* most recent eolchar */
		MyLevel,	/* logging iff level is 1 */
		save_flg;	/* saves 'clexec' */
static	char *	save_ptr;	/* saves 'execstr' */

/*--------------------------------------------------------------------------*/

static BUFFER *
makeMyBuff()
{
	register BUFFER *bp;

	if (!global_g_val(GMDHISTORY)) {
		bp = 0;
	} else if ((bp = bfind(MyBuff, BFINVS)) != 0) {
		b_set_invisible(bp);
		b_clr_flags(bp, BFSCRTCH); /* make it nonvolatile */
		set_rdonly(bp, MyBuff);
	} else {
		stopMyBuff();
	}
	return bp;
}

static void
stopMyBuff()
{
	register BUFFER *bp;

	if ((bp = find_b_name(MyBuff)) != 0)
		(void)zotbuf(bp);

	set_global_g_val(GMDABUFF,FALSE);

	tb_free(&MyText);
	tb_free(&MyArgs);
}

/*
 * Returns 0 or 1 according to whether we will add the glue-character in the
 * next call on 'hst_append()'.
 */
static int
willGlue()
{
	if ((tb_length(MyText) != 0) && isprint(MyGlue)) {
		register int c = tb_values(MyText)[0];
		if ((c != SHPIPE_LEFT[0]) || isRepeatable(c))
			return 1;
	}
	return 0;
}

/*
 * Returns true iff we display the complete, rather than the immediate portion
 * of the history line.  We do this for !-commands so that the user can see the
 * entire command when scrolling.
 *
 * The shift-commands also are a (similar) special case.
 */
static int
willExtend(src, srclen)
char *	src;
int	srclen;
{
	if ((tb_length(MyText) == 0)
	 && (srclen > 0)) {
		return (src[0] == SHPIPE_LEFT[0]) || isRepeatable(src[0]);
	}
	return FALSE;
}

/*
 * Returns a positive number if the length of the given LINE is at least as
 * long as the given string in 'src' and if it matches to the length in
 * 'srclen'.
 */
static int
sameLine(lp, src, srclen)
LINE *	lp;
char *	src;
int	srclen;
{
	if (srclen == 0)
		return 0;
	else {
		register int	dstlen = llength(lp);

		if (dstlen >= srclen) {
			if (!memcmp(lp->l_text, src, srclen)) {
				if (isRepeatable(*src)
				 && isRepeatable(lp->l_text[0])
				 && dstlen != srclen)
					return -1;
				return (dstlen - srclen);
			}
		}
	}
	return -1;
}

/*
 * Returns the length of the argument from the given line
 */
static int
parseArg(parm, lp)
HST *	parm;
LINE *	lp;
{
	int	len = llength(lp);

	if (len > 0) {
		if (willExtend(lp_args(lp))) {
			return len;
		} else {
			register char	*s = lp->l_text;
			register int	n;

			for (n = willGlue()+tb_length(MyText); n < len; n++)
				if ((*parm->endfunc)(s, n, s[n], parm->eolchar))
					break;
			return n;
		}
	}
	return 0;
}

/*
 * Convert the string 'src' into a string that we can read back with 'token()'. 
 * If it is a shell-command, this will be a single-token.  Repeated shift
 * commands are multiple tokens.
 */
static int
hst_macroize(src, ref)
char *	src;
char *	ref;
{
	register int	c;
	int	multi	= !isShellOrPipe(ref);	/* shift command? */
	int	count	= 0;

	if (tb_init(&MyArgs,abortc) != 0) {
		(void)tb_append(&MyArgs, '"');
		while ((c = *src++) != EOS) {
			if (multi && count++)
				(void)tb_sappend(&MyArgs, "\" \"");
			if (c == '\\' || c == '"')
				(void)tb_append(&MyArgs, '\\');
			(void)tb_append(&MyArgs, c);
		}
		(void)tb_append(&MyArgs, '"');
		return (tb_append(&MyArgs, EOS) != 0);
	}
	return FALSE;
}

/******************************************************************************/
void
hst_init(c)
int	c;
{
	if (++MyLevel == 1) {
		(void)tb_init(&MyText, abortc);
		MyGlue = EOS;
		if (c != EOS)
			(void)tb_append(&MyText, c);
	}
}

void
hst_glue(c)
int	c;
{
	/* ensure we don't repeat '/' delimiter */
	if (tb_length(MyText) == 0
	 || tb_values(MyText)[0] != c)
		MyGlue = c;
}

void
hst_append(cmd, glue)
char *	cmd;
int	glue;
{
	static	int	skip = 1;		/* e.g., after "!" */

	if (tb_length(MyArgs)			/* e.g., within "!!" command */
	 && hst_macroize(cmd, cmd)) {
		execstr = tb_values(MyArgs) + tb_length(MyArgs);
		return;
	}

	if (willExtend(cmd, (int)strlen(cmd))
	 && strlen(cmd) > skip
	 && hst_macroize(cmd + skip, cmd) != 0) {

		save_flg = clexec;
		save_ptr = execstr;

		clexec   = TRUE;		/* get the argument */
		execstr  = tb_values(MyArgs);

		cmd[skip] = EOS;
	}

	if (willGlue())
		(void)tb_append(&MyText, MyGlue);
	(void)tb_sappend(&MyText, cmd);
	MyGlue = glue;
}

void
hst_remove(cmd)
char *	cmd;
{
	if (MyLevel == 1) {
		char	temp[NLINE];
		int	len	= strlen(strcpy(temp, cmd));

		while (*cmd++)
			tb_unput(MyText);
		kbd_kill_response(temp, &len, killc);
	}
}

void
hst_flush()
{
	register BUFFER *bp;
	register WINDOW *wp;
	register LINE	*lp;

	if (MyLevel <= 0)
		return;
	if (MyLevel-- != 1)
		return;

	if (tb_length(MyArgs)) {
		char	buffer[NLINE],
			*base = tb_values(MyArgs);

		while (*base != EOS) {
			base = token(base, buffer, EOS);
			if (willGlue())
				(void)tb_append(&MyText, MyGlue);
			(void)tb_sappend(&MyText, tokval(buffer));
		}
		(void)tb_init(&MyArgs, abortc);

		clexec = save_flg;
		execstr = save_ptr;
	}

	if ((tb_length(MyText) != 0)
	 && ((bp = makeMyBuff()) != 0)) {

		/* suppress if this is the same as previous line */
		if (((lp = lBack(bp->b_line.l)) != 0)
		 && (lp != l_ref(bp->b_line.l))
		 && (sameLine(lp, tb_args(MyText)) == 0)) {
			(void)tb_init(&MyText, abortc);
			return;
		 }

		if (!addline(bp, tb_args(MyText))) {
			stopMyBuff();
			return;
		}

		/* patch: reuse logic from slowreadf()? */
		for_each_window(wp) {
			if (wp->w_bufp == bp) {
				wp->w_flag |= WFFORCE;
				if (wp == curwp)
					continue;
				/* force dot to the beginning of last-line */
				wp->w_force = -1;
				if (l_ref(wp->w_dot.l) != lBack(bp->b_line.l)) {
					wp->w_dot.l = lBACK(bp->b_line.l);
					wp->w_dot.o = 0;
					wp->w_flag |= WFMOVE;
				}
			}
		}
		updatelistbuffers();	/* force it to show current sizes */
		(void)tb_init(&MyText, abortc);
	 }
}

/*ARGSUSED*/
int
showhistory(f,n)
int	f,n;
{
	register BUFFER *bp = makeMyBuff();

	if (bp == 0 || popupbuff(bp) == FALSE)
		return no_memory("show-history");
	return TRUE;
}

/*
 * Find the last line in the history buffer that matches the portion of
 * the command that has been input to this point.  The substrings to the
 * right (up to eolchar) will form the set of history strings that the
 * user may scroll through.
 */
static LINE *
hst_find (parm, bp, lp, direction)
HST *	parm;
BUFFER *bp;
LINE *	lp;
int	direction;
{
	LINE	*base	= l_ref(bp->b_line.l),
		*lp0	= lp;

	if ((lp0 == 0)
	 || ((lp == base) && (direction > 0))) {
		return 0;
	}

	for(;;) {
		if (direction > 0) {
			if (lp == lback(base))	/* cannot wrap-around */
				return 0;
			lp = lforw(lp);
		} else
			lp = lback(lp);
		if (lp == base)
			return 0;		/* empty or no matches */

		if (!lisreal(lp)
		 || (llength(lp) <= tb_length(MyText)+willGlue())
		 || (sameLine(lp, tb_args(MyText)) < 0))
			continue;		/* prefix mismatches */

		if (willGlue()) {		/* avoid conflicts setall/set */
			register int len = tb_length(MyText);
			if (len > 0
			 && (len > 1 || !ispunct(tb_values(MyText)[0]))
			 && llength(lp) > len
			 && lp->l_text[len] != MyGlue)
				continue;
		}

		/* avoid picking up lines with range-spec, since this is too
		 * cumbersome to splice in 'namedcmd()'.
		 */
		if (islinespecchar(lp->l_text[0]))
			continue;

		/* '/' and '?' are not (yet) :-commands.  Don't display them
		 * in the command-name scrolling.
		 */
		if (tb_length(MyText) == 0) {
			if (lp->l_text[0] == '/'
			 || lp->l_text[0] == '?')
				continue;
		}

		/* compare the argument that will be shown for the original
		 * and current lines.
		 */
		if (lisreal(lp0)) {
			int	n0 = parseArg(parm, lp0),
				n1 = parseArg(parm, lp);
			if (n0 != 0
			 && n1 != 0
			 && n0 == n1
			 && sameLine(lp, lp0->l_text, n0) >= 0)
				continue;
		}

		return lp;
	}
}

/*
 * Update the display of the currently-scrollable buffer on the prompt-line.
 */
static void
hst_display(parm, src, srclen)
HST *	parm;
char *	src;
int	srclen;
{
	int	keylen	= tb_length(MyText) + willGlue();
	int	uselen	= srclen - keylen;
	register char	*s = src + keylen, *t = s;

	kbd_kill_response(parm->buffer, parm->position, killc);

	if (s != 0) {
		if (willExtend(src,srclen))
			t += uselen;
		else {
			while (uselen-- > 0) {
				if ((*parm->endfunc)(s, t-s, *t, parm->eolchar))
					break;
				t++;
			}
		}
		*parm->position = kbd_show_response(parm->buffer, s, t+1-s, parm->eolchar, parm->options);
	}
}

/*
 * Update the display using a LINE as source
 */
static void
display_LINE(parm, lp)
HST *	parm;
LINE *	lp;
{
	hst_display(parm, lp_args(lp));
}

/*
 * Update the display using a TBUFF as source
 */
static void
display_TBUFF(parm, tp)
HST *	parm;
TBUFF *	tp;
{
	hst_display(parm, tb_args(tp));
}

/*
 * Perform common scrolling functions for arrow-keys and ESC-mode.
 */
static	TBUFF *	original;	/* save 'buffer' on first-scroll */
static	int	any_edit,	/* true iff any edit happened */
		direction,	/* current scrolling +/- */
		distance;	/* distance from original entry */

static LINE *
hst_scroll(lp1, parm)
LINE *	lp1;
HST *	parm;
{
	BUFFER	*bp = makeMyBuff();
	LINE	*lp0 = l_ref(bp->b_line.l),
		*lp2 = hst_find(parm, bp, lp1, direction);

	if (lp1 != lp2) {
		if (lp2 == 0) {
			if (direction+distance == 0) {
				lp1 = lp0;
				distance = 0;
				display_TBUFF(parm, original);
			} else {
				if (lp1 == lp0)	/* nothing to scroll for */
					distance = 0;
				kbd_alarm();
			}
			return lp1;
		} else {
			distance += direction;
			display_LINE(parm, lp2);
			any_edit++;
			return lp2;
		}
	}
	return 0;
}

#define	isgraph(c)	(!isspecial(c) && !isspace(c) && isprint(c))

/*
 * Invoked on an escape-character, this processes history-editing until another
 * escape-character is entered.
 */
int
edithistory (buffer, position, given, options, endfunc, eolchar)
char *	buffer;
int *	position;
int *	given;
int	options;
int	(*endfunc) P(( char *, int, int, int ));
int	eolchar;
{
	HST	param;
	BUFFER	*bp;
	LINE	*lp1, *lp2;
	int	escaped	= FALSE;

	register int	c = *given;

#if	KSH_HISTORY
	if (c != ESC)				/* suppress immediate-return */
#endif
	if (!isspecial(c)) {
		if (is_edit_char(c)
		 || (c == abortc)
		 || (c == quotec)
		 || isspace(c)
		 || !iscntrl(c))
			return FALSE;
	}

	if ((bp = makeMyBuff()) == 0)		/* something is very wrong */
		return FALSE;

	if ((lp1 = l_ref(bp->b_line.l)) == 0)
		return FALSE;

	/* slightly better than global data... */
	param.buffer   = buffer;
	param.position = position;
	param.endfunc  = endfunc;
	param.eolchar  = eolchar;
	param.options  = options;

	any_edit = 0;
	distance = 0;

	/* save the original buffer, since we expect to scroll it */
	if (tb_copy(&original, MyText)) {
		/* make 'original' look just like a complete command... */
		if (willGlue())
			(void)tb_append(&original, MyGlue);
		(void)tb_sappend(&original, buffer);
	}

	/* process char-commands */
	for (;;) {
		register CMDFUNC *p;

		/* If the character is bound to up/down scrolling, scroll the
		 * history.
		 */
		direction = 0;	/* ...unless we find scrolling-command */
		if ((p = kcod2fnc(c)) != 0) {
			if (p->c_func == backline)
				direction = -1;
			else if (p->c_func == forwline)
				direction = 1;
		}
#if	KSH_HISTORY
		if (c == ESC) {
			escaped = !escaped;
		} else
#endif
		if (c == abortc) {
			*given = c;
			return FALSE;

		} else if ((direction != 0) && (escaped || !isgraph(c))) {

			if ((lp2 = hst_scroll(lp1, &param)) != 0)
				lp1 = lp2;
			else	/* cannot scroll */
				kbd_alarm();
#if	KSH_HISTORY
		/* patch: inline-editing should be done at this point */
#endif
		} else if (!escaped) {
			*given = c;
			if (any_edit)
				tungetc(c);
			return any_edit;

		} else
			kbd_alarm();

		c = kbd_key();
	}
}
#endif	/* OPT_HISTORY */
