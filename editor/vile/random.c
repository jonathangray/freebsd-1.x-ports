/*
 * This file contains the command processing functions for a number of random
 * commands. There is no functional grouping here, for sure.
 *
 * $Log: random.c,v $
 * Revision 1.1  1994/02/01 03:29:37  jkh
 * Initial revision
 *
 * Revision 1.107  1993/09/10  16:06:49  pgf
 * tom's 3.61 changes
 *
 * Revision 1.106  1993/09/06  16:33:24  pgf
 * guard against null return from getcwd(), and
 * changed glob() --> doglob()
 *
 * Revision 1.105  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.103  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.102  1993/07/27  18:06:20  pgf
 * see tom's 3.56 CHANGES entry
 *
 * Revision 1.101  1993/07/06  16:39:04  pgf
 * integrated Tuan DANG's changes for the djgpp compiler under DOS
 *
 * Revision 1.100  1993/07/06  12:33:47  pgf
 * initialize b_modtime_at_warn properly in ch_fname
 *
 * Revision 1.99  1993/07/01  16:15:54  pgf
 * tom's 3.51 changes
 *
 * Revision 1.98  1993/06/28  14:30:28  pgf
 * added second arg to catnap(), to allow user input to interrupt the nap.
 * this is only implemented for select() so far.
 *
 * Revision 1.97  1993/06/25  11:25:55  pgf
 * patches for Watcom C/386, from Tuan DANG
 *
 * Revision 1.96  1993/06/18  15:57:06  pgf
 * tom's 3.49 changes
 *
 * Revision 1.95  1993/06/02  14:28:47  pgf
 * see tom's 3.48 CHANGES
 *
 * Revision 1.94  1993/05/24  15:21:37  pgf
 * tom's 3.47 changes, part a
 *
 * Revision 1.93  1993/05/04  17:05:14  pgf
 * see tom's CHANGES, 3.45
 *
 * Revision 1.92  1993/04/28  14:34:11  pgf
 * see CHANGES, 3.44 (tom)
 *
 * Revision 1.91  1993/04/20  12:18:32  pgf
 * see tom's 3.43 CHANGES
 *
 * Revision 1.90  1993/04/08  14:59:16  pgf
 * moved istring() to insert.c
 *
 * Revision 1.89  1993/04/01  13:06:31  pgf
 * turbo C support (mostly prototypes for static)
 *
 * Revision 1.88  1993/03/25  19:50:58  pgf
 * see 3.39 section of CHANGES
 *
 * Revision 1.87  1993/03/17  10:00:29  pgf
 * initial changes to make VMS work again
 *
 * Revision 1.86  1993/03/16  10:53:21  pgf
 * see 3.36 section of CHANGES file
 *
 * Revision 1.85  1993/03/05  17:50:54  pgf
 * see CHANGES, 3.35 section
 *
 * Revision 1.84  1993/02/15  10:47:30  pgf
 * add include of sys/select.h for AIX
 *
 * Revision 1.83  1993/01/23  13:38:23  foxharp
 * lchange is now chg_buff,
 * fix for repeat counts on 'C' command, and
 * update buffer list on chdir
 *
 * Revision 1.82  1993/01/16  10:41:43  foxharp
 * use new macros, and add call to updatelistbuffers()
 *
 * Revision 1.81  1993/01/12  08:48:43  foxharp
 * tom dickey's changes to support "set number", i.e. line numbering
 *
 * Revision 1.80  1992/12/14  09:03:25  foxharp
 * lint cleanup, mostly malloc
 *
 * Revision 1.79  1992/12/04  09:51:00  foxharp
 * assume POSIX machines have getcwd.  they should.
 *
 * Revision 1.78  1992/12/04  09:14:36  foxharp
 * deleted unused assigns
 *
 * Revision 1.77  1992/08/20  23:40:48  foxharp
 * typo fixes -- thanks, eric
 *
 * Revision 1.76  1992/08/19  23:06:06  foxharp
 * handle DOS's multiple current directories better (i.e. one per drive), and
 * allow npopen to follow PATH to find "pwd", since it isn't always in /bin.
 * sheesh.
 *
 * Revision 1.75  1992/08/06  23:55:07  foxharp
 * changes to canonical pathnames and directory changing to support DOS and
 * its drive designators
 *
 * Revision 1.74  1992/08/05  22:10:03  foxharp
 * hopefully handle DOS pathnames better
 *
 * Revision 1.73  1992/08/04  20:13:29  foxharp
 * fclose() --> npclose().  thanks eric.
 *
 * Revision 1.72  1992/07/20  22:49:51  foxharp
 * took out ifdef'ed BEFORE code
 *
 * Revision 1.71  1992/07/18  13:13:56  foxharp
 * put all path-shortening in one place (shorten_path()), and took out some old code now
 * unnecessary
 *
 * Revision 1.70  1992/07/17  19:14:57  foxharp
 * clean up gcc -Wall warnings
 *
 * Revision 1.69  1992/07/16  22:18:54  foxharp
 * ins() takes an argument -- whether or not to playback, usually FALSE
 *
 * Revision 1.68  1992/07/15  08:58:46  foxharp
 * added, and ifdef'ed out, code for dealing with DOS drive designators
 * in ch_fname()
 *
 * Revision 1.67  1992/07/13  19:39:03  foxharp
 * finished canonicalizing pathnames
 *
 * Revision 1.66  1992/07/13  09:28:37  foxharp
 * preliminary changes for canonical path names
 *
 * Revision 1.65  1992/06/25  23:00:50  foxharp
 * changes for dos/ibmpc
 *
 * Revision 1.64  1992/05/29  09:40:53  foxharp
 * split out modes.c, fences.c, and insert.c from random.c
 *
 * Revision 1.63  1992/05/29  08:37:59  foxharp
 * getfence() changes for #ifdef matching and C comment matching
 *
 * Revision 1.62  1992/05/25  21:07:48  foxharp
 * extern func declarations moved to header
 *
 * Revision 1.61  1992/05/20  18:55:08  foxharp
 * better confirmation output from cd command
 *
 * Revision 1.60  1992/05/19  08:55:44  foxharp
 * more prototype and shadowed decl fixups
 *
 * Revision 1.59  1992/05/16  14:02:55  pgf
 * header/typedef fixups
 *
 * Revision 1.58  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.57  1992/05/13  09:14:50  pgf
 * when scanning for a fence character, don't go past end of line.
 * i hate do/while loops.
 *
 * Revision 1.56  1992/04/14  08:51:44  pgf
 * missing local var in DOS ifdef
 *
 * Revision 1.55  1992/03/24  22:45:09  pgf
 * allow ^D to back up past autoindented whitespace
 *
 * Revision 1.54  1992/03/24  07:45:08  pgf
 * made separate internal and external entry points for go[to]col, so that
 * users see columns starting at 1, and vile sees them starting from 0
 *
 * Revision 1.53  1992/03/19  23:23:31  pgf
 * removed extra string lib externs and include
 *
 * Revision 1.52  1992/03/05  09:19:55  pgf
 * changed some mlwrite() to mlforce(), due to new terse support
 *
 * Revision 1.51  1992/03/03  08:43:47  pgf
 * fixed off-by-one error in gotocol()
 *
 * Revision 1.50  1992/03/01  18:38:40  pgf
 * added fence matching on #if, #el, and #en, and
 * fixed compilation error #if COLOR
 *
 * Revision 1.49  1992/02/26  21:58:17  pgf
 * changes to entab/detabline, to better support shift operations
 *
 * Revision 1.48  1992/02/17  08:55:57  pgf
 * added showmode support, and
 * fixed backspace-after-wrapword bug, and
 * fixed '=' parsing in set commands, and
 * some small changes for saber cleanup
 *
 * Revision 1.47  1992/01/13  23:33:32  pgf
 * finished shiftwidth implementation -- ^D works now
 *
 * Revision 1.46  1992/01/10  08:08:46  pgf
 * added shiftwidth(), and some bug fixes to en/detabline()
 *
 * Revision 1.45  1992/01/05  00:06:13  pgf
 * split mlwrite into mlwrite/mlprompt/mlforce to make errors visible more
 * often.  also normalized message appearance somewhat.
 *
 * Revision 1.44  1992/01/03  23:31:49  pgf
 * use new ch_fname() to manipulate filenames, since b_fname is now
 * a malloc'ed sting, to avoid length limits
 *
 * Revision 1.43  1991/12/24  09:18:47  pgf
 * added current/change directory support  (Dave Lemke's changes)
 *
 * Revision 1.42  1991/11/16  18:39:50  pgf
 * pass new magic arg to regcomp, and
 * fixed bug in openup/opendown pair
 *
 * Revision 1.41  1991/11/13  20:09:27  pgf
 * X11 changes, from dave lemke
 *
 * Revision 1.40  1991/11/10  22:02:09  pgf
 * fixed bug in openup(), and made counts work right
 *
 * Revision 1.39  1991/11/08  14:13:58  pgf
 * fixed settab() and setfillcol()
 *
 * Revision 1.38  1991/11/08  13:10:26  pgf
 * lint cleanup, and
 * put in dave lemke's "autoinsert works going up" fix, and
 * made autoinsert clean up pre-existing leading whitespace
 *
 * Revision 1.37  1991/11/06  23:28:08  pgf
 * getfence() will scan for a fence if not on one to begin with.  it'll
 * scan in either direction, depending on arg to matchfence or matchfenceback
 *
 * Revision 1.36  1991/11/03  17:46:30  pgf
 * removed f,n args from all region functions -- they don't use them,
 * since they're no longer directly called by the user
 *
 * Revision 1.35  1991/11/01  14:38:00  pgf
 * saber cleanup
 *
 * Revision 1.34  1991/10/29  03:04:45  pgf
 * fixed argument mismatches
 *
 * Revision 1.33  1991/10/28  14:21:52  pgf
 * eliminated TABVAL and fillcol macros, converted gasave to per-buffer
 *
 * Revision 1.32  1991/10/27  01:51:48  pgf
 * new regex values are now settable and displayable
 *
 * Revision 1.31  1991/10/24  13:01:03  pgf
 * bug fix for append'ing in empty buffer
 *
 * Revision 1.30  1991/10/18  10:56:54  pgf
 * added code to use modified VALUE structures and lists to display settings
 * more easily (adjvalueset).  also removed some old ifdefs.
 *
 * Revision 1.29  1991/10/15  12:01:20  pgf
 * added backspacelimit support, and fewer tty chars are hard-coded now
 *
 * Revision 1.28  1991/10/08  01:09:38	pgf
 * added ^W and ^U processing to ins().  should make this depend on the tty
 * settings, and add option to control whether it's okay to backspace or
 * line or word kill back beyond the insert point
 *
 * Revision 1.27  1991/09/30  01:47:24	pgf
 * reformat listing of values a bit
 *
 * Revision 1.26  1991/09/26  13:13:07	pgf
 * created window values, and allow them to be displayed and set.  still needs
 * cleaning up
 *
 * Revision 1.25  1991/09/24  02:07:29	pgf
 * suppressed extra whitespace in listmodes
 *
 * Revision 1.24  1991/09/24  01:03:49	pgf
 * pass FALSE to gotoeol to avoid unnecessary work
 *
 * Revision 1.23  1991/09/23  01:57:20	pgf
 * n
 * don't pass null pointers to strcmp()
 *
 * Revision 1.22  1991/09/20  13:11:53	pgf
 * add local value settings to the listmodes output
 *
 * Revision 1.21  1991/09/19  13:39:53	pgf
 * added shortname synonyms to mode and value names
 *
 * Revision 1.20  1991/09/10  01:21:34	pgf
 * re-tabbed
 *
 * Revision 1.19  1991/09/10  00:43:31	pgf
 * ifdef'ed out obsolete "showm", and change calls to it to "listmodes"
 *
 * Revision 1.18  1991/08/16  11:12:26	pgf
 * added the third flavor of insertmode for replace-char, and
 * added support for the insertmode indicator on the modeline, and
 * created the catnap routine for use by fmatch, and the typahead
 * check in the ANSI_SPEC code in kbd_key
 *
 * Revision 1.17  91/08/13	12:51:29  pgf
 * make sure we pass a non-NULL arg to poll, even though the count is zero
 *
 * Revision 1.16  1991/08/13  02:52:25	pgf
 * the fmatch() code now works if you have poll or select, and
 * is enabled by "set showmatch"
 *
 * Revision 1.15  1991/08/12  15:06:21	pgf
 * added ANSI_SPEC capability -- can now use the arrow keys from
 * command or insert mode
 *
 * Revision 1.14  1991/08/08  13:21:25	pgf
 * removed ifdef BEFORE
 *
 * Revision 1.13  1991/08/07  12:35:07	pgf
 * added RCS log messages
 *
 * revision 1.12
 * date: 1991/08/06 15:25:02;
 *	global/local values
 * and list changes
 *
 * revision 1.11
 * date: 1991/06/26 09:35:48;
 * made count work correctly on flipchar, and
 * removed old ifdef BEFORE stuff
 *
 * revision 1.10
 * date: 1991/06/25 19:53:09;
 * massive data structure restructure
 *
 * revision 1.9
 * date: 1991/06/20 17:23:24;
 * fixed & --> && problem in indent_newline
 *
 * revision 1.8
 * date: 1991/06/16 17:38:02;
 * switched to modulo tab calculations, and
 * converted entab, detab, and trim to work on regions,
 * and stripped some old ifdef NOCOUNT stuff for openup and opendown
 * and added support for local and globla tabstop and fillcol values
 *
 * revision 1.7
 * date: 1991/06/15 09:09:27;
 * changed some forwchar calls to forwchar_to_eol, and
 * now prevent 'x', 's', 'r' from destroying the newline when used on
 * empty lines
 *
 * revision 1.6
 * date: 1991/06/06 13:58:23;
 * added autoindent mode and "set all"
 *
 * revision 1.5
 * date: 1991/05/31 11:19:06;
 * added showlength function, for "=" ex command, and
 * switched from stutterfunc to godotplus
 *
 * revision 1.4
 * date: 1991/04/04 09:40:25;
 * fixed autoinsert bug
 *
 * revision 1.3
 * date: 1991/02/12 09:49:33;
 * doindent had an unset return value, causing autoindents of 0 chars to fail
 *
 * revision 1.2
 * date: 1990/10/03 16:01:00;
 * make backspace work for everyone
 *
 * revision 1.1
 * date: 1990/09/21 10:25:54;
 * initial vile RCS revision
 */

#include	"estruct.h"
#include	"edef.h"
#include	"glob.h"

#if HAVE_POLL
# include <poll.h>
#endif
#if HAVE_SELECT && AIX
# include <sys/select.h>
#endif

#if WATCOM
#   include <direct.h>
#endif

#if TURBO
#   include <dir.h>
#endif

#if GO32
#   include <dirent.h>
#endif

extern CMDFUNC f_forwchar, f_backchar, f_forwchar_to_eol, f_backchar_to_bol;

/*--------------------------------------------------------------------------*/
#if MSDOS
static	int	drive2char P(( int ));
static	int	char2drive P(( int ));
#endif

/*--------------------------------------------------------------------------*/

/*
 * Set default parameters for an automatically-generated, view-only buffer.
 * This is invoked after buffer creation, but usually before the buffer is
 * loaded with text.
 */
void
set_rdonly(bp, name)
BUFFER	*bp;
char	*name;
{
	ch_fname(bp, name);

	b_clr_changed(bp);		/* assumes text is loaded... */
	bp->b_active = TRUE;

	make_local_b_val(bp,MDVIEW);
	set_b_val(bp,MDVIEW,TRUE);

	make_local_b_val(bp,VAL_TAB);
	set_b_val(bp,VAL_TAB,8);

	make_local_b_val(bp,MDDOS);
	set_b_val(bp,MDDOS,FALSE);

	make_local_b_val(bp,MDCMOD);
	set_b_val(bp,MDCMOD,FALSE);
}

/* generic "lister", which takes care of popping a window/buffer pair under
	the given name, and calling "func" with a couple of args to fill in
	the buffer */
int
liststuff(name,func,iarg,carg)
char *name;
void (*func) P(( int, char *));	/* ptr to function to execute */
int iarg;
char *carg;
{
	register BUFFER *bp;
	register int	s;
	WINDOW  *wp;

	/* create the buffer list buffer   */
	bp = bfind(name, BFSCRTCH);
	if (bp == NULL)
		return FALSE;

	if ((s=bclear(bp)) != TRUE) /* clear old text (?) */
		return (s);
	b_set_scratch(bp);
	if (popupbuff(bp) == FALSE) {
		(void)zotbuf(bp);
		return (FALSE);
	}

	if ((wp = bp2any_wp(bp)) != NULL) {
		make_local_w_val(wp,WMDNUMBER);
		set_w_val(wp,WMDNUMBER,FALSE);
	}
	/* call the passed in function, giving it both the integer and
		character pointer arguments */
	(*func)(iarg,carg);
	(void)gotobob(FALSE,1);

	set_rdonly(bp, non_filename());
	return TRUE;
}

/*
 * Display the current position of the cursor, lines and columns, in the file,
 * the character that is under the cursor (in hex), and the fraction of the
 * text that is before the cursor. The displayed column is not the current
 * column, but the column that would be used on an infinite width display.
 */
/* ARGSUSED */
int
showcpos(f, n)
int f,n;
{
	register LINE	*lp;		/* current line */
	register B_COUNT numchars = 0;	/* # of chars in file */
	register L_NUM	 numlines = 0;	/* # of lines in file */
	register B_COUNT predchars = 0;	/* # chars preceding point */
	register L_NUM	 predlines = 0;	/* # lines preceding point */
	register int	curchar = '\n';	/* character under cursor */
	long ratio;
	C_NUM col;
	C_NUM savepos;			/* temp save for current offset */
	C_NUM ecol;			/* column pos/end of current line */

	/* count chars and lines */
	for_each_line(lp, curbp) {
		/* if we are on the current line, record it */
		if (lp == l_ref(DOT.l)) {
			predlines = numlines;
			predchars = numchars + DOT.o;
			if (DOT.o == llength(lp))
				curchar = '\n';
			else
				curchar = char_at(DOT);
		}
		/* on to the next line */
		++numlines;
		numchars += llength(lp) + 1;
	}

	if (!b_val(curbp,MDNEWLINE))
		numchars--;

	/* if at end of file, record it */
	if (is_header_line(DOT,curbp)) {
		predlines = numlines;
		predchars = numchars;
	}

	/* Get real column and end-of-line column. */
	col = getccol(FALSE);
	savepos = DOT.o;
	DOT.o = lLength(DOT.l);
	ecol = getccol(FALSE);
	DOT.o = savepos;

	ratio = 0;		/* Ratio before dot. */
	if (numchars != 0)
		ratio = (100L*predchars) / numchars;

	/* summarize and report the info */
	mlforce(
"Line %d of %d, Col %d of %d, Char %D of %D (%D%%) char is 0x%x",
		predlines+1, numlines, col+1, ecol,
		predchars+1, numchars, ratio, curchar);
	return TRUE;
}

/* ARGSUSED */
int
showlength(f,n)
int f,n;
{
	/* actually, can be used to show any address-value */
	mlforce("%d", line_no(curbp, MK.l));
	return TRUE;
}

void
line_report(before)
L_NUM	before;
{
	L_NUM	after = line_count(curbp);

	if (do_report(before-after)) {
		if (before > after)
			mlforce("[%d fewer lines]", before - after);
		else
			mlforce("[%d more lines]", after - before);
	}
}

L_NUM
line_count(the_buffer)
BUFFER *the_buffer;
{
#if !SMALLER
	(void)bsizes(the_buffer);
	return the_buffer->b_linecount;
#else
	register LINE	*lp;		/* current line */
	register L_NUM	numlines = 0;	/* # of lines in file */

	for_each_line(lp, the_buffer)
		++numlines;

	return numlines;
#endif
}

L_NUM
line_no(the_buffer, the_line)	/* return the number of the given line */
BUFFER *the_buffer;
LINEPTR the_line;
{
	if (!same_ptr(the_line, null_ptr)) {
#if !SMALLER
		L_NUM	it;
		(void)bsizes(the_buffer);
		if ((it = l_ref(the_line)->l_number) == 0)
			it = the_buffer->b_linecount + 1;
		return it;
#else
		register LINE	*lp;		/* current line */
		register L_NUM	numlines = 0;	/* # of lines before point */

		for_each_line(lp, the_buffer) {
			/* if we are on the specified line, record it */
			if (lp == l_ref(the_line))
				break;
			++numlines;
		}

		/* and return the resulting count */
		return(numlines + 1);
#endif
	}
	return 0;
}

#if OPT_EVAL
L_NUM
getcline()	/* get the current line number */
{
	return line_no(curbp, DOT.l);
}
#endif

/*
 * Return current screen column.  Stop at first non-blank given TRUE argument.
 */
int
getccol(bflg)
int bflg;
{
	register int c, i, col;
	col = 0;
	for (i = w_left_margin(curwp); i < DOT.o; ++i) {
		c = lGetc(DOT.l, i);
		if (!isspace(c) && bflg)
			break;
		col = next_column(c,col);
	}
	return col;
}


/*
 * Set current column, based on counting from 1
 */
int
gotocol(f,n)
int f,n;
{
	if (!f || n <= 0)
		n = 1;
	return gocol(n - 1);
}

/* really set column, based on counting from 0, for internal use */
int
gocol(n)
int n;
{
	register int c;		/* character being scanned */
	register int i;		/* index into current line */
	register int col;	/* current cursor column   */
	register int llen;	/* length of line in bytes */

	col = 0;
	llen = lLength(DOT.l);

	/* scan the line until we are at or past the target column */
	for (i = w_left_margin(curwp); i < llen; ++i) {
		/* upon reaching the target, drop out */
		if (col >= n)
			break;

		/* advance one character */
		c = lGetc(DOT.l, i);
		col = next_column(c,col);
	}

	/* set us at the new position */
	DOT.o = i;

	/* and tell whether we made it */
	return(col >= n);
}

#if ! SMALLER
/*
 * Twiddle the two characters on either side of dot. If dot is at the end of
 * the line twiddle the two characters before it. Return with an error if dot
 * is at the beginning of line; it seems to be a bit pointless to make this
 * work. This fixes up a very common typo with a single stroke.
 * This always works within a line, so "WFEDIT" is good enough.
 */
/* ARGSUSED */
int
twiddle(f, n)
int f,n;
{
	MARK		dot;
	register int	cl;
	register int	cr;

	dot = DOT;
	if (is_empty_line(dot))
		return (FALSE);
	--dot.o;
	cr = char_at(dot);
	if (--dot.o < 0)
		return (FALSE);
	cl = char_at(dot);
	copy_for_undo(dot.l);
	put_char_at(dot, cr);
	++dot.o;
	put_char_at(dot, cl);
	chg_buff(curbp, WFEDIT);
	return (TRUE);
}
#endif


/*
 * change all tabs in the line to the right number of spaces.
 * leadingonly says only do leading whitespace
 */
int
detabline(leadingonly)
int leadingonly;
{
	register int	s;
	register int	c;
	int	ocol;

	ocol = getccol(FALSE);

	DOT.o = 0;

	/* detab the entire current line */
	while (DOT.o < lLength(DOT.l)) {
		c = char_at(DOT);
		if (leadingonly && !isspace(c))
			break;
		/* if we have a tab */
		if (c == '\t') {
			if ((s = ldelete(1L, FALSE)) != TRUE) {
				return s;
			}
			insspace( TRUE, curtabval - (DOT.o % curtabval) );
		}
		DOT.o++;
	}
	(void)gocol(ocol);
	return TRUE;
}

/*
 * change all tabs in the region to the right number of spaces
 */
int
detab_region()
{
	return do_fl_region(detabline,FALSE);
}

/*
 * convert all appropriate spaces in the line to tab characters.
 * leadingonly says only do leading whitespace
 */
int
entabline(leadingonly)
int leadingonly;
{
	register int fspace;	/* pointer to first space if in a run */
	register int ccol;	/* current cursor column */
	register char cchar;	/* current character */
	int	ocol;

	ocol = getccol(FALSE);

	/* entab the current line */
	/* would this have been easier if it had started at
		the _end_ of the line, rather than the beginning?  -pgf */
	fspace = -1;
	ccol = 0;

	detabline(leadingonly);	/* get rid of possible existing tabs */
	DOT.o = 0;
	while (1) {
		/* see if it is time to compress */
		if ((fspace >= 0) && (nextab(fspace) <= ccol))
			if (ccol - fspace < 2)
				fspace = -1;
			else {
				backchar(TRUE, ccol - fspace);
				(void)ldelete((long)(ccol - fspace), FALSE);
				linsert(1, '\t');
				fspace = -1;
			}

		if (DOT.o >= lLength(DOT.l))
			break;

		/* get the current character */
		cchar = char_at(DOT);

		if (cchar == ' ') { /* a space...compress? */
			if (fspace == -1)
				fspace = ccol;
		} else {
			if (leadingonly)
				break;
			fspace = -1;
		}
		ccol++;
		DOT.o++;
	}
	(void)gocol(ocol);
	return TRUE;
}

/*
 * convert all appropriate spaces in the region to tab characters
 */
int
entab_region()
{
	return do_fl_region(entabline,FALSE);
}

/* trim trailing whitespace from a line.  leave dot at end of line */
/*ARGSUSED*/
int
trimline(flag)
int	flag;
{
	register int off, orig;
	register LINE *lp;

	lp = l_ref(DOT.l);

	off = llength(lp)-1;
	orig = off;
	while (off >= 0) {
		if (!isspace(lgetc(lp,off)))
			break;
		off--;
	}

	if (off == orig)
		return TRUE;

	DOT.o = off+1;

	return ldelete((long)(orig - off),FALSE);
}

/*
 * trim trailing whitespace from a region
 */
int
trim_region()
{
	return do_fl_region(trimline,0);
}


#if AEDIT
/*
 * Delete blank lines around dot. What this command does depends if dot is
 * sitting on a blank line. If dot is sitting on a blank line, this command
 * deletes all the blank lines above and below the current line. If it is
 * sitting on a non blank line then it deletes all of the blank lines after
 * the line. Any argument is ignored.
 */
/* ARGSUSED */
int
deblank(f, n)
int f,n;
{
	register LINE	*lp1;
	register LINE	*lp2;
	long nld;

	lp1 = l_ref(DOT.l);
	while (llength(lp1)==0 && (lp2=lback(lp1))!=l_ref(curbp->b_line.l))
		lp1 = lp2;
	lp2 = lp1;
	nld = 0;
	while ((lp2=lforw(lp2))!=l_ref(curbp->b_line.l) && llength(lp2)==0)
		++nld;
	if (nld == 0)
		return (TRUE);
	DOT.l = l_ptr(lforw(lp1));
	DOT.o = 0;
	return (ldelete(nld, FALSE));
}

#endif

/* '~' is synonymous with 'M-~<space>' */
int
flipchar(f, n)
int f,n;
{
	int s;

	havemotion = &f_forwchar_to_eol;
	s = operflip(f,n);
	if (s != TRUE)
		return s;
	return forwchar_to_eol(f,n);
}

/* 'x' is synonymous with 'd<space>' */
int
forwdelchar(f, n)
int f,n;
{

	havemotion = &f_forwchar_to_eol;
	return operdel(f,n);
}

/* 'X' is synonymous with 'd<backspace>' */
int
backdelchar(f, n)
int f,n;
{
	havemotion = &f_backchar_to_bol;
	return operdel(f,n);
}

/* 'D' is synonymous with 'd$' */
/* ARGSUSED */
int
deltoeol(f, n)
int f,n;
{
	extern CMDFUNC f_gotoeol;

	if (lLength(DOT.l) == 0)
		return TRUE;

	havemotion = &f_gotoeol;
	return operdel(FALSE,1);
}

/* 'C' is synonymous with 'c$' */
int
chgtoeol(f, n)
int f,n;
{
	extern CMDFUNC f_gotoeol;

	if (lLength(DOT.l) == 0) {
		return ins();
	} else {
		havemotion = &f_gotoeol;
		return operchg(f,n);
	}
}

/* 'Y' is synonymous with 'yy' */
int
yankline(f, n)
int f,n;
{
	extern CMDFUNC f_godotplus;
	havemotion = &f_godotplus;
	return(operyank(f,n));
}

/* 'S' is synonymous with 'cc' */
int
chgline(f, n)
int f,n;
{
	extern CMDFUNC f_godotplus;
	havemotion = &f_godotplus;
	return(operchg(f,n));
}

/* 's' is synonymous with 'c<space>' */
int
chgchar(f, n)
int f,n;
{
	havemotion = &f_forwchar_to_eol;
	return(operchg(f,n));
}



/*	This function simply clears the message line,
		mainly for macro usage			*/

/* ARGSUSED */
int
clrmes(f, n)
int f, n;	/* arguments ignored */
{
	mlerase();
	return(TRUE);
}

#if ! SMALLER

/*	This function writes a string on the message line
		mainly for macro usage			*/

/* ARGSUSED */
int
writemsg(f, n)
int f, n;	/* arguments ignored */
{
	register int status;
	char buf[NPAT];		/* buffer to receive message into */

	buf[0] = EOS;
	if ((status = mlreply("Message to write: ", buf, sizeof(buf))) != TRUE)
		return(status);

	/* write the message out */
	mlforce("%s",buf);
	return(TRUE);
}
#endif


/* delay for the given number of milliseconds.  if "watchinput" is true,
	then user input will abort the delay
	FIXXXX -- the second arg is only implemented for UNIX with select() */
void
catnap(milli,watchinput)
int milli;
int watchinput;
{
#if UNIX
# if HAVE_SELECT

	struct timeval tval;
	fd_set read_bits;

	FD_ZERO(&read_bits);
	if (watchinput) {
		FD_SET(0, &read_bits);
	}
	tval.tv_sec = 0;
	tval.tv_usec = milli * 1000;	/* microseconds */
	(void)select (1, &read_bits, (fd_set*)0, (fd_set*)0, &tval);

# else
#  if HAVE_POLL

	struct pollfd pfd;
	(void)poll(&pfd, 0, milli); /* milliseconds */

#  else

	sleep(1); /* 1 second.	ugh. */

#  endif
# endif
#endif

#if VMS
	float	seconds = milli/1000.;
	lib$wait(&seconds);
#endif

#if TURBO
	delay(milli);
#endif

#if !(UNIX|VMS|TURBO)
	long i;
	for (i = 0; i < term.t_pause; i++)
		;
#endif
}

#if UNIX
#include	<sys/param.h>
#endif

/* return a string naming the current directory */
char *
current_directory(force)
int force;
{
	char *s;
	static char	dirname[NFILEN];
	static char *cwd;

	if (!force && cwd)
		return cwd;
#if USG && ! POSIX
	{
	FILE *f, *npopen();
	int n;
	f = npopen("pwd", "r");
	if (f == NULL) {
		npclose(f);
		return NULL;
	}
	n = fread(dirname, 1, NFILEN, f);

	dirname[n] = EOS;
	npclose(f);
	cwd = dirname;
	}
#else
# if (MSDOS & NEWDOSCC) || POSIX || VMS
	cwd = getcwd(dirname, NFILEN);
# else
	cwd = getwd(dirname);
# endif
#endif
#if MSDOS
	(void)mklower(cwd);
#endif
	if (cwd == NULL) {
		cwd = dirname;
		dirname[0] = slash;
		dirname[1] = EOS;
	} else {
		s = strchr(cwd, '\n');
		if (s)
			*s = EOS;
	}

#if MSDOS && NEWDOSCC
	update_dos_drv_dir(cwd);
#endif

	return cwd;
}

#if MSDOS

/* convert drive index to _letter_ */
static int
drive2char(d)
int	d;
{
	if (d < 0 || d >= 26) {
		mlforce("[Illegal drive index %d]", d);
		d = 0;
	}
	return (d + 'A');
}

/* convert drive _letter_ to index */
static int
char2drive(d)
int	d;
{
	if (isalpha(d)) {
		if (islower(d))
			d = toupper(d);
	} else {
		mlforce("[Not a drive '%c']", d);
		d = curdrive();
	}
	return (d - 'A');
}

/* returns drive _letter_ */
int
curdrive()
{
#if GO32
	union REGS  r;
	r.h.ah = 0x19;
	int86(0x21, &r, &r);
	return drive2char(r.h.al);
#else
	return drive2char(bdos(0x19, 0, 0) & 0xff);
#endif
}

/* take drive _letter_ as arg. */
int
setdrive(d)
int d;
{
	if (isalpha(d)) {
		bdos(0x0e, char2drive(d), 0);
		return TRUE;
	}
	mlforce("[Bad drive specifier]");
	return FALSE;
}


static int curd;		/* current drive-letter */
static char *cwds[26];		/* list of current dirs on each drive */

char *
curr_dir_on_drive(drive)
int drive;
{
	int	n = char2drive(drive);

	if (n != 0) {
		if (curd == 0)
			curd = curdrive();

		if (cwds[n])
			return cwds[n];
		else {
			cwds[n] = castalloc(char,NFILEN);

			if (cwds[n]) {
				if (setdrive(drive) == TRUE) {
					(void)strcpy(cwds[n], current_directory(TRUE));
					(void)setdrive(curd);
					(void)current_directory(TRUE);
					return cwds[n];
				}
			}
		}
	}
	return current_directory(FALSE);
}

void
update_dos_drv_dir(cwd)
char *cwd;
{
	char	*s;

	if ((s = is_msdos_drive(cwd)) != 0) {
		int n = char2drive(*cwd);

		if (!cwds[n])
			cwds[n] = castalloc(char,NFILEN);

		if (cwds[n])
			(void)strcpy(cwds[n], s);
	}
}
#endif


/* ARGSUSED */
int
cd(f,n)
int f, n;
{
	register int status;
	static	TBUFF	*last;
	char cdirname[NFILEN];

	status = mlreply_dir("Change to directory: ", &last, cdirname);
#if UNIX || VMS
	if (status == FALSE) {		/* empty reply, go HOME */
#if UNIX
		(void)lengthen_path(strcpy(cdirname, "~"));
#else	/* VMS */
		(void)strcpy(cdirname, "sys$login");
#endif
	} else
#endif
	if (status != TRUE)
		return status;

	return set_directory(cdirname);
}

/* ARGSUSED */
int
pwd(f,n)
int f, n;
{
	mlforce("%s",current_directory(f));
	return TRUE;
}

/* move to the named directory.  (Dave Lemke) */
int
set_directory(dir)
char	*dir;
{
    char       exdir[NFILEN];
    char *exdp;
#if MSDOS
    int curd = curdrive();
#endif
    WINDOW *wp;

    for_each_window(wp)
	wp->w_flag |= WFMODE;

    exdp = strcpy(exdir, dir);

    if (doglob(exdp)) {
#if MSDOS
	char	*s;
	if ((s = is_msdos_drive(exdp)) != 0) {
		if (setdrive(*exdp) == TRUE) {
			exdp = s;	/* skip device-part */
			if (!*exdp) {
				return pwd(TRUE,1);
			}
		} else {
			return FALSE;
		}
	}
#endif
	if (chdir(exdp) == 0) {
		(void)pwd(TRUE,1);
		updatelistbuffers();
		return TRUE;
	}
    }
#if MSDOS
    setdrive(curd);
    current_directory(TRUE);
#endif
    mlforce("[Couldn't change to \"%s\"]", exdir);
    return FALSE;
}

void
ch_fname(bp,fname)
BUFFER *bp;
char *fname;
{
	int len;
	char nfilen[NFILEN];
	char *np;
	char *holdp = NULL;

	np = fname;

	/* produce a full pathname, unless already absolute or "internal" */
	if (!isInternalName(np))
		np = lengthen_path(strcpy(nfilen, np));

	len = strlen(np)+1;

	if (bp->b_fname == 0 || strcmp(bp->b_fname, np)) {

		if (bp->b_fname && bp->b_fnlen < len ) {
			/* don't free it yet -- it _may_ have been passed in as
			 * the current file-name
			 */
			holdp = bp->b_fname;
			bp->b_fname = NULL;
		}

		if (!bp->b_fname) {
			bp->b_fname = strmalloc(np);
			if (!bp->b_fname) {
				bp->b_fname = out_of_mem;
				bp->b_fnlen = strlen(bp->b_fname);
				return;
			}
			bp->b_fnlen = len;
		}

		/* it'll fit, leave len untouched */
		(void)strcpy(bp->b_fname, np);

		if (holdp != out_of_mem)
			FreeIfNeeded(holdp);
		updatelistbuffers();
	}
#ifdef	MDCHK_MODTIME
	(void)get_modtime(bp, &(bp->b_modtime));
	bp->b_modtime_at_warn = 0;
#endif
}
