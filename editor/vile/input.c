/*	INPUT:	Various input routines for MicroEMACS
 *		written by Daniel Lawrence
 *		5/9/86
 *
 * $Log: input.c,v $
 * Revision 1.1  1994/02/01 03:29:27  jkh
 * Initial revision
 *
 * Revision 1.87  1993/09/10  16:06:49  pgf
 * tom's 3.61 changes
 *
 * Revision 1.86  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.85  1993/08/18  15:10:36  pgf
 * don't let the OPT_XTERM code do anything under X11
 *
 * Revision 1.84  1993/08/13  16:32:50  pgf
 * tom's 3.58 changes
 *
 * Revision 1.83  1993/07/27  18:06:20  pgf
 * see tom's 3.56 CHANGES entry
 *
 * Revision 1.82  1993/07/15  12:00:00  pgf
 * added mlquickask(), which does "raw" single character response queries
 *
 * Revision 1.81  1993/06/29  11:09:47  pgf
 * changed 'naptime' to 'timeout-value'
 *
 * Revision 1.80  1993/06/28  20:05:49  pgf
 * removed ifdef BEFORE stuff
 *
 * Revision 1.79  1993/06/28  15:07:35  pgf
 * when killing chars, check for c == killc or wkillc _before_ checking it
 * against backspace, in case DEL is someones killc or wkillc.
 *
 * Revision 1.78  1993/06/28  14:27:38  pgf
 * implemented GVAL_NAPTIME, as argument to catnap().  also added new arg
 * to catnap call, that terminates the nap when user input observed.
 *
 * Revision 1.77  1993/06/02  14:28:47  pgf
 * see tom's 3.48 CHANGES
 *
 * Revision 1.76  1993/05/24  15:25:41  pgf
 * tom's 3.47 changes, part b
 *
 * Revision 1.75  1993/05/05  10:31:30  pgf
 * cleaned up handling of SPEC keys from withing insert mode.  now, any
 * function bound to a SPECkey (i.e. any FN-? thing) can be executed
 * either from inside or outside insert mode.
 *
 * Revision 1.74  1993/05/04  17:05:14  pgf
 * see tom's CHANGES, 3.45
 *
 * Revision 1.73  1993/04/28  17:11:22  pgf
 * got rid of NeWS ifdefs
 *
 * Revision 1.72  1993/04/28  14:34:11  pgf
 * see CHANGES, 3.44 (tom)
 *
 * Revision 1.71  1993/04/22  11:17:00  pgf
 * support for dotcmdkreg
 *
 * Revision 1.70  1993/04/21  14:08:04  pgf
 * made dotcmdcnt static again
 *
 * Revision 1.69  1993/04/21  13:53:50  pgf
 * make repeat counts on '.' take precedence over original repeat counts
 *
 * Revision 1.68  1993/04/20  12:18:32  pgf
 * see tom's 3.43 CHANGES
 *
 * Revision 1.67  1993/04/07  13:54:23  pgf
 * fix bug in incr_dot_kregnum so "1P...  works correctly again.  this
 * was a mistranslation when the tbuffs were introduced
 *
 * Revision 1.66  1993/04/07  13:51:52  pgf
 * undo the recording glitch fix (1.61) which used to be reproducible with
 * ix<ESC>j.j.j.j.j.j.j typed very quickly.  this doesn't happen anymore,
 * and now i can do dd..."1P... and only have deletes happen when they're
 * supposed to.
 *
 * Revision 1.65  1993/04/01  13:07:50  pgf
 * see tom's 3.40 CHANGES
 *
 * Revision 1.64  1993/04/01  12:05:46  pgf
 * add setjmp/longjmp to tgetc() and catchintr(), so that ^C gets
 * through on BSD-style signals
 *
 * Revision 1.63  1993/03/25  19:50:58  pgf
 * see 3.39 section of CHANGES
 *
 * Revision 1.62  1993/03/19  12:33:18  pgf
 * fix for recording glitch
 *
 * Revision 1.61  1993/03/18  17:42:20  pgf
 * see 3.38 section of CHANGES
 *
 * Revision 1.60  1993/03/17  09:57:59  pgf
 * fix for calling shorten_path() correctly
 *
 * Revision 1.59  1993/03/16  10:53:21  pgf
 * see 3.36 section of CHANGES file
 *
 * Revision 1.58  1993/03/15  12:01:59  pgf
 * another fix to kbd_reply, so that search delimiters work correctly
 *
 * Revision 1.57  1993/03/11  17:55:36  pgf
 * don't return TRUE from kbd_reply if backspaced past start of buff
 *
 * Revision 1.56  1993/03/05  17:50:54  pgf
 * see CHANGES, 3.35 section
 *
 * Revision 1.55  1993/02/24  10:59:02  pgf
 * see 3.34 changes, in CHANGES file
 *
 * Revision 1.54  1993/02/15  10:37:31  pgf
 * cleanup for gcc-2.3's -Wall warnings
 *
 * Revision 1.53  1993/02/12  10:42:55  pgf
 * use insertion_cmd() routine to get the char that puts us in insert mode
 *
 * Revision 1.52  1993/02/08  14:53:35  pgf
 * see CHANGES, 3.32 section
 *
 * Revision 1.51  1993/01/16  10:35:40  foxharp
 * support for scrtch and shpipe chars in screen_string(), and use find_alt()
 * to pick up name of '#' buffer, rather than hist_lookup(1)
 *
 * Revision 1.50  1992/12/13  13:32:36  foxharp
 * got rid of extraneous assign
 *
 * Revision 1.49  1992/12/05  13:22:10  foxharp
 * make sure we escape eolchar with '\' if passed in in kbd_strings buffer,
 * since the user would have had to type the '\' to put it there themselves
 *
 * Revision 1.48  1992/12/04  09:25:34  foxharp
 * deleted unused assigns, no longer propagate pointer to block local
 * in kbd_string, and fix expansion arg to mlreply_no_bs()
 *
 * Revision 1.47  1992/12/03  00:32:59  foxharp
 * added new mlreply_no_bs, which doesn't do backslash processing
 *
 * Revision 1.46  1992/11/19  09:07:30  foxharp
 * added check on recursive replay in dotcmdplay() -- I think we should
 * never play or record a call to dotcmdplay, so we abort if we find ourselves
 * doing so.
 * also added kdone() call to finish up after ksetup()
 *
 * Revision 1.45  1992/08/20  23:40:48  foxharp
 * typo fixes -- thanks, eric
 *
 * Revision 1.44  1992/07/24  07:49:38  foxharp
 * shorten_name changes
 *
 * Revision 1.43  1992/07/18  13:13:56  foxharp
 * put all path-shortening in one place (shorten_path()), and took out some old code now
 * unnecessary
 *
 * Revision 1.42  1992/07/16  22:08:34  foxharp
 * make keyboard macros redoable -- when out of input on a dotcmd, be
 * sure to check for pending kbdmode input
 *
 * Revision 1.41  1992/07/15  23:23:12  foxharp
 * made '80i-ESC' work
 *
 * Revision 1.40  1992/07/04  14:32:08  foxharp
 * rearranged/improved the insertmode arrow key code
 *
 * Revision 1.39  1992/06/25  23:00:50  foxharp
 * changes for dos/ibmpc
 *
 * Revision 1.38  1992/05/19  08:55:44  foxharp
 * more prototype and shadowed decl fixups
 *
 * Revision 1.37  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.36  1992/03/19  23:21:33  pgf
 * linux portability (pathn)
 *
 * Revision 1.35  1992/03/07  10:28:52  pgf
 * don't write a null past the end of the input buffer in kbd_string
 *
 * Revision 1.34  1992/03/03  21:58:32  pgf
 * minor optimization in screen_string
 *
 * Revision 1.33  1992/03/03  09:35:52  pgf
 * added support for getting "words" out of the buffer via variables --
 * needed _nonspace character type
 *
 * Revision 1.32  1992/03/03  08:42:01  pgf
 * took out pre_colon_pos
 *
 * Revision 1.31  1992/02/17  09:05:12  pgf
 * make "RECORDED_ESC" work on machines whose natural chars are unsigned, and
 * add support for "pre_colon_pos", which is the value of DOT just before the
 * named command was run -- this lets ':' expand correctly in all cases
 *
 * Revision 1.30  1992/01/06  23:10:56  pgf
 * try no update() in get_recorded_char() -- don't know why it's
 * necessary
 *
 * Revision 1.29  1992/01/05  00:06:13  pgf
 * split mlwrite into mlwrite/mlprompt/mlforce to make errors visible more
 * often.  also normalized message appearance somewhat.
 *
 * Revision 1.28  1991/12/11  06:30:58  pgf
 * fixed backslashing, yet again -- should now be able to search for a
 * slash in a buffer
 *
 * Revision 1.27  1991/11/08  13:24:05  pgf
 * ifdefed unused function
 *
 * Revision 1.26  1991/11/01  14:38:00  pgf
 * saber cleanup
 *
 * Revision 1.25  1991/10/29  03:00:53  pgf
 * added speckey function, for '#' prefixing, and allow ESC O x in addition
 * to ESC [ x as ANSI fkeys
 *
 * Revision 1.24  1991/10/23  14:20:53  pgf
 * changes to fix interactions between dotcmdmode and kbdmode and tungetc
 *
 * Revision 1.23  1991/10/22  14:36:21  pgf
 * bug in ANSI_SPEC -- local declaration of f_insert hid global
 *
 * Revision 1.22  1991/10/22  03:08:09  pgf
 * made wkillc work in kbd_string
 *
 * Revision 1.21  1991/09/26  13:15:03  pgf
 * make backslash processing optional in kbd_string, and
 * fix type mismatch in ANSI_SPEC code (f_insert)
 *
 * Revision 1.20  1991/09/17  00:51:17  pgf
 * fixed even more backslashing bugs
 *
 * Revision 1.19  1991/09/13  03:27:06  pgf
 * bugfix for backslash changes
 *
 * Revision 1.18  1991/09/13  03:06:39  pgf
 * backslashing now works -- expansion chars and backslashes can be
 * escaped properly
 *
 * Revision 1.17  1991/09/12  13:03:16  pgf
 * kbd_string now recognizes leading eolchar corectly, but there are still
 * problems with trying to quote it, as in :s/xxx/\//g to change xxx to / .
 *
 * Revision 1.16  1991/09/12  12:27:41	pgf
 * un-record characters pushed back with tungetc
 *
 * Revision 1.15  1991/09/10  00:46:57	pgf
 * cleanup of the dotcmd stuff, to prevent catnap() for escape sequences
 * during playback
 *
 * Revision 1.14  1991/08/16  11:01:39	pgf
 * added catnap() before typahead check on esc char in ANSI_SPEC, and
 * added REPLACECHAR special check on ANSI_SPEC, and
 * allow quoting of %, #, :, with \ in kbd_string, so they don't expand
 *
 * Revision 1.13  1991/08/12  15:06:21	pgf
 * added ANSI_SPEC capability -- can now use the arrow keys from
 * command or insert mode
 *
 * Revision 1.12  1991/08/12  10:24:16	pgf
 * interrupts can now interrupt keyboard recording
 *
 * Revision 1.11  1991/08/07  12:35:07	pgf
 * added RCS log messages
 *
 * revision 1.10
 * date: 1991/06/26 09:37:37;
 * removed ifdef BEFORE
 * 
 * revision 1.9
 * date: 1991/06/25 19:52:47;
 * massive data structure restructure
 * 
 * revision 1.8
 * date: 1991/06/04 09:20:31;
 * kcod2key is now a macro
 * 
 * revision 1.7
 * date: 1991/06/03 17:34:53;
 * switch from "meta" etc. to "ctla" etc.
 * 
 * revision 1.6
 * date: 1991/06/03 10:22:14;
 * took out some old ifdefs, and
 * fixed "can't escape a slash w/ a backslash" bug in searching
 * 
 * revision 1.5
 * date: 1991/02/19 18:05:36;
 * took out extraneous check
 * 
 * revision 1.4
 * date: 1990/12/03 12:02:16;
 * change 'word-under-cursor' expansion char to ':'
 * 
 * revision 1.3
 * date: 1990/11/07 14:28:41;
 * added '+' expansion character, to expand to the path-style string under the
 * cursor
 * 
 * revision 1.2
 * date: 1990/10/03 16:00:52;
 * make backspace work for everyone
 * 
 * revision 1.1
 * date: 1990/09/21 10:25:28;
 * initial vile RCS revision
 */

#include	"estruct.h"
#include	"edef.h"

#define	SQUARE_LEFT	'['

#define	DEFAULT_REG	-1

#define RECORDED_ESC	-2

typedef	struct	_kstack	{
	struct	_kstack	*m_link;
	int	m_save;		/* old value of 'kbdmode'		*/
	int	m_indx;		/* index identifying this macro		*/
	int	m_rept;		/* the number of times to execute the macro */
	TBUFF  *m_kbdm;		/* the macro-text to execute		*/
	TBUFF  *m_dots;		/* workspace for "." command		*/
#ifdef GMDDOTMACRO
	TBUFF  *m_DOTS;		/* save-area for "." command		*/
	int	m_RPT0;		/* saves 'dotcmdcnt'			*/
	int	m_RPT1;		/* saves 'dotcmdrep'			*/
#endif
	} KSTACK;

/*--------------------------------------------------------------------------*/
static	TBUFF *	TempDot P(( int ));
static	void	record_dot_char P(( int ));
static	void	record_kbd_char P(( int ));
static	void	record_char P(( int ));
static	void	remove_backslashes P(( char * ));
static	int	countBackSlashes P(( char *, int ));
static	void	showChar P(( int ));
static	int	expandChar P(( char *, int, int *, int ));
static	int	eol_history P(( char *, int, int, int ));
#ifdef GMDDOTMACRO
static	void	dot_replays_macro P(( int ));
#endif

static	KSTACK *KbdStack;	/* keyboard/@-macros that are replaying */
static	TBUFF  *KbdMacro;	/* keyboard macro, recorded	*/
static	int	last_eolchar;	/* records last eolchar-match in 'kbd_string' */

/*--------------------------------------------------------------------------*/

/*
 * Returns a pointer to the buffer that we use for saving text to replay with
 * the "." command.
 */
static TBUFF *
TempDot(init)
int	init;
{
	static	TBUFF  *tmpcmd;		/* dot commands, 'til we're sure */

	if (kbdmode == PLAY) {
		if (init)
			(void)tb_init(&(KbdStack->m_dots), abortc);
		return KbdStack->m_dots;
	}
	if (init || (tmpcmd == 0))
		(void)tb_init(&tmpcmd, abortc);
	return tmpcmd;
}

/*
 * Dummy function to use when 'kbd_string()' does not handle automatic completion
 */
/*ARGSUSED*/
int
no_completion(c, buf, pos)
int	c;
char	*buf;
int	*pos;
{
	return FALSE;
}

/*
 * Ask a yes or no question in the message line. Return either TRUE, FALSE, or
 * ABORT. The ABORT status is returned if the user bumps out of the question
 * with an abortc. Used any time a confirmation is required.
 */

int
mlyesno(prompt)
char *prompt;
{
	char c; 		/* input character */

	for (;;) {
		mlprompt("%s [y/n]? ",prompt);
		c = tgetc(FALSE);	/* get the response */

		if (c == kcod2key(abortc))		/* Bail out! */
			return(ABORT);

		if (c=='y' || c=='Y')
			return(TRUE);

		if (c=='n' || c=='N')
			return(FALSE);
	}
}

/*
 * Ask a simple question in the message line. Return the single char response,
 *  if it was one of the valid responses.
 */

int
mlquickask(prompt,respchars,cp)
char *prompt;
char *respchars;
int *cp;
{

	for (;;) {
		mlprompt("%s ",prompt);
		*cp = tgetc(FALSE);	/* get the response */

		if (*cp == kcod2key(abortc))	/* Bail out! */
			return(ABORT);

		if (strchr(respchars,*cp))
			return TRUE;

		TTbeep();
	}
}

/*
 * Prompt for a named-buffer (i.e., "register")
 */
int
mlreply_reg(prompt, cbuf, retp, at_dft)
char	*prompt;
char	*cbuf;		/* 2-char buffer for register+eol */
int	*retp;		/* => the register-name */
int	at_dft;		/* default-value (e.g., for "@@" command) */
{
	register int status;
	register int c;

	if (clexec || isnamedcmd) {
		if ((status = mlreply(prompt, cbuf, 2)) != TRUE)
			return status;
		c = cbuf[0];
	} else {
		c = kbd_key();
	}

	if (c == '@' && at_dft != -1) {
		c = at_dft;
	} else if (reg2index(c) < 0) {
		TTbeep();
		mlforce("[Invalid register name]");
		return FALSE;
	}

	*retp = c;
	return TRUE;
}

/*
 * Prompt for a register-name and/or line-count (e.g., for the ":yank" and
 * ":put" commands).  The register-name, if given, is first.
 */
int
mlreply_reg_count(state, retp, next)
int	state;		/* negative=register, positive=count, zero=either */
int	*retp;		/* returns the register-index or line-count */
int	*next;		/* returns 0/1=register, 2=count */
{
	register int status;
	char	prompt[80];
	char	expect[80];
	char	buffer[10];
	int	length;

	*expect = EOS;
	if (state <= 0)
		(void)strcat(expect, " register");
	if (state == 0)
		(void)strcat(expect, " or");
	if (state >= 0) {
		(void)strcat(expect, " line-count");
		length = sizeof(buffer);
	} else
		length = 2;

	(void)lsprintf(prompt, "Specify%s: ", expect);
	*buffer = EOS;
	status = kbd_string(prompt, buffer, length, ' ', 0, no_completion);

	if (status == TRUE) {
		if (state <= 0
		 && isalpha(buffer[0])
		 && buffer[1] == EOS
		 && (*retp = reg2index(*buffer)) >= 0) {
			*next = isupper(*buffer) ? 1 : 0;
		} else if (state >= 0
		 && string_to_number(buffer, retp)
		 && *retp) {
			*next = 2;
		} else {
			mlforce("[Expected%s]", expect);
			kbd_alarm();
			status = ABORT;
		}
	}
	return status;
}

/*
 * Write a prompt into the message line, then read back a response. Keep
 * track of the physical position of the cursor. If we are in a keyboard
 * macro throw the prompt away, and return the remembered response. This
 * lets macros run at full speed. The reply is always terminated by a carriage
 * return. Handle erase, kill, and abort keys.
 */

int
mlreply(prompt, buf, bufn)
char *prompt;
char *buf;
int bufn;
{
	return kbd_string(prompt, buf, bufn, '\n', KBD_NORMAL, no_completion);
}

#ifdef NEEDED
/* as above, but don't expand special punctuation, like #, %, ~, etc. */
int
mlreply_no_exp(prompt, buf, bufn)
char *prompt;
char *buf;
int bufn;
{
	return kbd_string(prompt, buf, bufn, '\n', KBD_QUOTES, no_completion);
}
#endif

/* as above, but don't do anything to backslashes */
int
mlreply_no_bs(prompt, buf, bufn)
char *prompt;
char *buf;
int bufn;
{
	return kbd_string(prompt, buf, bufn, '\n', KBD_EXPAND, no_completion);
}

/* as above, but neither expand nor do anything to backslashes */
int
mlreply_no_opts(prompt, buf, bufn)
char *prompt;
char *buf;
int bufn;
{
	return kbd_string(prompt, buf, bufn, '\n', 0, no_completion);
}

/*	kcod2key:	translate 10-bit keycode to single key value */
/* probably defined as a macro in estruct.h */
#ifndef kcod2key
kcod2key(c)
int c;
{
	return c & (N_chars-1);
}
#endif


/* the numbered buffer names increment each time they are referenced */
void
incr_dot_kregnum()
{
	if (dotcmdmode == PLAY) {
		register int	c = tb_peek(dotcmd);
		if (isdigit(c) && c < '9')
			tb_stuff(dotcmd, ++c);
	}
}

void
tungetc(c)
int c;
{

	tungotc = c;
	if (dotcmdmode == RECORD) {
		tb_unput(TempDot(FALSE));
		if (kbdmode == RECORD)
			tb_unput(KbdMacro);
	} else if (dotcmdmode != PLAY && kbdmode == RECORD)
		tb_unput(KbdMacro);
}

/*
 * Record a character for "." commands
 */
static void
record_dot_char(c)
int c;
{
	if (dotcmdmode == RECORD) {
		TBUFF	*tmp = TempDot(FALSE);
		(void)tb_append(&tmp, c);
	}
}

/*
 * Record a character for kbd-macros
 */
static void
record_kbd_char(c)
int c;
{
	if (dotcmdmode != PLAY && kbdmode == RECORD)
		(void)tb_append(&KbdMacro, c);
}

/* if we should preserve this input, do so */
static void
record_char(c)
int c;
{
	if (c == ESC)
		c = RECORDED_ESC;

	record_dot_char(c);
	record_kbd_char(c);
}

/* get the next character of a replayed '.' or macro */
int
get_recorded_char(eatit)
int eatit;  /* consume the character? */
{
	register int	c = -1;
	register TBUFF	*buffer;

	if (dotcmdmode == PLAY) {

		if (interrupted) {
			dotcmdmode = STOP;
			c = kcod2key(abortc);
			return c;
		} else {

			if (!tb_more(buffer = dotcmd)) {
				if (!eatit) {
					if (dotcmdrep > 1)
						return tb_get(buffer, 0);
				} else { /* at the end of last repetition?  */
					if (--dotcmdrep < 1) {
						dotcmdmode = STOP;
						(void)dotcmdbegin();
						/* immediately start recording
						 * again, just in case.
						 */
					} else {
						/* reset the macro to the
						 * beginning for the next rep.
						 */
						tb_first(buffer);
					}
				}
			}

			/* if there is some left... */
			if (tb_more(buffer)) {
				if (eatit)
					c = tb_next(buffer);
				else
					c = tb_peek(buffer);
				return c;
			}
		}
	}

	if (kbdmode == PLAY) { /* if we are playing a keyboard macro back, */

		if (interrupted) {
			while (kbdmode == PLAY)
				finish_kbm();
			c = kcod2key(abortc);
		} else {

			if (!tb_more(buffer = KbdStack->m_kbdm)) {
				if (--(KbdStack->m_rept) >= 1)
					tb_first(buffer);
				else
					finish_kbm();
			}

			if (kbdmode == PLAY) {
				buffer = KbdStack->m_kbdm;
				if (eatit)
					record_dot_char(c = tb_next(buffer));
				else
					c = tb_peek(buffer);
			}
		}
	}

	return c;
}

/*	tgetc:	Get a key from the terminal driver, resolve any keyboard
		macro action					*/
int 
tgetc(quoted)
int quoted;
{
	register int c;	/* fetched character */

	if (tungotc >= 0) {
		c = tungotc;
		tungotc = -1;
		record_char(c);
	} else {
		if ((c = get_recorded_char(TRUE)) == -1) {
			/* fetch a character from the terminal driver */ 
			interrupted = FALSE;
			if (setjmp(read_jmp_buf)) {
				c = kcod2key(intrc);
			} else {
				doing_kbd_read = TRUE;
				do { /* if it's sysV style signals,
					 we want to try again, since this
					 must not have been SIGINT, but
					 was probably SIGWINCH */
					c = TTgetc();
				} while (c == -1);
			}
			doing_kbd_read = FALSE;
			if (!quoted && (c == kcod2key(intrc)))
				c = kcod2key(abortc);
			else
				record_char(c);
		}
	}

	if (!quoted && (c == RECORDED_ESC))
		c = ESC;

	/* and finally give the char back */
	return(lastkey = c);
}

/*	KBD_KEY:	Get one keystroke. The only prefix legal here
			is the SPEC prefix.  */
int
kbd_key()
{
	int    c;

#if OPT_XTERM && !X11
kbd_key_loop:
#endif
	/* get a keystroke */
	c = tgetc(FALSE);

#if ANSI_SPEC

	if ((UCHAR)c == (UCHAR)RECORDED_ESC) {
		/* if this is being replayed... */
		/* ...then only look for esc sequences if there's input left */
		if (get_recorded_char(FALSE) != -1)
			c = ESC;
		else
			return (last1key = ESC);
	}

	if (c == ESC) {
		int nextc;

		/* if there's no recorded input, and no user typeahead */
		if ((nextc = get_recorded_char(FALSE)) == -1 && !typahead()) {
			/* give it a little extra time... */
			catnap(global_g_val(GVAL_TIMEOUTVAL),TRUE);
		}

		/* and then, if there _was_ recorded input or new typahead... */
		if (nextc != -1 || typahead()) {
			c = tgetc(FALSE);
			if (c == SQUARE_LEFT || c == 'O') {
#if OPT_XTERM && !X11
				int	d = c;
#endif
				/* eat ansi sequences */
				c = tgetc(FALSE);
#if OPT_XTERM && !X11
				if (d == SQUARE_LEFT
				 && (d = xterm_button(c)) != FALSE) {
					if (insertmode || (d != TRUE))
						return abortc;
					goto kbd_key_loop;
				}
#endif
				if (abortc != ESC || !insertmode)
					return (last1key = SPEC | c);
				if (insertmode == REPLACECHAR) {
					/* eat the sequence, but return abort */
					return abortc;
				}
				return (lastkey = SPEC|c);
			} else {
				if (abortc != ESC)
					return (last1key = c);
				tungetc(c);
				return (last1key = ESC);
			}
		}
	}
#endif

#if	MSDOS | ST520
	if (c == 0) {			/* Apply SPEC prefix	*/
		c = tgetc(FALSE);
		return(last1key = SPEC | c);
	}
#endif

#if	AMIGA
	/* apply SPEC prefix */
	if ((unsigned)c == 155) {
		int	d;
		c = tgetc(FALSE);

		/* first try to see if it is a cursor key */
		if ((c >= 'A' && c <= 'D') || c == 'S' || c == 'T') {
			if (!insertmode)
				return(last1key = SPEC | c);
		}

		/* next, a 2 char sequence */
		d = tgetc(FALSE);
		if (d == '~') {
			if (!insertmode)
				return(last1key = SPEC | c);
		}

		/* decode a 3 char sequence */
		c = d + ' ';
		/* if a shifted function key, eat the tilde */
		if (d >= '0' && d <= '9')
			d = tgetc(FALSE);
		if (!insertmode)
			return(last1key = SPEC | c);
	}
#endif

#if  WANGPC
	if (c == 0x1F) {	/* Apply SPEC prefix	*/
		c = tgetc(FALSE);
		if (!insertmode)
			return(last1key = SPEC | c);
	}
#endif

	return (last1key = c);
}

/*	KBD_SEQ:	Get a command sequence (multiple keystrokes) from 
		the keyboard.
		Process all applicable prefix keys.
		Set lastcmd for commands which want stuttering.
*/
int
kbd_seq()
{
	int c;		/* fetched keystroke */

	/* get initial character */
	c = kbd_key();

	/* process CTLA prefix */
	if (c == cntl_a) {
		c = kbd_key();
		return (lastcmd = CTLA | c);
	}

	/* process CTLX prefix */
	if (c == cntl_x) {
		c = kbd_key();
		return (lastcmd = CTLX | c);
	}

	/* otherwise, just return it */
	return (lastcmd = c);
}


/* get a string consisting of inclchartype characters from the current
	position.  if inclchartype is 0, return everything to eol */
#if ANSI_PROTOS
int screen_string (char *buf, int bufn, CMASK inclchartype )
#else
int
screen_string(buf,bufn,inclchartype)
char *buf;
int bufn;
CMASK inclchartype;
#endif
{
	register int i = 0;
	MARK mk;

	mk = DOT;
	while ( i < bufn && !is_at_end_of_line(DOT)) {
		buf[i] = char_at(DOT);
#if !SMALLER
		if (i == 0) {
			if (inclchartype & _scrtch) {
				if (buf[0] != SCRTCH_LEFT[0])
					inclchartype &= ~_scrtch;
			}
			if (inclchartype & _shpipe) {
				if (buf[0] != SHPIPE_LEFT[0])
					inclchartype &= ~_shpipe;
			}
		}
		/* guard against things like "[Buffer List]" on VMS */
		if (inclchartype & _pathn) {
			if (!ispath(buf[i]) && (inclchartype == _pathn))
				break;
		}
#endif
		if (inclchartype && !istype(inclchartype, buf[i]))
			break;
		DOT.o++;
		i++;
#if !SMALLER
		if (inclchartype & _scrtch) {
			if ((i < bufn)
			 && (inclchartype & _pathn)
			 && ispath(char_at(DOT)))
				continue;
			if (buf[i-1] == SCRTCH_RIGHT[0])
				break;
		}
#endif
	}

#if !SMALLER
#if VMS
	if (inclchartype & _pathn) {
		;	/* override conflict with "[]" */
	} else
#endif
	if (inclchartype & _scrtch) {
		if (buf[i-1] != SCRTCH_RIGHT[0])
			i = 0;
	}
#endif

	buf[bufn-1] = EOS;
	if (i < bufn)
		buf[i] = EOS;
	DOT = mk;

	return buf[0] != EOS;
}

/*
 * Returns the character that ended the last call on 'kbd_string()'
 */
int
end_string()
{
	return last_eolchar;
}

/*
 * Returns an appropriate delimiter for /-commands, based on the end of the
 * last reply.  That is, in a command such as
 *
 *	:s/first/last/
 *
 * we will get prompts for
 *
 *	:s/	/-delimiter saved in 'end_string()'
 *	first/
 *	last/
 *
 * If a newline is used at any stage, subsequent delimiters are forced to a
 * newline.
 */
int
kbd_delimiter()
{
	register int	c = '\n';

	if (namedcmd) {
		register int	d = end_string();
		if (ispunct(d))
			c = d;
	}
	return c;
}

/* turn \X into X */
static void
remove_backslashes(s)
char *s;
{
	register char *cp;
	while (*s) {
		if (*s == '\\') {  /* shift left */
			for (cp = s; *cp; cp++)
				*cp = *(cp+1);
		}
		s++;
	}
}

/* count backslashes so we can tell at any point whether we have the current
 * position escaped by one.
 */
static int
countBackSlashes(buffer, len)
char *	buffer;
int	len;
{
	register int	count;

	if (len && buffer[len-1] == '\\') {
		count = 1;
		while (count+1 <= len &&
			buffer[len-1-count] == '\\')
			count++;
	} else {
		count = 0;
	}
	return count;
}

static void
showChar(c)
int	c;
{
	if (disinp) {
		int	save_expand = kbd_expand;
		kbd_expand = 1;	/* show all controls */
		kbd_putc(c);
		kbd_expand = save_expand;
	}
}

/* expand a single character (only used on interactive input) */
static int
expandChar(buf, bufn, position, c)
char *	buf;
int	bufn;
int *	position;
int	c;
{
	register int	cpos = *position;
	register char *	cp;
	register BUFFER *bp;
	char str[NFILEN];

	/* Is this a character that we know about? */
	if (strchr(global_g_val_ptr(GVAL_EXPAND_CHARS),c) == 0)
		return FALSE;

	if (c == '%' || c == '#') {
		bp = (c == '%') ? curbp : find_alt();
		if (bp == 0 || b_is_invisible(bp)) {
			kbd_alarm();	/* complain a little */
			return FALSE;	/* ...and let the user type it as-is */
		}
		cp = bp->b_fname;
		if (isInternalName(cp))
			cp = get_bname(bp);
		else if (!global_g_val(GMDEXPAND_PATH))
			cp = shorten_path(strcpy(str, cp), FALSE);
	} else if (c == ':') {
		if (screen_string(str, sizeof(str), (CMASK)_pathn))
			cp = str;
		else
			cp = NULL;
	} else {
		return FALSE;
	}

	if (cp != NULL) {
		while (cpos < bufn-1 && ((c = *cp++) != EOS)) {
			buf[cpos++] = c;
			showChar(c);
		}
		buf[cpos] = EOS;
		TTflush();
	}
	*position = cpos;
	return TRUE;
}

/*
 * Returns true for the (presumably control-chars) that we use for line edits
 */
int
is_edit_char(c)
int c;
{
	return (isreturn(c)
	  ||	isbackspace(c)
	  ||	(c == wkillc)
	  ||	(c == killc));
}

/*
 * Erases the response from the screen for 'kbd_string()'
 */
void
kbd_kill_response(buf, position, c)
char *	buf;
int *	position;
int	c;
{
	register int	cpos = *position;

	while (cpos > 0) {
		cpos--;
		if (disinp) {
			kbd_erase();
			if (!isprint(buf[cpos]))
				kbd_erase();
		}
		if (c == wkillc) {
			if (!isspace(buf[cpos])) {
				if (cpos > 0 && isspace(buf[cpos-1]))
					break;
			}
		}

		if (c != killc && c != wkillc)
			break;
	}
	if (disinp)
		TTflush();

	buf[*position = cpos] = EOS;
}

/*
 * Display the default response for 'kbd_string()', escaping backslashes if
 * necessary.
 *
 * patch: make 'dst[]' a TBUFF so we don't have to worry about overflow.
 */
int
kbd_show_response(dst, src, bufn, eolchar, options)
char	*dst;		/* string with escapes */
char	*src;		/* string w/o escapes */
int	bufn;		/* maximum # of chars we read from 'src[]' */
int	eolchar;
int	options;
{
	register int c, j, k;

	j = k = 0;
	/* add backslash escapes in front of volatile characters */
	while ((c = src[k++]) != EOS && k < bufn) {
		if ((c == '\\') || (c == eolchar && eolchar != '\n')) {
			if (options & KBD_QUOTES)
				dst[j++] = '\\'; /* add extra */
		} else if (strchr(global_g_val_ptr(GVAL_EXPAND_CHARS),c) != 0) {
			if ((options & KBD_QUOTES)
			 && (options & KBD_EXPAND))
				dst[j++] = '\\'; /* add extra */
		}
		dst[j++] = c;
	}
	dst[j] = EOS;

	/* put out the default response, which is in the buffer */
	j = 0;
	kbd_init();
	while ((c = dst[j]) != EOS && j < NLINE-1) {
		showChar(c);
		++j;
	}
	if (disinp)
		TTflush();
	return j;
}

/* default function for 'edithistory()' */
static int
/*ARGSUSED*/
eol_history(buffer, cpos, c, eolchar)
char *	buffer;
int	cpos;
int	c;
int	eolchar;
{
	if (isprint(eolchar)) {
		if (c == eolchar)
			return TRUE;
	}
	return FALSE;
}

/*	A more generalized prompt/reply function allowing the caller
	to specify a terminator other than '\n'.  Both are accepted.
	Assumes the buffer already contains a valid (possibly
	null) string to use as the default response.
*/
int
kbd_string(prompt, extbuf, bufn, eolchar, options, complete)
char *prompt;		/* put this out first */
char *extbuf;		/* the caller's (possibly full) buffer */
int bufn;		/* the length of  " */
int eolchar;		/* char we can terminate on, in addition to '\n' */
int options;		/* KBD_EXPAND/KBD_QUOTES, etc. */
int (*complete)P((int,char *,int *));/* handles completion */
{
	return kbd_reply(prompt, extbuf, bufn, eol_history, eolchar, options, complete);
}

/*
 * Same as 'kbd_string()', except for adding the 'endfunc' parameter.
 *
 * Returns:
 *	ABORT - abort character given (usually ESC)
 *	SORTOFTRUE - backspace from empty-buffer
 *	TRUE - buffer is not empty
 *	FALSE - buffer is empty
 */
int
kbd_reply(prompt, extbuf, bufn, endfunc, eolchar, options, complete)
char *prompt;		/* put this out first */
char *extbuf;		/* the caller's (possibly full) buffer */
int bufn;		/* the length of  " */
int (*endfunc)P((char *,int,int,int));	/* parsing with 'eolchar' delimiter */
int eolchar;		/* char we can terminate on, in addition to '\n' */
int options;		/* KBD_EXPAND/KBD_QUOTES */
int (*complete)P((int,char *,int *));	/* handles completion */
{
	int	c;
	int	done;
	int	cpos;		/* current character position in string */
	int	expanded;
	int	status;

	register int quotef;	/* are we quoting the next char? */
	register int backslashes; /* are we quoting the next expandable char? */
	int firstch = TRUE;
	int newpos;
	char buf[NLINE];

	last_eolchar = EOS;	/* ...in case we don't set it elsewhere */

	if (clexec) {
		int	s;
		execstr = token(execstr, extbuf, eolchar);
		if ((s = (*extbuf != EOS)) != FALSE) {
#if !SMALLER
			if ((options & KBD_LOWERC))
				(void)mklower(extbuf);
			else if ((options & KBD_UPPERC))
				(void)mkupper(extbuf);
#endif
			if (!(options & KBD_NOEVAL)) {
				(void)strncpy(extbuf, tokval(extbuf), (SIZE_T)bufn);
			}
			if (complete != no_completion) {
				cpos =
				newpos = strlen(extbuf);
				if (!(*complete)(NAMEC, extbuf, &newpos))
					extbuf[cpos] = EOS;
			}
			last_eolchar = *execstr;
			extbuf[bufn-1] = EOS;
		}
		return s;
	}

	quotef = FALSE;
	reading_msg_line = TRUE;

	/* prompt the user for the input string */
	if (prompt != 0)
		mlprompt("%s", prompt);

	if (bufn > sizeof(buf)) bufn = sizeof(buf);
	cpos = kbd_show_response(buf, extbuf, bufn, eolchar, options);
	backslashes = 0; /* by definition, there is an even 
					number of backslashes */
	for (;;) {
		int	EscOrQuo = ((quotef == TRUE) || ((backslashes & 1) != 0));

		/* get a character from the user */
		c = quotef ? tgetc(TRUE) : kbd_key();

		/* If it is a <ret>, change it to a <NL> */
		if (c == '\r' && quotef == FALSE)
			c = '\n';

		/*
		 * If they hit the line terminate (i.e., newline or unescaped
		 * eolchar), wrap it up.
		 *
		 * Don't allow newlines in the string -- they cause real
		 * problems, especially when searching for patterns
		 * containing them -pgf
		 */
		done = FALSE;
		if (c == '\n') {
			done = TRUE;
		} else if (!EscOrQuo && !is_edit_char(c)) {
			if ((*endfunc)(buf,cpos,c,eolchar)) {
				done = TRUE;
			}
		}

		if (complete != no_completion) {
			if (c == EOS) {	/* conflicts with null-terminated strings */
				kbd_alarm();
				continue;
			}
			kbd_unquery();
			if (done && (options & KBD_NULLOK) && cpos == 0)
				;
			else if ((done && !(options & KBD_MAYBEC))
			 || (!EscOrQuo
			  && !((options & KBD_EXPCMD) && isShellOrPipe(buf))
			  && (c == TESTC || c == NAMEC))) {
				int	ok = ((*complete)(c, buf, &cpos));

				if (ok) {
					done = TRUE;
					if (c != NAMEC) /* cancel the unget */
						(void)tgetc(FALSE);
				} else {
					if (done) {	/* stay til matched! */
						buf[cpos] = EOS;
						kbd_unquery();
						(void)((*complete)(TESTC, buf, &cpos));
					}
					continue;
				}
			}
		}

		if (done) {
			last_eolchar = c;
			if (options & KBD_QUOTES)
				remove_backslashes(buf); /* take out quoters */

			/* if buffer is empty, return FALSE */
			hst_append(buf, eolchar);
			status = (*strncpy(extbuf, buf, (SIZE_T)bufn) != EOS);
			break;
		}


#if	OPT_HISTORY
		if (!EscOrQuo
		 && edithistory(buf, &cpos, &c, options, endfunc, eolchar)) {
			backslashes = countBackSlashes(buf, cpos);
			firstch = TRUE;
			continue;
		} else
#endif
		if (c == abortc && quotef == FALSE) {
			buf[cpos] = EOS;
			status = esc(FALSE, 1);
			break;
		} else if ((isbackspace(c) ||
			c == wkillc ||
			c == killc) && quotef==FALSE) {

			if (prompt == 0 && c == killc)
				cpos = 0;

			if (cpos == 0) {
				buf[0] = EOS;
				if (prompt)
					mlerase();
				if (isbackspace(c)) {	/* splice calls */
					tungetc(c);
					status = SORTOFTRUE;
					break;
				}
				status = FALSE;
				break;
			}

		killit:
			kbd_kill_response(buf, &cpos, c);
			backslashes = countBackSlashes(buf, cpos);

		} else if (c == quotec && quotef == FALSE) {
			quotef = TRUE;
			continue;	/* keep firstch==TRUE */

		} else {
			if (firstch == TRUE) {
				/* clean the buffer on the first char typed */
				tungetc(c);
				c = killc;
				goto killit;
			}

			expanded = FALSE;
			if (!EscOrQuo) {
				if ((options & KBD_EXPAND)
#if OPT_HISTORY
				 || ((options & KBD_EXPCMD) && isShellOrPipe(buf))
#endif
				   )
					expanded = expandChar(buf, bufn, &cpos, c);
			}

			if (!expanded) {
				if (c == '\\')
					backslashes++;
				else
					backslashes = 0;
				quotef = FALSE;

				if (isspecial(c)
				 || (cpos >= bufn-1)) {
					if (!typahead())
						kbd_alarm();
					continue; /* keep firstch==TRUE */
				} else {
#if !SMALLER
					if ((options & KBD_LOWERC)
					 && isupper(c))
						c = tolower(c);
					else if ((options & KBD_UPPERC)
					 && islower(c))
						c = toupper(c);
#endif
					buf[cpos++] = c;
					buf[cpos] = EOS;
					if (disinp) {
						showChar(c);
						TTflush();
					}
				}
			}
		}
		firstch = FALSE;
	}
	reading_msg_line = FALSE;
	return status;
}

/* ARGSUSED */
int
speckey(f,n)
int f,n;
{

	tungetc( SPEC | kbd_key() );

	return TRUE;
}

/*
 * Make the "." replay the keyboard macro
 */
#ifdef GMDDOTMACRO
static void
dot_replays_macro(macnum)
int	macnum;
{
	extern	CMDFUNC	f_kbd_mac_exec;
	char	temp[NSTRING];
	TBUFF	*tmp;
	int	c;

	if (macnum == DEFAULT_REG) {
		if ((c = fnc2kcod(&f_kbd_mac_exec)) == -1)
			return;
		(void)kcod2str(c, temp);
	} else {
		(void)lsprintf(temp, "@%c", index2reg(macnum));
	}
	dotcmdbegin();
	tmp = TempDot(FALSE);
	(void)tb_sappend(&tmp, temp);
	dotcmdfinish();
	dotcmdbegin();
}
#endif

/*
 * Begin recording the dot command macro.
 * Set up variables and return.
 * we use a temporary accumulator, in case this gets stopped prematurely
 */
int
dotcmdbegin()
{
	/* never record a playback */
	if (dotcmdmode != PLAY) {
		(void)TempDot(TRUE);
		dotcmdmode = RECORD;
		return TRUE;
	}
	return FALSE;
}

/*
 * Finish dot command, and copy it to the real holding area
 */
int
dotcmdfinish()
{
	if (dotcmdmode == RECORD) {
		TBUFF	*tmp = TempDot(FALSE);
		if (tb_length(tmp) == 0	/* keep the old value */
		 || tb_copy(&dotcmd, tmp) != 0) {
			tb_first(dotcmd);
			dotcmdmode = STOP;
			return TRUE;
		}
	}
	return FALSE;
}


/* stop recording a dot command, 
	probably because the command is not re-doable */ 
void
dotcmdstop()
{
	if (dotcmdmode == RECORD)
		dotcmdmode = STOP;
}

/*
 * Execute a the '.' command, by putting us in PLAY mode.
 * The command argument is the number of times to loop. Quit as soon as a
 * command gets an error. Return TRUE if all ok, else FALSE.
 */
int
dotcmdplay(f, n)
int f,n;
{
	if (!f)
		n = dotcmdarg ? dotcmdcnt:1;
	else if (n < 0)
		return TRUE;

	if (f)	/* we definitely have an argument */
		dotcmdarg = TRUE;
	/* else
		leave dotcmdarg alone; */

	if (dotcmdmode != STOP || tb_length(dotcmd) == 0) {
		dotcmdmode = STOP;
		dotcmdarg = FALSE;
		return FALSE;
	}

	if (n == 0) n = 1;

	dotcmdcnt = dotcmdrep = n;  /* remember how many times to execute */
	dotcmdmode = PLAY;	/* put us in play mode */
	tb_first(dotcmd);	/*    at the beginning */

	if (ukb != 0) /* save our kreg, if one was specified */
		dotcmdkreg = ukb;
	else /* use our old one, if it wasn't */
		ukb = dotcmdkreg;

	return TRUE;
}

/*
 * Test if we are replaying either '.' command, or keyboard macro.
 */
int
kbd_replaying(match)
int	match;
{
	if (dotcmdmode == PLAY) {
		/*
		 * Force a false-return if we are in insert-mode and have
		 * only one character to display.
		 */
		if (match
		 && insertmode == INSERT
		 && b_val(curbp, MDSHOWMAT)
		 && KbdStack == 0
		 && (dotcmd->tb_last+1 >= dotcmd->tb_used)) {
			return FALSE;
		}
		return TRUE;
	}
	return (kbdmode == PLAY);
}

/*
 * Begin recording a keyboard macro.
 */
/* ARGSUSED */
int
kbd_mac_begin(f, n)
int f,n;
{
	if (kbdmode != STOP) {
		mlforce("[Macro already active]");
		return FALSE;
	}
	mlwrite("[Start macro]");

	kbdmode = RECORD;
	return (tb_init(&KbdMacro, abortc) != 0);
}

/*
 * End keyboard macro. Check for the same limit conditions as the above
 * routine. Set up the variables and return to the caller.
 */
/* ARGSUSED */
int
kbd_mac_end(f, n)
int f,n;
{
	if (kbdmode == STOP) {
		mlforce("[Macro not active]");
		return FALSE;
	}
	if (kbdmode == RECORD) {
		mlwrite("[End macro]");
		kbdmode = STOP;
#ifdef GMDDOTMACRO
		dot_replays_macro(DEFAULT_REG);
#endif
	}
	/* note that if kbd_mode == PLAY, we do nothing -- that makes
		the '^X-)' at the of the recorded buffer a no-op during
		playback */
	return TRUE;
}

/*
 * Execute a macro.
 * The command argument is the number of times to loop. Quit as soon as a
 * command gets an error. Return TRUE if all ok, else FALSE.
 */
/* ARGSUSED */
int
kbd_mac_exec(f, n)
int f,n;
{
	if (n <= 0)
		return TRUE;

	return start_kbm(n, DEFAULT_REG, KbdMacro);
}

/* ARGSUSED */
int
kbd_mac_save(f,n)
int f,n;
{
	ksetup();
	tb_first(KbdMacro);
	while (tb_more(KbdMacro))
		if (!kinsert(tb_next(KbdMacro)))
			break;
	kdone();
	mlwrite("[Keyboard macro saved in register %c.]", index2reg(ukb));
	return TRUE;
}

/*
 * Test if the given macro has already been started.
 */
int
kbm_started(macnum, force)
int	macnum;
int	force;
{
	if (force || (kbdmode == PLAY)) {
		register KSTACK *sp;
		for (sp = KbdStack; sp != 0; sp = sp->m_link) {
			if (sp->m_indx == macnum) {
				TTbeep();
				while (kbdmode == PLAY)
					finish_kbm();
				mlforce("[Error: currently executing %s%c]",
					macnum == DEFAULT_REG
						? "macro" : "register ",
					index2reg(macnum));
				return TRUE;
			}
		}
	}
	return FALSE;
}

/*
 * Start playback of the given keyboard command-string
 */
int
start_kbm(n, macnum, ptr)
int	n;			/* # of times to repeat */
int	macnum;			/* register to execute */
TBUFF *	ptr;			/* data to interpret */
{
	register KSTACK *sp;
	TBUFF  *tp = 0;

	if (interrupted)
		return FALSE;

	if (kbdmode == RECORD && KbdStack != 0)
		return TRUE;

	if (tb_length(ptr)
	 && (sp = typealloc(KSTACK)) != 0
	 && tb_copy(&tp, ptr) != 0) {

		/* make a copy of the macro in case recursion alters it */
		tb_first(tp);

		sp->m_save = kbdmode;
		sp->m_indx = macnum;
		sp->m_rept = n;
		sp->m_kbdm = tp;
		sp->m_link = KbdStack;

		KbdStack   = sp;
		kbdmode    = PLAY; 	/* start us in play mode */

		/* save data for "." on the same stack */
		sp->m_dots = 0;
		if (dotcmdmode == PLAY) {
#ifdef GMDDOTMACRO
			sp->m_DOTS = dotcmd;
			sp->m_RPT0 = dotcmdcnt;
			sp->m_RPT1 = dotcmdrep;
#endif
			dotcmd     = 0;
			dotcmdmode = RECORD;
		}
#ifdef GMDDOTMACRO
		  else {
			sp->m_DOTS = 0;
		  }
#endif
		return (tb_init(&dotcmd, abortc) != 0
		  &&    tb_init(&(sp->m_dots), abortc) != 0);
	}
	return FALSE;
}

/*
 * Finish a macro begun via 'start_kbm()'
 */
void
finish_kbm()
{
	if (kbdmode == PLAY) {
		register KSTACK *sp = KbdStack;

		kbdmode = STOP;
		if (sp != 0) {
			kbdmode  = sp->m_save;
			KbdStack = sp->m_link;

			tb_free(&(sp->m_kbdm));
			tb_free(&(sp->m_dots));
#ifdef GMDDOTMACRO
			tb_free(&dotcmd);
			if (sp->m_DOTS != 0) {
				dotcmd     = sp->m_DOTS;
				dotcmdcnt  = sp->m_RPT0;
				dotcmdrep  = sp->m_RPT1;
				dotcmdmode = PLAY;
			}
			dot_replays_macro(sp->m_indx);
#endif
			free((char *)sp);
		}
	}
}
