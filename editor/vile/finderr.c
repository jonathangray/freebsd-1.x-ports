/* Find the next error in mentioned in the shell output window.
 * Written for vile by Paul Fox, (c)1990
 *
 * $Log: finderr.c,v $
 * Revision 1.1  1994/02/01 03:29:23  jkh
 * Initial revision
 *
 * Revision 1.28  1993/09/10  16:06:49  pgf
 * tom's 3.61 changes
 *
 * Revision 1.27  1993/09/06  16:26:34  pgf
 * added DEC OSF/1 error message formats
 *
 * Revision 1.26  1993/09/03  16:17:24  pgf
 * added in initialization of name[] in finderrbug()
 *
 * Revision 1.25  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.24  1993/06/02  14:28:47  pgf
 * see tom's 3.48 CHANGES
 *
 * Revision 1.23  1993/05/24  15:21:37  pgf
 * tom's 3.47 changes, part a
 *
 * Revision 1.22  1993/05/11  16:22:22  pgf
 * see tom's CHANGES, 3.46
 *
 * Revision 1.21  1993/04/28  14:34:11  pgf
 * see CHANGES, 3.44 (tom)
 *
 * Revision 1.20  1993/04/28  09:41:21  pgf
 * finderrbuff now remembers last arg
 *
 * Revision 1.19  1993/04/20  12:18:32  pgf
 * see tom's 3.43 CHANGES
 *
 * Revision 1.18  1993/04/13  18:49:58  pgf
 * fixed leak in prev. fix
 *
 * Revision 1.17  1993/04/12  19:23:52  pgf
 * fixed bug in pathname handling when doing Entering/Leaving pairs, and
 * added finderrbuf() command
 *
 * Revision 1.16  1993/04/01  13:07:50  pgf
 * see tom's 3.40 CHANGES
 *
 * Revision 1.15  1993/03/05  17:50:54  pgf
 * see CHANGES, 3.35 section
 *
 * Revision 1.14  1993/01/16  10:34:20  foxharp
 * use for_each_window macro
 *
 * Revision 1.13  1992/12/04  09:12:25  foxharp
 * deleted unused assigns
 *
 * Revision 1.12  1992/08/19  22:57:45  foxharp
 * no longer need to multiply NFILEN -- it's bigger
 *
 * Revision 1.11  1992/07/16  22:06:58  foxharp
 * honor the "Entering/Leaving directory" messages that GNU make puts out
 *
 * Revision 1.10  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.9  1992/03/19  23:35:27  pgf
 * use b_linecount to index from end of file instead of top, to be
 * less affected by insertions deletions up above
 *
 * Revision 1.8  1992/03/05  09:01:12  pgf
 * shortened "found error" msg, and force it
 *
 * Revision 1.7  1992/01/05  00:06:13  pgf
 * split mlwrite into mlwrite/mlprompt/mlforce to make errors visible more
 * often.  also normalized message appearance somewhat.
 *
 * Revision 1.6  1991/11/01  14:38:00  pgf
 * saber cleanup
 *
 * Revision 1.5  1991/09/20  13:11:53  pgf
 * protect against passing null l_text pointer to sscanf
 *
 * Revision 1.4  1991/08/07  12:35:07  pgf
 * added RCS log messages
 *
 * revision 1.3
 * date: 1991/08/06 15:21:44;
 * sprintf changes
 *
 * revision 1.2
 * date: 1991/06/25 19:52:42;
 * massive data structure restructure
 *
 * revision 1.1
 * date: 1990/09/21 10:25:19;
 * initial vile RCS revision
 */

#include "estruct.h"
#include "edef.h"

#if FINDERR

static	char febuff[NBUFN+1];	/* name of buffer to find errors in */
static	unsigned newfebuff;	/* is the name new since last time? */

void
set_febuff(name)
char	*name;
{
	(void)strncpy(febuff, name, NBUFN);
	newfebuff = TRUE;
}

/* edits the file and goes to the line pointed at by the next compiler
        error in the "[output]" window.  It unfortunately doesn't mark
        the lines for you, so adding lines to the file throws off the
        later numbering.  Solutions to this seem messy at the moment */

/* ARGSUSED */
int
finderr(f,n)
int f,n;
{
	register BUFFER *sbp;
	register int s;
	struct LINE *dotp;
	int moveddot = FALSE;
	char	*text;

	int errline;
	char errfile[NFILEN];
	char ferrfile[NFILEN];
	char verb[32];

	static int oerrline = -1;
	static char oerrfile[256];

#if APOLLO
	static	char	lint_fmt1[] = "%32[^( \t] \t%[^(](%d)";
	static	char	lint_fmt2[] = "%32[^(]( arg %d ) \t%32[^( \t](%d) :: %[^(](%d))";
	int	num1, num2;
	char	nofile[NFILEN];
#endif

#define DIRLEVELS 20
	static int l = 0;
	static char *dirs[DIRLEVELS];

	/* look up the right buffer */
	if ((sbp = find_b_name(febuff)) == NULL) {
		mlforce("[No buffer to search for errors.]");
		return(FALSE);
	}
 
	if (newfebuff) {
		oerrline = -1;
		oerrfile[0] = EOS;
		while (l)
			free(dirs[l--]);
	}
	newfebuff = FALSE;

	dotp = getdot(sbp);

	for(;;) {
		/* to use this line, we need both the filename and the line
			number in the expected places, and a different line
			than last time */
		/* accept lines of the form:
			"file.c", line 223: error....
			or
			file.c: 223: error....
			or
			******** Line 187 of "filec.c"
		*/
		if (lisreal(dotp)) {
			static	TBUFF	*tmp;
#if SUNOS
			char *t;
#endif

			if (tb_init(&tmp, EOS) != 0
			 && tb_bappend(&tmp, dotp->l_text,
			 	(ALLOC_T)llength(dotp)) != 0
			 && tb_append(&tmp, EOS) != 0)
			 	text = tb_values(tmp);
			else
				break;	/* out of memory */

			if ( (sscanf(text,
				"\"%[^\" \t]\", line %d:",
				errfile, &errline) == 2
#if APOLLO 	/* C compiler */
			  ||  sscanf(text,
				"%32[*] Line %d of \"%[^\" \t]\"",
				verb, &errline, errfile) == 3
				/* sys5 lint */
			  ||  (!strncmp(text, "    ", 4)
			    && (sscanf(text+4, lint_fmt1,
			    	verb, errfile, &errline) == 3)
			     || sscanf(text+4, lint_fmt2,
			     	verb, &num1, nofile, &num2,
						errfile, &errline) == 6)
			  	/* C++ compiler */
			  ||  sscanf(text,
			  	"CC: \"%[^\"]\", line %d",
						errfile, &errline) == 2
#endif
#if OSF1
/* 	/usr/lib/cmplrs/cc/cfe: Error: test.c, line 8: Syntax Error  */
			  ||  sscanf(text,
			  	"%*s %*s %[^, \t], line %d",
						errfile, &errline) == 2
#endif
			  ||  sscanf(text,
				"%[^: \t]: %d:",
				errfile, &errline) == 2
#if SUNOS	/* lint-output */
			  ||  sscanf(text,
			  	"%[^:( \t](%d):",
				errfile, &errline) == 2
			  ||  ((t = strstr(text, "  ::  ")) != 0
			    && sscanf(t,
			  	"  ::  %[^( \t](%d)",
				errfile, &errline) == 2)
#endif
				) && (oerrline != errline ||
					strcmp(errfile,oerrfile))) {
				break;
			} else if (sscanf(text,
			"%*[^ \t:`]: %32[^ \t`] directory `", verb) == 1 ) {
				if (!strcmp(verb, "Entering")) {
					if (l < DIRLEVELS) {
						char *e,*d;
						/* find the start of path */
						d = strchr(text, '`');
						if (!d)
							continue;
						d += 1;
						/* find the term. quote */
						e = strchr(d, '\'');
						if (!e)
							continue;
						/* put a null in its place */
						*e = EOS;
						/* make a copy */
						dirs[++l] = strmalloc(d);
						/* put the quote back */
						*e = '\'';
					}
				} else if (!strcmp(verb, "Leaving")) {
					if (l > 0)
						free(dirs[l--]);
				}
			}
		}

		if (lforw(dotp) == l_ref(sbp->b_line.l)) {
			mlforce("[No more errors in %s buffer]", febuff);
			TTbeep();
			/* start over at the top of file */
			putdotback(sbp, lForw(sbp->b_line.l));
			while (l)
				free(dirs[l--]);
			return FALSE;
		}
		dotp = lforw(dotp);
		moveddot = TRUE;
	}
	/* put the new dot back, before possible changes to contents
				of current window from getfile() */
	if (moveddot)
		putdotback(sbp,dotp);

	(void)pathcat(ferrfile, dirs[l], errfile);

	if (!eql_bname(curbp, ferrfile) &&
		strcmp(ferrfile,curbp->b_fname)) {
		/* if we must change windows */
		struct WINDOW *wp;
		for_each_window(wp) {
			if (eql_bname(wp->w_bufp, ferrfile)
			 || !strcmp(wp->w_bufp->b_fname,ferrfile))
				break;
		}
		if (wp) {
			curwp = wp;
			make_current(curwp->w_bufp);
			upmode();
		} else {
			s = getfile(ferrfile,TRUE);
			if (s != TRUE)
				return s;
		}
	}

	mlforce("Error: %*S", dotp->l_used, dotp->l_text);

	/* it's an absolute move */
	curwp->w_lastdot = curwp->w_dot;
	s = gotoline(TRUE, -(curbp->b_linecount - errline + 1));

	oerrline = errline;
	(void)strcpy(oerrfile,errfile);

	return s;
}

struct LINE *
getdot(bp)
struct BUFFER *bp;
{
	register WINDOW *wp;
	if (bp->b_nwnd) {
		/* scan for windows holding that buffer,
					pull dot from the first */
		for_each_window(wp) {
			if (wp->w_bufp == bp) {
				return l_ref(wp->w_dot.l);
			}
		}
	}
	return l_ref(bp->b_dot.l);
}

void
putdotback(bp,dotp)
struct BUFFER *bp;
struct LINE *dotp;
{
	register WINDOW *wp;

	if (bp->b_nwnd) {
		for_each_window(wp) {
			if (wp->w_bufp == bp) {
				wp->w_dot.l = l_ptr(dotp);
				wp->w_dot.o = 0;
				wp->w_flag |= WFMOVE;
			}
		}
		return;
	}
	/* then the buffer isn't displayed */
	bp->b_dot.l = l_ptr(dotp);
	bp->b_dot.o = 0;
}

/*
 * Ask for a new finderr buffer name
 */
/* ARGSUSED */
int
finderrbuf(f, n)
int f,n;
{
	register int    s;
	char name[NBUFN+1];

	(void)strcpy(name, febuff);
	if ((s = mlreply("Buffer to scan for \"errors\": ", name, sizeof(name))) == ABORT)
		return s;
	set_febuff((s == FALSE) ? ScratchName(Output) : name);
	return TRUE;
}
#endif
