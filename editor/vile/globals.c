/* ed/vi/ex style global commands, where first the file is scanned for
 *	matching lines, then for each such line, an action is performed.
 *	written for vile by Paul Fox, (c)1990
 *
 * $Log: globals.c,v $
 * Revision 1.1  1994/02/01 03:29:24  jkh
 * Initial revision
 *
 * Revision 1.23  1993/09/16  10:57:54  pgf
 * used set_curwp() instead of swbuffer() to restore our window after
 * globber executes a command on the current line
 *
 * Revision 1.22  1993/07/27  18:06:20  pgf
 * see tom's 3.56 CHANGES entry
 *
 * Revision 1.21  1993/07/01  16:15:54  pgf
 * tom's 3.51 changes
 *
 * Revision 1.20  1993/05/24  15:21:37  pgf
 * tom's 3.47 changes, part a
 *
 * Revision 1.19  1993/05/04  17:05:14  pgf
 * see tom's CHANGES, 3.45
 *
 * Revision 1.18  1993/04/01  12:53:33  pgf
 * removed redundant includes and declarations
 *
 * Revision 1.17  1993/03/18  17:42:20  pgf
 * see 3.38 section of CHANGES
 *
 * Revision 1.16  1993/03/16  10:53:21  pgf
 * see 3.36 section of CHANGES file
 *
 * Revision 1.15  1993/02/24  10:59:02  pgf
 * see 3.34 changes, in CHANGES file
 *
 * Revision 1.14  1992/12/20  14:39:48  foxharp
 * implemented 'v' command -- easy -- just do same as 'g', but invert marks
 * before runnning the command
 *
 * Revision 1.13  1992/12/04  09:12:25  foxharp
 * deleted unused assigns
 *
 * Revision 1.12  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.11  1992/03/07  10:21:29  pgf
 * arg mismatch on fsearch()
 *
 * Revision 1.10  1992/01/05  00:06:13  pgf
 * split mlwrite into mlwrite/mlprompt/mlforce to make errors visible more
 * often.  also normalized message appearance somewhat.
 *
 * Revision 1.9  1991/11/01  14:38:00  pgf
 * saber cleanup
 *
 * Revision 1.8  1991/09/10  00:52:55  pgf
 * be careful to not rely on curbp during global ops, since some commands, like
 * print (pregion) change buffers
 *
 * Revision 1.7  1991/08/07  12:35:07  pgf
 * added RCS log messages
 *
 * revision 1.6
 * date: 1991/08/06 15:22:03;
 *  global/local values
 * 
 * revision 1.5
 * date: 1991/06/27 18:43:40;
 * fixed infinite loop if a global referenced '^' or '$' as the search string
 * 
 * revision 1.4
 * date: 1991/06/25 19:52:44;
 * massive data structure restructure
 * 
 * revision 1.3
 * date: 1991/05/31 11:07:32;
 * clean up globals routine, so it doesn't need or provide extra args
 * 
 * revision 1.2
 * date: 1991/04/22 09:02:42;
 * removed non-portable initialization
 * 
 * revision 1.1
 * date: 1990/09/21 10:25:21;
 * initial vile RCS revision
 */

#include	"estruct.h"
#include        "edef.h"

#if GLOBALS

static	int	globber P(( int, int, int ));

int
globals(f,n)
int f,n;
{
	return globber(f,n,'g');
}

int
vglobals(f,n)
int f,n;
{
	return globber(f,n,'v');
}

/* ARGSUSED */
static int
globber(f, n, g_or_v)
int f, n, g_or_v;
{
	int c, s;
	register LINE *lp;
	register char *fnp;	/* ptr to the name of the cmd to exec */
	char	cmd[NLINE];
	CMDFUNC *cfp;
	int foundone;
	WINDOW *wp;
	L_NUM	before;
	int	save_report;

	extern CMDFUNC f_godotplus;
	
	c = kbd_delimiter();
	if (readpattern("global pattern: ", &pat[0], &gregexp, c, FALSE) != TRUE) {
		mlforce("[No pattern.]");
		return FALSE;
	}

	/* in some sense, it would be nice to search first, before
                making the user answer the next question, but the
                searching delay is too long, and unexpected in the
                middle of a command.  */

	fnp = kbd_engl("action to perform on each matching line: ", cmd);
	/* get the name of, and then the function to execute */
	if (!fnp) {
	        mlforce("[No function]");
		return FALSE;
	} else if (!(cfp = engl2fnc(fnp))) {
	        mlforce("[No such function]");
		return FALSE;
	} else if ((cfp->c_flags & GLOBOK) == 0) {
	        mlforce("[Function not allowed]");
		return FALSE;
	}
	
	
	/* call the searcher, telling it to do line marking */
	s = fsearch(FALSE,0,TRUE,FALSE);
	if (s != TRUE)
		return s;
	
	calledbefore = FALSE;
	
	if (g_or_v == 'v') {  /* invert the sense of all the matches */
		for_each_line(lp, curbp)
			lflipmark(lp);
	}
	/* loop through the buffer -- we must clear the marks no matter what */
	s = TRUE;
	lp = lForw(curbp->b_line.l);
	wp = curwp;
	/* loop until there are no marked lines in the buffer */
	foundone = FALSE;
	before = line_count(curbp);
	save_report = global_g_val(GVAL_REPORT);
	for(;;) {
		if (lp == l_ref(wp->w_bufp->b_line.l)) {
			/* at the end -- only quit if we found no 
				marks on the last pass through. otherwise,
				go through again */
			if (foundone)
				foundone = FALSE;
			else
				break;
			lsetnotmarked(lp); /* always unmark the header line */
		}
		if (lismarked(lp)) {
			foundone = TRUE;
			lsetnotmarked(lp);
			/* call the function, if there is one, and results
				have been ok so far */
			if (cfp && s) {
				if (!calledbefore && (cfp->c_flags & UNDO)) {
					if (b_val(wp->w_bufp,MDVIEW))
						return(rdonly());
					mayneedundo();
					set_global_g_val(GVAL_REPORT,0);
				}
				havemotion = &f_godotplus;
				wp->w_dot.l = l_ptr(lp);
				wp->w_dot.o = 0;
				s = (cfp->c_func)(FALSE, 1);
				/* function may have switched on us */
				set_curwp(wp);
				lp = l_ref(wp->w_dot.l);
				havemotion = NULL;
				calledbefore = TRUE;
			}
		}
		lp = lforw(lp);
	}
	set_global_g_val(GVAL_REPORT,save_report);
	line_report(before);

	return s;
}

#else
globalhello() { }
#endif
