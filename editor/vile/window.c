/*
 * Window management. Some of the functions are internal, and some are
 * attached to keys that the user actually types.
 *
 * $Log: window.c,v $
 * Revision 1.1  1994/02/01 03:29:43  jkh
 * Initial revision
 *
 * Revision 1.32  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.31  1993/08/13  16:32:50  pgf
 * tom's 3.58 changes
 *
 * Revision 1.30  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.29  1993/07/27  18:06:20  pgf
 * see tom's 3.56 CHANGES entry
 *
 * Revision 1.28  1993/07/01  16:15:54  pgf
 * tom's 3.51 changes
 *
 * Revision 1.27  1993/06/18  15:57:06  pgf
 * tom's 3.49 changes
 *
 * Revision 1.26  1993/06/02  14:28:47  pgf
 * see tom's 3.48 CHANGES
 *
 * Revision 1.25  1993/05/24  15:21:37  pgf
 * tom's 3.47 changes, part a
 *
 * Revision 1.24  1993/04/20  12:18:32  pgf
 * see tom's 3.43 CHANGES
 *
 * Revision 1.23  1993/04/20  12:00:59  pgf
 * scroll sideways by a half screen by default in mv{left,right}window()
 *
 * Revision 1.22  1993/04/01  12:57:22  pgf
 * removed redundant includes and declarations
 *
 * Revision 1.21  1993/02/24  10:59:02  pgf
 * see 3.34 changes, in CHANGES file
 *
 * Revision 1.20  1993/01/23  13:38:23  foxharp
 * new unlink_window() function
 *
 * Revision 1.19  1993/01/16  10:43:38  foxharp
 * use new macros, and call updatelistbuffers appropriately
 *
 * Revision 1.18  1992/12/14  09:36:50  foxharp
 * now that sideways scrolling works on short lines, don't need to restrict user to
 * certain cursor positions
 *
 * Revision 1.17  1992/12/14  08:32:20  foxharp
 * fix for the sideways-shifted-but-deextended bug.  thanks to Tom Dickey.
 * also lint cleanup.
 *
 * Revision 1.16  1992/12/04  09:15:38  foxharp
 * delete unused assigns
 *
 * Revision 1.15  1992/08/20  23:40:48  foxharp
 * typo fixes -- thanks, eric
 *
 * Revision 1.14  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.13  1992/01/05  00:06:13  pgf
 * split mlwrite into mlwrite/mlprompt/mlforce to make errors visible more
 * often.  also normalized message appearance somewhat.
 *
 * Revision 1.12  1991/11/04  14:20:18  pgf
 * fixed broken mvdnwind()
 *
 * Revision 1.11  1991/11/01  14:38:00  pgf
 * saber cleanup
 *
 * Revision 1.10  1991/10/22  14:08:23  pgf
 * took out old ifdef BEFORE code
 *
 * Revision 1.9  1991/09/30  01:47:24  pgf
 * keep sideways motions local to a window
 *
 * Revision 1.8  1991/09/26  13:09:55  pgf
 * w_sideways is now one of the window values
 *
 * Revision 1.7  1991/08/07  12:35:07  pgf
 * added RCS log messages
 *
 * revision 1.6
 * date: 1991/08/06 15:27:21;
 * new splitwind algorithm
 * 
 * revision 1.5
 * date: 1991/08/02 10:27:07;
 * added dave lemke's scroll fix to mvupwind()
 * 
 * revision 1.4
 * date: 1991/06/25 19:53:41;
 * massive data structure restructure
 * 
 * revision 1.3
 * date: 1991/02/21 09:14:20;
 * added horizontal scrolling, and made newlength and
 * newwidth into commands
 * 
 * revision 1.2
 * date: 1990/09/25 11:38:28;
 * took out old ifdef BEFORE code
 * 
 * revision 1.1
 * date: 1990/09/21 10:26:21;
 * initial vile RCS revision
 */

#include        "estruct.h"
#include	"edef.h"

#if	MEGAMAX & ST520
overlay	"window"
#endif

static	void	unlink_window P(( WINDOW * ));
static	LINEPTR	adjust_forw P(( WINDOW *, LINEPTR, int ));
static	LINEPTR	adjust_back P(( WINDOW *, LINEPTR, int ));
static	int	scroll_sideways P(( int, int ));

/*--------------------------------------------------------------------------*/

/*
 * Unlink the given window-pointer from the list
 */
static
void	unlink_window(thewp)
	WINDOW	*thewp;
{
	register WINDOW *p, *q;

	for (p = wheadp, q = 0; p != 0; q = p, p = p->w_wndp)
		if (p == thewp) {
			if (q != 0)
				q->w_wndp = p->w_wndp;
			else
				wheadp = p->w_wndp;
			break;
		}
}

/*
 * Set the current window (and associated current buffer).
 */
int
set_curwp (wp)
WINDOW	*wp;
{
	curwp = wp;
	make_current(curwp->w_bufp);
	upmode();
	updatelistbuffers();
	return (TRUE);
}

/*
 * Adjust a LINEPTR forward by the given number of screen-rows, limited by
 * the end of the buffer.
 */
static LINEPTR
adjust_forw(wp, lp, n)
WINDOW	*wp;
LINEPTR	lp;
int	n;
{
	register int i;
	for (i = n; i > 0 && !same_ptr(lp, wp->w_bufp->b_line.l); ) {
		if ((i -= line_height(wp, lp)) < 0)
			break;
		lp = lFORW(lp);
	}
	return lp;
}

/*
 * Adjust a LINEPTR backward by the given number of screen-rows, limited by
 * the end of the buffer.
 */
static LINEPTR
adjust_back(wp, lp, n)
WINDOW	*wp;
LINEPTR	lp;
int	n;
{
	register int i;
	for (i = n; i > 0 && !same_ptr(lp, wp->w_bufp->b_line.l); ) {
		if ((i -= line_height(wp, lp)) < 0)
			break;
		lp = lBACK(lp);
	}
	return lp;
}

/*
 * Reposition dot's line to line "n" of the window. If the argument is
 * positive, it is that line. If it is negative it is that line from the
 * bottom. If it is 0 the window is centered around dot (this is what 
 * the standard redisplay code does). Defaults to 0.
 */
int
reposition(f, n)
int f,n;
{
	if (f == FALSE)		/* default to 0 to center screen */
		n = 0;
	curwp->w_force = n;
	curwp->w_flag |= WFFORCE;
	return (TRUE);
}

/*
 * Refresh the screen. With no argument, it just does the refresh. With an
 * argument it recenters "." in the current window.
 */
/* ARGSUSED */
int
refresh(f, n)
int f,n;
{

	if (f == FALSE) {
		sgarbf = TRUE;
	} else {
	        curwp->w_force = 0;             /* Center dot. */
	        curwp->w_flag |= WFFORCE;
	}

	return (TRUE);
}

/*
 * The command make the next window (next => down the screen) the current
 * window. There are no real errors, although the command does nothing if
 * there is only 1 window on the screen.
 *
 * with an argument this command finds the <n>th window from the top
 *
 */
int
nextwind(f, n)
int f, n;	/* default flag and numeric argument */
{
	register WINDOW *wp;
	register int nwindows;		/* total number of windows */

	if (f) {

		/* first count the # of windows */
		nwindows = 1;
		for_each_window(wp)
			nwindows++;

		/* if the argument is negative, it is the nth window
		   from the bottom of the screen			*/
		if (n < 0)
			n = nwindows + n + 1;

		/* if an argument, give them that window from the top */
		if (n > 0 && n <= nwindows) {
			wp = wheadp;
			while (--n)
				wp = wp->w_wndp;
		} else {
			mlforce("[Window number out of range]");
			return(FALSE);
		}
	} else {
		if ((wp = curwp->w_wndp) == NULL)
			wp = wheadp;
	}
	return set_curwp(wp);
}

int
poswind(f,n)
int f,n;
{
	register int c;
	register int rows;
	int s;

	c = kbd_key();
	if (c == abortc)
		return FALSE;

	if (c == '+' || c == '\r' || c == 'H') {
		rows = 1;
	} else if (c == '.' || c == 'M') {
		rows = 0;
	} else if (c == '-' || c == 'L') {
		rows = -1;
	} else {
		TTbeep();
		return FALSE;
	}

	if (f == TRUE) {
		s = gotoline(f,n);
		if (s != TRUE)
			return(s);
	}
	return(reposition(TRUE,rows));
}

/*
 * This command makes the previous window (previous => up the screen) the
 * current window. There aren't any errors, although the command does not do a
 * lot if there is 1 window.
 */
int
prevwind(f, n)
int f,n;
{
	register WINDOW *wp1;
	register WINDOW *wp2;

	/* if we have an argument, we mean the nth window from the bottom */
	if (f)
		return(nextwind(f, -n));

	wp1 = wheadp;
	wp2 = curwp;

	if (wp1 == wp2)
		wp2 = NULL;

	while (wp1->w_wndp != wp2)
		wp1 = wp1->w_wndp;

	return set_curwp(wp1);
}

/*
 * This command moves the current window down by "arg" lines. Recompute the
 * top line in the window. The move up and move down code is almost completely
 * the same; most of the work has to do with reframing the window, and picking
 * a new dot. We share the code by having "move down" just be an interface to
 * "move up". Magic.
 */
int
mvdnwind(f, n)
int f,n;
{
	if (!f)
		n = 1;
	return (mvupwind(TRUE, -n));
}

/*
 * Move the current window up by "arg" lines. Recompute the new top line of
 * the window. Look to see if "." is still on the screen. If it is, you win.
 * If it isn't, then move "." to center it in the new framing of the window
 * (this command does not really move "." (except as above); it moves the 
 * frame).
 */
int
mvupwind(f, n)
int f,n;
{
	register LINE  *lp;
	register int    i;
	int             was_n = n;

	lp = l_ref(curwp->w_line.l);

	if (!f)
		n = 1;

	if (n < 0)
		curwp->w_flag |= WFKILLS;
	else
		curwp->w_flag |= WFINS;

	if (n < 0) {
		while (n++ && lforw(lp) != l_ref(curbp->b_line.l))
			lp = lforw(lp);
	} else {
		while (n-- && lback(lp) != l_ref(curbp->b_line.l))
			lp = lback(lp);
	}

	curwp->w_line.l = l_ptr(lp);
	curwp->w_flag |= WFHARD | WFMODE;

	/* is it still in the window */
	for (i = 0; i < curwp->w_ntrows; lp = lforw(lp)) {
		if ((i += line_height(curwp,l_ptr(lp))) > curwp->w_ntrows)
			break;
		if (lp == l_ref(DOT.l))
			return (TRUE);
		if (lforw(lp) == l_ref(curbp->b_line.l))
			break;
	}
	/*
	 * now lp is either just past the window bottom, or it's the last
	 * line of the file
	 */

	/* preserve the current column */
	if (curgoal < 0)
		curgoal = getccol(FALSE);

	if (was_n < 0)
		DOT.l = curwp->w_line.l;
	else
		DOT.l = l_ptr(lback(lp));
	DOT.o = getgoal(l_ref(DOT.l));
	return (TRUE);
}

int
mvdnnxtwind(f, n)
int f,n;
{
	int	status;

	(void)nextwind(FALSE, 1);
	status = mvdnwind(f, n);
	(void)prevwind(FALSE, 1);
	return status;
}

int
mvupnxtwind(f, n)
int f,n;
{
	int	status;

	(void)nextwind(FALSE, 1);
	status = mvupwind(f, n);
	(void)prevwind(FALSE, 1);
	return status;
}

static int
scroll_sideways(f,n)
int	f,n;
{
	int	original = w_val(curwp,WVAL_SIDEWAYS);

	if (!f) {
		int	nominal = term.t_ncol / 2;
		n = (n > 0) ? nominal : -nominal;
	}

	make_local_w_val(curwp,WVAL_SIDEWAYS);
	w_val(curwp, WVAL_SIDEWAYS) += n;

	if (w_val(curwp, WVAL_SIDEWAYS) < 0) {
		if (original == 0)
			TTbeep();
		w_val(curwp, WVAL_SIDEWAYS) = 0;
	}

	if (original != w_val(curwp,WVAL_SIDEWAYS))
        	curwp->w_flag  |= WFHARD|WFMOVE|WFMODE;

	return TRUE;
}

int
mvrightwind(f,n)
int f,n;
{
	return scroll_sideways(f,n);
}

int
mvleftwind(f,n)
int f,n;
{
	return scroll_sideways(f,-n);
}

/*
 * This command makes the current window the only window on the screen.
 * Try to set the framing so that "." does not have to move on the
 * display. Some care has to be taken to keep the values of dot and mark in
 * the buffer structures right if the distruction of a window makes a buffer
 * become undisplayed.
 */
/* ARGSUSED */
int
onlywind(f, n)
int f,n;
{
        register WINDOW *wp;

        wp = wheadp;
        while (wp != NULL) {
		register WINDOW *nwp;
		nwp = wp->w_wndp;
        	if (wp != curwp) {
	                if (--wp->w_bufp->b_nwnd == 0)
	                        undispbuff(wp->w_bufp,wp);
			unlink_window(wp);
	                free((char *) wp);
	        }
                wp = nwp;
        }
        wheadp = curwp;
        wheadp->w_wndp = NULL;

        curwp->w_line.l = adjust_back(curwp, curwp->w_line.l, curwp->w_toprow);
        curwp->w_ntrows = term.t_nrow-1;
        curwp->w_toprow = 0;
        curwp->w_flag  |= WFMODE|WFHARD;
        return (TRUE);
}

/*
 * Delete the current window, placing its space in the window above,
 * or, if it is the top window, the window below.
 */

/* ARGSUSED */
int
delwind(f,n)
int f, n;	/* arguments are ignored for this command */
{
	return delwp(curwp);
}

int
delwp(thewp)
WINDOW *thewp;
{
	register WINDOW *wp;	/* window to receive deleted space */

	/* if there is only one window, don't delete it */
	if (wheadp->w_wndp == NULL) {
		mlforce("[Cannot delete the only window]");
		return(FALSE);
	}

	/* find receiving window and give up our space */
	if (thewp == wheadp) { /* there's nothing before */
		/* find the next window down */
		wp = thewp->w_wndp;
                wp->w_line.l = adjust_back(wp, wp->w_line.l, wp->w_toprow);
		wp->w_ntrows += wp->w_toprow;  /* add in the new rows */
		wp->w_toprow = 0;	/* and we're at the top of the screen */
		wheadp = wp;	/* and at the top of the list as well */
	} else {
		/* find window before thewp in linked list */
		wp = wheadp;
		while(wp->w_wndp != thewp)
			wp = wp->w_wndp;
		/* add thewp's rows to the next window up */
		wp->w_ntrows += thewp->w_ntrows+1;
		
		wp->w_wndp = thewp->w_wndp; /* make their next window ours */
	}

	/* get rid of the current window */
	if (--thewp->w_bufp->b_nwnd == 0)
		undispbuff(thewp->w_bufp,thewp);
	if (thewp == curwp) {
		curwp = wp;
		curwp->w_flag |= WFHARD;
		make_current(curwp->w_bufp);
	}
	free((char *)thewp);
	upmode();
	return(TRUE);
}

/*
 * Copy the window-traits struct, and adjust the embedded VAL struct so that
 * modes that are local remain so.
 */
void
copy_traits(dst, src)
W_TRAITS *dst;
W_TRAITS *src;
{
	*dst = *src;
	copy_mvals(NUM_W_VALUES, dst->w_vals.wv, src->w_vals.wv);
}

/*
	Split the current window.  A window smaller than 3 lines cannot be
	split.  An argument of 1 forces the cursor into the upper window, an
	argument of two forces the cursor to the lower window.  The only other
	error that is possible is a "malloc" failure allocating the structure
	for the new window.
 */
WINDOW *
splitw(f, n)
int f,n;
{
        register WINDOW *wp;
        register LINE   *lp;
        register int    ntru;
        register int    ntrl;
        register int    ntrd;
        register WINDOW *wp1;
        register WINDOW *wp2;

        if (curwp->w_ntrows < 3) {
                mlforce("[Cannot split a %d line window]", curwp->w_ntrows);
                return NULL;
        }
        if ((wp = typealloc(WINDOW)) == NULL) {
		(void)no_memory("WINDOW");
                return NULL;
        }
	++curwp->w_bufp->b_nwnd;	       /* Displayed twice.     */
        wp->w_bufp  = curwp->w_bufp;
        copy_traits(&(wp->w_traits), &(curwp->w_traits));
        wp->w_flag  = 0;
        wp->w_force = 0;
        ntru = (curwp->w_ntrows-1) / 2;         /* Upper size           */
        ntrl = (curwp->w_ntrows-1) - ntru;      /* Lower size           */

        lp = l_ref(curwp->w_line.l);
        ntrd = 0;
        while (lp != l_ref(DOT.l)) {
                ntrd += line_height(wp,l_ptr(lp));
                lp = lforw(lp);
        }

	/* ntrd is now the row containing dot */
        if (((f == FALSE) && (ntrd <= ntru)) || ((f == TRUE) && (n == 1))) {
                /* Old is upper window. */
	        /* Adjust the top line if necessary */
                if (ntrd == ntru) {             /* Hit mode line.       */
			if (ntrl > 1) {
				ntru++;
				ntrl--;
			} else {
	                        curwp->w_line.l = lFORW(curwp->w_line.l);
			}
		}
                curwp->w_ntrows = ntru; /* new size */
		/* insert new window after curwp in window list */
                wp->w_wndp = curwp->w_wndp;
                curwp->w_wndp = wp;
		/* set new window's position and size */
                wp->w_toprow = curwp->w_toprow+ntru+1;
                wp->w_ntrows = ntrl;
		/* try to keep lower from reframing */
		wp->w_line.l = adjust_forw(wp, wp->w_line.l, ntru+1);
		wp->w_dot.l = wp->w_line.l;
		wp->w_dot.o = 0;
        } else {
		/* Old is lower window  */
                wp1 = NULL;
                wp2 = wheadp;
                while (wp2 != curwp) {
                        wp1 = wp2;
                        wp2 = wp2->w_wndp;
                }
                if (wp1 == NULL)
                        wheadp = wp;
                else
                        wp1->w_wndp = wp;
                wp->w_wndp   = curwp;
                wp->w_toprow = curwp->w_toprow;
                wp->w_ntrows = ntru;
                ++ntru;                         /* Mode line.           */
                curwp->w_toprow += ntru;
                curwp->w_ntrows  = ntrl;
		wp->w_dot.l = wp->w_line.l;
		/* move upper window dot to bottom line of upper */
		wp->w_dot.l = adjust_forw(wp, wp->w_dot.l, ntru-2);
		wp->w_dot.o = 0;
		/* adjust lower window topline */
		curwp->w_line.l = adjust_forw(curwp, curwp->w_line.l, ntru);
        }
        curwp->w_flag |= WFMODE|WFHARD;
        wp->w_flag |= WFMODE|WFHARD;
        return wp;
}

/* external callable version -- return int instead of (WINDOW *) */
int
splitwind(f,n)
int f,n;
{
	return (splitw(f,n)) ? TRUE:FALSE;
}

/*
 * Enlarge the current window. Find the window that loses space. Make sure it
 * is big enough. If so, hack the window descriptions, and ask redisplay to do
 * all the hard work. You don't just set "force reframe" because dot would
 * move.
 */
/* ARGSUSED */
int
enlargewind(f, n)
int f,n;
{
        register WINDOW *adjwp;

        if (n < 0)
                return (shrinkwind(f, -n));
        if (wheadp->w_wndp == NULL) {
                mlforce("[Only one window]");
                return (FALSE);
        }
        if ((adjwp=curwp->w_wndp) == NULL) {
                adjwp = wheadp;
                while (adjwp->w_wndp != curwp)
                        adjwp = adjwp->w_wndp;
        }
        if (adjwp->w_ntrows <= n) {
                mlforce("[Impossible change]");
                return (FALSE);
        }
        if (curwp->w_wndp == adjwp) {           /* Shrink below.        */
                adjwp->w_line.l  = adjust_forw(adjwp, adjwp->w_line.l, n);
                adjwp->w_toprow += n;
        } else {                                /* Shrink above.        */
                curwp->w_line.l  = adjust_back(curwp, curwp->w_line.l, n);
                curwp->w_toprow -= n;
        }
        curwp->w_ntrows += n;
        adjwp->w_ntrows -= n;
        curwp->w_flag |= WFMODE|WFHARD|WFINS;
        adjwp->w_flag |= WFMODE|WFHARD|WFKILLS;
        return (TRUE);
}

/*
 * Shrink the current window. Find the window that gains space. Hack at the
 * window descriptions. Ask the redisplay to do all the hard work.
 */
int
shrinkwind(f, n)
int f,n;
{
        register WINDOW *adjwp;

        if (n < 0)
                return (enlargewind(f, -n));
        if (wheadp->w_wndp == NULL) {
                mlforce("[Only one window]");
                return (FALSE);
        }
        if ((adjwp=curwp->w_wndp) == NULL) {
                adjwp = wheadp;
                while (adjwp->w_wndp != curwp)
                        adjwp = adjwp->w_wndp;
        }
        if (curwp->w_ntrows <= n) {
                mlforce("[Impossible change]");
                return (FALSE);
        }
        if (curwp->w_wndp == adjwp) {           /* Grow below.          */
                adjwp->w_line.l  = adjust_back(adjwp, adjwp->w_line.l, n);
                adjwp->w_toprow -= n;
        } else {                                /* Grow above.          */
                curwp->w_line.l  = adjust_forw(curwp, curwp->w_line.l, n);
                curwp->w_toprow += n;
        }
        curwp->w_ntrows -= n;
        adjwp->w_ntrows += n;
        curwp->w_flag |= WFMODE|WFHARD|WFKILLS;
        adjwp->w_flag |= WFMODE|WFHARD|WFINS;
        return (TRUE);
}

#if !SMALLER || OPT_EVAL

/*	Resize the current window to the requested size	*/
int
resize(f, n)
int f, n;	/* default flag and numeric argument */
{
	int clines;	/* current # of lines in window */
	
	/* must have a non-default argument, else ignore call */
	if (f == FALSE)
		return(TRUE);

	/* find out what to do */
	clines = curwp->w_ntrows;

	/* already the right size? */
	if (clines == n)
		return(TRUE);

	return(enlargewind(TRUE, n - clines));
}
#endif

/*
 * Pick a window for a pop-up. Split the screen if there is only one window.
 * Pick the uppermost window that isn't the current window. An LRU algorithm
 * might be better. Return a pointer, or NULL on error.
 */
WINDOW  *
wpopup()
{
        register WINDOW *wp;
        register WINDOW *owp;
        register WINDOW *biggest_wp;

	owp = curwp;
        wp = biggest_wp = wheadp;                /* Find window to split   */
        while (wp->w_wndp != NULL) {
                wp = wp->w_wndp;
		if(wp->w_ntrows > biggest_wp->w_ntrows)
			biggest_wp = wp;
	}
	curwp = biggest_wp;
	wp = splitw(FALSE,0); /* yes -- choose the unoccupied half */
	curwp = owp;
	if (wp == NULL ) { /* maybe biggest_wp was too small  */
		wp = wheadp;		/* Find window to use	*/
	        while (wp!=NULL && wp==curwp) /* uppermost non-current window */
	                wp = wp->w_wndp;
	}
        return wp;
}

int
scrnextup(f, n)		/* scroll the next window up (back) a page */
int f,n;
{
	int	status;

	(void)nextwind(FALSE, 1);
	status = backhpage(f, n);
	(void)prevwind(FALSE, 1);
	return status;
}

int
scrnextdw(f, n)		/* scroll the next window down (forward) a page */
int f,n;
{
	int	status;

	(void)nextwind(FALSE, 1);
	status = forwhpage(f, n);
	(void)prevwind(FALSE, 1);
	return status;
}

#if ! SMALLER
/* ARGSUSED */
int
savewnd(f, n)		/* save ptr to current window */
int f,n;
{
	swindow = curwp;
	return(TRUE);
}

/* ARGSUSED */
int
restwnd(f, n)		/* restore the saved screen */
int f,n;
{
	register WINDOW *wp;

	/* find the window */
	for_each_window(wp) {
		if (wp == swindow)
			return set_curwp(wp);
	}

	mlforce("[No such window exists]");
	return(FALSE);
}
#endif

int
newlength(f,n)	/* resize the screen, re-writing the screen */
int f,n;	/* numeric argument */
{
	WINDOW *wp;	/* current window being examined */
	WINDOW *nextwp;	/* next window to scan */
	WINDOW *lastwp;	/* last window scanned */
	int lastline;	/* screen line of last line of current window */

	if (!f) {
		mlforce("[No length given]");
		return FALSE;
	}

	/* make sure it's in range */
	if (n < 3 || n > term.t_mrow + 1) {
		mlforce("[Screen length out of range]");
		return(FALSE);
	}

	if (term.t_nrow == n - 1)
		return(TRUE);
	else if (term.t_nrow < n - 1) {

		/* go to the last window */
		wp = wheadp;
		while (wp->w_wndp != NULL)
			wp = wp->w_wndp;

		/* and enlarge it as needed */
		wp->w_ntrows = n - wp->w_toprow - 2;
		wp->w_flag |= WFHARD|WFMODE;

	} else {

		/* rebuild the window structure */
		nextwp = wheadp;
		lastwp = NULL;
		while (nextwp != NULL) {
			wp = nextwp;
			nextwp = wp->w_wndp;
	
			/* get rid of it if it is too low */
			if (wp->w_toprow > n - 2) {

				if (--wp->w_bufp->b_nwnd == 0) {
					undispbuff(wp->w_bufp,wp);
				}
	
				/* update curwp and lastwp if needed */
				if (wp == curwp) {
					curwp = wheadp;
					make_current(curwp->w_bufp);
				}
				if (lastwp != NULL)
					lastwp->w_wndp = NULL;

				/* free the structure */
				free((char *)wp);
				wp = NULL;

			} else {
				/* need to change this window size? */
				lastline = wp->w_toprow + wp->w_ntrows - 1;
				if (lastline >= n - 2) {
					wp->w_ntrows = n - wp->w_toprow - 2;
					wp->w_flag |= WFHARD|WFMODE;
				}
			}
	
			lastwp = wp;
		}
	}

	/* screen is garbage */
	term.t_nrow = n - 1;
	sgarbf = TRUE;
	return(TRUE);
}

int
newwidth(f,n)	/* resize the screen, re-writing the screen */
int f,n;	/* numeric argument */
{
	register WINDOW *wp;

	if (!f) {
		mlforce("[No width given]");
		return FALSE;
	}

	/* make sure it's in range */
	if (n < 10 || n > term.t_mcol) {
		mlforce("[Screen width out of range]");
		return(FALSE);
	}

	/* otherwise, just re-width it (no big deal) */
	term.t_ncol = n;
	term.t_margin = n / 10;
	term.t_scrsiz = n - (term.t_margin * 2);

	/* force all windows to redraw */
	for_each_window(wp)
		wp->w_flag |= WFHARD | WFMOVE | WFMODE;

	sgarbf = TRUE;

	return(TRUE);
}

#if OPT_EVAL
int
getwpos()	/* get screen offset of current line in current window */
{
	register int sline;	/* screen line from top of window */
	register LINE *lp;	/* scannile line pointer */

	/* search down the line we want */
	lp = l_ref(curwp->w_line.l);
	sline = 1;
	while (lp != l_ref(DOT.l)) {
		sline += line_height(curwp,l_ptr(lp));
		lp = lforw(lp);
	}

	/* and return the value */
	return(sline);
}
#endif

/*
 * Initialize all of the windows.
 */
void
winit()
{
        register WINDOW *wp;

        wp = typealloc(WINDOW);			/* First window         */
        if (wp==NULL)
		ExitProgram(BAD(1));
        wheadp = wp;
        curwp  = wp;
        wp->w_wndp  = NULL;                     /* Initialize window    */
        wp->w_dot  = nullmark;
	wp->w_line = nullmark;
#if WINMARK
        wp->w_mark = nullmark;
#endif
        wp->w_lastdot = nullmark;
        wp->w_toprow = 0;
        wp->w_values = global_w_values;
        wp->w_ntrows = term.t_nrow-1;           /* "-1" for mode line.  */
        wp->w_force = 0;
        wp->w_flag  = WFMODE|WFHARD;            /* Full.                */
	wp->w_bufp = NULL;
}

/* For memory-leak testing (only!), releases all display storage. */
#if NO_LEAKS
void	wp_leaks()
{
	register WINDOW *wp;

	while ((wp = wheadp) != 0) {
        	wp = wp->w_wndp;
		free((char *)wheadp);
		wheadp = wp;
	}
}
#endif
