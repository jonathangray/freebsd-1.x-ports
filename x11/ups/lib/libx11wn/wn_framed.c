/* wn_framed.c - the Sunview frame deamon */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char wn_wn_framed_sccsid[] = "@(#)wn_framed.c	1.10 25/4/92 (UKC)";

#ifdef SUNVIEW
#include <sys/time.h>
#include <sys/types.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <pixrect/pixrect.h>
#include <pixrect/memvar.h>
#include <pixrect/pixfont.h>
#include <sunwindow/rect.h>
#include <sunwindow/rectlist.h>
#include <sunwindow/pixwin.h>
#include <sunwindow/win_input.h>
#include <sunwindow/win_cursor.h>
#include <sunwindow/win_struct.h>
#include <sunwindow/notify.h>
#include <suntool/window.h>
#include <suntool/frame.h>
#include <pixrect/memvar.h>

#include <local/ukcprog.h>
#include "framed.h"

#include "wn_framed.h"

/*  File descriptor of the frame
 */
static int Frame_wd;

/*  File descriptor of the main (enclosed) window
 */
static int Main_wd;

/*  Process id of parent.
 */
static int Parent_pid;

static struct rect Saved_rect;

/*  How often in seconds we check that the parent is still alive.
 */
#define CHECK_INTERVAL	2

/*  Dimenstions of the border.
 */
#define BORDER_WIDTH	2
#define TBAR_HEIGHT	18

#define dbputs(s)

static int Iconic;

static int
is_iconic(wfd)
int wfd;
{
	struct rect r;

	win_getrect(wfd, &r);
	return r.r_width == 64 && r.r_height == 64;
}

/*  This is called by the notifier when the frame window changes size.
 *  It resizes the enclosed window.
 */
/* ARGSUSED */
static Notify_value
sizechild(client, sig, mode)
Notify_client client;
int sig;
Notify_signal_mode mode;
{
	static int window_inserted = FALSE;
	Rect wr, fwr;
	coord win_getheight(), win_getwidth();

	dbputs("Enter ws");
	win_getrect(Frame_wd, &fwr);

	if (fwr.r_top == Saved_rect.r_top &&
	    fwr.r_left == Saved_rect.r_left &&
	    fwr.r_width == Saved_rect.r_width &&
	    fwr.r_height == Saved_rect.r_height) {
		dbputs("flip Iconic");
		Iconic = !Iconic;
	}

	dbputs("get saved rect");
	win_getsavedrect(Frame_wd, &Saved_rect);

	if (Iconic && window_inserted) {
		dbputs("remove Main_wd");
		win_remove(Main_wd);
		window_inserted = FALSE;
	}
	if (!Iconic) {
		wr.r_top = TBAR_HEIGHT;
		wr.r_left = BORDER_WIDTH;
		wr.r_width = fwr.r_width - (2 * BORDER_WIDTH);
		wr.r_height = fwr.r_height - (TBAR_HEIGHT + BORDER_WIDTH);
		dbputs("Setrect on Main_wd");
		win_setrect(Main_wd, &wr);
		if (!window_inserted) {
			dbputs("Insert Main_wd");
			win_insert(Main_wd);
			window_inserted = TRUE;
		}
	}
	dbputs("Leave ws");
	return NOTIFY_DONE;
}

/* ARGSUSED */
static Notify_value
checkparent(client, which)
Notify_client client;
int which;
{
	if (kill(Parent_pid, 0) == -1 && errno == ESRCH)
		exit(0);
	return NOTIFY_DONE;
}

/* ARGSUSED */
static Notify_value
die(client, which)
Notify_client client;
int which;
{
	exit(0);
}

/*  Frame daemon. This process creates (with window_create()) and
 *  manages (with window_main_loop()) a frame window for the window
 *  referred to by Main_wd.
 */
void
_wn_framed(argc, argv)
int argc;
char **argv;
{
	int wd, i;
	struct itimerval itval;
	Frame base_frame;
	Notify_client client;
	char *getenv();

	Parent_pid = atoi(argv[ARGN_PID]);
	Main_wd = atoi(argv[ARGN_WFD]);

	base_frame = window_create(NULL, FRAME,
					FRAME_LABEL, argv[ARGN_TITLE],
					FRAME_ARGS, argc - ARGN_ARGS_OFFSET,
						    argv + ARGN_ARGS_OFFSET,
					0);
	client = (Notify_client) &base_frame;
	Frame_wd = (int) window_get(base_frame, WIN_FD);

	win_setlink(Main_wd, WL_PARENT, win_fdtonumber(Frame_wd));
	win_setlink(Main_wd, WL_COVERED, win_fdtonumber(Frame_wd));

	Iconic = is_iconic(Frame_wd);
	(void) sizechild(client, 0, (Notify_signal_mode)0);

	notify_set_signal_func(client, sizechild, SIGWINCH, NOTIFY_SYNC);
	notify_set_signal_func(client, die, SIGHUP, NOTIFY_SYNC);

	itval.it_interval.tv_sec = CHECK_INTERVAL;
	itval.it_interval.tv_usec = 0;
	itval.it_value = itval.it_interval;
	notify_set_itimer_func(client, checkparent, ITIMER_REAL,
						&itval, (struct itimerval *)NULL);
#if 0
	signal(SIGALRM, checkparent);
	setitimer(ITIMER_REAL, &itval, (struct itimerval *)NULL);
#endif

	kill(Parent_pid, SIGEMT);
	if (getenv("FRAMED_PAUSE") != NULL)
		sleep(atoi(getenv("FRAMED_PAUSE")));
	window_main_loop(base_frame);
	kill(Parent_pid, SIGHUP);
	exit(0);
}
#endif /* SUNVIEW */
