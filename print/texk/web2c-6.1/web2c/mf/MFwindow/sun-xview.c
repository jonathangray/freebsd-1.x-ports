/* Link this to sun.c if you are running on a XView system (OpenWindows) */

/*
 *  author = "Pierre MacKay (from the original by Paul Richards)"
 *  version = "0.5",
 *  date = "5 May 1991 - June 1992"
 *  filename = "sun_xview.c",
 *  contact = "Pierre MacKay",
 *  email = "mackay@cs.washington.edu"
 * Graphics window interface to Metafont for Suns running
 *	OpenWindows Version 2
 * This code is converted to XView from a SunView translation of the
 * original sun.c.  Ideally it should be converted to true Xlib
 * functions, but it's pretty lightweight stuff except maybe over
 * a network. 
 * Destroy checking is bogus, but XView cleans up the mess ok, even
 * if you "quit" from the pulldown menu.  Otherwise it seems clean.
 * 
 * For the moment this and the SunView interface seem to be
 * mutually exclusive, since they use library routines with
 * the same names, which mightily confuses the loader. 
 * It doesn't help much to change the name of MFTERM since
 * XView wants to name its own terminal "sun" just like
 * the SunView terminal
 *
 * WARNING: the library sequence is critical, as I know
 * to my grief.  Make sure that virmf is loaded with libraries
 * in the sequence: -lxview -lXt -lX11 -lsuntool -lsunwindow -lpixrect
 * or you will get some rather bewildering (and fatal) errors.
 */

#define	EXTERN	extern
#include "../mfd.h" 

#ifdef XVIEW
/* xview headers use the non-POSIX 4.3 BSD types in <sys/types.h> */
#undef _POSIX_SOURCE
/* two name conflicts with mfd.h; fortunately we don't need either */
#undef ord
#undef reset

#include <stdio.h>
#include <sys/types.h>
#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/notice.h>

static void repaint_proc();
static void resize_proc();

static Notify_value mf_destroy_func();
extern Notify_error notify_dispatch();

static int destroy_ready;  /* could be used for tighter control */

/*
 * global handle on the graphics subwindow 
 *
 */

struct	MFsubwindow {
	int	mf_flags;
#define	MF_RESTART	0x01
	struct	pixwin *mf_pixwin;
	struct	rect mf_rect;
      } sun_mf_subwin; /* Make sure that names storage is allocated. */

static struct	MFsubwindow	*metafont_sw = &sun_mf_subwin;	/* window handle */

/*
 * Gray background for graphics area
 */

static short	mf_graybackground_image[] = {0x5555, 0xaaaa};
	mpr_static(sun_xview_gray_bkgr, 2, 2, 1, mf_graybackground_image);


Rect *rect;
Frame frame;
Canvas canvas;
Pixwin *pw;

/*
 * init_screen: boolean;  return true if window operations legal
 */

#ifdef __SUNVIEW_COMPAT__
mf_xview_initscreen()
#else
mf_sun_initscreen()
#endif
{
	(void)xv_init(NULL); /* Worked for a while without this---sinister!! */
	frame = xv_create(NULL,FRAME,
			  XV_LABEL, "METAFONT",
			  FRAME_SHOW_LABEL, TRUE,
			  WIN_ERROR_MSG, 
			    "! Window access requires METAFONT to run under OpenWindows\n",
			  0);
	/* interpose a destroy procedure so we can shut down cleanly */
	/* if only it worked.  As it is, this interposed procedure   */
	/* is needed for error-free compilation and loading, but I   */
	/* can't seem to get into it from the menu "quit" button.    */
	/* If you have a fix, let us know.                           */
	(void) notify_interpose_destroy_func(frame, mf_destroy_func);

 	canvas = xv_create(frame, CANVAS,
			       CANVAS_RESIZE_PROC, resize_proc,
			       WIN_ERROR_MSG, "Can't create canvas",
			       0);
	pw = canvas_pixwin(canvas);

	metafont_sw->mf_pixwin = canvas_pixwin(canvas);


	/* 
	 * Instead of using window_main_loop, just show the frame.
	 * Metafont's procedures will be in control, not the notifier.
	 */
	xv_set(frame,WIN_SHOW, TRUE, 0);

	rect = (Rect *)xv_get(canvas, WIN_RECT);  /* Get current dimensions */
	pw_replrop(pw,
		   0, 0,
		   rect->r_width,
		   rect->r_height,
		   PIX_SRC,
		   &sun_xview_gray_bkgr, 0, 0);	/* clear subwindow */

	return(1); /* Caller expects a TRUE value */
}

/*
 * updatescreen; -- Flush pending output and make sure screen is ready to view
 */

#ifdef __SUNVIEW_COMPAT__
mf_xview_updatescreen()
#else
mf_sun_updatescreen()
#endif
{
	/* Flush out the top rows */
        XFlush( (Display *) XV_DISPLAY_FROM_WINDOW(frame));
	(void)notify_dispatch();
	rect = (Rect *)xv_get(canvas, WIN_RECT);  /* Get current dimensions */
	if (metafont_sw->mf_flags & MF_RESTART) {
		metafont_sw->mf_flags &= ~MF_RESTART;
		pw_replrop(pw,
			   0, 0,
			   rect->r_width,
			   rect->r_height,
			   PIX_SRC,
			   &sun_xview_gray_bkgr, 0, 0);	/* clear subwindow */
	}
}

/*
 * blankrectangle: reset rectangle bounded by ([left,right],[top,bottom])
 *			to background color
 */
#ifdef __SUNVIEW_COMPAT__
mf_xview_blankrectangle(left, right, top, bottom)
#else
mf_sun_blankrectangle(left, right, top, bottom)
#endif
 
	screencol left, right;
	screenrow top, bottom;
{
	pw_writebackground(pw, left, top,
				right-left+1, bottom-top+1, PIX_CLR);
}

/*
 * paintrow -- paint "row" starting with color "init_color",  up to next
 *		transition specified by "transition_vector", switch colors,
 *		and continue for "vector_size" transitions.
 */

#ifdef __SUNVIEW_COMPAT__
mf_xview_paintrow(row, init_color, transition_vector, vector_size)
#else
mf_sun_paintrow(row, init_color, transition_vector, vector_size)
#endif
	screenrow	row;
	pixelcolor	init_color;
	transspec	transition_vector;
	screencol	vector_size;
{
	register	col;
	register	color;

	color = (init_color == 0)? 0 : 1;

	do {
		col = *transition_vector++;
		pw_vector(pw,
			  col, row, (*transition_vector)-1, row,
			  PIX_SRC, color);
		color = 1 - color;
	} while (--vector_size);
	(void)notify_dispatch();
}

static void
repaint_proc( /* Ignore args */ )
{
	/* if repainting is required, just restart */
	metafont_sw->mf_flags |= MF_RESTART;
}

static void
resize_proc( /* Ignore args */ )
{
	metafont_sw->mf_flags |= MF_RESTART;
}

static Notify_value
  mf_destroy_func(client, status)
Notify_client client;
Destroy_status status;
{
  puts("entered destroy function");
  if (status == DESTROY_CHECKING) {
    int answer = notice_prompt(client,NULL,
			       NOTICE_MESSAGE_STRINGS, "Really quit?", NULL,
			       NOTICE_BUTTON_YES, "Yes",
			       NOTICE_BUTTON_NO, "No",
			       NULL);
    if (answer == NOTICE_NO)
      notify_veto_destroy(client);
  } else if (status == DESTROY_CLEANUP) {
    return(notify_next_destroy_func(client,status));
  } else if (status == DESTROY_SAVE_YOURSELF)
    return(notify_next_destroy_func(client,status));
  /* Don't want to save a MF window */
  else puts("Process died.");
  return NOTIFY_DONE;
}


int
quit()
{
  fprintf(stderr,"now we are in quit");
  fflush(stderr);
  xv_destroy_safe(frame);
  return XV_OK;
}
#else
int mf_xiew_dummy;
#endif /* XVIEW */
