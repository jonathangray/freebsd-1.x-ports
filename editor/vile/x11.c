/*
 * 	X11 support, Dave Lemke, 11/91
 *
 * $Log: x11.c,v $
 * Revision 1.1  1994/02/01 03:29:44  jkh
 * Initial revision
 *
 * Revision 1.29  1993/09/10  16:06:49  pgf
 * tom's 3.61 changes
 *
 * Revision 1.28  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.27  1993/08/18  16:50:10  pgf
 * bumped mrow from 100 to 200 -- some people have very good eyesight
 *
 * Revision 1.26  1993/08/17  19:08:28  pgf
 * took out trace.h include
 *
 * Revision 1.25  1993/08/13  16:32:50  pgf
 * tom's 3.58 changes
 *
 * Revision 1.24  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.23  1993/07/27  18:06:20  pgf
 * see tom's 3.56 CHANGES entry
 *
 * Revision 1.22  1993/07/20  18:09:46  pgf
 * change order of class and progname lookups for defaults
 *
 * Revision 1.21  1993/07/15  10:37:58  pgf
 * see 3.55 CHANGES
 *
 * Revision 1.20  1993/07/08  15:13:21  pgf
 * removed inadvertent debug code
 *
 * Revision 1.19  1993/07/08  15:04:58  pgf
 * up max columns from 150 to 200 -- my screen is 164.  reduce max rows
 * from 200 to 100.
 *
 * Revision 1.18  1993/06/18  15:57:06  pgf
 * tom's 3.49 changes
 *
 * Revision 1.17  1993/05/05  11:18:08  pgf
 * fixed off by one in column accounting
 *
 * Revision 1.16  1993/04/28  15:37:54  pgf
 * fixed type mismatch on color routines
 *
 * Revision 1.15  1993/04/01  12:53:33  pgf
 * removed redundant includes and declarations
 *
 * Revision 1.14  1993/03/05  17:50:54  pgf
 * see CHANGES, 3.35 section
 *
 * Revision 1.13  1993/02/24  10:59:02  pgf
 * see 3.34 changes, in CHANGES file
 *
 * Revision 1.12  1993/02/15  10:13:41  pgf
 * more changes from phil rubini -- some typos, and keypad func. keys
 *
 * Revision 1.11  1993/02/12  10:44:07  pgf
 * added code to support arrow keys and function keys (from Phil Rubini?  I
 * lost the mail)
 *
 * Revision 1.10  1992/11/19  09:20:44  foxharp
 * eric krohn's window resize fix, and his new name, foreground, and
 * background support
 *
 * Revision 1.9  1992/08/20  23:40:48  foxharp
 * typo fixes -- thanks, eric
 *
 * Revision 1.8  1992/08/04  20:16:14  foxharp
 * prototype fixups
 *
 * Revision 1.7  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.6  1992/05/13  09:17:23  pgf
 * put in chris sherman's class_hint changes, changed strdup to strmalloc,
 * which is our routine that does the same thing
 *
 * Revision 1.5  1992/04/10  18:47:25  pgf
 * change abs to absol to get rid of name conflicts
 *
 * Revision 1.4  1992/03/07  10:27:03  pgf
 * avoid macro expansion loop -- Xos.h defines strchr and strrchr
 *
 * Revision 1.3  1991/12/11  21:23:13  pgf
 * added Log keyword
 *
 * Revision 1.2  1991/12/10
 * fixes from dave -- ISC conflicts in X header files, so we undef
 * it here.  also, a color bug
 *
 * Revision 1.1  1991/11/13
 * Initial revision
 */

#include	"estruct.h"
#include	"edef.h"

/* undef for the benefit of some X header files -- if you really _are_
	both ISC and X11, well, you know what to do. */
#undef ISC

#if X11

#if VMS
#undef UNIX
#endif

/* redefined in X11/Xos.h */
#undef strchr
#undef strrchr

#include	<X11/Xlib.h>
#include	<X11/Xutil.h>
#include	<X11/keysym.h>
#include	<X11/Xos.h>
#include	<X11/Xatom.h>

#define	XCalloc(type)	((type*)calloc(1, sizeof(type)))

#if !APOLLO || defined(__STDCPP__)	/* not in apollo sr10.2 */
extern	XClassHint *XAllocClassHint P((void)); /* usually in <X11/xutil.h> */
#else
#define XAllocClassHint() XCalloc(XClassHint)
#endif

#define	MARGIN	8
#define	SCRSIZ	64
#define	NPAUSE	10		/* # times thru update to pause */
#define	absol(x)	((x) > 0 ? (x) : -(x))
#define	CEIL(a,b)	((a + b - 1) / (b))

#define onMsgRow(tw)	(ttrow == (tw->rows - 1))

/* XXX -- use xcutsel instead */
#undef	SABER_HACK		/* hack to support Saber since it doesn't do
				 * selections right */


#define X_PIXEL ULONG

/* local screen rep flags */
#define	CELL_DIRTY	0x1
#define	CELL_REVERSE	0x2
#define	CELL_CURSOR	0x4
#define	CELL_SELECTION	0x8

/* local screen rep line flags */
#define	LINE_DIRTY	0x1

static char *displayname;
static Display *dpy;

typedef struct _text_win {
    /* X stuff */
    Display    *dpy;
    int         screen;
    Window      win;
    unsigned    bw;
    XFontStruct *pfont;
    GC          textgc;
    GC          reversegc;
    X_PIXEL	fg;
    X_PIXEL	bg;
    int         char_width,
                char_ascent,
                char_height;
    char       *fontname;

    /* text stuff */
    Bool        reverse;
    unsigned    rows,
                cols;
    Bool        show_cursor;
    int         cur_row,
                cur_col;
    UCHAR **sc;		/* what the screen looks like */
    UCHAR **attr;	/* per-char cell flags */
    UCHAR *line_attr;	/* pre-line attributes */

    /* selection stuff */
    Time        lasttime;	/* for multi-click */
    Time        click_timeout;
    int         numclicks;
    int         sel_start_col,
                sel_start_row;
    int         sel_end_col,
                sel_end_row;
    int         wipe_row,
                wipe_col;
    Bool        have_selection;
    Bool        show_selection;
    Bool	was_on_msgline;
    Atom        sel_prop;
    UCHAR *	selection_data;
    int         selection_len;
}           TextWindowRec, *TextWindow;

static	TextWindow cur_win;
static	TBUFF	*PasteBuf;
static	int	drawing_ruler;	/* for 'ruler' mode */

static	int	x_getc   P(( void )),
		x_cres   P(( char * ));

static	void	x_open   P(( void )),
		x_close  P(( void )),
		x_putc   P(( int )),
		x_flush  P(( void )),
		x_kopen  P(( void )),
		x_kclose P(( void )),
		x_move   P(( int, int )),
		x_eeol   P(( void )),
		x_eeop   P(( void )),
		x_beep   P(( void )),
		x_rev    P(( int ));

#if COLOR
static	void	x_fcol   P(( int )),
		x_bcol   P(( int ));
#endif

#ifdef SCROLLCODE
static	void	x_scroll P(( int, int, int ));
#endif

static	SIGT	x_quit (DEFINE_SIG_ARGS);

static	void	turnOffCursor P(( TextWindow ));
static	void	turnOnCursor P(( TextWindow ));
static	LINEPTR	row2line P(( WINDOW *, int, int * ));
static	void	free_selection P(( TextWindow ));
static	void	free_win_data P(( TextWindow ));
static	void	clear_row_selection P(( TextWindow, int, int, int ));
static	void	save_selection P(( TextWindow ));
static	void	change_selection P(( TextWindow, Bool, Bool ));
static	void	x_stash_selection P(( TextWindow ));
static	int	set_character_class P(( char * ));
static	void	x_touch P(( TextWindow, int, int, UINT, UINT ));
static	void	x_resize_screen P(( TextWindow, ALLOC_T, ALLOC_T ));
static	void	x_paste_selection P(( TextWindow ));
static	Bool	x_give_selection P(( TextWindow, XSelectionRequestEvent *, Atom, Atom ));
static	void	x_own_selection P(( TextWindow ));
static	void	extend_selection P(( TextWindow, int, int, Bool ));
static	void	multi_click P(( TextWindow, int, int ));
static	void	start_selection P(( TextWindow, XButtonPressedEvent *, int, int ));
static	XMotionEvent * compress_motion P(( XMotionEvent * ));
static	void	x_process_event P(( XEvent * ));
static	int	decoded_key P(( XEvent * ));
#if NEEDED
static	Bool	check_kbd_ev P(( Display *, XEvent *, char * ));
#endif
static	char *	strndup P(( char *, SIZE_T ));
static	X_PIXEL	color_value P(( Display *, int, char * ));
static	char *	any_resource P(( Display *, char * ));
static	void	x_get_defaults P(( Display *, int ));
static	void	wait_for_scroll P(( TextWindow ));
static	void	flush_line P(( UCHAR *, int, Bool, int, int ));
static	int	set_character_class_range P(( int, int, int ));
static	void	x_lose_selection P(( TextWindow ));
static	int	add2paste P(( TBUFF **, int ));
static	int	copy_paste P(( TBUFF **, char *, SIZE_T ));
static	void	x_get_selection P(( TextWindow, Atom, Atom, char *, SIZE_T, int ));
static	XFontStruct *query_font P(( TextWindow, char * ));

#define MY_NAME         "xvile"
#define MY_CLASS	"XVile"
#define	FONTNAME	"7x13"

static	char *	fontname;
static	char *	geometry;
static	char *	progname;
static	Bool	reverse_video;
static	char *	foreground_name;
static	char *	background_name;
static	X_PIXEL	foreground;
static	X_PIXEL	background;

static int  multi_click_time = 500;

static int  startx = 100,
            starty = 100;

static unsigned  start_rows = 36,
                 start_cols = 80;


TERM        term = {
    NULL,			/* these four values are set dynamically at
				 * open time */
    NULL,
    NULL,
    NULL,
    MARGIN,
    SCRSIZ,
    NPAUSE,
    x_open,
    x_close,
    x_kopen,
    x_kclose,
    x_getc,
    x_putc,
    x_flush,
    x_move,
    x_eeol,
    x_eeop,
    x_beep,
    x_rev,
    x_cres

#if	COLOR
    ,x_fcol,
    x_bcol
#endif

#if SCROLLCODE
    ,x_scroll
#endif
};


#define	x_width(tw)		((tw)->cols * (tw)->char_width)
#define	x_height(tw)		((tw)->rows * (tw)->char_height)
#define	x_pos(tw, c)		((c) * (tw)->char_width)
#define	y_pos(tw, r)		((r) * (tw)->char_height)
#define	text_y_pos(tw, r)	(y_pos((tw), (r)) + (tw)->char_ascent)

static void
turnOffCursor(tw)
TextWindow tw;
{
	tw->line_attr[tw->cur_row] |= LINE_DIRTY;
	tw->attr[tw->cur_row][tw->cur_col] &= ~CELL_CURSOR;
	tw->attr[tw->cur_row][tw->cur_col] |= CELL_DIRTY;
}

static void
turnOnCursor(tw)
TextWindow tw;
{
	tw->line_attr[tw->cur_row] |= LINE_DIRTY;
	tw->attr[tw->cur_row][tw->cur_col] |= (CELL_DIRTY|CELL_CURSOR);
}

/* why isn't this one standard? */
static char *
strndup(str, n)
    char       *str;
    SIZE_T      n;
{
    register char *t;

    if ((t = malloc((ALLOC_T)n)) != 0)
    	(void)memcpy(t, str, n);
    return t;
}

static void
free_selection(tw)
	TextWindow tw;
{
	if (tw->selection_len != 0) {
		free((char *)(tw->selection_data));
		tw->selection_len = 0;
	}
}

static void
free_win_data(tw)
	TextWindow tw;
{
	register int r;
	if (tw->sc != 0) {
		for (r = 0; r < tw->rows; r++) {
			free((char *)(tw->sc[r]));
			free((char *)(tw->attr[r]));
		}
		free((char *)(tw->sc));
		free((char *)(tw->attr));
		free((char *)(tw->line_attr));
	}
}

/* ARGSUSED */
void
x_preparse_args(pargc, pargv)
    int        *pargc;
    char     ***pargv;
{
    progname = pathleaf(*pargv[0]);
}

/* ARGSUSED */
void
x_setname(name)
    char     *name;
{
    if (name && *name != EOS)
	progname = name;
}

char       *
x_current_fontname()
{
    return cur_win->fontname;
}

void
x_set_rv()
{
    reverse_video = !reverse_video;
}

void
x_set_geometry(g)
    char       *g;
{
    geometry = g;
}

void
x_set_dpy(dn)
    char       *dn;
{
    displayname = dn;
}

void
x_setforeground(colorname)
    char       *colorname;
{
    foreground_name = colorname;
}

void
x_setbackground(colorname)
    char       *colorname;
{
    background_name = colorname;
}

static XFontStruct *
query_font(tw, fname)
	TextWindow tw;
	char	*fname;
{
	XFontStruct *pf;

	if ((pf = XLoadQueryFont(dpy, fname)) != 0) {
		if (pf->max_bounds.width != pf->min_bounds.width) {
			(void)fprintf(stderr,
				"proportional font, things will be miserable\n");
		}
		tw->pfont = pf;
		tw->char_width  = pf->max_bounds.width;
		tw->char_height = pf->ascent + pf->descent;
		tw->char_ascent = pf->ascent;
		tw->fontname = strmalloc(fname);
	}
	return pf;
}

int
x_setfont(fname)
    char       *fname;
{
    XFontStruct *pfont;
    XSizeHints  xsh;
    int         oldw,
                oldh;

    fontname = fname;
    if (cur_win) {
	oldw = x_width(cur_win);
	oldh = x_height(cur_win);
	if ((pfont = query_font(cur_win, fontname)) != 0) {

	    XSetFont(dpy, cur_win->textgc, pfont->fid);
	    XSetFont(dpy, cur_win->reversegc, pfont->fid);

	    /* is size changed, resize it, otherwise refresh */
	    if (oldw != x_width(cur_win) || oldh != x_height(cur_win)) {
		XResizeWindow(dpy, cur_win->win,
			      x_width(cur_win), x_height(cur_win));
	    } else {
		x_touch(cur_win, 0, 0, cur_win->cols, cur_win->rows);
		x_flush();
	    }
	    xsh.flags = PResizeInc | PMaxSize;
	    xsh.width_inc = cur_win->char_width;
	    xsh.height_inc = cur_win->char_height;
	    xsh.max_width = term.t_mrow * cur_win->char_height;
	    xsh.max_height = term.t_mcol * cur_win->char_width;
	    XSetNormalHints(dpy, cur_win->win, &xsh);

	    FreeIfNeeded(cur_win->fontname);
	    cur_win->fontname = strmalloc(fontname);
	    return 1;
	}
	return 0;
    }
    return 1;
}

static
/* ARGSUSED */
SIGT x_quit (ACTUAL_SIG_ARGS)
{
    x_close();
    ExitProgram(GOOD);
    /* NOTREACHED */
    SIGRET;
}

static void
x_resize_screen(tw, rows, cols)
    TextWindow  tw;
    ALLOC_T     rows;
    ALLOC_T     cols;
{
    unsigned    r,
                c;

    if (rows != tw->rows) {
	free_win_data(tw);
	tw->rows = rows;
	/* allocate screen */
	tw->sc   = typeallocn(UCHAR *, rows);
	tw->attr = typeallocn(UCHAR *, rows);
	tw->line_attr = typeallocn(UCHAR, rows);
    }
    tw->cols = cols;
    if (tw->cur_col >= tw->cols)
        tw->cur_col = tw->cols - 1;

    if (!tw->sc || !tw->attr || !tw->line_attr) {
	(void)fprintf(stderr, "couldn't allocate memory for screen\n");
	ExitProgram(BAD(-1));
    }
    /* init it */
    for (r = 0; r < tw->rows; r++) {
	tw->sc[r] = typeallocn(UCHAR, cols);
	tw->attr[r] = typeallocn(UCHAR, cols);
	if (!tw->sc[r] || !tw->attr[r]) {
	    (void)fprintf(stderr, "couldn't allocate memory for screen\n");
	    ExitProgram(BAD(-1));
	}
	tw->line_attr[r] = LINE_DIRTY;
	(void)memset((char *) tw->sc[r], ' ', (SIZE_T)tw->cols);
	for (c = 0; c < tw->cols; c++) {
	    tw->attr[r][c] = CELL_DIRTY;
	}
    }
    if (tw->cur_row >= tw->rows)
        tw->cur_row = tw->rows - 1;
}

static X_PIXEL
color_value(disp, screen, t)
    Display    *disp;
    int         screen;
    char       *t;
{
    XColor      xc;
    Colormap    cmap;

    cmap = DefaultColormap(disp, screen);

    XParseColor(disp, cmap, t, &xc);
    XAllocColor(disp, cmap, &xc);
    return xc.pixel;
}

static char *
any_resource(disp, name)
	Display *disp;
	char	*name;
{
	register char *it;

	if ((it = XGetDefault(disp, progname, name)) == 0)
		it = XGetDefault(disp, MY_CLASS, name);
	return it;
}

static void
x_get_defaults(disp, screen)
	Display    *disp;
	int         screen;
{
	char       *t;

	if (!fontname)
		fontname = any_resource(disp, "font");

	if (!geometry)
		geometry = any_resource(disp, "geometry");

	(void) set_character_class( any_resource(disp, "charClass") );

	if ((t = any_resource(disp, "multiClickTime")) != 0)
		multi_click_time = atoi(t);

	if ((t = any_resource(disp, "reverseVideo")) != 0)
		reverse_video = stol(t);

	foreground = BlackPixel(dpy, 0);
	t = any_resource(disp, "foreground");
	if (foreground_name)
		t = foreground_name;
	if (t)
		foreground = color_value(disp, screen, t);

	background = WhitePixel(dpy, 0);
	t = any_resource(disp, "background");
	if (background_name)
		t = background_name;
	if (t)
		background = color_value(disp, screen, t);
}

static void
x_open()
{
    TextWindow  tw;
    int         screen;
    XFontStruct *pfont;
    XGCValues   gcvals;
    ULONG	gcmask;
    XSetWindowAttributes swat;
    ULONG	winmask;
    XSizeHints  xsh;
    XWMHints    xwmh;
    int         flags;

    tw = XCalloc(TextWindowRec);
    if (!tw) {
	(void)fprintf(stderr, "insufficient memory, exiting\n");
	ExitProgram(BAD(-1));
    }
    dpy = XOpenDisplay(displayname);

#ifdef undef
    XSynchronize(dpy, 1);
#endif

    if (!dpy) {
	(void)fprintf(stderr, "couldn't open X display\n");
	ExitProgram(GOOD);
    }
    tw->dpy = dpy;
    tw->screen = screen = DefaultScreen(dpy);

    x_get_defaults(dpy, screen);

    pfont = query_font(tw, fontname);
    if (!pfont) {
	pfont = query_font(tw, FONTNAME);
	if (!pfont) {
	    (void)fprintf(stderr, "couldn't get font \"%s\" or \"%s\", exiting\n",
		    fontname, FONTNAME);
	    ExitProgram(BAD(-1));
	}
    }

    if (reverse_video) {
	tw->bg = foreground;
	tw->fg = background;
    } else {
	tw->fg = foreground;
	tw->bg = background;
    }

    gcmask = GCForeground | GCBackground | GCFont;
    gcvals.foreground = tw->fg;
    gcvals.background = tw->bg;
    gcvals.font = tw->pfont->fid;
    tw->textgc = XCreateGC(dpy, RootWindow(dpy, screen), gcmask, &gcvals);

    gcvals.foreground = tw->bg;
    gcvals.background = tw->fg;
    gcvals.font = tw->pfont->fid;
    tw->reversegc = XCreateGC(dpy, RootWindow(dpy, screen), gcmask, &gcvals);

    if (geometry) {
	flags = XParseGeometry(geometry, &startx, &starty,
			       &start_cols, &start_rows);
    } else
	flags = 0;
    x_resize_screen(tw, (ALLOC_T)start_rows, (ALLOC_T)start_cols);

    if ((flags & XValue) && (flags & XNegative))
	startx = DisplayWidth(dpy, screen) - x_width(tw);
    if ((flags & YValue) && (flags & YNegative))
	starty = DisplayHeight(dpy, screen) - x_height(tw);

    tw->reverse = False;

    tw->bw = 1;

    tw->click_timeout = multi_click_time;

    swat.background_pixel = tw->bg;
    swat.border_pixel = tw->fg;
    swat.event_mask = ExposureMask | KeyPressMask | StructureNotifyMask |
	ButtonPress | ButtonRelease | ButtonMotionMask |
	FocusChangeMask | EnterWindowMask | LeaveWindowMask;
    winmask = CWBackPixel | CWBorderPixel | CWEventMask;
    tw->win = XCreateWindow(dpy, RootWindow(dpy, screen), startx, starty,
		x_width(tw), x_height(tw), tw->bw,
		(int)CopyFromParent, InputOutput,
		(Visual *)CopyFromParent, winmask, &swat);


    /* these can go bigger, but they suck up lots of VM if they do */
    term.t_mcol = 200;		/* XXX */
    term.t_mrow = 200;		/* XXX */

    xsh.flags = PPosition | PResizeInc | PSize | PMaxSize;
    if (flags & (XValue | YValue))
	xsh.flags |= USPosition;
    if (flags & (WidthValue | HeightValue))
	xsh.flags |= USSize;

    xsh.width_inc = tw->char_width;
    xsh.height_inc = tw->char_height;
    xsh.x = startx;
    xsh.y = starty;
    xsh.width = x_width(tw);
    xsh.height = x_height(tw);
    xsh.max_width = term.t_mrow * tw->char_height;
    xsh.max_height = term.t_mcol * tw->char_width;
    XSetStandardProperties(dpy, tw->win,
    		MY_CLASS,	/* application name */
		MY_CLASS,	/* icon name */
		(Pixmap) 0,	/* icon pixmap */
		(char **)0, 0, &xsh);

    xwmh.flags = InputHint;
    xwmh.input = True;
    XSetWMHints(dpy, tw->win, &xwmh);

    {
      XClassHint *class_hints;
      class_hints = XAllocClassHint();
      class_hints->res_name = strmalloc(MY_NAME);
      class_hints->res_class = strmalloc(MY_CLASS);
      XSetClassHint(dpy,tw->win,class_hints);
      free(class_hints->res_name);
      free(class_hints->res_class);
      XFree((char *)class_hints);
    }
    cur_win = tw;
    XMapWindow(dpy, tw->win);

    XSync(dpy, 0);

    tw->sel_prop = XInternAtom(dpy, "VILE_SELECTION", False);

    (void)signal(SIGHUP, x_quit);
    (void)signal(SIGINT, catchintr);
    (void)signal(SIGTERM, x_quit);

    /* main code assumes that it can access a cell at nrow x ncol */
    term.t_ncol = tw->cols;
    term.t_nrow = tw->rows - 1;
}

static void
x_close()
{
    XCloseDisplay(dpy);
}

static void
x_kopen()
{
}

static void
x_kclose()
{
}

static void
x_touch(tw, sc, sr, ec, er)
	TextWindow tw;
	int	sc;
	int	sr;
	UINT    ec;
	UINT	er;
{
	register UINT	r;
	register UINT	c;

	if (er > tw->rows)
		er = tw->rows;
	if (ec > tw->cols)
		ec = tw->cols;

	for (r = sr; r < er; r++) {
		tw->line_attr[r] |= LINE_DIRTY;
		for (c = sc; c < ec; c++) {
			tw->attr[r][c] |= CELL_DIRTY;
		}
	}
}

/* XXX this mostly works, except for cursor dirt.  doesn't seem to be
 * a noticeable win, however...
 *
 * seems to give a apparent win under Xremote, but seems to repaint the stuff
 * that's scrolled...
 */
#define	copy_area

#ifdef copy_area
static void
wait_for_scroll(tw)
    TextWindow  tw;
{
    XEvent      ev;
    int         sc,
                sr;
    unsigned    ec,
                er;
    XGraphicsExposeEvent *gev;

    while (1) {			/* loop looking for a gfx expose or no expose */
	if (XCheckTypedEvent(tw->dpy, NoExpose, &ev))
	    return;
	if (XCheckTypedEvent(tw->dpy, GraphicsExpose, &ev)) {
	    gev = (XGraphicsExposeEvent *) & ev;
	    sc = gev->x / tw->char_width;
	    sr = gev->y / tw->char_height;
	    ec = CEIL(gev->x + gev->width,  tw->char_width);
	    er = CEIL(gev->y + gev->height, tw->char_height);
	    x_touch(tw, sc, sr, ec, er);
	    x_flush();
	    return;
	}
	XSync(tw->dpy, 0);
    }
}

#endif

/*
 * XXX this may not be any faster than having the guts do the scrolling
 * instead
 */
static void
x_scroll(from, to, count)
    int         from,
                to,
                count;
{
    int         rf,
                rt,
                fst,
                tst,
                finc,
                tinc,
                i,
                diff,
                c;

    /*
     * XXX since there aren't any hooks (yet) for scrolling, stop showing the
     * selection as soon as the text changes
     */
    save_selection(cur_win);

    /*
     * figure out what lines to move first, to prevent being hosed if the
     * scroll overlaps itself
     */
    if (from < to) {
	fst = from + count - 1;
	finc = -1;
	tst = to + count - 1;
	tinc = -1;
    } else {
	fst = from;
	finc = 1;
	tst = to;
	tinc = 1;
    }

#ifndef copy_area
    turnOffCursor(cur_win);
#endif

    for (rf = fst, rt = tst, i = 0; i < count; i++, rf += finc, rt += tinc) {
	(void)memcpy(
		(char *) cur_win->sc[rt],
		(char *) cur_win->sc[rf],
		(SIZE_T)cur_win->cols);
	(void)memset((char *) cur_win->sc[rf], ' ', (SIZE_T)cur_win->cols);

        /* only mark row if it isn't going to be overwritten during 
         * this scroll
         */
        if (!((rf > tst && rf < (tst + count)) || 
        	(rf < tst && rf > (tst - count))))
	    cur_win->line_attr[rf] |= LINE_DIRTY;
#ifndef copy_area
	cur_win->line_attr[rt] |= LINE_DIRTY;
#else
	if (rf == cur_win->cur_row) {	/* erase scrolled cursor */
	    cur_win->line_attr[rt] |= LINE_DIRTY;
	    cur_win->attr[rt][cur_win->cur_col] |= CELL_DIRTY;
	}
#endif

	for (c = 0; c < cur_win->cols; c++) {

#ifndef copy_area
	    cur_win->attr[rt][c] = cur_win->attr[rf][c] | CELL_DIRTY;
	    cur_win->attr[rt][c] &= ~CELL_CURSOR;
#endif

	    /* memor() would be useful here... */
	    cur_win->attr[rf][c] |= CELL_DIRTY;
	}
    }

#ifdef copy_area
    XCopyArea(cur_win->dpy, cur_win->win, cur_win->win, cur_win->textgc,
	      x_pos(cur_win, 0), y_pos(cur_win, from),
	      x_width(cur_win), (unsigned)(count * cur_win->char_height),
	      x_pos(cur_win, 0), y_pos(cur_win, to));
    XFlush(dpy);
    wait_for_scroll(cur_win);
#endif

    /* some lines weren't scrolled, so we need to wipe them */
    /*
     * XXX - why is this necessary?  it only gets hit when the scrolling code
     * gets 'smart' and realizes it doesn't have to copy all the lines around,
     * but then it doesn't bother to clean them either.
     */
    if (absol(from - to) != count) {
	diff = absol(from - to) - count;
	for (i = 0, rf = fst; i <= diff; i++, rf -= finc) {
	    (void)memset((char *) cur_win->sc[rf], ' ', (SIZE_T)cur_win->cols);
	    cur_win->line_attr[rf] |= LINE_DIRTY;
	    for (c = 0; c < cur_win->cols; c++) {
		/* memor() would be useful here... */
		cur_win->attr[rf][c] |= CELL_DIRTY;
	    }
	}
    }
}

#define	CLEAR_THRESH	8

static void
flush_line(text, len, rev, sr, sc)
UCHAR	*text;
int	len;
Bool	rev;
int	sr;
int	sc;
{
	GC	fore_gc = ( rev ? cur_win->reversegc : cur_win->textgc);
	GC	back_gc = (!rev ? cur_win->reversegc : cur_win->textgc);
	int	fore_yy = text_y_pos(cur_win, sr);
	int	back_yy = y_pos(cur_win, sr);
    char *p;
    int         cc,
                tlen,
                i;

    /* break line into TextStrings and FillRects */
    p = (char *)text;
    cc = 0;
    tlen = 0;
    for (i = 0; i < len; i++) {
	if (text[i] == ' ') {
	    cc++;
	    tlen++;
	} else {
	    if (cc >= CLEAR_THRESH) {
		tlen -= cc;
		XDrawImageString(dpy, cur_win->win, fore_gc,
				 (int)x_pos(cur_win, sc), fore_yy,
				 p, tlen);
		p += tlen + cc;
		sc += tlen;
		XFillRectangle(dpy, cur_win->win, back_gc,
			       x_pos(cur_win, sc), back_yy,
			       (unsigned)(cc * cur_win->char_width),
			       (unsigned)(cur_win->char_height));
		sc += cc;
		tlen = 1;	/* starting new run */
	    } else
		tlen++;
	    cc = 0;
	}
    }
    if (cc >= CLEAR_THRESH) {
	tlen -= cc;
	XDrawImageString(dpy, cur_win->win, fore_gc,
			 x_pos(cur_win, sc), fore_yy,
			 p, tlen);
	sc += tlen;
	XFillRectangle(dpy, cur_win->win, back_gc,
		       x_pos(cur_win, sc), back_yy,
		       (unsigned)(cc * cur_win->char_width),
		       (unsigned)(cur_win->char_height));
    } else if (tlen > 0) {
	XDrawImageString(dpy, cur_win->win, fore_gc,
			 x_pos(cur_win, sc), fore_yy,
			 p, tlen);
    }
}

#ifdef old
static void
clear_line(row, start, count)
    int         row,
                start,
                count;
{
    XClearArea(dpy, cur_win->win, x_pos(cur_win, start), y_pos(cur_win, row),
	       (count * cur_win->char_width), cur_win->char_height, False);
}

#endif

#undef	clear_hack

/* make sure the screen looks like we want it to */
static void
x_flush()
{
    UCHAR *	start;
    int         len;
    int         r,
                c;
    int         sc;
    Bool        isrev;
    Bool        flush;

#ifdef clear_hack
    int         clear_count;
    int         clear_start;

#endif
    int		revmask = (CELL_REVERSE | CELL_SELECTION | (drawing_ruler ? 0 : CELL_CURSOR));

#define	reversed(c)	((c) & revmask)

    for (r = 0; r < cur_win->rows; r++) {
	if (!(cur_win->line_attr[r] & LINE_DIRTY))
	    continue;
	start = NULL;
	len = 0;
	isrev = False;
	flush = False;
	sc = 0;

#ifdef clear_hack
	clear_count = 0;
	clear_start = -1;
#endif

	cur_win->line_attr[r] &= ~LINE_DIRTY;
	for (c = 0; c < cur_win->cols;) {
	    if (cur_win->attr[r][c] & CELL_DIRTY) {

#ifdef clear_hack
		if (cur_win->sc[r][c] == ' ' && clear_start <= 0) {
		    if (clear_start == -1)
			clear_start = c;
		    clear_count++;
		    c++;
		    continue;
		}
#endif

		if (isrev != (reversed(cur_win->attr[r][c]) ? True : False)) {
		    if (len) {
			flush_line(start, len, isrev, r, sc);
			start = NULL;
			len = 0;
			flush = False;
		    }
		    isrev = !isrev;
		    continue;
		}
		if (!len) {
		    start = &(cur_win->sc[r][c]);
		    sc = c;
		}
		len++;
		cur_win->attr[r][c] &= ~CELL_DIRTY;
	    } else if (len)
		flush = True;
	    c++;

#ifdef clear_hack
	    if (clear_count && clear_start == 0) {
		clear_line(r, clear_start, clear_count);
		clear_count = 0;
		clear_start = -1;
	    }
#endif

	    if (flush && len) {
		flush_line(start, len, isrev, r, sc);
		len = 0;
		flush = False;
	    }
	}

#ifdef clear_hack
	if (clear_count && clear_start == 0) {
	    clear_line(r, clear_start, clear_count);
	    clear_count = 0;
	    clear_start = -1;
	}
#endif

	if (len) {
	    flush_line(start, len, isrev, r, sc);
	}
    }
    /* last bit for cursor -- pretty sick, but it works */
    if (!cur_win->show_cursor)
	XDrawRectangle(dpy, cur_win->win, cur_win->textgc,
	  x_pos(cur_win, cur_win->cur_col), y_pos(cur_win, cur_win->cur_row),
		       (unsigned)(cur_win->char_width - 1),
		       (unsigned)(cur_win->char_height - 1));
    XFlush(dpy);
}

static void
x_move(row, col)
	int	row;
	int	col;
{
	if ((row != cur_win->cur_row && row >= 0)
	 || (col != cur_win->cur_col && col >= 0)) {
		turnOffCursor(cur_win);
		cur_win->cur_col = col;
		cur_win->cur_row = row;
		turnOnCursor(cur_win);
	}
}

#define	in_selection(tw, r)	((r) >= (tw)->sel_start_row && \
				 (r) <= (tw)->sel_end_row)

/* ARGSUSED */
void
x_putline(row, str, len)
    int         row;
    char       *str;
    int		len;
{
    int         c,
                i;

    if (len > cur_win->cols - cur_win->cur_col)
	len = cur_win->cols - cur_win->cur_col;

    /*
     * XXX since there aren't any hooks (yet) for scrolling, stop showing the
     * selection as soon as the text changes
     */
    if (in_selection(cur_win, row))
	save_selection(cur_win);

    turnOffCursor(cur_win);

    (void)memcpy(
    	(char *) &(cur_win->sc[row][cur_win->cur_col]),
    	(char *) str,
	len);
    for (i = 0, c = cur_win->cur_col; i < len; c++, i++) {
	if (cur_win->reverse)
	    cur_win->attr[row][c] |= CELL_REVERSE;
	else
	    cur_win->attr[row][c] &= ~CELL_REVERSE;
	cur_win->attr[row][c] |= CELL_DIRTY;
	cur_win->attr[row][c] |= CELL_DIRTY;
    }
    cur_win->cur_row = row;
    cur_win->cur_col = c - 1;

    turnOnCursor(cur_win);
}

static void
x_putc(c)
    int	c;
{
    /*
     * XXX since there aren't any hooks (yet) for scrolling, stop showing the
     * selection as soon as the text changes
     */
    if (in_selection(cur_win, cur_win->cur_row))
	save_selection(cur_win);

    turnOffCursor(cur_win);

    /* minibuffer prompt spits out real backspaces for some silly reason... */
    if (isprint(c)) {
	if (cur_win->reverse)
	    cur_win->attr[cur_win->cur_row][cur_win->cur_col] |= CELL_REVERSE;
	else
	    cur_win->attr[cur_win->cur_row][cur_win->cur_col] &= ~CELL_REVERSE;
	cur_win->sc[cur_win->cur_row][cur_win->cur_col] = c;
	cur_win->cur_col++;
    } else if (c == '\b') {
	cur_win->cur_col--;
    }
    turnOnCursor(cur_win);
}

/*
 * clear to end of line
 */
static void
x_eeol()
{
    int         c;

    c = cur_win->cur_col;

    (void)memset((char *) &(cur_win->sc[cur_win->cur_row][c]), ' ',
	   (SIZE_T)(cur_win->cols - c));

#ifdef old			/* XXX this might be faster, but it looks
				 * worse */
    clear_line(cur_win->cur_row, c, cur_win->cols - c);
#else
    while (c < cur_win->cols)
	cur_win->attr[cur_win->cur_row][c++] |= CELL_DIRTY;
#endif
}

/*
 * clear to end of page
 */
static void
x_eeop()
{
    int         r,
                sc,
                c;

    r = cur_win->cur_row;
/* XXX the old stuff is faster, but the only place the editot uses this
 * is to erase the whole page when updating, and in that case it usually
 * looks worse
 */

#ifdef old
    x_eeol();
    while (r < cur_win->rows) {
	clear_line(r, 0, cur_win->cols);
	(void)memset((char *) &(cur_win->sc[r][0]), ' ', cur_win->cols);
	r++;
    }
#else
    sc = cur_win->cur_col;
    while (r < cur_win->rows) {
	cur_win->line_attr[r] |= LINE_DIRTY;
	for (c = sc; c < cur_win->cols; c++)
	    cur_win->attr[r][c] |= CELL_DIRTY;
	(void)memset((char *) &(cur_win->sc[r][0]), ' ', (SIZE_T)cur_win->cols);
	r++;
	sc = 0;
    }
#endif
}


#ifdef notyet
x_putline(row, s, fg, bg)
    int         row;
    char       *s;
    int         fg,
                bg;
{
}

x_fastpoutline(row, s)
    int         row;
    char       *s;
{
}

x_setrowcolors(row, fg, bg)
    int         row,
                fg,
                bg;
{
}

x_cls()
{
}

#endif

/* selection processing stuff */

/* multi-click code stolen from xterm */
/*
 * double click table for cut and paste in 8 bits
 *
 * This table is divided in four parts :
 *
 *	- control characters	[0,0x1f] U [0x80,0x9f]
 *	- separators		[0x20,0x3f] U [0xa0,0xb9]
 *	- binding characters	[0x40,0x7f] U [0xc0,0xff]
 *  	- exceptions
 */
static int  charClass[256] = {
/* NUL  SOH  STX  ETX  EOT  ENQ  ACK  BEL */
    32, 1, 1, 1, 1, 1, 1, 1,
/*  BS   HT   NL   VT   NP   CR   SO   SI */
    1, 32, 1, 1, 1, 1, 1, 1,
/* DLE  DC1  DC2  DC3  DC4  NAK  SYN  ETB */
    1, 1, 1, 1, 1, 1, 1, 1,
/* CAN   EM  SUB  ESC   FS   GS   RS   US */
    1, 1, 1, 1, 1, 1, 1, 1,
/*  SP    !    "    #    $    %    &    ' */
    32, 33, 34, 35, 36, 37, 38, 39,
/*   (    )    *    +    ,    -    .    / */
    40, 41, 42, 43, 44, 45, 46, 47,
/*   0    1    2    3    4    5    6    7 */
    48, 48, 48, 48, 48, 48, 48, 48,
/*   8    9    :    ;    <    =    >    ? */
    48, 48, 58, 59, 60, 61, 62, 63,
/*   @    A    B    C    D    E    F    G */
    64, 48, 48, 48, 48, 48, 48, 48,
/*   H    I    J    K    L    M    N    O */
    48, 48, 48, 48, 48, 48, 48, 48,
/*   P    Q    R    S    T    U    V    W */
    48, 48, 48, 48, 48, 48, 48, 48,
/*   X    Y    Z    [    \    ]    ^    _ */
    48, 48, 48, 91, 92, 93, 94, 48,
/*   `    a    b    c    d    e    f    g */
    96, 48, 48, 48, 48, 48, 48, 48,
/*   h    i    j    k    l    m    n    o */
    48, 48, 48, 48, 48, 48, 48, 48,
/*   p    q    r    s    t    u    v    w */
    48, 48, 48, 48, 48, 48, 48, 48,
/*   x    y    z    {    |    }    ~  DEL */
    48, 48, 48, 123, 124, 125, 126, 1,
/* x80  x81  x82  x83  IND  NEL  SSA  ESA */
    1, 1, 1, 1, 1, 1, 1, 1,
/* HTS  HTJ  VTS  PLD  PLU   RI  SS2  SS3 */
    1, 1, 1, 1, 1, 1, 1, 1,
/* DCS  PU1  PU2  STS  CCH   MW  SPA  EPA */
    1, 1, 1, 1, 1, 1, 1, 1,
/* x98  x99  x9A  CSI   ST  OSC   PM  APC */
    1, 1, 1, 1, 1, 1, 1, 1,
/*   -    i   c/    L   ox   Y-    |   So */
    160, 161, 162, 163, 164, 165, 166, 167,
/*  ..   c0   ip   <<    _        R0    - */
    168, 169, 170, 171, 172, 173, 174, 175,
/*   o   +-    2    3    '    u   q|    . */
    176, 177, 178, 179, 180, 181, 182, 183,
/*   ,    1    2   >>  1/4  1/2  3/4    ? */
    184, 185, 186, 187, 188, 189, 190, 191,
/*  A`   A'   A^   A~   A:   Ao   AE   C, */
    48, 48, 48, 48, 48, 48, 48, 48,
/*  E`   E'   E^   E:   I`   I'   I^   I: */
    48, 48, 48, 48, 48, 48, 48, 48,
/*  D-   N~   O`   O'   O^   O~   O:    X */
    48, 48, 48, 48, 48, 48, 48, 216,
/*  O/   U`   U'   U^   U:   Y'    P    B */
    48, 48, 48, 48, 48, 48, 48, 48,
/*  a`   a'   a^   a~   a:   ao   ae   c, */
    48, 48, 48, 48, 48, 48, 48, 48,
/*  e`   e'   e^   e:    i`  i'   i^   i: */
    48, 48, 48, 48, 48, 48, 48, 48,
/*   d   n~   o`   o'   o^   o~   o:   -: */
    48, 48, 48, 48, 48, 48, 48, 248,
/*  o/   u`   u'   u^   u:   y'    P   y: */
48, 48, 48, 48, 48, 48, 48, 48};

static int
set_character_class_range(low, high, value)
    register int low,
                high;		/* in range of [0..255] */
    register int value;		/* arbitrary */
{

    if (low < 0 || high > 255 || high < low)
	return (-1);

    for (; low <= high; low++)
	charClass[low] = value;

    return (0);
}


/*
 * set_character_class - takes a string of the form
 *
 *                 low[-high]:val[,low[-high]:val[...]]
 *
 * and sets the indicated ranges to the indicated values.
 */

static int
set_character_class(s)
    register char *s;
{
    register int i;		/* iterator, index into s */
    int         len;		/* length of s */
    int         acc;		/* accumulator */
    int         low,
                high;		/* bounds of range [0..127] */
    int         base;		/* 8, 10, 16 (octal, decimal, hex) */
    int         numbers;	/* count of numbers per range */
    int         digits;		/* count of digits in a number */
    static char *errfmt = "%s:  %s in range string \"%s\" (position %d)\n";

    if (!s || !s[0])
	return -1;

    base = 10;			/* in case we ever add octal, hex */
    low = high = -1;		/* out of range */

    for (i = 0, len = strlen(s), acc = 0, numbers = digits = 0;
	    i < len; i++) {
	int        c = s[i];

	if (isspace(c)) {
	    continue;
	} else if (isdigit(c)) {
	    acc = acc * base + (c - '0');
	    digits++;
	    continue;
	} else if (c == '-') {
	    low = acc;
	    acc = 0;
	    if (digits == 0) {
		(void)fprintf(stderr, errfmt, progname, "missing number", s, i);
		return (-1);
	    }
	    digits = 0;
	    numbers++;
	    continue;
	} else if (c == ':') {
	    if (numbers == 0)
		low = acc;
	    else if (numbers == 1)
		high = acc;
	    else {
		(void)fprintf(stderr, errfmt, progname, "too many numbers",
			s, i);
		return (-1);
	    }
	    digits = 0;
	    numbers++;
	    acc = 0;
	    continue;
	} else if (c == ',') {
	    /*
	     * now, process it
	     */

	    if (high < 0) {
		high = low;
		numbers++;
	    }
	    if (numbers != 2) {
		(void)fprintf(stderr, errfmt, progname, "bad value number",
			s, i);
	    } else if (set_character_class_range(low, high, acc) != 0) {
		(void)fprintf(stderr, errfmt, progname, "bad range", s, i);
	    }
	    low = high = -1;
	    acc = 0;
	    digits = 0;
	    numbers = 0;
	    continue;
	} else {
	    (void)fprintf(stderr, errfmt, progname, "bad character", s, i);
	    return (-1);
	}			/* end if else if ... else */

    }

    if (low < 0 && high < 0)
	return (0);

    /*
     * now, process it
     */

    if (high < 0)
	high = low;
    if (numbers < 1 || numbers > 2) {
	(void)fprintf(stderr, errfmt, progname, "bad value number", s, i);
    } else if (set_character_class_range(low, high, acc) != 0) {
	(void)fprintf(stderr, errfmt, progname, "bad range", s, i);
    }
    return (0);
}

/*
 */
static void
clear_row_selection(tw, row, left, right)
	TextWindow  tw;
	int         row;
	int         left;
	int         right;
{
	register int	col;

	if (right > tw->cols)
		right = tw->cols;
	if (left < right) {
		tw->line_attr[row] |= LINE_DIRTY;
		for (col = left; col < right; col++) {
			tw->attr[row][col] |= CELL_DIRTY;
			tw->attr[row][col] &= ~CELL_SELECTION;
		}
	}
}

/*
 * Save the selection and clear the highlighting.
 */
static void
save_selection(tw)
	TextWindow  tw;
{
	if (tw->show_selection)
		change_selection(tw, False, True);
}

/*
 * Change the highlighting that indicates a selection (using the 'set'
 * parameter to turn highlighting on or off).  Optionally save the last
 * highlighted selection before modifying the highlighting.
 */
static void
change_selection(tw, set, save)
	TextWindow  tw;
	Bool        set;
	Bool        save;
{
	WINDOW	*wp;
	int     r,
		c,
		start,
		end,
		left;

	tw->show_selection = set;
	if (save) {
		x_stash_selection(tw);
		tw->have_selection = False;
	}
	start = tw->sel_start_col;

	wp = row2window(tw->sel_start_row);

	for (r = tw->sel_start_row; r <= tw->sel_end_row; r++) {
		end = (r == tw->sel_end_row) ? tw->sel_end_col : (tw->cols - 1);
		left = 0;
		if (wp != 0) {
			fast_ptr LINEPTR lp;
			int	row, right, radj;
#ifdef WMDLINEWRAP
			int	next;
#endif

			lp = row2line(wp, r, &row);
			if (!same_ptr(lp, win_head(wp))) {
				radj  = 0;
				left  = nu_width(wp) + w_left_margin(wp);
				right = offs2col(wp, lp, lLength(lp)
							+ w_val(wp,WMDLIST));
#ifdef WMDLINEWRAP
				next = line_height(wp,lp);
				if (w_val(wp, WMDLINEWRAP) && (next > 1)) {
					if (row != r)
						left = 0;
					if ((row + next - 1) == r)
						right -= (radj = tw->cols * (next-1));
					else
						right = tw->cols;
					if (r != row
					 && r != tw->sel_start_row)
						start = left = 0;
				}
#endif
				if (start >= right)
					start = right-1;
				if (end >= right) {
					end = right-1;
					if (end < left)
						end = left;
				}

				if (lLength(lp) > 0) {
					/* test for control-char at start */
					if (r == tw->sel_start_row) {
						int	offs = col2offs(wp,lp,start+radj);
						if (!isprint(lGetc(lp,offs))) {
							start = offs2col(wp,lp,offs-1);
							if (start != w_left_margin(wp))
								start++;
						}
					}

					/* test for control-char at end */
					if (r == tw->sel_end_row) {
						int	offs = col2offs(wp,lp,end+radj);
						if (!isprint(lGetc(lp,offs)))
							end = offs2col(wp,lp,offs+1)-1;
					}
				}
			} else {
				end = left-1;
			}
		}
		if (start < left)
			start = left;
		if (start <= end) {
			clear_row_selection(tw, r, 0, start);
			tw->line_attr[r] |= LINE_DIRTY;
			for (c = start; c <= end; c++) {
				tw->attr[r][c] |= CELL_DIRTY;
				if (set) {
					tw->attr[r][c] |= CELL_SELECTION;
				} else {
					tw->attr[r][c] &= ~CELL_SELECTION;
				}
			}
		} else {
			clear_row_selection(tw, r, 0, end+1);
		}
		clear_row_selection(tw, r, end+1, (int)tw->cols);
		start = left;
	}
}

static void
x_lose_selection(tw)
	TextWindow  tw;
{
	if (tw->have_selection)
		change_selection(tw, False, False);
	tw->have_selection = False;
	free_selection(tw);
	tw->was_on_msgline = False;
	x_flush();			/* show the changes */
}

/*
 * Copy a single character into the paste-buffer, quoting it if necessary
 */
static int
add2paste(p, c)
TBUFF	**p;
int	c;
{
	if (c == '\n' || isblank(c))
		;
	else if (isspecial(c) || (c == '\r') || !isprint(c))
	 	(void)tb_append(p, quotec);
	return (tb_append(p, c) != 0);
}

/*
 * Copy the selection into the PasteBuf buffer.  If we are pasting into a
 * window, check to see if:
 *
 *	+ the window's buffer is modifiable (if not, don't waste time copying
 *	  text!)
 *	+ the buffer uses 'autoindent' mode (if so, trim leading whitespace
 *	  from each line).
 */
static int
copy_paste(p, value, length)
TBUFF	**p;
char	*value;
SIZE_T	length;
{
	WINDOW	*wp = row2window(ttrow);
	BUFFER	*bp = (wp != NULL) ? wp->w_bufp : 0;
	int	status;

	if (bp != 0 && b_val(bp,MDVIEW)) {
		status = FALSE;
	} else {
		status = TRUE;

		if (bp != 0 && (b_val(bp,MDCMOD) || b_val(bp,MDAIND))) {
			register int	trim = TRUE;
			register int	c;

			/*
			 * If the cursor points before the first nonwhite on
			 * the line, convert the insert into an 'O' command. 
			 * If it points to the end of the line, convert it into
			 * an 'o' command.  Otherwise (if it is within the
			 * nonwhite portion of the line), assume the user knows
			 * what (s)he is doing.
			 */
			if (setwmark(ttrow, ttcol)) {	/* MK gets cursor */
				LINE	*lp	= l_ref(MK.l);
				int	first	= -1;
				int	last	= -1;
				int	cmd	= 0;

				for (c = 0; c < llength(lp); c++) {
					if (!isblank(lp->l_text[c])) {
						if (first < 0)
							first = c;
						last = c;
					}
				}
				/* If the line contains only a single nonwhite,
				 * we will insert before it.
				 */
				if (first >= MK.o)
					cmd = -1;
				else if (last <= MK.o)
					cmd = 1;
				if (insertmode) {
					if ((*value != '\n')
					 && (cmd > 1 || (MK.o == 0)))
						(void)tb_append(p, '\n');
				} else if (cmd != 0
					&& (c = insertion_cmd(cmd)) >= 0) {
					*tb_values(*p) = c;
				}
			}

			while (length-- != 0) {
				if ((c = *value++) == '\n')
					trim = TRUE;
				else if (trim && isblank(c))
					continue;
				else
					trim = FALSE;

				if (!add2paste(p, c)) {
					status = FALSE;
					break;
				}
			}
		} else {
			while (length-- > 0) {
				if (!add2paste(p, *value++)) {
					status = FALSE;
					break;
				}
			}
		}
	}
	return status;
}

/* ARGSUSED */
static void
x_get_selection(tw, selection, type, value, length, format)
	TextWindow  tw;
	Atom        selection;
	Atom        type;
	char       *value;
	SIZE_T      length;
	int         format;
{
	int	c, do_ins;

	if (format != 8 || type != XA_STRING)
		return;			/* can't handle incoming data */

	if (length != 0) {
		/* should be impossible to hit this with existing paste */
		/* XXX massive hack -- leave out 'i' if in prompt line */
		do_ins = !insertmode
			&& !onMsgRow(tw)
			&& ((c = insertion_cmd(0)) != -1);

		if (tb_init(&PasteBuf, abortc)) {
			if ((do_ins && !tb_append(&PasteBuf, c))
			 || !copy_paste(&PasteBuf, value, length)
			 || (do_ins && !tb_append(&PasteBuf, abortc)))
				tb_free(&PasteBuf);
		}
	}
#if !defined(DOALLOC) || !defined(DBMALLOC) /* cannot intercept that one */
	free(value);
#endif
}

static void
x_paste_selection(tw)
	TextWindow  tw;
{
	if (tw->have_selection) {
		/* local transfer */
		if (tw->selection_len == 0)	/* stash it if it hasn't been */
			x_stash_selection(tw);
			x_get_selection(tw, XA_PRIMARY, XA_STRING,
				strndup((char *) tw->selection_data, tw->selection_len),
				tw->selection_len, 8);
		} else {
			XConvertSelection(tw->dpy, XA_PRIMARY, XA_STRING,
				tw->sel_prop, tw->win, CurrentTime);
	}
}

static void
x_stash_selection(tw)
	TextWindow tw;
{
	WINDOW	*wp;
	UCHAR	*data;
	UCHAR	*dp;
	int	length;

	if (!tw->have_selection)
		return;
	free_selection(tw);

	if ((wp = row2window(tw->sel_start_row)) != 0) {
		KILL	*kp;		/* pointer into kill register */
		MARK	save;
		int	region_flag = fulllineregions;
		int	report_flag = global_g_val(GVAL_REPORT);
		int	saverow;
		int	savecol;
		int	extend_left = FALSE;
		int	extend_right = FALSE;

		beginDisplay;
		saverow = ttrow;
		savecol = ttcol;

		/*
		 * If the selection is in a non-linewrapped window, extend the
		 * selection to the end of the line whenever it is already on
		 * the end-markers.
		 */
#ifdef WMDLINEWRAP
		if (!w_val(wp,WMDLINEWRAP))
#endif
		{
			if (w_val(wp,WVAL_SIDEWAYS)
			 && (tw->sel_start_col <= nu_width(wp)))
				extend_left = TRUE;
			if (tw->sel_end_col >= tw->cols - 1) {
				(void)setwmark(tw->sel_end_row, tw->sel_end_col);
				if ((nu_width(wp)
				   + lLength(MK.l)
				   - w_val(wp,WVAL_SIDEWAYS)) > tw->cols)
					extend_right = TRUE;
			}
		}

		save = DOT;	/* just in case it isn't start or end */
		(void)setwmark(tw->sel_start_row, tw->sel_start_col);
		swapmark();
		(void)setwmark(tw->sel_end_row,   tw->sel_end_col);

		if (extend_left)
			DOT.o = w_left_margin(curwp);
		if (extend_right)
			MK.o = lLength(MK.l);
		else if (!is_at_end_of_line(MK)
		  && (MK.o != w_left_margin(curwp) || same_ptr(MK.l,DOT.l)))
			MK.o += 1;

		if (x_on_msgline())	/* disable messages? */
			set_global_g_val(GVAL_REPORT,0);

		fulllineregions = FALSE;
		(void)yankregion();
		fulllineregions = region_flag;

		DOT = save;
		if (saverow != ttrow)	/* we showed a message */
			movecursor(saverow, savecol);
		set_global_g_val(GVAL_REPORT,report_flag);
		endofDisplay;

		if (!(length = kchars)
		 || !(dp = data = castalloc(UCHAR, kchars))
		 || !(kp = kbs[0].kbufh))
			return;

		while (kp->d_next != 0) {
			(void)memcpy((char *)dp, (char *)kp->d_chunk, KBLOCK);
			kp = kp->d_next;
			dp += KBLOCK;
		}
		(void)memcpy((char *)dp, (char *)kp->d_chunk, (SIZE_T)(kbs[0].kused));

	} else {	/* must be message-line */
		length = tw->sel_end_col - tw->sel_start_col + 1;
		data = castalloc(UCHAR, length);
		if (!data)
			return;
		(void)memcpy(
			(char *)data,
			(char *) &(tw->sc[tw->sel_start_row][tw->sel_start_col]),
			(SIZE_T)length);
	}
	tw->selection_len  = length;
	tw->selection_data = data;

	/* clear the highlighting */
	change_selection(tw, False, False);
	x_flush();

#ifdef SABER_HACK
	XChangeProperty(dpy, RootWindow(tw->dpy, tw->screen), XA_CUT_BUFFER0,
		XA_STRING, 8, PropModeReplace,
		tw->selection_data, tw->selection_len);
#endif
}

/* ARGSUSED */
static Bool
x_give_selection(tw, ev, target, prop)
	TextWindow  tw;
	XSelectionRequestEvent *ev;
	Atom        target;
	Atom        prop;
{
	if (tw->show_selection)
		x_stash_selection(tw);
	XChangeProperty(dpy, ev->requestor, prop, XA_STRING, 8,
		PropModeReplace, tw->selection_data, tw->selection_len);
	return True;
}

static void
x_own_selection(tw)
	TextWindow  tw;
{
	free_selection(tw);
	if (!tw->have_selection)
		XSetSelectionOwner(tw->dpy, XA_PRIMARY, tw->win, CurrentTime);
	change_selection(tw, True, False);
	x_flush();			/* show the changes */
	tw->have_selection = True;
}

static void
extend_selection(tw, nr, nc, wipe)
	TextWindow  tw;
	int	nr;
	int	nc;
	Bool	wipe;
{
	WINDOW	*wp0, *wp1;

	/* Don't allow selection to go onto the modeline.
	 * XXX Later, modify this to cause autoscrolling.
	 */
	if ((wp0 = row2window(nr)) != 0) {
		if (nr == mode_row(wp0)) {
			nr--;
			nc = tw->cols;
		}
	}

	wp1 = row2window(tw->sel_start_row);

	if (tw->have_selection)	/* erase any old one */
		change_selection(tw, False, False);

	if (wipe) {	/* a wipe is always relative to its starting point */

		/* Don't allow selection to go outside a window */
		if (wp0 != wp1) {
			if (nr > tw->wipe_row) {
				nr = tw->sel_end_row;
				nc = tw->cols;
			} else {
				nr = tw->sel_start_row;
				nc = 0;
			}
		}

		if (nr > tw->wipe_row) {
			tw->sel_end_row = nr;
			tw->sel_end_col = nc;
			tw->sel_start_col = tw->wipe_col;
			tw->sel_start_row = tw->wipe_row;
		} else if (nr == tw->wipe_row) {
			tw->sel_end_row = tw->sel_start_row = nr;
			if (nc > tw->wipe_col) {
				tw->sel_start_col = tw->wipe_col;
				tw->sel_end_col = nc;
			} else {
				tw->sel_start_col = nc;
				tw->sel_end_col = tw->wipe_col;
			}
		} else {
			tw->sel_start_row = nr;
			tw->sel_start_col = nc;
			tw->sel_end_col = tw->wipe_col;
			tw->sel_end_row = tw->wipe_row;
		}
	} else {	/* extend-selection */

		/* Don't allow selection to go outside a window */
		if (wp0 != wp1) {
			if (wp0 != 0) {
				if (wp1 != 0) {
					if (nr < tw->sel_start_row) {
						nr = wp1->w_toprow;
						nc = 0;
					} else {
						nr = mode_row(wp1) - 1;
						nc = tw->cols;
					}
				} else {
					nr = term.t_nrow - 1;
					nc = 0;
				}
			} else {
				nr = mode_row(wp1) - 1;
				nc = tw->cols;
			}
		}

		if (nr < tw->sel_start_row) {
			if (!tw->have_selection)
				tw->sel_end_row = tw->sel_start_row;
			tw->sel_start_row = nr;
		} else {
			tw->sel_end_row = nr;
		}

		if (nc < tw->sel_start_col) {
			if (!tw->have_selection)
				tw->sel_end_col = tw->sel_start_col;
				tw->sel_start_col = nc;
			} else {
				tw->sel_end_col = nc;
		}
	}

	x_own_selection(tw);

#ifdef SABER_HACK
	x_stash_selection(tw);
#endif

	/* Show the extended location on the message line if 'ruler' mode is
	 * enabled.
	 */
	(void)setwmark(nr,nc);
#ifdef WMDRULER
	if (wp1 != 0 && w_val(wp1,WMDRULER) && !x_on_msgline()) {
		int savecol;
		int saverow;
		int saveflg;

		beginDisplay;
		saverow = ttrow;
		savecol = ttcol;
		saveflg = curwp->w_flag;

		drawing_ruler = True;
		swapmark();
		mlforce("%s (%d,%d)\n",
			wipe ? "select" : "extend",
			line_no(curwp->w_bufp, DOT.l),
			getccol(FALSE)+1);
		swapmark();
		curwp->w_flag = saveflg;
		drawing_ruler = False;

		movecursor(saverow, savecol);
		endofDisplay;
	}
#endif
}

static LINEPTR
row2line(wp, r, rowp)
	WINDOW	*wp;
	int	r;
	int	*rowp;
{
	fast_ptr LINEPTR lp;
	register int	row,
			next;

	for (lp = wp->w_line.l, row = wp->w_toprow; ; ) {
		next = line_height(wp,lp);
		if (row+next > r)
			break;
		row += next;
		if (!same_ptr(lp, win_head(wp)))
			lp = lFORW(lp);
	}
	*rowp = row;
	return lp;
}

static void
multi_click(tw, nr, nc)
	TextWindow  tw;
	int         nr;
	int         nc;
{
	WINDOW	*wp;
	LINEPTR	lp;
	int	row;
	UCHAR	*p;
	int	sc;
	int	cclass;

	tw->numclicks++;

	sc = tw->sel_start_col;
	switch (tw->numclicks) {
	case 0:
	case 1:			/* shouldn't happen */
		abort();
	case 2:			/* word */
		/* find word start */
		p = &(tw->sc[nr][sc]);
		cclass = charClass[*p];
		do {
			--sc;
			--p;
		} while (sc >= 0 && charClass[*p] == cclass);
		sc++;
		/* and end */
		p = &(tw->sc[nr][nc]);
		cclass = charClass[*p];
		do {
			++nc;
			++p;
		} while (nc < tw->cols && charClass[*p] == cclass);
		--nc;
		break;
	case 3:			/* line (includes trailing newline) */
		sc = 0;
		if ((wp = row2window(tw->sel_start_row)) != 0) {
			lp = row2line(wp, tw->sel_start_row, &row);
			if (!same_ptr(lp, win_head(wp))) {
				nc = 0;
				tw->sel_start_row = row;
				tw->sel_end_row = row + line_height(wp,lp);
			}
		} else {
			p = tw->sc[tw->rows-1];
			for (nc = tw->cols - 1; nc > 0; nc--)
				if (!isblank(p[nc]))
					break;
		}
		break;
	case 4:			/* screen */
		/* XXX blow off till we can figure out where screen starts & ends */
	default:
		break;
	}
	tw->sel_start_col = sc;
	tw->sel_end_col = nc;
	x_own_selection(tw);

#ifdef SABER_HACK
	x_stash_selection(tw);
#endif
}

static void
start_selection(tw, ev, nr, nc)
	TextWindow  tw;
	XButtonPressedEvent *ev;
	int	nr;
	int	nc;
{
	if ((tw->lasttime != 0)
	 && (absol(ev->time - tw->lasttime) < tw->click_timeout)
	 && (nr == tw->sel_start_row)) { /* ignore extra clicks on other rows */
		multi_click(tw, nr, nc);
	} else {
		int	my_row;
		int	my_col;
		int	saved_selection = tw->have_selection;

		beginDisplay;
		my_row = ttrow;
		my_col = ttcol;

		tw->lasttime = ev->time;
		tw->numclicks = 1;

		if (tw->have_selection)
			change_selection(tw, False, True);

		tw->was_on_msgline = onMsgRow(tw);
		tw->sel_start_row = tw->sel_end_row = tw->wipe_row = nr;
		tw->sel_start_col = tw->sel_end_col = tw->wipe_col = nc;

		/* If we're not on the message-line, update the cursor position
		 * on the screen.  Note that clicking on the modeline causes
		 * the window to scroll up by one line.
		 */
		if (reading_msg_line) {
			;	/* ignore */
		} else if (setcursor(nr, nc)) {
			/* Calling 'refresh()' forces the point to be centered,
			 * but the ensuing 'update()' causes the message-line
			 * to be cleared (including the yank-message from the
			 * previous selection).  So we call 'x_flush()'
			 * directly to get rid of the selection highlighting.
			 */
			if (ev->state)
				(void) refresh(True, 0);
			else
				x_flush();
			(void)update(FALSE);
			if (x_on_msgline()) {
				movecursor(my_row, my_col);
				x_flush();
			} else if (!saved_selection) {
				my_row = ttrow;
				my_col = ttcol;
				mlerase();
				movecursor(my_row,my_col);
				x_flush();
			}
		}
		endofDisplay;
	}
}

static XMotionEvent *
compress_motion(ev)
    XMotionEvent *ev;
{
    XEvent      nev;

    while (XPending(ev->display)) {
	XPeekEvent(ev->display, &nev);
	if (nev.type == MotionNotify &&
		nev.xmotion.window == ev->window &&
		nev.xmotion.subwindow == ev->subwindow) {
	    XNextEvent(ev->display, (XEvent *) ev);
	} else
	    break;
    }
    return ev;
}

/*
 * handle any non keyboard events
 */
static void
x_process_event(ev)
    XEvent     *ev;
{
    int         sc,
                sr;
    unsigned    ec,
                er;

    int         nr,
                nc;
    Bool        changed = False;
    XSelectionEvent event;
    XMotionEvent *mev;
    XExposeEvent *gev;

    switch (ev->type) {
    case SelectionClear:
	x_lose_selection(cur_win);
	break;
    case SelectionRequest:
	event.type = SelectionNotify;
	event.display = ev->xselectionrequest.display;
	event.requestor = ev->xselectionrequest.requestor;
	event.selection = ev->xselectionrequest.selection;
	event.time = ev->xselectionrequest.time;
	event.target = ev->xselectionrequest.target;
	event.target = ev->xselectionrequest.target;
	if (ev->xselectionrequest.property == None)	/* obsolete requestor */
	    ev->xselectionrequest.property = ev->xselectionrequest.target;
	if (x_give_selection(cur_win, (XSelectionRequestEvent *) ev,
			     ev->xselectionrequest.target,
			     ev->xselectionrequest.property)) {
	    event.property = ev->xselectionrequest.property;
	} else {
	    event.property = None;
	}
	(void) XSendEvent(dpy, event.requestor, False, (long) 0,
			  (XEvent *) & event);
	break;
    case SelectionNotify:
	if (ev->xselection.property == None) {
	    x_get_selection(cur_win, ev->xselection.selection,
			    None, (char *) 0, 0, 8);
	} else {
	    ULONG	bytesafter;
	    ULONG	length;
	    int         format;
	    Atom        type;
	    UCHAR	*value;

	    (void) XGetWindowProperty(dpy, cur_win->win,
				      ev->xselection.property,
			  0L, 100000L, False, AnyPropertyType, &type, &format,
				      &length, &bytesafter, &value);
	    XDeleteProperty(dpy, cur_win->win, ev->xselection.property);
	    x_get_selection(cur_win, ev->xselection.selection,
			    type, (char *) value, (SIZE_T)length, format);
	}
	break;

    case Expose:
	gev = (XExposeEvent *)ev;
	sc = gev->x / cur_win->char_width;
	sr = gev->y / cur_win->char_height;
	ec = CEIL(gev->x + gev->width,  cur_win->char_width);
	er = CEIL(gev->y + gev->height, cur_win->char_height);
	x_touch(cur_win, sc, sr, ec, er);
	if (ev->xexpose.count == 0)
		x_flush();
	break;
    case EnterNotify:
    case FocusIn:
	cur_win->show_cursor = True;
	turnOnCursor(cur_win);
	x_flush();
	break;
    case LeaveNotify:
    case FocusOut:
	cur_win->show_cursor = False;
	turnOffCursor(cur_win);
	x_flush();
	break;
    case ConfigureNotify:
	nr = ev->xconfigure.height / cur_win->char_height;
	nc = ev->xconfigure.width  / cur_win->char_width;

	if (nc != cur_win->cols) {
		changed = True;
		newwidth(True, nc);
	}
	if (nr != cur_win->rows) {
		changed = True;
		newlength(True, nr);
	}
	if (changed) {
		x_resize_screen(cur_win, (ALLOC_T)nr, (ALLOC_T)nc);
		(void) refresh(True, 0);
		(void) update(False);
	}
	break;
    case MotionNotify:
	if (ev->xmotion.state != Button1Mask)
	    return;
	mev = compress_motion((XMotionEvent *) ev);
	nc = mev->x / cur_win->char_width;
	nr = mev->y / cur_win->char_height;
	/* bounds check */
	if (nr < 0 || nc < 0 || nr >= cur_win->rows || nc >= cur_win->cols)
	    return;
	/* ignore any spurious motion during a multi-cick */
	if (cur_win->numclicks > 1)
	    return;
	extend_selection(cur_win, nr, nc, True);
	break;
    case ButtonPress:
	nc = ev->xbutton.x / cur_win->char_width;
	nr = ev->xbutton.y / cur_win->char_height;
	switch (ev->xbutton.button) {
	case Button1:		/* move button and set selection point */
	    start_selection(cur_win, (XButtonPressedEvent *) ev, nr, nc);
	    break;
	case Button2:		/* paste selection */
	    if (ev->xbutton.state) {	/* if modifier, paste at mouse */
		if (!setcursor(nr, nc)) {
		    kbd_alarm();	/* don't know how to paste here */
		    break;
		}
	    }
	    x_paste_selection(cur_win);
	    break;
	case Button3:		/* end selection */
	    extend_selection(cur_win, nr, nc, False);
	    cur_win->wipe_row = 0;
	    cur_win->wipe_col = 0;
	    break;
	}
	break;
    }
}

/*
 * Return true if there are characters remaining to be pasted.  This is used in
 * the type-ahead check.
 */
int
x_is_pasting()
{
	return tb_more(PasteBuf);
}

/*
 * Return true if we want to disable reports of the cursor position because the
 * cursor really should be on the message-line.
 */
int
x_on_msgline()
{
	return reading_msg_line || cur_win->was_on_msgline;
}

/*
 * Because we poll our input-characters in 'x_getc()', it is possible to have
 * exposure-events pending while doing lengthy processes (e.g., reading from a
 * pipe).  This procedure is invoked from a timer-handler and is designed to
 * handle the exposure-events, and to get keypress-events (i.e., for stopping a
 * lengthy process).
 */
#if OPT_WORKING
void
x_working()
{
	register TextWindow tw = cur_win;
	XEvent	ev;

	while (XPending(tw->dpy)) {
		if (XCheckTypedEvent(tw->dpy, KeyPress, &ev)) {
			int	num = decoded_key(&ev);
			if (num >= 0) {
				if (num == intrc) {
					(void)tb_init(&PasteBuf, abortc);
#if VMS
					kbd_alarm(); /* signals? */
#else
					(void)signal_pg(SIGINT);
#endif
				} else
					(void)tb_append(&PasteBuf, num);
			}
		} else {
			XNextEvent(tw->dpy, &ev);
			x_process_event(&ev);
		}
	}
}
#endif

/*
 * main event loop.  this means we'll be stuck if an event that needs
 * instant processing comes in while its off doing other work, but
 * there's no (easy) way around that.
 */
static int
x_getc()
{
	XEvent	ev;
	int	num;

	while (1) {

		if (tb_more(PasteBuf))	/* handle any queued pasted text */
			return tb_next(PasteBuf);

		XNextEvent(dpy, &ev);
		if (ev.type == KeyPress) {
			if ((num = decoded_key(&ev)) >= 0) {
				save_selection(cur_win);
				return num;
			}
			/* else, could be shift-key, etc. */
		} else {
			x_process_event(&ev);
		}
	}
}

static int
decoded_key(ev)
	XEvent	*ev;
{
	char	buffer[10];
	KeySym	keysym;
	int	num;

	num = XLookupString((XKeyPressedEvent *) ev, buffer, sizeof(buffer),
		&keysym, (XComposeStatus *) 0);

	switch (keysym) {
	/* Arrow keys */
	case XK_Up:		return SPEC|'A';
	case XK_Down:		return SPEC|'B';
	case XK_Right:		return SPEC|'C';
	case XK_Left:		return SPEC|'D';
	/* page scroll */
	case XK_Next:		return SPEC|'n';
	case XK_Prior:		return SPEC|'p';
	/* editing */
	case XK_Insert:		return SPEC|'i';
#if (ULTRIX || ultrix)
	case DXK_Remove:	return SPEC|'r';
#endif
	case XK_Find:		return SPEC|'f';
	case XK_Select:		return SPEC|'s';
	/* command keys */
	case XK_Menu:		return SPEC|'m';
	case XK_Help:		return SPEC|'h';
	/* function keys */
	case XK_F1:		return SPEC|'1';
	case XK_F2:		return SPEC|'2';
	case XK_F3:		return SPEC|'3';
	case XK_F4:		return SPEC|'4';
	case XK_F5:		return SPEC|'5';
	case XK_F6:		return SPEC|'6';
	case XK_F7:		return SPEC|'7';
	case XK_F8:		return SPEC|'8';
	case XK_F9:		return SPEC|'9';
	case XK_F10:		return SPEC|'0';
	case XK_F11:		return ESC;
	case XK_F12:		return SPEC|'@';
	case XK_F13:		return SPEC|'#';
	case XK_F14:		return SPEC|'$';
	case XK_F15:		return SPEC|'%';
	case XK_F16:		return SPEC|'^';
	case XK_F17:		return SPEC|'&';
	case XK_F18:		return SPEC|'*';
	case XK_F19:		return SPEC|'(';
	case XK_F20:		return SPEC|')';
	/* keypad function keys */
	case XK_KP_F1:		return SPEC|'P';
	case XK_KP_F2:		return SPEC|'Q';
	case XK_KP_F3:		return SPEC|'R';
	case XK_KP_F4:		return SPEC|'S';
	/* ordinary keys */
	default:		return num ? buffer[0] : -1;
	}
}

#if NEEDED
/* ARGSUSED */
static Bool
check_kbd_ev(disp, ev, arg)
    Display    *disp;
    XEvent     *ev;
    char       *arg;
{
    return (ev->type == KeyPress);
}

int
x_key_events_ready()
{
    XEvent      ev;

    /* XXX may want to use another mode */
    if (XEventsQueued(dpy, QueuedAlready))
	return XPeekIfEvent(dpy, &ev, check_kbd_ev, (char *)0);
    return FALSE;
}
#endif

/*
 * change reverse video status
 */
static void
x_rev(state)
    int         state;
{
    cur_win->reverse = state;
}

/* change screen resolution */
/*ARGSUSED*/
static int
x_cres(flag)
char *flag;
{
    return TRUE;
}

#if COLOR
static void
x_fcol(color)
int color;
{
}

static void
x_bcol(color)
int color;
{
}

#endif

/* change palette string */
/* ARGSUSED */
void
spal(dummy)
char *dummy;
{
}

#if OPT_FLASH
static void
invert_display P(( void ))
{
	register int r, c;

	for (r = 0; r < cur_win->rows; r++) {
		cur_win->line_attr[r] |= LINE_DIRTY;
		for (c = 0; c < cur_win->cols; c++) {
			cur_win->attr[r][c] ^= CELL_REVERSE;
			cur_win->attr[r][c] |= CELL_DIRTY;
		}
	}
	x_flush();
}
#endif

/* beep */
static void
x_beep()
{
#if OPT_FLASH
	if (global_g_val(GMDFLASH)) {
		beginDisplay;
		invert_display();
		invert_display();
		endofDisplay;
		return;
	}
#endif
	XBell(cur_win->dpy, 0);
}

#if NO_LEAKS
void
x11_leaks()
{
	if (cur_win != 0) {
		free_selection(cur_win);
		free_win_data(cur_win);
		FreeIfNeeded(cur_win->fontname);
	}
}
#endif

#else
x11hello() {}
#endif				/* X11 */
