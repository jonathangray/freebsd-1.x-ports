/*
 * DVI previewer for X.
 *
 * Eric Cooper, CMU, September 1985.
 *
 * Code derived from dvi-imagen.c.
 *
 * Modification history:
 * 1/1986	Modified for X.10	--Bob Scheifler, MIT LCS.
 * 7/1988	Modified for X.11	--Mark Eichin, MIT
 * 12/1988	Added 'R' option, toolkit, magnifying glass
 *					--Paul Vojta, UC Berkeley.
 * 2/1989	Added tpic support	--Jeffrey Lee, U of Toronto
 * 4/1989	Modified for System V	--Donald Richardson, Clarkson Univ.
 * 3/1990	Added VMS support	--Scott Allendorf, U of Iowa
 * 7/1990	Added reflection mode	--Michael Pak, Hebrew U of Jerusalem
 * 1/1992	Added greyscale code	--Till Brychcy, Techn. Univ. Muenchen
 *					  and Lee Hetherington, MIT
 *
 *	Compilation options:
 *	SYSV	compile for System V
 *	VMS	compile for VMS
 *	X10	compile for X10
 *	NOTOOL	compile without toolkit (X11 only)
 *	BUTTONS	compile with buttons on the side of the window (needs toolkit)
 *	MSBITFIRST	store bitmaps internally with most significant bit first
 *	BMSHORT	store bitmaps in shorts instead of bytes
 *	BMLONG	store bitmaps in longs instead of bytes
 *	ALTFONT	default for -altfont option
 *	A4	use European size paper
 *	TEXXET	support reflection dvi codes (right-to-left typesetting)
 *	GREY	use grey levels to shrink fonts
 */

#ifndef	ALTFONT
#define	ALTFONT	"cmr10"
#endif

#ifndef	A4
#define	DEFAULT_PAPER		"us"
#else
#define	DEFAULT_PAPER		"a4"
#endif

#if	!defined(X10) && !defined(NOTOOL)
#define	TOOLKIT
#else
#undef	TOOLKIT
#undef	BUTTONS
#endif

#include <ctype.h>

#define	EXTERN
#define	INIT(x)	=x
#ifndef	TOOLKIT
#define	NTINIT(x)	=x
#else
#define	NTINIT(x)
#endif
#include "xdvi.h"

#include "patchlevel.h"
static	struct {_Xconst char	a[33], b, c, d;}
#ifndef X10
	version = {"This is xdvi for X11, patchlevel ", '0' + PATCHLEVEL / 10,
		'0' + PATCHLEVEL % 10, 0};
#else
	version = {"This is xdvi for X10, patchlevel ", '0' + PATCHLEVEL / 10,
		'0' + PATCHLEVEL % 10, 0};
#endif

#ifndef	X_NOT_STDC_ENV
#include <stdlib.h>
#endif

#ifndef X10
/* Xlib and Xutil are already included */
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#include "xdvi.icon"
#endif /* X10 */

#ifdef	TOOLKIT
#undef Boolean
#include <X11/Intrinsic.h>
#ifdef OLD_X11_TOOLKIT
#include <X11/Atoms.h>
#else /* not OLD_X11_TOOLKIT */
#include <X11/Xatom.h>
#include <X11/StringDefs.h>
#endif /* not OLD_X11_TOOLKIT */
#include <X11/Shell.h>	/* needed for def. of XtNiconX */
#ifndef	XtSpecificationRelease
#define	XtSpecificationRelease	0
#endif
#if	XtSpecificationRelease >= 4
#include <X11/Xaw/Viewport.h>
#ifdef	BUTTONS
#include <X11/Xaw/Command.h>
#endif
#else	/* XtSpecificationRelease < 4 */
#define	XtPointer caddr_t
#include <X11/Viewport.h>
#ifdef	BUTTONS
#include <X11/Command.h>
#endif
#endif	/* XtSpecificationRelease */
#else	/* !TOOLKIT */
#define	XtNumber(arr)	(sizeof(arr)/sizeof(arr[0]))
typedef	int		Position;
typedef	unsigned int	Dimension;
#ifndef X10
typedef	unsigned long	Pixel;
#define	XtPending()	XPending(DISP)
#else
#define	XtPending	XPending
#define	XMoveResizeWindow XConfigureWindow
#endif
#endif	/* TOOLKIT */

#ifdef	HAS_SIGIO
#include <fcntl.h>
#include <signal.h>
#endif

#ifndef X10
#ifndef	GREY
static	Display	*DISP;
static	Screen	*SCRN;
#endif
#define	DPY	DISP,
static	Cursor	redraw_cursor, ready_cursor;

#ifdef	VMS
/*
 * Magnifying glass cursor
 *
 * Developed by Tom Sawyer, April 1990
 * Contibuted by Hunter Goatley, January 1991
 *
 */

#define mag_glass_width 16
#define mag_glass_height 16
#define mag_glass_x_hot 6
#define mag_glass_y_hot 6
static char mag_glass_bits[] = {
	0xf8, 0x03, 0x0c, 0x06, 0xe2, 0x09, 0x13, 0x1a, 0x01, 0x14, 0x01, 0x14,
	0x01, 0x10, 0x01, 0x10, 0x01, 0x10, 0x03, 0x10, 0x02, 0x18, 0x0c, 0x34,
	0xf8, 0x6f, 0x00, 0xd8, 0x00, 0xb0, 0x00, 0xe0
};
#include <decw$cursor.h>	/* Include the DECWindows cursor symbols */
static	int	DECWCursorFont;	/* Space for the DECWindows cursor font  */
static	Pixmap	MagnifyPixmap;	/* Pixmap to hold our special mag-glass  */
#endif	/* VMS */

#define	SetCursor(x)	XDefineCursor(DISP, WINDOW(mane), x)
#define	ClearPage(wr)	XClearWindow(DISP, WINDOW(wr));
#define	ClearArea(win, x, y, w, h)	XClearArea(DISP, win, x, y, w, h, False)
#define	DarkenArea(win, x, y, w, h) \
			XFillRectangle(DISP, win, ruleGC, x, y, w, h)
#define	CopyArea(win, x, y, w, h, x2, y2) \
			XCopyArea(DISP, win, win, DefaultGCOfScreen(SCRN), \
				x, y, w, h, x2, y2)
#define	Flush()		XFlush(DISP)
#ifndef X11HEIGHT
#define	X11HEIGHT	8	/* Height of server default font */
#endif
#else	/* X10 */
#define	DPY
#define	GC		int
#define	SetCursor(x)
#define	ClearPage(wr)	XClear(WINDOW(wr));
#define	ClearArea(win, x, y, w, h)	XPixSet(win, x, y, w, h, backpix)
#define	DarkenArea(win, x, y, w, h)	XPixSet(win, x, y, w, h, foreGC)
#define	CopyArea(win, x, y, w, h, x2, y2) \
			XMoveArea(win, x, y, x2, y2, w, h, GXcopy);
#define	XBell(a,b)	XFeep(b/10-1)
#define	Flush()		XFlush()
#define	ConnectionNumber(DISP)	(_XlibCurrentDisplay->fd)
#ifndef X10FONT
#define	X10FONT	"helv10b"	/* Font for X10 error messages */
#define	X10HEIGHT	10
#endif
#endif	/* X10 */

#define	MAGBORD	1	/* border size for magnifier */

/*
 * Command line flags.
 */

static	Dimension	bwidth	= 2;

#ifdef	TOOLKIT

#define	RESOURCE(x)	resource.x

static	struct _resource {
	char	*debug_arg;
	int	_shrink_factor;
	int	density;
#ifdef	GREY
	float	gamma;
#endif
	int	pixels_per_inch;
	char	*sidemargin;
	char	*topmargin;
	char	*xoffset;
	char	*yoffset;
	_Xconst char	*paper;
	char	*alt_font;
	Boolean	list_fonts;
	Boolean	reverse;
	Boolean	hush_spec;
	Boolean	hush_chars;
	Pixel	fore_Pixel;
	char	*fore_color;
	Pixel	back_Pixel;
	char	*back_color;
	Pixel	brdr_Pixel;
	char	*brdr_color;
	Pixel	hl_Pixel;
	char	*high_color;
	Pixel	cr_Pixel;
	char	*curs_color;
	char	*icon_geometry;
	Boolean	keep_flag;
	char	*copy_arg;
	Boolean	copy;
	Boolean	thorough;
	Boolean	version_flag;
#ifdef	BUTTONS
	Boolean	expert;
#endif
	int	mg_size[5];
#ifdef	GREY
	Boolean	use_grey;
#endif
} resource;

#else	/* !TOOLKIT */

#define	RESOURCE(x)	x
static	char	*debug_arg;
#ifdef	GREY
static	float	gamma	= 1.0;
#endif
static	char	*sidemargin, *topmargin;
static	char	*xoffset, *yoffset;
static	_Xconst	char	*paper		= DEFAULT_PAPER;
static	Boolean	reverse;
static	Boolean	keep_flag	= False;
#ifndef X10
static	Pixel	fore_Pixel, back_Pixel, brdr_Pixel, hl_Pixel, cr_Pixel;
static	char	*icon_geometry;
static	Boolean	copy	= 2;
static	Boolean	thorough;
#endif	/* X10 */
static	Boolean	version_flag	= False;
static	int	mg_size[5]	= {200, 350, 600, 900, 1200};

#endif	/* TOOLKIT */

static	char	*curr_page;

#ifndef	TOOLKIT
static	char	*fore_color;
static	char	*back_color;
static	char	*brdr_color;
static	char	*high_color;
static	char	*curs_color;
#endif
static	GC	foreGC, highGC;
#ifndef X10
static	GC	ruleGC;
static	GC	foreGC2;
#else	/* X10 */
#define	ruleGC	foreGC
#endif	/* X10 */

static	int	pageno_correct	= 1;
static	int	bak_shrink;

#define	clip_w	mane.width
#define	clip_h	mane.height
static	Dimension	window_w, window_h;
#ifndef X10
static	Position	main_x, main_y;
static	XImage	*image;
static	int	backing_store;
#else	/* X10 */
#define	main_x	0
#define	main_y	0
static	int	GXfunc;
static	int	backpix, backmap, bdrmap;
/*
 * Cursor and mask for valid cursor
 */
#include "xdvi_curs.h"
#include "xdvi_mask.h"
#endif	/* X10 */

static	Position mag_x, mag_y, new_mag_x, new_mag_y;
static	Boolean	mag_moved = False;
static	int	home_x, home_y;
static	int	min_x, max_x, min_y, max_y;

struct WindowRec mane	= {(Window) 0, 3, 0, 0, 0, 0, MAXDIM, 0, MAXDIM, 0};
struct WindowRec alt	= {(Window) 0, 1, 0, 0, 0, 0, MAXDIM, 0, MAXDIM, 0};
/*	currwin is temporary storage except for within redraw() */
struct WindowRec currwin = {(Window) 0, 3, 0, 0, 0, 0, MAXDIM, 0, MAXDIM, 0};

#ifdef	TOOLKIT
static	Widget	top_level, vport_widget, draw_widget, clip_widget;
#ifdef	BUTTONS
static	Widget	form_widget, line_widget, panel_widget;
#endif
static	Widget	x_bar, y_bar;	/* horizontal and vertical scroll bars */

static	Arg	vport_args[] = {
#ifdef	BUTTONS
	{XtNborderWidth, (XtArgVal) 0},
	{XtNtop,	(XtArgVal) XtChainTop},
	{XtNbottom,	(XtArgVal) XtChainBottom},
	{XtNleft,	(XtArgVal) XtChainLeft},
	{XtNright,	(XtArgVal) XtChainRight},
#endif
	{XtNallowHoriz,	(XtArgVal) True},
	{XtNallowVert,	(XtArgVal) True},
};

/*	Note:  Argument order in the following is important! */

static	Arg	draw_args[] = {
	{XtNwidth,	(XtArgVal) 0},
	{XtNheight,	(XtArgVal) 0},
#ifdef	GREY
	{XtNbackground,	(XtArgVal) 0},
#endif
	{XtNx,		(XtArgVal) 0},
	{XtNy,		(XtArgVal) 0},
	{XtNlabel,	(XtArgVal) ""},
};

#ifdef	BUTTONS
static	Arg	form_args[] = {
	{XtNdefaultDistance, (XtArgVal) 0},
};
#define	XTRA_WID	79

static	Arg	line_args[] = {
	{XtNbackground,	(XtArgVal) 0},
	{XtNwidth,	(XtArgVal) 1},
	{XtNheight,	(XtArgVal) 0},
	{XtNfromHoriz,	(XtArgVal) NULL},
	{XtNborderWidth, (XtArgVal) 0},
	{XtNtop,	(XtArgVal) XtChainTop},
	{XtNbottom,	(XtArgVal) XtChainBottom},
	{XtNleft,	(XtArgVal) XtChainRight},
	{XtNright,	(XtArgVal) XtChainRight},
};

static	Arg	panel_args[] = {
	{XtNfromHoriz,	(XtArgVal) NULL},
	{XtNwidth,	(XtArgVal) (XTRA_WID - 1)},
	{XtNheight,	(XtArgVal) 0},
	{XtNborderWidth, (XtArgVal) 0},
	{XtNtop,	(XtArgVal) XtChainTop},
	{XtNbottom,	(XtArgVal) XtChainBottom},
	{XtNleft,	(XtArgVal) XtChainRight},
	{XtNright,	(XtArgVal) XtChainRight},
};

static	struct {
	_Xconst	char	*label;
	_Xconst	char	*name;
	int	closure;
	int	y_pos;
	}
	command_table[] = {
		{"Quit",	"quit",		'q',		50},
		{"Shrink1",	"sh1",		1 << 8 | 's',	150},
		{"Shrink2",	"sh2",		2 << 8 | 's',	200},
		{"Shrink3",	"sh3",		3 << 8 | 's',	250},
		{"Shrink4",	"sh4",		4 << 8 | 's',	300},
		{"Page-10",	"prev10",	10 << 8 | 'p',	400},
		{"Page-5",	"prev5",	5 << 8 | 'p',	450},
		{"Prev",	"prev",		'p',		500},
		{"Next",	"next",		'n',		600},
		{"Page+5",	"next5",	5 << 8 | 'n',	650},
		{"Page+10",	"next10",	10 << 8 | 'n',	700},
};

static	void	handle_command();

static	XtCallbackRec	command_call[] = {
	{handle_command, NULL},
	{NULL,		NULL},
};

static	Arg	command_args[] = {
	{XtNlabel,	(XtArgVal) NULL},
	{XtNx,		(XtArgVal) 6},
	{XtNy,		(XtArgVal) 0},
	{XtNwidth,	(XtArgVal) 64},
	{XtNheight,	(XtArgVal) 30},
	{XtNcallback,	(XtArgVal) command_call},
};

static	void
create_buttons(h)
	XtArgVal	h;
{
	int i;

	line_args[2].value = h;
	line_args[3].value = (XtArgVal) vport_widget;
	line_widget = XtCreateManagedWidget("line", widgetClass, form_widget,
		line_args, XtNumber(line_args));
	panel_args[0].value = (XtArgVal) line_widget;
	panel_args[2].value = h;
	panel_widget = XtCreateManagedWidget("panel", compositeWidgetClass,
		form_widget, panel_args, XtNumber(panel_args));

	command_args[2].value = (XtArgVal) vport_widget;
	for (i = 0; i < XtNumber(command_table); ++i) {
	    command_args[0].value = (XtArgVal) command_table[i].label;
	    command_args[2].value = (XtArgVal) command_table[i].y_pos;
	    command_call[0].closure = (caddr_t) command_table[i].closure;
	    (void) XtCreateManagedWidget(command_table[i].name,
		commandWidgetClass, panel_widget,
		command_args, XtNumber(command_args));
	}
}
#endif	/* BUTTONS */

#ifdef	NOQUERY
#define	drawWidgetClass	widgetClass
#else

/* ARGSUSED */
static	XtGeometryResult
QueryGeometry(w, constraints, reply)
	Widget	w;
	XtWidgetGeometry *constraints, *reply;
{
	reply->request_mode = CWWidth | CWHeight;
	reply->width = page_w;
	reply->height = page_h;
	return XtGeometryAlmost;
}

#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>

	/* if the following gives you trouble, just compile with -DNOQUERY */
static	WidgetClassRec	drawingWidgetClass = {
  {
    /* superclass         */    &widgetClassRec,
    /* class_name         */    "Draw",
    /* widget_size        */    sizeof(WidgetRec),
    /* class_initialize   */    NULL,
    /* class_part_initialize*/  NULL,
    /* class_inited       */    FALSE,
    /* initialize         */    NULL,
    /* initialize_hook    */    NULL,
    /* realize            */    XtInheritRealize,
    /* actions            */    NULL,
    /* num_actions        */    0,
    /* resources          */    NULL,
    /* num_resources      */    0,
    /* xrm_class          */    NULLQUARK,
    /* compress_motion    */    FALSE,
    /* compress_exposure  */    TRUE,
    /* compress_enterleave*/    FALSE,
    /* visible_interest   */    FALSE,
    /* destroy            */    NULL,
    /* resize             */    XtInheritResize,
    /* expose             */    XtInheritExpose,
    /* set_values         */    NULL,
    /* set_values_hook    */    NULL,
    /* set_values_almost  */    XtInheritSetValuesAlmost,
    /* get_values_hook    */    NULL,
    /* accept_focus       */    XtInheritAcceptFocus,
    /* version            */    XtVersion,
    /* callback_offsets   */    NULL,
    /* tm_table           */    XtInheritTranslations,
    /* query_geometry       */  QueryGeometry,
    /* display_accelerator  */  XtInheritDisplayAccelerator,
    /* extension            */  NULL
  }
};

#define	drawWidgetClass	&drawingWidgetClass

#endif	/* NOQUERY */

#else	/* !TOOLKIT */
#define	BAR_WID		12	/* width of darkened area */
#define	BAR_THICK	15	/* gross amount removed */

static	Window	top_level;
static	Window	x_bar, y_bar;
static	int	x_bgn, x_end, y_bgn, y_end;	/* scrollbar positions */
#endif	/* TOOLKIT */

/*
 *	Mechanism to keep track of the magnifier window.  The problems are,
 *	(a) if the button is released while the window is being drawn, this
 *	could cause an X error if we continue drawing in it after it is
 *	destroyed, and
 *	(b) creating and destroying the window too quickly confuses the window
 *	manager, which is avoided by waiting for an expose event before
 *	destroying it.
 */
static	short	alt_stat;	/* 1 = wait for expose, */
				/* -1 = destroy upon expose */
static	Boolean	alt_canit;	/* stop drawing this window */

/*
 *	Data for buffered events.
 */

static	Boolean	canit		= False,
		has_arg		= False;
static	VOLATILE short	event_counter	= 0;
static	VOLATILE short	event_freq	= 70;
static	int	number		= 0,
		sign		= 1;
static	jmp_buf	canit_env;

static	void	can_exposures(), read_events(), keystroke();

#ifdef	lint
#ifndef	X10
char	xdvi_bits[288];
#ifdef	TOOLKIT
WidgetClass	viewportWidgetClass, widgetClass;
WidgetClassRec	widgetClassRec;
#ifdef	BUTTONS
WidgetClass	formWidgetClass, compositeWidgetClass, commandWidgetClass;
#endif	/* BUTTONS */
#endif	/* TOOLKIT */
#else	/* X10 */
short	xdvi_bits[15], xdvi_mask_bits[15];
Display	*_XlibCurrentDisplay;
#endif	/* X10 */
#endif	/* lint */

#ifdef	GREY
static	void
init_pix(warn)
	Boolean	warn;
{
	static	int	shrink_allocated_for = 0;
	static	Boolean	colors_allocated = False;
	int	i;

	if (!colors_allocated)
	{
	    Pixel plane_masks[4];
	    Pixel pixel;
	    XColor color, fc, bc;
	    XGCValues	values;

	    if (RESOURCE(gamma) == 0.0) RESOURCE(gamma) = 1.0;

	    if (!RESOURCE(copy))
		/* allocate 4 color planes for 16 colors (for GXor drawing) */
		if (!XAllocColorCells(DISP, DefaultColormapOfScreen(SCRN),
					  False, plane_masks, 4, &pixel, 1))
		    RESOURCE(copy) = warn = True;

	    /* get foreground and background RGB values for interpolating */
	    fc.pixel = RESOURCE(fore_Pixel);
	    XQueryColor(DISP, DefaultColormapOfScreen(SCRN), &fc);
	    bc.pixel = RESOURCE(back_Pixel);
	    XQueryColor(DISP, DefaultColormapOfScreen(SCRN), &bc);

	    for (i = 0; i < 16; ++i) {
		double	pow();
		double	frac = RESOURCE(gamma) > 0 ?
		    pow((double) i / 15, 1 / RESOURCE(gamma))
		    : 1 - pow((double) (15 - i) / 15, -RESOURCE(gamma));

		color.red = frac * ((double) fc.red - bc.red) + bc.red;
		color.green = frac * ((double) fc.green - bc.green) + bc.green;
		color.blue = frac * ((double) fc.blue - bc.blue) + bc.blue;

		color.pixel = pixel;
		color.flags = DoRed | DoGreen | DoBlue;

		if (!RESOURCE(copy)) {
		    if (i & 1) color.pixel |= plane_masks[0];
		    if (i & 2) color.pixel |= plane_masks[1];
		    if (i & 4) color.pixel |= plane_masks[2];
		    if (i & 8) color.pixel |= plane_masks[3];
		    XStoreColor(DISP, DefaultColormapOfScreen(SCRN), &color);
		    palette[i] = color.pixel;
		}
		else {
		    if (!XAllocColor(DISP, DefaultColormapOfScreen(SCRN),
			&color))
			palette[i] = (i * 100 >= density * 15)
			    ? RESOURCE(fore_Pixel) : RESOURCE(back_Pixel);
		    else
			palette[i] = color.pixel;
		}
	    }

	    /* Make sure fore_ and back_Pixel are a part of the palette */
	    RESOURCE(fore_Pixel) = palette[15];
	    RESOURCE(back_Pixel) = palette[0];
	    if (WINDOW(mane) != (Window) 0)
		XSetWindowBackground(DISP, WINDOW(mane), palette[0]);

#define	MakeGC(fcn, fg, bg)	(values.function = fcn, values.foreground=fg,\
		values.background=bg,\
		XCreateGC(DISP, RootWindowOfScreen(SCRN),\
			GCFunction|GCForeground|GCBackground, &values))

	    foreGC = ruleGC = MakeGC(RESOURCE(copy) ? GXcopy : GXor,
		RESOURCE(fore_Pixel), RESOURCE(back_Pixel));
	    foreGC2 = NULL;

	    colors_allocated = True;
	    if (RESOURCE(copy) && warn)
		Puts("Note:  overstrike characters may be incorrect.");
	}
#undef	MakeGC

	if (mane.shrinkfactor == 1) return;

	if (shrink_allocated_for < mane.shrinkfactor) {
	    if (pixeltbl != NULL) free((char *) pixeltbl);
	    pixeltbl = (Pixel *) xmalloc((unsigned)
		(mane.shrinkfactor * mane.shrinkfactor + 1) * sizeof(Pixel),
		"pixel table");
	    shrink_allocated_for = mane.shrinkfactor;
	}

	for (i = 0; i <= mane.shrinkfactor * mane.shrinkfactor; ++i)
	    pixeltbl[i] =
		palette[(i * 30 + mane.shrinkfactor * mane.shrinkfactor)
		    / (2 * mane.shrinkfactor * mane.shrinkfactor)];
}
#endif	/* GREY */

#ifdef	sun
extern	char	*sprintf();
#endif

#ifndef	atof	/* on the Next it's a macro */
extern	double	atof();
#endif

/********************************
 *	  tpic routines		*
 *******************************/

/* Things we need from spec_draw, unfortunately */

/* (ignored for now)
extern int pen_size, blacken, whiten, shade;
*/

#define	toint(x)	((int) ((x) + 0.5))
#define	xconv(x)	(toint(specialConv*(x))/shrink_factor + PXL_H)
#define	yconv(y)	(toint(specialConv*(y))/shrink_factor + PXL_V)

/*
 *	Draw a line from (fx,fy) to (tx,ty).
 *	Right now, we ignore pen_size.
 */
void
line_btw(fx, fy, tx, ty)
int fx, fy, tx, ty;
{
	register int	fcx = xconv(fx),
			tcx = xconv(tx),
			fcy = yconv(fy),
			tcy = yconv(ty);

	if ((fcx < max_x || tcx < max_x) && (fcx >= min_x || tcx >= min_x) &&
	    (fcy < max_y || tcy < max_y) && (fcy >= min_y || tcy >= min_y))
#ifndef X10
		XDrawLine(DISP, WINDOW(currwin), ruleGC,
		    fcx - currwin.base_x, fcy - currwin.base_y,
		    tcx - currwin.base_x, tcy - currwin.base_y);
#else
		XLine(WINDOW(currwin),
		    fcx - currwin.base_x, fcy - currwin.base_y,
		    tcx - currwin.base_x, tcy - currwin.base_y,
		    1, 1, ruleGC, GXcopy, AllPlanes);
#endif
}

/*
 *	Draw a dot at (x,y)
 */
void
dot_at(x, y)
	int	x, y;
{
	register int	cx = xconv(x),
			cy = yconv(y);

	if (cx < max_x && cx >= min_x && cy < max_y && cy >= min_y)
#ifndef X10
	    XDrawPoint(DISP, WINDOW(currwin), ruleGC,
		cx - currwin.base_x, cy - currwin.base_y);
#else
	    XPixSet(WINDOW(currwin), cx - currwin.base_x, cy - currwin.base_y,
		1, 1, ruleGC);
#endif
}

/*
 *	Apply the requested attributes to the last path (box) drawn.
 *	Attributes are reset.
 *	(Not currently implemented.)
 */
	/* ARGSUSED */
void
do_attribute_path(last_min_x, last_max_x, last_min_y, last_max_y)
int last_min_x, last_max_x, last_min_y, last_max_y;
{
}

/*
 *	Put a rectangle on the screen.  hl determines the GC.
 */

void
put_rectangle(x, y, w, h, hl)
	int x, y, w, h;
	WIDEARG(Boolean, int) hl;
{
	if (x < max_x && x + w >= min_x && y < max_y && y + h >= min_y) {
		if (--event_counter == 0) read_events(False);
#ifndef X10
		XFillRectangle(DISP, WINDOW(currwin), hl ? highGC : ruleGC,
		    x - currwin.base_x, y - currwin.base_y,
		    w ? w : 1, h ? h : 1);
#else
		XPixSet(WINDOW(currwin), x - currwin.base_x, y - currwin.base_y,
		    w ? w : 1, h ? h : 1, hl ? highGC : ruleGC);
#endif
	}
}

void
put_bitmap(bitmap, x, y)
	register struct bitmap *bitmap;
	register int x, y;
{

	if (debug & DBG_BITMAP)
		Printf("X(%d,%d)\n", x - currwin.base_x, y - currwin.base_y);
	if (x < max_x && x + bitmap->w >= min_x &&
	    y < max_y && y + bitmap->h >= min_y) {
		if (--event_counter == 0) read_events(False);
#ifndef X10
		image->width = bitmap->w;
		image->height = bitmap->h;
		image->data = bitmap->bits;
		image->bytes_per_line = bitmap->bytes_wide;
		XPutImage(DISP, WINDOW(currwin), foreGC, image,
			0, 0,
			x - currwin.base_x, y - currwin.base_y,
			bitmap->w, bitmap->h);
		if (foreGC2)
		    XPutImage(DISP, WINDOW(currwin), foreGC2, image,
			0, 0,
			x - currwin.base_x, y - currwin.base_y,
			bitmap->w, bitmap->h);
#else
		XBitmapBitsPut(WINDOW(currwin),
			x - currwin.base_x, y - currwin.base_y,
			bitmap->w, bitmap->h, bitmap->bits,
			foreGC, backpix, NULL, GXfunc, AllPlanes);
#endif
	}
}

#ifdef	GREY
void
put_image(img, x, y)
	register XImage *img;
	register int x, y;
{
	if (x < max_x && x + img->width >= min_x &&
	    y < max_y && y + img->height >= min_y) {

	    if (--event_counter == 0) read_events (False);

	    XPutImage(DISP, WINDOW(currwin), foreGC, img,
	    	    0, 0,
		    x - currwin.base_x, y - currwin.base_y,
		    img->width, img->height);
	}
}
#endif	/* GREY */

/*
 *	Event-handling routines
 */

static	void
expose(windowrec, x, y, w, h)
	register struct WindowRec *windowrec;
	int	x, y, w, h;
{
	if (windowrec->min_x > x) windowrec->min_x = x;
	if (windowrec->max_x < x + w)
	    windowrec->max_x = x + w;
	if (windowrec->min_y > y) windowrec->min_y = y;
	if (windowrec->max_y < y + h)
	    windowrec->max_y = y + h;
}

static	void
clearexpose(windowrec, x, y, w, h)
	struct WindowRec *windowrec;
	int	x, y, w, h;
{
	ClearArea(WINDOW(*windowrec), x, y, w, h);
	expose(windowrec, x, y, w, h);
}

static	void
scrollwindow(windowrec, x0, y0)
	register struct WindowRec *windowrec;
	int	x0, y0;
{
	int	x, y;
	int	x2 = 0, y2 = 0;
	int	ww, hh;

	x = x0 - windowrec->base_x;
	y = y0 - windowrec->base_y;
	ww = windowrec->width - x;
	hh = windowrec->height - y;
	windowrec->base_x = x0;
	windowrec->base_y = y0;
	if (currwin.win == windowrec->win) {
	    currwin.base_x = x0;
	    currwin.base_y = y0;
	}
	windowrec->min_x -= x;
	if (windowrec->min_x < 0) windowrec->min_x = 0;
	windowrec->max_x -= x;
	if (windowrec->max_x > windowrec->width)
	    windowrec->max_x = windowrec->width;
	windowrec->min_y -= y;
	if (windowrec->min_y < 0) windowrec->min_y = 0;
	windowrec->max_y -= y;
	if (windowrec->max_y > windowrec->height)
	    windowrec->max_y = windowrec->height;
	if (x < 0) {
	    x2 = -x;
	    x = 0;
	    ww = windowrec->width - x2;
	}
	if (y < 0) {
	    y2 = -y;
	    y = 0;
	    hh = windowrec->height - y2;
	}
	if (ww <= 0 || hh <= 0) {
	    ClearPage(*windowrec);
	    windowrec->min_x = windowrec->min_y = 0;
	    windowrec->max_x = windowrec->width;
	    windowrec->max_y = windowrec->height;
	}
	else {
	    CopyArea(WINDOW(*windowrec), x, y, ww, hh, x2, y2);
	    if (x > 0) clearexpose(windowrec, ww, 0, x, windowrec->height);
	    if (x2 > 0) clearexpose(windowrec, 0, 0, x2, windowrec->height);
	    if (y > 0) clearexpose(windowrec, 0, hh, windowrec->width, y);
	    if (y2 > 0) clearexpose(windowrec, 0, 0, windowrec->width, y2);
	}
}

#ifdef	TOOLKIT
/*
 *	routines for X11 toolkit
 */

static	Arg	arg_wh[] = {
	{XtNwidth,	(XtArgVal) &window_w},
	{XtNheight,	(XtArgVal) &window_h},
};

static	Position	window_x, window_y;
static	Arg	arg_xy[] = {
	{XtNx,		(XtArgVal) &window_x},
	{XtNy,		(XtArgVal) &window_y},
};

#define	get_xy()	XtGetValues(draw_widget, arg_xy, XtNumber(arg_xy))

#define	mane_base_x	0
#define	mane_base_y	0

static	void
home(scrl)
	Boolean	scrl;
{
	if (!scrl) XUnmapWindow(DISP, WINDOW(mane));
	get_xy();
	if (x_bar != NULL) {
	    register int coord = (page_w - clip_w) / 2;
	    if (coord > home_x / mane.shrinkfactor)
		coord = home_x / mane.shrinkfactor;
	    XtCallCallbacks(x_bar, XtNscrollProc,
		(XtPointer) (window_x + coord));
	}
	if (y_bar != NULL) {
	    register int coord = (page_h - clip_h) / 2;
	    if (coord > home_y / mane.shrinkfactor)
		coord = home_y / mane.shrinkfactor;
	    XtCallCallbacks(y_bar, XtNscrollProc,
		(XtPointer) (window_y + coord));
	}
	if (!scrl) {
	    XMapWindow(DISP, WINDOW(mane));
	    /* Wait for the server to catch up---this eliminates flicker. */
	    XSync(DISP, False);
	}
}

static	Boolean	resized	= False;

static	void
get_geom()
{
	static	Dimension	new_clip_w, new_clip_h;
	static	Arg	arg_wh_clip[] = {
		{XtNwidth,	(XtArgVal) &new_clip_w},
		{XtNheight,	(XtArgVal) &new_clip_h},
	};
	register int	old_clip_w;

	XtGetValues(vport_widget, arg_wh, XtNumber(arg_wh));
	XtGetValues(clip_widget, arg_wh_clip, XtNumber(arg_wh_clip));
	/* Note:  widgets may be destroyed but not forgotten */
	x_bar = page_w <= new_clip_w ? NULL
	    : XtNameToWidget(vport_widget, "horizontal");
	y_bar = page_h <= new_clip_h ? NULL
	    : XtNameToWidget(vport_widget, "vertical");
	old_clip_w = clip_w;
			/* we need to do this because */
			/* sizeof(Dimension) != sizeof(int) */
	clip_w = new_clip_w;
	clip_h = new_clip_h;
	if (old_clip_w == 0) home(False);
	resized = False;
}

static	void
center(x, y)
	int x, y;
{
/*	We use the clip widget here because it gives a more exact value. */
	x -= clip_w/2;
	y -= clip_h/2;
	if (x_bar) XtCallCallbacks(x_bar, XtNscrollProc, (XtPointer) x);
	if (y_bar) XtCallCallbacks(y_bar, XtNscrollProc, (XtPointer) y);
	XWarpPointer(DISP, None, None, 0, 0, 0, 0, -x, -y);
}

/*
 *	callback routines
 */

/* The following callback routine should never be called. */
	/*ARGSUSED*/
static	void
handle_key(widget, junk, event, cont)
	Widget	widget;
	XtPointer junk;
	XEvent	*event;
	Boolean	*cont;		/* unused */
{
	XBell(DISP, 20);
}

	/*ARGSUSED*/
static	void
handle_resize(widget, junk, event, cont)
	Widget	widget;
	XtPointer junk;
	XEvent	*event;
	Boolean	*cont;		/* unused */
{
	resized = True;
}

#ifdef	BUTTONS
	/*ARGSUSED*/
static	void
handle_command(widget, client_data, call_data)
	Widget	widget;
	XtPointer client_data;
	XtPointer call_data;
{
	int	int_client_data	= (int) client_data;	/* Apollo cc bug */

	keystroke((int_client_data) & 0xff, (int_client_data) >> 8,
		((int_client_data) >> 8) != 0, (XEvent *) NULL);
}
#endif	/* BUTTONS */

void
reconfig()
{
	draw_args[0].value = (XtArgVal) page_w;
	draw_args[1].value = (XtArgVal) page_h;
	XtSetValues(draw_widget, draw_args, (Cardinal) 2);
	get_geom();
}

#else	/* !TOOLKIT */

/*
 *	brute force scrollbar routines
 */

static	void
paint_x_bar()
{
	register int	new_x_bgn = mane.base_x * clip_w / page_w;
	register int	new_x_end = (mane.base_x + clip_w) * clip_w / page_w;

	if (new_x_bgn >= x_end || x_bgn >= new_x_end) {	/* no overlap */
	    ClearArea(x_bar, x_bgn, 1, x_end - x_bgn, BAR_WID);
	    DarkenArea(x_bar, new_x_bgn, 1, new_x_end - new_x_bgn, BAR_WID);
	}
	else {		/* this stuff avoids flicker */
	    if (x_bgn < new_x_bgn)
		ClearArea(x_bar, x_bgn, 1, new_x_bgn - x_bgn, BAR_WID);
	    else
		DarkenArea(x_bar, new_x_bgn, 1, x_bgn - new_x_bgn, BAR_WID);
	    if (new_x_end < x_end)
		ClearArea(x_bar, new_x_end, 1, x_end - new_x_end, BAR_WID);
	    else
		DarkenArea(x_bar, x_end, 1, new_x_end - x_end, BAR_WID);
	}
	x_bgn = new_x_bgn;
	x_end = new_x_end;
}

static	void
paint_y_bar()
{
	register int	new_y_bgn = mane.base_y * clip_h / page_h;
	register int	new_y_end = (mane.base_y + clip_h) * clip_h / page_h;

	if (new_y_bgn >= y_end || y_bgn >= new_y_end) {	/* no overlap */
	    ClearArea(y_bar, 1, y_bgn, BAR_WID, y_end - y_bgn);
	    DarkenArea(y_bar, 1, new_y_bgn, BAR_WID, new_y_end - new_y_bgn);
	}
	else {		/* this stuff avoids flicker */
	    if (y_bgn < new_y_bgn)
		ClearArea(y_bar, 1, y_bgn, BAR_WID, new_y_bgn - y_bgn);
	    else
		DarkenArea(y_bar, 1, new_y_bgn, BAR_WID, y_bgn - new_y_bgn);
	    if (new_y_end < y_end)
		ClearArea(y_bar, 1, new_y_end, BAR_WID, y_end - new_y_end);
	    else
		DarkenArea(y_bar, 1, y_end, BAR_WID, new_y_end - y_end);
	}
	y_bgn = new_y_bgn;
	y_end = new_y_end;
}

static	void
scrollmane(x, y)
	int	x, y;
{
	register int	old_base_x = mane.base_x;
	register int	old_base_y = mane.base_y;
	if (x > page_w - clip_w) x = page_w - clip_w;
	if (x < 0) x = 0;
	if (y > page_h - clip_h) y = page_h - clip_h;
	if (y < 0) y = 0;
	scrollwindow(&mane, x, y);
	if (old_base_x != mane.base_x && x_bar) paint_x_bar();
	if (old_base_y != mane.base_y && y_bar) paint_y_bar();
}

void
reconfig()
{
	int	x_thick = 0;
	int	y_thick = 0;
#ifdef	X10
	int	old_clip_w = clip_w;
	int	old_clip_h = clip_h;
	int	old_x_thick = x_thick;
	int	old_y_thick = y_thick;
#endif

		/* determine existence of scrollbars */
	if (window_w < page_w) x_thick = BAR_THICK;
	if (window_h - x_thick < page_h) y_thick = BAR_THICK;
	clip_w = window_w - y_thick;
	if (clip_w < page_w) x_thick = BAR_THICK;
	clip_h = window_h - x_thick;

		/* process drawing (clip) window */
	if (mane.win == NULL) {	/* initial creation */
#ifndef X10
	    XWindowAttributes attrs;

	    mane.win = XCreateSimpleWindow(DISP, top_level, y_thick, x_thick,
			(unsigned int) clip_w, (unsigned int) clip_h, 0,
			brdr_Pixel, back_Pixel);
	    XSelectInput(DISP, WINDOW(mane), ExposureMask |
			ButtonPressMask | ButtonMotionMask | ButtonReleaseMask);
	    XGetWindowAttributes(DISP, WINDOW(mane), &attrs);
	    backing_store = attrs.backing_store;
#else
	    mane.win = XCreateWindow(top_level, y_thick, x_thick,
			clip_w, clip_h, 0, bdrmap, backmap);
	    XSelectInput(WINDOW(mane),  ExposeRegion | ExposeCopy |
			ButtonPressed | ButtonReleased |
			LeftDownMotion | MiddleDownMotion | RightDownMotion);
#endif
	    XMapWindow(DPY WINDOW(mane));
	}
	else
#ifdef	X10
	if (clip_w != old_clip_w || clip_h != old_clip_h ||
		x_thick != old_x_thick || y_thick != old_y_thick) {
#endif
	    XMoveResizeWindow(DPY WINDOW(mane),
		y_thick, x_thick, clip_w, clip_h);
#ifdef	X10
	    XSync(False);
	}
#endif

		/* process scroll bars */
	if (x_thick) {
	    if (x_bar) {
		XMoveResizeWindow(DPY x_bar,
		    y_thick - 1, -1, clip_w, BAR_THICK - 1);
		paint_x_bar();
	    }
	    else {
#ifndef X10
		x_bar = XCreateSimpleWindow(DISP, top_level, y_thick - 1, -1,
				(unsigned int) clip_w, BAR_THICK - 1, 1,
				brdr_Pixel, back_Pixel);
		XSelectInput(DISP, x_bar,
			ExposureMask | ButtonPressMask | Button2MotionMask);
#else
		x_bar = XCreateWindow(top_level,
				y_thick - 1, -1, clip_w, BAR_THICK - 1, 1,
				bdrmap, backmap);
		XSelectInput(x_bar,
			ExposeRegion | ButtonPressed | MiddleDownMotion);
#endif
		XMapWindow(DPY x_bar);
	    }
	    x_bgn = mane.base_x * clip_w / page_w;
	    x_end = (mane.base_x + clip_w) * clip_w / page_w;
	}
	else
	    if (x_bar) {
		XDestroyWindow(DPY x_bar);
		x_bar = NULL;
	    }

	if (y_thick) {
	    if (y_bar) {
		XMoveResizeWindow(DPY y_bar,
		    -1, x_thick - 1, BAR_THICK - 1, clip_h);
		paint_y_bar();
	    }
	    else {
#ifndef X10
		y_bar = XCreateSimpleWindow(DISP, top_level, -1, x_thick - 1,
				BAR_THICK - 1, (unsigned int) clip_h, 1,
				brdr_Pixel, back_Pixel);
		XSelectInput(DISP, y_bar,
			ExposureMask | ButtonPressMask | Button2MotionMask);
#else
		y_bar = XCreateWindow(top_level,
				-1, x_thick - 1, BAR_THICK - 1, clip_h, 1,
				bdrmap, backmap);
		XSelectInput(y_bar,
			ExposeRegion | ButtonPressed | MiddleDownMotion);
#endif
		XMapWindow(DPY y_bar);
	    }
	    y_bgn = mane.base_y * clip_h / page_h;
	    y_end = (mane.base_y + clip_h) * clip_h / page_h;
	}
	else
	    if (y_bar) {
		XDestroyWindow(DPY y_bar);
		y_bar = NULL;
	    }
}

static	void
home(scrl)
	Boolean	scrl;
{
	int	x = 0, y = 0;

	if (page_w > clip_w) {
	    x = (page_w - clip_w) / 2;
	    if (x > home_x / mane.shrinkfactor)
		x = home_x / mane.shrinkfactor;
	}
	if (page_h > clip_h) {
	    y = (page_h - clip_h) / 2;
	    if (y > home_y / mane.shrinkfactor)
		y = home_y / mane.shrinkfactor;
	}
	if (scrl)
	    scrollmane(x, y);
	else {
	    mane.base_x = x;
	    mane.base_y = y;
	    if (currwin.win == mane.win) {
		currwin.base_x = x;
		currwin.base_y = y;
	    }
	    if (x_bar) paint_x_bar();
	    if (y_bar) paint_y_bar();
	}
}

#define	get_xy()
#define	window_x 0
#define	window_y 0
#define	mane_base_x	mane.base_x
#define	mane_base_y	mane.base_y
#endif	/* TOOLKIT */

static	void
compute_mag_pos(xp, yp)
	int	*xp, *yp;
{
	register int t;

	t = mag_x + main_x - alt.width/2;
#ifndef X10
	if (t > WidthOfScreen(SCRN) - alt.width - 2*MAGBORD)
	    t = WidthOfScreen(SCRN) - alt.width - 2*MAGBORD;
#else
	if (t > (int) window_w - alt.width - 2*MAGBORD)
	    t = window_w - alt.width - 2*MAGBORD;
#endif
	if (t < 0) t = 0;
	*xp = t;
	t = mag_y + main_y - alt.height/2;
#ifndef X10
	if (t > HeightOfScreen(SCRN) - alt.height - 2*MAGBORD)
	    t = HeightOfScreen(SCRN) - alt.height - 2*MAGBORD;
#else
	if (t > (int) window_h - alt.height - 2*MAGBORD)
	    t = window_h - alt.height - 2*MAGBORD;
#endif
	if (t < 0) t = 0;
	*yp = t;
}

#ifdef	TOOLKIT
	/*ARGSUSED*/
static	void
handle_button(widget, junk, ev, cont)
	Widget	widget;
	XtPointer junk;
	XEvent *ev;
#define	event	(&(ev->xbutton))
	Boolean	*cont;		/* unused */
#else	/* !TOOLKIT */
static	void
handle_button(event)
	XButtonEvent *event;
#endif	/* TOOLKIT */
{
	int	x, y;
#ifndef X10
	int	w	= RESOURCE(mg_size[event->button - 1]);
	XSetWindowAttributes attr;
#else
	int	w	= RESOURCE(mg_size[2 - (event->detail & ValueMask)]);
#endif

	if (alt.win != (Window) 0 || mane.shrinkfactor == 1 || w <= 0)
	    XBell(DISP, 20);
	else {
	    mag_x = event->x;
	    mag_y = event->y;
	    alt.width = alt.height = w;
#ifndef X10
	    main_x = event->x_root - mag_x;
	    main_y = event->y_root - mag_y;
#endif
	    compute_mag_pos(&x, &y);
	    alt.base_x = (event->x + mane_base_x) * mane.shrinkfactor -
		alt.width/2;
	    alt.base_y = (event->y + mane_base_y) * mane.shrinkfactor -
		alt.height/2;
#ifndef X10
	    attr.save_under = True;
	    attr.border_pixel = RESOURCE(brdr_Pixel);
	    attr.background_pixel = RESOURCE(back_Pixel);
	    attr.override_redirect = True;
	    alt.win = XCreateWindow(DISP, RootWindowOfScreen(SCRN),
			x, y, alt.width, alt.height, MAGBORD,
			0,	/* depth from parent */
			InputOutput, CopyFromParent,
			CWSaveUnder | CWBorderPixel | CWBackPixel |
			CWOverrideRedirect, &attr);
	    XSelectInput(DISP, WINDOW(alt), ExposureMask);
#else
	    alt.win = XCreateWindow(WINDOW(mane),
			x, y, alt.width, alt.height, MAGBORD,
			bdrmap, backmap);
	    XSelectInput(WINDOW(alt), ExposeRegion);
#endif
	    XMapWindow(DPY WINDOW(alt));
	    alt_stat = 1;	/* waiting for exposure */
	}
}

#ifdef	TOOLKIT
#undef	event

	/*ARGSUSED*/
static	void
handle_motion(widget, junk, ev, cont)
	Widget	widget;
	XtPointer junk;
	XEvent *ev;
#define	event	(&(ev->xmotion))
	Boolean	*cont;		/* unused */
{
	new_mag_x = event->x;
	main_x = event->x_root - new_mag_x;
	new_mag_y = event->y;
	main_y = event->y_root - new_mag_y;
	mag_moved = (new_mag_x != mag_x || new_mag_y != mag_y);
}

#undef	event
#endif	/* TOOLKIT */

static	void
movemag(x, y)
	int	x, y;
{
	int	xx, yy;

	mag_x = x;
	mag_y = y;
	if (mag_x == new_mag_x && mag_y == new_mag_y) mag_moved = False;
	compute_mag_pos(&xx, &yy);
	XMoveWindow(DPY WINDOW(alt), xx, yy);
	scrollwindow(&alt, (x + mane_base_x) * mane.shrinkfactor - alt.width/2,
	    (y + mane_base_y) * mane.shrinkfactor - alt.height/2);
}

#ifdef	TOOLKIT
	/*ARGSUSED*/
static	void
handle_release(widget, junk, ev, cont)
	Widget	widget;
	XtPointer junk;
	XEvent *ev;
#define	event	(&(ev->xbutton))
	Boolean	*cont;		/* unused */
#else	/* !TOOLKIT */
static	void
handle_release()
#endif	/* TOOLKIT */
{
	if (alt.win)
	    if (alt_stat) alt_stat = -1;	/* destroy upon expose */
	    else {
		XDestroyWindow(DPY WINDOW(alt));
		if (currwin.win == alt.win) alt_canit = True;
		alt.win = (Window) 0;
		mag_moved = False;
		can_exposures(&alt);
	    }
}

#ifdef	TOOLKIT
#undef	event

	/*ARGSUSED*/
static	void
handle_exp(widget, closure, ev, cont)
	Widget	widget;
	XtPointer closure;
	register XEvent *ev;
#define	event	(&(ev->xexpose))
	Boolean	*cont;		/* unused */
{
	struct WindowRec *windowrec = (struct WindowRec *) closure;

	if (windowrec == &alt)
	    if (alt_stat < 0) {	/* destroy upon exposure */
		alt_stat = 0;
		handle_release(widget, (caddr_t) NULL, ev, (Boolean *) NULL);
		return;
	    }
	    else
		alt_stat = 0;
	expose(windowrec, event->x, event->y, event->width, event->height);
}

#undef	event
#endif	/* TOOLKIT */

/* |||
 *	Currently the event handler does not coordinate XCopyArea requests
 *	with GraphicsExpose events.  This can lead to problems if the window
 *	is partially obscured and one, for example, drags a scrollbar.
 */

#ifndef X10
#define	XKEY(ev)	(ev).xkey
#ifndef	TOOLKIT
#define	XANY(ev)	(ev).xany
#define	XCONFIG(ev)	(ev).xconfigure
#define	XEXPOSE(ev)	(ev).xexpose
#define	XMOTION(ev)	(ev).xmotion
#define	XBUTTON(ev)	(ev).xbutton
#define	ISEXPOSE(ev)	((ev).type == Expose)
#endif	/* TOOLKIT */
#else	/* X10 */
#define	XANY(ev)	(ev)
#define	XCONFIG(ev)	(*((XExposeEvent *) &(ev)))
#define	XEXPOSE(ev)	(*((XExposeEvent *) &(ev)))
#define	XMOTION(ev)	(*((XMouseMovedEvent *) &(ev)))
#define	XBUTTON(ev)	(*((XButtonEvent *) &(ev)))
#define	XKEY(ev)	(*((XKeyEvent *) &(ev)))
#define	ConfigureNotify	ExposeWindow
#define	Expose		ExposeRegion
#define	ISEXPOSE(ev)	((ev).type == ExposeWindow || (ev).type == ExposeRegion)
#define	MotionNotify	MouseMoved
#define	ButtonPress	ButtonPressed
#define	ButtonRelease	ButtonReleased
#define	KeyPress	KeyPressed
#endif	/* X10 */

static	void
keystroke(ch, number0, arg0, eventp)
	char	ch;
	int	number0;
	Boolean	arg0;
	XEvent	*eventp;
{
	int	next_page;
#ifdef	TOOLKIT
	Window	ww;
#endif

	next_page = current_page;
	switch (ch) {
	    case 'q':
	    case '\003':	/* control-C */
	    case '\004':	/* control-D */
#ifdef	VMS
	    case '\032':	/* control-Z */
#endif
		exit(0);
	    case 'n':
	    case 'f':
	    case ' ':
	    case '\r':
	    case '\n':
		/* scroll forward; i.e. go to relative page */
		next_page = current_page + (arg0 ? number0 : 1);
		break;
	    case 'p':
	    case 'b':
	    case '\b':
	    case '\177':	/* Del */
		/* scroll backward */
		next_page = current_page - (arg0 ? number0 : 1);
		break;
	    case 'g':
		/* go to absolute page */
		next_page = (arg0 ? number0 - pageno_correct :
		    total_pages - 1);
		break;
	    case 'P':		/* declare current page */
		pageno_correct = arg0 * number0 - current_page;
		return;
	    case 'k':		/* toggle keep-position flag */
		RESOURCE(keep_flag) = (arg0 ? number0 : !RESOURCE(keep_flag));
		return;
	    case '\f':
		/* redisplay current page */
		break;
	    case '^':
		home(True);
		return;
#ifdef	TOOLKIT
	    case 'l':
		if (!x_bar) goto bad;
		XtCallCallbacks(x_bar, XtNscrollProc,
		    (XtPointer) (-2 * (int) clip_w / 3));
		return;
	    case 'r':
		if (!x_bar) goto bad;
		XtCallCallbacks(x_bar, XtNscrollProc,
		    (XtPointer) (2 * (int) clip_w / 3));
		return;
	    case 'u':
		if (!y_bar) goto bad;
		XtCallCallbacks(y_bar, XtNscrollProc,
		    (XtPointer) (-2 * (int) clip_h / 3));
		return;
	    case 'd':
		if (!y_bar) goto bad;
		XtCallCallbacks(y_bar, XtNscrollProc,
		    (XtPointer) (2 * (int) clip_h / 3));
		return;
	    case 'c':
		center(eventp->xkey.x, eventp->xkey.y);
		return;
	    case 'M':
		XTranslateCoordinates(DISP, eventp->xkey.window,
			WINDOW(mane), eventp->xkey.x, eventp->xkey.y,
			&home_x, &home_y, &ww);	/* throw away last argument */
		home_x *= mane.shrinkfactor;
		home_y *= mane.shrinkfactor;
		return;
#ifdef	BUTTONS
	    case 'x':
		if (arg0 && resource.expert == (number0 != 0)) return;
		if (resource.expert) {	/* create buttons */
		    XtResizeWidget(vport_widget, window_w -= XTRA_WID, window_h,
			0);
		    create_buttons((XtArgVal) window_h);
		    resource.expert = False;
		}
		else {		/* destroy buttons */
		    XtResizeWidget(vport_widget, window_w += XTRA_WID, window_h,
			0);
		    XtDestroyWidget(panel_widget);
		    XtDestroyWidget(line_widget);
		    resource.expert = True;
		}
		return;
#endif	/* BUTTONS */
#else	/* !TOOLKIT */
	    case 'l':
		if (mane.base_x <= 0) goto bad;
		scrollmane(mane.base_x - 2 * clip_w / 3, mane.base_y);
		return;
	    case 'r':
		if (mane.base_x >= page_w - clip_w) goto bad;
		scrollmane(mane.base_x + 2 * clip_w / 3, mane.base_y);
		return;
	    case 'u':
		if (mane.base_y <= 0) goto bad;
		scrollmane(mane.base_x, mane.base_y - 2 * clip_h / 3);
		return;
	    case 'd':
		if (mane.base_y >= page_h - clip_h) goto bad;
		scrollmane(mane.base_x, mane.base_y + 2 * clip_h / 3);
		return;
	    case 'c':	/* unchecked scrollmane() */
		scrollwindow(&mane, mane.base_x + XKEY(*eventp).x - clip_w/2,
		    mane.base_y + XKEY(*eventp).y - clip_h/2);
		if (x_bar) paint_x_bar();
		if (y_bar) paint_y_bar();
#ifndef X10
		XWarpPointer(DISP, None, None, 0, 0, 0, 0,
		    clip_w/2 - XKEY(*eventp).x, clip_h/2 - XKEY(*eventp).y);
#else
		XWarpMouse(WINDOW(mane), clip_w/2, clip_h/2, GXcopy);
#endif
		return;
	    case 'M':
		home_x = (XKEY(*eventp).x - (y_bar ? BAR_THICK : 0)
		    + mane.base_x) * mane.shrinkfactor;
		home_y = (XKEY(*eventp).y - (x_bar ? BAR_THICK : 0)
		    + mane.base_y) * mane.shrinkfactor;
		return;
#endif	/* TOOLKIT */

#ifndef X10
	    case '\020':	/* Control P */
		Printf("Unit = %d, bitord = %d, byteord = %d\n",
		    BitmapUnit(DISP), BitmapBitOrder(DISP),
		    ImageByteOrder(DISP));
		return;
#endif
	    case 's':
		if (!arg0) {
		    int temp;
		    number0 = ROUNDUP(unshrunk_page_w, window_w - 2);
		    temp = ROUNDUP(unshrunk_page_h, window_h - 2);
		    if (number0 < temp) number0 = temp;
		}
		if (number0 <= 0) goto bad;
		if (number0 == mane.shrinkfactor) return;
		mane.shrinkfactor = number0;
		init_page();
		if (number0 != 1 && number0 != bak_shrink) {
		    bak_shrink = number0;
#ifdef	GREY
		    if (use_grey) init_pix(False);
#endif
		    reset_fonts();
		}
		reconfig();
		home(False);
		break;
	    case 'S':
		if (!arg0) goto bad;
		if (number0 < 0) goto bad;
		if (number0 == density) return;
		density = number0;
		reset_fonts();
		if (mane.shrinkfactor == 1) return;
		break;
#ifdef GREY
	    case 'G':
		use_grey = (arg0 ? number0 : !use_grey);
		if (use_grey) init_pix(False);
		reset_fonts();
		break;
#endif
	    case 'R':
		/* reread DVI file */
		--dvi_time;	/* then it will notice a change */
		break;
	    default:
		goto bad;
	}
	if (0 <= next_page && next_page < total_pages) {
	    if (current_page != next_page) {
		current_page = next_page;
		hush_spec_now = hush_spec;
		if (!RESOURCE(keep_flag)) home(False);
	    }
	    canit = True;
	    Flush();
	    return;	/* don't use longjmp here:  it might be called from
			 * within the toolkit, and we don't want to longjmp out
			 * of Xt routines. */
	}
	bad:  XBell(DISP, 10);
}


#ifndef X10
#define	TRSIZE	100
#endif	/* X10 */

static	void
read_events(wait)
	Boolean	wait;
{
	char	ch;
	Boolean	arg0;
	int	number0;
	XEvent	event;
#ifndef X10
	char	trbuf[TRSIZE];
#endif
	char	*string;
	int	nbytes;

	alt_canit = False;
	for (;;) {
	    ch = '\0';
	    event_counter = event_freq;
	    /*
	     * The above line clears the flag indicating that an event is
	     * pending.  So if an event comes in right now, the flag will be
	     * set again needlessly, but we just end up making an extra call.
	     * Also, watch out, if we destroy the magnifying glass while
	     * writing it.
	     */
	    if (!XtPending() && (!wait || canit || mane.min_x < MAXDIM ||
		    alt.min_x < MAXDIM || mag_moved))
		if (!wait && (canit || alt_canit)) longjmp(canit_env, 1);
		else return;
#ifdef	TOOLKIT
	    XtNextEvent(&event);
	    if (resized) get_geom();
	    if (event.xany.window == WINDOW(alt) &&
		    event.type == Expose) {
		handle_exp((Widget) NULL, (XtPointer) &alt, &event,
		    (Boolean *) NULL);
		continue;
	    }
	    if (event.type != KeyPress) {
		XtDispatchEvent(&event);
		continue;
	    }
	    string = trbuf;
	    nbytes = XLookupString(&event.xkey, string, TRSIZE, NULL, NULL);
	    if (nbytes > 1) ch = '?';
	    if (nbytes != 0) ch = *string;
#else	/* !TOOLKIT */

	    XNextEvent(DPY &event);
	    if (XANY(event).window == WINDOW(mane) ||
		XANY(event).window == WINDOW(alt)) {

		struct WindowRec *wr = &mane;

		if (XANY(event).window == WINDOW(alt)) {
		    wr = &alt;
		    /* check in case we already destroyed the window */
		    if (alt_stat < 0) { /* destroy upon exposure */
			alt_stat = 0;
			handle_release();
			continue;
		    }
		    else
			alt_stat = 0;
		}
		switch (event.type) {
#ifndef X10
		case GraphicsExpose:
#else
		case ExposeWindow:
#endif
		case Expose:
#ifdef X10
		    if (XEXPOSE(event).detail & ExposeCopy)
			ClearArea(event.window,
			    XEXPOSE(event).x, XEXPOSE(event).y,
			    XEXPOSE(event).width, XEXPOSE(event).height);
#endif
		    expose(wr, XEXPOSE(event).x, XEXPOSE(event).y,
			XEXPOSE(event).width, XEXPOSE(event).height);
#ifdef X10
		case ExposeCopy:	/* throw away junk event */
#endif
		    break;

		case MotionNotify:
#ifdef X10
		case LeftDownMotion:
		case MiddleDownMotion:
		case RightDownMotion:
#endif
		    new_mag_x = XMOTION(event).x;
		    new_mag_y = XMOTION(event).y;
		    mag_moved = (new_mag_x != mag_x || new_mag_y != mag_y);
		    break;

		case ButtonPress:
		    handle_button(&XBUTTON(event));
		    break;

		case ButtonRelease:
		    handle_release();
		    break;
		}	/* end switch */
	    }	/* end if window == {mane,alt}.win */

	    else if (XANY(event).window == x_bar) {
		if (ISEXPOSE(event))
		    DarkenArea(x_bar, x_bgn, 1, x_end - x_bgn, BAR_WID);
		else if (event.type == MotionNotify)
		    scrollmane(XMOTION(event).x * page_w / clip_w,
			mane.base_y);
#ifndef X10
		else switch (XBUTTON(event).button)
#else
		else if (event.type == ButtonPress)
		    switch (3 - (XBUTTON(event).detail & ValueMask))
#endif
		{
		    case 1:
			scrollmane(mane.base_x + XBUTTON(event).x, mane.base_y);
			break;
		    case 2:
			scrollmane(XBUTTON(event).x * page_w / clip_w,
			    mane.base_y);
			break;
		    case 3:
			scrollmane(mane.base_x - XBUTTON(event).x, mane.base_y);
		}
	    }

	    else if (XANY(event).window == y_bar) {
		if (ISEXPOSE(event))
		    DarkenArea(y_bar, 1, y_bgn, BAR_WID, y_end - y_bgn);
		else if (event.type == MotionNotify)
		    scrollmane(mane.base_x,
			XMOTION(event).y * page_h / clip_h);
#ifndef X10
		else switch (XBUTTON(event).button)
#else
		else if (event.type == ButtonPress)
		    switch (3 - (XBUTTON(event).detail & ValueMask))
#endif
		{
		    case 1:
			scrollmane(mane.base_x, mane.base_y + XBUTTON(event).y);
			break;
		    case 2:
			scrollmane(mane.base_x,
			    XBUTTON(event).y * page_h / clip_h);
			break;
		    case 3:
			scrollmane(mane.base_x, mane.base_y - XBUTTON(event).y);
		}
	    }

	    else if (XANY(event).window == top_level)
		switch (event.type) {
		case ConfigureNotify:
		    if (XANY(event).window == top_level &&
			(XCONFIG(event).width != window_w ||
			XCONFIG(event).height != window_h)) {
			    register Window old_mane_win = mane.win;

			    window_w = XCONFIG(event).width;
			    window_h = XCONFIG(event).height;
			    reconfig();
			    if (old_mane_win == NULL) home(False);
		    }
		    break;

#ifndef X10
		case MapNotify:		/* if running w/o WM */
		    if (mane.win == NULL) {
			reconfig();
			home(False);
		    }
		    break;
#endif

		case KeyPress:
#ifndef X10
		    string = trbuf;
		    nbytes = XLookupString(&event.xkey, string, TRSIZE, NULL,
			NULL);
#else
		    string = XLookupMapping(&event, &nbytes);
#endif
		    if (nbytes > 1) ch = '?';
		    if (nbytes != 0) ch = *string;
		    break;
		}
#endif	/* TOOLKIT */
	    if (ch == '\0') continue;
	    if (ch >= '0' && ch <= '9') {
		has_arg = True;
		number = number * 10 + sign * (ch - '0');
		continue;
	    }
	    else if (ch == '-') {
		has_arg = True;
		sign = -1;
		number = 0;
		continue;
	    }
	    number0 = number;
	    number = 0;
	    sign = 1;
	    arg0 = has_arg;
	    has_arg = False;
	    keystroke(ch, number0, arg0, &event);
	}
}

static	void
redraw(windowrec)
	struct WindowRec *windowrec;
{
	char	*errtext;
#ifdef X10
	static FontInfo *font = 0;
#endif

	currwin = *windowrec;
	min_x = currwin.min_x + currwin.base_x;
	min_y = currwin.min_y + currwin.base_y;
	max_x = currwin.max_x + currwin.base_x;
	max_y = currwin.max_y + currwin.base_y;
	can_exposures(windowrec);

	if (debug & DBG_EVENT)
	    Printf("Redraw %d x %d at (%d, %d) (base=%d,%d)\n", max_x - min_x,
		max_y - min_y, min_x, min_y, currwin.base_x, currwin.base_y);
	SetCursor(redraw_cursor);
	Flush();
	if ((errtext = (char *) setjmp(dvi_env)) != NULL) {
	    ClearPage(mane);
#ifndef X10
	    get_xy();
	    XDrawString(DISP, WINDOW(mane), foreGC,
		5 - window_x, 5 + X11HEIGHT - window_y,
		errtext, strlen(errtext));
#else
	    if (!font) font = XOpenFont(X10FONT);
	    XTextMask(WINDOW(mane), 5, 5 + X10HEIGHT, errtext, strlen(errtext),
		font->id, foreGC);
#endif
	    if (dvi_file) {
		Fclose(dvi_file);
		dvi_file = NULL;
	    }
	}
	else {
	    draw_page();
	    hush_spec_now = True;
	}
}

void
redraw_page()
{
	if (debug & DBG_EVENT) Fputs("Redraw page:  ", stdout);
	ClearPage(mane);
#ifndef	X10
	if (backing_store != NotUseful) {
	    mane.min_x = mane.min_y = 0;
	    mane.max_x = page_w;
	    mane.max_y = page_h;
	}
	else {
#endif
	    get_xy();
	    mane.min_x = -window_x;
	    mane.max_x = -window_x + clip_w;
	    mane.min_y = -window_y;
	    mane.max_y = -window_y + clip_h;
#ifndef	X10
	}
#endif
	redraw(&mane);
}

/*
 *	Interrupt system for receiving events.  The program sets a flag
 *	whenever an event comes in, so that at the proper time (i.e., when
 *	reading a new dvi item), we can check incoming events to see if we
 *	still want to go on printing this page.  This way, one can stop
 *	displaying a page if it is about to be erased anyway.  We try to read
 *	as many events as possible before doing anything and base the next
 *	action on all events read.
 *	Note that the Xlib and Xt routines are not reentrant, so the most we
 *	can do is set a flag in the interrupt routine and check it later.
 *	Also, sometimes the interrupts are not generated (some systems only
 *	guarantee that SIGIO is generated for terminal files, and on the system
 *	I use, the interrupts are not generated if I use "(xdvi foo &)" instead
 *	of "xdvi foo").  Therefore, there is also a mechanism to check the
 *	event queue every 70 drawing operations or so.  This mechanism is
 *	disabled if it turns out that the interrupts do work.
 *	For a fuller discussion of some of the above, see xlife in
 *	comp.sources.x.
 */

static	void
can_exposures(windowrec)
	struct WindowRec *windowrec;
{
	windowrec->min_x = windowrec->min_y = MAXDIM;
	windowrec->max_x = windowrec->max_y = 0;
}

#ifdef	HAS_SIGIO
static	void
handle_intr() {
	event_counter = 1;
	event_freq = -1;	/* forget Plan B */
}

static	void
enable_intr() {
	int	socket	= ConnectionNumber(DISP);
	if (!isatty(0)) {
	    Puts("trying...");
	    if (dup2(socket, 0) == -1) perror(prog);
	    socket = 0;
	}
	(void) signal(SIGIO, handle_intr);
	(void) fcntl(socket, F_SETOWN, getpid());
	(void) fcntl(socket, F_SETFL, fcntl(socket, F_GETFL, 0) | FASYNC);
}
#endif	/* HAS_SIGIO */

static	void
do_pages()
{
	if (debug & DBG_BATCH) {
#ifdef	TOOLKIT
	    while (mane.min_x == MAXDIM) read_events(True);
#else	/* !TOOLKIT */
	    while (mane.min_x == MAXDIM)
		if (setjmp(canit_env)) break;
		else read_events(True);
#endif	/* TOOLKIT */
	    for (current_page = 0; current_page < total_pages; ++current_page)
		redraw_page();
	}
	else {	/* normal operation */
#ifdef	HAS_SIGIO
	    enable_intr();
#endif
#ifdef	__convex__
	    /* convex C turns off optimization for the entire function
	       if setjmp return value is discarded.*/
	    if (setjmp(canit_env))	/*optimize me*/;
#else
	    (void) setjmp(canit_env);
#endif
	    for (;;) {
		if (mane.win) SetCursor(ready_cursor);
		read_events(True);
		if (canit) {
		    canit = False;
		    can_exposures(&mane);
		    can_exposures(&alt);
		    redraw_page();
		}
		else if (mag_moved) {
		    if (alt.win == (Window) 0) mag_moved = False;
		    else if (abs(new_mag_x - mag_x) >
			2 * abs(new_mag_y - mag_y))
			    movemag(new_mag_x, mag_y);
		    else if (abs(new_mag_y - mag_y) >
			2 * abs(new_mag_x - mag_x))
			    movemag(mag_x, new_mag_y);
		    else movemag(new_mag_x, new_mag_y);
		}
		else if (alt.min_x < MAXDIM) redraw(&alt);
		else if (mane.min_x < MAXDIM) redraw(&mane);
		Flush();
	    }
	}
}

static	NORETURN void
usage() {
#ifndef X10
#ifndef	VMS
#ifdef	BUTTONS
	Fputs("\
Usage: xdvi [+[<page>]] [-s <shrink>] [-S <density>] [-p <pixels>] [-l] [-rv]\n\
	[-expert] [-paper <papertype>] [-mgs[n] <size>] [-altfont <font>]\n\
	[-margins <dimen>] [-sidemargin <dimen>] [-topmargin <dimen>]\n\
	[-offsets <dimen>] [-xoffset <dimen>] [-yoffset <dimen>] [-keep]\n\
	[-hushspecials] [-hushchars] [-hush] [-nogrey] [-gamma <g>] \
[-version]\n\
	[-fg <color>] [-bg <color>] [-hl <color>] [-bd <color>] \
[-cr <color>]\n\
	[-bw <width>] [-geometry <geometry>] [-icongeometry <geometry>]\n\
	[-iconic] [-display <host:display>] [-copy] [-thorough] dvi_file\n",
	stderr);
#else	/* !BUTTONS */
	Fputs("\
Usage: xdvi [+[<page>]] [-s <shrink>] [-S <density>] [-p <pixels>] [-l] [-rv]\n\
	[-paper <papertype>] [-mgs[n] <size>] [-altfont <font>]\n\
	[-margins <dimen>] [-sidemargin <dimen>] [-topmargin <dimen>]\n\
	[-offsets <dimen>] [-xoffset <dimen>] [-yoffset <dimen>] [-keep]\n\
	[-hushspecials] [-hushchars] [-hush] [-nogrey] [-gamma <g>] \
[-version]\n\
	[-fg <color>] [-bg <color>] [-hl <color>] [-bd <color>] \
[-cr <color>]\n\
	[-bw <width>] [-geometry <geometry>] [-icongeometry <geometry>]\n\
	[-iconic] [-display <host:display>] [-copy] [-thorough] dvi_file\n",
	stderr);
#endif	/* BUTTONS */
#else	/* VMS */
	Fputs("\
Usage: xdvi [+[<page>]] [-s <shrink>] [-density <%>] [-p <pixels>] [-l] [-rv]\n\
	[-paper <papertype>] [-mgs[n] <size>] [-altfont <font>]\n\
	[-margins <dimen>] [-sidemargin <dimen>] [-topmargin <dimen>]\
\n", stderr);
	Fputs("\
	[-offsets <dimen>] [-xoffset <dimen>] [-yoffset <dimen>] [-keep]\n\
	[-hushspecials] [-hushchars] [-hush] [-nogrey] [-gamma <g>] \
[-version]\n\
	[-fg <color>] [-bg <color>] [-hl <color>] [-bd <color>] \
[-cr <color>]\n\
	[-bw <width>] [-geometry <geometry>] [-icongeometry <geometry>]\n\
	[-iconic] [-display <host::display>] [-copy] [-thorough] dvi_file\n",
	stderr);
#endif	/* VMS */
#else	/* X10 */
	Fputs("\
Usage: xdvi [+[<page>]] [-s <shrink>] [-S <density>] [-p <pixels>] [-l]\n\
	[-paper <papertype>] [-mgs[n] <size>] [-altfont <font>]\n\
	[-margins <dimen>] [-sidemargin <dimen>] [-topmargin <dimen>]\n\
	[-offsets <dimen>] [-xoffset <dimen>] [-yoffset <dimen>] [-keep]\n\
	[-hushspecials] [-hushchars] [-hush] [-version]\n\
	[-fg <color>] [-bg <color>] [-hl <color>] [-bd <color>] \
[-cr <color>]\n\
	[-bw <width>] [-geometry <geometry> | =<geometry>]\n\
	[-display <host:display> | <host:display>] dvi_file\n", stderr);
#endif	/* X10 */
	exit(1);
}

static	int
atopix(arg)
	_Xconst	char	*arg;
{
	int	len	= strlen(arg);

	return (len > 2 && arg[len - 2] == 'c' && arg[len - 1] == 'm' ?
		1.0 / 2.54 : 1.0) * atof(arg) * pixels_per_inch + 0.5;
}

/**
 **	Main programs start here.
 **/

#ifdef	TOOLKIT

static	XrmOptionDescRec	options[] = {
{"-d",		".debugLevel",	XrmoptionSepArg,	(caddr_t) NULL},
{"-s",		".shrinkFactor", XrmoptionSepArg,	(caddr_t) NULL},
#ifndef	VMS
{"-S",		".densityPercent", XrmoptionSepArg,	(caddr_t) NULL},
#endif
{"-density",	".densityPercent", XrmoptionSepArg,	(caddr_t) NULL},
#ifdef	GREY
{"-gamma",	".gamma",	XrmoptionSepArg,	(caddr_t) NULL},
#endif
{"-p",		".pixelsPerInch", XrmoptionSepArg,	(caddr_t) NULL},
{"-margins",	".Margin",	XrmoptionSepArg,	(caddr_t) NULL},
{"-sidemargin",	".sideMargin",	XrmoptionSepArg,	(caddr_t) NULL},
{"-topmargin",	".topMargin",	XrmoptionSepArg,	(caddr_t) NULL},
{"-offsets",	".Offset",	XrmoptionSepArg,	(caddr_t) NULL},
{"-xoffset",	".xOffset",	XrmoptionSepArg,	(caddr_t) NULL},
{"-yoffset",	".yOffset",	XrmoptionSepArg,	(caddr_t) NULL},
{"-paper",	".paper",	XrmoptionSepArg,	(caddr_t) NULL},
{"-altfont",	".altFont",	XrmoptionSepArg,	(caddr_t) NULL},
{"-l",		".listFonts",	XrmoptionNoArg,		(caddr_t) "on"},
{"+l",		".listFonts",	XrmoptionNoArg,		(caddr_t) "off"},
{"-hushspecials", ".hushSpecials", XrmoptionNoArg,	(caddr_t) "on"},
{"+hushspecials", ".hushSpecials", XrmoptionNoArg,	(caddr_t) "off"},
{"-hushchars",	".hushLostChars", XrmoptionNoArg,	(caddr_t) "on"},
{"+hushchars",	".hushLostChars", XrmoptionNoArg,	(caddr_t) "off"},
{"-hush",	".Hush",	XrmoptionNoArg,		(caddr_t) "on"},
{"+hush",	".Hush",	XrmoptionNoArg,		(caddr_t) "off"},
{"-fg",		".foreground",	XrmoptionSepArg,	(caddr_t) NULL},
{"-foreground",	".foreground",	XrmoptionSepArg,	(caddr_t) NULL},
{"-bg",		".background",	XrmoptionSepArg,	(caddr_t) NULL},
{"-background",	".background",	XrmoptionSepArg,	(caddr_t) NULL},
{"-hl",		".highlight",	XrmoptionSepArg,	(caddr_t) NULL},
{"-cr",		".cursorColor",	XrmoptionSepArg,	(caddr_t) NULL},
{"-icongeometry",".iconGeometry",XrmoptionSepArg,	(caddr_t) NULL},
{"-keep",	".keepPosition",XrmoptionNoArg,		(caddr_t) "on"},
{"+keep",	".keepPosition",XrmoptionNoArg,		(caddr_t) "off"},
{"-copy",	".copy",	XrmoptionNoArg,		(caddr_t) "on"},
{"+copy",	".copy",	XrmoptionNoArg,		(caddr_t) "off"},
{"-thorough",	".thorough",	XrmoptionNoArg,		(caddr_t) "on"},
{"+thorough",	".thorough",	XrmoptionNoArg,		(caddr_t) "off"},
{"-version",	".version",	XrmoptionNoArg,		(caddr_t) "on"},
{"+version",	".version",	XrmoptionNoArg,		(caddr_t) "off"},
#ifdef	BUTTONS
{"-expert",	".expert",	XrmoptionNoArg,		(caddr_t) "on"},
{"+expert",	".expert",	XrmoptionNoArg,		(caddr_t) "off"},
#endif
{"-mgs",	".magnifierSize1",XrmoptionSepArg,	(caddr_t) NULL},
{"-mgs1",	".magnifierSize1",XrmoptionSepArg,	(caddr_t) NULL},
{"-mgs2",	".magnifierSize2",XrmoptionSepArg,	(caddr_t) NULL},
{"-mgs3",	".magnifierSize3",XrmoptionSepArg,	(caddr_t) NULL},
{"-mgs4",	".magnifierSize4",XrmoptionSepArg,	(caddr_t) NULL},
{"-mgs5",	".magnifierSize5",XrmoptionSepArg,	(caddr_t) NULL},
#ifdef	GREY
{"-nogrey",	".grey",	XrmoptionNoArg,		(caddr_t) "off"},
{"+nogrey",	".grey",	XrmoptionNoArg,		(caddr_t) "on"},
#endif
};

#define	offset(field)	XtOffsetOf(struct _resource, field)

static	int	basedpi	= BDPI;		/* default value for -p option */

static	XtResource	application_resources[] = {
{"debugLevel", "DebugLevel", XtRString, sizeof(char *),
  offset(debug_arg), XtRString, (caddr_t) NULL},
{"shrinkFactor", "ShrinkFactor", XtRInt, sizeof(int),
  offset(_shrink_factor), XtRString, "3"},
{"densityPercent", "DensityPercent", XtRInt, sizeof(int),
  offset(density), XtRString, "40"},
#ifdef	GREY
{"gamma", "Gamma", XtRFloat, sizeof(float),
  offset(gamma), XtRString, "1"},
#endif
{"pixelsPerInch", "PixelsPerInch", XtRInt, sizeof(int),
  offset(pixels_per_inch), XtRInt, (caddr_t) &basedpi},
{"sideMargin", "Margin", XtRString, sizeof(char *),
  offset(sidemargin), XtRString, (caddr_t) NULL},
{"topMargin", "Margin", XtRString, sizeof(char *),
  offset(topmargin), XtRString, (caddr_t) NULL},
{"xOffset", "Offset", XtRString, sizeof(char *),
  offset(xoffset), XtRString, (caddr_t) NULL},
{"yOffset", "Offset", XtRString, sizeof(char *),
  offset(yoffset), XtRString, (caddr_t) NULL},
{"paper", "Paper", XtRString, sizeof(char *),
  offset(paper), XtRString, (caddr_t) DEFAULT_PAPER},
{"altFont", "AltFont", XtRString, sizeof(char *),
  offset(alt_font), XtRString, (caddr_t) ALTFONT},
{"listFonts", "ListFonts", XtRBoolean, sizeof(Boolean),
  offset(list_fonts), XtRString, "false"},
{"reverseVideo", "ReverseVideo", XtRBoolean, sizeof(Boolean),
  offset(reverse), XtRString, "false"},
{"hushSpecials", "Hush", XtRBoolean, sizeof(Boolean),
  offset(hush_spec), XtRString, "false"},
{"hushLostChars", "Hush", XtRBoolean, sizeof(Boolean),
  offset(hush_chars), XtRString, "false"},
{"foreground", "Foreground", XtRPixel, sizeof(Pixel),
  offset(fore_Pixel), XtRPixel, (caddr_t) &resource.fore_Pixel},
{"foreground", "Foreground", XtRString, sizeof(char *),
  offset(fore_color), XtRString, (caddr_t) NULL},
{"background", "Background", XtRPixel, sizeof(Pixel),
  offset(back_Pixel), XtRPixel, (caddr_t) &resource.back_Pixel},
{"background", "Background", XtRString, sizeof(char *),
  offset(back_color), XtRString, (caddr_t) NULL},
{"borderColor", "BorderColor", XtRPixel, sizeof(Pixel),
  offset(brdr_Pixel), XtRPixel, (caddr_t) &resource.brdr_Pixel},
{"borderColor", "BorderColor", XtRString, sizeof(char *),
  offset(brdr_color), XtRString, (caddr_t) NULL},
{"highlight", "Highlight", XtRPixel, sizeof(Pixel),
  offset(hl_Pixel), XtRPixel, (caddr_t) &resource.hl_Pixel},
{"highlight", "Highlight", XtRString, sizeof(char *),
  offset(high_color), XtRString, (caddr_t) NULL},
{"cursorColor", "CursorColor", XtRPixel, sizeof(Pixel),
  offset(cr_Pixel), XtRPixel, (caddr_t) &resource.cr_Pixel},
{"cursorColor", "CursorColor", XtRString, sizeof(char *),
  offset(curs_color), XtRString, (caddr_t) NULL},
{"iconGeometry", "IconGeometry", XtRString, sizeof(char *),
  offset(icon_geometry), XtRString, (caddr_t) NULL},
{"keepPosition", "KeepPosition", XtRBoolean, sizeof(Boolean),
  offset(keep_flag), XtRString, "false"},
{"copy", "Copy", XtRString, sizeof(char *),
  offset(copy_arg), XtRString, (caddr_t) NULL},
{"copy", "Copy", XtRBoolean, sizeof(Boolean),
  offset(copy), XtRString, "false"},
{"thorough", "Thorough", XtRBoolean, sizeof(Boolean),
  offset(thorough), XtRString, "false"},
{"version", "Version", XtRBoolean, sizeof(Boolean),
  offset(version_flag), XtRString, "false"},
#ifdef	BUTTONS
{"expert", "Expert", XtRBoolean, sizeof(Boolean),
  offset(expert), XtRString, "false"},
#endif
{"magnifierSize1", "MagnifierSize", XtRInt, sizeof(int),
  offset(mg_size[0]), XtRString, "200"},
{"magnifierSize2", "MagnifierSize", XtRInt, sizeof(int),
  offset(mg_size[1]), XtRString, "350"},
{"magnifierSize3", "MagnifierSize", XtRInt, sizeof(int),
  offset(mg_size[2]), XtRString, "600"},
{"magnifierSize4", "MagnifierSize", XtRInt, sizeof(int),
  offset(mg_size[3]), XtRString, "900"},
{"magnifierSize5", "MagnifierSize", XtRInt, sizeof(int),
  offset(mg_size[4]), XtRString, "1200"},
#ifdef	GREY
{"grey", "Grey", XtRBoolean, sizeof (Boolean),
 offset(use_grey), XtRString, "true"},
#endif
};
#undef	offset

static	Arg	temp_args1[] = {
	{XtNiconX,	(XtArgVal) 0},
	{XtNiconY,	(XtArgVal) 0},
};

static	Arg	temp_args2 = {XtNborderWidth,	(XtArgVal) &bwidth};

static	Pixmap	icon_pm;

static	Arg	temp_args3[] = {
	{XtNiconPixmap,	(XtArgVal) &icon_pm},
};

static	Arg	temp_args4[] = {
	{XtNtitle,	(XtArgVal) 0},
	{XtNinput,	(XtArgVal) True},
};

static	Arg	set_wh_args[] = {
	{XtNwidth,	(XtArgVal) 0},
	{XtNheight,	(XtArgVal) 0},
};
#else	/* !TOOLKIT */

static	char	*display;
static	char	*geometry;
static	char	*margins;
static	char	*offsets;
static	Boolean	hush;

#ifndef X10
static	Boolean	iconic	= False;

static	Pixel
string_to_pixel(strp)		/* adapted from the toolkit */
	char	**strp;
{
	char	*str = *strp;
	Status	status;
	XColor	color, junk;

	if (*str == '#') {	/* an rgb definition */
	    status = XParseColor(DISP, DefaultColormapOfScreen(SCRN),
		str, &color);
	    if (status != 0)
		status = XAllocColor(DISP, DefaultColormapOfScreen(SCRN),
		    &color);
	}
	else	/* a name */
	    status = XAllocNamedColor(DISP, DefaultColormapOfScreen(SCRN),
		str, &color, &junk);
	if (status == 0) {
	    Fprintf(stderr, "Cannot allocate colormap entry for \"%s\"\n", str);
	    *strp = NULL;
	    return (Pixel) 0;
	}
	return color.pixel;
}
#endif	/* X10 */

static	struct option {
	_Xconst	char	*name;
	_Xconst	char	*resource;
	enum	{FalseArg, TrueArg, StickyArg, SepArg} argclass;
	enum	{BooleanArg, StringArg, NumberArg, FloatArg} argtype;
	int	classcount;
	caddr_t	address;
}	options[] = {
		/* the display option MUST be first */
{"-display",	NULL,		SepArg,	StringArg, 1,	(caddr_t) &display},
{"-d",		"debugLevel",	SepArg,	StringArg, 1,	(caddr_t) &debug_arg},
{"+",		NULL,		StickyArg, StringArg, 1,(caddr_t) &curr_page},
{"-s",		"shrinkFactor", SepArg, NumberArg, 1,	(caddr_t) &shrink_factor},
{"-S",		NULL,		SepArg, NumberArg, 2,	(caddr_t) &density},
{"-density",	"densityPercent", SepArg, NumberArg, 1,	(caddr_t) &density},
#ifdef	GREY
{"-gamma",	"gamma",	SepArg,	FloatArg, 1,	(caddr_t) &gamma},
#endif
{"-p",		"pixelsPerInch", SepArg, NumberArg, 1,	(caddr_t) &pixels_per_inch},
{"-margins",	"Margin",	SepArg,	StringArg, 3,	(caddr_t) &margins},
{"-sidemargin",	"sideMargin",	SepArg,	StringArg, 1,	(caddr_t) &sidemargin},
{"-topmargin",	"topMargin",	SepArg,	StringArg, 1,	(caddr_t) &topmargin},
{"-offsets",	"Offset",	SepArg,	StringArg, 3,	(caddr_t) &offsets},
{"-xoffset",	"xOffset",	SepArg,	StringArg, 1,	(caddr_t) &xoffset},
{"-yoffset",	"yOffset",	SepArg,	StringArg, 1,	(caddr_t) &yoffset},
{"-paper",	"paper",	SepArg,	StringArg, 1,	(caddr_t) &paper},
{"-altfont",	"altFont",	SepArg,	StringArg, 1,	(caddr_t) &alt_font},
{"-l",		NULL,		TrueArg, BooleanArg, 2,	(caddr_t) &list_fonts},
{"+l",		"listFonts",	FalseArg, BooleanArg, 1,(caddr_t) &list_fonts},
{"-rv",		NULL,		TrueArg, BooleanArg, 2,	(caddr_t) &reverse},
{"+rv",		"reverseVideo",	FalseArg, BooleanArg, 1,(caddr_t) &reverse},
{"-hush",	NULL,		TrueArg, BooleanArg, 6,	(caddr_t) &hush},
{"+hush",	"Hush",		FalseArg, BooleanArg, 5,(caddr_t) &hush},
{"-hushspecials", NULL,		TrueArg, BooleanArg, 2,	(caddr_t) &hush_spec},
{"+hushspecials", "hushSpecials", FalseArg, BooleanArg, 1,(caddr_t) &hush_spec},
{"-hushchars",	NULL,		TrueArg, BooleanArg, 2,	(caddr_t) &hush_chars},
{"+hushchars",	"hushLostChars", FalseArg, BooleanArg, 1,(caddr_t) &hush_chars},
{"-bw",		NULL,		SepArg,	NumberArg, 2,	(caddr_t) &bwidth},
{"-borderwidth", "borderWidth",	SepArg,	NumberArg, 1,	(caddr_t) &bwidth},
{"-fg",		NULL,		SepArg,	StringArg, 2,	(caddr_t) &fore_color},
{"-foreground",	"foreground",	SepArg,	StringArg, 1,	(caddr_t) &fore_color},
{"-bg",		NULL,		SepArg,	StringArg, 2,	(caddr_t) &back_color},
{"-background",	"background",	SepArg,	StringArg, 1,	(caddr_t) &back_color},
{"-bd",		NULL,		SepArg,	StringArg, 2,	(caddr_t) &brdr_color},
{"-bordercolor","borderColor",	SepArg,	StringArg, 1,	(caddr_t) &brdr_color},
{"-hl",		"highlight",	SepArg,	StringArg, 1,	(caddr_t) &high_color},
{"-cr",		"cursorColor",	SepArg,	StringArg, 1,	(caddr_t) &curs_color},
#ifdef	X10
{"=",		NULL,		StickyArg, StringArg, 2,(caddr_t) &geometry},
#endif
{"-geometry",	"geometry",	SepArg,	StringArg, 1,	(caddr_t) &geometry},
#ifndef	X10
{"-icongeometry","iconGeometry",StickyArg, StringArg, 1,(caddr_t) &icon_geometry},
{"-iconic",	NULL,		TrueArg, BooleanArg, 2,	(caddr_t) &iconic},
{"+iconic",	"iconic",	FalseArg, BooleanArg, 1,(caddr_t) &iconic},
{"-keep",	NULL,		TrueArg, BooleanArg, 2,	(caddr_t) &keep_flag},
{"+keep",	"keepPosition",	FalseArg, BooleanArg, 1,(caddr_t) &keep_flag},
{"-copy",	NULL,		TrueArg, BooleanArg, 2,	(caddr_t) &copy},
{"+copy",	"copy",		FalseArg, BooleanArg, 1,(caddr_t) &copy},
{"-thorough",	NULL,		TrueArg, BooleanArg, 2,	(caddr_t) &thorough},
{"+thorough",	"thorough",	FalseArg, BooleanArg, 1,(caddr_t) &thorough},
{"-version",	NULL,		TrueArg, BooleanArg, 2,	(caddr_t)&version_flag},
{"+version",	"version",	FalseArg, BooleanArg, 1,(caddr_t)&version_flag},
#endif	/* X10 */
{"-mgs",	NULL,		SepArg, NumberArg, 2,	(caddr_t) &mg_size[0]},
{"-mgs1",	"magnifierSize1",SepArg, NumberArg, 1,	(caddr_t) &mg_size[0]},
{"-mgs2",	"magnifierSize2",SepArg, NumberArg, 1,	(caddr_t) &mg_size[1]},
{"-mgs3",	"magnifierSize3",SepArg, NumberArg, 1,	(caddr_t) &mg_size[2]},
#ifndef X10
{"-mgs4",	"magnifierSize4",SepArg, NumberArg, 1,	(caddr_t) &mg_size[3]},
{"-mgs5",	"magnifierSize5",SepArg, NumberArg, 1,	(caddr_t) &mg_size[4]},
#endif
#ifdef	GREY
{"-nogrey",	NULL,		FalseArg, BooleanArg, 2,(caddr_t) &use_grey},
{"+nogrey",	"grey",		TrueArg, BooleanArg, 1,	(caddr_t) &use_grey},
#endif
};

/*
 *	Process the option table.  This is not guaranteed for all possible
 *	option tables, but at least it works for this one.
 */

static	void
parse_options(argc, argv)
	int argc;
	char **argv;
{
	char	**arg;
	char	**argvend = argv + argc;
	char	*optstring;
	caddr_t	addr;
	struct option *opt, *lastopt, *candidate;
	int	len1, len2, matchlen;

	/*
	 * Step 1.  Process command line options.
	 */
	for (arg = argv + 1; arg < argvend; ++arg) {
	    len1 = strlen(*arg);
	    candidate = NULL;
	    matchlen = 0;
	    for (opt = options; opt < options + XtNumber(options); ++opt) {
		len2 = strlen(opt->name);
		if (opt->argclass == StickyArg) {
		    if (matchlen <= len2 && !strncmp(*arg, opt->name, len2)) {
			candidate = opt;
			matchlen = len2;
		    }
		}
		else if (len1 <= len2 && matchlen <= len1 &&
		    !strncmp(*arg, opt->name, len1)) {
		    if (len1 == len2) {
			candidate = opt;
			break;
		    }
		    if (matchlen < len1) candidate = opt;
		    else if (candidate && candidate->argclass != StickyArg)
			candidate = NULL;
		    matchlen = len1;
		}
	    }
	    if (candidate == NULL) {
#ifdef	X10
		if (**arg == '-') usage();
		if (index(*arg, ':') != NULL) {	/* display */
		    --arg;
		    candidate = options;
		}
		else if (dvi_name) usage();
#else
		if (**arg == '-' || dvi_name) usage();
#endif
		else {
		    dvi_name = *arg;
		    continue;
		}
	    }
		/* flag it for subsequent processing */
	    candidate->resource = (char *) candidate;
		/* store the value */
	    addr = candidate->address;
	    switch (candidate->argclass) {
		case FalseArg:	*((Boolean *) addr) = False; continue;
		case TrueArg:	*((Boolean *) addr) = True; continue;
		case StickyArg:	optstring = *arg + strlen(candidate->name);
		    break;
		case SepArg:
		    ++arg;
		    if (arg >= argvend) usage();
		    optstring = *arg;
		    break;
	    }
	    switch (candidate->argtype) {
		case StringArg:	*((char **) addr) = optstring; break;
		case NumberArg:	*((int *) addr) = atoi(optstring); break;
		case FloatArg:  *((float *) addr) = atof(optstring); break;
		default:  ;
	    }
	}
	/*
	 * Step 2.  Propagate classes for command line arguments.  Backwards.
	 */
	for (opt = options + XtNumber(options) - 1; opt >= options; --opt)
	    if (opt->resource == (char *) opt) {
		addr = opt->address;
		lastopt = opt + opt->classcount;
		for (candidate = opt; candidate < lastopt; ++candidate) {
		    if (candidate->resource != NULL) {
			switch (opt->argtype) {
			    case BooleanArg:
				*((Boolean *) candidate->address) =
				    *((Boolean *) addr);
				break;
			    case StringArg:
				*((char **) candidate->address) =
				    *((char **) addr);
				break;
			    case NumberArg:
				*((int *) candidate->address) = *((int *) addr);
				break;
			    case FloatArg:
				*((float *) candidate->address) =
				    *((float *) addr);
				break;
			}
			candidate->resource = NULL;
		    }
		}
	    }

#ifndef X10
	if ((DISP = XOpenDisplay(display)) == NULL)
	    oops("Can't open display");
	SCRN = DefaultScreenOfDisplay(DISP);
#else
	if (XOpenDisplay(display) == NULL)
	    oops("Can't open display");
#endif
	/*
	 * Step 3.  Handle resources (including classes).
	 */
	for (opt = options; opt < options + XtNumber(options); ++opt)
	    if (opt->resource &&
#ifndef X10
		    ((optstring = XGetDefault(DISP, prog, opt->resource)) ||
		    (optstring = XGetDefault(DISP, "XDvi", opt->resource))))
#else
		    (optstring = XGetDefault(DPY prog, opt->resource)))
#endif
		{
		    lastopt = opt + opt->classcount;
		    for (candidate = opt; candidate < lastopt; ++candidate)
			if (candidate->resource != NULL) switch (opt->argtype) {
			    case BooleanArg:
				*((Boolean *) candidate->address) =
				    (strcmp(optstring, "on") == 0);
				break;
			    case StringArg:
				*((char **) candidate->address) = optstring;
				break;
			    case NumberArg:
				*((int *) candidate->address) = atoi(optstring);
				break;
			    case FloatArg:
				*((float *) candidate->address) =
				    atof(optstring);
			}
		}
}

#endif	/* TOOLKIT */

static	_Xconst	char	*paper_types[] = {
	"us",		"8.5x11",
	"usr",		"11x8.5",
	"legal",	"8.5x14",
	"foolscap",	"13.5x17.0",	/* ??? */

	/* ISO `A' formats, Portrait */
	"a1",		"59.4x84.0cm",
	"a2",		"42.0x59.4cm",
	"a3",		"29.7x42.0cm",
	"a4",		"21.0x29.7cm",
	"a5",		"14.85x21.0cm",
	"a6",		"10.5x14.85cm",
	"a7",		"7.42x10.5cm",

	/* ISO `A' formats, Landscape */
	"a1r",		"84.0x59.4cm",
	"a2r",		"59.4x42.0cm",
	"a3r",		"42.0x29.7cm",
	"a4r",		"29.7x21.0cm",
	"a5r",		"21.0x14.85cm",
	"a6r",		"14.85x10.5cm",
	"a7r",		"10.5x7.42cm",

	/* ISO `B' formats, Portrait */
	"b1",		"70.6x100.0cm",
	"b2",		"50.0x70.6cm",
	"b3",		"35.3x50.0cm",
	"b4",		"25.0x35.3cm",
	"b5",		"17.6x25.0cm",
	"b6",		"13.5x17.6cm",
	"b7",		"8.8x13.5cm",

	/* ISO `B' formats, Landscape */
	"b1r",		"100.0x70.6cm",
	"b2r",		"70.6x50.0cm",
	"b3r",		"50.0x35.3cm",
	"b4r",		"35.3x25.0cm",
	"b5r",		"25.0x17.6cm",
	"b6r",		"17.6x13.5cm",
	"b7r",		"13.5x8.8cm",

	/* ISO `C' formats, Portrait */
	"c1",		"64.8x91.6cm",
	"c2",		"45.8x64.8cm",
	"c3",		"32.4x45.8cm",
	"c4",		"22.9x32.4cm",
	"c5",		"16.2x22.9cm",
	"c6",		"11.46x16.2cm",
	"c7",		"8.1x11.46cm",

	/* ISO `C' formats, Landscape */
	"c1r",		"91.6x64.8cm",
	"c2r",		"64.8x45.8cm",
	"c3r",		"45.8x32.4cm",
	"c4r",		"32.4x22.9cm",
	"c5r",		"22.9x16.2cm",
	"c6r",		"16.2x11.46cm",
	"c7r",		"11.46x8.1cm",
};

static	Boolean
set_paper_type() {
	_Xconst	char	*arg, *arg1;
	char	temp[21];
	_Xconst	char	**p;
	char	*q;

	if (strlen(RESOURCE(paper)) > sizeof(temp) - 1) return False;
	arg = RESOURCE(paper);
	q = temp;
	for (;;) {	/* convert to lower case */
	    char c = *arg++;
	    if (c >= 'A' && c <= 'Z') c ^= ('a' ^ 'A');
	    *q++ = c;
	    if (c == '\0') break;
	}
	arg = temp;
	/* perform substitutions */
	for (p = paper_types; p < paper_types + XtNumber(paper_types); p += 2)
	    if (strcmp(temp, *p) == 0) {
		arg = p[1];
		break;
	    }
	arg1 = index(arg, 'x');
	if (arg1 == NULL) return False;
	unshrunk_paper_w = atopix(arg);
	unshrunk_paper_h = atopix(arg1 + 1);
	return (unshrunk_paper_w != 0 && unshrunk_paper_h != 0);
}

/*
 *	main program
 */

int
main(argc, argv)
	int argc;
	char **argv;
{

#ifndef	TOOLKIT
#ifndef X10
	XSizeHints	size_hints;
	XWMHints	wmhints;
#else
	OpaqueFrame frame;
	char	def[32];
	int	mouspix;
	Color	cdef;
	int	x_thick, y_thick;
#endif
#endif	/* TOOLKIT */
	int	screen_w, screen_h;

#ifndef	VMS
	prog = rindex(*argv, '/');
#else
	prog = rindex(*argv, ']');
#endif
	if (prog != NULL) ++prog; else prog = *argv;

#ifdef	VMS
	if (index(prog, '.') != NULL) *index(prog, '.') = '\0';
#endif

#ifdef	TOOLKIT
	top_level = XtInitialize(prog, "XDvi", options, XtNumber(options),
		&argc, argv);
	while (--argc > 0) {
	    if (*(*++argv) == '+')
		if (curr_page != NULL) usage();
		else curr_page = *argv + 1;
	    else if (dvi_name != NULL) usage();
		else dvi_name = *argv;
	}

	XtGetApplicationResources(top_level, (XtPointer) &resource,
	    application_resources, XtNumber(application_resources), NULL, 0);
	DISP = XtDisplay(top_level);
	SCRN = XtScreen(top_level);
	shrink_factor = resource._shrink_factor;
	density = resource.density;
	pixels_per_inch = resource.pixels_per_inch;
	alt_font = resource.alt_font;
	list_fonts = resource.list_fonts;
	hush_spec = resource.hush_spec;
	hush_chars = resource.hush_chars;
#ifdef	GREY
	use_grey = resource.use_grey;
#endif

#else	/* !TOOLKIT */

	parse_options(argc, argv);
#ifndef X10
	if (fore_color) fore_Pixel = string_to_pixel(&fore_color);
	if (back_color) back_Pixel = string_to_pixel(&back_color);
	if (brdr_color) brdr_Pixel = string_to_pixel(&brdr_color);
	if (high_color) hl_Pixel = string_to_pixel(&high_color);
	if (curs_color) cr_Pixel = string_to_pixel(&curs_color);
#endif

#endif	/* TOOLKIT */

	if (shrink_factor <= 0 || density <= 0 || pixels_per_inch <= 0 ||
		dvi_name == NULL) usage();
	if (shrink_factor != 1) bak_shrink = shrink_factor;
	mane.shrinkfactor = shrink_factor;
	if (RESOURCE(debug_arg) != NULL)
	    debug = isdigit(*RESOURCE(debug_arg)) ? atoi(RESOURCE(debug_arg))
		: DBG_ALL;
	if (RESOURCE(sidemargin)) home_x = atopix(RESOURCE(sidemargin));
	if (RESOURCE(topmargin)) home_y = atopix(RESOURCE(topmargin));
	offset_x = RESOURCE(xoffset) ? atopix(RESOURCE(xoffset))
	    : pixels_per_inch;
	offset_y = RESOURCE(yoffset) ? atopix(RESOURCE(yoffset))
	    : pixels_per_inch;
	if (!set_paper_type()) oops("Don't recognize paper type %s",
	    RESOURCE(paper));

	init_font_open();
	open_dvi_file();
	if (curr_page) {
		current_page = (*curr_page ? atoi(curr_page) : total_pages) - 1;
		if (current_page < 0 || current_page >= total_pages) usage();
	}
	if (RESOURCE(version_flag)) Puts((_Xconst char *) &version);

#ifndef X10

	/*
	 *	X11 colors
	 */

	if (RESOURCE(reverse)) {
	    if (!RESOURCE(fore_color))
		RESOURCE(fore_Pixel) = WhitePixelOfScreen(SCRN);
	    if (!RESOURCE(back_color))
		RESOURCE(back_Pixel) = BlackPixelOfScreen(SCRN);
	    /* Set them nonzero */
	    RESOURCE(fore_color) = RESOURCE(back_color) = (char *) &version;
	} else {
	    if (!RESOURCE(fore_color))
		RESOURCE(fore_Pixel) = BlackPixelOfScreen(SCRN);
	    if (!RESOURCE(back_color))
		RESOURCE(back_Pixel) = WhitePixelOfScreen(SCRN);
	}

#ifdef	GREY
	if (DefaultDepthOfScreen(SCRN) == 1)
	    use_grey = False;
#endif

#ifdef	TOOLKIT
	if (!resource.copy_arg)
#else
	if (copy == 2)
#endif
#ifdef	GREY
	    RESOURCE(copy) = (!RESOURCE(thorough) && !use_grey
		&& DefaultDepthOfScreen(SCRN) > 1);
#else
	    RESOURCE(copy) = (!RESOURCE(thorough)
		&& DefaultDepthOfScreen(SCRN) > 1);
#endif

#ifdef	GREY
	if (use_grey)
	    init_pix(True);
	else
#endif
	{
	    XGCValues	values;
	    Pixel	set_bits = (Pixel)
				(RESOURCE(fore_Pixel) & ~RESOURCE(back_Pixel));
	    Pixel	clr_bits = (Pixel)
				(RESOURCE(back_Pixel) & ~RESOURCE(fore_Pixel));

#define	MakeGC(fcn, fg, bg)	(values.function = fcn, values.foreground=fg,\
		values.background=bg,\
		XCreateGC(DISP, RootWindowOfScreen(SCRN),\
			GCFunction|GCForeground|GCBackground, &values))

	    if (RESOURCE(copy) || (set_bits && clr_bits))
		ruleGC = MakeGC(GXcopy,
		    RESOURCE(fore_Pixel), RESOURCE(back_Pixel));
	    if (RESOURCE(copy)) foreGC = ruleGC;
	    else if (!RESOURCE(thorough) && ruleGC) {
		foreGC = ruleGC;
		Puts("Note:  overstrike characters may be incorrect.");
	    }
	    else {
		if (set_bits) foreGC = MakeGC(GXor, set_bits, 0);
		if (clr_bits)
		    *(foreGC ? &foreGC2 : &foreGC) =
			MakeGC(GXandInverted, clr_bits, 0);
		if (!ruleGC) ruleGC = foreGC;
	    }
	}

	{
	    XGCValues	values;

	    highGC = ruleGC;
	    if (RESOURCE(high_color))
		highGC = MakeGC(GXcopy,
		    RESOURCE(hl_Pixel), RESOURCE(back_Pixel));
	}

	if (!RESOURCE(brdr_color)) RESOURCE(brdr_Pixel) = RESOURCE(fore_Pixel);

#ifndef	VMS
	ready_cursor = XCreateFontCursor(DISP, XC_cross);
	redraw_cursor = XCreateFontCursor(DISP, XC_watch);
#else
	DECWCursorFont = XLoadFont(DISP, "DECW$CURSOR");
	XSetFont(DISP, foreGC, DECWCursorFont);
	redraw_cursor = XCreateGlyphCursor(DISP, DECWCursorFont, DECWCursorFont,
		decw$c_wait_cursor, decw$c_wait_cursor + 1,
		&RESOURCE(fore_color), &RESOURCE(back_color));
	MagnifyPixmap = XCreateBitmapFromData (DISP, RootWindowOfScreen(SCRN),
		mag_glass_bits, mag_glass_width, mag_glass_height);
	ready_cursor = XCreatePixmapCursor(DISP, MagnifyPixmap, MagnifyPixmap,
		&RESOURCE(back_color), &RESOURCE(fore_color),
		mag_glass_x_hot, mag_glass_y_hot);
#endif	/* VMS */

	if (!RESOURCE(curs_color))
	    RESOURCE(cr_Pixel) = RESOURCE(high_color) ? RESOURCE(hl_Pixel)
		: RESOURCE(fore_Pixel);
	{
	    XColor bg_Color, cr_Color;

	    bg_Color.pixel = RESOURCE(back_Pixel);
	    XQueryColor(DISP, DefaultColormapOfScreen(SCRN), &bg_Color);
	    cr_Color.pixel = RESOURCE(cr_Pixel);
	    XQueryColor(DISP, DefaultColormapOfScreen(SCRN), &cr_Color);
	    XRecolorCursor(DISP, ready_cursor, &cr_Color, &bg_Color);
	    XRecolorCursor(DISP, redraw_cursor, &cr_Color, &bg_Color);
	}

#ifdef	TOOLKIT

	/*
	 *	X11 windows (toolkit)
	 */

		/* The following code is lifted from Xterm */
	if (resource.icon_geometry != NULL) {
	    int scr, junk;

	    for(scr = 0;	/* yyuucchh */
		SCRN != ScreenOfDisplay(DISP, scr);
		scr++);

	    XGeometry(DISP, scr, resource.icon_geometry, "", 0, 0, 0, 0, 0,
			(int *) &temp_args1[0].value,
			(int *) &temp_args1[1].value, &junk, &junk);
	    XtSetValues(top_level, temp_args1, XtNumber(temp_args1));
	}
		/* Set icon pixmap */
	XtGetValues(top_level, temp_args3, XtNumber(temp_args3));
	if (icon_pm == (Pixmap) 0) {
	    temp_args3[0].value = (XtArgVal) (XCreateBitmapFromData(DISP,
				RootWindowOfScreen(SCRN),
				xdvi_bits, xdvi_width, xdvi_height));
	    XtSetValues(top_level, temp_args3, XtNumber(temp_args3));
	}
	temp_args4[0].value = (XtArgVal) dvi_name;
	XtSetValues(top_level, temp_args4, XtNumber(temp_args4));

#ifdef	BUTTONS
	form_widget = XtCreateManagedWidget("form", formWidgetClass,
		top_level, form_args, XtNumber(form_args));

	line_args[0].value = (XtArgVal) resource.high_color
	    ? resource.hl_Pixel : resource.fore_Pixel;
#else	/* !BUTTONS */
#define	form_widget	top_level	/* for calls to XtAddEventHandler */
#endif	/* BUTTONS */
	vport_widget = XtCreateManagedWidget("vport", viewportWidgetClass,
		form_widget, vport_args, XtNumber(vport_args));
	clip_widget = XtNameToWidget(vport_widget, "clip");
	draw_args[0].value = (XtArgVal) page_w;
	draw_args[1].value = (XtArgVal) page_h;
#ifdef	GREY
	draw_args[2].value = (XtArgVal) resource.back_Pixel;
#endif
	draw_widget = XtCreateManagedWidget("drawing", drawWidgetClass,
		vport_widget, draw_args, XtNumber(draw_args));
	{	/* set default window size */
#ifdef	BUTTONS
	    int xtra_wid = resource.expert ? 0 : XTRA_WID;
#else
#define	xtra_wid	0
#endif
	    XtWidgetGeometry constraints;
	    XtWidgetGeometry reply;

	    XtGetValues(top_level, &temp_args2, 1);	/* get border width */
	    screen_w = WidthOfScreen(SCRN) - 2 * bwidth - xtra_wid;
	    screen_h = HeightOfScreen(SCRN) - 2 * bwidth;
	    constraints.request_mode = reply.request_mode = 0;
	    constraints.width = page_w;
	    if (page_w > screen_w) {
		constraints.request_mode = CWWidth;
		constraints.width = screen_w;
	    }
	    constraints.height = page_h;
	    if (page_h > screen_h) {
		constraints.request_mode = CWHeight;
		constraints.height = screen_h;
	    }
	    if (constraints.request_mode != 0
		    && constraints.request_mode != (CWWidth | CWHeight))
		(void) XtQueryGeometry(vport_widget, &constraints, &reply);
	    if (!(reply.request_mode & CWWidth))
		reply.width = constraints.width;
	    set_wh_args[0].value = (XtArgVal) ((reply.width < screen_w
				? reply.width : screen_w) + xtra_wid);
	    if (!(reply.request_mode & CWHeight))
		reply.height = constraints.height;
	    set_wh_args[1].value = (XtArgVal) (reply.height < screen_h
					? reply.height : screen_h);
	    XtSetValues(top_level, set_wh_args, XtNumber(set_wh_args));
#ifdef	BUTTONS
	    set_wh_args[0].value -= xtra_wid;
	    XtSetValues(vport_widget, set_wh_args, XtNumber(set_wh_args));
	    if (!resource.expert) create_buttons(set_wh_args[1].value);
#endif	/* BUTTONS */
	}
	if (resource.fore_color) {
	    static Arg fore_args = {XtNforeground, (XtArgVal) 0};

	    fore_args.value = resource.fore_Pixel;
	    XtSetValues(draw_widget, &fore_args, 1);
	}
	if (resource.back_color) {
	    static Arg back_args = {XtNbackground, (XtArgVal) 0};

	    back_args.value = resource.back_Pixel;
	    XtSetValues(draw_widget, &back_args, 1);
	    XtSetValues(clip_widget, &back_args, 1);
	}
	XtAddEventHandler(form_widget, KeyPressMask, False, handle_key,
		(caddr_t) NULL);
	XtAddEventHandler(vport_widget, StructureNotifyMask, False,
		handle_resize, (caddr_t) NULL);
	XtAddEventHandler(draw_widget, ExposureMask, False, handle_exp,
		(caddr_t) &mane);
	XtAddEventHandler(draw_widget, ButtonPressMask, False, handle_button,
		(caddr_t) NULL);
	XtAddEventHandler(draw_widget, ButtonMotionMask, False, handle_motion,
		(caddr_t) NULL);
	XtAddEventHandler(draw_widget, ButtonReleaseMask, False, handle_release,
		(caddr_t) NULL);
	XtRealizeWidget(top_level);

	currwin.win = mane.win = XtWindow(draw_widget);

	{
	    XWindowAttributes attrs;

	    XGetWindowAttributes(DISP, WINDOW(mane), &attrs);
	    backing_store = attrs.backing_store;
	}

#else	/* !TOOLKIT */

	/*
	 *	X11 windows (non toolkit)
	 */

	screen_w = WidthOfScreen(SCRN) - 2*bwidth;
	screen_h = HeightOfScreen(SCRN) - 2*bwidth;
	size_hints.flags = PMinSize;
	size_hints.min_width = size_hints.min_height = 50;
	size_hints.x = size_hints.y = 0;
	if (geometry != NULL) {
	    int flag = XParseGeometry(geometry, &size_hints.x, &size_hints.y,
		&window_w, &window_h);

	    if (flag & (XValue | YValue))
		size_hints.flags |= USPosition;
	    if (flag & (WidthValue | HeightValue))
		size_hints.flags |= USSize;
	    if (flag & XNegative) size_hints.x += screen_w - window_w;
	    if (flag & YNegative) size_hints.y += screen_h - window_h;
	}
	if (!(size_hints.flags & USSize)) {
	    int x_thick = 0;
	    int y_thick = 0;
	    if (screen_w < page_w) x_thick = BAR_THICK;
	    if (screen_h < page_h + x_thick) y_thick = BAR_THICK;
	    window_w = page_w + y_thick;
	    if (window_w > screen_w) {
		x_thick = BAR_THICK;
		window_w = screen_w;
	    }
	    window_h = page_h + x_thick;
	    if (window_h > screen_h) window_h = screen_h;
	    size_hints.flags |= PSize;
	}
	size_hints.width = window_w;
	size_hints.height = window_h;
	top_level = XCreateSimpleWindow(DISP, RootWindowOfScreen(SCRN),
		size_hints.x, size_hints.y, window_w, window_h, bwidth,
		brdr_Pixel, back_Pixel);
	XSetStandardProperties(DISP, top_level, dvi_name, prog, NULL,
		argv, argc, &size_hints);

	wmhints.flags = InputHint | StateHint | IconPixmapHint;
	wmhints.input = True;	/* window manager must direct input */
	wmhints.initial_state = iconic ? IconicState : NormalState;
	wmhints.icon_pixmap = XCreateBitmapFromData(DISP,
				RootWindowOfScreen(SCRN),
				xdvi_bits, xdvi_width, xdvi_height);
	if (icon_geometry != NULL) {
	    int junk;

	    wmhints.flags |= IconPositionHint;
	    XGeometry(DISP, DefaultScreen(DISP), icon_geometry, "",
		0, 0, 0, 0, 0, &wmhints.icon_x, &wmhints.icon_y, &junk, &junk);
	}
	XSetWMHints(DISP, top_level, &wmhints);

	XSelectInput(DISP, top_level, KeyPressMask | StructureNotifyMask);
	XMapWindow(DISP, top_level);
	Flush();

#endif	/* TOOLKIT */

	XRebindKeysym(DISP, XK_Home, NULL, 0, (_Xconst ubyte *) "^", 1);
	XRebindKeysym(DISP, XK_Left, NULL, 0, (_Xconst ubyte *) "l", 1);
	XRebindKeysym(DISP, XK_Up, NULL, 0, (_Xconst ubyte *) "u", 1);
	XRebindKeysym(DISP, XK_Right, NULL, 0, (_Xconst ubyte *) "r", 1);
	XRebindKeysym(DISP, XK_Down, NULL, 0, (_Xconst ubyte *) "d", 1);
	XRebindKeysym(DISP, XK_Prior, NULL, 0, (_Xconst ubyte *) "b", 1);
	XRebindKeysym(DISP, XK_Next, NULL, 0, (_Xconst ubyte *) "f", 1);

	image = XCreateImage(DISP, DefaultVisualOfScreen(SCRN), 1, XYBitmap, 0,
			     (char *)NULL, 0, 0, BITS_PER_BMUNIT, 0);
	image->bitmap_unit = BITS_PER_BMUNIT;
#ifndef	MSBITFIRST
	image->bitmap_bit_order = LSBFirst;
#else
	image->bitmap_bit_order = MSBFirst;
#endif
	{
	    short endian = MSBFirst << 8 | LSBFirst;
	    image->byte_order = *((char *) &endian);
	}

#else	/* X10 */

	/*
	 *	X10 colors
	 */

	if (reverse) {
		foreGC = WhitePixel;
		highGC = WhitePixel;
		backpix = BlackPixel;
		backmap = BlackPixmap;
		bdrmap = WhitePixmap;
		mouspix = WhitePixel;
		GXfunc = GXor;
	} else {
		foreGC = BlackPixel;
		highGC = BlackPixel;
		backpix = WhitePixel;
		backmap = WhitePixmap;
		bdrmap = BlackPixmap;
		mouspix = BlackPixel;
		GXfunc = GXand;
	}
	if (DisplayCells() > 2) {
		if (fore_color && XParseColor(fore_color, &cdef) &&
			XGetHardwareColor(&cdef))
			foreGC = cdef.pixel;
		if (back_color && XParseColor(back_color, &cdef) &&
			XGetHardwareColor(&cdef)) {
			backpix = cdef.pixel;
			backmap = XMakeTile(backpix);
		}
		if (brdr_color && XParseColor(brdr_color, &cdef) &&
			XGetHardwareColor(&cdef))
			bdrmap = XMakeTile(cdef.pixel);
		if (high_color && XParseColor(high_color, &cdef) &&
			XGetHardwareColor(&cdef))
			highGC = cdef.pixel;
		if (curs_color && XParseColor(curs_color, &cdef) &&
			XGetHardwareColor(&cdef))
			mouspix = cdef.pixel;
	}

	/*
	 *	X10 windows
	 */

	frame.bdrwidth = bwidth;
	screen_w = DisplayWidth() - 2*bwidth;
	screen_h = DisplayHeight() - 2*bwidth;
	x_thick = y_thick = 0;
	if (screen_w < page_w) x_thick = BAR_THICK;
	if (screen_h < page_h + x_thick) y_thick = BAR_THICK;
	frame.width = page_w + y_thick;
	if (frame.width > screen_w) {
	    x_thick = BAR_THICK;
	    frame.width = screen_w;
	}
	frame.height = page_h + x_thick;
	if (frame.height > screen_h) frame.height = screen_h;
	frame.border = bdrmap;
	frame.background = backmap;
	frame.x = 0;
	frame.y = 0;
	Sprintf(def, "=%dx%d+0+0", frame.width, frame.height);
	top_level = XCreate("DVI Previewer", prog, geometry, def,
		&frame, 50, 50);
	XSelectInput(top_level, ExposeWindow | KeyPressed);
	XMapWindow(top_level);
	XDefineCursor(top_level,
	    XCreateCursor(xdvi_width, xdvi_height, xdvi_bits, xdvi_mask_bits,
			  xdvi_x_hot, xdvi_y_hot, mouspix, backpix, GXcopy));
#endif	/* X10 */

	do_pages();
	return 0;	/* do_pages() returns if DBG_BATCH is specified */
}
