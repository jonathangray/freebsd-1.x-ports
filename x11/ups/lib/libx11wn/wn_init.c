/* wn_init.c - initialisation, including opening the main window */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char wn_wn_init_c_sccsid[] = "@(#)wn_init.c	1.44 15/9/92 (UKC)";

#ifdef SVR4
#include <sys/fcntl.h>
#endif

#include <sys/types.h>
#include <sys/file.h>
#include <sys/param.h>		/* for MAXPATHLEN */
#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>

#ifdef __STDC__
#include <unistd.h>
#endif

#include <local/ukcprog.h>

#include "wn.h"
#include "wn_priv.h"

#ifdef X11
#include <X11/Xresource.h>
#endif

#include "wn_bm.h"
#include "wn_event.h"
#include "wn_cu.h"
#include "wn_rop.h"
#include "wn_misc.h"
#include "wn_win.h"
#include "wn_init.h"
#include "wn_replay.h"
#include "wn_color.h"
#include "sccsdata.h"

#if defined(clipper) || (defined(vax) && !defined(ultrix))
#define NO_PUTENV
#endif

#ifdef NO_PUTENV
static int my_putenv PROTO((const char *s));
#define putenv	my_putenv
#endif

#ifndef SVR4
/*  BUG: this shouldn't be here, it should be in a system header file.
 */
int putenv PROTO((const char *s));
#endif

#ifdef X11
static const char *get_xdef PROTO((const char *name, const char *def));
static int _wn_create_x_window PROTO((swin_t *w, const char *name, int x, int y, int width, int height, int border_width, const char *geometry));
static void create_gc PROTO((void));
static void set_colors PROTO((void));
static Display *open_display PROTO((const char *displayname, const char **p_errmsg));
static void parse_x11_geometry PROTO((XSizeHints *xsh, const char *geometry));
static void get_def_color PROTO((const char *defaultname, xpixel_t def_pixel, int def_rgb_val, const char *argspec, XColor *color));
static int _wn_create_x_window PROTO((swin_t *w, const char *name, int x, int y, int width, int height, int border_width, const char *geometry));
#endif

#ifdef SUNVIEW
static int my_error_handler PROTO((int errnum, int winopnum));
#endif

static int add_wm_fd PROTO((int fd, char *errbuf));
static void parse_x11_geometry PROTO(( XSizeHints *xsh, const char *geometry));

#ifdef SUNVIEW
static int (*Old_sun_error_handler)();

static int
my_error_handler(errnum, winopnum)
int errnum, winopnum;
{
	(*Old_sun_error_handler)(errnum, winopnum);
	if (errnum != 0)
		wn__panic("SUNVIEW graphical call failed");
}
#endif /* SUNVIEW */

static bool App_suggests_xpos = FALSE, App_suggests_ypos = FALSE;
static bool App_suggests_width = FALSE, App_suggests_height = FALSE;
static int App_xpos, App_ypos, App_width, App_height;

/*  Values for the foreground and background pixels.
 *  Initialised for mono displays - color ones overwrite them.
 */
int _wn_Bg_pixel = 0;
int _wn_Fg_pixel = 1;
#ifdef X11
xplanemask_t _wn_Planemask = 0x1;
#endif /* X11 */
#ifdef SUNVIEW
int _wn_Planemask = 0x1;
#endif /* SUNVIEW */

int wn__Batching_enabled = TRUE;

#ifdef X11
/*  Kludge for ICL guide - see _wn_dont_map_windows_on_create() and _wn_map_stdwin().
 */
static int Map_windows_on_create = TRUE;
#endif /* X11 */

/*  BUG: need a comment explaining why these aren't all ifdef'ed.
 */
static const char *Stdwin_geometry = NULL;
static bool Want_blanket = FALSE;
static const char Default_appname[] = "wn";
static const char *Appname = Default_appname;
static const char Default_appclass[] = "Wn";
static const char *Appclass = Default_appclass;

static int Want_to_start_as_icon = FALSE;

#ifdef X11
Display *wn__Dpy;
#ifdef X11
Colormap _wn_Cmap;
extern int _Xdebug;
#endif /* X11 */

GC _wn_Gc;

static const char *Default_display = NULL;

static const char *Bg_color_name = NULL, *Fg_color_name = NULL;
int _wn_Autoraise = FALSE;
const char *_wn_Icon_geometry = NULL;

static bool Want_reverse_video = FALSE;
#endif /* X11 */

const char *
wn_version()
{
	return _wn_sccsdata[0];
}

void
wn_set_classname(name)
const char *name;
{
	Appclass = name;
}

const char *
wn_get_default(name)
const char *name;
{
#ifdef X11
	/*  The code to read the app-defaults database is
	 *  by Greg McFarlane <gregm@otc.research.otca.oz.au>.
	 */
	const char *value;
	static XrmDatabase applicationDB = NULL;

	if (applicationDB == NULL) {
		char path[MAXPATHLEN];

		sprintf(path, "/usr/lib/X11/app-defaults/%s", Appclass);
		applicationDB = XrmGetFileDatabase(path);
	}

	if (wn__Dpy == NULL)
		return NULL;

	value = XGetDefault(wn__Dpy, Appname, name);

	if (value == NULL && Appclass != NULL && strcmp(Appclass, Appname) != 0)
		value = XGetDefault(wn__Dpy, Appclass, name);

	if (value == NULL && applicationDB != NULL) {
		char app_name[512], class_name[512];
		XrmValue xrm_value;
		char *str_type;

		sprintf(app_name, "%s.%s", Appname, name);
		sprintf(class_name, "%s.%s", Appclass, name);

		if (XrmGetResource (applicationDB, app_name, class_name,
							&str_type, &xrm_value))
			value = xrm_value.addr;
	}

	return value;
#else
#ifdef SUNVIEW
	char option_name[100];

	/*  We want space for "/Appname/name" and a NUL.
	 */
	if (1 + strlen(Appname) + 1 + strlen(name) + 1 >= sizeof(option_name))
		return NULL;
	sprintf(option_name, "/%s/%s", Appname, name);

	if (defaults_exists(option_name, (int *)NULL))
		return defaults_get_string(option_name, "", (int *)NULL);
	return NULL;
#else
	return NULL;
#endif /* !SUNVIEW */
#endif /* !X11 */
}

void
wn_use_color_hint(use_color)
int use_color;
{
	wn__Use_mono = !use_color;
}

static const char **Unmunge_args;
static int N_unmunge_args = 0;
static int Unmunge_args_size = 0;

void
_wn_add_to_unmunge_list(args, nargs)
const char **args;
int nargs;
{
	const char *s;
	int i;

	if (Unmunge_args_size == 0) {
		Unmunge_args_size = 16;
		Unmunge_args = (const char **) wn__e_malloc(Unmunge_args_size *
								sizeof(char *));
	}
	if (N_unmunge_args + nargs + 1 >= Unmunge_args_size) {
		Unmunge_args_size *= 2;
		Unmunge_args = (const char **)wn__e_realloc((char *)Unmunge_args,
						Unmunge_args_size * sizeof(char *));
	}
	for (i = 0; i < nargs; ++i) {
		s = strcpy(wn__e_malloc(strlen(args[i]) + 1), args[i]);
		Unmunge_args[N_unmunge_args++] = s;
	}
	Unmunge_args[N_unmunge_args] = NULL;
}

const char **
wn_unmunge_args(args, pos)
const char **args;
int pos;
{
	const char **uargs, **dst;
	int i, nargs, argn;

	for (nargs = 0; args[nargs] != NULL; ++nargs)
		;
	uargs = (const char **)wn__e_malloc((nargs + N_unmunge_args + 1) * sizeof(char *));
	dst = uargs;

	for (argn = 0; argn < pos && args[argn] != NULL; ++argn)
		*dst++ = args[argn];
	for (i = 0; i < N_unmunge_args; ++i)
		*dst++ = Unmunge_args[i];
	for (; args[argn] != NULL; ++argn)
		*dst++ = args[argn];
	*dst++ = NULL;

	return uargs;
}

int
wn_munge_args(argc, argv)
int argc;
const char **argv;
{
	const char **src, **dst, **lim, *s;
	int len;
#ifdef X11
	const char *font, *pos;
#endif /* X11 */

	if ((Appname = strrchr(*argv, '/')) != NULL && Appname[1] != '\0')
		Appname++;
	else
		Appname = *argv;
	src = dst = argv;
	lim = argv + argc;
#ifdef X11
	/*  Under X11, XGetDefault requires a display argument, so we must
	 *  fish this out first.  We must also get any -name flag.
	 */
	for (src = dst; src < lim - 1; ++src) {
		if (strlen(*src) > 1 && strncmp(*src, "-display",
						         strlen(*src)) == 0)
			Default_display = *++src;
		else if (strcmp(*src, "-name") == 0)
			Appname = *++src;
	}
	wn__Dpy = open_display(Default_display, (const char **)NULL);
#endif /* X11 */

#ifdef X11
	/* XrmGetDefault("Font", "font"); */
	if ((font = wn_get_default("Font")) != NULL) 
		_wn_Sysfont_file = font;
	Stdwin_geometry = wn_get_default("Geometry");
	if (wn_get_default("ReverseVideo") != NULL)
		Want_reverse_video = TRUE;
	if ((s = wn_get_default("ForceMono")) != NULL && strcmp(s, "yes") == 0)
		wn__Use_mono = TRUE;

	/*  We allow auto raise to be set by an environment variable because
	 *  passing arguments is hard for ICL guide.
	 */
	if (getenv("WN_AUTORAISE") != NULL)
		_wn_Autoraise = TRUE;
	else
		_wn_Autoraise = wn_get_default("AutoRaise") != NULL;
#endif /* X11 */

	if (getenv("KSTMONO") != NULL)
		wn__Use_mono = TRUE;
	if (getenv("KSTCOLOR") != NULL || getenv("KSTCOLOUR") != NULL)
		wn__Use_mono = FALSE;

	src = dst = argv;
	while (src < lim) {
		s = *src;
		len = strlen(s);
		if (strcmp(s, "-blanket") == 0) {
			Want_blanket = TRUE;
			src++;
		}
#ifdef X11
		else if (*s == '#' && s[1] == '+' && isdigit(s[2]))
			_wn_Icon_geometry = *src++ + 1; /* +1 drops the '#' */
		else if (strcmp(s, "-iconic") == 0) {
			Want_to_start_as_icon = TRUE;
			++src;
		}
#ifdef X11
		else if (len > 1 && strncmp(s, "-display", len) == 0 &&
								src[1] != NULL) {
			_wn_add_to_unmunge_list(src, 2);
			Default_display = src[1];
			src += 2;
		}
		else if (len > 1 && strncmp(s, "-geometry", len) == 0 &&
								src[1] != NULL) {
			Stdwin_geometry = src[1];
			src += 2;
		}
		else if (strcmp(s, "-name") == 0 && src[1] != NULL) {
			Appname = src[1];
			src += 2;
		}
		else if (strcmp(s, "-xdebug") == 0) {
			_Xdebug = TRUE;
			++src;
		}
#endif /* X11 */
		else if ((pos = strchr(s, ':')) != NULL && isdigit(pos[1])) {
			_wn_add_to_unmunge_list(src, 1);
			Default_display = *src++;
		}
		else if (strcmp(s, "-fn") == 0 && src[1] != NULL) {
			_wn_add_to_unmunge_list(src, 1);
			_wn_Sysfont_file = src[1];
			src += 2;
		}
		else if (strcmp(s, "-fg") == 0 && src[1] != NULL) {
			_wn_add_to_unmunge_list(src, 2);
			Fg_color_name = src[1];
			src += 2;
		}
		else if (strcmp(s, "-bg") == 0 && src[1] != NULL) {
			_wn_add_to_unmunge_list(src, 2);
			Bg_color_name = src[1];
			src += 2;
		}
		else if (strcmp(s, "-rv") == 0 || strcmp(s, "-reverse") == 0) {
			Want_reverse_video = !Want_reverse_video;
			++src;
		}
		else if (strcmp(s, "-wn_autoraise") == 0) {
			_wn_Autoraise = TRUE;
			++src;
		}
#endif /* X11 */
		else if (strcmp(s, "-color") == 0 || strcmp(s, "-colour") == 0) {
			_wn_add_to_unmunge_list(src, 1);
			wn__Use_mono = FALSE;
			++src;
		}
		else if (strcmp(s, "-mono") == 0) {
			_wn_add_to_unmunge_list(src, 1);
			wn__Use_mono = TRUE;
			++src;
		}
		else if (strcmp(s, "-wn_debug") == 0) {
			wn__Batching_enabled = FALSE;
			++src;
		}
		else if (strcmp(s, "-wn_record") == 0 && src[1] != NULL) {
			if (wn_set_record_file(src[1]) != 0)
				fprintf(stderr, "wn: can't open %s (%s) so not recording\n",
							src[1], _wn_reason());
			src += 2;
		}
		else if (strcmp(s, "-wn_replay") == 0 && src[1] != NULL) {
			if (wn_set_replay_file(src[1]) != 0)
				fprintf(stderr, "wn: can't open %s (%s) so not replaying\n",
							src[1], _wn_reason());
			src += 2;
		}
		else if (strcmp(s, "-wn_replay_warp") == 0) {
			wn_set_replay_cursor_mode(WN_RP_WARP_MOUSE);
			++src;
		}
		else
			*dst++ = *src++;
	}
	*dst = NULL;
	argc = dst - argv;
#ifdef SUNVIEW
	argc = _wn_grab_sun_args(argc, argv);
#endif /* SUNVIEW */
	return argc;
}

int
wn_open_stdwin()
{
	return wn_create_window(Appname);
}

/*  return sys_errlist[errno] if in range
 */
const char *
_wn_reason()
{
	extern char *sys_errlist[];
	extern int errno, sys_nerr;

	return (errno > 0 && errno < sys_nerr) ? sys_errlist[errno]
					       : "unknown reason";
} 

#ifdef X11
static void
get_def_color(defaultname, def_pixel, def_rgb_val, argspec, color)
const char *defaultname;
xpixel_t def_pixel;
int def_rgb_val;
const char *argspec;
XColor *color;
{
	const char *colorspec;

	colorspec = (argspec != NULL) ? argspec : wn_get_default(defaultname);
	if (colorspec == NULL ||
			XParseColor(wn__Dpy, _wn_Cmap, colorspec, color) == 0) {
		color->pixel = def_pixel;

		/*  Set up a default color in case XQueryColor fails.
		 */
		color->red = color->green = color->blue = def_rgb_val;

		(void) XQueryColor(wn__Dpy, _wn_Cmap, color);
	}
}
		
/*  Wn was originally written for monochrome displays, and thus offers
 *  facilities like a XOR line mode.
 *
 *  To allow this on color displays, we have to have two contiguous pixel
 *  values, such that WN_FG = WN_BG | _wn_Planemask.
 */
static void
set_colors()
{
	XColor fg_color, bg_color;
	Visual *v;
	xpixel_t black_pixel, white_pixel;
	xplanemask_t planes;
	bool want_mono;
	xpixel_t rw_pixel;

#ifdef X11
	_wn_Cmap = DefaultColormap(wn__Dpy, DefaultScreen(wn__Dpy));
#endif /* X11 */

	black_pixel = BlackPixel(wn__Dpy, DefaultScreen(wn__Dpy));
	white_pixel = WhitePixel(wn__Dpy, DefaultScreen(wn__Dpy));

	get_def_color("Foreground", black_pixel, 0x0000,
						Fg_color_name, &fg_color);
	get_def_color("Background", white_pixel, 0xffff,
						Bg_color_name, &bg_color);

	if (Want_reverse_video) {
		XColor tmp;

		tmp = fg_color;
		fg_color = bg_color;
		bg_color = tmp;
	}

	/*  The -mono flag under X means "do not disturb the color map".
	 *  This is useful for example if you are using ups to debug a
	 *  color allocation problem - you don't want ups to perturb the
	 *  color map by allocating cells.
	 */
	v = DefaultVisual(wn__Dpy, DefaultScreen(wn__Dpy));
	want_mono = wn__Use_mono || (v->class != PseudoColor &&
						    v->class != DirectColor);

	if (!want_mono) {
		if (XAllocColorCells(wn__Dpy, _wn_Cmap, TRUE,
						&planes, 1, &rw_pixel, 1) == 0) {
			fprintf(stderr, "%s: Using monochrome as there are no free colormap entries\n", Appname);

			want_mono = wn__Use_mono = TRUE;
		}
		
	}

	if (want_mono) {
		/*  The code to handle StaticGray and GrayScale visuals
		 *  sent to me by Jur van der Burg <vdburg@utrtsc.enet.dec.com>.
		 *  I an unable to test it as we have no displays of this
		 *  type locally.
		 */
		if (v->class == StaticGray || v->class == GrayScale) {
			int i, j;

			j = DisplayPlanes(wn__Dpy, DefaultScreen(wn__Dpy));
			for (i = 0, planes = 1; i < j; ++i)
				planes <<= 1;
			--planes;
		}
		else {
			planes = 1;
		}

		if (wn__Use_mono ||
				XAllocColor(wn__Dpy, _wn_Cmap, &fg_color) == 0)
			fg_color.pixel = Want_reverse_video ? white_pixel
							    : black_pixel;

		if (wn__Use_mono ||
				XAllocColor(wn__Dpy, _wn_Cmap, &bg_color) == 0)
			bg_color.pixel = Want_reverse_video ? black_pixel
							    : white_pixel;

		_wn_Fg_pixel = fg_color.pixel;
		_wn_Bg_pixel = bg_color.pixel;

		/*  Wn has the assumption that logical operations work
		 *  on monochrome displays as if a set pixel was a one
		 *  bit.  We have to frig things to make this work if
		 *  the foreground pixel is zero.
		 *
		 *  BUG: what if the fg and bg pixels differ by more than
		 *       one bit?
		 */
		if (_wn_Fg_pixel < _wn_Bg_pixel)
			_wn_fix_ropfuncs();
	}
	else {
		fg_color.pixel = _wn_Fg_pixel = rw_pixel | planes;
		bg_color.pixel = _wn_Bg_pixel = rw_pixel;
#ifdef X11
		fg_color.flags = bg_color.flags = DoRed | DoGreen | DoBlue;
#endif /* X11 */
		XStoreColor(wn__Dpy, _wn_Cmap, &fg_color);
		XStoreColor(wn__Dpy, _wn_Cmap, &bg_color);
	}

#ifdef X11
	wn__set_x11_cursor_colors(&fg_color, &bg_color);
#endif /* X11 */

	_wn_Planemask = planes;
}

/*  Open the display, and set up an error handler which aborts with a core
 *  dump rather than just exiting.
 */
static Display *
open_display(displayname, p_errmsg)
const char *displayname;
const char **p_errmsg;
{
	static char errbuf[128];
	static Display *display;
	static int need_display = TRUE;
	static char deq[] = "DISPLAY=";
	const char *real_displayname;
	char *buf;
	int display_fd;

	if (need_display) {
		display = XOpenDisplay(displayname);
		if (display == NULL) {
			const char *name;

			if ((name = displayname) == NULL)
				name = getenv("DISPLAY");
			if (name != NULL)
				sprintf(errbuf, "Can't open display %s", name);
			else
				strcpy(errbuf, "Can't open display (DISPLAY environment variable unset)");
		}
		else {
#ifdef X11
			if (getenv("XSYNCHRONISE") != NULL)
				(void) XSynchronize(display, TRUE);
			real_displayname = DisplayString(display);
			display_fd = ConnectionNumber(display);
#endif /* X11 */

			buf = wn__e_malloc(strlen(real_displayname) + sizeof(deq));
			(void) strcpy(buf, deq);
			(void) strcat(buf, real_displayname);
			(void) putenv(buf);

			if (add_wm_fd(display_fd, errbuf) != 0) {
				XCloseDisplay(display);
				display = NULL;
			}
		}
		need_display = FALSE;
	}
	if (p_errmsg != NULL)
		*p_errmsg = (display != NULL) ? NULL : errbuf;
	return display;
}
#endif /* X11 */

static int
add_wm_fd(fd, errbuf)
int fd;
char *errbuf;
{
	if (fcntl(fd, F_SETFD, 1) != 0) {
#ifdef SVR4
		/*  Richard Almeida says F_SETOWN fails on almost all SVR4
		 *  systems.
		 */
#else
		if (errbuf != NULL) {
			sprintf(errbuf,
				"Can't set close-on-exec flag for display fd %d (%s)\n",
				fd, _wn_reason());
		}
		return -1;
#endif
	}

	if (fcntl(fd, F_SETOWN, getpid()) != 0) {
		if (errbuf != NULL) {
			sprintf(errbuf,
				"Can't set ownership of display fd %d (%s)\n",
			 	fd, _wn_reason());
		}
		return -1;
	}

	_wn_change_wn_fdmask(fd);
	return 0;
}

#ifdef NO_PUTENV
/*  Define putenv for machines the don't have it in the standard library.
 */
static int
my_putenv(s)
const char *s;
{
	int nlen;
	const char *cptr;
	const char **nenv, **eptr;
	extern const char **environ;

	/*  First see if there is an existing 'name=value' with the
	 *  same name as s.
	 */
	for (cptr = s; *cptr != '=' && *cptr != '\0'; cptr++)
		;
	if (*cptr == '=' && cptr > s) {
		nlen = cptr - s + 1;
		for (eptr = environ; *eptr != NULL; eptr++) {
			if (strncmp(*eptr, s, nlen) == 0) {
				*eptr = s;
				return 0;
			}
		}
	}
	
	/*  New name, so must change environ.
	 */
	for (eptr = environ; *eptr != NULL; eptr++)
		;
	nenv = (const char **) malloc((eptr - environ + 2) * sizeof(char *));
	if (nenv == NULL)
		return -1;
	eptr = environ;
	environ = nenv;
	while ((*nenv++ = *eptr++) != NULL)
		;
	*nenv = s;
	nenv[1] = NULL;
	return 0;
}
#endif /* NO_PUTENV */

#ifdef X11
static void
create_gc()
{
	XGCValues   gcv;
	Window rootwin;
#define GCMASK	(GCForeground | GCBackground)

	rootwin = RootWindow(wn__Dpy, DefaultScreen(wn__Dpy));

	gcv.foreground = _wn_Fg_pixel;
	gcv.background = _wn_Bg_pixel;
	_wn_Gc = XCreateGC(wn__Dpy, rootwin, GCMASK, &gcv);
}

void
wn_get_X11_info(p_display, p_win, p_gc, p_colormap)
Display **p_display;
Window *p_win;
GC *p_gc;
Colormap *p_colormap;
{
	_wn_init();
	*p_display = wn__Dpy;
	*p_gc = _wn_Gc;
	*p_win = wn_is_open(WN_STDWIN) ? WN_TO_W(WN_STDWIN)->w_win : 0;
	*p_colormap = _wn_Cmap;
}
#endif /* X11 */

const char *
wn_open_display()
{
	const char *errmsg;

	(void) _wn_init();
#ifdef X11
	(void) open_display(Default_display, &errmsg);
#endif
#ifdef SUNVIEW
	if (getenv("WINDOW_PARENT") != NULL)
		errmsg = NULL;
	else
		errmsg =
		      "Can't open window (WINDOW_PARENT environment variable unset)";
#endif
	return errmsg;
}

/*  Do any initialisation prior to creating the main window.
 *  Under X, this means making a connection to the server.
 *
 *  This function is called once before opening any windows.
 *
 *  We return -1 for failure, 0 for success.
 */
int
_wn_init()
{
#ifdef SUNVIEW
	int (*win_errorhandler())();
	int wakeup_fd;
#endif /* SUNVIEW */
	static int done_init = FALSE;

	if (done_init)
		return 0;
#ifdef X11
	if ((wn__Dpy = open_display(Default_display, (const char **)NULL)) == NULL)
		return -1;
	set_colors();
	create_gc();
	if (getenv("XDEBUG") != NULL)
		_Xdebug = TRUE;
#endif /* X11 */
#ifdef SUNVIEW
	Old_sun_error_handler = win_errorhandler(my_error_handler);
	_wn_Planemask = 1;
	if ((wakeup_fd = wn__setup_sunview_wakeup_pipe()) != -1)
		_wn_set_sunview_wakeup_fd(wakeup_fd);
#endif /* SUNVIEW */
	if (getenv("WN_DEBUG") != NULL)
		wn__Batching_enabled = FALSE;
	_wn_define_machine_bitmap_format();
	done_init = TRUE;
	return 0;
}

/*  Create the standard window, and fill in the machine dependent
 *  parts of w (usually the window identifier).
 *  Also fill in the size and root relative position of the window.
 *
 *  Since this function is completely machine dependent, we have
 *  seperate versions for each system.
 *
 *  Return -1 for failure, 0 for success.
 */

#ifdef SUNVIEW
void
_wn_make_retained_pixrect(pw, width, height)
struct pixwin *pw;
int width, height;
{
	struct pixrect *pr;
	struct mprp_data *mp;
	int depth;
	extern struct pixrectops mem_ops;

	depth = pw->pw_pixrect->pr_depth;
	pr = pw->pw_prretained;
	
	if (depth == 8) {
		if (pr != NULL) {
			mp = (struct mprp_data *) pr->pr_data;
			free((char *)mp->mpr.md_image);
		}
		else {
			pr = (struct pixrect *) wn__e_malloc(sizeof(struct pixrect));
			pr->pr_ops = &mem_ops;
			pr->pr_depth = depth;
			mp = (struct mprp_data *)
					wn__e_malloc(sizeof(struct mprp_data));
			mp->mpr.md_offset.x = 0;
			mp->mpr.md_offset.y = 0;
			mp->mpr.md_primary = 1;
			mp->mpr.md_flags = MP_PLANEMASK;
			mp->planes = 0xff;
			pr->pr_data = (char *) mp;
			pw->pw_prretained = pr;
		}
		pr->pr_size.x = width;
		pr->pr_size.y = height;
		mp->mpr.md_linebytes = width + (width & 1);
		mp->mpr.md_image = (short *) wn__e_malloc(mp->mpr.md_linebytes *
									height);
	}
	else {
		if (pr != NULL)
			pr_destroy(pr);
		pw->pw_prretained = mem_create(width, height, depth);
	}
}

/*  Sunview version of _wn_make_window().
 */
int
_wn_make_window(w, name, is_mainwin)
swin_t *w;
const char *name;
int is_mainwin;
{
	char wname[WIN_NAMESIZE];
	int pfd, wfd;
	int planes;
	struct rect wr;
	extern swin_t *_wn_Fdtab[];
	
	if (is_mainwin && Want_blanket) {
		pfd = (we_getgfxwindow(wname) != 0) ? -1 : open(wname, O_RDWR);
		if (pfd == -1)
			return -1;
		wfd = win_getnewwindow();
		if (wfd == -1 || win_insertblanket(wfd, pfd) == -1)
			return -1;
		close(pfd);
	}
	else
		wfd = _wn_create_sun_window(name);
	(void) fcntl(wfd, F_SETFD, 1);

	if ((w->w_pw = pw_open(wfd)) == NULL) {
		close(wfd);
		return -1;
	}

	if (wn__Use_mono || w->w_pw->pw_pixrect->pr_depth == 1)
		pw_use_fast_monochrome(w->w_pw);
	else
		_wn_init_sunview_colors(w, is_mainwin);

	win_getsize(wfd, &wr);
	w->w_width = wr.r_width;
	w->w_height = wr.r_height;
	w->w_bw->bw_resized = FALSE;

	_wn_make_retained_pixrect(w->w_pw, wr.r_width, wr.r_height);
	pr_putattributes(w->w_pw->pw_prretained, &w->w_bw->bw_planes);

	if (is_mainwin)
		signal(SIGWINCH, _wn_catch_sigwinch);

	w->w_bw->bw_can_refresh = FALSE;
	pw_exposed(w->w_pw);
	w->w_bw->bw_can_refresh = TRUE;
	pw_getattributes(w->w_pw, &w->w_bw->bw_planes);
	w->w_bw->bw_planes &= 0xff;

	_wn_Fdtab[wfd] = w;
	_wn_change_wn_fdmask(wfd);
	return 0;
}
#endif /* SUNVIEW */

#ifdef X11
static const char *
get_xdef(name, def)
const char *name, *def;
{
	char *buf;
	const char *res;

	if (name == NULL || *name == '\0')
		return wn_get_default(def);
	
	buf = wn__e_malloc(strlen(name) + 1 + strlen(def) + 1);
	(void) sprintf(buf, "%s.%s", name, def);
	res = wn_get_default(buf);
	free(buf);
	return res;
}

/*  X windows version of _wn_make_window().
 */
int
_wn_make_window(w, name, is_mainwin)
swin_t *w;
const char *name;
int is_mainwin;
{
	XWindowAttributes infobuf;
	Window bwin;
	const char *geometry, *wins;

	if (is_mainwin && Stdwin_geometry != NULL)
		geometry = Stdwin_geometry;
	else
		geometry = get_xdef(name, "Geometry");

	if (is_mainwin && (Want_blanket || wn_get_default("Blanket") != NULL)) {
		if ((wins = getenv("WINDOWID")) != NULL) {
			bwin = atoi(wins);
			XGetWindowAttributes(wn__Dpy, bwin, &infobuf);
		}
	}
	else {
		memset((char *)&infobuf, '\0', sizeof(infobuf));
	}

	return _wn_create_x_window(w, name, infobuf.x, infobuf.y,
						infobuf.width, infobuf.height,
						infobuf.border_width, geometry);
}
#endif /* X11 */

#ifdef X11
/*  This version of parse_x11_geometry is from a version sent to
 *  me by der Mouse (<mouse@thunder.mcrcim.mcgill.edu>).
 *  I have reformatted and modified it a bit.
 */
static void
parse_x11_geometry(xsh, geometry)
XSizeHints *xsh;
const char *geometry;
{
	int x, y, flags;
	unsigned w, h;

	flags = XParseGeometry(geometry, &x, &y, &w, &h);

	if ((flags & WidthValue) != 0)
		xsh->width = w;
	if ((flags & HeightValue) != 0)
		xsh->height = h;
	
	/*  When XNegative, x still gets a negative number!  (Similarly y.)
	 *
	 *  The 4s are 2 * border_width, and border width is hardwired (!)
	 *  to 2 in _wn_create_x_window() - der Mouse.
	 */
	if ((flags & XValue) != 0) {
		if ((flags & XNegative) != 0) {
			xsh->x = DisplayWidth(wn__Dpy, DefaultScreen(wn__Dpy))
							   - xsh->width - 4 + x;
		}
		else {
			xsh->x = x;
     		}
	}

	if ((flags & YValue) != 0) {
		if ((flags & YNegative) != 0) {
			xsh->y = DisplayHeight(wn__Dpy,DefaultScreen(wn__Dpy)) -
						     xsh->height - 4 + y;
		}
		else {
			xsh->y = y;
		}
	}

	if ((flags & (XValue | YValue)) != 0)
		xsh->flags = (xsh->flags & ~PPosition) | USPosition;
	
	if ((flags & (WidthValue | HeightValue)) != 0)
		xsh->flags = (xsh->flags & ~PSize) | USSize;
}

static int
_wn_create_x_window(w, name, x, y, width, height, border_width, geometry)
swin_t *w;
const char *name;
int x, y, width, height, border_width;
const char *geometry;
{
	Window      win;		/* Window ID */
	static XSizeHints  xsh;		/* Size hints for window manager */

	/* Deal with providing the window with an initial position & size.
	 * Fill out the XSizeHints struct to inform the window manager. See
	 * Sections 9.1.6 & 10.3.
	 */
	memset((char *)&xsh, '\0', sizeof(xsh));
	xsh.flags = PPosition | PSize;
	if (width != 0) {
		xsh.x = x;
		xsh.y = y;
		xsh.width = width;
		xsh.height = height;
	}
	else {
		xsh.x = App_suggests_xpos ? App_xpos : 0;
		xsh.y = App_suggests_ypos ? App_ypos : 0;
		xsh.width = App_suggests_width ? App_width : 650;
		xsh.height = App_suggests_height ? App_height : 550;
		border_width = 2;
		if (geometry != NULL)
			parse_x11_geometry(&xsh, geometry);
		
		/*  Dubious - we want an application supplied xpos or
		 *  ypos to place the window without prompting for
		 *  position, and twm won't do this unless USPosition
		 *  is set, so set it.  Must look at ICCCM sometime.
		 */
		if (App_suggests_xpos || App_suggests_ypos)
			xsh.flags = (xsh.flags & ~PPosition) | USPosition;
	}
	App_suggests_xpos = App_suggests_ypos = FALSE;
	App_suggests_width = App_suggests_height = FALSE;

	/*  Create the Window with the information in the XSizeHints, the
	 *  border width,  and the border & background pixels. See Section 3.3.
	 */
	win = XCreateSimpleWindow(wn__Dpy,
				  RootWindow(wn__Dpy, DefaultScreen(wn__Dpy)),
				  xsh.x, xsh.y,
				  (unsigned)xsh.width, (unsigned)xsh.height,
				  (unsigned)border_width,
				  (xpixel_t)_wn_Fg_pixel, (xpixel_t)_wn_Bg_pixel);

	/*  Set the standard properties for the window managers. See Section 9.1.
	 */
	XSetStandardProperties(wn__Dpy, win, name, name, None,
						(char **)&Appname, 1, &xsh);

	XSelectInput(wn__Dpy, win, ExposureMask);
	w->w_win = win;
	return 0;
}

void
_wn_map_X_window(w, iw)
swin_t *w, *iw;
{
	XWMHints xwmh;
	XEvent x_event;
	XClassHint class_hint;

	/*  BUG: The wn window creation stuff for X11 needs rewriting.
	 *       The whole thing is a mess.
	 */
	xwmh.initial_state = Want_to_start_as_icon ? IconicState : NormalState;
	xwmh.input = True;
	xwmh.flags = InputHint | StateHint;

	if (_wn_Icon_geometry != NULL) {
		int res;
		unsigned junk_width, junk_height;

		res = XParseGeometry(_wn_Icon_geometry, &xwmh.icon_x, &xwmh.icon_y,
						   &junk_width, &junk_height);
		if ((res & (XValue | YValue)) == (XValue | YValue))
			xwmh.flags |= IconPositionHint;
	}

	if (iw != NULL) {
		xwmh.icon_window = iw->w_win;
		xwmh.flags |= IconWindowHint;
	}


	XSetWMHints(wn__Dpy, w->w_win, &xwmh);

	if (Appclass == Default_appclass && Appname != Default_appname) {
		char *class;

		class = wn__e_malloc(strlen(Appname) + 1);
		strcpy(class, Appname);
		if (islower(*class))
			*class = toupper(*class);
		Appclass = class;
	}

	class_hint.res_name = (char *)Appname;
	class_hint.res_class = (char *)Appclass;
	XSetClassHint(wn__Dpy, w->w_win, &class_hint);

	if (Map_windows_on_create) {
		XMapWindow(wn__Dpy, w->w_win);
		if (!Want_to_start_as_icon) {
			do {
				XNextEvent(wn__Dpy, &x_event);
			} while (x_event.type != Expose);
		}
	}
	wn__update_size(w);
}
#endif /* X11 */

void
wn_suggest_window_position(x, y)
int x, y;
{
	App_suggests_xpos = x != -1;
	App_xpos = x;
	App_suggests_ypos = y != -1;
	App_ypos = y;
}

void
wn_suggest_window_size(width, height)
int width, height;
{
	App_suggests_width = width > 0;
	App_width = width;
	App_suggests_height = height > 0;
	App_height = height;
}

void
_wn_dont_map_windows_on_create()
{
#ifdef X11
	Map_windows_on_create = FALSE;
#endif
}

void
_wn_map_iclguide_window(wn)
int wn;
{
#ifdef X11
	swin_t *w;
	Window win;
	XSetWindowAttributes xwa;

	W_CHECK(wn);
	w = WN_TO_W(wn);

	xwa.override_redirect = TRUE;
	XChangeWindowAttributes(wn__Dpy, w->w_win, CWOverrideRedirect, &xwa);

	win = Want_to_start_as_icon ? w->w_bw->bw_assocw->w_win : w->w_win;
	w->w_bw->bw_is_mapped = !Want_to_start_as_icon;
	w->w_bw->bw_assocw->w_bw->bw_is_mapped = Want_to_start_as_icon;

	XMapWindow(wn__Dpy, win);
	wn__update_size(w);
#endif
}
