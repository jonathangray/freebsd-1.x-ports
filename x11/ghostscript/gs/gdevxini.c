/* Copyright (C) 1989, 1992, 1993 Aladdin Enterprises.  All rights reserved.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* gdevxini.c */
/* X Windows driver initialization for Ghostscript library */
#include "gx.h"			/* for gx_bitmap; includes std.h */
#include "memory_.h"
#include "math_.h"
#include "x_.h"
#include "gxdevice.h"
#include "gdevx.h"

extern char *getenv(P1(const char *));

private XtResource resources[] = {
    {XtNbackground, XtCBackground, XtRPixel, sizeof(Pixel),
     XtOffsetOf(gx_device_X, background),
     XtRString, (XtPointer)"XtDefaultBackground"},
    {XtNborderColor, XtCBorderColor, XtRPixel, sizeof(Pixel),
     XtOffsetOf(gx_device_X, borderColor),
     XtRString, (XtPointer)"XtDefaultForeground"},
    {XtNborderWidth, XtCBorderWidth, XtRDimension, sizeof(Dimension),
     XtOffsetOf(gx_device_X, borderWidth),
     XtRImmediate, (XtPointer)1},
    {"dingbatFonts", "DingbatFonts", XtRString, sizeof(String), 
     XtOffsetOf(gx_device_X, dingbatFonts),
     XtRString, "ZapfDingbats: -Adobe-ITC Zapf Dingbats-Medium-R-Normal--"},
    {XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
     XtOffsetOf(gx_device_X, foreground),
     XtRString, (XtPointer)"XtDefaultForeground"},
    {XtNgeometry, XtCGeometry, XtRString, sizeof(String), 
     XtOffsetOf(gx_device_X, geometry),
     XtRString, NULL},
    {"logExternalFonts", "LogExternalFonts", XtRBoolean, sizeof(Boolean), 
     XtOffsetOf(gx_device_X, logXFonts),
     XtRImmediate, (XtPointer)False},
    {"maxGrayRamp", "MaxGrayRamp", XtRInt, sizeof(int), 
     XtOffsetOf(gx_device_X, maxGrayRamp),
     XtRImmediate, (XtPointer)128},
    {"maxRGBRamp", "MaxRGBRamp", XtRInt, sizeof(int), 
     XtOffsetOf(gx_device_X, maxRGBRamp),
     XtRImmediate, (XtPointer)5},
    {"palette", "Palette", XtRString, sizeof(String), 
     XtOffsetOf(gx_device_X, palette),
     XtRString, "Color"},

    /* I had to compress the whitespace out of the default string to
     * satisfy certain balky compilers.
     */
    {"regularFonts", "RegularFonts", XtRString, sizeof(String), 
     XtOffsetOf(gx_device_X, regularFonts),
     XtRString, "\
AvantGarde-Book:-Adobe-ITC Avant Garde Gothic-Book-R-Normal--\n\
AvantGarde-BookOblique:-Adobe-ITC Avant Garde Gothic-Book-O-Normal--\n\
AvantGarde-Demi:-Adobe-ITC Avant Garde Gothic-Demi-R-Normal--\n\
AvantGarde-DemiOblique:-Adobe-ITC Avant Garde Gothic-Demi-O-Normal--\n\
Bookman-Demi:-Adobe-ITC Bookman-Demi-R-Normal--\n\
Bookman-DemiItalic:-Adobe-ITC Bookman-Demi-I-Normal--\n\
Bookman-Light:-Adobe-ITC Bookman-Light-R-Normal--\n\
Bookman-LightItalic:-Adobe-ITC Bookman-Light-I-Normal--\n\
Courier:-Adobe-Courier-Medium-R-Normal--\n\
Courier-Bold:-Adobe-Courier-Bold-R-Normal--\n\
Courier-BoldOblique:-Adobe-Courier-Bold-O-Normal--\n\
Courier-Oblique:-Adobe-Courier-Medium-O-Normal--\n\
Helvetica:-Adobe-Helvetica-Medium-R-Normal--\n\
Helvetica-Bold:-Adobe-Helvetica-Bold-R-Normal--\n\
Helvetica-BoldOblique:-Adobe-Helvetica-Bold-O-Normal--\n\
Helvetica-Narrow:-Adobe-Helvetica-Medium-R-Narrow--\n\
Helvetica-Narrow-Bold:-Adobe-Helvetica-Bold-R-Narrow--\n\
Helvetica-Narrow-BoldOblique:-Adobe-Helvetica-Bold-O-Narrow--\n\
Helvetica-Narrow-Oblique:-Adobe-Helvetica-Medium-O-Narrow--\n\
Helvetica-Oblique:-Adobe-Helvetica-Medium-O-Normal--\n\
NewCenturySchlbk-Bold:-Adobe-New Century Schoolbook-Bold-R-Normal--\n\
NewCenturySchlbk-BoldItalic:-Adobe-New Century Schoolbook-Bold-I-Normal--\n\
NewCenturySchlbk-Italic:-Adobe-New Century Schoolbook-Medium-I-Normal--\n\
NewCenturySchlbk-Roman:-Adobe-New Century Schoolbook-Medium-R-Normal--\n\
Palatino-Bold:-Adobe-Palatino-Bold-R-Normal--\n\
Palatino-BoldItalic:-Adobe-Palatino-Bold-I-Normal--\n\
Palatino-Italic:-Adobe-Palatino-Medium-I-Normal--\n\
Palatino-Roman:-Adobe-Palatino-Medium-R-Normal--\n\
Times-Bold:-Adobe-Times-Bold-R-Normal--\n\
Times-BoldItalic:-Adobe-Times-Bold-I-Normal--\n\
Times-Italic:-Adobe-Times-Medium-I-Normal--\n\
Times-Roman:-Adobe-Times-Medium-R-Normal--\n\
ZapfChancery-MediumItalic:-Adobe-ITC Zapf Chancery-Medium-I-Normal--"},
    {"symbolFonts", "SymbolFonts", XtRString, sizeof(String), 
     XtOffsetOf(gx_device_X, symbolFonts),
     XtRString, "Symbol: -Adobe-Symbol-Medium-R-Normal--"},
    {"useBackingPixmap", "UseBackingPixmap", XtRBoolean, sizeof(Boolean), 
     XtOffsetOf(gx_device_X, useBackingPixmap),
     XtRImmediate, (XtPointer)True},
    {"useExternalFonts", "UseExternalFonts", XtRBoolean, sizeof(Boolean), 
     XtOffsetOf(gx_device_X, useXFonts),
     XtRImmediate, (XtPointer)True},
    {"useFontExtensions", "UseFontExtensions", XtRBoolean, sizeof(Boolean), 
     XtOffsetOf(gx_device_X, useFontExtensions),
     XtRImmediate, (XtPointer)True},
    {"useScalableFonts", "UseScalableFonts", XtRBoolean, sizeof(Boolean), 
     XtOffsetOf(gx_device_X, useScalableFonts),
     XtRImmediate, (XtPointer)True},
    {"useXPutImage", "UseXPutImage", XtRBoolean, sizeof(Boolean), 
     XtOffsetOf(gx_device_X, useXPutImage),
     XtRImmediate, (XtPointer)True},
    {"useXSetTile", "UseXSetTile", XtRBoolean, sizeof(Boolean), 
     XtOffsetOf(gx_device_X, useXSetTile),
     XtRImmediate, (XtPointer)True},
    {"xResolution", "Resolution", XtRFloat, sizeof(float),
     XtOffsetOf(gx_device_X, xResolution),
     XtRString, (XtPointer)"0.0"},
    {"yResolution", "Resolution", XtRFloat, sizeof(float),
     XtOffsetOf(gx_device_X, yResolution),
     XtRString, (XtPointer)"0.0"},
};

private String
fallback_resources[] = {
    "Ghostscript*Background: white",
    "Ghostscript*Foreground: black",
    NULL
};

/* Define constants for orientation from ghostview */
/* Number represents clockwise rotation of the paper in degrees */
typedef enum {
  Portrait = 0,		/* Normal portrait orientation */
  Landscape = 90,	/* Normal landscape orientation */
  Upsidedown = 180,	/* Don't think this will be used much */
  Seascape = 270	/* Landscape rotated the wrong way */
} orientation;

/* Forward references */
private void gdev_x_setup_colors(P1(gx_device_X *));
private void gdev_x_setup_fontmap(P1(gx_device_X *));

/* Catch the alloc error when there is not enough resources for the
 * backing pixmap.  Automatically shut off backing pixmap and let the
 * user know when this happens.
 */
private Boolean alloc_error;
private XErrorHandler oldhandler;

private int
x_catch_alloc(Display *dpy, XErrorEvent *err)
{
    if (err->error_code == BadAlloc)
	alloc_error = True;
    if (alloc_error)
	return 0;
    return oldhandler(dpy, err);
}

int
x_catch_free_colors(Display *dpy, XErrorEvent *err)
{
    if (err->request_code == X_FreeColors) return 0;
    return oldhandler(dpy, err);
}

/* Open the X device */
int
gdev_x_open(register gx_device_X *xdev)
{
    XSizeHints sizehints;
    char *window_id;
    XEvent event;
    XVisualInfo xvinfo;
    int nitems;
    XtAppContext app_con;
    Widget toplevel;
    Display *dpy;
    int zero = 0;

#ifdef DEBUG
    if (gs_debug['X']) {
	extern int _Xdebug;

	_Xdebug = 1;
    }
#endif
    if (!(xdev->dpy = XOpenDisplay((char *)NULL))) {
	char *dispname = getenv("DISPLAY");

	eprintf1("gs: Cannot open X display `%s'.\n",
		 (dispname == NULL ? "(null)" : dispname));
	exit(1);
    }
    xdev->dest = 0;
    if ((window_id = getenv("GHOSTVIEW"))) {
	if (!(xdev->ghostview = sscanf(window_id, "%d %d",
				       &(xdev->win), &(xdev->dest)))) {
	    eprintf("gs: Cannot get Window ID from ghostview.\n");
	    exit(1);
	}
    }
    if (xdev->ghostview) {
	XWindowAttributes attrib;
	Atom type;
	int format;
	unsigned long nitems, bytes_after;
	char *buf;
	Atom ghostview_atom = XInternAtom(xdev->dpy, "GHOSTVIEW", False);

	if (XGetWindowAttributes(xdev->dpy, xdev->win, &attrib)) {
	    xdev->scr = attrib.screen;
	    xvinfo.visual = attrib.visual;
	    xdev->cmap = attrib.colormap;
	    xdev->width = attrib.width;
	    xdev->height = attrib.height;
	}
	/* Delete property if explicit dest is given */
	if (XGetWindowProperty(xdev->dpy, xdev->win, ghostview_atom, 0,
			       256, (xdev->dest != 0), XA_STRING,
			       &type, &format, &nitems, &bytes_after,
			       (unsigned char **)&buf) == 0 &&
	    type == XA_STRING) {
	    int llx, lly, urx, ury;
	    int left_margin = 0, bottom_margin = 0;
	    int right_margin = 0, top_margin = 0;

	    /* We declare page_orientation as an int so that we can */
	    /* use an int * to reference it for sscanf; compilers */
	    /* might be tempted to use less space to hold it if */
	    /* it were declared as an orientation. */
	    int /*orientation*/ page_orientation;
	    float xppp, yppp;	/* pixels per point */
	    nitems = sscanf(buf,
			    "%d %d %d %d %d %d %f %f %d %d %d %d",
			    &(xdev->bpixmap), &page_orientation,
			    &llx, &lly, &urx, &ury,
			    &(xdev->x_pixels_per_inch),
			    &(xdev->y_pixels_per_inch),
			    &left_margin, &bottom_margin,
			    &right_margin, &top_margin);
	    if (!(nitems == 8 || nitems == 12)) {
		eprintf("gs: Cannot get ghostview property.\n");
		exit(1);
	    }
	    if (xdev->dest && xdev->bpixmap) {
		eprintf("gs: Both destination and backing pixmap specified.\n");
		exit(1);
	    }
	    if (xdev->dest) {
		Window root;
		int x, y;
		unsigned int width, height;
		unsigned int border_width, depth;

		if (XGetGeometry(xdev->dpy, xdev->dest, &root, &x, &y,
				 &width, &height, &border_width, &depth)) {
		    xdev->width = width;
		    xdev->height = height;
		}
	    }
	    xppp = xdev->x_pixels_per_inch / 72.0;
	    yppp = xdev->y_pixels_per_inch / 72.0;
	    switch (page_orientation) {
	    case Portrait:
		xdev->initial_matrix.xx = xppp;
		xdev->initial_matrix.xy = 0.0;
		xdev->initial_matrix.yx = 0.0;
		xdev->initial_matrix.yy = -yppp;
		xdev->initial_matrix.tx = -llx * xppp;
		xdev->initial_matrix.ty = ury * yppp;
		break;
	    case Landscape:
		xdev->initial_matrix.xx = 0.0;
		xdev->initial_matrix.xy = yppp;
		xdev->initial_matrix.yx = xppp;
		xdev->initial_matrix.yy = 0.0;
		xdev->initial_matrix.tx = -lly * xppp;
		xdev->initial_matrix.ty = -llx * yppp;
		break;
	    case Upsidedown:
		xdev->initial_matrix.xx = -xppp;
		xdev->initial_matrix.xy = 0.0;
		xdev->initial_matrix.yx = 0.0;
		xdev->initial_matrix.yy = yppp;
		xdev->initial_matrix.tx = urx * xppp;
		xdev->initial_matrix.ty = -lly * yppp;
		break;
	    case Seascape:
		xdev->initial_matrix.xx = 0.0;
		xdev->initial_matrix.xy = -yppp;
		xdev->initial_matrix.yx = -xppp;
		xdev->initial_matrix.yy = 0.0;
		xdev->initial_matrix.tx = ury * xppp;
		xdev->initial_matrix.ty = urx * yppp;
		break;
	    }

	    /* The following sets the imageable area according to the */
	    /* bounding box and margins sent by ghostview.            */
	    xdev->l_margin = (llx - left_margin) / 72.0;
	    xdev->b_margin = (lly - bottom_margin) / 72.0;
	    xdev->r_margin = xdev->width / xdev->x_pixels_per_inch -
			     (urx + right_margin) / 72.0;
	    xdev->t_margin = xdev->height / xdev->y_pixels_per_inch -
			     (ury + top_margin) / 72.0;
	} else {
	    eprintf("gs: Cannot get ghostview property.\n");
	    exit(1);
	}
    } else {
	Screen *scr = DefaultScreenOfDisplay(xdev->dpy);

	xdev->scr = scr;
	xvinfo.visual = DefaultVisualOfScreen(scr);
	xdev->cmap = DefaultColormapOfScreen(scr);
    }

    xvinfo.visualid = XVisualIDFromVisual(xvinfo.visual);
    xdev->vinfo = XGetVisualInfo(xdev->dpy, VisualIDMask, &xvinfo, &nitems);
    if (xdev->vinfo == NULL) {
	eprintf("gs: Cannot get XVisualInfo.\n");
	exit(1);
    }
    /* Get X Resources.  Use the toolkit for this. */
#if !defined(XtSpecificationRelease) || XtSpecificationRelease < 5
    /* R4 and earlier toolkits may cause a Bad Access on XFreeColors. */
    oldhandler = XSetErrorHandler(x_catch_free_colors);
#endif
    XtToolkitInitialize();
    app_con = XtCreateApplicationContext();
    XtAppSetFallbackResources(app_con, fallback_resources);
    dpy = XtOpenDisplay(app_con, NULL, "ghostscript", "Ghostscript",
			NULL, 0, &zero, NULL);
    toplevel = XtAppCreateShell(NULL, "Ghostscript",
				applicationShellWidgetClass, dpy, NULL, 0);
    XtGetApplicationResources(toplevel, (XtPointer) xdev,
			      resources, XtNumber(resources), NULL, 0);
    XtDestroyWidget(toplevel);
    XtCloseDisplay(dpy);
    XtDestroyApplicationContext(app_con);
#if !defined(XtSpecificationRelease) || XtSpecificationRelease < 5
    oldhandler = XSetErrorHandler(oldhandler);
#endif

    gdev_x_setup_colors(xdev);
    gdev_x_setup_fontmap(xdev);

    if (!xdev->ghostview) {
	XWMHints wm_hints;
	XSetWindowAttributes xswa;
	gx_device *dev = (gx_device *)xdev;

	/* Take care of resolution and paper size. */
	if (xdev->x_pixels_per_inch == FAKE_RES ||
	    xdev->y_pixels_per_inch == FAKE_RES) {
	    float xsize, ysize;

	    xsize = (float)xdev->width / xdev->x_pixels_per_inch;
	    ysize = (float)xdev->height / xdev->y_pixels_per_inch;

	    if (xdev->xResolution == 0.0 && xdev->yResolution == 0.0) {
		float dpi, xdpi, ydpi;

		xdpi = 25.4 * WidthOfScreen(xdev->scr) /
		       WidthMMOfScreen(xdev->scr);
		ydpi = 25.4 * HeightOfScreen(xdev->scr) /
		       HeightMMOfScreen(xdev->scr);
		dpi = min(xdpi, ydpi);

		while (xsize*dpi > WidthOfScreen(xdev->scr)-32 ||
		       ysize*dpi > HeightOfScreen(xdev->scr)-32) {
		    dpi *= 0.95;
		}
		xdev->x_pixels_per_inch = dpi;
		xdev->y_pixels_per_inch = dpi;
	    } else {
		xdev->x_pixels_per_inch = xdev->xResolution;
		xdev->y_pixels_per_inch = xdev->yResolution;
	    }
	    if (xdev->width > WidthOfScreen(xdev->scr)) {
		xdev->width = xsize*xdev->x_pixels_per_inch;
	    }
	    if (xdev->height > HeightOfScreen(xdev->scr)) {
		xdev->height = ysize*xdev->y_pixels_per_inch;
	    }
	}

	sizehints.x = 0;
	sizehints.y = 0;
	sizehints.width = xdev->width;
	sizehints.height = xdev->height;
	sizehints.flags = 0;

	if (xdev->geometry != NULL) {
	    /*
	     * Note that border_width must be set first.  We can't use
	     * scr, because that is a Screen*, and XWMGeometry wants
	     * the screen number.
	     */
	    char gstr[40];
	    int bitmask;

	    sprintf(gstr, "%dx%d+%d+%d", sizehints.width,
		    sizehints.height, sizehints.x, sizehints.y);
	    bitmask = XWMGeometry(xdev->dpy, DefaultScreen(xdev->dpy),
				  xdev->geometry, gstr, xdev->borderWidth,
				  &sizehints,
				  &sizehints.x, &sizehints.y,
				  &sizehints.width, &sizehints.height,
				  &sizehints.win_gravity);

	    if (bitmask & (XValue | YValue))
		sizehints.flags |= USPosition;
	}

	gx_default_get_initial_matrix(dev, &(xdev->initial_matrix));

	xswa.event_mask = ExposureMask;
	xswa.background_pixel = xdev->background;
	xswa.border_pixel = xdev->borderColor;
	xswa.colormap = xdev->cmap;
	xdev->win = XCreateWindow(xdev->dpy, RootWindowOfScreen(xdev->scr),
				  sizehints.x, sizehints.y,	/* upper left */
				  xdev->width, xdev->height,
				  xdev->borderWidth,
				  xdev->vinfo->depth,
				  InputOutput,	/* class */
				  xdev->vinfo->visual,	/* visual */
				  CWEventMask | CWBackPixel |
				  CWBorderPixel | CWColormap,
				  &xswa);
	XStoreName(xdev->dpy, xdev->win, "ghostscript");
	XSetWMNormalHints(xdev->dpy, xdev->win, &sizehints);
	wm_hints.flags = InputHint;
	wm_hints.input = False;
	XSetWMHints(xdev->dpy, xdev->win, &wm_hints); /* avoid input focus */
    }

    xdev->ht.pixmap = (Pixmap) 0;
    xdev->ht.id = gx_no_bitmap_id;;
    xdev->fill_style = FillSolid;
    xdev->function = GXcopy;
    xdev->fid = (Font) 0;

    /* Set up a graphics context */
    xdev->gc = XCreateGC(xdev->dpy, xdev->win, 0, (XGCValues *) NULL);
    XSetFunction(xdev->dpy, xdev->gc, GXcopy);
    XSetLineAttributes(xdev->dpy, xdev->gc, 0,
		       LineSolid, CapButt, JoinMiter);

    gdev_x_clear_window(xdev);

    if (!xdev->ghostview) {	/* Make the window appear. */
	XMapWindow(xdev->dpy, xdev->win);

	/* Before anything else, do a flush and wait for */
	/* an exposure event. */
	XFlush(xdev->dpy);
	XNextEvent(xdev->dpy, &event);
    } else {
	/* Create an unmapped window, that the window manager will ignore.
	 * This invisible window will be used to receive "next page"
	 * events from ghostview */
	XSetWindowAttributes attributes;

	attributes.override_redirect = True;
	xdev->mwin = XCreateWindow(xdev->dpy, RootWindowOfScreen(xdev->scr),
				   0, 0, 1, 1, 0, CopyFromParent,
				   CopyFromParent, CopyFromParent,
				   CWOverrideRedirect, &attributes);
	xdev->next = XInternAtom(xdev->dpy, "NEXT", False);
	xdev->page = XInternAtom(xdev->dpy, "PAGE", False);
	xdev->done = XInternAtom(xdev->dpy, "DONE", False);
    }

    xdev->ht.no_pixmap = XCreatePixmap(xdev->dpy, xdev->win, 1, 1,
				       xdev->vinfo->depth);

    XSync(xdev->dpy, 0);
    return 0;
}

/* Allocate the backing pixmap, if any, and clear the window. */
void
gdev_x_clear_window(gx_device_X *xdev)
{
    if (!xdev->ghostview) {
	if (xdev->useBackingPixmap) {
	    oldhandler = XSetErrorHandler(x_catch_alloc);
	    alloc_error = False;
	    xdev->bpixmap =
		XCreatePixmap(xdev->dpy, xdev->win,
			      xdev->width, xdev->height,
			      xdev->vinfo->depth);
	    XSync(xdev->dpy, False);	/* Force the error */
	    if (alloc_error) {
		xdev->useBackingPixmap = False;
		eprintf("Warning: Failed to allocated backing pixmap.\n");
		if (xdev->bpixmap) {
		    XFreePixmap(xdev->dpy, xdev->bpixmap);
		    xdev->bpixmap = None;
		    XSync(xdev->dpy, False);	/* Force the error */
		}
	    }
	    oldhandler = XSetErrorHandler(oldhandler);
	} else
	    xdev->bpixmap = (Pixmap) 0;
    }
    /* Clear the destination pixmap to avoid initializing with garbage. */
    if (xdev->dest != (Pixmap) 0) {
	XSetForeground(xdev->dpy, xdev->gc, xdev->background);
	XFillRectangle(xdev->dpy, xdev->dest, xdev->gc,
		       0, 0, xdev->width, xdev->height);
    } else {
	xdev->dest = (xdev->bpixmap != (Pixmap) 0 ?
		      xdev->bpixmap : (Pixmap) xdev->win);
    }

    /* Clear the background pixmap to avoid initializing with garbage. */
    if (xdev->bpixmap != (Pixmap) 0) {
	if (!xdev->ghostview)
	    XSetWindowBackgroundPixmap(xdev->dpy, xdev->win, xdev->bpixmap);
	XSetForeground(xdev->dpy, xdev->gc, xdev->background);
	XFillRectangle(xdev->dpy, xdev->bpixmap, xdev->gc,
		       0, 0, xdev->width, xdev->height);
    }
    /* Initialize foreground and background colors */
    xdev->back_color = xdev->background;
    XSetBackground(xdev->dpy, xdev->gc, xdev->background);
    xdev->fore_color = xdev->background;
    XSetForeground(xdev->dpy, xdev->gc, xdev->background);
    xdev->colors_or = xdev->colors_and = xdev->background;
}

#if HaveStdCMap
/* get Standard colormap if available */
private XStandardColormap *
x_get_std_cmap(gx_device_X *xdev, Atom prop)
{
    int i;
    XStandardColormap *scmap, *sp;
    int nitems;

    if (XGetRGBColormaps(xdev->dpy, RootWindowOfScreen(xdev->scr),
			 &scmap, &nitems, prop))
	for (i = 0, sp = scmap; i < nitems; i++, sp++)
	    if (xdev->cmap == sp->colormap)
		return sp;

    return NULL;
}
#endif

/* Setup color mapping. */
private void
gdev_x_setup_colors(gx_device_X *xdev)
{
    char palette;

    palette = ((xdev->vinfo->class != StaticGray) &&
	       (xdev->vinfo->class != GrayScale) ? 'C' :  /* Color */
	       (xdev->vinfo->colormap_size > 2) ?  'G' :  /* GrayScale */
						   'M');  /* MonoChrome */
    if (xdev->ghostview) {
	Atom gv_colors = XInternAtom(xdev->dpy, "GHOSTVIEW_COLORS", False);
	Atom type;
	int format;
	unsigned long nitems, bytes_after;
	char *buf;

	/* Delete property if explicit dest is given */
	if (XGetWindowProperty(xdev->dpy, xdev->win, gv_colors, 0,
			       256, (xdev->dest != 0), XA_STRING,
			       &type, &format, &nitems, &bytes_after,
			       (unsigned char **)&buf) == 0 &&
	    type == XA_STRING) {
	    nitems = sscanf(buf, "%*s %d %d", &(xdev->foreground),
			    		      &(xdev->background));
	    if (nitems != 2 || *buf != 'M' && *buf != 'G' && *buf != 'C') {
		eprintf("gs: Malformed ghostview color property.\n");
		exit(1);
	    }
	    palette = max(palette, *buf);
	}
    } else {
	if      (xdev->palette[0] == 'c') xdev->palette[0] = 'C';
	else if (xdev->palette[0] == 'g') xdev->palette[0] = 'G';
	else if (xdev->palette[0] == 'm') xdev->palette[0] = 'M';
	palette = max(palette, xdev->palette[0]);
    }

    /* set up color mappings here */
    xdev->color_mask = 0xffff - (0xffff >> xdev->vinfo->bits_per_rgb);
    xdev->half_dev_color = 1 << (15 - xdev->vinfo->bits_per_rgb);
#define num_rgb (1 << xdev->vinfo->bits_per_rgb)

#if HaveStdCMap
    xdev->std_cmap = NULL;
#endif
    xdev->dither_colors = NULL;
    xdev->dynamic_colors = NULL;
    xdev->dynamic_size = 0;
    xdev->dynamic_number = 0;
    xdev->color_info.depth = xdev->vinfo->depth;

    if (palette == 'C') {
	xdev->color_info.num_components = 3;
	xdev->color_info.max_gray =
	    xdev->color_info.max_rgb = num_rgb - 1;
#if HaveStdCMap
	/* Get a standard color map if available */
	if (xdev->vinfo->visual == DefaultVisualOfScreen(xdev->scr)) {
	    xdev->std_cmap = x_get_std_cmap(xdev, XA_RGB_DEFAULT_MAP);
	} else {
	    xdev->std_cmap = x_get_std_cmap(xdev, XA_RGB_BEST_MAP);
	}
	if (xdev->std_cmap) {
	    xdev->color_info.dither_gray =
		xdev->color_info.dither_rgb =
		min(xdev->std_cmap->red_max,
		    min(xdev->std_cmap->green_max,
			xdev->std_cmap->blue_max)) + 1;
	} else
#endif
	/* Otherwise set up a rgb cube of our own */
	/* The color cube is limited to about 1/2 of the available */
	/* colormap, the user specified maxRGBRamp (usually 5), */
	/* or the number of representable colors */
#define cube(r) (r*r*r)
#define cbrt(r) pow(r, 1.0/3.0)
	{
	    int ramp_size =
			min((int)cbrt((double)xdev->vinfo->colormap_size / 2.0),
			    min(xdev->maxRGBRamp, num_rgb));

	    while (!xdev->dither_colors && ramp_size >= 2) {
		int max_rgb = ramp_size - 1;
		int r, g, b;

		xdev->color_info.dither_gray =
		    xdev->color_info.dither_rgb = ramp_size;
		xdev->dither_colors =
		    (x_pixel *) gs_malloc(sizeof(x_pixel), cube(ramp_size),
					  "gdev_x_rgb_cube");
		if (xdev->dither_colors == NULL) {
		    ramp_size--;
		    goto no_cube;
		}
		for (r = 0; r < ramp_size; r++) {
		    for (g = 0; g < ramp_size; g++) {
			for (b = 0; b < ramp_size; b++) {
			    if (r == 0 && g == 0 && b == 0) {
				xdev->dither_colors[0] = xdev->foreground;
			    } else if (r == max_rgb && g == max_rgb &&
				       b == max_rgb) {
				xdev->dither_colors[cube_index(r, g, b)] =
				    xdev->background;
			    } else {
				XColor xc;

				xc.red = (0xffff * r / max_rgb) &
					 xdev->color_mask;
				xc.green = (0xffff * g / max_rgb) &
					 xdev->color_mask;
				xc.blue = (0xffff * b / max_rgb) &
					 xdev->color_mask;
				if (XAllocColor(xdev->dpy, xdev->cmap, &xc)) {
				    xdev->dither_colors[cube_index(r, g, b)] =
					xc.pixel;
				} else {
				    eprintf3("Warning: failed to allocate %dx%dx%d RGB cube.\n",
					     ramp_size, ramp_size, ramp_size);
				    ramp_size--;
				    if (cube_index(r, g, b) - 1 > 0) {
					oldhandler =
					  XSetErrorHandler(x_catch_free_colors);
					XFreeColors(xdev->dpy, xdev->cmap,
						    xdev->dither_colors + 1,
						    cube_index(r, g, b) - 1, 0);
					/* Force any errors */
					XSync(xdev->dpy, False);
					oldhandler =
					    XSetErrorHandler(oldhandler);
				    }
				    gs_free((char *)xdev->dither_colors,
					    sizeof(x_pixel), cube(ramp_size),
					    "gdev_x_rgb_cube");
				    xdev->dither_colors = NULL;
				    goto no_cube;
				}
			    }
			}
		    }
		}
no_cube:	;
	    }

	    if (!xdev->dither_colors) {
		goto grayscale;
	    }
	}

	/* Allocate space for dynamic colors, do at most 256 */
	/* If there are more avaiaible, we ask the X server */
	xdev->dynamic_size = min(256, xdev->vinfo->colormap_size -
				      cube(xdev->color_info.dither_rgb));
	if (xdev->dynamic_size) {
	    xdev->dynamic_colors =
		(XColor *) gs_malloc(sizeof(XColor), xdev->dynamic_size,
				     "gdev_x_dynamic_colors");
	    if (xdev->dynamic_colors == NULL) {
		xdev->dynamic_size = 0;
	    }
	}
#undef cube
    } else if (palette == 'G') {
grayscale:
	xdev->color_info.num_components = 1;
	xdev->color_info.max_gray = num_rgb - 1;
#if HaveStdCMap
	/* Get a standard color map if available */
	if (xdev->std_cmap = x_get_std_cmap(xdev, XA_RGB_GRAY_MAP)) {
	    xdev->color_info.dither_gray = xdev->std_cmap->red_max +
		xdev->std_cmap->green_max +
		xdev->std_cmap->blue_max + 1;
	} else
#endif
	/* Otherwise set up a gray ramp of our own */
	/* The gray ramp is limited to about 1/2 of the available */
	/* colormap, the user specified maxGrayRamp (usually 128), */
	/* or the number of representable grays */
	{
	    int ramp_size = min(xdev->vinfo->colormap_size / 2,
				min(xdev->maxGrayRamp, num_rgb));

	    while (!xdev->dither_colors && ramp_size >= 3) {
		int max_gray = ramp_size - 1;
		int i;

		xdev->color_info.dither_gray = ramp_size;
		xdev->dither_colors =
		    (x_pixel *) gs_malloc(sizeof(x_pixel), ramp_size,
					  "gdev_x_gray_ramp");
		if (xdev->dither_colors == NULL) {
		    ramp_size--;
		    goto no_ramp;
		}
		for (i = 0; i < ramp_size; i++) {
		    XColor xc;

		    xc.red = xc.green = xc.blue =
			(0xffff * i / max_gray) & xdev->color_mask;
		    if (i == 0) {
			xdev->dither_colors[i] = xdev->foreground;
		    } else if (i == max_gray) {
			xdev->dither_colors[i] = xdev->background;
		    } else {
			if (XAllocColor(xdev->dpy, xdev->cmap, &xc)) {
			    xdev->dither_colors[i] = xc.pixel;
			} else {
			    eprintf1("Warning: failed to allocate %d level gray ramp.\n",
				     ramp_size);
			    ramp_size /= 2;
			    if (i - 1 > 0) {
				oldhandler =
					XSetErrorHandler(x_catch_free_colors);
				XFreeColors(xdev->dpy, xdev->cmap,
					    xdev->dither_colors + 1, i - 1, 0);
				XSync(xdev->dpy, False); /* Force any errors */
				oldhandler = XSetErrorHandler(oldhandler);
			    }
			    gs_free((char *)xdev->dither_colors,
				    sizeof(x_pixel), ramp_size,
				    "gdev_x_gray_ramp");
			    xdev->dither_colors = NULL;
			    goto no_ramp;
			}
		    }
		}
no_ramp:	;
	    }
	    if (!xdev->dither_colors) {
		goto monochrome;
	    }
	}

	/* Allocate space for dynamic colors, do at most 256 */
	/* If there are more avaiaible, we ask the X server */
	xdev->dynamic_size = min(256, xdev->vinfo->colormap_size -
				      xdev->color_info.dither_gray);
	if (xdev->dynamic_size) {
	    xdev->dynamic_colors =
		(XColor *) gs_malloc(sizeof(XColor), xdev->dynamic_size,
				     "gdev_x_dynamic_colors");
	    if (xdev->dynamic_colors == NULL) {
		xdev->dynamic_size = 0;
	    }
	}
    } else if (palette == 'M') {
monochrome:
	xdev->color_info.num_components = 1;
	xdev->color_info.max_gray = 1;
	xdev->color_info.dither_gray = 2;
    } else {
	eprintf1("gs: Unknown palette: %s\n", xdev->palette);
	exit(1);
    }
}

/* Setup X11 font mapping. */
private char *
get_ps_name(char **cpp, int *len)
{
    char *ret;
    *len = 0;
    /* skip over whitespace and newlines */
    while (**cpp == ' ' || **cpp == '\t' || **cpp == '\n') {
	(*cpp)++;
    }
    /* return font name up to ":", whitespace, or end of string */
    if (**cpp == ':' || **cpp == '\0') {
	return NULL;
    }
    ret = *cpp;
    while (**cpp != ':' &&
	   **cpp != ' ' && **cpp != '\t' && **cpp != '\n' &&
	   **cpp != '\0') {
	(*cpp)++;
	(*len)++;
    }
    return ret;
}

private char *
get_x11_name(char **cpp, int *len)
{
    char *ret;
    int dashes = 0;
    *len = 0;
    /* skip over whitespace and the colon */
    while (**cpp == ' ' || **cpp == '\t' ||
	   **cpp == ':') {
	(*cpp)++;
    }
    /* return font name up to end of line or string */
    if (**cpp == '\0' || **cpp == '\n') {
	return NULL;
    }
    ret = *cpp;
    while (dashes != 7 &&
	   **cpp != '\0' && **cpp != '\n') {
	if (**cpp == '-') dashes++;
	(*cpp)++;
	(*len)++;
    }
    while (**cpp != '\0' && **cpp != '\n') {
	(*cpp)++;
    }
    if (dashes != 7) return NULL;
    return ret;
}

private void
gdev_x_setup_fontmap(gx_device_X *xdev)
{
    char *ps_name;
    char *x11_name;
    int ps_name_len;
    int x11_name_len;
    x11fontmap *font;
    char *cp;

    if (!xdev->useXFonts) return;	/* If no external fonts, don't bother */

    cp = xdev->regularFonts;
    while (ps_name = get_ps_name(&cp, &ps_name_len)) {
	if (x11_name = get_x11_name(&cp, &x11_name_len)) {
	    font = (x11fontmap *)gs_malloc(sizeof(x11fontmap), 1,
					   "gdev_x_setup_fontmap");
	    if (font == NULL) continue;
	    font->ps_name = (char *)gs_malloc(sizeof(char), ps_name_len+1,
					      "gdev_x_setup_fontmap");
	    if (font->ps_name == NULL) {
		gs_free((char *)font, sizeof(x11fontmap), 1, "gdev_x_fontmap");
		continue;
	    }
	    strncpy(font->ps_name, ps_name, ps_name_len);
	    font->ps_name[ps_name_len] = '\0';
	    font->x11_name = (char *)gs_malloc(sizeof(char), x11_name_len,
					       "gdev_x_setup_fontmap");
	    if (font->x11_name == NULL) {
		gs_free(font->ps_name, sizeof(char), strlen(font->ps_name)+1,
			"gdev_x_font_psname");
		gs_free((char *)font, sizeof(x11fontmap), 1, "gdev_x_fontmap");
		continue;
	    }
	    strncpy(font->x11_name, x11_name, x11_name_len-1);
	    font->x11_name[x11_name_len-1] = '\0';
	    font->std_names = NULL;
	    font->iso_names = NULL;
	    font->std_count = -1;
	    font->iso_count = -1;
	    font->next = xdev->regular_fonts;
	    xdev->regular_fonts = font;
	}
    }

    cp = xdev->symbolFonts;
    while (ps_name = get_ps_name(&cp, &ps_name_len)) {
	if (x11_name = get_x11_name(&cp, &x11_name_len)) {
	    font = (x11fontmap *)gs_malloc(sizeof(x11fontmap), 1,
					   "gdev_x_setup_fontmap");
	    if (font == NULL) continue;
	    font->ps_name = (char *)gs_malloc(sizeof(char), ps_name_len+1,
					      "gdev_x_setup_fontmap");
	    if (font->ps_name == NULL) {
		gs_free((char *)font, sizeof(x11fontmap), 1, "gdev_x_fontmap");
		continue;
	    }
	    strncpy(font->ps_name, ps_name, ps_name_len);
	    font->ps_name[ps_name_len] = '\0';
	    font->x11_name = (char *)gs_malloc(sizeof(char), x11_name_len,
					       "gdev_x_setup_fontmap");
	    if (font->x11_name == NULL) {
		gs_free(font->ps_name, sizeof(char), strlen(font->ps_name)+1,
			"gdev_x_font_psname");
		gs_free((char *)font, sizeof(x11fontmap), 1, "gdev_x_fontmap");
		continue;
	    }
	    strncpy(font->x11_name, x11_name, x11_name_len-1);
	    font->x11_name[x11_name_len-1] = '\0';
	    font->std_names = NULL;
	    font->iso_names = NULL;
	    font->std_count = -1;
	    font->iso_count = -1;
	    font->next = xdev->symbol_fonts;
	    xdev->symbol_fonts = font;
	}
    }

    cp = xdev->dingbatFonts;
    while (ps_name = get_ps_name(&cp, &ps_name_len)) {
	if (x11_name = get_x11_name(&cp, &x11_name_len)) {
	    font = (x11fontmap *)gs_malloc(sizeof(x11fontmap), 1,
					   "gdev_x_setup_fontmap");
	    if (font == NULL) continue;
	    font->ps_name = (char *)gs_malloc(sizeof(char), ps_name_len+1,
					      "gdev_x_setup_fontmap");
	    if (font->ps_name == NULL) {
		gs_free((char *)font, sizeof(x11fontmap), 1, "gdev_x_fontmap");
		continue;
	    }
	    strncpy(font->ps_name, ps_name, ps_name_len);
	    font->ps_name[ps_name_len] = '\0';
	    font->x11_name = (char *)gs_malloc(sizeof(char), x11_name_len,
					       "gdev_x_setup_fontmap");
	    if (font->x11_name == NULL) {
		gs_free(font->ps_name, sizeof(char), strlen(font->ps_name)+1,
			"gdev_x_font_psname");
		gs_free((char *)font, sizeof(x11fontmap), 1, "gdev_x_fontmap");
		continue;
	    }
	    strncpy(font->x11_name, x11_name, x11_name_len-1);
	    font->x11_name[x11_name_len-1] = '\0';
	    font->std_names = NULL;
	    font->iso_names = NULL;
	    font->std_count = -1;
	    font->iso_count = -1;
	    font->next = xdev->dingbat_fonts;
	    xdev->dingbat_fonts = font;
	}
    }
}
