/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)screen.c	1.3 olvwm version 6/13/92"

/*
 * Based on
#ident	"@(#)screen.c	26.28	91/09/14 SMI"
 *
 */

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/Xresource.h>
#include <olgx/olgx.h>

#include "i18n.h"
#include "ollocale.h"
#include "mem.h"
#include "olwm.h"
#include "defaults.h"
#include "globals.h"
#include "resources.h"
#include "environ.h"
#include "win.h"
#include "menu.h"
#include "slots.h"
#include "virtual.h"
#include "cursors.h"

#include "iconimage.h"
#include "iconmask.h"

/*-------------------------------------------------------------------------
 *	Global Data
 *-------------------------------------------------------------------------*/
List	*ScreenInfoList;			/* List of managed screens */
extern	Bool BoolString();

/*-------------------------------------------------------------------------
 *	Local Data
 *-------------------------------------------------------------------------*/
#define gray50_width 		8		/* background gray bitmap */
#define gray50_height 		8
static unsigned char gray50_bits[] = {
    0x55, 0xaa, 0x55, 0xaa, 0x55, 0xaa, 0x55, 0xaa
};
#define busy_gray_width 	8		/* frame busy bitmap */
#define busy_gray_height 	8
static unsigned char busy_gray_bits[] = {
    0x88, 0x00, 0x22, 0x00, 0x88, 0x00, 0x22, 0x00
};

static Bool usingDefaultRootStipple;

/*
 * Set from GlyphFont; used in moveresize.c,winframe.c,winresize.c
 */
int 	Resize_height, Resize_width;

/*
 * Quarks.  The 'C' suffix indicates a class name, and the 'I' suffix 
 * indicates an instance name.  All are also suffixed with 'Q' to indicate 
 * that they're quarks.  Note that the "class" in the "visclass" variables 
 * refers to the visual class, not the resource class.
 */
static XrmQuark screenClassQ;
static XrmQuark visdepthCQ;
static XrmQuark visdepthIQ;
static XrmQuark visclassCQ;
static XrmQuark visclassIQ;
static XrmQuark visidCQ;
static XrmQuark visidIQ;
static XrmQuark cmapCQ;
static XrmQuark cmapIQ;
static XrmQuark workspaceColorCQ;
static XrmQuark workspaceColorIQ;
static XrmQuark windowColorCQ;
static XrmQuark windowColorIQ;
static XrmQuark foregroundColorCQ;
static XrmQuark foregroundColorIQ;
static XrmQuark backgroundColorCQ;
static XrmQuark backgroundColorIQ;
static XrmQuark borderColorCQ;
static XrmQuark borderColorIQ;
static XrmQuark reverseVideoCQ;
static XrmQuark reverseVideoIQ;
static XrmQuark stippledRubberBandsCQ;
static XrmQuark stippledRubberBandsIQ;

static XrmQuark	virtualForegroundColorCQ;
static XrmQuark	virtualForegroundColorIQ;
static XrmQuark	virtualBackgroundColorCQ;
static XrmQuark	virtualBackgroundColorIQ;
static XrmQuark	virtualFontColorCQ;
static XrmQuark	virtualFontColorIQ;
static XrmQuark	virtualGridColorCQ;
static XrmQuark	virtualGridColorIQ;
static XrmQuark	inputFocusColorCQ;
static XrmQuark	inputFocusColorIQ;
static XrmQuark virtualMapNameCQ;
static XrmQuark virtualMapNameIQ;
static XrmQuark virtualGeometryCQ;
static XrmQuark virtualGeometryIQ;
static XrmQuark virtualFontCQ;
static XrmQuark virtualFontIQ;
static XrmQuark virtualDesktopCQ;
static XrmQuark virtualDesktopIQ;
static XrmQuark virtualIconGeometryCQ;
static XrmQuark virtualIconGeometryIQ;
static XrmQuark virtualScaleCQ;
static XrmQuark virtualScaleIQ;
static XrmQuark virtualGridCQ;
static XrmQuark virtualGridIQ;
static XrmQuark virtualPixmapColorCQ;
static XrmQuark virtualPixmapColorIQ;


/*-------------------------------------------------------------------------
 *	Local Functions
 *-------------------------------------------------------------------------*/

/*
 * makeScreenQuarks -- set up quarks for screen resources
 */
static void
makeScreenQuarks()
{
	screenClassQ = XrmStringToQuark("Screen");

	visdepthCQ = XrmStringToQuark("Depth");
	visdepthIQ = XrmStringToQuark("depth");
	visclassCQ = XrmStringToQuark("Visual");
	visclassIQ = XrmStringToQuark("visual");
	visidCQ = XrmStringToQuark("VisualID");
	visidIQ = XrmStringToQuark("visualID");
	cmapCQ = XrmStringToQuark("Colormap");
	cmapIQ = XrmStringToQuark("colormap");
	workspaceColorCQ = XrmStringToQuark("WorkspaceColor");
	workspaceColorIQ = XrmStringToQuark("workspaceColor");
	windowColorCQ = XrmStringToQuark("WindowColor");
	windowColorIQ = XrmStringToQuark("windowColor");
	foregroundColorCQ = XrmStringToQuark("Foreground");
	foregroundColorIQ = XrmStringToQuark("foreground");
	backgroundColorCQ = XrmStringToQuark("Background");
	backgroundColorIQ = XrmStringToQuark("background");
	borderColorCQ = XrmStringToQuark("BorderColor");
	borderColorIQ = XrmStringToQuark("borderColor");
	foregroundColorIQ = XrmStringToQuark("foreground");
	reverseVideoCQ = XrmStringToQuark("ReverseVideo");
	reverseVideoIQ = XrmStringToQuark("reverseVideo");
	stippledRubberBandsCQ = XrmStringToQuark("StippledRubberBands");
	stippledRubberBandsIQ = XrmStringToQuark("stippledRubberBands");

	virtualForegroundColorCQ = XrmStringToQuark("VirtualForegroundColor");
	virtualForegroundColorIQ = XrmStringToQuark("virtualForegroundColor");
	virtualBackgroundColorCQ = XrmStringToQuark("VirtualBackgroundColor");
	virtualBackgroundColorIQ = XrmStringToQuark("virtualBackgroundColor");
	virtualFontColorCQ = XrmStringToQuark("VirtualFontColor");
	virtualFontColorIQ = XrmStringToQuark("virtualFontColor");
	virtualGridColorCQ = XrmStringToQuark("VirtualGridColor");
	virtualGridColorIQ = XrmStringToQuark("virtualGridColor");
	inputFocusColorCQ = XrmStringToQuark("InputFocusColor");
	inputFocusColorIQ = XrmStringToQuark("inputFocusColor");
	virtualMapNameCQ = XrmStringToQuark("VirtualBackgroundMap");
	virtualMapNameIQ = XrmStringToQuark("virtualBackgroundMap");
	virtualGeometryCQ = XrmStringToQuark("VirtualGeometry");
	virtualGeometryIQ = XrmStringToQuark("virtualGeometry");
	virtualDesktopCQ = XrmStringToQuark("VirtualDesktop");
	virtualDesktopIQ = XrmStringToQuark("virtualDesktop");
	virtualIconGeometryCQ = XrmStringToQuark("VirtualIconGeometry");
	virtualIconGeometryIQ = XrmStringToQuark("virtualIconGeometry");
	virtualScaleCQ = XrmStringToQuark("VirtualScale");
	virtualScaleIQ = XrmStringToQuark("virtualScale");
	virtualGridCQ = XrmStringToQuark("VirtualGrid");
	virtualGridIQ = XrmStringToQuark("virtualGrid");
	virtualPixmapColorCQ = XrmStringToQuark("VirtualPixmapColor");
	virtualPixmapColorIQ = XrmStringToQuark("virtualPixmapColor");
}

/*
 * getResource -- gets the resource value for a given instance/class quark
 *
 * Probes are made into the resource database using the following instance and 
 * class components:
 *
 * class:		Olwm.Screen.<classname>
 * instance:		<root-instance-name>.screen#.<instname>
 *
 * Returns NULL on failure
 */
static char *
getResource(scrInfo,classQ,instanceQ)
	ScreenInfo	*scrInfo;
	XrmQuark	classQ;
	XrmQuark	instanceQ;
{

	XrmQuark 	classes[4];
	XrmQuark 	instances[4];
	XrmQuark 	type;
	XrmValue 	value;

	classes[0] = TopClassQ;
	classes[1] = screenClassQ;
	classes[2] = classQ;
	classes[3] = 0;
	instances[0] = TopInstanceQ;
	instances[1] = scrInfo->instanceQ;
	instances[2] = instanceQ;
	instances[3] = 0;

	if (!XrmQGetResource(OlwmDB, instances, classes, &type, &value)) {
		return (char *)NULL;
	}
	return (char *)value.addr;
}

/*
 * isColorScreen -- check to see if a screen supports color.
 */
static Bool
isColorScreen(scrInfo,visInfo,nvisuals)
	ScreenInfo	*scrInfo;
	XVisualInfo	*visInfo;
	int		nvisuals;
{
	int		screen  = scrInfo->screen;
	int		i;

	for (i=0; i<nvisuals;  i++) {
	    	if (visInfo[i].screen == screen) {
			switch (visInfo[i].class) {
		     	case StaticColor:
		     	case PseudoColor:
			case GrayScale:
		     	case DirectColor:
		    		return True;
				/*NOTREACHED*/
		    		break;
			default:
				break;
			}
		}
	}
	return False;
}

/*
 *	use3D	- Determine whether a screen can support the 3D look
 */
static Bool
use3D(scrInfo)
	ScreenInfo	*scrInfo;
{
	int	depth = scrInfo->depth;
	int	visclass = scrInfo->visual->class;
		/* REMIND: this is illegal; visual is supposed to be opaque */
	Bool	result = True;

	/*
 	 * If 2d-look specified then use it no matter what the visual is
	 */
	if (!GRV.F3dUsed)
		return False;

	switch (visclass) {
	case StaticGray:
	case GrayScale:
		if (depth < 2)
			result = False;
		break;
	case DirectColor:
	case PseudoColor:
		if (depth < 4)
			result = False;
		break;
	case StaticColor:
		if (depth < 8)
			result = False;
		break;
	case TrueColor:
		if (depth < 6)
			result = False;
		break;
	}
	return result;
}

/*
 *	initBasic	- sets root/depth/visual/colormap basics
 */
static void
initBasic(dpy,scrInfo,visInfo,nvis)
	Display		*dpy;
	ScreenInfo	*scrInfo;
	XVisualInfo	*visInfo;
	int		nvis;
{
	char instance[MAX_NAME];

	scrInfo->rootid = RootWindow(dpy,scrInfo->screen);
	scrInfo->iscolor = isColorScreen(scrInfo,visInfo,nvis);

	sprintf(instance, "screen%d", scrInfo->screen);
	scrInfo->instanceQ = (XrmQuark) XrmStringToQuark(instance);
}


/*
 *	initVisual	- initialize screen's visual information using
 *			  data obtained from the resource database.
 *
 */
static void
initVisual(dpy, scrInfo)
    Display *dpy;
    ScreenInfo *scrInfo;
{
    char *buf;
    char *p;
    XVisualInfo vtemplate;
    unsigned int vinfomask = 0;
    XVisualInfo *vinfo = NULL;
    int nitems;

    if ((buf = getResource(scrInfo,visdepthCQ,visdepthIQ)) != NULL) {
	/* atoi returns 0 on error, which is invalid anyway */
	vtemplate.depth = atoi(buf);
	if (vtemplate.depth != 0)
	    vinfomask |= VisualDepthMask;
    }

    if ((buf = getResource(scrInfo,visclassCQ,visclassIQ)) != NULL) {
	vinfomask |= VisualClassMask;
	if (0 == strcmp(buf, "StaticGray"))
	    vtemplate.class = StaticGray;
	else if (0 == strcmp(buf, "GrayScale"))
	    vtemplate.class = GrayScale;
	else if (0 == strcmp(buf, "StaticColor"))
	    vtemplate.class = StaticColor;
	else if (0 == strcmp(buf, "PseudoColor"))
	    vtemplate.class = PseudoColor;
	else if (0 == strcmp(buf, "TrueColor"))
	    vtemplate.class = TrueColor;
	else if (0 == strcmp(buf, "DirectColor"))
	    vtemplate.class = DirectColor;
	else
	    vinfomask &= ~VisualClassMask;
    }

    if ((buf = getResource(scrInfo,visidCQ,visidIQ)) != NULL) {
	/*
	 * Note: %i converts from hex (if leading "0x"), from octal (if
	 * leading "0"), otherwise from decimal.
	 */
	if (1 == sscanf(buf, "%i", &vtemplate.visualid))
	    vinfomask |= VisualIDMask;
    }

    if (vinfomask != 0) {
	vinfomask |= VisualScreenMask;
	vtemplate.screen = scrInfo->screen;

	vinfo = XGetVisualInfo(dpy, vinfomask, &vtemplate, &nitems);
    }

    if (vinfo == NULL) {
	/* use default visual and depth */
	scrInfo->visual = DefaultVisual(dpy, scrInfo->screen);
	scrInfo->depth = DefaultDepth(dpy,scrInfo->screen);
    } else {
	/* use the first visual found -- ignore the others */
	scrInfo->visual = vinfo->visual;
	scrInfo->depth = vinfo->depth;
	XFree((char *)vinfo);
    }

#ifdef DEBUG

    printf("screen #%d visual: 0x%08x, depth=%d, class=%d%s\n",
	   scrInfo->screen,
	   scrInfo->visual->visualid,
	   scrInfo->depth,
	   scrInfo->visual->class,
	   (scrInfo->visual == DefaultVisual(dpy, scrInfo->screen))
		? " [default]" : "");

#endif /* DEBUG */
}


/*
 *	initColormap	- initialize screen's colormap
 *
 * If this screen is using the default visual, simply use the default 
 * colormap.  Otherwise, search for a standard colormap and use it instead.  
 * If one can't be found, we'll have to create one.
 */
static void
initColormap(dpy, scrInfo)
    Display *dpy;
    ScreenInfo *scrInfo;
{
    XStandardColormap *cmaps;
    int i, ncmaps;
    Colormap cm = 0;

    if (scrInfo->visual == DefaultVisual(dpy, scrInfo->screen)) {
	cm = DefaultColormap(dpy,scrInfo->screen);
#ifdef DEBUG
	printf("using default colormap (0x%x) on screen %d\n",
	       cm, scrInfo->screen);
#endif /* DEBUG */
    } else {
	if (XGetRGBColormaps(dpy, scrInfo->rootid, &cmaps, &ncmaps,
			     XA_RGB_DEFAULT_MAP)) { 
	    for (i=0; i<ncmaps; ++i) {
		if (cmaps[i].visualid == scrInfo->visual->visualid) {
		    cm = cmaps[i].colormap;
#ifdef DEBUG
		    printf("using rgb default map 0x%x for screen %d\n",
			   cm, scrInfo->screen);
#endif /* DEBUG */
		    break;
		}
	    }
	    XFree((char *) cmaps);
	}

	/*
	 * We didn't find one in the property, or there wasn't a property at 
	 * all.  We'll have to create our own colormap.
	 */
	if (cm == 0) {
	    cm = XCreateColormap(dpy, scrInfo->rootid,
				 scrInfo->visual, AllocNone);
#ifdef DEBUG
	    printf("creating colormap 0x%x for screen %d\n",
		   cm, scrInfo->screen);
#endif /* DEBUG */
	}
    }

    scrInfo->colormap = cm;
}


/*
 *	makePixmap	- make a screen pixmap from bitmapfile 
 *			  or built-in default
 */
static Bool
makePixmap(dpy,scrInfo,bitmapfile,pixmap)
	Display		*dpy;
	ScreenInfo	*scrInfo;
	char		*bitmapfile;
	Pixmap		*pixmap;		/* RETURN */
{
	Pixmap		bitmap;
	unsigned int	width,height;
	int		x,y;
	int		status = BitmapNoMemory;
	GC		gc;
	XGCValues	gcv;
	Bool		freeBitmap = False;

	/*
	 *	Read the bitmap file (which should be full path)
	 */
	if (bitmapfile[0] == '/') {
		status = XReadBitmapFile(dpy,scrInfo->rootid,bitmapfile,
				&width,&height,&bitmap,&x,&y);

		/* REMIND - should print error msg for readbitmap failure */
	}

	/*
 	 *	If that fails then use our built-in gray bitmap
 	 */
	if (status) {
	    bitmap = scrInfo->pixmap[GRAY50_BITMAP];
	    width = gray50_width;
	    height = gray50_height;
	    usingDefaultRootStipple = GRV.PaintWorkspace;
	} else {
	    freeBitmap = True;
	}
		
	/*
 	 *	Create a screen depth pixmap from the bitmap
	 */
	gcv.foreground = scrInfo->colorInfo.black;
	gcv.background = scrInfo->colorInfo.white;
	gc = XCreateGC(dpy,scrInfo->rootid,
			GCForeground|GCBackground,&gcv);
	*pixmap = XCreatePixmap(dpy,scrInfo->rootid,
			width,height,scrInfo->depth);
	XCopyPlane(dpy,bitmap,*pixmap,gc,0,0,width,height,0,0,1);
	XFreeGC(dpy,gc);

	if (freeBitmap)
	    XFreePixmap(dpy,bitmap);

	return True;
}


/*
 *	makeColor	- alloc a color using colorname or defaultcolor
 */
static Bool
makeColor(dpy,scrInfo,colorname,defaultcolor,color)
	Display		*dpy;
	ScreenInfo	*scrInfo;
	char		*colorname;
	char		*defaultcolor;
	XColor		*color;		/* RETURN */
{
	Colormap	cmap = scrInfo->colormap;

	if (!scrInfo->iscolor) 
		return False;

	if (!colorname)
		colorname = defaultcolor;

	if (!XParseColor(dpy,cmap,colorname,color)) {
		if (colorname == defaultcolor ||
		    !XParseColor(dpy,cmap,defaultcolor,color)) {
			return False;
		}
	}
	if (!XAllocColor(dpy,cmap,color))
		return False;

	/* REMIND - should print error msg for above failures */

	return True;
}


/*
 *	makeRootColor	- allocate a color for the root window
 *
 * This is necessary in addition to makeColor because olwm may be using a 
 * visual other than the default visual.  This routine allocates a color cell 
 * both from the default colormap and from the colormap for olwm's visual.
 */
static Bool
makeRootColor(dpy, scrInfo, colorname, defaultcolor)
	Display		*dpy;
	ScreenInfo	*scrInfo;
	char		*colorname;
	char		*defaultcolor;
{
	XColor olwmcolor;		
	XColor rootcolor;

	if (!scrInfo->iscolor) 
		return False;

	if (!XParseColor(dpy, scrInfo->colormap, colorname, &olwmcolor)) {
		if (colorname == defaultcolor ||
		  !XParseColor(dpy,scrInfo->colormap,defaultcolor,&olwmcolor)) {
			return False;
		}
	}
	rootcolor = olwmcolor;

	/* REMIND - should print error msg for failures below */

	if (!XAllocColor(dpy, scrInfo->colormap, &olwmcolor))
		return False;
	if (!XAllocColor(dpy,DefaultColormap(dpy,scrInfo->screen),&rootcolor)) {
		XFreeColors(dpy, scrInfo->colormap, &(olwmcolor.pixel), 1, 0);
		return False;
	}

	scrInfo->colorInfo.workspaceColor = olwmcolor.pixel;
	scrInfo->colorInfo.workspaceRootPixel = rootcolor.pixel;
	return True;
}


/*
 * setScreenWorkspaceColor - sets the workspace/root to be either
 * 	a color, a pixmap or none/default.
 */
static void
setScreenWorkspaceColor(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	Bool		update = False;
	Pixmap		pixmap;
	char		*colorname;

	if (scrInfo->colorInfo.flags & CIWorkspaceColorAlloced) {
		unsigned long	pixels[2],i=0;

		pixels[i++] = scrInfo->colorInfo.workspaceColor;
		pixels[i++] = scrInfo->colorInfo.workspaceRootPixel;
		XFreeColors(dpy,scrInfo->colormap,pixels,i,0);
		scrInfo->colorInfo.flags ^= CIWorkspaceColorAlloced;
		update = True;
	}

	colorname = getResource(scrInfo,workspaceColorCQ,workspaceColorIQ);
	if (!colorname)
		colorname = GRV.WorkspaceColor;
	if (!colorname) 
		colorname = DEFWORKSPACECOLOR;

	usingDefaultRootStipple = False;

	/*
	 *	Determine whether its a color, bitmap, or neither
	 *	If cant make a color, then try pixmap by falling thru 
	 *	If cant make a pixmap, then try none by falling thru
	 */
	switch (colorname[0]) {
	default:			/* color name string */
		scrInfo->colorInfo.workspaceType = BG_Color;
		if (makeRootColor(dpy,scrInfo,colorname,DEFWORKSPACECOLOR)) {
			scrInfo->colorInfo.flags |= CIWorkspaceColorAlloced;
			break;
		}
		/* FALL THRU */
	case '/':			/* bitmap file */
		scrInfo->colorInfo.workspaceType = BG_Pixmap;
		scrInfo->colorInfo.workspaceColor = scrInfo->colorInfo.bg0Color;
		if (makePixmap(dpy,scrInfo,colorname,&pixmap)) {
			break;
		}
		/* FALL THRU */
	case '\0':			/* null means disable root setting */
		scrInfo->colorInfo.workspaceType = BG_None;
		break;
	}

	/*
	 *	Set the root window background as appropriate
	 *	only if PaintWorkspace is True
	 */
	if (GRV.PaintWorkspace) {

		switch (scrInfo->colorInfo.workspaceType) {
		case BG_Color:			/* background color */
			XSetWindowBackground(dpy,scrInfo->rootid,
				scrInfo->colorInfo.workspaceRootPixel);
			break;
		case BG_Pixmap:			/* background pixmap */
			XSetWindowBackgroundPixmap(dpy,scrInfo->rootid,pixmap);
			XFreePixmap(dpy,pixmap);
			break;
		case BG_None:			/* mimic xsetroot -def */
			XSetWindowBackgroundPixmap(dpy,scrInfo->rootid,
								(Pixmap)None);
			break;
		}
		XClearWindow(dpy,scrInfo->rootid);
	}

	if (update)
		updateScreenWorkspaceColor(dpy,scrInfo);
}

/*
 *	setScreenWindowColor 	- computes the various window
 *				  color(s) for a screen 
 */ 
static void
setScreenWindowColor(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	Bool		update = False;
	XColor		fg,bg0,bg1,bg2,bg3;
	char		*colorname;

	if (scrInfo->colorInfo.flags & CIWindowColorAlloced) {
		unsigned long 	pixels[4],i=0;

		pixels[i++] = scrInfo->colorInfo.bg1Color;

		if (scrInfo->use3D) {
			pixels[i++] = scrInfo->colorInfo.bg2Color;
			pixels[i++] = scrInfo->colorInfo.bg3Color;
			pixels[i++] = scrInfo->colorInfo.bg0Color;
		}

		XFreeColors(dpy,scrInfo->colormap,pixels,i,0);
		scrInfo->colorInfo.flags ^= CIWindowColorAlloced;
		update = True;
	}

	colorname = getResource(scrInfo,windowColorCQ,windowColorIQ);
	if (!colorname)
		colorname = GRV.WindowColor;

	/*
 	 * 	If color screen and we can make a pixel from colorname 
	 * 	(or default) then use that pixel.
	 */
	if (scrInfo->iscolor && 
	    makeColor(dpy,scrInfo,colorname,DEFWINDOWCOLOR,&bg1)) {

		/*
		 *	If 3D mode then get all 4 bg colors
		 */
		if (scrInfo->use3D) {
			Colormap	cmap = scrInfo->colormap;

			fg.pixel = scrInfo->colorInfo.fgColor;
			XQueryColor(dpy,cmap,&fg);
	
			olgx_calculate_3Dcolors(&fg,&bg1,&bg2,&bg3,&bg0);

			/* REMIND: check return values */

			XAllocColor(dpy,cmap,&bg2);
			XAllocColor(dpy,cmap,&bg3);
			XAllocColor(dpy,cmap,&bg0);

			scrInfo->colorInfo.flags |= CIWindowColorAlloced;

			scrInfo->colorInfo.bg0Color = bg0.pixel;
			scrInfo->colorInfo.bg1Color = bg1.pixel;
			scrInfo->colorInfo.bg2Color = bg2.pixel;
			scrInfo->colorInfo.bg3Color = bg3.pixel;

		/*
		 *	Else if 2D mode then just use bg1
		 */
		} else {
			scrInfo->colorInfo.flags |= CIWindowColorAlloced;

			scrInfo->colorInfo.bg0Color = 
			scrInfo->colorInfo.bg1Color = 
			scrInfo->colorInfo.bg2Color = 
			scrInfo->colorInfo.bg3Color = bg1.pixel;
		}
	} else {
		scrInfo->colorInfo.bg0Color = 
		scrInfo->colorInfo.bg1Color = 
		scrInfo->colorInfo.bg2Color = 
		scrInfo->colorInfo.bg3Color = scrInfo->colorInfo.white;
	}

	if (update)
		updateScreenWindowColor(dpy,scrInfo);
}

/*
 *	setScreenForegroundColor - sets window foreground color for a screen
 */
static void
setScreenForegroundColor(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	Bool		update = False;
	XColor		color;
	char		*colorname;

	if (scrInfo->colorInfo.flags & CIForegroundColorAlloced) {
		unsigned long	pixel[1];
		pixel[0] = scrInfo->colorInfo.fgColor;
		XFreeColors(dpy,scrInfo->colormap,pixel,1,0);
		scrInfo->colorInfo.flags ^= CIForegroundColorAlloced;
		update = True;
	}

	colorname = getResource(scrInfo,foregroundColorCQ,foregroundColorIQ);
	if (!colorname)
		colorname = GRV.ForegroundColor;

	/*
 	 *	If color screen and we can make a pixel 
	 *	from colorname (or default) then use that pixel.
	 *	Otherwise just use black.
	 */
	if (scrInfo->iscolor &&
	    makeColor(dpy,scrInfo,colorname,DEFFOREGROUNDCOLOR,&color)) {
		scrInfo->colorInfo.flags |= CIForegroundColorAlloced;
		scrInfo->colorInfo.fgColor = color.pixel;
	} else {
		scrInfo->colorInfo.fgColor = scrInfo->colorInfo.black;
	}

	if (update)
		updateScreenForegroundColor(dpy,scrInfo);
}

/*
 *	setScreenBackgroundColor - sets window Background color for a screen
 */
static void
setScreenBackgroundColor(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	Bool		update = False;
	XColor		color;
	char		*colorname;

	if (scrInfo->colorInfo.flags & CIBackgroundColorAlloced) {
		unsigned long	pixel[1];
		pixel[0] = scrInfo->colorInfo.bgColor;
		XFreeColors(dpy,scrInfo->colormap,pixel,1,0);
		scrInfo->colorInfo.flags ^= CIBackgroundColorAlloced;
		update = True;
	}

	colorname = getResource(scrInfo,backgroundColorCQ,backgroundColorIQ);
	if (!colorname)
		colorname = GRV.BackgroundColor;

	/*
 	 *	If color screen and we can make a pixel 
	 *	from colorname (or default) then use that pixel.
	 *	Otherwise just use white.
	 */
	if (scrInfo->iscolor &&
	    makeColor(dpy,scrInfo,colorname,DEFBACKGROUNDCOLOR,&color)) {
		scrInfo->colorInfo.flags |= CIBackgroundColorAlloced;
		scrInfo->colorInfo.bgColor = color.pixel;
	} else {
		scrInfo->colorInfo.bgColor = scrInfo->colorInfo.white;
	}

	if (update)
		updateScreenBackgroundColor(dpy,scrInfo);
}

/*
 *	setScreenBorderColor - sets border color for a screen
 */
static void
setScreenBorderColor(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	Bool		update = False;
	XColor		color;
	char		*colorname;

	if (scrInfo->colorInfo.flags & CIBorderColorAlloced) {
		unsigned long	pixel[1];
		pixel[0] = scrInfo->colorInfo.borderColor;
		XFreeColors(dpy,scrInfo->colormap,pixel,1,0);
		scrInfo->colorInfo.flags ^= CIBorderColorAlloced;
		update = True;
	}

	colorname = getResource(scrInfo,borderColorCQ,borderColorIQ);
	if (!colorname)
		colorname = GRV.BorderColor;

	/*
 	 *	If color screen and doing 3d and we can make a pixel 
	 *	from colorname (or default) then use that pixel.
	 *	Otherwise just use black.
	 */
	if (scrInfo->iscolor &&
	    makeColor(dpy,scrInfo,colorname,DEFBORDERCOLOR,&color)) {
		scrInfo->colorInfo.flags |= CIBorderColorAlloced;
		scrInfo->colorInfo.borderColor = color.pixel;
	} else {
		scrInfo->colorInfo.borderColor = scrInfo->colorInfo.black;
	}

	if (update)
		updateScreenBorderColor(dpy,scrInfo);
}

/*
 *	setScreenVirtualForegroundColor - sets virtual window color for a screen
 */
void
setScreenVirtualForegroundColor(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	Bool		update = False;
	XColor		color;
	char		*colorname;

	if (scrInfo->colorInfo.flags & CIVirtualForegroundColorAlloced) {
		unsigned long	pixel[1];
		pixel[0] = scrInfo->colorInfo.virtualFgColor;
		XFreeColors(dpy,scrInfo->colormap,pixel,1,0);
		scrInfo->colorInfo.flags ^= CIVirtualForegroundColorAlloced;
		update = True;
	}

	colorname = getResource(scrInfo,
			virtualForegroundColorCQ,virtualForegroundColorIQ);
	if (!colorname)
	    colorname = GRV.VirtualForegroundColor;

	if (!colorname) {
	    if (scrInfo->iscolor)
		scrInfo->colorInfo.virtualFgColor = scrInfo->colorInfo.bg1Color;
	    else scrInfo->colorInfo.virtualFgColor = scrInfo->colorInfo.white;
	}
	else {

	    /*
 	     *	If color screen and doing 3d and we can make a pixel 
	     *	from colorname (or default) then use that pixel.
	     *	Otherwise just use white.
	     */
	    if (scrInfo->iscolor &&
	        makeColor(dpy,scrInfo,colorname,DEFVIRTUALFORECOLOR,&color)) {
		scrInfo->colorInfo.flags |= CIVirtualForegroundColorAlloced;
		scrInfo->colorInfo.virtualFgColor = color.pixel;
	    } else {
		scrInfo->colorInfo.virtualFgColor = scrInfo->colorInfo.white;
	    }
	}

	if (update)
		updateScreenVirtualForegroundColor(dpy,scrInfo);
}

/*
 *	setScreenVirtualBackgroundColor - sets virtual backgr color for a screen
 */
void
setScreenVirtualBackgroundColor(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	Bool		update = False;
	XColor		color;
	char		*colorname;

	if (scrInfo->colorInfo.flags & CIVirtualBackgroundColorAlloced) {
		unsigned long	pixel[1];
		pixel[0] = scrInfo->colorInfo.virtualBgColor;
		XFreeColors(dpy,scrInfo->colormap,pixel,1,0);
		scrInfo->colorInfo.flags ^= CIVirtualBackgroundColorAlloced;
		update = True;
	}

	colorname = getResource(scrInfo,
			virtualBackgroundColorCQ,virtualBackgroundColorIQ);
	if (!colorname)
	    colorname = GRV.VirtualBackgroundColor;

	if (!colorname) {
	    if (scrInfo->iscolor)
		scrInfo->colorInfo.virtualBgColor = scrInfo->colorInfo.bg2Color;
	    else scrInfo->colorInfo.virtualBgColor = scrInfo->colorInfo.black;
	}
	else {

	    /*
 	     *	If color screen and doing 3d and we can make a pixel 
	     *	from colorname (or default) then use that pixel.
	     *	Otherwise just use white.
	     */
	    if (scrInfo->iscolor &&
	        makeColor(dpy,scrInfo,colorname,DEFVIRTUALBACKCOLOR,&color)) {
		scrInfo->colorInfo.flags |= CIVirtualBackgroundColorAlloced;
		scrInfo->colorInfo.virtualBgColor = color.pixel;
	    } else {
		scrInfo->colorInfo.virtualBgColor = scrInfo->colorInfo.black;
	    }
	}

	if (update)
		updateScreenVirtualBackgroundColor(dpy,scrInfo);
}

/*
 *	setScreenVirtualPixmapColor - sets virtual Pixmap color for a screen
 */
void
setScreenVirtualPixmapColor(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	Bool		update = False;
	XColor		color;
	char		*colorname;

	if (scrInfo->colorInfo.flags & CIVirtualPixmapColorAlloced) {
		unsigned long	pixel[1];
		pixel[0] = scrInfo->colorInfo.virtualPixmapColor;
		XFreeColors(dpy,scrInfo->colormap,pixel,1,0);
		scrInfo->colorInfo.flags ^= CIVirtualPixmapColorAlloced;
		update = True;
	}

	colorname = getResource(scrInfo, virtualPixmapColorCQ,virtualPixmapColorIQ);
	if (!colorname)
	    colorname = GRV.VirtualPixmapColor;

	if (!colorname) {
	    if (scrInfo->iscolor)
	        scrInfo->colorInfo.virtualPixmapColor = scrInfo->colorInfo.virtualFgColor;
	}
	else {

	    /*
 	     *	If color screen and doing 3d and we can make a pixel 
	     *	from colorname (or default) then use that pixel.
	     *	Otherwise just use VirtualBGcolor.
	     */
	    if (scrInfo->iscolor &&
	        makeColor(dpy,scrInfo,colorname,DEFVIRTUALPIXMAPCOLOR,&color)) {
		scrInfo->colorInfo.flags |= CIVirtualPixmapColorAlloced;
		scrInfo->colorInfo.virtualPixmapColor = color.pixel;
	    } else {
		scrInfo->colorInfo.virtualPixmapColor = scrInfo->colorInfo.virtualFgColor;
	    }
	}

	if (update)
		updateScreenVirtualPixmapColor(dpy,scrInfo);
}

/*
 *	setScreenVirtualFontColor - sets virtual Font color for a screen
 */
void
setScreenVirtualFontColor(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	Bool		update = False;
	XColor		color;
	char		*colorname;

	if (scrInfo->colorInfo.flags & CIVirtualFontColorAlloced) {
		unsigned long	pixel[1];
		pixel[0] = scrInfo->colorInfo.virtualFontColor;
		XFreeColors(dpy,scrInfo->colormap,pixel,1,0);
		scrInfo->colorInfo.flags ^= CIVirtualFontColorAlloced;
		update = True;
	}

	colorname = getResource(scrInfo, virtualFontColorCQ,virtualFontColorIQ);
	if (!colorname)
	    colorname = GRV.VirtualFontColor;

	if (!colorname) {
	    scrInfo->colorInfo.virtualFontColor = scrInfo->colorInfo.black;
	}
	else {

	    /*
 	     *	If color screen and doing 3d and we can make a pixel 
	     *	from colorname (or default) then use that pixel.
	     *	Otherwise just use black.
	     */
	    if (scrInfo->iscolor &&
	        makeColor(dpy,scrInfo,colorname,DEFVIRTUALFONTCOLOR,&color)) {
		scrInfo->colorInfo.flags |= CIVirtualFontColorAlloced;
		scrInfo->colorInfo.virtualFontColor = color.pixel;
	    } else {
		scrInfo->colorInfo.virtualFontColor = scrInfo->colorInfo.black;
	    }
	}

	if (update)
		updateScreenVirtualFontColor(dpy,scrInfo);
}

/*
 *	setScreenVirtualGridColor - sets virtual Grid color for a screen
 */
void
setScreenVirtualGridColor(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	Bool		update = False;
	XColor		color;
	char		*colorname;

	if (scrInfo->colorInfo.flags & CIVirtualGridColorAlloced) {
		unsigned long	pixel[1];
		pixel[0] = scrInfo->colorInfo.virtualGridColor;
		XFreeColors(dpy,scrInfo->colormap,pixel,1,0);
		scrInfo->colorInfo.flags ^= CIVirtualGridColorAlloced;
		update = True;
	}

	colorname = getResource(scrInfo, virtualGridColorCQ,virtualGridColorIQ);
	if (!colorname)
	    colorname = GRV.VirtualGridColor;

	if (!colorname) {
	    if (scrInfo->iscolor)
	        scrInfo->colorInfo.virtualGridColor = scrInfo->colorInfo.black;
	}
	else {

	    /*
 	     *	If color screen and doing 3d and we can make a pixel 
	     *	from colorname (or default) then use that pixel.
	     *	Otherwise just use black.
	     */
	    if (scrInfo->iscolor &&
	        makeColor(dpy,scrInfo,colorname,DEFVIRTUALGRIDCOLOR,&color)) {
		scrInfo->colorInfo.flags |= CIVirtualGridColorAlloced;
		scrInfo->colorInfo.virtualGridColor = color.pixel;
	    } else {
		scrInfo->colorInfo.virtualGridColor = scrInfo->colorInfo.black;
	    }
	}

	if (update)
		updateScreenVirtualGridColor(dpy,scrInfo);
}

void
setScreenVirtualIconColor(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	char		*colorname;

	colorname = getResource(scrInfo,backgroundColorCQ,backgroundColorIQ);
	if (!colorname)
	    scrInfo->colorInfo.vIconColor = scrInfo->colorInfo.bg1Color;
	else scrInfo->colorInfo.vIconColor = scrInfo->colorInfo.bgColor;
}

/*
 *	setScreenInputFocusColor - sets InputFocus color for a screen
 */
void
setScreenInputFocusColor(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	Bool		update = False;
	XColor		fg, bg0, bg1, bg2, bg3;
	char		*colorname;

	if (scrInfo->colorInfo.flags & CIInputFocusColorAlloced) {
		unsigned long	pixels[4], i = 0;

		pixels[i++] = scrInfo->colorInfo.inputBg0Color;
		if (scrInfo->use3D) {
		    pixels[i++] = scrInfo->colorInfo.inputBg2Color;
		    pixels[i++] = scrInfo->colorInfo.inputBg3Color;
		    pixels[i++] = scrInfo->colorInfo.inputBg1Color;
		}
		XFreeColors(dpy,scrInfo->colormap,pixels,i,0);
		scrInfo->colorInfo.flags ^= CIInputFocusColorAlloced;
		update = True;
	}

	colorname = getResource(scrInfo, inputFocusColorCQ,inputFocusColorIQ);
	if (!colorname)
	    colorname = GRV.InputFocusColor;

	if (!colorname) {
	    scrInfo->colorInfo.inputBg0Color = scrInfo->colorInfo.bg0Color;
	    scrInfo->colorInfo.inputBg1Color = scrInfo->colorInfo.bg1Color;
	    scrInfo->colorInfo.inputBg2Color = scrInfo->colorInfo.bg2Color;
	    scrInfo->colorInfo.inputBg3Color = scrInfo->colorInfo.bg3Color;
	    scrInfo->colorInfo.virtualInputColor = scrInfo->colorInfo.virtualFgColor;
	}
	else {
	    /*
 	     *	If color screen and we can make a pixel from colornam
	     *  then use it.
	     */
	    if (scrInfo->iscolor &&
	        makeColor(dpy,scrInfo,colorname,DEFWINDOWCOLOR,&bg1)) {

		/*
		 * If 3D move then get all four colors
		 */
		if (scrInfo->use3D) {
		    Colormap	cmap = scrInfo->colormap;

		    fg.pixel = scrInfo->colorInfo.virtualFgColor;
		    XQueryColor(dpy, cmap, &fg);
		    olgx_calculate_3Dcolors(&fg, &bg1, &bg2, &bg3, &bg0);

		    /* REMIND:  Check return values */
		    XAllocColor(dpy, cmap, &bg2);
		    XAllocColor(dpy, cmap, &bg3);
		    XAllocColor(dpy, cmap, &bg0);

		    scrInfo->colorInfo.flags |= CIInputFocusColorAlloced;

		    scrInfo->colorInfo.inputBg0Color = bg0.pixel;
		    scrInfo->colorInfo.inputBg1Color = bg1.pixel;
		    scrInfo->colorInfo.inputBg2Color = bg2.pixel;
		    scrInfo->colorInfo.inputBg3Color = bg3.pixel;
	    	    scrInfo->colorInfo.virtualInputColor = bg1.pixel;
		} else {
		    /*
		     * If in 2D mode just use bg1
		     */
		    scrInfo->colorInfo.flags |= CIInputFocusColorAlloced;
		    scrInfo->colorInfo.inputBg0Color =
		    scrInfo->colorInfo.inputBg1Color =
		    scrInfo->colorInfo.inputBg2Color =
		    scrInfo->colorInfo.inputBg3Color = bg1.pixel;
	    	    scrInfo->colorInfo.virtualInputColor = bg1.pixel;
		}
	    }
	    else {
	        scrInfo->colorInfo.inputBg0Color = scrInfo->colorInfo.bg0Color;
	        scrInfo->colorInfo.inputBg1Color = scrInfo->colorInfo.bg1Color;
	        scrInfo->colorInfo.inputBg2Color = scrInfo->colorInfo.bg2Color;
	        scrInfo->colorInfo.inputBg3Color = scrInfo->colorInfo.bg3Color;
	        scrInfo->colorInfo.virtualInputColor = scrInfo->colorInfo.virtualFgColor;
	    }
	}

	if (update)
		updateScreenInputFocusColor(dpy,scrInfo);
}

/*
 *	initColors	- setups workspace/window/background colors
 */
static void
initColors(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	XColor 		color;
	unsigned long	black,white;
	char		*resStr;

	if (scrInfo->visual == DefaultVisual(dpy, scrInfo->screen)) {
		black = BlackPixel(dpy,scrInfo->screen);
		white = WhitePixel(dpy,scrInfo->screen);
	} else {
		/*
		 * Allocate black and white from this screen's colormap.
		 * REMIND: check return values from XAllocColor.
		 */
		color.red = color.green = color.blue = 0;
		(void) XAllocColor(dpy, scrInfo->colormap, &color);
		black = color.pixel;

		color.red = color.green = color.blue = 65535;
		(void) XAllocColor(dpy, scrInfo->colormap, &color);
		white = color.pixel;
	}

	if ((resStr = getResource(scrInfo,reverseVideoCQ,reverseVideoIQ))) {
		scrInfo->colorInfo.reverseVideo = BoolString(resStr, False);
	} else {
		scrInfo->colorInfo.reverseVideo = GRV.ReverseVideo;
	}

	if (scrInfo->colorInfo.reverseVideo) {
		scrInfo->colorInfo.black = white;
		scrInfo->colorInfo.white = black;
	} else {
		scrInfo->colorInfo.black = black;
		scrInfo->colorInfo.white = white;
	}

	setScreenForegroundColor(dpy,scrInfo);
	setScreenBackgroundColor(dpy,scrInfo);
	setScreenBorderColor(dpy,scrInfo);
	setScreenWindowColor(dpy,scrInfo);
	setScreenWorkspaceColor(dpy,scrInfo);

	setScreenVirtualForegroundColor(dpy,scrInfo);
	setScreenVirtualBackgroundColor(dpy,scrInfo);
	setScreenVirtualFontColor(dpy,scrInfo);
	setScreenVirtualGridColor(dpy,scrInfo);
	setScreenInputFocusColor(dpy,scrInfo);
	setScreenVirtualPixmapColor(dpy, scrInfo);
	setScreenVirtualIconColor(dpy, scrInfo);
}

/*
 *	initPixmaps	- inits the pixmaps
 */
static void
initPixmaps(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	int		result, junk;
	unsigned int	width, height;
	Pixmap		tempPixmap;

	scrInfo->pixmap[BUSY_STIPPLE] = XCreateBitmapFromData(
		dpy,scrInfo->rootid,
		(char *)busy_gray_bits,busy_gray_width,busy_gray_height);

	scrInfo->pixmap[ICON_BITMAP] = None;
	if (GRV.DefaultIconImage != NULL) {
	    if (BitmapSuccess == XReadBitmapFile(dpy, scrInfo->rootid,
		    GRV.DefaultIconImage, &width, &height,
		    &tempPixmap, &junk, &junk))
	    {
		scrInfo->pixmap[ICON_BITMAP] = tempPixmap;
		scrInfo->dfltIconWidth = width;
		scrInfo->dfltIconHeight = height;
	    }
	}

	if (scrInfo->pixmap[ICON_BITMAP] == None) {
	    scrInfo->pixmap[ICON_BITMAP] =
		XCreateBitmapFromData(dpy, scrInfo->rootid,
		(char *) iconimage_bits, iconimage_width, iconimage_height);
	    scrInfo->dfltIconWidth = iconimage_width;
	    scrInfo->dfltIconHeight = iconimage_height;
	}

	scrInfo->pixmap[ICON_MASK] = None;
	if (GRV.DefaultIconMask != NULL) {
	    if (BitmapSuccess == XReadBitmapFile(dpy, scrInfo->rootid,
		    GRV.DefaultIconMask, &width, &height,
		    &tempPixmap, &junk, &junk))
	    {
		scrInfo->pixmap[ICON_MASK] = tempPixmap;
	    }
	}

	if (scrInfo->pixmap[ICON_MASK] == None) {
	    scrInfo->pixmap[ICON_MASK] =
		XCreateBitmapFromData(dpy, scrInfo->rootid,
		    (char *) iconmask_bits, iconmask_width, iconmask_height);
	}

	scrInfo->pixmap[PROTO_DRAWABLE] =
		XCreatePixmap(dpy, scrInfo->rootid, 1, 1, scrInfo->depth);

	scrInfo->pixmap[GRAY50_BITMAP] = XCreateBitmapFromData(
		dpy, scrInfo->rootid,
		(char *)gray50_bits, gray50_width, gray50_height);
}


/*
 * initGCs - initialize all the GCs used by olwm on this screen.  This must be 
 * called after initPixmaps and initColors.
 */
static void
initGCs(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	XGCValues       values;
static	char		dashList[2] = { 1, 1 };
	Window		rootwin = scrInfo->rootid;
	unsigned long	valuemask;
	char		*resString;

	/*
	 * Set up the GC for drawing rubber-band lines on the root window.  If
	 * we painted this screen's root with the default stipple pattern,
	 * paint rubber-band lines with a stipple pattern so that they are
	 * visible against the root stipple.  This can be overridden with the
	 * StippledRubberBands screen-specific resource.  If we are doing
	 * stippled, force allplanes off, because allplanes can't support
	 * stippled graphics.
	 */
	resString = getResource(scrInfo, stippledRubberBandsCQ,
				stippledRubberBandsIQ);
	valuemask = GCFunction | GCForeground | GCSubwindowMode;
	if ((resString == NULL && usingDefaultRootStipple) ||
	    (resString != NULL && BoolString(resString, False)))
	{
#ifdef ALLPLANES
	    extern int allplanes;
	    allplanes = 0;
#endif
	    valuemask |= GCTileStipXOrigin | GCFillStyle | GCStipple;
	}

	values.fill_style = FillStippled;
	values.foreground = ~0L;		/* paint all bitplanes */
	values.function = GXxor;
	values.stipple = scrInfo->pixmap[GRAY50_BITMAP];
	values.subwindow_mode = IncludeInferiors;
	values.ts_x_origin = 1;
	scrInfo->gc[ROOT_GC] = XCreateGC(dpy, rootwin, valuemask, &values);

        /* 
	 * Create a GC for Foregound w/ TitleFont
	 */
        values.function = GXcopy;
        values.foreground = scrInfo->colorInfo.fgColor;
        values.font = GRV.TitleFontInfo->fid;
        values.graphics_exposures = False;
        scrInfo->gc[FOREGROUND_GC] = XCreateGC(dpy,
		scrInfo->pixmap[PROTO_DRAWABLE],
                (GCFont | GCFunction | GCForeground | GCGraphicsExposures),
                &values );

	/* 
	 * Create a GC for drawing the icon name and pixmap when selected
	 * (used only in 3D) and the frame border
	 */
        values.function = GXcopy;
        values.foreground = scrInfo->colorInfo.borderColor;
	values.graphics_exposures = False;
        scrInfo->gc[BORDER_GC] = XCreateGC( dpy,
		scrInfo->pixmap[PROTO_DRAWABLE],
		( GCFunction | GCForeground | GCGraphicsExposures ),
		&values );

        /* 
	 * Create a GC for drawing using the window color and title font
	 */
        values.function = GXcopy;
        values.foreground = scrInfo->colorInfo.bg1Color;
        values.font = GRV.TitleFontInfo->fid;
        values.graphics_exposures = False;
        scrInfo->gc[WINDOW_GC] = XCreateGC( dpy,
		scrInfo->pixmap[PROTO_DRAWABLE],
		( GCFunction | GCForeground | GCFont | GCGraphicsExposures ),
		&values );

	/* 
	 * Create a GC for drawing in the workspace color
	 */
	values.function = GXcopy;
	values.foreground = scrInfo->colorInfo.workspaceColor;
	values.line_width = 0;
	scrInfo->gc[WORKSPACE_GC] = XCreateGC(dpy,
		scrInfo->pixmap[PROTO_DRAWABLE],
		( GCFunction | GCForeground | GCLineWidth ),
		&values);

        /* 
	 * Create a GC for busy stipple in foreground
	 */
        values.function = GXcopy;
        values.foreground = scrInfo->colorInfo.fgColor;
	values.fill_style = FillStippled;
	values.stipple = scrInfo->pixmap[BUSY_STIPPLE];
        values.graphics_exposures = False;
        scrInfo->gc[BUSY_GC] = XCreateGC( dpy,
		scrInfo->pixmap[PROTO_DRAWABLE],
		( GCFunction | GCForeground | GCGraphicsExposures | 
		  GCStipple | GCFillStyle),
		&values );

	/* 
	 * Create a GC for drawing the icon name (just like FOREGROUND_GC, but
	 * using IconFont).  Is also used for the icon pixmap.  Hence both
	 * fg/bf are set for borderless icons (ie bg = workspace)
	 */
        values.function = GXcopy;
        values.foreground = scrInfo->colorInfo.fgColor;
	values.background = scrInfo->colorInfo.workspaceColor;
	values.font = GRV.IconFontInfo->fid;
	values.graphics_exposures = False;
        scrInfo->gc[ICON_NORMAL_GC] = XCreateGC( dpy,
		scrInfo->pixmap[PROTO_DRAWABLE],
		( GCFunction | GCForeground | GCBackground | 
		  GCFont | GCGraphicsExposures ),
		&values );

	/* 
	 * Create a GC for drawing the icon pixmap with a clip mask.
	 * Used to XCopyPlane() icon_mask and icon_pixmap into background.
	 */
        values.function = GXcopy;
        values.foreground = scrInfo->colorInfo.fgColor;
	values.background = scrInfo->colorInfo.bgColor;
	values.graphics_exposures = False;
        scrInfo->gc[ICON_MASK_GC] = XCreateGC( dpy,
		scrInfo->pixmap[PROTO_DRAWABLE],
		( GCFunction | GCForeground | GCBackground | 
		  GCGraphicsExposures ),
		&values );

	/* 
	 * Create a GC for icon border w/ dashed lines
	 */
        values.function = GXcopy;
        values.foreground = scrInfo->colorInfo.borderColor;
	values.line_width = 0;
	values.line_style = LineOnOffDash;
	values.graphics_exposures = False;
        scrInfo->gc[ICON_BORDER_GC] = XCreateGC( dpy,
		scrInfo->pixmap[PROTO_DRAWABLE],
		( GCFunction | GCForeground | GCGraphicsExposures | 
		  GCLineWidth | GCLineStyle ),
		&values );
	XSetDashes( dpy, scrInfo->gc[ICON_BORDER_GC], 1, dashList, 2 );

	/*
	 * Create a GC for the Input Focus color option -- this is the
	 * sames as BG1 if the Input Focus Color is not defined
	 */
	values.foreground = values.background = scrInfo->colorInfo.inputBg1Color;
	values.graphics_exposures = False;
	values.function = GXcopy;
        values.font = GRV.TitleFontInfo->fid;
	scrInfo->gc[INPUTFOCUS_GC] = XCreateGC(dpy,
			scrInfo->pixmap[PROTO_DRAWABLE],
			( GCFunction | GCForeground | GCBackground |
		  	  GCGraphicsExposures | GCFont ),
		 	&values);
	
	/*
	 * Create a GC for drawing in normal mode on the VDM
	 */
	scrInfo->gc[VDM_GC] = XCreateGC(dpy, scrInfo->pixmap[PROTO_DRAWABLE],
					0, NULL);
	XCopyGC(dpy, scrInfo->gc[ROOT_GC],
		GCFunction | GCPlaneMask | GCForeground | GCBackground |
		GCLineWidth | GCCapStyle | GCJoinStyle |
		GCFillRule | GCFont, scrInfo->gc[VDM_GC]);
	XSetFunction(dpy, scrInfo->gc[VDM_GC], GXcopy);
	dashList[0] = dashList[1] = 3;
	XSetDashes(dpy, scrInfo->gc[VDM_GC], 1, dashList, 2);
	XSetForeground(dpy, scrInfo->gc[VDM_GC], 
			scrInfo->colorInfo.virtualFontColor);
	XSetBackground(dpy, scrInfo->gc[VDM_GC], 
			scrInfo->colorInfo.virtualFgColor);
	dashList[0] = dashList[1] = 1;

	/*
	 * Set a GC for drawing the Input Focus window in the VDM;
	 * if we're not using the Input Focus Color option we can use
	 * the VDM_GC, otherwise we can use the INPUT_FOCUS_GC
	 */
	scrInfo->gc[VDM_INPUT_GC] = XCreateGC(dpy,
					scrInfo->pixmap[PROTO_DRAWABLE],
					0, NULL);
	XCopyGC(dpy, scrInfo->gc[VDM_GC],
		GCFunction | GCPlaneMask | GCForeground |
		GCLineWidth | GCLineStyle | GCCapStyle | GCJoinStyle |
		GCFillRule | GCFont, scrInfo->gc[VDM_INPUT_GC]);
	XSetBackground(dpy, scrInfo->gc[VDM_INPUT_GC],
					scrInfo->colorInfo.virtualInputColor);
}

/*
 * initOLGX - initialize all the olgx Graphics_info structures used by olwm
 *
 *	Creating all of these in one place will hopefully prevent the
 *	creation of redundant gis variables.  (There is some motivation for 
 *	creating olgx_gis* as they are needed, so they can also be better
 *	managed, but so far it's resulted in more gis variables than really 
 *	needed.  If we change back to that scheme, some Upd* routines for 
 *	dynamically changing resources will need to be reorganized.)
 */
static void
initOLGX(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	unsigned long pixvals[5];
	int dflag = scrInfo->use3D ? OLGX_3D_COLOR : OLGX_2D;

	/*
	 *	Common set of colors all GInfo's
	 */
	pixvals[OLGX_WHITE] = scrInfo->colorInfo.bg0Color;
	pixvals[OLGX_BG1] = scrInfo->colorInfo.bg1Color;
	pixvals[OLGX_BG2] = scrInfo->colorInfo.bg2Color;
	pixvals[OLGX_BG3] = scrInfo->colorInfo.bg3Color;
	pixvals[OLGX_BLACK] = scrInfo->colorInfo.fgColor;

	/* 
	 * Gis for drawing in window color with title font
	 *	most window objects and frame title
 	 */
	scrInfo->gi[NORMAL_GINFO] = olgx_main_initialize(dpy,
		scrInfo->screen, scrInfo->depth, dflag,
		GRV.GlyphFontInfo,
		GRV.TitleFontInfo,
		pixvals,NULL);

	/* 
	 * Gis for drawing in window color with button font
	 *	notice buttons & menu buttons
	 */
	scrInfo->gi[BUTTON_GINFO] = olgx_main_initialize(dpy,
		scrInfo->screen, scrInfo->depth, dflag,
	 	GRV.GlyphFontInfo,
		GRV.ButtonFontInfo,
		pixvals,NULL);

	/* 
	 * Gis for drawing in window color with text font
	 *	notice descriptive text and 2D resize corners
	 *
	 * NOTE: this is always in 2D, because the resize corners may be
	 * painted in 2D even if everything else is in 3D.  This relies
	 * on the fact that notice text is never truncated, so it will
	 * never require the 3D "more arrow".
	 */
	pixvals[OLGX_WHITE] = scrInfo->colorInfo.bg1Color;
	scrInfo->gi[TEXT_GINFO] = olgx_main_initialize(dpy,
		scrInfo->screen, scrInfo->depth, OLGX_2D,
	       	GRV.GlyphFontInfo,
		GRV.TextFontInfo,
		pixvals,NULL);

	/* 
	 * Gis for drawing pushpin in reverse - useful only in 2D
	 *	swap fb/bg0 entries
         */
	pixvals[OLGX_WHITE] = scrInfo->colorInfo.fgColor;
	pixvals[OLGX_BLACK] = scrInfo->colorInfo.bg0Color;

	scrInfo->gi[REVPIN_GINFO] = olgx_main_initialize(dpy,
		scrInfo->screen, scrInfo->depth, dflag,
		GRV.GlyphFontInfo,
		GRV.TitleFontInfo,
		pixvals,NULL);
	
	/*
	 * Gis for drawing glyphs when the have the input focus and
	 * the input focus color is specified
	 */

	pixvals[OLGX_WHITE] = scrInfo->colorInfo.inputBg0Color;
	pixvals[OLGX_BG1] = scrInfo->colorInfo.inputBg1Color;
	pixvals[OLGX_BG2] = scrInfo->colorInfo.inputBg2Color;
	pixvals[OLGX_BG3] = scrInfo->colorInfo.inputBg3Color;
	pixvals[OLGX_BLACK] = scrInfo->colorInfo.fgColor;

	scrInfo->gi[INPUTFOCUS_GINFO] = olgx_main_initialize(dpy,
		scrInfo->screen, scrInfo->depth, dflag,
		GRV.GlyphFontInfo,
		GRV.TitleFontInfo,
		pixvals, NULL);
}

/*
 * updateScreenWorkspaceColor -- change all GC/Ginfo's that use WorkspaceColor
 */
static
updateScreenWorkspaceColor(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	XGCValues       values;

	/*
	 * Change GC's 
	 */
	values.foreground = values.background = 
					scrInfo->colorInfo.workspaceColor;

	XChangeGC(dpy,scrInfo->gc[ICON_NORMAL_GC],GCBackground,&values);
	XChangeGC(dpy,scrInfo->gc[WORKSPACE_GC],GCForeground,&values);

	/* no Ginfo's use workspaceColor */
}

/*
 * updateScreenWindowColor -- change all GC/Ginfo's that use WindowColor
 */
static
updateScreenWindowColor(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	XGCValues       values;

	/*
	 * Change GC's 
	 */
	values.foreground = scrInfo->colorInfo.bg1Color;

	XChangeGC(dpy,scrInfo->gc[WINDOW_GC],GCForeground,&values);

	/*
	 * Change Ginfo's 
	 */
	olgx_set_single_color(scrInfo->gi[BUTTON_GINFO],
			OLGX_WHITE,scrInfo->colorInfo.bg0Color,OLGX_SPECIAL);
	olgx_set_single_color(scrInfo->gi[BUTTON_GINFO],
			OLGX_BG1,scrInfo->colorInfo.bg1Color,OLGX_SPECIAL);
	olgx_set_single_color(scrInfo->gi[BUTTON_GINFO],
			OLGX_BG2,scrInfo->colorInfo.bg2Color,OLGX_SPECIAL);
	olgx_set_single_color(scrInfo->gi[BUTTON_GINFO],
			OLGX_BG3,scrInfo->colorInfo.bg3Color,OLGX_SPECIAL);

	olgx_set_single_color(scrInfo->gi[TEXT_GINFO],
			OLGX_WHITE,scrInfo->colorInfo.bg0Color,OLGX_SPECIAL);
	olgx_set_single_color(scrInfo->gi[TEXT_GINFO],
			OLGX_BG1,scrInfo->colorInfo.bg1Color,OLGX_SPECIAL);
	olgx_set_single_color(scrInfo->gi[TEXT_GINFO],
			OLGX_BG2,scrInfo->colorInfo.bg2Color,OLGX_SPECIAL);
	olgx_set_single_color(scrInfo->gi[TEXT_GINFO],
			OLGX_BG3,scrInfo->colorInfo.bg3Color,OLGX_SPECIAL);

	olgx_set_single_color(scrInfo->gi[NORMAL_GINFO],
			OLGX_WHITE,scrInfo->colorInfo.bg0Color,OLGX_SPECIAL);
	olgx_set_single_color(scrInfo->gi[NORMAL_GINFO],
			OLGX_BG1,scrInfo->colorInfo.bg1Color,OLGX_SPECIAL);
	olgx_set_single_color(scrInfo->gi[NORMAL_GINFO],
			OLGX_BG2,scrInfo->colorInfo.bg2Color,OLGX_SPECIAL);
	olgx_set_single_color(scrInfo->gi[NORMAL_GINFO],
			OLGX_BG3,scrInfo->colorInfo.bg3Color,OLGX_SPECIAL);

	olgx_set_single_color(scrInfo->gi[REVPIN_GINFO],
			OLGX_BLACK,scrInfo->colorInfo.bg0Color,OLGX_SPECIAL);
	olgx_set_single_color(scrInfo->gi[REVPIN_GINFO],
			OLGX_BG1,scrInfo->colorInfo.bg1Color,OLGX_SPECIAL);
	olgx_set_single_color(scrInfo->gi[REVPIN_GINFO],
			OLGX_BG2,scrInfo->colorInfo.bg2Color,OLGX_SPECIAL);
	olgx_set_single_color(scrInfo->gi[REVPIN_GINFO],
			OLGX_BG3,scrInfo->colorInfo.bg3Color,OLGX_SPECIAL);
}

/*
 * updateScreenForegroundColor -- change all GC/Ginfo's that use Foreground
 */
static
updateScreenForegroundColor(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	XGCValues       values;

	/*
	 * Change GC's 
	 */
	values.foreground = scrInfo->colorInfo.fgColor;

	XChangeGC(dpy,scrInfo->gc[FOREGROUND_GC],GCForeground,&values);
	XChangeGC(dpy,scrInfo->gc[BUSY_GC],GCForeground,&values);
	XChangeGC(dpy,scrInfo->gc[ICON_NORMAL_GC],GCForeground,&values);
	XChangeGC(dpy,scrInfo->gc[ICON_MASK_GC],GCForeground,&values);

	/*
	 * Change Ginfo's 
	 */
	olgx_set_single_color(scrInfo->gi[BUTTON_GINFO],
			OLGX_BLACK,scrInfo->colorInfo.fgColor,OLGX_SPECIAL);
	olgx_set_single_color(scrInfo->gi[TEXT_GINFO],
			OLGX_BLACK,scrInfo->colorInfo.fgColor,OLGX_SPECIAL);
	olgx_set_single_color(scrInfo->gi[NORMAL_GINFO],
			OLGX_BLACK,scrInfo->colorInfo.fgColor,OLGX_SPECIAL);
	olgx_set_single_color(scrInfo->gi[REVPIN_GINFO],
			OLGX_WHITE,scrInfo->colorInfo.fgColor,OLGX_SPECIAL);
}

/*
 * updateScreenBackgroundColor -- change all GC/Ginfo's that use Background
 */
static
updateScreenBackgroundColor(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	XGCValues       values;

	values.background = scrInfo->colorInfo.bgColor;

	XChangeGC(dpy,scrInfo->gc[ICON_MASK_GC],GCBackground,&values);
}

/*
 * updateScreenBorderColor -- change all GC/Ginfo's that use Border
 */
static
updateScreenBorderColor(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	XGCValues       values;

	/*
	 * Change GC's 
	 */
	values.foreground = scrInfo->colorInfo.borderColor;

	XChangeGC(dpy,scrInfo->gc[BORDER_GC],GCForeground,&values);
	XChangeGC(dpy,scrInfo->gc[ICON_BORDER_GC],GCForeground,&values);
}

/*
 * updateScreenVirtualForegroundColor -- change all GC/Ginfo's that use vfg
 */
updateScreenVirtualForegroundColor(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	XGCValues       values;

	/*
	 * Change GC's 
	 */
	values.background = scrInfo->colorInfo.virtualFgColor;

	XChangeGC(dpy,scrInfo->gc[VDM_GC],GCBackground,&values);
	VirtualUpdateVirtualWindows(scrInfo->vdm->client);
}

/*
 * updateScreenVirtualBackgroundColor -- change all GC/Ginfo's that use vfg
 */
updateScreenVirtualBackgroundColor(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	/*
	 * No GC/Ginfo's to change
	 */
	
	XSetWindowBackground(dpy, PANEWINOFCLIENT(scrInfo->vdm->client),
			     scrInfo->colorInfo.virtualBgColor);
	XClearWindow(dpy, PANEWINOFCLIENT(scrInfo->vdm->client));
}

/*
 * updateScreenVirtualFontColor -- change all GC/Ginfo's that use vfg
 */
updateScreenVirtualFontColor(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	XGCValues       values;

	/*
	 * Change GC's 
	 */
	values.foreground = scrInfo->colorInfo.virtualFontColor;
	XChangeGC(dpy,scrInfo->gc[VDM_GC],GCForeground,&values);
	XChangeGC(dpy,scrInfo->gc[VDM_INPUT_GC],GCForeground,&values);
	ClientRefresh(scrInfo->vdm->client);
}

/*
 * updateScreenVirtualGridColor -- change all GC/Ginfo's that use vfg
 */
updateScreenVirtualGridColor(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	/*
	 * Nothing to change ...
	 */
}

/*
 * updateScreenInputFocusColor -- change all GC/Ginfo's that use vfg
 */
updateScreenInputFocusColor(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
       olgx_set_single_color(scrInfo->gi[INPUTFOCUS_GINFO], OLGX_BLACK,
			     scrInfo->colorInfo.fgColor, OLGX_SPECIAL);
       olgx_set_single_color(scrInfo->gi[INPUTFOCUS_GINFO], OLGX_BG1,
			     scrInfo->colorInfo.inputBg1Color, OLGX_SPECIAL);
       olgx_set_single_color(scrInfo->gi[INPUTFOCUS_GINFO], OLGX_BG2,
			     scrInfo->colorInfo.inputBg2Color, OLGX_SPECIAL);
       olgx_set_single_color(scrInfo->gi[INPUTFOCUS_GINFO], OLGX_BG3,
			     scrInfo->colorInfo.inputBg3Color, OLGX_SPECIAL);
       olgx_set_single_color(scrInfo->gi[INPUTFOCUS_GINFO], OLGX_WHITE,
			     scrInfo->colorInfo.inputBg0Color, OLGX_SPECIAL);
}

updateScreenVirtualDesktop(dpy,scrInfo)
    Display		*dpy;
    ScreenInfo		*scrInfo;

{
char	*size;

    size = getResource(scrInfo, virtualDesktopCQ, virtualDesktopIQ);
    if (!size)
	size = GRV.VirtualDesktop;
    ResizeVDM(scrInfo->vdm, size);
}

updateScreenVirtualIconGeometry(dpy,scrInfo)
    Display		*dpy;
    ScreenInfo		*scrInfo;

{
char	*geometry;

    geometry = getResource(scrInfo,virtualIconGeometryCQ,virtualIconGeometryIQ);
    if (!geometry)
	geometry = GRV.VirtualIconGeometry;
    if (scrInfo->vdm->resources->iconGeometry)
	free(scrInfo->vdm->resources->iconGeometry);
    scrInfo->vdm->resources->iconGeometry = strdup(geometry);
    VirtualSetGeometry(scrInfo->vdm->client->iconwin, geometry);
}

updateScreenVirtualScale(dpy,scrInfo)
    Display		*dpy;
    ScreenInfo		*scrInfo;

{
char	*scale;
char	temp[20];

    scale = getResource(scrInfo, virtualScaleCQ, virtualScaleIQ);
    if (!scale) {
	sprintf(temp, "%d", GRV.VDMScale);
	scale = temp;
    }
    RescaleVDM(scrInfo->vdm, atoi(scale));
}

/*
 * initFonts - init things that depend on the fonts
 */
static void
initFonts(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	Resize_width = Resize_height = 0;
	updateScreenGlyphFont(dpy,scrInfo);
}

/*
 * updateScreenTitleFont -- change all GC/Ginfo's that use TitleFont
 */
static
updateScreenTitleFont(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	XFontStruct	*font = GRV.TitleFontInfo;
	XGCValues       values;

	values.font = font->fid;
	XChangeGC(dpy,scrInfo->gc[FOREGROUND_GC],GCFont,&values);
	XChangeGC(dpy,scrInfo->gc[WINDOW_GC],GCFont,&values);

	olgx_set_text_font(scrInfo->gi[NORMAL_GINFO],font,OLGX_NORMAL);
	olgx_set_text_font(scrInfo->gi[REVPIN_GINFO],font,OLGX_NORMAL);
}

/*
 * updateScreenTextFont -- change all GC/Ginfo's that use TextFont
 */
static
updateScreenTextFont(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	XFontStruct	*font = GRV.TextFontInfo;

	olgx_set_text_font(scrInfo->gi[TEXT_GINFO],font,OLGX_NORMAL);
}

/*
 * updateScreenButtonFont -- change all GC/Ginfo's that use ButtonFont
 */
static
updateScreenButtonFont(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	XFontStruct	*font = GRV.ButtonFontInfo;

	olgx_set_text_font(scrInfo->gi[BUTTON_GINFO],font,OLGX_NORMAL);
}

/*
 * updateScreenIconFont -- change all GC/Ginfo's that use IconFont
 */
static
updateScreenIconFont(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	XFontStruct	*font = GRV.IconFontInfo;
	XGCValues       values;

	values.font = font->fid;
	XChangeGC(dpy,scrInfo->gc[ICON_NORMAL_GC],GCFont,&values);
}

/*
 * updateScreenGlyphFont -- change all GC/Ginfo's that use GlyphFont
 */
static
updateScreenGlyphFont(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	XFontStruct	*font = GRV.GlyphFontInfo;

	olgx_set_glyph_font(scrInfo->gi[NORMAL_GINFO],font,OLGX_NORMAL);
	olgx_set_glyph_font(scrInfo->gi[REVPIN_GINFO],font,OLGX_NORMAL);
	olgx_set_glyph_font(scrInfo->gi[TEXT_GINFO],font,OLGX_NORMAL);
	olgx_set_glyph_font(scrInfo->gi[BUTTON_GINFO],font,OLGX_NORMAL);

	/*
 	 * w/h of resize corner glyph.  Set if unset (ie 0)
	 */
	if (Resize_width == 0 && Resize_height == 0) {
		unsigned char 		s[2];
		XCharStruct	xcs;
		int 		i1,i2;

		/* US_RESIZE_OUTLINE is really unsigned char */
		s[0]=UL_RESIZE_OUTLINE;  
		s[1]='\0';
		XTextExtents(GRV.GlyphFontInfo,s,1,&i1,&i2,&i2,&xcs);
		Resize_height = xcs.ascent + xcs.descent;
		Resize_width = xcs.width;
	}
}

/*
 * updateScreenVirtualFont -- change all GC/Ginfo's that use VirtualFont
 */
updateScreenVirtualFont(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
	char		*font;
	XGCValues       values;
	XFontStruct	*fid;

	font = getResource(scrInfo,virtualFontCQ,virtualFontIQ);
	if (!font)
	    font = GRV.VirtualFontName;
	fid = XLoadQueryFont(dpy, font);
	if (!fid)
	    fid = GRV.TitleFontInfo;
	values.font = fid->fid;
	XChangeGC(dpy,scrInfo->gc[VDM_GC],GCFont,&values);
	XChangeGC(dpy,scrInfo->gc[VDM_INPUT_GC],GCFont,&values);
	scrInfo->vdm->max_ascent = fid->max_bounds.ascent;

	ClientRefresh(scrInfo->vdm->client);
}

/*
 */
updateScreenVirtualGeometry(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
char	*geom;
	
	geom = getResource(scrInfo,virtualGeometryCQ,virtualGeometryIQ);
	if (!geom)
	    geom = GRV.VirtualGeometry;
	if (!geom || !*geom)
	    return;
	if (scrInfo->vdm->resources->geometry)
	    free(scrInfo->vdm->resources->geometry);
	scrInfo->vdm->resources->geometry = strdup(geom);
	VirtualSetGeometry(scrInfo->vdm->client->framewin, geom);
}

updateScreenVirtualMap(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
Pixmap	pm;
char	*name;
XGCValues	gcv;
	
	name = getResource(scrInfo,virtualMapNameCQ,virtualMapNameIQ);
	if (!name)
	    name = GRV.VirtualBackgroundMap;
	if (!name || !*name)
	    return;
	if (scrInfo->vdm->resources->background)
	    free(scrInfo->vdm->resources->background);
	scrInfo->vdm->resources->background = strdup(name);
	pm = VirtualGetPixmap(dpy, scrInfo, name);
	if (pm != None) {
	    XSetWindowBackgroundPixmap(dpy,
			PANEWINOFCLIENT(scrInfo->vdm->client), pm);
	    XClearWindow(dpy, PANEWINOFCLIENT(scrInfo->vdm->client));
	}
}

updateScreenVirtualPixmapColor(dpy,scrInfo)
	Display		*dpy;
	ScreenInfo	*scrInfo;
{
Pixmap	pm;
char	*name;
XGCValues	gcv;
	
	if (!scrInfo->vdm->resources->background)
	    return;
	pm = VirtualGetPixmap(dpy, scrInfo, scrInfo->vdm->resources->background);
	if (pm != None) {
	    XSetWindowBackgroundPixmap(dpy,
			PANEWINOFCLIENT(scrInfo->vdm->client), pm);
	    XClearWindow(dpy, PANEWINOFCLIENT(scrInfo->vdm->client));
	}
}

/*
 *	initScreenInfo	- creates the ScreenInfo for a particular screen
 */
static void
initScreenInfo(dpy,screenno,visInfo,nvis)
	Display		*dpy;
	int		screenno;
	XVisualInfo	*visInfo;
	int		nvis;
{
	ScreenInfo	*scrInfo;
	Client		*client;

	/*
         *	Create a new ScreenInfo and minimally initialize it
 	 */
	scrInfo = MemNew(ScreenInfo);
	scrInfo->screen = screenno;
	initBasic(dpy,scrInfo,visInfo,nvis);
	initVisual(dpy,scrInfo);
	initColormap(dpy, scrInfo);
	initPixmaps(dpy,scrInfo);

	/*
	 *	Insert the proto ScreenInfo into the list so that 
	 *	ClientCreate can find it and get the client for MakeRoot
 	 */
	ScreenInfoList = ListCons(scrInfo,ScreenInfoList);
	if ((client = ClientCreate(dpy,scrInfo->screen)) == NULL)
		return;
	scrInfo->rootwin = MakeRoot(dpy,client);

	/*
 	 *	Initialize the rest of the ScreenInfo fields
	 */
	scrInfo->use3D = use3D(scrInfo);
	initColors(dpy,scrInfo);
	initGCs(dpy,scrInfo);
	initOLGX(dpy,scrInfo);
	initFonts(dpy,scrInfo);
	InitCursors(dpy,scrInfo);

	/*
	 *	Initialize the screen dependent parts of menus
	 */
	InitMenus(dpy,scrInfo);
	scrInfo->menuCache = InitScreenMenus(dpy,scrInfo);

	/* REMIND: shouldn't this be in SlotInit? */
	scrInfo->framepos = 0;

	/*
 	 *	Initialize the icon slots for this screen 
	 */
	scrInfo->iconGrid = SlotInit(dpy,screenno);

	/*
	 *	Make a new environment for this screen number
	 */
	scrInfo->environment = MakeEnviron(dpy,screenno);

	/*
 	 *	Initalize the colormap focus for screen/root
	 */
	ColorFocusInit(dpy,scrInfo->rootwin);

	initVDM(dpy, scrInfo, client);

	/*
 	 *	Set the cursor for that screen's root window
	 */
	if (GRV.PointerWorkspace)
	    XDefineCursor(dpy,scrInfo->rootid,GRV.BasicPointer);

}

initVDM(dpy, scrInfo, client)
    Display	*dpy;
    ScreenInfo	*scrInfo;
    Client	*client;
{
	char		*s;
	VirtualResources	*resources;

	/*
	 * Create VDM for screen
	 */

	resources = (VirtualResources *) MemAlloc(sizeof(VirtualResources));
        s = getResource(scrInfo,virtualDesktopCQ,virtualDesktopIQ);
    	if (!s)
	    resources->size = strdup(GRV.VirtualDesktop);
	else resources->size = strdup(s);
	s = getResource(scrInfo,virtualGeometryCQ,virtualGeometryIQ);
	if (!s)
	    resources->geometry = strdup(GRV.VirtualGeometry);
	else resources->geometry = strdup(s);
	s = getResource(scrInfo,virtualIconGeometryCQ,virtualIconGeometryIQ);
	if (!s)
	    resources->iconGeometry = strdup(GRV.VirtualIconGeometry);
	else resources->iconGeometry = strdup(s);
	s = getResource(scrInfo,virtualScaleCQ,virtualScaleIQ);
	if (!s)
	    resources->scale = GRV.VDMScale;
	else resources->scale = atoi(s);
	s = getResource(scrInfo,virtualGridCQ,virtualGridIQ);
	if (!s)
	    resources->grid = GRV.VirtualGrid;
	else resources->grid = atoi(s);
	s = getResource(scrInfo,virtualMapNameCQ,virtualMapNameIQ);
	if (s)
	    resources->background = strdup(s);
	else if (GRV.VirtualBackgroundMap)
	    resources->background = strdup(GRV.VirtualBackgroundMap);
	else resources->background = NULL;

	MakeVDM(dpy, scrInfo, client, resources);
	/*
	 * Need this to set the ascent value; there's probably a better
	 * way REMIND SDO
	 */
        updateScreenVirtualFont(dpy,scrInfo);
}

/*-------------------------------------------------------------------------
 *	Global Functions
 *-------------------------------------------------------------------------*/

/*
 * InitScreens - inits all managed screens
 */
void
InitScreens(dpy)
	Display		*dpy;
{
	XVisualInfo	*visInfo;
	int		scr,nvis;
	ScreenInfo	*scrInfo;

	makeScreenQuarks();

	/*
 	 *	Get the visual info for all the screens
	 */
	visInfo = XGetVisualInfo(dpy,VisualNoMask,(XVisualInfo *)NULL,&nvis);

	/*
 	 *	If only managing a single screen then use the defaultscreen
	 */
	if (GRV.SingleScreen) {
		scr = DefaultScreen(dpy);
		initScreenInfo(dpy,scr,visInfo,nvis);
	/*
 	 *	Else manage all screens for this display
	 */
	} else {
		for (scr=0; scr<ScreenCount(dpy); scr++) {
			initScreenInfo(dpy,scr,visInfo,nvis);
		}
	}

	XFree((char *)visInfo);

	/*
 	 *	Make an input-only window that is invisible.  This window
	 *	will have the focus when no client window has it.  Only
	 *	is needed for all screen - so use the last screen in
 	 *	the ScreenInfoList.
	 */
	scrInfo = (ScreenInfo *)ScreenInfoList->value;
	MakeNoFocus(dpy,scrInfo->rootwin);
	/*
	 * Initialize the screen Menu pixmaps
	 */
}

/*
 * DestroyScreens - shuts down all screens
 */
void
DestroyScreens(dpy)
	Display		*dpy;
{

	ScreenInfo	*si;
	List		*l = ScreenInfoList;

	/*
	 *	For each screen/root-window, destroy the WinRoot object,
	 *	install the root colormap, reset the background to the 
	 *	default and restore input focus to the root windows.
 	 */
	for (si = ListEnum(&l); si; si = ListEnum(&l)) {


		/*
		 * Should only do this if we painted it in the first place
		 */
		if (GRV.PaintWorkspace) {
		    InstallDefaultColormap(dpy,si->rootwin,False);
		    XSetWindowBackgroundPixmap(dpy,si->rootid,None);
		    XClearWindow(dpy,si->rootid);
		}

		(*(WinFunc(si->rootwin,core.destroyfunc)))(dpy,si->rootwin);
		XSetInputFocus(dpy,PointerRoot,RevertToPointerRoot,CurrentTime);
	}
}


/*
 * GetScrInfoOfScreen - return the ScreenInfo for a particular screen no
 */
ScreenInfo *
GetScrInfoOfScreen(screenno)
	int		screenno;
{
	ScreenInfo	*si;
	List		*l = ScreenInfoList;

	for (si = ListEnum(&l); si; si = ListEnum(&l)) {
		if (si->screen == screenno)
			return si;
	}
	return (ScreenInfo *)NULL;
}

/*
 *	GetScrInfoOfRoot - return the ScreenInfo for a particular root win
 */
ScreenInfo *
GetScrInfoOfRoot(root)
	Window		root;
{
	ScreenInfo	*si;
	List		*l = ScreenInfoList;

	for (si = ListEnum(&l); si; si = ListEnum(&l)) {
		if (si->rootid == root)
			return si;
	}
	return (ScreenInfo *)NULL;
}

/*
 *	GetFirstScrInfo - return the ScreenInfo for the lowest-numbered screen
 */
ScreenInfo *
GetFirstScrInfo()
{
	ScreenInfo	*si;
	ScreenInfo	*lowestScrInfo;
	List		*l = ScreenInfoList;
	int		lowestScreen = 99999;	/* REMIND */

	for (si = ListEnum(&l); si; si = ListEnum(&l)) {
		if (si->screen < lowestScreen) {
			lowestScrInfo = si;
			lowestScreen = si->screen;
		}
	}
	return lowestScrInfo;
}

/*
 * SetWorkspaceColor  - set workspace color for each screen
 */
void
SetWorkspaceColor(dpy)
	Display		*dpy;
{
	ScreenInfo	*si;
	List		*l = ScreenInfoList;

	for (si = ListEnum(&l); si; si = ListEnum(&l)) {
		setScreenWorkspaceColor(dpy,si);
	}
	WinRedrawAllWindows();
}

/*
 * SetWindowColor - set various window background colors for each screen
 */
void
SetWindowColor(dpy)
	Display		*dpy;
{
	ScreenInfo	*si;
	List		*l = ScreenInfoList;

	for (si = ListEnum(&l); si; si = ListEnum(&l)) {
		setScreenWindowColor(dpy,si);
	}
	WinRedrawAllWindows();
}

/*
 * SetForegroundColor - set the window foreground color for each screen
 */
void
SetForegroundColor(dpy)
	Display		*dpy;
{
	ScreenInfo	*si;
	List		*l = ScreenInfoList;

	for (si = ListEnum(&l); si; si = ListEnum(&l)) {
		setScreenForegroundColor(dpy,si);
	}
	WinRedrawAllWindows();
}

/*
 * SetBackgroundColor - set the background color for each screen
 */
void
SetBackgroundColor(dpy)
	Display		*dpy;
{
	ScreenInfo	*si;
	List		*l = ScreenInfoList;

	for (si = ListEnum(&l); si; si = ListEnum(&l)) {
		setScreenBackgroundColor(dpy,si);
	}
	WinRedrawAllWindows();
}

/*
 * SetBorderColor - set the border color for each screen
 */
void
SetBorderColor(dpy)
	Display		*dpy;
{
	ScreenInfo	*si;
	List		*l = ScreenInfoList;

	for (si = ListEnum(&l); si; si = ListEnum(&l)) {
		setScreenBorderColor(dpy,si);
	}
	WinRedrawAllWindows();
}

/*
 * SetTitleFont - set Title Font for each screen
 */
void
SetTitleFont(dpy)
	Display		*dpy;
{
	ScreenInfo	*si;
	List		*l = ScreenInfoList;

	for (si = ListEnum(&l); si; si = ListEnum(&l)) {
		updateScreenTitleFont(dpy,si);
	}
	WinRedrawAllWindows();
}

/*
 * SetTextFont - set Text Font for each screen
 */
void
SetTextFont(dpy)
	Display		*dpy;
{
	ScreenInfo	*si;
	List		*l = ScreenInfoList;

	for (si = ListEnum(&l); si; si = ListEnum(&l)) {
		updateScreenTextFont(dpy,si);
	}
	/* affects notices only so don't redraw */
}

/*
 * SetButtonFont - set Button Font for each screen
 */
void
SetButtonFont(dpy)
	Display		*dpy;
{
	ScreenInfo	*si;
	List		*l = ScreenInfoList;

	for (si = ListEnum(&l); si; si = ListEnum(&l)) {
		updateScreenButtonFont(dpy,si);
	}
	WinRedrawAllWindows();	/* should be just pinned menus */
}

/*
 * SetIconFont - set Icon Font for each screen
 */
void
SetIconFont(dpy)
	Display		*dpy;
{
	ScreenInfo	*si;
	List		*l = ScreenInfoList;

	for (si = ListEnum(&l); si; si = ListEnum(&l)) {
		updateScreenIconFont(dpy,si);
	}
	WinRedrawAllWindows();	/* should be just icon windows */
}

/*
 * SetGlyphFont - set Glyph Font for each screen
 */
void
SetGlyphFont(dpy)
	Display		*dpy;
{
	ScreenInfo	*si;
	List		*l = ScreenInfoList;

	for (si = ListEnum(&l); si; si = ListEnum(&l)) {
		updateScreenGlyphFont(dpy,si);
	}
	WinRedrawAllWindows();
}

/*
 * SetIconLocation - calls SlotSetLocations for each screen
 */
void
SetIconLocation(dpy)
	Display		*dpy;
{
	ScreenInfo	*si;
	List		*l = ScreenInfoList;

	for (si = ListEnum(&l); si; si = ListEnum(&l)) {
		SlotSetLocations(dpy,si->iconGrid);
	}
}

/*
 * ReparentScreens - reparents each of the screens window trees
 */
int
ReparentScreens(dpy)
	Display		*dpy;
{
	ScreenInfo	*si;
	List		*l = ScreenInfoList;

	for (si = ListEnum(&l); si; si = ListEnum(&l)) {
		ReparentTree(dpy,si->rootid);
	}
}

int
CreateScreenWindowMenuInfo(dpy)
	Display		*dpy;
{
	ScreenInfo	*si;
	List		*l = ScreenInfoList;

	for (si = ListEnum(&l); si; si = ListEnum(&l)) {
		CreateWindowMenuInfo(dpy,si);
	}
}

int
DestroyScreenWindowMenuInfo(dpy)
	Display		*dpy;
{
	ScreenInfo	*si;
	List		*l = ScreenInfoList;

	for (si = ListEnum(&l); si; si = ListEnum(&l)) {
		DestroyWindowMenuInfo(dpy,si);
	}
}

int
CreateScreenUserMenuInfo(dpy)
	Display		*dpy;
{
	ScreenInfo	*si;
	List		*l = ScreenInfoList;

	for (si = ListEnum(&l); si; si = ListEnum(&l)) {
		CreateUserMenuInfo(dpy,si);
	}
}

int
DestroyScreenUserMenuInfo(dpy)
	Display		*dpy;
{
	ScreenInfo	*si;
	List		*l = ScreenInfoList;

	for (si = ListEnum(&l); si; si = ListEnum(&l)) {
		DestroyUserMenuInfo(dpy,si);
	}
}

CreateAutoRootMenuScreen(dpy, si)
    Display	*dpy;
    ScreenInfo	*si;
{
MenuInfo	*info;
XEvent		ev;
extern MenuInfo	*FindMenuInfo();

    info = FindMenuInfo(si->rootwin, si->menuTable[MENU_ROOT]);
    if (!info)
	return;
    info->menuX = GRV.AutoRootMenuX;
    info->menuY = GRV.AutoRootMenuY;
    (void) MakePinMenu(dpy, si->rootwin, info);
}

int
CreateAutoRootMenu(dpy)
    Display	*dpy;
{
	ScreenInfo	*si;
	List		*l = ScreenInfoList;

	if (!GRV.AutoShowRootMenu)
	    return;
	for (si = ListEnum(&l); si; si = ListEnum(&l)) {
		CreateAutoRootMenuScreen(dpy,si);
	}
}
