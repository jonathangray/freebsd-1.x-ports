/*****************************************************************************/
/**               Copyright 1990 by Solbourne Computer Inc.                 **/
/**                          Longmont, Colorado                             **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    name of Solbourne not be used in advertising                         **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    SOLBOURNE COMPUTER INC. DISCLAIMS ALL WARRANTIES WITH REGARD         **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL SOLBOURNE                **/
/**    BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-           **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/

/**********************************************************************
 *
 * $XConsortium: vdt.c,v 1.140 90/03/23 11:42:33 jim Exp $
 *
 * $Log: vdt.c,v $
 * Revision 1.1  1994/05/18 18:13:02  asami
 * Initial revision
 *
 * Revision 1.2  1991/09/25  23:04:32  cross
 * Modified all calls to FindBitmap to make them FindPixmap
 * Also, took out the code that makes the returned bitmap into
 * a pixmap, cause XcprFilePixmapFile now returns a pixmap all
 * the time...
 *
 * Revision 1.1  1991/09/25  22:54:13  cross
 * Initial revision
 *
 * Revision 10.0  91/06/12  09:05:53  toml
 * Revision bump
 * 
 * Revision 9.1  91/06/12  08:39:24  toml
 * Added patch to do gridded movement in panner
 * 
 * Revision 9.0  91/04/23  07:40:54  toml
 * Revision bump
 * 
 * Revision 8.5  91/04/18  08:19:20  toml
 * Disable backing store on the virtual desktop
 * 
 * Revision 8.4  91/04/15  14:54:18  toml
 * Fixes panner interaction under OpenWindows
 * 
 * Revision 8.3  90/12/29  15:26:03  toml
 * Made PannerOpaqueScroll only take effect if StickyAbove is also set.
 * 
 * Revision 8.2  90/12/29  11:24:22  toml
 * Added PannerOpaqueScroll
 * 
 * Revision 8.1  90/12/29  09:56:37  toml
 * Disallow unmapnotify is window is panner
 * 
 * Revision 8.0  90/11/15  20:02:56  toml
 * Revision bump
 * 
 * Revision 7.4  90/11/12  21:34:52  toml
 * Implemented Scr->StickyAbove
 * 
 * Revision 7.3  90/11/12  20:39:37  toml
 * Fixes to stickyroot patches
 * 
 * Revision 7.2  90/11/12  19:57:21  toml
 * Patches to allow sticky windows to lower
 * 
 * Revision 1.3  90/11/04  19:23:50  brossard
 * When mapping a window and warping the cursor to it, only scroll
 * the desktop if the window is not visible on screen.  (This was
 * actually done in the previous revistion, this one just fixes
 * the maths to do it right).
 * 
 * Revision 1.2  90/11/04  18:38:36  brossard
 * Sticky windows are now child of the virtual root.
 * This has the advantage that they can now be raised and lowered like
 * any other window.  They no longuer are above everything else.
 * It has the disadvantage that when you move the desktop, the
 * sticky windows have to be moved back after scrolling the desktop.
 * 
 *
 * Virtual Desktop routines
 *
 * 22-Aug-90 Tom LaStrange        Initial Version.
 *
 **********************************************************************/

#if !defined(lint) && !defined(SABER)
static char RCSinfo[]=
"$XConsortium: vdt.c,v 1.140 90/03/23 11:42:33 jim Exp $";
#endif

#include <stdio.h>
#include <X11/X.h>
#include <X11/Xatom.h>
#include "twm.h"
#include "screen.h"
#include "vdt.h"
#include "parse.h"
#include "move.h"

#define MAX_VDT_WIDTH	32000
#define MAX_VDT_HEIGHT	32000

static int pointerX;
static int pointerY;

/* private swm atoms */
/* it would be nice at some point to get these atoms blessed so that
 * we don't have to make them swm specific
 */

/* version 1.2 of swm sends synthetic ConfigureNotify events
 * with respect to the actual root window rather than a 
 * clients logical root window.  OI clients lokk for this 
 * version string on the root window to determine how to
 * interpret the ConfigureNotify event
 */
#define SWM_VERSION "1.2"

Atom XA_SWM_ROOT = None;
Atom XA_SWM_VROOT = None;
Atom XA_SWM_VERSION = None;

/***********************************************************************
 *
 *  Procedure:
 *	InitVirtualDesktop - init vdt stuff
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

void
InitVirtualDesktop()
{
    XA_SWM_ROOT          = XInternAtom(dpy, "__SWM_ROOT", False);
    XA_SWM_VROOT         = XInternAtom(dpy, "__SWM_VROOT", False);
    XA_SWM_VERSION       = XInternAtom(dpy, "__SWM_VERSION", False);
}

/***********************************************************************
 *
 *  Procedure:
 *	SetSWM_ROOT - set the XA_SWM_ROOT property 
 *		This property always indicates to the application
 *		what its "root" window is.  This is currently needed
 *		by OI based clients, other toolkits don't know about
 *		virtual desktops.
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

void
SetSWM_ROOT(tmp_win)
TwmWindow *tmp_win;
{
    if (Scr->VirtualDesktop && !tmp_win->iconmgr)
    {
	XChangeProperty(dpy, tmp_win->w, XA_SWM_ROOT, XA_WINDOW,
	    32, PropModeReplace, (unsigned char *)&tmp_win->root, 1);
    }
}

/***********************************************************************
 *
 *  Procedure:
 *	SetSWM_VERSION - set the XA_SWM_VERSION property 
 *		This property always indicates to the application
 *		what its "root" window is.  This is needed by OI
 *		clients so they can know how to interpret the
 *		incoming synthetic ConfigureNotify events.
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

void
SetSWM_VERSION()
{
    static char *version = SWM_VERSION;
    XChangeProperty(dpy,Scr->Root,XA_SWM_VERSION, XA_STRING, 8,
	PropModeReplace, (unsigned char *)version, strlen(version));
}

/***********************************************************************
 *
 *  Procedure:
 *	RemoveSWM_VERSION - remove the XA_SWM_VERSION property 
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

void
RemoveSWM_VERSION()
{
    XDeleteProperty(dpy, Scr->Root, XA_SWM_VERSION);
}

/***********************************************************************
 *
 *  Procedure:
 *	MakeVirtual - make a small virtual window
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

Window
MakeVirtual(tmp_win, x, y, width, height, background, border)
TwmWindow *tmp_win;
int x;
int y;
int width;
int height;
long background;
long border;
{
    Window virtual;

    width /= Scr->PannerScale;
    height /= Scr->PannerScale;
    if (width <= 0) width = 1;
    if (height <= 0) height = 1;
    if (width > MAX_VDT_WIDTH) width = MAX_VDT_WIDTH;
    if (height > MAX_VDT_HEIGHT) height = MAX_VDT_HEIGHT;
    virtual = XCreateSimpleWindow(dpy, Scr->Panner, x, y,
	width, height, 1, border, background);
    XGrabButton(dpy, Button2, 0L, virtual,
	True, ButtonPressMask | ButtonReleaseMask,
	GrabModeAsync, GrabModeAsync, Scr->Panner, None);
    XSelectInput(dpy, virtual, KeyPressMask | ExposureMask );
    XSaveContext(dpy, virtual, TwmContext, (caddr_t) tmp_win);
    XSaveContext(dpy, virtual, VirtualContext, (caddr_t) tmp_win);
    XSaveContext(dpy, virtual, ScreenContext, (caddr_t) Scr);
    return (virtual);
}

/***********************************************************************
 *
 *  Procedure:
 *	ResizeVirtual - resize one of the small virtual windows
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

void
ResizeVirtual(window, width, height)
Window window;
int width;
int height;
{
    if (window) {
	width /= Scr->PannerScale;
	height /= Scr->PannerScale;
	if (width <= 0) width = 1;
	if (height <= 0) height = 1;
	XResizeWindow(dpy, window, width, height);
    }
}

/***********************************************************************
 *
 *  Procedure:
 *	MapFrame - map a client window and its frame
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

void
MapFrame(tmp_win)
TwmWindow *tmp_win;
{
    XMapWindow(dpy, tmp_win->w);
    XMapWindow(dpy, tmp_win->frame);
    if (tmp_win->virtualWindow && !tmp_win->sticky)
	XMapWindow(dpy, tmp_win->virtualWindow);
}

/***********************************************************************
 *
 *  Procedure:
 *	UnmapFrame - unmap a client window and its frame
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

void
UnmapFrame(tmp_win)
TwmWindow *tmp_win;
{
    XUnmapWindow(dpy, tmp_win->frame);
    if (!tmp_win->iconmgr && tmp_win->w != Scr->Panner)
	XUnmapWindow(dpy, tmp_win->w);
    if (tmp_win->virtualWindow && !tmp_win->sticky)
	XUnmapWindow(dpy, tmp_win->virtualWindow);
}

/***********************************************************************
 *
 *  Procedure:
 *	RaiseFrame - raise a client window and its frame
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

void
RaiseFrame(tmp_win)
TwmWindow *tmp_win;
{
    XRaiseWindow(dpy, tmp_win->frame);
    if (tmp_win->virtualWindow && !tmp_win->sticky)
	XRaiseWindow(dpy, tmp_win->virtualWindow);
}

/***********************************************************************
 *
 *  Procedure:
 *	LowerFrame - lower a client window and its frame
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

void
LowerFrame(tmp_win)
TwmWindow *tmp_win;
{
    XWindowChanges xwc;

    if (Scr->StickyAbove && tmp_win->sticky && Scr->VirtualDesktop) {
	xwc.sibling = Scr->VirtualDesktop;
	xwc.stack_mode = Above;
	XConfigureWindow(dpy, tmp_win->frame, CWSibling|CWStackMode, &xwc);
    }
    else
	XLowerWindow(dpy, tmp_win->frame);
    if (tmp_win->virtualWindow && !tmp_win->sticky)
	XLowerWindow(dpy, tmp_win->virtualWindow);
}

/***********************************************************************
 *
 *  Procedure:
 *	MapIcon - map the icon window
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

void
MapIcon(tmp_win)
TwmWindow *tmp_win;
{
    XMapRaised(dpy, tmp_win->icon_w);
    if (tmp_win->virtualIcon && !tmp_win->sticky)
	XMapRaised(dpy, tmp_win->virtualIcon);
}

/***********************************************************************
 *
 *  Procedure:
 *	UnmapIcon - unmap the icon window
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

void
UnmapIcon(tmp_win)
TwmWindow *tmp_win;
{
    XUnmapWindow(dpy, tmp_win->icon_w);
    if (tmp_win->virtualIcon && !tmp_win->sticky)
	XUnmapWindow(dpy, tmp_win->virtualIcon);
}

/***********************************************************************
 *
 *  Procedure:
 *	RaiseIcon - raise a client icon
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

void
RaiseIcon(tmp_win)
TwmWindow *tmp_win;
{
    XRaiseWindow(dpy, tmp_win->icon_w);
    if (tmp_win->virtualIcon && !tmp_win->sticky)
	XRaiseWindow(dpy, tmp_win->virtualIcon);
}

/***********************************************************************
 *
 *  Procedure:
 *	LowerIcon - lower an icon
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

void
LowerIcon(tmp_win)
TwmWindow *tmp_win;
{
    XWindowChanges xwc;

    if (Scr->StickyAbove && tmp_win->sticky && Scr->VirtualDesktop) {
	xwc.sibling = Scr->VirtualDesktop;
	xwc.stack_mode = Above;
	XConfigureWindow(dpy, tmp_win->icon_w, CWSibling|CWStackMode, &xwc);
    }
    else
	XLowerWindow(dpy, tmp_win->icon_w);
    if (tmp_win->virtualIcon && !tmp_win->sticky)
	XLowerWindow(dpy, tmp_win->virtualIcon);
}

/***********************************************************************
 *
 *  Procedure:
 *	MoveIcon - move an icon
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

void
MoveIcon(tmp_win, x, y)
TwmWindow *tmp_win;
{
    XWindowChanges xwc;

    XMoveWindow(dpy, tmp_win->icon_w, x, y);
    tmp_win->icon_loc_x = x;
    tmp_win->icon_loc_y = y;
    if (tmp_win->virtualIcon)
	XMoveWindow(dpy, tmp_win->virtualIcon,
		    x/Scr->PannerScale, y/Scr->PannerScale);
}

/***********************************************************************
 *
 *  Procedure:
 *	MakeVirtualDesktop - create the virtual desktop window
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

void
MakeVirtualDesktop(width, height)
int width, height;
{
    Pixmap pm;
    unsigned int pmWidth, pmHeight;
	Bool pmPixmap;
    XSetWindowAttributes attr;
    unsigned attrMask;
    extern char *display_name;
    int fres;
    char tmp[255];

/*
 *  Make vdt size an integral multiple of physical screen size
 *  if the given width and/or height are less than physical
 *  width and/or height.
 *  WM Kules, wmk@fed.frb.gov, Oct 15, 1990
 */
    if (width < Scr->MyDisplayWidth)
       width = width * Scr->MyDisplayWidth;
    if (height < Scr->MyDisplayHeight)
       height = height * Scr->MyDisplayHeight;
  
    pm = None;
    if (Scr->vdtPixmap) {
#ifndef XLOADIMAGE
/*
 *   Code segment here is original.  If XLOADIMAGE is not defined, this will
 * be used.  Else, use the xloadimage program (fork & exec) to load root
 * images.  This allows you to load more type of images than just Xbm and
 * Xpm.  If XLOADIMAGE is defined, it should be defined as the full-path
 * name of the xloadimage executable.
 *                     Chris P. Ross	12-90
 */
	pm = FindPixmap(Scr->vdtPixmap, &pmWidth, &pmHeight, &pmPixmap,
			NULL, NULL);
#if 0
/* I think this code isn't needed now that FindPixmap will return a pixmap */
	if (pm) {
	    GC gc;
	    XGCValues gcv;

#ifdef XPM
		if (!bmPixmap) {
#endif
	    pm = XCreatePixmap(dpy, Scr->Root,
			       bmWidth, bmHeight, Scr->d_depth);
	    gcv.foreground = Scr->vdtC.fore;
	    gcv.background = Scr->vdtC.back;
	    gc = XCreateGC(dpy, Scr->Root, GCForeground|GCBackground, &gcv);
	    XCopyPlane(dpy, bm, pm, gc, 0,0, bmWidth, bmHeight, 0,0, 1);
	    XFreeGC(dpy, gc);
#ifdef XPM
		} else {
			pm = bm;
		}
#endif
	}
#endif /* 0 */
#else  /* defined(XLOADIMAGE) */
		fres = fork();
		if (fres < 0) fprintf(stderr, "%s: error in fork()...\n");
		if (fres == 0) {		/* This is the child */
			execlp("xloadimage", "xsetbg", "-display",
				XDisplayName(display_name),
			       Scr->vdtPixmap, NULL);
			/* No xloadimage in path, call it explicitly... */
			execlp(XLOADIMAGE, "xsetbg", "-display",
				XDisplayName(display_name),
			       Scr->vdtPixmap, NULL);
			/* Give up, you don't get a background... */
			exit(0);
	    }
	    /* fres > 0 :  Parent, therefore continue */
#endif  /* XLOADIMAGE */
    }
    attrMask = CWOverrideRedirect | CWEventMask |
               CWBackPixmap | CWBackingStore;
    attr.override_redirect = True;
    attr.event_mask = SubstructureRedirectMask | SubstructureNotifyMask;
    attr.backing_store = NotUseful;
    attr.background_pixmap = Scr->rootWeave;
    if (pm)
	attr.background_pixmap = pm;
    else if (Scr->vdtBackgroundSet) {
	attrMask &= ~CWBackPixmap;
	attrMask |= CWBackPixel;
	attr.background_pixel = Scr->vdtC.back;
    }

    Scr->VirtualDesktop = XCreateWindow(dpy, Scr->Root, 0, 0, 
	    width+50, height+50, 0, Scr->d_depth, CopyFromParent,
	    Scr->d_visual, attrMask, &attr);
    Scr->vdtWidth = width;
    Scr->vdtHeight = height;

    /* this property allows ssetroot to find the virtual root window */
    XChangeProperty(dpy, Scr->VirtualDesktop, XA_SWM_VROOT, XA_WINDOW, 32,
	PropModeReplace, (unsigned char *)&Scr->VirtualDesktop, 1);

    if (pm)
	XFreePixmap(dpy, pm);
}

/***********************************************************************
 *
 *  Procedure:
 *	MakePanner - create the virtual desktop panner window
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

void
MakePanner()
{
    static XTextProperty wName={(unsigned char *) "Virtual Desktop",XA_STRING,8,15};
    static XTextProperty iName={(unsigned char *) "Desktop",XA_STRING,8,7};
    XSetWindowAttributes attr;
    XSizeHints *sizeHints;
    XWMHints *wmHints;
    XClassHint *classHints;
    int status;
    int x, y, width, height;
    Pixmap pm;
    unsigned int pmWidth, pmHeight, pmPixmap;
    unsigned attrMask;

    width = Scr->vdtWidth/Scr->PannerScale;
    height = Scr->vdtHeight/Scr->PannerScale;

    sizeHints = XAllocSizeHints();
    sizeHints->flags = PBaseSize;
    sizeHints->base_width = width;
    sizeHints->base_height = height;
    sizeHints->min_width = Scr->MyDisplayWidth/Scr->PannerScale;
    sizeHints->min_height = Scr->MyDisplayHeight/Scr->PannerScale;
    sizeHints->max_width = MAX_VDT_WIDTH/Scr->PannerScale;
    sizeHints->max_height = MAX_VDT_HEIGHT/Scr->PannerScale;
    XWMGeometry(dpy, Scr->screen, Scr->PannerGeometry, NULL,
	1, sizeHints, &x, &y, &width, &height, &sizeHints->win_gravity);
    sizeHints->flags = USPosition|PWinGravity|PMinSize|PMaxSize;

    wmHints = XAllocWMHints();
    wmHints->initial_state = Scr->PannerState;
    wmHints->flags = StateHint;

    classHints = XAllocClassHint();
    classHints->res_name = "virtualDesktop";
    classHints->res_class = "Twm";

    pm = None;
    if (Scr->PannerPixmap) {
	pm = FindPixmap(Scr->PannerPixmap, &pmWidth, &pmHeight, &pmPixmap,
		NULL, NULL);
#if 0
	if (bm) {
	    GC gc;
	    XGCValues gcv;

#ifdef XPM
	    if (!bmPixmap) {
#endif  /* XPM */
		pm = XCreatePixmap(dpy, Scr->Root, bmWidth, bmHeight,
					Scr->d_depth);
		gcv.foreground = Scr->PannerC.fore;
		gcv.background = Scr->PannerC.back;
		gc = XCreateGC(dpy, Scr->Root, GCForeground|GCBackground, &gcv);
		XCopyPlane(dpy, bm, pm, gc, 0,0, bmWidth, bmHeight, 0,0, 1);
		XFreeGC(dpy, gc);
#ifdef XPM
	    } else {
		pm = bm;
	    }
#endif  /* XPM */
	}
#endif  /* 0 */
    }

    attrMask = CWBackPixmap|CWEventMask;
    attr.background_pixmap = Scr->rootWeave;
    attr.event_mask = ExposureMask | ButtonPressMask;
    if (pm)
	attr.background_pixmap = pm;
    else if (Scr->PannerBackgroundSet) {
	attrMask &= ~CWBackPixmap;
	attrMask |= CWBackPixel;
	attr.background_pixel = Scr->PannerC.back;
    }
    Scr->Panner = XCreateWindow(dpy, Scr->Root, x, y, 
	    sizeHints->base_width, sizeHints->base_height, 1,
	    Scr->d_depth, CopyFromParent,
	    Scr->d_visual, attrMask, &attr);
    
    Scr->PannerWidth = sizeHints->base_width;
    Scr->PannerHeight = sizeHints->base_height;

    XGrabButton(dpy, Button1, 0L, Scr->Panner,
	True, ButtonPressMask | ButtonReleaseMask | ButtonMotionMask,
	GrabModeAsync, GrabModeAsync, Scr->Panner, None);
    XGrabButton(dpy, Button3, 0L, Scr->Panner,
	True, ButtonPressMask | ButtonReleaseMask | ButtonMotionMask,
	GrabModeAsync, GrabModeAsync, Scr->Panner, None);
    XSetWMProperties(dpy, Scr->Panner, &wName, &iName, NULL, 0,
	sizeHints, wmHints, classHints);

    if (Scr->PannerState != WithdrawnState)
	XMapRaised(dpy, Scr->Panner);
    XSaveContext(dpy, Scr->Panner, ScreenContext, (caddr_t) Scr);
}

void
HandlePannerExpose(ev)
XEvent *ev;
{
    XEvent dummy;

    if (ev->xexpose.count)
	return;

    if (!Scr->PannerOutlineWidth) {
	Scr->PannerOutlineWidth = Scr->MyDisplayWidth / Scr->PannerScale -1;
	Scr->PannerOutlineHeight = Scr->MyDisplayHeight / Scr->PannerScale -1;
    }
    XClearWindow(dpy, Scr->Panner);
    XDrawRectangle(dpy, Scr->Panner, Scr->PannerGC,
	Scr->PannerOutlineX, Scr->PannerOutlineY,
	Scr->PannerOutlineWidth, Scr->PannerOutlineHeight);

    /* flush other expose events */
    while (XCheckTypedWindowEvent (dpy, Scr->Panner, Expose, &dummy)) ;
}

#define GridMask (ShiftMask | ControlMask | Mod1Mask | Mod2Mask)

static int pannerButton = 0;
static int use_grid = 0;

void
HandlePannerButtonPress(ev)
XEvent *ev;
{
    if (!pannerButton) {
	pannerButton = ev->xbutton.button;
	pointerX = Scr->PannerOutlineX + Scr->PannerOutlineWidth/2;
	pointerY = Scr->PannerOutlineY + Scr->PannerOutlineHeight/2;
	XWarpPointer(dpy, None, Scr->Panner, 0,0,0,0,
	    pointerX, pointerY);
	XClearWindow(dpy, Scr->Panner);
	XDrawRectangle(dpy, Scr->Panner, Scr->DrawGC,
	    Scr->PannerOutlineX, Scr->PannerOutlineY,
	    Scr->PannerOutlineWidth, Scr->PannerOutlineHeight);
    }
}

void
HandlePannerButtonRelease(ev)
XEvent *ev;
{
    if (ev->xbutton.button == pannerButton) {
	pannerButton = 0;
	use_grid = ev->xmotion.state & GridMask;

	XDrawRectangle(dpy, Scr->Panner, Scr->DrawGC,
	    Scr->PannerOutlineX, Scr->PannerOutlineY,
	    Scr->PannerOutlineWidth, Scr->PannerOutlineHeight);
	XClearWindow(dpy, Scr->Panner);
	XDrawRectangle(dpy, Scr->Panner, Scr->PannerGC,
	    Scr->PannerOutlineX, Scr->PannerOutlineY,
	    Scr->PannerOutlineWidth, Scr->PannerOutlineHeight);

	if (use_grid) {
	    int xx = Scr->PannerOutlineX * Scr->PannerScale;
	    int yy = Scr->PannerOutlineY * Scr->PannerScale;

	    xx = xx - xx % Scr->MyDisplayWidth;
	    yy = yy - yy % Scr->MyDisplayHeight;
	    MoveDesktop(xx, yy);
	}
	else {
	    MoveDesktop(Scr->PannerOutlineX*Scr->PannerScale,
			Scr->PannerOutlineY*Scr->PannerScale);
	}
    }
}

static int TRUNCATE(pos, grid)
    int pos, grid;
{
    pos *= Scr->PannerScale;
    return ((pos - pos % grid) + Scr->PannerScale - 1) / Scr->PannerScale;
}

void
HandlePannerMotionNotify(ev)
XEvent *ev;
{
    XEvent dummyev;
    int deltaX, deltaY;
    int newOutlineX, newOutlineY;

    use_grid = ev->xmotion.state & GridMask;

    /* figure out the position of the next outline */
    deltaX = ev->xmotion.x - pointerX;
    deltaY = ev->xmotion.y - pointerY;

    if (use_grid) {
	newOutlineX = ev->xmotion.x;
	newOutlineY = ev->xmotion.y;
    }
    else {
	newOutlineX = Scr->PannerOutlineX + deltaX;
	newOutlineY = Scr->PannerOutlineY + deltaY;
    }

    if (newOutlineX < 0)
	newOutlineX = 0;
    if (newOutlineY < 0)
	newOutlineY = 0;

    if (use_grid) {
	newOutlineX = TRUNCATE (newOutlineX, Scr->MyDisplayWidth);
	newOutlineY = TRUNCATE (newOutlineY, Scr->MyDisplayHeight);
    }

    if ((newOutlineX + Scr->PannerOutlineWidth) >= Scr->PannerWidth)
	newOutlineX = Scr->PannerWidth - Scr->PannerOutlineWidth - 1;
    if ((newOutlineY + Scr->PannerOutlineHeight) >= Scr->PannerHeight)
	newOutlineY = Scr->PannerHeight - Scr->PannerOutlineHeight - 1;

    if (Scr->PannerOutlineX != newOutlineX ||
	Scr->PannerOutlineY != newOutlineY) {
	pointerX = ev->xmotion.x;
	pointerY = ev->xmotion.y;

	/* get rid of the last outline */
	XDrawRectangle(dpy, Scr->Panner, Scr->DrawGC,
		       Scr->PannerOutlineX, Scr->PannerOutlineY,
		       Scr->PannerOutlineWidth, Scr->PannerOutlineHeight);

	Scr->PannerOutlineX = newOutlineX;
	Scr->PannerOutlineY = newOutlineY;

	/* draw the new outline */
	XDrawRectangle(dpy, Scr->Panner, Scr->DrawGC,
		       Scr->PannerOutlineX, Scr->PannerOutlineY,
		       Scr->PannerOutlineWidth, Scr->PannerOutlineHeight);
    }
    while(XCheckTypedEvent(dpy,MotionNotify,&dummyev));
    
    if ((Scr->PannerOpaqueScroll && Scr->StickyAbove) ||
	(Scr->PannerOutlineX != newOutlineX ||
	 Scr->PannerOutlineY != newOutlineY)) {
	if (use_grid) {
	    int xx = Scr->PannerOutlineX * Scr->PannerScale;
	    int yy = Scr->PannerOutlineY * Scr->PannerScale;

	    xx = xx - xx % Scr->MyDisplayWidth;
	    yy = yy - yy % Scr->MyDisplayHeight;
	    MoveDesktop(xx, yy);
	}
	else {
	    MoveDesktop(Scr->PannerOutlineX*Scr->PannerScale,
			Scr->PannerOutlineY*Scr->PannerScale);
	}
    }
}


/***********************************************************************
 *
 *  Procedure:
 *	MoveDesktop - move the desktop to a pixel location
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

void
MoveDesktop(x, y)
int x;
int y;
{
    TwmWindow *tmp;
    XEvent ev;
    register int delta_x, delta_y;

    if (x != Scr->vdtPositionX || y != Scr->vdtPositionY) {
	Scr->vdtSaveX = Scr->vdtPositionX;
	Scr->vdtSaveY = Scr->vdtPositionY;
	if (x < 0) x = 0;
	if (y < 0) y = 0;
	if ((x + Scr->MyDisplayWidth) > Scr->vdtWidth)
	    x = Scr->vdtWidth - Scr->MyDisplayWidth;
	if ((y + Scr->MyDisplayHeight) > Scr->vdtHeight)
	    y = Scr->vdtHeight - Scr->MyDisplayHeight;
	XMoveWindow(dpy, Scr->VirtualDesktop, -x, -y);
	Scr->PannerOutlineX = x / Scr->PannerScale;
	Scr->PannerOutlineY = y / Scr->PannerScale;
	delta_x = x - Scr->vdtPositionX; delta_y = y - Scr->vdtPositionY;
	Scr->vdtPositionX = x;
	Scr->vdtPositionY = y;

	for (tmp = Scr->TwmRoot.next; tmp != NULL; tmp = tmp->next) {
	    if (!tmp->sticky)
		SendSyntheticConfigureNotify(tmp);
	    else if (!Scr->StickyAbove) {
		if (tmp->icon_w)
		    MoveIcon(tmp, tmp->icon_loc_x + delta_x, tmp->icon_loc_y + delta_y);
		SetupFrame( tmp, tmp->frame_x + delta_x, tmp->frame_y + delta_y,
				tmp->frame_width, tmp->frame_height, -1, True );
	    }
	}

	/* go repaint the panner */
	ev.xexpose.count = 0;
	HandlePannerExpose(&ev);
    }
}

/***********************************************************************
 *
 *  Procedure:
 *	ScrollDesktop - scroll the desktop by screenfuls
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

void
ScrollDesktop(func, pos)
int func;
char *pos;
{
    int x, y;
    int status;
    unsigned int width, height;

    x = Scr->vdtPositionX;
    y = Scr->vdtPositionY;

    switch (func)
    {
	case F_SCROLLBACK:
	    x = Scr->vdtSaveX;
	    y = Scr->vdtSaveY;
	    break;
	case F_SCROLL:
	    status = XParseGeometry(pos, &x, &y, &width, &height);
	    if ((status & (XValue & YValue)) != (XValue & YValue)) {
		fprintf (stderr,
			 "twm: ignoring invalid f.scroll geometry \"%s\"\n", pos);
		return;
	    }
	    x = abs(x) * Scr->MyDisplayWidth;
	    y = abs(y) * Scr->MyDisplayHeight;
	    if (status & XNegative)
		x = Scr->vdtWidth - Scr->MyDisplayWidth - x;
	    if (status & YNegative)
		y = Scr->vdtHeight - Scr->MyDisplayHeight - y;
	    break;
	case F_SCROLLHOME:
	    x = 0;
	    y = 0;
	    break;
  	case F_SCROLLRIGHT:
 	    x += Scr->vdtScrollDistanceX;
  	    break;
  	case F_SCROLLLEFT:
 	    x -= Scr->vdtScrollDistanceX;
  	    break;
  	case F_SCROLLDOWN:
 	    y += Scr->vdtScrollDistanceY;
  	    break;
  	case F_SCROLLUP:
 	    y -= Scr->vdtScrollDistanceY;
  	    break;
    }
    if (x < 0)
	x = 0;
    if (y < 0)
	y = 0;
    if ((x + Scr->MyDisplayWidth) > Scr->vdtWidth)
	x = Scr->vdtWidth - Scr->MyDisplayWidth;
    if ((y + Scr->MyDisplayHeight) > Scr->vdtHeight)
	y = Scr->vdtHeight - Scr->MyDisplayHeight;
    MoveDesktop(x, y);
}

/***********************************************************************
 *
 *  Procedure:
 *	ResizeDesktop - resize the desktop, the coordinates are in panner
 *		units.
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

void
ResizeDesktop(width, height)
int width;
int height;
{
    int vdtWidth, vdtHeight;

    if (width != Scr->PannerWidth || height != Scr->PannerHeight) {
	Scr->PannerWidth = width;
	Scr->PannerHeight = height;

	vdtWidth = width * Scr->PannerScale;
	vdtHeight = height * Scr->PannerScale;

	if (vdtWidth < Scr->vdtWidth) {
	    if ((Scr->PannerOutlineX + Scr->PannerOutlineWidth) >= Scr->PannerWidth)
		Scr->PannerOutlineX = Scr->PannerWidth-Scr->PannerOutlineWidth - 1;
	}
	if (vdtHeight < Scr->vdtHeight) {
	    if ((Scr->PannerOutlineY+Scr->PannerOutlineHeight) >= Scr->PannerHeight)
		Scr->PannerOutlineY = Scr->PannerHeight-Scr->PannerOutlineHeight-1;
	}
	Scr->vdtWidth = vdtWidth;
	Scr->vdtHeight = vdtHeight;

	MoveDesktop(Scr->PannerOutlineX * Scr->PannerScale,
	    Scr->PannerOutlineY * Scr->PannerScale);

	/* make it just a tad larger than requested */
	XResizeWindow(dpy, Scr->VirtualDesktop, vdtWidth + 2*Scr->PannerScale,
	    vdtHeight + 2*Scr->PannerScale);
    }
}

/***********************************************************************
 *
 *  Procedure:
 *	HandlePannerMove - prepare to move a small panner window
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

void
HandlePannerMove(ev, tmp_win)
XButtonEvent *ev;
TwmWindow *tmp_win;
{
    int cancel;
    int x_root, y_root;
    unsigned objWidth, objHeight;
    int moving_frame;

    tmp_win->root = Scr->Panner;

    x_root = ev->x_root;
    y_root = ev->y_root;

    if (ev->window == tmp_win->virtualWindow) {
	moving_frame = True;
	objWidth = tmp_win->frame_width + 2*tmp_win->frame_bw;
	objHeight = tmp_win->frame_height + 2*tmp_win->frame_bw;
    } else {
	moving_frame = False;
	objWidth = tmp_win->icon_w_width + 2;
	objHeight = tmp_win->icon_w_height + 2;
    }

    StartMove(tmp_win, ev->window, tmp_win->title_height,
	&x_root, &y_root, &cancel, IN_PANNER, Scr->PannerScale,
	objWidth, objHeight, False, False);

    tmp_win->root = Scr->VirtualDesktop;

    if (!cancel) {
	if (moving_frame) {
	    SetupWindow (tmp_win, x_root, y_root,
		 tmp_win->frame_width, tmp_win->frame_height, -1);
	}
	else {
	    MoveIcon(tmp_win, x_root, y_root);
	}
    }
}

/***********************************************************************
 *
 *  Procedure:
 *	ScrollTo - scroll the virtual desktop to a window of non of
 *		the frame is visible
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

void
ScrollTo(tmp_win)
TwmWindow *tmp_win;
{
    int x, y;
    int xr, yb;

    if (!tmp_win->sticky) {
	x = Scr->vdtPositionX;
	y = Scr->vdtPositionY;
	xr = x + Scr->MyDisplayWidth;
	yb = y + Scr->MyDisplayHeight;

	if ((tmp_win->frame_x + tmp_win->frame_width) <= x)
	    x = tmp_win->frame_x;
	else if (tmp_win->frame_x >= xr)
	    x = tmp_win->frame_x;

	if ((tmp_win->frame_y + tmp_win->frame_height) <= y)
	    y = tmp_win->frame_y;
	else if (tmp_win->frame_y >= yb)
	    y = tmp_win->frame_y;

	/* if we are going to scroll, we might as well do it in both
	 * directions
	 */
	if (x != Scr->vdtPositionX || y != Scr->vdtPositionY) {
	    x = tmp_win->frame_x;
	    y = tmp_win->frame_y;
	    MoveDesktop(x, y);
	}
    }
}

/***********************************************************************
 *
 *  Procedure:
 *	ScrollToQuadrant - scroll the virtual desktop to a the quadrant 
 *		that contains the client window.
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

void
ScrollToQuadrant(tmp_win)
TwmWindow *tmp_win;
{
    register int x, y;
    int xr, yb;

    if (!tmp_win->sticky) {
	x=tmp_win->frame_x;
	y=tmp_win->frame_y;
	/* 16 is the average cursor width */
	if ( x + tmp_win->frame_width < Scr->vdtPositionX + 16 ||
		x >  Scr->vdtPositionX + Scr->MyDisplayWidth - 16 ||
	     y + tmp_win->frame_height < Scr->vdtPositionY + 16 ||
		y >  Scr->vdtPositionY + Scr->MyDisplayHeight - 16 ){
	    x = (x / Scr->MyDisplayWidth) * Scr->MyDisplayWidth;
	    y = (y / Scr->MyDisplayHeight) * Scr->MyDisplayHeight;
	    MoveDesktop(x, y);
	}
    }
}

/***********************************************************************
 *
 *  Procedure:
 *	PaintVirtualWindow - paint the window name in the virtual window
 *	
 *  Returned Value:
 *	None
 *
 ***********************************************************************
 */

void
PaintVirtualWindow(tmp_win)
TwmWindow *tmp_win;
{
    int x, y;
    int xr, yb;

    if (Scr->ShowVirtualNames) {
	FBF(tmp_win->virtual.fore, tmp_win->virtual.back,
	    Scr->VirtualFont.font->fid);

	XClearArea(dpy, tmp_win->virtualWindow, 1, 1, 9999, Scr->VirtualFont.height, False);
	XDrawString (dpy, tmp_win->virtualWindow, Scr->NormalGC,
		     1, 1+Scr->VirtualFont.y, 
		     tmp_win->name, strlen(tmp_win->name));
    }
}
