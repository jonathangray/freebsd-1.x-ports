/*      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

/* states.c - functions relating to changes in client state 
 *	(Normal, Iconic, Withdrawn)
 */

#ident	"@(#)states.c	1.3 olvwm version 6/13/92"

/*
 * Based on
#ident	"@(#)states.c	26.50	91/09/14 SMI"
 *
 */

#include <errno.h>
#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Intrinsic.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>

#include "i18n.h"
#include "ollocale.h"
#include "mem.h"
#include "olwm.h"
#include "win.h"
#include "group.h"
#include "globals.h"
#include "properties.h"
#include "virtual.h"

/***************************************************************************
* global data
***************************************************************************/

extern Atom AtomWMClass;
extern Atom AtomDecorAdd;
extern Atom AtomDecorClose;
extern Atom AtomDecorDel;
extern Atom AtomDecorFooter;
extern Atom AtomDecorHeader;
extern Atom AtomDecorIconName;
extern Atom AtomDecorPin;
extern Atom AtomDecorResize;
extern Atom AtomDeleteWindow;
extern Atom AtomMenuFull;
extern Atom AtomMenuLimited;
extern Atom AtomNone;
extern Atom AtomOlwmTimestamp;
extern Atom AtomPinIn;
extern Atom AtomPinOut;
extern Atom AtomProtocols;
extern Atom AtomSaveYourself;
extern Atom AtomTakeFocus;
extern Atom AtomWinAttr;
extern Atom AtomWTBase;
extern Atom AtomWTCmd;
extern Atom AtomWTHelp;
extern Atom AtomWTNotice;
extern Atom AtomWTOther;

extern Client *CurrentClient;

extern	int	WinDrawFunc();
extern	void	IconPaneSetPixmap();
extern	void	IconPaneSetMask();

static void	checkGroupBinding();

/***************************************************************************
* private data
***************************************************************************/

/* sanity checks for getting stuff out of hints */
#define IsCard16(x)	((x) == ((unsigned short)(x)) && (x) > 0 )
#define IsInt16(x)	((x) == ((short) (x)))

static WMDecorations BaseWindow = {
    WMDecorationCloseButton | WMDecorationResizeable | WMDecorationHeader 
	| WMDecorationIconName,
    MENU_FULL,
    0,
    PIN_IN,
    0
};

static WMDecorations CmdWindow = {
    WMDecorationPushPin | WMDecorationResizeable | WMDecorationHeader
	| WMDecorationIconName,
    MENU_LIMITED,
    0,
    PIN_IN,
    0
};

static WMDecorations NoticeWindow = {
    WMDecorationIconName,
    MENU_NONE,
    0,
    PIN_IN,
    0
};

static WMDecorations HelpWindow = {
    WMDecorationPushPin | WMDecorationHeader | WMDecorationIconName
	| WMDecorationWarpToPin,
    MENU_LIMITED,
    0,
    PIN_IN,
    0
};

static WMDecorations OtherWindow = {
    WMDecorationIconName,
    MENU_NONE,
    0, 
    PIN_IN,
    0
};

static WMDecorations TransientWindow = {
    WMDecorationResizeable | WMDecorationIconName,
    MENU_LIMITED,
    0,
    PIN_IN,
    0
};

static WMDecorations MinimalWindow = {
    WMDecorationResizeable | WMDecorationIconName,
    MENU_FULL,
    0,
    PIN_IN,
    0
};

static WMDecorations NoDecors = {
    WMNoDecor,
    MENU_NONE,
    0,
    PIN_OUT,
    0
};

typedef struct {
	char *class, *instance, *name;
} minimalclosure;


/***************************************************************************
* private functions
***************************************************************************/

/*
 * Determine FocusMode from wmHints and protocols
 */
static FocusMode
focusModeFromHintsProtocols(wmHints,protocols)
	XWMHints	*wmHints;
	int		protocols;
{
	FocusMode	focusMode;

	if (wmHints && wmHints->input) {
		if (protocols & TAKE_FOCUS)
			focusMode = LocallyActive;	
		else
			focusMode = Passive;	
	} else { /* wmHints->input == False */
		if (protocols & TAKE_FOCUS)
			focusMode = GloballyActive;	
		else
			focusMode = NoInput;	
	}
	return focusMode;
}

/* matchInstClass -- run through the list of names to be minimally
 * decorated, and see if this window's class or instance match
 * any.
 *
 * In olvwm, we check the name as well as class/instance
 */
static Bool
matchInstClass(str,mc)
char *str;
minimalclosure *mc;
{
    if ((mc->class != NULL) && (!strcmp(str, mc->class)))
	return True;
    if ((mc->instance != NULL) && (!strcmp(str, mc->instance)))
	return True;
    if ((mc->name != NULL) && (!strncmp(str, mc->name, strlen(str))))
	return True;
    return False;
}


/*
 * getOlWinDecors - given the window attributes and decoration add/delete
 *	requests, determine what kind of window (according to the OpenLook
 *	kinds of windows) the client represents, and determine what sort of
 *	decorations are appropriate.
 */
WMDecorations *
getOLWinDecors(dpy, win, transient, cli)
Display *dpy;
Window  win;
Bool    transient;
Client	*cli;
{
        WMDecorations	       *decors;
	minimalclosure		mc;
	XWMHints	        *wmHints = cli->wmHints;
	OLWinAttr		*winAttrs;
	Bool			oldVersion;
	int			decorFlags;

        decors = MemNew(WMDecorations);

	/*
	 * REMIND: there is no way for a program to 
	 * specify the default item, even if it should
	 * specify the menu
	 * so this get's initialized here
	 * (one per window)
	 */
	decors->def_item = 0;

	/* if the instance or class strings match any of the names
	 * listed for minimal decoration, only provide resize corners
	 * and a menu.
	 */
	mc.class = cli->wmClass;
	mc.instance = cli->wmInstance;
	if (!XFetchName(dpy, win, &mc.name))
	    mc.name = NULL;
	if (ListApply(GRV.Minimals,matchInstClass,&mc) != NULL)
	{
            *decors = MinimalWindow;
	    return decors;
	}
	if (ListApply(GRV.NoDecors, matchInstClass, &mc) != NULL) {
	    *decors = NoDecors;
	    if (mc.name)
		XFree(mc.name);
	    return decors;
	}
	if (mc.name)
	    XFree(mc.name);

#ifdef SHAPE
	if (cli->isShaped) {
	    *decors = MinimalWindow;
	    return decors;
	}
#endif

	oldVersion = False;
	winAttrs = MemNew(OLWinAttr);

	/*
	 * If the _OL_WIN_ATTR property is not present then make the
	 * window into a base window unless is a transient window.
	 */
	if (!PropGetOLWinAttr(dpy,win,winAttrs,&oldVersion)) {
		if (transient) {
			*decors = TransientWindow;
			if (GRV.TransientsTitled)
				decors->flags |= WMDecorationHeader;
		} else {
			*decors = BaseWindow;
		}
	/*
 	 * Else we do have that property; so interpret it
 	 */
	} else {
		/*
		 * Choose the decor from win_type
		 */
		if ((winAttrs->flags & WA_WINTYPE) == 0) {
		    	*decors = BaseWindow;
		} else if (winAttrs->win_type == AtomWTBase) {
		    	*decors = BaseWindow;
		} else if (winAttrs->win_type == AtomWTCmd) {
		    	*decors = CmdWindow;
		} else if (winAttrs->win_type == AtomWTHelp) {
		    	*decors = HelpWindow;
		} else if (winAttrs->win_type == AtomWTNotice) {
		    	*decors = NoticeWindow;
		} else if (winAttrs->win_type == AtomWTOther) {
		    	*decors = OtherWindow;

		    	/* 
			 * This is the only case where we look at menu type
			 */
		    	if (winAttrs->flags & WA_MENUTYPE) {
				if (winAttrs->menu_type == AtomMenuFull)
					decors->menu_type = MENU_FULL;
				else if (winAttrs->menu_type == AtomMenuLimited)
					decors->menu_type = MENU_LIMITED;
				else if (winAttrs->menu_type == AtomNone)
					decors->menu_type = MENU_NONE;
		   	 }
		}

		/*
	 	 * Backward compatibility.  If we had a old/short attribute 
		 * property, and the client specified an icon window, we're 
		 * probably dealing with an old XView client.  These clients 
		 * assume the window manager doesn't put the icon name in 
		 * the icon, so they paint it into the icon window itself.
		 * Turn off the painting of the icon name for icons of 
		 * these windows.
		 */
		if (oldVersion && wmHints && (wmHints->flags & IconWindowHint))
			decors->flags &= ~WMDecorationIconName;

		/*
		 * Set cancel if something specified
		 */
		if (winAttrs->flags & WA_CANCEL)
			decors->cancel = (winAttrs->cancel != 0);

		/*
		 * Set the pin state
		 */
		if (winAttrs->flags & WA_PINSTATE) {
			decors->pushpin_initial_state = 
						winAttrs->pin_initial_state;
		} else {
			decors->pushpin_initial_state = PIN_OUT;
		}
	}

	/*
 	 * Apply DecorAdd flags
	 */
	if (PropGetOLDecorAdd(dpy,win,&decorFlags)) {
		decors->flags |= decorFlags;
	}

	/*
 	 * Apply DecorDel flags
	 */
	if (PropGetOLDecorDel(dpy,win,&decorFlags)) {
		decors->flags &= ~decorFlags;
	}

	/*
	 * If the window has no header it can't have any header
	 * decorations; ie. pushpin or a close button.
	 */
	if (!(decors->flags & WMDecorationHeader)) {
		decors->flags &= ~(WMDecorationHeaderDeco);
	}

        /* 
	 * If the window wants both a pushpin and a close button it
         * only can have a pushpin.
         */
        if ((decors->flags & WMDecorationCloseButton) &&
            (decors->flags & WMDecorationPushPin))
                decors->flags &= ~(WMDecorationCloseButton);

	/*
	 * Don't warp to the pin if there's no pin.
	 */
	if (!(decors->flags & WMDecorationPushPin))
	    decors->flags &= ~WMDecorationWarpToPin;

        return  decors;
}


/*
 * calcPosition
 *
 * Calculate the next position to place a new window.  This function places
 * all new windows on the diagonal and makes sure that there is enough room on
 * the screen for the new window's size passed in w and h.
 */
static void
calcPosition(dpy, screen, attrs, frame)
    Display		*dpy;
    int			screen;
    XWindowAttributes	*attrs;
    WinPaneFrame	*frame;
{
	int		stepValue;
	ScreenInfo	*scrInfo;

	if ((scrInfo = GetScrInfoOfScreen(screen)) == NULL) {
		attrs->x = attrs->y = 0;
		return;
	}

	/* if the height of the current window is too large ... */
	if ((scrInfo->framepos + frame->core.height
		> DisplayHeight(dpy, screen)) ||
	    (scrInfo->framepos + frame->core.width
		> DisplayWidth(dpy, screen)))
	{
	    scrInfo->framepos = 0;
	}

	/* REMIND this should really be based on the header height */
	stepValue = 30;

	/* we will return the current position */
	attrs->x = attrs->y = scrInfo->framepos;

	/* calculate the next return value */
	scrInfo->framepos = scrInfo->framepos + stepValue;
	if ((scrInfo->framepos > DisplayWidth(dpy, screen)) ||
	    (scrInfo->framepos > DisplayHeight(dpy, screen)))
	{
	    scrInfo->framepos = 0;
	}
}


/*
 * iconifyOne -- iconify one client
 */
static void *
iconifyOne(cli, winIcon)
Client *cli;
WinGeneric *winIcon;
{
	if (cli->groupmask == GROUP_DEPENDENT)
    	    RemoveSelection(cli);	/* warp if necessary */
	else
	    DrawIconToWindowLines(cli->dpy, winIcon, cli->framewin);

	UnmapWindow(cli->framewin);
	XUnmapWindow(cli->dpy, PANEWINOFCLIENT(cli));
	cli->framewin->fcore.panewin->pcore.pendingUnmaps++;
	cli->wmState = IconicState;
	ClientSetWMState(cli);
	return NULL;
}

/* deiconifyOne -- deiconify one client
 */
static void *
deiconifyOne(cli, winIcon, raise)
Client *cli;
WinGeneric *winIcon;
Bool raise;
{
	if (cli->groupmask != GROUP_DEPENDENT)
	    DrawIconToWindowLines(cli->dpy, winIcon, cli->framewin);

	if (raise)
	    RaiseWindow(cli->framewin);

	MapWindow(cli->framewin);
	XMapRaised(cli->dpy, PANEWINOFCLIENT(cli));
	cli->wmState = NormalState;
	ClientSetWMState(cli);
	return NULL;
}


/*
 * markFrame
 *
 * Marks a client's frame window with a given value.  Suitable for calling by
 * ListApply or GroupApply.
 */
static void *
markFrame(cli, value)
    Client *cli;
    int value;
{
    if (cli->framewin != NULL)
	cli->framewin->core.tag = value;
    return NULL;
}


/*
 * deiconifyGroup
 *
 * Deiconify a window group, preserving stacking order.  Mark all the frames
 * that are to be deiconified, then query the server for all children-of-root.
 * Walk backward through this array (i.e. from top to bottom).  For each group
 * member found, stack it just below the previous one (raise the first one to
 * the top) and deiconify it.  Finally, unmark all the frames in the group.
 * Note: this algorithm depends on having the stacking order of windows
 * preserved when the group is iconified.
 */
static void
deiconifyGroup(cli, winIcon)
    Client *cli;
    WinIconFrame* winIcon;
{
    Window root, parent;
    Window *children;
    Window prev = None;
    unsigned int nchildren;
    int i;
    WinGeneric *wi;
    XWindowChanges xwc;

    GroupApply(cli->groupid, markFrame, 1, GROUP_LEADER|GROUP_DEPENDENT);

    (void) XQueryTree(cli->dpy, cli->scrInfo->rootid, &root, &parent,
		      &children, &nchildren);

    xwc.stack_mode = Below;
    for (i=nchildren-1; i>=0; --i) {
	wi = WIGetInfo(children[i]);
	if (wi != NULL && wi->core.tag == 1) {
	    if (prev == None) {
		RaiseWindow(wi);
	    } else {
		xwc.sibling = prev;
		ConfigureWindow(cli->dpy, wi,
				 CWSibling|CWStackMode, &xwc);
	    }
	    prev = children[i];
	    deiconifyOne(wi->core.client, winIcon, False);
	}
    }

    if (children != NULL)
	XFree((char *)children);

    GroupApply(cli->groupid, markFrame, 0, GROUP_LEADER|GROUP_DEPENDENT);
}


/***************************************************************************
* global functions
***************************************************************************/


/*
 * StateNew -- A client is mapping a top-level window (either a new window
 *	or a Withdrawn window).  The window may become Iconic or Normal 
 *	depending on the hints.  Check to see if this window needs to be mapped
 *	and if so add the required adornments.
 *		dpy 		-- display pointer
 *		rootWin		-- root window
 *					if None will determine the root window
 *					for the client window
 *		window 		-- client's window
 *		fexisting	-- the window already exists and we
 *				   are starting olwm, so positioning should
 *				   be special-cased
 *		ourWinInfo	-- if is this one of our menu windows, this
 *			will be its WinMenu structure; this window must
 *			be a subclass of Pane
 *			If this is a VDM,  this will be a subclass of
 *			VPane
 */
Client *
StateNew(dpy, rootWin, window, fexisting, ourWinInfo)
Display *dpy;
Window rootWin;
Window window;
Bool fexisting;
WinPane *ourWinInfo;
{
	Client 		*cli;
	WinGeneric	*winGeneric;
	WinPane		*winPane;
	WinIconFrame	*winIcon;
	WinPaneFrame	*winFrame;
	WinIconPane	*winIconPane;
	XSizeHints	*normHints;
	Bool		preICCCM;
	Bool		transient = False;
	int		status;
	int		initstate;
	XWindowAttributes paneAttr;
	int 		screen;
	int		tmpx, tmpy;
	ScreenInfo 	*scrInfo;
	int		winState;
	Window		iconWin;
	minimalclosure	mc;
	int		icon_x, icon_y;

	/*
	 * If the window is thought to be new (i.e. if ourWinInfo is null, as
	 * it is always except for the case of pinned menus and the VDM) and
	 * the window * has already been registered in the WinInfo database
	 * and it's anything other than colormap window, then return.
	 *
	 * This is to head off (a) clients that might be mapping the olwm
	 * frame, (b) clients that map their top-level window (pane) more than
	 * once before olwm can reparent it to a frame, and (c) olwm's own 
	 * popup menus.
	 */
	if (!ourWinInfo &&
	   (winGeneric = WIGetInfo(window)) != NULL &&
	    winGeneric->core.kind != WIN_COLORMAP) {
		return NULL;
	}

	/* Find the screen the client window is on.
	 * If ourWinInfo is valid, use it's screen
	 * Else if know the root then use it's screen
	 * Lastly QueryTree to find out from the server
	 */
	if (ourWinInfo) {
		screen = ourWinInfo->core.client->scrInfo->screen;
	} else if (rootWin != None) {
		if ((scrInfo = GetScrInfoOfRoot(rootWin)) == NULL) 
			return NULL;
		screen = scrInfo->screen;
	} else {
		Window	root, parent, *children;
		unsigned int	nChild;
		Status	result;
		
		result  = XQueryTree(dpy, window, &root, &parent, 
				&children, &nChild);

		if (result == 0 || parent != root)
			return NULL;
		if ((scrInfo = GetScrInfoOfRoot(root)) == NULL) 
			return NULL;
		screen = scrInfo->screen;
	}

	/*
	 * Select for events on the pane right now (including StructureNotify)
	 * so that we are guaranteed to get a DestroyNotify if the window goes
	 * away.  If the window has already gone away, the call to
	 * XGetWindowAttributes below will tell us without race conditions.
	 */
	if (!ourWinInfo)
	    XSelectInput(dpy, window,
			 PropertyChangeMask | StructureNotifyMask |
			 ColormapChangeMask | EnterWindowMask);

        /* get all the info about the new pane */
        status = XGetWindowAttributes(dpy, window, &paneAttr);
        if ( status == 0 ) {
            return NULL;
        }

	/*
	 * If it's an override-redirect window, or if already exists but is 
	 * unmapped, ignore it after first removing our StructureNotify 
	 * interest.
	 */
	if (paneAttr.override_redirect ||
		(fexisting && paneAttr.map_state != IsViewable)) {
	    if (!ourWinInfo)
		XSelectInput(dpy, window, NoEventMask);
	    return NULL;
	}

	/* Create the client structure so we can start hooking things to it */
	if ((cli = ClientCreate(dpy,screen)) == NULL)
	{
	    return NULL;
	}

#ifdef SHAPE
	{
	    Bool bshaped, cshaped;
	    int bx, by, cx, cy;
	    unsigned int bw, bh, cw, ch;

	    if (ShapeSupported &&
		0 != XShapeQueryExtents(dpy, window, &bshaped, &bx, &by,
					&bw, &bh, &cshaped, &cx, &cy,
					&cw, &ch))
	    {
		XShapeSelectInput(dpy, window, ShapeNotifyMask);
		cli->isShaped = bshaped;
	    } else {
		cli->isShaped = False;
	    }
	}
#endif /* SHAPE */

	/*
 	 * Turn on prop read filtering with set of available properties
 	 */
	PropSetAvailable(dpy,window);

	/*
	 * Get the WM_TRANSIENT_FOR hint.  If the property exists but has a
	 * contents of zero, or the window itself, substitute the root's
	 * window ID.  This is because some (buggy) clients actually write
	 * zero in the WM_TRANSIENT_FOR property, and we want to give them
	 * transient window behavior.
	 */
	if (!PropGetWMTransientFor(dpy,window,cli->scrInfo->rootid,
					&(cli->transientFor))) {
		cli->transientFor = 0;
		transient = False;
	} else {
		transient = True;
	}

	/*
	 * Get the WM_NORMAL_HINTS property.  If it's short, then we have a
	 * pre-ICCCM client on our hands, so we interpret some values 
	 * specially.
	 */
	normHints = MemNew(XSizeHints);

	if (!PropGetWMNormalHints(dpy,window,normHints,&preICCCM)) {
		normHints->win_gravity = NorthWestGravity;
		normHints->flags = PWinGravity;
	}

	/*
	 * We got a short property.  Assume that this is a pre-X11R4
	 * client who's using the short version of the property.  Copy
	 * the data into a correctly-sized structure.  Then, depending
	 * on the flags set, ignore the window's real geometry and use
	 * the data in the hint (but only if it passes some sanity 
	 * checking).  The sanity checking is necessary because early 
	 * versions of XView write a short property, but rely on the 
	 * window manager to look at the window's geometry instead of 
	 * at the values in the hint.
	 */
	if (preICCCM) {
		int	 maxDpyWidth = 2*DisplayWidth(dpy,screen);
		int	 maxDpyHeight = 2*DisplayHeight(dpy,screen);

		if (!fexisting
		    && (normHints->flags & (USPosition|PPosition))
		    && IsInt16(normHints->x) 
		    && IsInt16(normHints->y)
		    && normHints->x > -maxDpyWidth
		    && normHints->y > -maxDpyHeight
		    && normHints->x < maxDpyWidth
		    && normHints->y < maxDpyHeight) {
			paneAttr.x = normHints->x;
			paneAttr.y = normHints->y;
		}
		if ((normHints->flags & (USSize|PSize)) 
		    && IsCard16(normHints->width) 
		    && IsCard16(normHints->height)
		    && normHints->width >= MINSIZE
		    && normHints->height >= MINSIZE
		    && normHints->width < maxDpyWidth
		    && normHints->height < maxDpyHeight) {
			paneAttr.width = normHints->width;
			paneAttr.height = normHints->height;
		}
	}

	cli->normHints = normHints;

	/*
	 * Get the WM_HINTS
	 */
	cli->wmHints = MemNew(XWMHints);

	if (!PropGetWMHints(dpy,window,cli->wmHints)) {
		cli->wmHints->flags = 0L;
	}

	/* 
	 * Get the protocols in which the client will participate
	 */
	if (!PropGetWMProtocols(dpy,window,&(cli->protocols))) {
		cli->protocols = 0;
	}

        /* 
	 * Figure out what focus mode this window intends
	 */
	cli->focusMode = focusModeFromHintsProtocols(cli->wmHints,
						     cli->protocols);

	/* 
	 * Get the window class and instance strings
	 */
	if (!PropGetWMClass(dpy,window,&(cli->wmClass),&(cli->wmInstance))) {
		cli->wmClass = cli->wmInstance = NULL;
	}

	/* 
	 * Get the OpenLook window type and associated decorations
	 */
	cli->wmDecors = getOLWinDecors(dpy, window, transient, cli);

	/*
	 * Establish window groups.  Policy: if the window is transient, this 
	 * takes priority over any window group specified in WM_HINTS.  If
	 * it's transient, make it be part of the window group of the window 
	 * it is transient for.  Otherwise, use the group specified in
	 * WM_HINTS.  If no group is specified in WM_HINTS, consider the 
	 * window to be the leader of its own group.
	 */
	if (transient) {
	    winGeneric = WIGetInfo(cli->transientFor);
	    if (winGeneric != NULL)
		cli->groupid = winGeneric->core.client->groupid;
	    else
		cli->groupid = cli->transientFor;
	} else if ((cli->wmHints) && (cli->wmHints->flags & WindowGroupHint)) {
	    cli->groupid = cli->wmHints->window_group;
	} else {
	    cli->groupid = window;
	}

	if (cli->groupid == window)
	    cli->groupmask = GROUP_LEADER;
	else
	{
	    if (((cli->wmDecors->flags & WMDecorationPushPin) &&
		 (cli->wmDecors->menu_type == MENU_LIMITED))
		|| transient)
	    {
		cli->groupmask = GROUP_DEPENDENT;
	    }
	    else
	    {
		cli->groupmask = GROUP_INDEPENDENT;
	    }
	}
	GroupAdd(cli->groupid,cli,cli->groupmask);

	/* 
	 * Officially set up the frame
	 */
	winFrame = MakeFrame(cli,window,&paneAttr);

	/*
	 * If the hints don't specify a location, we choose a suitable one, 
	 * taking into account the window size and decoration sizes.
	 */
	if ( !fexisting &&
	     !(normHints->flags & USPosition) &&
	    (!(normHints->flags & PPosition) ||
	     (GRV.PPositionCompat && paneAttr.x <= 1 && paneAttr.y <= 1)))
	{
	    calcPosition(dpy, screen, &paneAttr, winFrame);
	}
	else if (!fexisting && (normHints->flags & USPosition) &&
		 !GRV.UseRelativePosition) {
	    paneAttr.x += cli->scrInfo->vdm->offsetX;
	    paneAttr.y += cli->scrInfo->vdm->offsetY;
	}

	/*
	 * See if the position needs to be constrained to a logical
	 * screen due to entries in .olvwmrc
	 */
	if (!XFetchName(dpy, window, &mc.name))
	    mc.name = NULL;
	icon_x = icon_y = 0;
	if (cli->groupmask != GROUP_DEPENDENT || !GroupLeader(cli->groupid))
	    if (cli->wmHints)
	        if (cli->wmHints->flags & IconPositionHint)
	            SearchProgString(dpy, cli->scrInfo, mc.name,
				cli->wmInstance, cli->wmClass,
				&paneAttr.x, &paneAttr.y,
			        &cli->wmHints->icon_x, &cli->wmHints->icon_y);
	        else SearchProgString(dpy, cli->scrInfo, mc.name,
				cli->wmInstance, cli->wmClass,
				&paneAttr.x, &paneAttr.y,
			        &icon_x, &icon_y);
	    else SearchProgString(dpy, cli->scrInfo, mc.name, NULL, NULL,
			         &paneAttr.x, &paneAttr.y, &icon_x, &icon_y);
	else {
	    /*
	     * Map the popup on the same screen as the group leader
	     */
	    Client *leader = GroupLeader(cli->groupid);
	    int	curScreen;
	    int	dw = DisplayWidth(dpy, screen);
	    int	dh = DisplayHeight(dpy, screen);

	    if (leader != NULL) {
		curScreen = (leader->framewin->core.x +
				(leader->framewin->core.width / 2)) / dw;
		if (leader->framewin->core.x +
				(leader->framewin->core.width / 2) < 0)
		    curScreen--;
		if (paneAttr.x + (int) paneAttr.width < 0)
		    paneAttr.x = dw + (paneAttr.x % dw) + (curScreen * dw);
		else paneAttr.x = (paneAttr.x % dw) + (curScreen * dw);
		curScreen = (leader->framewin->core.y +
				(leader->framewin->core.height / 2)) / dh;
		if (leader->framewin->core.y +
				(leader->framewin->core.height / 2) < 0)
		    curScreen--;
		if (paneAttr.y + (int) paneAttr.height < 0)
		    paneAttr.y = dh + (paneAttr.y % dh) + (curScreen * dh);
		else paneAttr.y = (paneAttr.y % dh) + (curScreen * dh);
	    }
	}
	    

	/*
	 * If a non-olwm created window then create the pane for it
	 * Else if it is a pinned menu then call the creation callback
	 * so that it can fix up its pane
	 */
	if (ourWinInfo == NULL) {
		winPane = MakePane(cli,winFrame,window,&paneAttr);
	} else {
		winPane = ourWinInfo;
		(WinClass(winPane)->core.createcallback)(ourWinInfo,cli,
								winFrame);
	}

	/* 
	 * Officially set up the icon
	 */
	winIcon = MakeIcon(cli,window,&paneAttr);
	winIconPane = MakeIconPane(cli,winIcon,cli->wmHints,fexisting);

	/* 
	 * Keep track of any subwindows that need colormap installation
	 */
	TrackSubwindows(cli);

	/* 
	 * Size and generally configure the frame window tree
	 */
	FrameSetPosFromPane(winFrame,paneAttr.x,paneAttr.y);
	WinCallConfig(dpy, winPane, NULL);

	/* 
	 * Size and generally configure the icon window tree
	 */
	WinCallConfig(dpy, winIconPane, NULL);
	if (cli->wmHints != NULL && (cli->wmHints->flags & IconPositionHint))
	    IconSetPos(winIcon,cli->wmHints->icon_x,cli->wmHints->icon_y);
	else
	    IconSetPos(winIcon,icon_x,icon_y);
	WinCallConfig(dpy, winIcon, NULL);

        /*
	 * We manually move the icon pane window, since all the configuration
	 * has been done with the icon pane parented to root.
	 *
	 * No longer true, since we always have a parented icon frame
	 * now; see winipane.c
        WinRootPos(winIconPane, &tmpx, &tmpy);
        XMoveWindow(dpy, winIconPane->core.self, tmpx, tmpy);
	 */

	/*
	 * Set the sticky bit according to the sticky list.  We check the
	 * window's group below (stickyness is inherited by group).
	 * sticky was set when the client was created; if this is a VDM
	 * then sticky will be set to true in the callback
	 */
	mc.class = cli->wmClass;
	mc.instance = cli->wmInstance;
	if (ListApply(GRV.StickyList, matchInstClass, &mc) != NULL)
	    cli->sticky = True;
	if (mc.name)
	    XFree(mc.name);

	/* 
	 * Determine the proper initial state of the window. 
	 * If the window already exists and there is a WM_STATE property 
	 * then use the state that the last window manager left there, 
	 * otherwise use WM_HINTS.
	 */
	if (fexisting &&
	    PropGetWMState(dpy,winPane->core.self,&winState,&iconWin)) {
	    if (winState == IconicState)
		initstate = IconicState;
	    else
		initstate = NormalState;
	} else {
	    /* For new windows, check the initial_state field of WM_HINTS. */
	    if (cli->wmHints && (cli->wmHints->flags & StateHint)
		&& (cli->wmHints->initial_state == IconicState))
		    initstate = IconicState;
	    else
		    initstate = NormalState;
	}

	/*
	 * Don't allow the popup into iconic state if its leader is in normal 
	 * state.
	 *
	 * Also set the sticky field of the client based on the leader
	 */
	if (cli->groupmask == GROUP_DEPENDENT) {
	    Client *leader = GroupLeader(cli->groupid);
	    if (leader != NULL) {
		if (leader->wmState == NormalState && initstate == IconicState)
		    initstate = NormalState;
		cli->sticky = leader->sticky;
	    }
	}

	if (cli->sticky)
	    MakeSticky(cli, True);
	MakeVirtual(cli);
	ClientProcessDragDropInterest(cli, PropertyNewValue);

	/*
	 * Put the window into the correct initial state
	 */
	switch ( initstate ) {
	case NormalState:
	    cli->wmState = NormalState;
	    MapRaised(winFrame);
	    XMapRaised(dpy, winPane->core.self);
	    if (!fexisting) {
		FrameWarpPointer(cli);
		if (GRV.AutoInputFocus)
		    ClientSetFocus(cli, True, CurrentTime);
		if (GRV.AutoColorFocus)
		    LockColormap(dpy, cli, winPane);
	    }
	    break;
	case IconicState:
	    cli->wmState = IconicState;
	    /* unmap the window in case it was mapped originally */
	    XUnmapWindow(dpy, winPane->core.self);
	    winPane->pcore.pendingUnmaps++;
	    /* dependent group followers don't get their own icons */
	    if (cli->groupmask != GROUP_DEPENDENT)
		IconShow(cli, winIcon);
	    break;
	}
	ClientSetWMState(cli);

	/* 
	 * Get the window state
	 */
	ClientGetWindowState(cli);

	/*
 	 * Turn off prop read filtering
 	 */
	PropClearAvailable();

	return cli;
}

/*
 * ReparentTree -- called at start up, this routine queries the window
 *	tree and reparents all the windows 
 */
void
ReparentTree(dpy,treeroot)
Display	*dpy;
Window 	treeroot;
{
	unsigned int numChildren;
	Window *children, root, parent, w;
	int ii;
	Client *cli;

	children = NULL;

	if (XQueryTree(dpy, treeroot, &root, &parent,
				      &children, &numChildren)) 
	{
	    for (ii=0; ii<numChildren; ii++)
	    {
		w = children[ii];
		if (WIGetInfo(w) == NULL)
		{
	            cli = StateNew(dpy, treeroot, w, True, NULL);
		    if (cli != NULL)
		    {
			cli->framewin->fcore.panewin->pcore.pendingUnmaps++;	
					/* unmap because of reparent */
		    }
		}
	    }
	}

	if (children != NULL)
		XFree((char *)children);
}


/* 
 * StateNormIcon - transition a window to IconicState from NormalState
 */
void
StateNormIcon(cli)
Client *cli;
{
	WinIconFrame *winIcon = cli->iconwin;

	if (winIcon == NULL)
		return;

	/* don't do some other transition */
	if (cli->wmState != NormalState)
		return;

	/* we can't iconify if we are a dependent */
	if (cli->groupmask == GROUP_DEPENDENT)
		return;

	/* Map the icon window */
	IconShow(cli, winIcon);

	/* iconify self and dependents */
	if (cli->groupmask == GROUP_LEADER)
	    GroupApply(cli->groupid, iconifyOne, winIcon,
		       GROUP_LEADER|GROUP_DEPENDENT);
	else
	    iconifyOne(cli,winIcon);

	if (cli == CurrentClient && !GRV.FocusFollowsMouse)
	    ClientSetFocus(cli, False, TimeFresh(cli->iconwin));
}

/* 
 * StateIconNorm - transition a window to NormalState from IconicState 
 */
void
StateIconNorm(cli)
Client *cli;
{
	WinIconFrame *winIcon = cli->iconwin;

	if (winIcon == NULL)
		return;

	/* don't do some other transition */
	if (cli->wmState != IconicState)
		return;

	if (cli->groupmask == GROUP_LEADER)
	    deiconifyGroup(cli, winIcon);
	else
	    deiconifyOne(cli,winIcon,True);

	/* Unmap icons */
	IconHide(cli, winIcon);

	if (cli == CurrentClient && !GRV.FocusFollowsMouse)
	    ClientSetFocus(cli, True, TimeFresh(cli->framewin));
}

/*
 * StateWithdrawn - a window is being withdrawn; tear down all related
 *	structures; clear the client out of all lists it may be
 * 	on; reparent the pane window
 */
void
StateWithdrawn(cli)
Client *cli;
{
	WinIconFrame *iconInfo = cli->iconwin;
	WinPaneFrame *frameInfo = cli->framewin;
	WinPane *paneInfo;
	Window pane;
	Display *dpy = cli->dpy;

	if (iconInfo == NULL || frameInfo == NULL)
		return;

	paneInfo = (WinPane*)(frameInfo->fcore.panewin);
	pane = paneInfo->core.self;
	iconInfo = cli->iconwin;

        /* Return the pointer if necessary */
	FrameUnwarpPointer(cli);

        /* Unmap the frame and pane. */
        UnmapWindow(frameInfo);
        XUnmapWindow(dpy, pane);

	/* Unmap the icon */
	if (iconInfo != NULL)
	    IconHide(cli, iconInfo);

	/* move the pane and unparent it */
	FrameUnparentPane(cli, frameInfo, paneInfo);

	DestroyClient(cli);
}

/************************************************************************
 *		Top-Level Window Property Update Functions
 ************************************************************************/

/*
 * Refresh SizeHints from WM_NORMAL_HINTS property.  The new values
 * can simply be copied into the client's normHints.
 */
void
StateUpdateWMNormalHints(cli,event)
	Client		*cli;
	XPropertyEvent	*event;
{
	Window		pane;
	XSizeHints	sizeHints;
	Bool		preICCCM;

	if (event->state != PropertyNewValue)
		return;

	pane = PANEWINOFCLIENT(cli);

	if (!PropGetWMNormalHints(cli->dpy,pane,&sizeHints,&preICCCM))
		return;

	*(cli->normHints) = sizeHints;
}

/*
 * Reapply WMHints from the WM_HINTS property.  Ignore everything but
 * InputHint and Icon{Pixmap/Mask}Hint.
 */
void
StateUpdateWMHints(cli,event)
	Client		*cli;
	XPropertyEvent	*event;
{
	Window		pane;
	XWMHints	wmHints;
	WinIconPane	*iconPane;
	Pixmap		iconMask;

	if (cli->framewin == NULL || cli->iconwin == NULL ||
	    event->state != PropertyNewValue)
		return;

	pane = PANEWINOFCLIENT(cli);
	iconPane = (WinIconPane *)cli->iconwin->fcore.panewin;

	if (!PropGetWMHints(cli->dpy,pane,&wmHints))
		return;

	if (wmHints.flags & InputHint) {
		cli->focusMode = 
			focusModeFromHintsProtocols(&wmHints,cli->protocols);
	}

	if (wmHints.flags & IconPixmapHint)
		IconPaneSetPixmap(cli->dpy,iconPane,wmHints.icon_pixmap);
	if (wmHints.flags & IconMaskHint) 
		IconPaneSetMask(cli->dpy,iconPane,wmHints.icon_mask);
	if (wmHints.flags & IconPixmapHint || wmHints.flags & IconMaskHint) 
		WinDrawFunc(iconPane);

	if (cli->wmHints == NULL)
		cli->wmHints = MemNew(XWMHints);

	*(cli->wmHints) = wmHints;
}

/*
 * Reset client protocols and focusMode from WM_PROTOCOLS
 */
void
StateUpdateWMProtocols(cli,event)
	Client		*cli;
	XPropertyEvent	*event;
{
	Window		pane;
	int		protocols;

	if (cli->framewin == NULL || event->state != PropertyNewValue)
		return;

	pane = PANEWINOFCLIENT(cli);

	if (!PropGetWMProtocols(cli->dpy,pane,&protocols))
		return;

	if (cli->protocols == protocols)
		return;

	cli->focusMode = focusModeFromHintsProtocols(cli->wmHints,protocols);
	cli->protocols = protocols;
}

/*
 * StateUpdateWinAttr - reread the _OL_WIN_ATTR property. 
 *	For now just apply WA_PINSTATE.
 */
void
StateUpdateWinAttr(cli,event)
	Client		*cli;
	XPropertyEvent	*event;
{
	OLWinAttr	winAttr;
	Bool		old;
	Window		pane;

	if (cli->framewin == NULL || event->state != PropertyNewValue)
		return;

	pane = PANEWINOFCLIENT(cli);

	if (!PropGetOLWinAttr(cli->dpy,pane,&winAttr,&old))
		return;
		
	if ((winAttr.flags & WA_PINSTATE) && ClientIsPinnable(cli)) {
		WinPushPin *pushPin = (WinPushPin *)cli->framewin->winDeco;
		PushPinSetPinState(cli->dpy,pushPin,
					winAttr.pin_initial_state,False);
	}
}

/*
 * StateUpdateDecorAdd - read the DecorAdd property and reapply it
 */
void
StateUpdateDecorAdd(cli,event)
	Client		*cli;
	XPropertyEvent	*event;
{
	/* REMIND - this needs to be implemented */
}

/*
 * StateUpdateDecorDel - read the DecorDel property and reapply it
 */
void
StateUpdateDecorDel(cli,event)
	Client		*cli;
	XPropertyEvent	*event;
{
	/* REMIND - this needs to be implemented */
}

/*
 * Check to see if a given focus action requires any change to the
 * key bindings
 */

typedef struct {
    List **list;
    KeySym start_sym;
    KeySym end_sym;
    unsigned int modstate;
    Boolean  state;
    char *desc;
} FocusClosure;

/*
 * Check the binding state for a given key group
 */
static void
checkGroupBinding(dpy, mc, focus, fcl)
    Display *dpy;
    minimalclosure *mc;
    Boolean focus;
    FocusClosure *fcl;
{
    List *list = *(fcl->list);
    Boolean newstate = fcl->state;

    newstate = focus ? (ListApply(list, matchInstClass, mc) != NULL) : False;

    if (newstate != fcl->state) {
      fcl->state = newstate;
      GrabVKeys(dpy, False, True);
      SetProgKeys(dpy, fcl->start_sym, fcl->end_sym, fcl->modstate, fcl->state);
      GrabVKeys(dpy, True, True);
    }
}

/*
 * Whenever the focus changes, see if we need to enable/disable certain
 * function keys
 */
CheckBindingState(win, focus)
      WinGenericFrame *win;
      Boolean focus;
{
      int i;
      minimalclosure mc;

      static FocusClosure fcl[] = {
          { &GRV.NoVirtualKey, NoSymbol, NoSymbol, 0L, False, "All" },
          { &GRV.NoVirtualFKey, XK_F1, XK_F12, 0L, False, "F" },
          { &GRV.NoVirtualLKey, XK_L1, XK_L10, 0L, False, "L" },
          { &GRV.NoVirtualRKey, XK_R1, XK_R15, 0L, False, "R" }
      };

      mc.name = win->fcore.name;
      mc.instance = win->core.client->wmInstance;
      mc.class = win->core.client->wmClass;

      for ( i = 0; i < sizeof(fcl) / sizeof(fcl[0]); i++)
          checkGroupBinding(win->core.client->dpy, &mc, focus, &fcl[i]);
}
