/*
	TransSkel version 1.02 - Transportable application skeleton
	
	TransSkel is public domain and is written by:

			Paul DuBois
			Wisconsin Regional Primate Research Center
			1220 Capital Court
			Madison WI  53706  USA

	UUCP:	allegra,ihnp4,seismo}!uwvax!uwmacc!dubois
	ARPA:	dubois@unix.macc.wisc.edu
			dubois@rhesus.primate.wisc.edu

	This version of TransSkel written for LightspeedC.  LightspeedC is a
	trademark of:
			THINK Technologies, Inc
			420 Bedford Street  Suite 350
			Lexington, MA  02173  USA

  History
  06/13/86	Beta version.
  08/27/86	Version number changed to 1.01.
  			v1.0 DoGrow bug fixed - the port at the point of the
  			InvalRect could have been anything; the fix is to set
  			the port to the grown window first.  This also explains
  			why the kludge to DoActivate in v1.0 worked.
  10/02/86	Version number changed to 1.02, as a result of adding
			modifications by David W. Berry (well!dwb@lll-lcc.arpa)
			for supporting window zooming.  Also used his modifications
			for supporting modeless dialogs (though not in the same
			form).  Dialogs can be #define'd on or off.
*/


/*
	The following symbol controls support for dialogs.
	Changing #define to #undef disables the support.
*/

# define	supportDialogs


#ifdef MPWC
# ifdef		supportDialogs
#	include	<Dialogs.h>
# else
#	include	<Windows.h>
# endif

# include	<Events.h>
# include	<OSEvents.h>
# include	<Menus.h>
# include	<Memory.h>
# include	<DiskInit.h>
# include	<ToolUtils.h>
# include	<Fonts.h>
# include	<Desk.h>
#else
# ifdef		supportDialogs
#	include	<DialogMgr.h>
# else
#	include	<WindowMgr.h>
# endif

# include	<EventMgr.h>
# include	<MenuMgr.h>
#endif MPWC


# define	nil			0L
# define	mBarHeight	20	/* menu bar height.  All window sizing
							   code takes this into account */


/*
	This window zooming stuff may need to be removed if/when Think
	supports it in the compiler.
*/

#ifdef DODO
pascal short	TrackBox() = 0xa83b;	/* declare traps */
pascal void		ZoomWindow() = 0xa83a;
#endif DODO

/* enum */									/* declare part codes */
/* {
	inZoomIn = 7,
	inZoomOut
}; */



/*
	New(TypeName) returns handle to new object, for any TypeName.
	If there is insufficient memory, the result is nil.
*/

# define	New(x)	(x **) NewHandle ((Size) sizeof (x))


/*
	Window and Menu handler types, constants, variables.

	whList and mhList are the lists of window and menu handlers.
	whClobOnRmve and mhClobOnRmve are true if the handler disposal proc
	is to be called when a handler is removed.  They are temporarily set
	false when handlers are installed for windows or menus that already
	have handlers - the old handler is removed WITHOUT calling the
	disposal proc.

	Default lower limits on window sizing of 80 pixels both directions is
	sufficient to allow text windows room to draw a grow box and scroll
	bars without having the thumb and arrows overlap.  These values may
	be changed if such a constraint is undesirable with SkelGrowBounds.
	Default upper limits are for the Macintosh, not the Lisa, but are set
	per machine in SkelInit.
*/

typedef struct WHandler
{
	WindowPtr	whWind;			/* window/dialog to be handled  */
	ProcPtr		whClobber;		/* data structure disposal proc */
	ProcPtr		whMouse;		/* mouse-click handler proc     */
	ProcPtr		whKey;			/* key-click handler proc       */
	ProcPtr		whUpdate;		/* update handler proc          */
	ProcPtr		whActivate;		/* activate event handler proc  */
	ProcPtr		whClose;		/* close "event" handler proc   */
	ProcPtr		whIdle;			/* main loop proc               */
# ifdef	supportDialogs
	ProcPtr		whEvent;		/* event proc                   */
# endif
	Rect		whGrow;			/* limits on window sizing      */
	Boolean		whSized;		/* true = window was resized    */
	Boolean		whFrontOnly;	/* true = idle only when active */
	struct WHandler	**whNext;	/* next window handler          */
} WHandler;

static WHandler	**whList = nil;
static Boolean	whClobOnRmve = true;
static Rect		growRect = { 80, 80, 512, 342 - mBarHeight };


typedef struct MHandler
{
	int				mhID;			/* menu id                     */
	ProcPtr			mhSelect;		/* item selection handler proc */
	ProcPtr			mhClobber;		/* menu disposal handler proc  */
	ProcPtr			mhUpdate;		/* menu update handler, L. Tierney */
	struct MHandler	**mhNext;		/* next menu handler           */
} MHandler;


static MHandler	**mhList = nil;			/* list of menu handlers */
static Boolean	mhClobOnRmve = true;


/*
	Variables for default Apple menu handler.  appleID is set to 1 if
	SkelApple is called and is the id of the Apple menu, appleAboutProc
	is the procedure to execute if there is an About... item and it's
	chosen from the Apple menu.  If doAbout is true, then the menu
	contains the About... item, otherwise it's just desk accessories.
*/

static MenuHandle	appleMenu;
static int			appleID = 0;
static ProcPtr		appleAboutProc = nil;
static Boolean		doAbout = false;


/*
	Miscellaneous

	screenPort points to the window manager port.
	
	doneFlag determines when SkelMain returns.  It is set by calling
	SkelWhoa(), which the host does to request a halt.

	pBkgnd points to a background procedure, to be run during event
	processing.  Set it with SkelBackground.  If nil, there's no
	procedure.

	pEvent points to an event-inspecting hook, to be run whenever an
	event occurs.  Set it with SkelEventHook.  If nil, there's no
	procedure.

	eventMask controls the event types requested in the GetNextEvent
	call in SkelMain.

	diskInitPt is the location at which the disk initialization dialog
	appears, if an uninitialized disk is inserted.
*/

static GrafPtr	screenPort;
static int		doneFlag = false;
static ProcPtr	pBkgnd = nil;
static Boolean	(*pEvent)() = nil;
static int		eventMask = everyEvent;
static Point	diskInitPt = { /* v = */ 120, /* h = */ 100 };

# ifdef	supportDialogs

/*
	Events that are passed to dialogs.  Others are ignored.
	Standard mask passes , mousedown, keydown, autokey, update,
	activate and null events.  Null events are controlled by bit 0.
*/

static int	dlogEventMask = 0x16b;

# endif


/* -------------------------------------------------------------------- */
/*						Internal (private) Routines						*/
/* -------------------------------------------------------------------- */


/*
	Get handler associated with user or dialog window.
	Return nil if window doesn't belong to any known handler.
	This routine is absolutely fundamental to TransSkel.
*/


static WHandler **GetWDHandler (theWind)
WindowPtr	theWind;
{
register WHandler	**h;

	for (h = whList; h != nil; h = (**h).whNext)
	{
		if ((**h).whWind == theWind)
			return (h);
	}
	return (nil);
}


/*
	Get handler associated with user window.
	Return nil if window doesn't belong to any known handler.
	The order of the two tests is critical:  theWind might be nil.
*/

static WHandler **GetWHandler (theWind)
WindowPtr	theWind;
{
register WHandler	**h;

	if ((h = GetWDHandler (theWind)) != nil
		&& ((WindowPeek) theWind)->windowKind != dialogKind)
	{
			return (h);
	}
	return (nil);
}


# ifdef	supportDialogs

/*
	Get handler associated with dialog window.
	Return nil if window doesn't belong to any known handler.
	The order of the two tests is critical:  theDialog might be nil.
*/

static WHandler **GetDHandler (theDialog)
DialogPtr	theDialog;
{
register WHandler	**h;

	if ((h = GetWDHandler (theDialog)) != nil
		&& ((WindowPeek) theDialog)->windowKind == dialogKind)
	{
			return (h);
	}
	return (nil);
}

# endif


/*
	General menu-handler.  Just passes selection to the handler's
	select routine.  If the select routine is nil, selecting items from
	the menu is a nop.
*/

static DoMenuCommand (command)
long		command;
{
register int		menu;
register int		item;
register MHandler	**mh;
register ProcPtr	p;

	menu = HiWord (command);
	item = LoWord (command);
	for (mh = mhList; mh != nil; mh = (**mh).mhNext)
	{
		if ((menu == (**mh).mhID) && ((p = (**mh).mhSelect) != nil))
		{
			(*p) (item, menu);/* menu argument added, L. Tierney */
			break;
		}
	}
	HiliteMenu (0);		/* command done, turn off menu hiliting */
}


/*
	Apple menu handler
	
	DoAppleItem:  If the first item was chosen, and there's an "About..."
	item, call the procedure associated with it (if not nil).  If there
	is no "About..." item or the item was not the first one, then open
	the associated desk accessory.  The port is saved and restored
	because OpenDeskAcc does not always preserve it correctly.
	
	DoAppleClobber disposes of the Apple menu.
*/


static DoAppleItem (item)
int		item;
{
GrafPtr		curPort;
Str255		str;

	if (doAbout && item == 1)
	{
		if (appleAboutProc != nil)
			(*appleAboutProc) ();
	}
	else
	{
		GetPort (&curPort);
		GetItem (appleMenu, item, str);		/* get DA name */
		(void) OpenDeskAcc (str);			/* open it */
		SetPort (curPort);
	}
}

static DoAppleClobber () { DisposeMenu (appleMenu); }


/* -------------------------------------------------------------------- */
/*						Window-handler routing routines					*/
/*																		*/
/*	Each routine sets the port to the handler's window before executing	*/
/*	the handler procedure.												*/
/* -------------------------------------------------------------------- */


/*
	Pass local mouse coordinates, click time, and the modifiers flag
	word to the handler.
*/

static DoMouse (h, theEvent)
WHandler	**h;
EventRecord	*theEvent;

{
register ProcPtr	p;
Point				thePt;

	if (h != nil)
	{
		SetPort ((**h).whWind);
		if ((p = (**h).whMouse) != nil)
		{
			thePt = theEvent->where;	/* make local copy */
			GlobalToLocal (&thePt);
			(*p) (thePt, theEvent->when, theEvent->modifiers);
		}
	}
}


/*
	Pass the character and the modifiers flag word to the handler.
*/

static DoKey (h, ch, mods)
WHandler	**h;
char		ch;
int			mods;
{
register ProcPtr	p;

	if (h != nil)
	{
		SetPort ((**h).whWind);
		if ((p = (**h).whKey) != nil)
			(*p) (ch, mods);
	}
}


/*
	Call the window updating procedure, passing to it an indicator whether
	the window has been resized or not.  Then clear the flag, assuming
	the update proc took whatever action was necessary to respond to
	resizing.

	If the handler doesn't have any update proc, the Begin/EndUpdate
	stuff is still done, to clear the update region.  Otherwise the
	Window Manager will keep generating update events for the window,
	stalling updates of other windows.

	Make sure to save and restore the port, as it's not always the
	active window that is updated.
*/

static DoUpdate (h)
WHandler	**h;

{
register WHandler	**rh;
register ProcPtr	p;
register GrafPtr	updPort;
GrafPtr				tmpPort;

	if ((rh = h) != nil)
	{
		GetPort (&tmpPort);
		SetPort (updPort = (**rh).whWind);
		BeginUpdate (updPort);
		if ((p = (**rh).whUpdate) != nil)
		{
			(*p) ((**rh).whSized);
			(**rh).whSized = false;
		}
		EndUpdate (updPort);
		SetPort (tmpPort);
	}
}


/*
	Pass activate/deactivate notification to handler.
*/

static DoActivate (h, active)
WHandler	**h;
Boolean		active;

{
register ProcPtr	p;

	if ((h != nil) && ((p = (**h).whActivate) != nil))
	{
		SetPort ((**h).whWind);
		(*p) (active);
	}
}


/*
	Execute a window handler's close proc.  This may be used by handlers
	for temp windows that want to remove themselves when the window
	is closed:  they can call SkelRmveWind to dispose of the window
	and remove the handler from the window handler list.  Thus, windows
	may be dynamically created and destroyed without filling up the
	handler list with a bunch of invalid handlers.
	
	If the handler doesn't have a close proc, just hide the window.
	The host should provide some way of reopening the window (perhaps
	a menu selection).  Otherwise the window will be lost from user
	control if it is hidden, since it won't receive user events.

	The port is set to the window manager port after calling the
	handler proc, to avoid a dangling port.

	This is called both for regular and dialog windows.
*/

static DoClose (h)
WHandler	**h;
{
register WHandler	**rh;
register ProcPtr	p;

	if ((rh = h) != nil)
	{
		SetPort ((**rh).whWind);
		if ((p = (**rh).whClose) != nil)
			(*p) ();
		else
			HideWindow ((**rh).whWind);
		SetPort (screenPort);
	}
}


/*
	Execute a window handler's clobber proc.

	The port is set to the window manager port after calling the
	handler proc, to avoid a dangling port.

	This is called both for regular and dialog windows.
*/

static DoClobber (h)
WHandler	**h;
{
register ProcPtr	p;

	if (h != nil)
	{
		SetPort ((**h).whWind);
		if ((p = (**h).whClobber) != nil)
			(*p) ();
		SetPort (screenPort);
	}
}


/*
	Execute handler's idle proc.

	Make sure to save and restore the port, since idle procs may be
	called for any window, not just the active one.
*/

static DoIdle (h)
WHandler	**h;
{
register ProcPtr	p;
GrafPtr				tmpPort;

	if (h != nil)
	{
		GetPort (&tmpPort);
		SetPort ((**h).whWind);
		if ((p = (**h).whIdle) != nil)
			(*p) ();
		SetPort (tmpPort);
	}
}


# ifdef	supportDialogs

/* -------------------------------------------------------------------- */
/*							Dialog-handling routines					*/
/* -------------------------------------------------------------------- */


/*
	Handle event if it's for a dialog.  The event must be one of
	those that is passed to dialogs according to dlogEventMask.
	This mask can be set so that disk-inserts, for instance, don't
	get eaten up.
*/

static DoDialog (theEvent)
EventRecord		*theEvent;
{
register WHandler	**dh;
DialogPtr			theDialog;
register int		what;
short				item;
GrafPtr				tmpPort;

/*
	handle command keys before they get to IsDialogEvent
*/

	what = theEvent->what;
	if((what == keyDown || what == autoKey) && (theEvent->modifiers & cmdKey))
	{
	   	DoMenuCommand (MenuKey (theEvent->message & charCodeMask));
	   	return (true);
	}
	
	if(((1 << what) & dlogEventMask) && IsDialogEvent (theEvent))
	{
		
		if (DialogSelect (theEvent, &theDialog, &item)
		   && (dh = GetDHandler (theDialog)) != nil
		   && (**dh).whEvent != nil)
		{
			GetPort (&tmpPort);
			SetPort (theDialog);
			(*(**dh).whEvent) (item, theEvent);
			SetPort (tmpPort);
		}
		else if (theEvent->what == activateEvt) { /* L. Tierney */
		  GetPort (&tmpPort);
		  SetPort (theDialog);
          activateDialog((theEvent->modifiers & activeFlag) != 0);
		  SetPort (tmpPort);
        }
		return (true);
	}
	return (false);
}

# endif


/* -------------------------------------------------------------------- */
/*							Event-handling routines						*/
/* -------------------------------------------------------------------- */


/*
	Have either sized or zoomed the window.  Invalidate it to force
	an update and set the 'resized' flag in the window handler true.
*/

static TriggerUpdate (h, thePort)
WHandler	**h;
GrafPtr		thePort;
{
	SetPort (thePort);
	InvalRect (&thePort->portRect);
	if (h != nil)
		(**h).whSized = true;
}


/*
	Size a window.  If the window has a handler, use the grow limits
	in the handler record, otherwise use the defaults.

	The portRect is invalidated to force an update event.  (The port
	must be set first, as it could be pointing anywhere.)  The handler's
	update procedure should check the parameter passed to it to check
	whether the window has changed size, if it needs to adjust itself to
	the new size.  THIS IS A CONVENTION.  Update procs must notice grow
	"events", there is no procedure specifically for such events.
	
	The clipping rectangle is not reset.  If the host application
	keeps the clipping set equal to the portRect or something similar,
	then it will have to arrange to treat window growing with more
	care.
*/

static DoGrow (h, thePort, startPt)
WHandler	**h;
GrafPtr		thePort;
Point		startPt;
{
Rect				r;
register long		growRes;

	if (h != nil)
		r = (**h).whGrow;
	else
		r = growRect;	/* use default */

	/* grow result non-zero if size change	*/

	if (growRes = GrowWindow (thePort, startPt, &r))
	{
		SizeWindow (thePort, LoWord (growRes), HiWord (growRes), false);
		TriggerUpdate (h, thePort);
	}
}


/*
	Zoom the current window.  Very similar to DoGrow
*/

DoZoom (h, thePort, partCode)
register WHandler	**h;
GrafPtr				thePort;
short				partCode;
{
	ZoomWindow (thePort, partCode, 0);
	TriggerUpdate (h, thePort);
}


/*
	General event handler
*/

static DoEvent (theEvt)
EventRecord	*theEvt;

{
register EventRecord	*theEvent;
Point					evtPt;
GrafPtr					evtPort;
register int			evtPart;
register char			evtChar;
register int			evtMods;
register WHandler		**h;
Rect					r;

	theEvent = theEvt;

# ifdef	supportDialogs

	if(DoDialog (theEvent))
		return;

# endif

	evtPt = theEvent->where;
	switch (theEvent->what)
	{

		case nullEvent:
			break;
/*
	Mouse click.  Get the window that the click occurred in, and the
	part of the window.
*/
		case mouseDown:
		{
			evtPart = FindWindow (evtPt, &evtPort);
			h = GetWHandler (evtPort);

			switch (evtPart)
			{
/*
	Click in a desk accessory window.  Pass back to the system.
*/
				case inSysWindow:
				{
					SystemClick (theEvent, evtPort);
					break;
				}
/*
	Click in menu bar.  Track the mouse and execute selected command,
	if any.
*/
				case inMenuBar:
				{
					UpdateMenus(); /* Menu update; L. Tierney */
					DoMenuCommand (MenuSelect (evtPt));
					break;
				}
/*
	Click in grow box.  Resize window.
*/
				case inGrow:
				{
					DoGrow (h, evtPort, evtPt);
					break;
				}
/*
	Click in title bar.  Drag the window around.  Leave at least
	4 pixels visible in both directions.
*/
				case inDrag:
				{
					r = screenPort->portRect;
					r.top += mBarHeight;			/* skip down past menu bar */
					InsetRect (&r, 4, 4);
					DragWindow (evtPort, evtPt, &r);
					break;
				}
/*
	Click in close box.  Call the close proc if the window has one.
*/
				case inGoAway:
				{
					if (TrackGoAway (evtPort, evtPt))
						DoClose (GetWDHandler (evtPort));
					break;
				}
/*
	Click in content region.  If the window wasn't frontmost (active),
	just select it, otherwise pass the click to the window's mouse
	click handler.
*/
				case inContent:
				{
					if (evtPort != FrontWindow ())
						SelectWindow (evtPort);
					else
						DoMouse (h, theEvent);
					break;
				}

/*
	Click in zoom box.  Track the click and then zoom the window if
	necessary
*/
				case inZoomIn:
				case inZoomOut:
				{
					if(TrackBox(evtPort, evtPt, evtPart))
						DoZoom (h, evtPort, evtPart);
					break;
				}

			}
			break;	/* mouseDown */
		}
/*
	Key event.  If the command key was down, process as menu item
	selection, otherwise pass the character and the modifiers flags
	to the active window's key handler.

	If dialogs are supported, there's no check for command-key
	equivalents, since that would have been checked in DoDialog.
*/
		case keyDown:
		case autoKey:
		{
			evtChar = theEvent->message & charCodeMask;
			evtMods = theEvent->modifiers;

# ifndef	supportDialogs
			if (evtMods & cmdKey)		/* try menu equivalent */
			{
				DoMenuCommand (MenuKey (evtChar));
				break;
			}

# endif

			DoKey (GetWHandler (FrontWindow ()), evtChar, evtMods);
			break;
		}
/*
	Update a window.
*/
		case updateEvt:
		{
			DoUpdate (GetWHandler ((WindowPtr) theEvent->message));
			break;
		}
/*
	Activate or deactivate a window.
*/
		case activateEvt:
		{
			DoActivate (GetWHandler ((WindowPtr) theEvent->message),
						((theEvent->modifiers & activeFlag) != 0));
			break;
		}
/*
	handle inserts of uninitialized disks
*/
		case diskEvt:
		{
			if (HiWord (theEvent->message) != noErr)
			{
				DILoad ();
				(void) DIBadMount (diskInitPt, theEvent->message);
				DIUnload ();
			}
			break;
		}
	}
}

/* menu updating on click in menu bar, added L. Tierney */
static UpdateMenus()
{ 
  MHandler **mh;
  ProcPtr p;
  
  for (mh = mhList; mh != nil; mh = (*mh)->mhNext)
    if ((p = (*mh)->mhUpdate) != nil) (*p)((*mh)->mhID);
}

/* -------------------------------------------------------------------- */
/*						Interface (public) Routines						*/
/* -------------------------------------------------------------------- */


/*
	Initialize the various Macintosh Managers.
	Set default upper limits on window sizing.
	FlushEvents does NOT toss disk insert events, so that disks
	inserted while the application is starting up don't result
	in dead drives.
*/

SkelInit ()
{
	MaxApplZone ();
	FlushEvents (everyEvent - diskMask, 0 );
#ifdef MPWC
	InitGraf (&(qd.thePort));
#else
	InitGraf (&thePort);
#endif MPWC	
	InitFonts ();
	InitWindows ();
	InitMenus ();
	TEInit ();
	InitDialogs (nil);		/* no restart proc */
	InitCursor ();
/*
	Set upper limits of window sizing to machine screen size.  Allow
	for the menu bar.
*/
	GetWMgrPort (&screenPort);
	growRect.right = screenPort->portRect.right;
	growRect.bottom = screenPort->portRect.bottom - mBarHeight;
}


/*
	Main loop.

	Task care of DA's with SystemTask.
	Run background task if there is one.
	If there is an event, check for an event hook.  If there isn't
	one defined, or if there is but it returns false, call the
	general event handler.  (Hook returns true if TransSkel should
	ignore the event.)
	If no event, call the "no-event" handler for the front window and for
	any other windows with idle procedures that are always supposed
	to run.  This is done in such a way that it is safe for idle procs
	to remove the handler for their own window if they want (unlikely,
	but...)  This loop doesn't check whether the window is really
	a dialog window or not, but it doesn't have to, because such
	things always have a nil idle proc.
	
	doneFlag is reset upon exit.  This allows it to be called
	repeatedly, or recursively.

	If dialogs are supported, null events are looked at (in SkelMain)
	and passed to the event handler.  This is necessary to make sure
	DialogSelect gets called repeatedly, or the caret won't blink if
	a dialog has any editText items.

	If an event-inspecting hook is installed, null events are not passed
	to it.
*/

SkelMain ()
{
EventRecord			theEvent;
register WHandler	**wh, **wh2;
register WindowPtr	w;
Boolean				haveEvent;

	while (!doneFlag)
	{	
		SystemTask ();
		if (pBkgnd != nil)
			(*pBkgnd) ();

		haveEvent = GetNextEvent (eventMask, &theEvent);

/*		if (pEvent == nil || (haveEvent && (*pEvent) (&theEvent) == false))
			DoEvent(&theEvent);*/
		if (haveEvent
			&& (pEvent == nil || (*pEvent) (&theEvent) == false))
			DoEvent(&theEvent);

		if (!haveEvent)
		{
			for (wh = whList; wh != nil; wh = wh2)
			{
				wh2 = (**wh).whNext;
				w = (**wh).whWind;
				if ( (w == FrontWindow () || !(**wh).whFrontOnly ) )
				{
					SystemTask ();
					DoIdle (wh);
				}
			}
		}
	}
	doneFlag = false;
}


/*
	Tell SkelMain to stop
*/

SkelWhoa () { doneFlag = true; }


/*
	Clobber all the menu, window and dialog handlers
*/

SkelClobber ()
{
	while (whList != nil)
		SkelRmveWind ((**whList).whWind);

	while (mhList != nil)
		SkelRmveMenu (GetMHandle((**mhList).mhID));
}


/* -------------------------------------------------------------------- */
/*						Menu-handler interface routines					*/
/* -------------------------------------------------------------------- */


/*
	Install handler for a menu.  Remove any previous handler for it.
	Pass the following parameters:

	theMenu	Handle to the menu to be handled.  Must be created by host.
	pSelect	Proc that handles selection of items from menu.  If this is
			nil, the menu is installed, but nothing happens when items
			are selected from it.
	pClobber Proc for disposal of handler's data structures.  Usually
			nil for menus that remain in menu bar until program
			termination.
	
	The menu is installed and drawn in the menu bar.
*/
/* update added, L. TIerney */
SkelMenu (theMenu, pSelect, pClobber)
MenuHandle	theMenu;
ProcPtr		pSelect;
ProcPtr		pClobber;
{
register MHandler	**mh;

	mhClobOnRmve = false;
	SkelRmveMenu (theMenu);
	mhClobOnRmve = true;

	mh = New (MHandler);
	(**mh).mhNext = mhList;
	mhList = mh;
	(**mh).mhID = (**theMenu).menuID;	/* get menu id number */
	(**mh).mhSelect = pSelect;			/* install selection handler */
	(**mh).mhClobber = pClobber;		/* install disposal handler */
	(**mh).mhUpdate = nil;
	InsertMenu (theMenu, 0);			/* put menu at end of menu bar */
	DrawMenuBar ();
}

/* install menu update handler; L. Tierney */
SkelMenuUpdateProc(theMenu, pUpdate)
	MenuHandle theMenu;
	ProcPtr pUpdate;
{
  MHandler **mh;
  
  for (mh = mhList; mh != nil; mh = (**mh).mhNext)
    if ((**mh).mhID == (**theMenu).menuID) {
      (**mh).mhUpdate = pUpdate;
      break;
    }
}
	
/*
	Remove a menu handler.  This calls the handler's disposal routine
	and then takes the handler out of the handler list and disposes
	of it.

	Note that the menu MUST be deleted from the menu bar before calling
	the clobber proc, because the menu bar will end up filled with
	garbage if the menu was allocated with NewMenu (see discussion of
	DisposeMenu in Menu Manager section of Inside Macintosh).
*/

SkelRmveMenu (theMenu)
MenuHandle	theMenu;
{
register int		mID;
register MHandler	**h, **h2;
register ProcPtr	p;

	mID = (**theMenu).menuID;
	if (mhList != nil)				/* if list empty, ignore */
	{
		if ((**mhList).mhID == mID)	/* is it the first element? */
		{
			h2 = mhList;
			mhList = (**mhList).mhNext;
		}
		else
		{
			for (h = mhList; h != nil; h = h2)
			{
				h2 = (**h).mhNext;
				if (h2 == nil)
					return;						/* menu not in list! */
				if ((**h2).mhID == mID)			/* found it */
				{
					(**h).mhNext = (**h2).mhNext;
					break;
				}
			}
		}
		DeleteMenu (mID);
		DrawMenuBar ();
		if (mhClobOnRmve && (p = (**h2).mhClobber) != nil)
			(*p) (theMenu);				/* call disposal routine */
		DisposHandle ((Handle) h2);				/* get rid of handler record */
	}
}


/*
	Install a handler for the Apple menu.
	
	SkelApple is called if TransSkel is supposed to handle the apple
	menu itself.  The title is the title of the first item.  If nil,
	then only desk accessories are put into the menu.  If not nil, then
	the title is entered as the first item, followed by a gray line,
	then the desk accessories.
*/

SkelApple (aboutTitle, aboutProc)
StringPtr	aboutTitle;
ProcPtr		aboutProc;
{
Str255	appleTitle;

	appleTitle[0] = 1;		/* build apple menu title */
	appleTitle[1] = 0x14;	/* "apple" character */
	appleID = 1;
	appleMenu = NewMenu (appleID, appleTitle);
	if (aboutTitle != nil)
	{
		doAbout = true;
		AppendMenu (appleMenu, aboutTitle);	/* add About... item title */
		AppendMenu (appleMenu, "\p(-");		/* add gray line */
		appleAboutProc = aboutProc;
	}
	AddResMenu (appleMenu, 'DRVR');		/* add desk accessories */
	SkelMenu (appleMenu, DoAppleItem, DoAppleClobber);
}


/* -------------------------------------------------------------------- */
/*					Window-handler interface routines					*/
/* -------------------------------------------------------------------- */


/*
	Install handler for a window.  Remove any previous handler for it.
	Pass the following parameters:

	theWind	Pointer to the window to be handled.  Must be created by host.
	pMouse	Proc to handle mouse clicks in window.  The proc will be
			passed the point (in local coordinates), the time of the
			click, and the modifier flags word.
	pKey	Proc to handle key clicks in window.  The proc will be passed
			the character and the modifier flags word.
	pUpdate	Proc for updating window.  TransSkel brackets calls to update
			procs with calls to BeginUpdate and EndUpdate, so the visRgn
			is set up correctly.  A flag is passed indicating whether the
			window was resized or not.  BY CONVENTION, the entire portRect
			is invalidated when the window is resized.  That way, the
			handler's update proc can redraw the entire content region
			without interference from BeginUpdate/EndUpdate.  The flag
			is set to false after the update proc is called; the
			assumption is made that it will notice the resizing and
			respond appropriately.
	pActivate Proc to execute when window is activated or deactivated.
			A boolean is passed to it which is true if the window is
			coming active, false if it's going inactive.
	pClose	Proc to execute when mouse clicked in close box.  Useful
			mainly to temp window handlers that want to know when to
			self-destruct (with SkelRmveWind).
	pClobber Proc for disposal of handler's data structures
	pIdle	Proc to execute when no events are pending.
	frontOnly True if pIdle should execute on no events only when
			theWind is frontmost, false if executes all the time.  Note
			that if it always goes, everything else may be slowed down!

	If a particular procedure is not needed (e.g., key events are
	not processed by a handler), pass nil in place of the appropriate
	procedure address.

	All handler procedures may assume that the port is set correctly
	at the time they are called.
*/
 
SkelWindow (theWind, pMouse, pKey, pUpdate, pActivate, pClose,
				pClobber, pIdle, frontOnly)

WindowPtr	theWind;
ProcPtr		pMouse, pKey, pUpdate, pActivate, pClose, pClobber, pIdle;
Boolean		frontOnly;
{
register WHandler	**hHand, *hPtr;

	whClobOnRmve = false;
	SkelRmveWind (theWind);
	whClobOnRmve = true;
/*
	Get new handler, attach to list of handlers.  It is attached to the
	beginning of the list, which is simpler; the order is presumably
	irrelevant to the host, anyway.
*/
	hHand = New (WHandler);
	(**hHand).whNext = whList;
	whList = hHand;
/*
	Fill in handler fields
*/
	hPtr = *hHand;
	hPtr->whWind = theWind;
	hPtr->whMouse = pMouse;
	hPtr->whKey = pKey;
	hPtr->whUpdate = pUpdate;
	hPtr->whActivate = pActivate;
	hPtr->whClose = pClose;
	hPtr->whClobber = pClobber;
	hPtr->whIdle = pIdle;
	hPtr->whFrontOnly = frontOnly;
	hPtr->whSized = false;
	hPtr->whGrow = growRect;
	SetPort (theWind);
}


/*
	Remove a window handler.  This calls the handler's disposal routine
	and then takes the handler out of the handler list and disposes
	of it.

	SkelRmveWind is also called by SkelRmveDlog.
*/

SkelRmveWind (theWind)
WindowPtr	theWind;
{
register WHandler	**h, **h2;

	if (whList != nil)		/* if list empty, ignore */
	{
		if ((**whList).whWind == theWind)	/* is it the first element? */
		{
			h2 = whList;
			whList = (**whList).whNext;
		}
		else
		{
			for (h = whList; h != nil; h = h2)
			{
				h2 = (**h).whNext;
				if (h2 == nil)
					return;						/* theWind not in list! */
				if ((**h2).whWind == theWind)	/* found it */
				{
					(**h).whNext = (**h2).whNext;
					break;
				}
			}
		}
		if (whClobOnRmve)
			DoClobber (h2);		/* call disposal routine */
		DisposHandle ((Handle) h2);		/* get rid of handler record */
	}
}


# ifdef	supportDialogs

/* -------------------------------------------------------------------- */
/*					Dialog-handler interface routines					*/
/* -------------------------------------------------------------------- */


/*
	Install a dialog handler.  Remove any previous handler for it.
	SkelDialog calls SkelWindow as a subsidiary to install a window
	handler, then sets the event procedure on return.

	Pass the following parameters:

	theDialog	Pointer to the dialog to be handled.  Must be created
			by host.
	pEvent	Event-handling proc for dialog events.
	pClose	Proc to execute when mouse clicked in close box.  Useful
			mainly to dialog handlers that want to know when to
			self-destruct (with SkelRmveDlog).
	pClobber Proc for disposal of handler's data structures

	If a particular procedure is not needed, pass nil in place of
	the appropriate procedure address.

	All handler procedures may assume that the port is set correctly
	at the time they are called.
*/

SkelDialog (theDialog, pEvent, pClose, pClobber)
DialogPtr	theDialog;
ProcPtr		pEvent;
ProcPtr		pClose;
ProcPtr		pClobber;
{
	SkelWindow (theDialog, nil, nil, nil, nil, pClose, pClobber, nil, false);
	(**GetWDHandler (theDialog)).whEvent = pEvent;
}


/*
	Remove a dialog and its handler
*/

SkelRmveDlog (theDialog)
DialogPtr	theDialog;
{
	SkelRmveWind (theDialog);
}

# endif


/* -------------------------------------------------------------------- */
/*					Miscellaneous interface routines					*/
/* -------------------------------------------------------------------- */


/*
	Override the default sizing limits for a window, or, if theWind
	is nil, reset the default limits used by SkelWindow.
*/

SkelGrowBounds (theWind, hLo, vLo, hHi, vHi)
WindowPtr	theWind;
int			hLo, vLo, hHi, vHi;
{
register WHandler	**h;
Rect				r;

	if (theWind == nil)
		SetRect (&growRect, hLo, vLo, hHi, vHi);
	else if ((h = GetWHandler (theWind)) != nil)
	{
		SetRect (&r, hLo, vLo, hHi, vHi);
		(**h).whGrow = r;
	}
}


/*
	Set the event mask.
*/

SkelEventMask (mask)
int		mask;
{
	eventMask = mask;
}


/*
	Return the event mask.
*/

SkelGetEventMask (mask)
int		*mask;
{
	*mask = eventMask;
}


/*
	Install a background task.  If p is nil, the current task is
	disabled.
*/

SkelBackground (p)
ProcPtr	p;
{
	pBkgnd = p;
}


/*
	Return the current background task.  Return nil if none.
*/

SkelGetBackground (p)
ProcPtr	*p;
{
	*p = pBkgnd;
}


/*
	Install an event-inspecting hook.  If p is nil, the hook is
	disabled.
*/

SkelEventHook (p)
Boolean	(*p)();
{
	pEvent = p;
}


/*
	Return the current event-inspecting hook.  Return nil if none.
*/

SkelGetEventHook (p)
Boolean	(**p)();
{
	*p = pEvent;
}


# ifdef	supportDialogs

/*
	Set the mask for event types that will be passed to dialogs.
	Bit 1 is always set, so that null events will be passed.
*/

SkelDlogMask (mask)
int		mask;
{
	dlogEventMask = mask | 1;
}


/*
	Return the current dialog event mask.
*/

SkelGetDlogMask (mask)
int		*mask;
{
	*mask = dlogEventMask;
}

# endif
