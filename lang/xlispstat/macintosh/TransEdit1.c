/*
	TransEdit.c version 1.0 - TransSkel plug-in module supporting an
	arbitrary number of generic edit windows.  Each window may be
	bound to a file.

	*** Requires FakeAlert.c for proper linking! ***

	Shortcomings:
		Doesn't check for the obvious out of memory conditions.
	
	TransSkel and TransEdit are public domain, and are written by:

			Paul DuBois
			Wisconsin Regional Primate Research Center
			1220 Capital Court
			Madison WI  53706  USA

	UUCP:	{allegra,ihnp4,seismo}!uwvax!uwmacc!dubois
	ARPA:	dubois@unix.macc.wisc.edu
			dubois@rhesus.primate.wisc.edu

	This version of TransEdit written for LightspeedC.  LightspeedC
	is a trademark of:
			THINK Technologies, Inc
			420 Bedford Street  Suite 350
			Lexington, MA  02173  USA

  History
  08/25/86	Genesis.  Beta version.
  09/15/86	Changed to allow arbitrary number of windows.
  11/04/86	Added conditional stuff to allow compilation in
 			single- or multiple-window mode.  Changed version to 1.0.
*/


/*
	The following symbol controls the compile mode.  If it is #define'd,
	TransEdit allows only a single edit window, and generates less code.
	If it is #undef'ed, TransEdit allows an arbitrary number of edit
	windows, but generates more code.
*/

# undef	singleEdit


#ifdef MPWC
# include	<Memory.h>
# include	<Controls.h>	/* includes WindowMgr.h, QuickDraw.h, MacTypes.h */
# include	<Packages.h>
# include	<Files.h>
# include	<ToolUtils.h>
# include	<Scrap.h>
# define arrow qd.arrow
#else
# include	<ControlMgr.h>	/* includes WindowMgr.h, QuickDraw.h, MacTypes.h */
# include	<StdFilePkg.h>
# include	<FileMgr.h>
# include	<ToolBoxUtil.h>
#endif MPWC
# include	"TransEdit.h"

/*
	Edit window types, constants, variables.
*/


# define	enter			3
# define	cr				13
# define	monaco			4
# ifndef	shiftKey
# define	shiftKey		0x0200
# endif		shiftKey

typedef enum			/* Edit menu item numbers */
{
	undo = 1,
	/* --- */
	cut = 3,
	copy,
	paste,
	clear		/* (it's ok if the host doesn't have this item) */
};


/*
	Default values for edit window text display characteristics
	and event notification procedures
*/

static int		e_font = monaco;	/* default font                 */
static int		e_size = 9;			/* default pointsize            */
static int		e_wrap = 0;			/* default word wrap (on)       */
static int		e_just = teJustLeft;/* default justification        */
static ProcPtr	e_key = nil;		/* default key procedure        */
static ProcPtr	e_activate = nil;	/* default activation procedure */
static ProcPtr	e_close = nil;		/* default close procedure      */
static ProcPtr	e_key_filter = nil;	/* default key filter procedure - L. Tierney */
static ProcPtr	e_idle = nil;		/* default idle procedure - L. Tierney */


# ifndef	singleEdit

/*
	New(TypeName) returns handle to new object, for any TypeName.
	If there is insufficient memory, the result is nil.
*/

# define	New(x)	(x **) NewHandle ((Size) sizeof (x))


/*
	ewList points to a list of structures describing the known edit
	windows.
*/


typedef struct EditInfo
{
	WindowPtr		editWind;	/* the edit window                   */
	Boolean			bound;		/* whether window is bound to file   */
	SFReply			editFile;	/* file it's bound to, if bound true */
	TEHandle		editTE;		/* window text                       */
	Boolean			dirty;		/* whether text modified since save  */
	ControlHandle	scroll;		/* scroll bar                        */
	int				visLines;	/* # lines visible in window, max    */
	ProcPtr			eKey;		/* key click notifier                */
	ProcPtr			eActivate;	/* activate event notifier           */
	ProcPtr			eClose;		/* close notifier                    */
	ProcPtr         eKeyFilter; /* key filter - L. Tierney           */
	ProcPtr         eIdle; 		/* idle procedure - L. Tierney       */
	struct EditInfo	**eNext;	/* next information structure        */
} EditInfo, *EIPtr, **EIHandle;


static EIHandle		ewList = nil;

# endif


/*
	Global variables - most of these are always synced to
	the current window.  Note that not all these are set by
	SyncGlobals, since some are not often needed.  When they
	are all needed, use SyncAllGlobals.
*/

# ifndef	singleEdit
static EIHandle			editInfo;		/* window's info structure      */
# endif

static WindowPtr		editWind = nil;	/* the window                   */
static TEHandle			editTE;			/* window text                  */
static ControlHandle	editScroll;		/* the scroll bar               */
static SFReply			editFile;		/* file information             */
static int				visLines;		/* number of lines in window    */
static Boolean			bound;			/* true if window bound to file */
static Boolean			dirty;			/* whether window is dirty      */
static ProcPtr			eKey;			/* key click notifier           */
static ProcPtr			eActivate;		/* activate event notifier      */
static ProcPtr			eClose;			/* close notifier               */
static ProcPtr			eKeyFilter;		/* key filter - L. Tierney      */
static ProcPtr			eIdle;			/* idle procedure - L. Tierney  */


static int		windID = 0;
static Point	dlogWhere = { 70, 100 };	/* GetFile/PutFile location */
static OSType	creator = 'TEDT';			/* default file creator */

static RgnHandle	clipRgn;


/* -------------------------------------------------------------------- */
/*				Miscellaneous Internal (private) Routines				*/
/* -------------------------------------------------------------------- */


/*
	Save and restore the current window's clip region
*/

static SaveClipRgn ()
{
	clipRgn = NewRgn ();
	GetClip (clipRgn);
}


static RestoreClipRgn ()
{
	SetClip (clipRgn);
	DisposeRgn (clipRgn);
}


/*
	Draw grow box in lower right hand corner of window.
*/


static DrawGrowBox ()
{
Rect		r;

	SaveClipRgn ();
	r = editWind->portRect;
	r.left = r.right - 15;		/* draw only in corner */
	r.top = r.bottom - 15;
	ClipRect (&r);
	DrawGrowIcon (editWind);
	RestoreClipRgn ();
}


/* -------------------------------------------------------------------- */
/*			Lowest-level Internal (Private) Edit Window Routines		*/
/* -------------------------------------------------------------------- */


# ifndef	singleEdit

/*
	Get edit window info associated with window.
	Return nil if window isn't a known edit window.
*/

static EIHandle GetEInfo (theWind)
WindowPtr	theWind;
{
register EIHandle	h;

	for (h = ewList; h != nil; h = (**h).eNext)
	{
		if ((**h).editWind == theWind)
			return (h);
	}
	return (nil);
}

# endif


# ifdef	singleEdit
# define	SyncAllGlobals	SyncGlobals
# endif


/*
	Synchronize globals to an edit window and make it the
	current port.  theWind must be a legal edit window, with one
	exception:  if theWind is nil, the variables are synced to the
	port that's already current.  That is safe (and correct) because:
	(i)	 nil is only passed by edit window handler procedures,
		 which are only attached to edit windows
	(ii) TransSkel always sets the port to the window before
		 calling the handler proc.
	Hence, using the current port under these circumstances always
	produces a legal edit window.
*/

static SyncGlobals (theWind)
WindowPtr	theWind;
{

	if (theWind == nil)					/* use current window */
		GetPort (&theWind);

	SetPort (theWind);

# ifndef	singleEdit

	editWind = theWind;
	editInfo = GetEInfo (editWind);
	editTE = (**editInfo).editTE;
	editScroll = (**editInfo).scroll;
	visLines = (**editInfo).visLines;

# endif

}


# ifndef	singleEdit

static SyncAllGlobals (theWind)
WindowPtr	theWind;
{

	SyncGlobals (theWind);				/* sync display globals */
	editFile = (**editInfo).editFile;	/* sync file, state, and */
	bound = (**editInfo).bound;			/* procedure globals */
	dirty = (**editInfo).dirty;
	eKey = (**editInfo).eKey;
	eActivate = (**editInfo).eActivate;
	eClose = (**editInfo).eClose;
	eKeyFilter = (**editInfo).eKeyFilter; /* L. Tierney */
	eIdle = (**editInfo).eIdle; /* L. Tierney */
}

# endif


/*
	Set dirty flag for current window
*/

static SetDirty (boolVal)
Boolean	boolVal;
{

# ifdef	singleEdit
	dirty = boolVal;
# else
	(**editInfo).dirty = boolVal;
# endif
}


/* -------------------------------------------------------------------- */
/*					Internal (private) Display Routines					*/
/* -------------------------------------------------------------------- */


/*
	Calculate the dimensions of the editing rectangle for
	editWind (which must be set properly and is assumed to be
	the current port).  (The viewRect and destRect are the
	same size.)  Assumes the port, text font and text size are all
	set properly.  The viewRect is sized so that an integral
	number of lines can be displayed in it, i.e., so that a
	partial line never shows at the bottom.  If that's not
	done, funny things can happen to the caret.
*/

static GetEditRect (r)
Rect	*r;
{
FontInfo		f;
register int	lineHeight;

	GetFontInfo (&f);
	lineHeight = f.ascent + f.descent + f.leading;
	*r = editWind->portRect;
	r->left += 4;
	r->right -= 17;		/* leave room for scroll bar */
	r->top += 2;
	r->bottom = r->top + ((r->bottom - r->top - 2) / lineHeight) * lineHeight;
}


/*
	Set the edit rect properly.
*/

static SetEditRect ()
{
Rect	r;

	GetEditRect (&r);
	(**editTE).destRect.right = r.right;
	(**editTE).viewRect = r;
}



/*
	Calculate the dimensions of the scroll bar rectangle for
	editWind (which must be set properly).  Make sure that
	the edges overlap the window frame and the grow box.
*/

static CalcScrollRect (r)
Rect		*r;
{
	*r = editWind->portRect;
	++r->right;
	--r->top;
	r->left = r->right - 16;
	r->bottom -= 14;
}


/*
	Return true if the mouse is in the non-scrollbar part of the
	edit window.
*/

static Boolean PtInText (pt)
Point	pt;
{
Rect	r;

	r = editWind->portRect;
	r.right -= 15;
	return (PtInRect (pt, &r));
}


/*
	Set the cursor appropriately.  If theCursor == iBeamCursor, check
	that it's really in the text area of an edit window (and if not
	set the cursor to an arrow instead).  Otherwise, set the cursor
	to the given type (usually a watch).

	If the cursor is supposed to be set to an i-beam, it is assumed
	that the globals are synced, because DoCursor changes them and
	syncs them back.

	Pass -1 for theCursor to set the cursor to the arrow.
*/

static DoCursor (theCursor)
int		theCursor;
{
Point	pt;
GrafPtr	savePort;

	if (theCursor == iBeamCursor)			/* check whether there's an edit */
	{										/* window in front and if so,    */
		theCursor = -1;						/* whether the cursor's in its   */
		if (IsEWindow (FrontWindow ()))		/* text area                     */
		{
			GetPort (&savePort);
			SyncGlobals (FrontWindow ());
			GetMouse (&pt);
			if (PtInText (pt))
				theCursor = iBeamCursor;
			SyncGlobals (savePort);
		}
	}
	SetCursor (theCursor == -1 ? &arrow : *GetCursor (theCursor));
}


/*
	Calculate the number of lines currently scrolled off
	the top.
*/

static LinesOffTop ()
{
register TEPtr	ePtr;

	ePtr = *editTE;
	return (((*ePtr).viewRect.top - (*ePtr).destRect.top)
				/ (*ePtr).lineHeight);
}


/*
	Return the line number that the caret (or the beginning of
	the currently selected text) is in.  Value returned is in
	the range 0..(**editTE).nLines.  If = (**editTE).nLines, the
	caret is past the last line.  The only special case to watch out
	for is when the caret is at the very end of the text.  If the
	last character is not a carriage return, then the caret is on
	the (nLines-1)th line, not the (nLines)th line.

	(This really should do a binary search for speed.)
*/

static LineWithCaret ()
{
register int	i;
register int	nLines;
register int	teLength;
register int	selStart;
register int	lineStart;

	selStart = (**editTE).selStart;
	nLines = (**editTE).nLines;
	teLength = (**editTE).teLength;

	if (selStart == teLength)
	{
		if (teLength != 0 && (*((**editTE).hText))[teLength-1] != cr)
			return (nLines - 1);
		return (nLines);
	}

	for (i = 0; /* empty */; ++i)
	{
		if ((lineStart = (**editTE).lineStarts[i]) >= selStart)
		{
			if (lineStart != selStart)
				--i;
			return (i);
		}
	}
}


/*
	Return the number of the last displayable line.  That's one
	more than nLines if the text is empty or the last character
	is a carriage return.
*/

static LastLine ()
{
register int	nLines;
register int	teLength;

	nLines = (**editTE).nLines;
	teLength = (**editTE).teLength;

	if (teLength == 0 || (*((**editTE).hText))[teLength-1] == cr)
		nLines++;
	return (nLines);
}


/*
	Set the maximum value of the scroll bar.  It's set so that if
	there's more text than fits in the window, the bottom line can
	be scrolled up at least a little below the bottom of the window.

	The shenanigans with topLines and scrollableLines have to do with
	the case where there may be less text than fills the window, but
	most of it's scrolled off the top.  This can happen when you
	scroll a bunch of stuff up, then delete everything visible in
	the window.
*/

static SetScrollMax ()
{
register int	topLines;
register int	scrollableLines;
register int	max;

	topLines = LinesOffTop ();
	scrollableLines = LastLine () - visLines;
	max = (topLines > scrollableLines ? topLines : scrollableLines);

	if (max < 0)
		max = 0;

	if (max != GetCtlMax (editScroll))
	{
		SetCtlMax (editScroll, max);
		HiliteControl (editScroll, max > 0 ? 0 : 255);
	}
}


/*
	Set scroll bar current value (but only if it's different than
	the current value, to avoid needless flashing).
*/

static SetScrollValue (value)
int		value;
{
	if (GetCtlValue (editScroll) != value)
		SetCtlValue (editScroll, value);
}


/*
    Scroll to the correct position.  lDelta is the
    amount to CHANGE the current scroll setting by.
*/

static ScrollText (lDelta)
int		lDelta;
{
register int	topVisLine;
register int	newTopVisLine;

	topVisLine = LinesOffTop ();
    newTopVisLine = topVisLine + lDelta;
    if (newTopVisLine < 0)					/* clip to range */
    	newTopVisLine = 0;
    if (newTopVisLine > GetCtlMax (editScroll))
		newTopVisLine = GetCtlMax (editScroll);
    SetScrollValue (newTopVisLine);
    TEScroll (0, (topVisLine-newTopVisLine )*(**editTE).lineHeight, editTE);
}


/*
	Scroll to home position without redrawing.
*/

static ScrollToHome ()
{
Rect				r;

	r = (**editTE).destRect;
	OffsetRect (&r, 0, 2 - r.top);
	(**editTE).destRect = r;
}

/*
	ClikLoop proc for autoscrolling text when the mouse is dragged out
	of the text view rectangle.

	The clipping region has to be set to include the scroll bar,
	because whenever this proc is called, TE has the region set down
	to the view rectangle - if it's not reset, changes to the scroll
	bar will not show up!
*/

static pascal Boolean AutoScroll ()
{
Point	p;
Rect	r;

	SaveClipRgn ();
	ClipRect (&editWind->portRect);
	GetMouse (&p);
	r = (**editTE).viewRect;
	if (p.v < r.top)
		ScrollText (-1);
	else if (p.v > r.bottom)
		ScrollText (1);
	RestoreClipRgn ();
	return (true);			/* true = 'keep tracking mouse' */
}


/*
	Filter proc for tracking mousedown in scroll bar.  The code for
	the part originally hit is shoved into the control's reference
	value by Mouse() before this is called.

	I suspect odd scrolling may occur for hits in paging regions if
	the window is allowed to size such that less than two lines show.
*/

static pascal void TrackScroll (theScroll, partCode)
ControlHandle	theScroll;
short				partCode;
{
register int	lDelta;

    if (partCode == GetCRefCon (theScroll))	/* still in same part? */
    {
        switch (partCode)
        {
            case inUpButton: lDelta = -1; break;
            case inDownButton: lDelta = 1; break;
            case inPageUp: lDelta = -(visLines - 1); break;
            case inPageDown: lDelta = visLines - 1; break;
        }
        ScrollText (lDelta);
    }
}


/*
	Set the scroll bar properly and adjust the text in the
	window so that the line containing the caret is visible.
	If the line with the caret if more than a line outside of
	the viewRect, try to place it in the middle of the window.

	Yes, it is necessary to SetScrollMax at the end.
*/

static AdjustDisplay ()
{
register int	caretLine;
register int	topVisLine;
register int	d;

	SetScrollMax ();
	caretLine = LineWithCaret ();
	topVisLine = LinesOffTop ();
	if ((d = caretLine - topVisLine) < 0)
		ScrollText (d == -1 ? -1 : d - visLines / 2);
	else if (( d = caretLine - (topVisLine + visLines - 1)) > 0)
		ScrollText (d == 1 ? 1 : d + visLines / 2);
	else
		SetScrollValue (topVisLine);
	SetScrollMax ();	/* might have changed from scrolling */
}


/*
	Overhaul the entire display.  This is called for major
	catastrophes, such as resizing the window, or changes to
	the word wrap style.  It makes sure the view and
	destination rectangles are sized properly, and that the bottom
	line of text never scrolls up past the bottom line of the
	window, if there's enough to fill the window, and that the
	scroll bar max and current values are set properly.

	Resizing the dest rect just means resetting the right edge
	(the top is NOT reset), since text might be scrolled off the
	top (i.e., destRect.top != 0).

	Doesn't redraw the control, though!
*/

static OverhaulDisplay (showCaret, recalc)
Boolean	showCaret;
Boolean	recalc;
{
Rect			r;

	r = (**editTE).viewRect;	/* erase current viewRect */
	EraseRect (&r);
	SetEditRect ();				/* recalculate editing rects */
	if (recalc)
		TECalText (editTE);			/* recalculate line starts */
	visLines = ((**editTE).viewRect.bottom - (**editTE).viewRect.top)
					/ (**editTE).lineHeight;

# ifndef	singleEdit
	(**editInfo).visLines = visLines;
# endif

/*
	If there is text, but none of it is visible in the window
	(it's all scrolled off the top), pull some down.
*/

	if (showCaret)
		AdjustDisplay ();
	else
		SetScrollMax ();
	r = (**editTE).viewRect;
	TEUpdate (&r, editTE);
}


/* ---------------------------------------------------------------- */
/*						Window Handler Routines						*/
/* ---------------------------------------------------------------- */


/*
	Handle mouse clicks in window.  The viewRect is never tested
	directly, because if it were, clicks along the top, left and
	bottom edges of the window wouldn't register.
*/

static Mouse (thePt, t, mods)
Point	thePt;
long	t;
int		mods;
{
register int	thePart;
register int	oldCtlValue;

	SyncGlobals (nil);		/* sync to current port */

	if ((thePart = TestControl (editScroll, thePt)) == inThumb)
	{
		oldCtlValue = GetCtlValue (editScroll);
		if (TrackControl (editScroll, thePt, nil) == inThumb)
			ScrollText (GetCtlValue (editScroll) - oldCtlValue);
	}
	else if (thePart != 0)
	{
		SetCRefCon (editScroll, (long) thePart);
		(void) TrackControl (editScroll, thePt, (ProcPtr) TrackScroll);
	}
	else if (PtInText (thePt))
	{
		TEClick (thePt, (mods & shiftKey) != 0, editTE);
	}

	SetScrollMax ();
}


/*
    Handle key clicks in window
*/

static Key (c, mods)
char	c;
int		mods;
{
	SyncAllGlobals (nil);		/* sync to current port */

	if (eKeyFilter != nil)
	  c = (*eKeyFilter) (c);  /* Filter the character - L. Tierney */
	if (c != enter && c != '\0') /* '\0' used to signal a character to be ignored */
		TEKey (c, editTE);
	AdjustDisplay ();
	SetDirty (true);
	if (eKey != nil)	/* report event to the host */
		(*eKey) ();
}


/*
	When the window comes active, highlight the scroll bar appropriately.
	When the window is deactivated, un-highlight the scroll bar.
	Redraw the grow box in any case.  Set the cursor (DoCursor avoids
	changing it from an ibeam to an arrow back to an ibeam, in the case
	where one edit window is going inactive and another is coming
	active).

	Report the event to the host.
*/

static Activate (active)
Boolean	active;
{
	SyncAllGlobals (nil);		/* sync to current port */

	DrawGrowBox ();
	if (active)
	{
		TEActivate (editTE);
		HiliteControl (editScroll, GetCtlMax (editScroll) > 0 ? 0 : 255);
	}
	else
	{
		TEDeactivate (editTE);
		HiliteControl (editScroll, 255);
	}
	DoCursor (iBeamCursor);
	if (eActivate != nil)	/* report event to the host */
		(*eActivate) (active);
}


/*
	Close box was clicked.  If user specified notify proc, call it.
	Otherwise do default close operation (ask about saving if dirty,
	etc.).
*/

static Close ()
{
	SyncAllGlobals (nil);		/* sync to current port */

	if (eClose != nil)
		(*eClose) ();
	else
		(void) EWindowClose (editWind);
}


/*
	Update window.  The update event might be in response to a
	window resizing.  If so, move and resize the scroll bar.
	The ValidRect call is done because the HideControl adds the
	control bounds box to the update region - which would generate
	another update event!  Since everything gets redrawn below,
	the ValidRect is used to cancel the update.
*/

static Update (resized)
Boolean	resized;
{
Rect			r;

	SyncGlobals (nil);		/* sync to current port */

	r = editWind->portRect;
	EraseRect (&r);
	if (resized)
	{
		HideControl (editScroll);
		r = (**editScroll).contrlRect;
		ValidRect (&r);
		CalcScrollRect (&r);
		SizeControl (editScroll, 16, r.bottom - r.top);
		MoveControl (editScroll, r.left, r.top);
		OverhaulDisplay (false, (**editTE).crOnly >= 0);
		ShowControl (editScroll);
	}
	else
	{
		OverhaulDisplay (false, false);
		DrawControls (editWind);	/* redraw scroll bar */
	}

	DrawGrowBox ();
}


/*
	Remove the edit window from the list, and dispose of it.
	This is called by SkelRmveWind, not directly by user program.

	At this point it's too late to back out if any changes have been
	made to the text.

	Since the clobber procedure is never called except for real edit
	windows, and since the list must therefore be non-empty, it is
	not necessary to check the legality of the window or that the
	window's in the list.
*/

static Clobber ()
{
# ifndef	singleEdit
register EIHandle	h, h2;
# endif

	SyncGlobals (nil);					/* sync to current port */

# ifndef	singleEdit

	if ((**ewList).editWind == editWind)	/* is it the first window in list? */
	{
		h2 = ewList;
		ewList = (**ewList).eNext;
	}
	else
	{
		for (h = ewList; h != nil; h = h2)
		{
			h2 = (**h).eNext;
			if ((**h2).editWind == editWind)	/* found it */
			{
				(**h).eNext = (**h2).eNext;
				break;
			}
		}
	}
	DisposHandle ((Handle) h2);			/* get rid of information structure */

# endif
	TEDispose (editTE);			/* toss text record */
	DisposeWindow (editWind);	/* disposes of scroll bar, too */
	editWind = nil;
/*	DoCursor (iBeamCursor);*//* Taken out because of SetPort problems after window kill - L. Tierney */
}


/*
	Blink the caret and make sure the cursor's an i-beam when it's
	in the non-scrollbar part of the window.
*/

static Idle ()
{
	SyncGlobals (nil);
	if (eIdle != nil) (*eIdle)(); /* L. Tierney */
	TEIdle (editTE);			/* blink that caret! */
	DoCursor (iBeamCursor);
}


/* ---------------------------------------------------------------- */
/*						Internal File Routines						*/
/* ---------------------------------------------------------------- */


static ErrMesg (s)
StringPtr	s;
{
	(void) FakeAlert (s, "\p", "\p", "\p", 1, 1, "\pOK", "\p", "\p");
}


/*
	Save the contents of the edit window.  If there is no file bound
	to the window, ask for a file name.  If askForFile is true, ask
	for a name even if the window is currently bound to a file.  If
	bindToFile is true, bind the window to the file written to (if
	that's different than the currently bound file), and clear the
	window's dirty flag.

	Return true if the file was written without error.  Return false
	if (a) user was asked for name and clicked Cancel (b) there was
	some error writing the file.  In the latter case, the window is
	not bound to any new name given by user.

	Always returns false if the window isn't an edit window.  This
	simplifies EWindowSave, EWindowSaveAs, EWindowSaveCopy.  (They
	don't do the test.)
*/

static Boolean SaveFile (theWind, askForFile, bindToFile)
WindowPtr	theWind;
Boolean		askForFile;
Boolean		bindToFile;
{
short		f;
FInfo	fndrInfo;	/* finder info */
SFReply	tmpFile;
Handle	hText;
long	count;
OSErr	result;
Boolean	haveNewFile = false;

	if (!IsEWindow (theWind))
		return (false);

	SyncAllGlobals (theWind);
	if (bound == false || askForFile)
	{
		SFPutFile (dlogWhere, "\pSave file as:", editFile.fName,
						nil, &tmpFile);
		if (!tmpFile.good)
			return (false);
		else
		{
			haveNewFile = true;
			if (GetFInfo (tmpFile.fName, tmpFile.vRefNum, &fndrInfo)
					== noErr) /* exists */
			{
				if (fndrInfo.fdType != 'TEXT')
				{
					ErrMesg ("\pNot a TEXT File");
					return (false);
				}
			}
			else	/* doesn't exist.  create it. */
			{
				if (Create (tmpFile.fName, tmpFile.vRefNum,
							creator, 'TEXT') != noErr)
				{
					ErrMesg ("\pCan't Create");
					return (false);
				} 
			}
		}
	}
	
/*************************************************************************/
/* 			Added to make saving to existing file work - LjT			 */
/*************************************************************************/
	else {
# ifdef	singleEdit
		tmpFile = editFile;
# else
		tmpFile = (**editInfo).editFile;
# endif
	}
/*************************************************************************/
/* 			Added to make saving to existing file work - LjT			 */
/*************************************************************************/
	
	if ( FSOpen (tmpFile.fName, tmpFile.vRefNum, &f) != noErr)
		ErrMesg ("\pCan't Open");
	else
	{
		DoCursor (watchCursor);
		(void) SetFPos (f, fsFromStart, 0L);
		hText = (**editTE).hText;
		HLock (hText);
		count = (**editTE).teLength;
		result = FSWrite (f, &count, *hText);
		(void) GetFPos (f, &count);
		(void) SetEOF (f, count);
		(void) FSClose (f);
		(void) FlushVol (nil, tmpFile.vRefNum);
		HUnlock (hText);
		DoCursor (iBeamCursor);
		if (result == noErr)
		{
			if (bindToFile)
			{
				SetDirty (false);
				if (haveNewFile)	/* name different than current */
				{
					SetWTitle (editWind, tmpFile.fName);

# ifdef	singleEdit
					bound = true;
					editFile = tmpFile;
# else
					(**editInfo).bound = true;
					(**editInfo).editFile = tmpFile;
# endif

				}
			}
			return (true);
		}
		ErrMesg ("\pWrite error!");
	}
	return (false);
}


/*
	Revert to version of file saved on disk.  Doesn't check whether
	the window's really bound to a file or not, doesn't ask whether
	to really revert if the window's dirty, does no redrawing, etc.
	Just reports whether the file was read in successfully.
*/

static Boolean Revert ()
{
Boolean	result = false;
short		f;
long	len;
Handle	h;

	DoCursor (watchCursor);
	if (FSOpen (editFile.fName, editFile.vRefNum, &f) != noErr)
		ErrMesg ("\pCouldn't open file");
	else
	{
		(void) GetEOF (f, &len);
		if (len >= 32000)
			ErrMesg ("\pFile is too big");
		else
		{
			h = (Handle) TEGetText (editTE);
			SetHandleSize (h, len);
			HLock (h);
			(void) FSRead (f, &len, *h);
			HUnlock (h);
			(**editTE).teLength = len;
			TESetSelect (0L, 0L, editTE);	/* set caret at start */
			result = true;
			SetDirty (false);
		}
		(void) FSClose (f);
	}
	DoCursor (iBeamCursor);
	return (result);
}


/* ------------------------------------------------------------ */
/*			Lowest-level Interface (Public) Routines			*/
/* ------------------------------------------------------------ */


/*
	Return true/false to indicate whether the window is really an
	edit window.
*/

Boolean IsEWindow (theWind)
WindowPtr	theWind;
{
# ifdef	singleEdit
	return (theWind == editWind && editWind != nil);
# else
	return (GetEInfo (theWind) != nil);
# endif
}


/*
	Return true/false to indicate whether the text associated with
	the window has been changed since the last save/revert (or since
	created, if not bound to file).
*/

Boolean IsEWindowDirty (theWind)
WindowPtr	theWind;
{
# ifndef	singleEdit
register EIHandle	eInfo;
	if ((eInfo = GetEInfo (theWind)) != nil)
		return ((**eInfo).dirty);
# else
	if (IsEWindow (theWind))
		return (dirty);
# endif
	return (false);
}

/* change setting of dirty - L. Tierney */
SetEWindowDirty (theWind, ndirty)
WindowPtr	theWind;
int ndirty;
{
# ifndef	singleEdit
register EIHandle	eInfo;
	if ((eInfo = GetEInfo (theWind)) != nil)
		(**eInfo).dirty = ndirty;
# else
	if (IsEWindow (theWind))
		dirty = ndirty;
# endif
	return (false);
}

/*
	Return a handle to the TextEdit record associated with the edit
	window, or nil if it's not an edit window
*/

TEHandle GetEWindowTE (theWind)
WindowPtr	theWind;
{
# ifndef	singleEdit
register EIHandle	eInfo;
	if ((eInfo = GetEInfo (theWind)) != nil)
		return ((**eInfo).editTE);
# else
	if (IsEWindow (theWind))
		return (editTE);
# endif
	return (nil);
}


/*
	Return true/false depending on whether the editor is bound to
	a file or not, and a copy of the file info in the second
	argument.  Pass nil for fileInfo if only want the return status.
	Returns false if it's not an edit window.
*/

Boolean GetEWindowFile (theWind, fileInfo)
WindowPtr	theWind;
SFReply		*fileInfo;
{
# ifndef	singleEdit
register EIHandle	eInfo;
	if ((eInfo = GetEInfo (theWind)) != nil)
	{
		if (fileInfo != nil)
			*fileInfo = (**eInfo).editFile;
		return ((**eInfo).bound);
	}
# else
	if (IsEWindow (theWind))
	{
		if (fileInfo != nil)
			*fileInfo = editFile;
		return (bound);
	}
# endif
	return (false);
}


/* ---------------------------------------------------------------- */
/*					Interface Display Routines						*/
/* ---------------------------------------------------------------- */


/*
	Install event notification procedures for an edit window.
*/

SetEWindowProcs (theWind, pKey, pActivate, pClose)
WindowPtr	theWind;
ProcPtr		pKey;
ProcPtr		pActivate;
ProcPtr		pClose;
{
# ifndef	singleEdit
register EIHandle	eInfo;
# endif

	if (theWind == nil)			/* reset window creation defaults */
	{
		e_key = pKey;
		e_activate = pActivate;
		e_close = pClose;
		return;
	}

# ifndef	singleEdit

	if ((eInfo = GetEInfo (theWind)) != nil)
	{
		(**eInfo).eKey = pKey;
		(**eInfo).eActivate = pActivate;
		(**eInfo).eClose = pClose;
	}

# else

	if (IsEWindow (theWind))
	{
		eKey = pKey;
		eActivate = pActivate;
		eClose = pClose;
	}

# endif

}

/* add filter for key clicks - L. Tieryen */
SetEWindowKeyFilter (theWind, pKeyFilter)
WindowPtr	theWind;
ProcPtr		pKeyFilter;
{
# ifndef	singleEdit
register EIHandle	eInfo;
# endif

# ifndef	singleEdit

	if ((eInfo = GetEInfo (theWind)) != nil)
		(**eInfo).eKeyFilter = pKeyFilter;
# else

	if (IsEWindow (theWind))
		eKeyFilter = pKeyFilter;
# endif
}

/* add idle procedure - L. Tieryen */
SetEWindowIdle (theWind, pIdle)
WindowPtr	theWind;
ProcPtr		pIdle;
{
# ifndef	singleEdit
register EIHandle	eInfo;
# endif

# ifndef	singleEdit

	if ((eInfo = GetEInfo (theWind)) != nil)
		(**eInfo).eIdle = pIdle;
# else

	if (IsEWindow (theWind))
		eIdle = pIdle;
# endif
}

/*
	Change the text display characteristics of an edit window
	and redisplay it.

	Scroll to home position before overhauling, because although
	the overhaul sets the viewRect to display an integral number
	of lines, there's no guarantee that the destRect offset will
	also be integral except at home position.  Clipping is set to
	an empty rect so the scroll doesn't show.
*/

SetEWindowStyle (theWind, font, size, wrap, just)
WindowPtr	theWind;
int			font;
int			size;
int			wrap;
int			just;
{
GrafPtr				savePort;
FontInfo			f;
register TEHandle	te;
int					oldWrap;

	if (theWind == nil)			/* reset window creation defaults */
	{
		e_font = font;
		e_size = size;
		e_wrap = wrap;
		e_just = just;
		return;
	}

	if (IsEWindow (theWind))
	{
		GetPort (&savePort);
		SyncGlobals (theWind);	/* sync and set port */
		te = editTE;
		ScrollToHome ();

		oldWrap = (**te).crOnly;
		(**te).crOnly = wrap;	/* set word wrap */
		TESetJust (just, te);	/* set justification */

		TextFont (font);	 	/* set the font and point size */
		TextSize (size);		/* of text record */
		GetFontInfo (&f);
		(**te).lineHeight = f.ascent + f.descent + f.leading;
		(**te).fontAscent = f.ascent;
		(**te).txFont = font;
		(**te).txSize = size;

		OverhaulDisplay (true, (oldWrap >= 0 || wrap >= 0));

		SetPort (savePort);
	}
}


/*
	Redo display.  Does not save current port.  This is used by hosts
	that mess with the text externally to TransEdit.  The arguments
	determine whether the text is scrolled to show the line with the
	caret, whether the lineStarts are recalculated, and whether the
	text should be marked dirty or not.
*/

EWindowOverhaul (theWind, showCaret, recalc, dirty)
WindowPtr	theWind;
Boolean		showCaret;
Boolean		recalc;
Boolean		dirty;
{
	if (IsEWindow (theWind))
	{
		SyncGlobals (theWind);
		OverhaulDisplay (showCaret, recalc);
		DrawControls (editWind);
		SetDirty (dirty);
	}
}

/* show caret - L. Tierney */
EWindowAdjustDisplay(theWind)
WindowPtr	theWind;
{
	if (IsEWindow (theWind))
	{
		SyncGlobals (theWind);
		AdjustDisplay();
	}
}

/* ---------------------------------------------------------------- */
/*						Menu Interface Routine						*/
/* ---------------------------------------------------------------- */


/*
	Do Edit menu selection.  This is only valid if an edit
	window is frontmost.
*/

EWindowEditOp (item)
int		item;
{

	if (!IsEWindow (FrontWindow ()))
		return;				/* host messed up */

	SyncGlobals (FrontWindow ());

	switch (item)
	{

/*
	cut selection, put in TE Scrap, clear clipboard and put
	TE scrap in it
*/
		case cut:
		{
			TECut (editTE);
			(void) ZeroScrap ();
			(void) TEToScrap ();
			break;
		}
/*
	copy selection to TE Scrap, clear clipboard and put
	TE scrap in it
*/
		case copy:
		{
			TECopy (editTE);
			(void) ZeroScrap ();
			(void) TEToScrap ();
			break;
		}
/*
	get clipboard into TE scrap, put TE scrap into edit record
*/
		case paste:
		{
			(void) TEFromScrap ();
			TEPaste (editTE);
			break;
		}
/*
	delete selection without putting into TE scrap or clipboard
*/
		case clear:
		{
			TEDelete (editTE);
			break;
		}

	}
	AdjustDisplay ();
	if (item != copy)    /* L. Tierney */
	  SetDirty (true);
}


/* ---------------------------------------------------------------- */
/*						Interface File Routines						*/
/* ---------------------------------------------------------------- */


/*
	Set file creator for any files created by TransEdit
*/

SetEWindowCreator (creat)
OSType	creat;
{
	creator = creat;
}



/*
	Save the contents of the given window
*/

Boolean EWindowSave (theWind)
WindowPtr	theWind;
{
	return (SaveFile (theWind,	/* window to save */
					  false,	/* don't ask for file if have one */
					  true));	/* bind to new file if one given */
}


/*
	Save the contents of the given window under a new name
	and bind to that name.
*/

Boolean EWindowSaveAs (theWind)
WindowPtr	theWind;
{
	return (SaveFile (theWind,	/* window to save */
					  true,		/* ask for file even if have one */
					  true));	/* bind to new file if one given */
}


/*
	Save the contents of the given window under a new name, but
	don't bind to the name.
*/

Boolean EWindowSaveCopy (theWind)
WindowPtr	theWind;
{
	return (SaveFile (theWind,	/* window to save */
					  true,		/* ask for file even if have one */
					  false));	/* don't bind to file */
}


/*
	Close the window.  If it's dirty and is either bound to a file
	or (if not bound) has some text in it, ask about saving it first,
	giving user option of saving changes, tossing them, or
	cancelling altogether.

	Return true if the file was saved and the window closed, false if
	user cancelled or there was an error.
*/

Boolean EWindowClose (theWind)
WindowPtr	theWind;
{
	if (IsEWindow (theWind) == false)
		return (false);

	SyncAllGlobals (theWind);
	if ( (bound || (**editTE).teLength > 0) && dirty)
	{
		switch (FakeAlert ("\pSave changes to \"", editFile.fName,
				"\p\"?", "\p", 3, 3,
				"\pCancel", "\pDiscard", "\pSave"))	/* ask whether to save */
		{

			case 1:			/* cancel Close */
				return (false);

			case 2:			/* toss changes */
				break;

			case 3:
				if (SaveFile (editWind,	/* window to save */
							  false,	/* don't ask for name */
							  false)	/* don't bind to name */
						== false)
					return (false);	/* cancelled or error - cancel Close */
				break;
		}
	}
	SkelRmveWind (editWind);
	return (true);
}


/*
	Revert to saved version of file on disk.  theWind must be an edit
	window, and must be bound to a file.  Returns false if one of these
	conditions is not met, or if they are met but there was an error
	reading the file.

	The window need not be dirty, but if it is, the user is asked
	whether to really revert.
*/

Boolean EWindowRevert (theWind)
WindowPtr	theWind;
{
	if (!IsEWindow (theWind))
		return (false);
	SyncAllGlobals (theWind);
	if (!bound)
		return (false);		/* no file to revert to */
	if (dirty)
	{
		if (FakeAlert ("\p\"", editFile.fName,
				"\p\" has been changed.  Really revert?",
				"\p", 2, 1, "\pCancel", "\pRevert", "\p") == 1)
			return (false);
	}
	if (Revert () == false)
		return (false);
	ScrollToHome ();
	OverhaulDisplay (true, true);
	DrawControls (editWind);
	ValidRect (&editWind->portRect);
	return (true);
}


/* ---------------------------------------------------------------- */
/*			Interface Initialization/Termination Routines			*/
/* ---------------------------------------------------------------- */


/*
	Initialize the window and associated data structures.
	Return window pointer or nil if some sort of error.

	Preserves the current port.
*/

WindowPtr NewEWindow (bounds, title, visible, behind,
							goAway, refNum, bindToFile)
Rect		*bounds;
StringPtr	title;
Boolean		visible;
WindowPtr	behind;
Boolean		goAway;
long		refNum;
Boolean		bindToFile;
{
GrafPtr		savePort;
Rect		r;
OSType		type = 'TEXT';
Str255		s, s2;
StringPtr	tPtr;

# ifndef	singleEdit
register EIHandle	eInfo;
# endif

# ifdef	singleEdit

	if (editWind != nil)	/* allow only one window at a time */
		return (nil);

# endif

/*
	If supposed to bind to file, ask for name.  Return without doing
	anything if Cancel button clicked.
*/

	if (bindToFile)
	{
		SFGetFile (dlogWhere, "\p", nil, 1, &type, nil, &editFile);
		if (!editFile.good)
			return (nil);
	}
	bound = bindToFile;

/*
	Create window and install handler.  Set window title:  If window is
	to be bound to file, use name of file.  Otherwise use any title that
	was passed in.  If nil was passed, use a default name ("Untitled nnn").
	Also copy the name into the file info structure even if the window is
	unbound, because the Save operations expect to find it there as the
	most likely name to use if the window is untitled.

	Save and restore port, because it gets reset by the rest of the
	initialization code.
*/

	if (bound)
		tPtr = editFile.fName;
	else
	{
		if (title != nil)
			tPtr = title;
		else
		{

# ifndef	singleEdit
			BlockMove ("\pUntitled ", s, 10L);
			NumToString ((long) ++windID, s2);
			BlockMove (&s2[1], &s[10], (long) s2[0]);
			s[0] += s2[0];
			tPtr = s;
# else
			tPtr = (StringPtr) "\pUntitled";
# endif

		}
		BlockMove (tPtr, editFile.fName, (long) (tPtr[0] + 1));
	}

	editWind = NewWindow (nil, bounds, tPtr, false, 8 /* documentProc */,
								behind, goAway, refNum);

	GetPort (&savePort);
	SkelWindow (editWind,	/* the window */
				Mouse,		/* mouse click handler */
				Key,		/* key click handler */
				Update,		/* window updating procedure */
				Activate,	/* window activate/deactivate procedure */
				Close,		/* window close procedure */
				Clobber,	/* window disposal procedure */
				Idle,		/* idle proc */
				true);		/* idle only when frontmost */


/*
	Build the scroll bar.
*/

	CalcScrollRect (&r);
	editScroll = NewControl (editWind, &r, "\p", true, 0, 0, 0,
								scrollBarProc, 0L);



/*
	Create the TE record used for text display.  Use default
	characteristics.
*/

	GetEditRect (&r);
	editTE = TENew (&r, &r);
	SetClikLoop (AutoScroll, editTE);			/* set autoscroll proc */


# ifndef	singleEdit

/*
	Get new information structure, attach to list of known edit
	windows.
*/

	eInfo = New (EditInfo);
	editInfo = eInfo;
	(**eInfo).eNext = ewList;
	ewList = eInfo;
	(**eInfo).editWind = editWind;
	(**eInfo).scroll = editScroll;
	(**eInfo).editTE = editTE;
	(**eInfo).bound = bound;
	(**eInfo).editFile = editFile;

# endif

/*
	Install default event notification procedures, font characteristics.
*/

	SetEWindowProcs (editWind, e_key, e_activate, e_close);
	SetEWindowStyle (editWind, e_font, e_size, e_wrap, e_just);
	SetEWindowKeyFilter (editWind, e_key_filter); /* L. Tierney */
	SetEWindowIdle (editWind, e_idle); /* L. Tierney */
	SetDirty (false);

/*
	If supposed to read file, do so.  Check the return value of
	Revert and toss the window if there was an error.
*/

	if (bindToFile && Revert () == false)
	{
		SkelRmveWind (editWind);
		SetPort (savePort);
		return (nil);
	}

/*
	Show window if specified as visible, and return a pointer to it.
*/

	SyncGlobals (editWind);
	OverhaulDisplay (true, true);
	if (visible)
		ShowWindow (editWind);
	SetPort (savePort);
	return (editWind);
}


/*
	Look through the list of windows, shutting down all the edit
	windows.  If any window is dirty, ask user about saving it first.
	If the user cancels on any such request, ClobberEWindows returns
	false.  If all edit windows are shut down, return true.  It is
	then safe for the host to exit.

	When a window *is* shut down, have to start looking through the
	window list again, since theWind no longer points anywhere
	meaningful.
*/

Boolean ClobberEWindows ()
{
WindowPtr	theWind;

	for (;;)
	{
		for (theWind = FrontWindow ();
				theWind != nil;
					theWind = (WindowPtr) ((WindowPeek) theWind)->nextWindow)
		{
			if (IsEWindow (theWind))
				break;
		}
		if (theWind == nil)
			return (true);		/* all edit windows are shut down */

		if (theWind != FrontWindow ())
		{
			SelectWindow (theWind);
			ShowWindow (theWind);
			EWindowOverhaul (theWind, false, false, IsEWindowDirty (theWind));
			SetPort (theWind);
			ValidRect (&theWind->portRect);
		}

		if (EWindowClose (theWind) == false)
			return (false);		/* cancel or error */
	}
}
