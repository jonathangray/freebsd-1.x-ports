#ifdef MPWC
# include	<Dialogs.h>
# include	<Memory.h>
#else
# include	<ControlMgr.h>
# include	<DialogMgr.h>
#endif MPWC


# define	nil		(0L)


/*
	In-memory item list for dialog with four items:

	1	"^0^1^2^3" (static text)
	2	Button 1
	3	Button 2
	4	Button 3

	The caller of FakeAlert passes the four strings that are to be
	substituted into the first item, the number of buttons that
	should be used, and the titles to put into each button.
	A copy of the item list is hacked to use the right number of
	buttons.

	Thanks to Erik Kilk and Jason Haines.  Some of the stuff to do
	this is modified from code they wrote.
*/


static short	itemList [] =
{
	3,					/* max number of items - 1 */

/*
	statText item
*/
	0, 0,				/* reserve a long for item handle */
	10, 27, 61, 225,	/* display rectangle */
	((8+128) << 8) | 8,	/* 8 + 128 = statText (disabled), title 8 bytes long */
	'^0', '^1',		/* ^0^1^2^3 */
	'^2', '^3',

/*
	first button
*/

	0, 0,				/* reserve a long for item handle */
	104, 140, 124, 210,	/* display rectangle */
	(4 << 8) | 0,		/* 4 = pushButton, title is 0 bytes long*/

/*
	second button
*/

	0, 0,				/* reserve a long for item handle */
	104, 30, 124, 100,	/* display rectangle */
	(4 << 8) | 0,		/* 4 = pushButton, title is 0 bytes long */

/*
	third button
*/

	0, 0,				/* reserve a long for item handle */
	72, 30, 92, 100,	/* display rectangle */
	(4 << 8) | 0		/* 4 = pushButton, title is 0 bytes long */
};


/*
	Set dialog button title and draw bold outline if makeBold true.
	This must be done after the window is shown or else the bold
	outline won't show up (which is probably the wrong way to do it).
*/

static SetDControl (theDialog, itemNo, title, makeBold)
DialogPtr	theDialog;
int			itemNo;
StringPtr	title;
Boolean		makeBold;
{
Handle		itemHandle;
short		itemType;
Rect		itemRect;
PenState	pState;

	GetDItem (theDialog, itemNo, &itemType, &itemHandle, &itemRect);
	SetCTitle ((ControlHandle) itemHandle, title);
	if (makeBold)
	{
		GetPenState (&pState);
		PenNormal ();
		PenSize (3, 3);
		InsetRect (&itemRect, -4, -4);
		FrameRoundRect (&itemRect, 16, 16);
		SetPenState (&pState);
	}
}


/*
	Fake an alert, using an in-memory window and item list.
	The message to be presented is constructed from the first
	four arguments.  nButtons is the number of buttons to use,
	defButton is the default button, the next three args are
	the titles to put into the buttons.  The return value is
	the button number (1..nButtons).  This must be interpreted
	by the caller, since the buttons may be given arbitrary
	titles.

	nButtons should be between 1 and 3, inclusive.
	defButton should be between 1 and nButtons, inclusive.
*/


FakeAlert (s1, s2, s3, s4, nButtons, defButton, t1, t2, t3)
StringPtr	s1, s2, s3, s4;
int			nButtons;
int			defButton;
StringPtr	t1, t2, t3;
{
GrafPtr		savePort;
register DialogPtr	theDialog;
register Handle		iListHandle;
Rect		bounds;
short		itemHit;

	InitCursor ();
	GetPort (&savePort);
	iListHandle = NewHandle (512L);
	HLock (iListHandle);
	BlockMove ((Ptr) &itemList, *iListHandle, 512L);
	((short *) *iListHandle)[0] = nButtons;	/* = number items - 1 */
	SetRect (&bounds, 115, 80, 355, 220);
	theDialog = NewDialog (nil, &bounds, "\p", false, dBoxProc, (WindowPtr) -1L,
							false, 0L, iListHandle);

	ParamText (s1, s2, s3, s4);		/* construct message */

	SetPort (theDialog);
	ShowWindow (theDialog);

	switch (nButtons)				/* set button titles */
	{
		case 3:
			SetDControl (theDialog, 4, t3, defButton == 3);
			/* fall through... */
		case 2:
			SetDControl (theDialog, 3, t2, defButton == 2);
			/* fall through... */
		case 1:
			SetDControl (theDialog, 2, t1, defButton == 1);
	}

/*
	ModalDialog returns 1 if return/enter hit, which, since
	the statText item is first, can be unambiguously
	interpreted as "choose default".
*/
	ModalDialog (nil, &itemHit);
	itemHit = (itemHit == 1 ? defButton : itemHit - 1);
	HUnlock (iListHandle);
	/*HPurge (iListHandle);*/
	DisposDialog (theDialog);
	SetPort (savePort);
	return ((int) itemHit);
}
