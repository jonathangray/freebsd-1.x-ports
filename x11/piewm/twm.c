/*****************************************************************************/
/**       Copyright 1988 by Evans & Sutherland Computer Corporation,        **/
/**                          Salt Lake City, Utah                           **/
/**  Portions Copyright 1989 by the Massachusetts Institute of Technology   **/
/**                        Cambridge, Massachusetts                         **/
/**                                                                         **/
/**                           All Rights Reserved                           **/
/**                                                                         **/
/**    Permission to use, copy, modify, and distribute this software and    **/
/**    its documentation  for  any  purpose  and  without  fee is hereby    **/
/**    granted, provided that the above copyright notice appear  in  all    **/
/**    copies and that both  that  copyright  notice  and  this  permis-    **/
/**    sion  notice appear in supporting  documentation,  and  that  the    **/
/**    names of Evans & Sutherland and M.I.T. not be used in advertising    **/
/**    in publicity pertaining to distribution of the  software  without    **/
/**    specific, written prior permission.                                  **/
/**                                                                         **/
/**    EVANS & SUTHERLAND AND M.I.T. DISCLAIM ALL WARRANTIES WITH REGARD    **/
/**    TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES  OF  MERCHANT-    **/
/**    ABILITY  AND  FITNESS,  IN  NO  EVENT SHALL EVANS & SUTHERLAND OR    **/
/**    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAM-    **/
/**    AGES OR  ANY DAMAGES WHATSOEVER  RESULTING FROM LOSS OF USE, DATA    **/
/**    OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER    **/
/**    TORTIOUS ACTION, ARISING OUT OF OR IN  CONNECTION  WITH  THE  USE    **/
/**    OR PERFORMANCE OF THIS SOFTWARE.                                     **/
/*****************************************************************************/


/***********************************************************************
 *
 * $XConsortium: twm.c,v 1.111 90/03/23 13:23:34 jim Exp $
 *
 * twm - "Tom's Window Manager"
 *
 * $Log: twm.c,v $
 * Revision 1.1  1994/05/18 18:13:02  asami
 * Initial revision
 *
 * Revision 1.4  1991/10/04  22:36:41  cross
 * Added the initial setting of Scr->tbpm.delete
 *
 * Revision 1.3  1991/10/03  22:01:21  cross
 * Added '-k' to the usage message
 *
 * Revision 1.2  1991/09/26  00:24:04  cross
 * changed references to ..ispixmap.. to ..isxpm..
 *
 * Revision 1.1  1991/09/26  00:21:06  cross
 * Initial revision
 *
 * Revision 10.0  91/06/12  09:05:48  toml
 * Revision bump
 * 
 * Revision 9.0  91/04/23  07:40:51  toml
 * Revision bump
 * 
 * Revision 8.4  90/12/29  16:39:43  toml
 * RJC patches
 * 
 * Revision 8.3  90/12/29  11:24:30  toml
 * Added PannerOpaqueScroll
 * 
 * Revision 8.2  90/12/29  10:13:16  toml
 * StayUpMenus
 * 
 * Revision 8.1  90/11/16  14:16:35  toml
 * Removed xsync
 * 
 * Revision 8.0  90/11/15  20:02:53  toml
 * Revision bump
 * 
 * Revision 7.3  90/11/13  15:06:59  toml
 * More fixes
 * 
 * Revision 7.2  90/11/12  20:57:10  toml
 * Added StickyAbove variable
 * 
 * Revision 7.1  90/11/12  19:57:32  toml
 * Patches to allow sticky windows to lower
 * 
 * Revision 1.2  90/11/04  18:38:33  brossard
 * Sticky windows are now child of the virtual root.
 * This has the advantage that they can now be raised and lowered like
 * any other window.  They no longuer are above everything else.
 * It has the disadvantage that when you move the desktop, the
 * sticky windows have to be moved back after scrolling the desktop.
 * 
 *
 * 27-Oct-87 Thomas E. LaStrange	File created
 *
 ***********************************************************************/

#if !defined(lint) && !defined(SABER)
static char RCSinfo[] =
"$XConsortium: twm.c,v 1.111 90/03/23 13:23:34 jim Exp $";
#endif

#include <stdio.h>
#include <signal.h>
#include <fcntl.h>
#include "twm.h"
#include "add_window.h"
#include "gc.h"
#include "parse.h"
#include "version.h"
#include "menus.h"
#include "events.h"
#include "util.h"
#include "gram.h"
#include "screen.h"
#include "iconmgr.h"
#include <X11/Xproto.h>
#include <X11/bitmaps/root_weave>
#include "vdt.h"
#define XK_LATIN1
#include <X11/keysymdef.h>

Display *dpy;			/* which display are we talking to */
char *display_name = NULL;	/* JMO 2/13/90 for m4 */
int KeepTmpFile = False;	/* JMO 3/28/90 for m4 */
Window ResizeWindow;		/* the window we are resizing */

int MultiScreen = TRUE;		/* try for more than one screen? */
int NumScreens;			/* number of screens in ScreenList */
#ifdef SHAPE
int HasShape;			/* server supports shape extension? */
int ShapeEventBase, ShapeErrorBase;
#endif
ScreenInfo **ScreenList;	/* structures for each screen */
ScreenInfo *Scr = NULL;		/* the cur and prev screens */
int PreviousScreen;		/* last screen that we were on */
int FirstScreen;		/* TRUE ==> first screen of display */
Bool PrintErrorMessages = False;	/* controls error messages */
static int RedirectError;	/* TRUE ==> another window manager running */
static int CatchRedirectError();	/* for settting RedirectError */
static int TwmErrorHandler();	/* for everything else */
char Info[INFO_LINES][INFO_SIZE];		/* info strings to print */
int InfoLines;
char *InitFile = NULL;

Cursor UpperLeftCursor;		/* upper Left corner cursor */
Cursor RightButt;
Cursor MiddleButt;
Cursor LeftButt;

XContext VirtualContext;	/* context for virtual windows */
XContext TwmContext;		/* context for twm windows */
XContext MenuContext;		/* context for all menu windows */
XContext IconManagerContext;	/* context for all window list windows */
XContext ScreenContext;		/* context to get screen data */
XContext ColormapContext;	/* context for colormap operations */

XClassHint NoClass;		/* for applications with no class */

XGCValues Gcv;

char *Home;			/* the HOME environment variable */
int HomeLen;			/* length of Home */
int ParseError;			/* error parsing the .twmrc file */

int HandlingEvents = FALSE;	/* are we handling events yet? */

Window JunkRoot;		/* junk window */
Window JunkChild;		/* junk window */
int JunkX;			/* junk variable */
int JunkY;			/* junk variable */
unsigned int JunkWidth, JunkHeight, JunkBW, JunkDepth, JunkMask;
Bool JunkIsXpm[1] = { False };

char *ProgramName;
int Argc;
char **Argv;
char **Environ;

Bool RestartPreviousState = False;	/* try to restart in previous state */

unsigned long black, white;

/***********************************************************************
 *
 *  Procedure:
 *	main - start of twm
 *
 ***********************************************************************
 */

main(argc, argv, environ)
    int argc;
    char **argv;
    char **environ;
{
    Window root, parent, *children;
    unsigned int nchildren;
    int i, j;
    unsigned long valuemask;	/* mask for create windows */
    XSetWindowAttributes attributes;	/* attributes for create windows */
    SigProc old_handler;
    int numManaged, firstscrn, lastscrn, scrnum;
    extern ColormapWindow *CreateColormapWindow();

    ProgramName = argv[0];
    Argc = argc;
    Argv = argv;
    Environ = environ;

    for (i = 1; i < argc; i++) {
	if (argv[i][0] == '-') {
	    switch (argv[i][1]) {
	      case 'd':				/* -display dpy */
		if (++i >= argc) goto usage;
		display_name = argv[i];
		continue;
	      case 's':				/* -single */
		MultiScreen = FALSE;
		continue;
	      case 'f':				/* -file twmrcfilename */
		if (++i >= argc) goto usage;
		InitFile = argv[i];
		continue;
	      case 'v':				/* -verbose */
		PrintErrorMessages = True;
		continue;
	      case 'q':				/* -quiet */
		PrintErrorMessages = False;
		continue;
	/* This option saves a /tmp/twmrc* file which holds       */
	/* the settings of all of the m4 predefines.  JMO 3/28/90 */
	      case 'k':				/* -keep */
		KeepTmpFile = True;
		continue;
	    }
	}
      usage:
	fprintf (stderr,
		 "usage:  %s [-display dpy] [-f file] [-k] [-s] [-q] [-v]\n",
		 ProgramName);
	exit (1);
    }

    old_handler = signal(SIGINT, SIG_IGN);
    if (old_handler != SIG_IGN)
	signal(SIGINT, Done);

    old_handler = signal(SIGHUP, SIG_IGN);
    if (old_handler != SIG_IGN)
	signal(SIGHUP, Done);

    signal(SIGQUIT, Done);
    signal(SIGTERM, Done);

    Home = getenv("HOME");
    if (Home == NULL)
	Home = "./";

    HomeLen = strlen(Home);

    NoClass.res_name = NoName;
    NoClass.res_class = NoName;

    if (!(dpy = XOpenDisplay(display_name))) {
	fprintf (stderr, "%s:  unable to open display \"%s\"\n",
		 ProgramName, XDisplayName(display_name));
	exit (1);
    }

#ifdef XSYNC
    XSynchronize(dpy, True);
#endif

    if (fcntl(ConnectionNumber(dpy), F_SETFD, 1) == -1) {
	fprintf (stderr, 
		 "%s:  unable to mark display connection as close-on-exec\n",
		 ProgramName);
	exit (1);
    }

#ifdef SHAPE
    HasShape = XShapeQueryExtension (dpy, &ShapeEventBase, &ShapeErrorBase);
#endif
    VirtualContext = XUniqueContext();
    TwmContext = XUniqueContext();
    MenuContext = XUniqueContext();
    IconManagerContext = XUniqueContext();
    ScreenContext = XUniqueContext();
    ColormapContext = XUniqueContext();

    InternUsefulAtoms ();
    InitVirtualDesktop();

    /* Set up the per-screen global information. */

    NumScreens = ScreenCount(dpy);

    if (MultiScreen)
    {
	firstscrn = 0;
	lastscrn = NumScreens - 1;
    }
    else
    {
	firstscrn = lastscrn = DefaultScreen(dpy);
    }

    InfoLines = 0;

    /* for simplicity, always allocate NumScreens ScreenInfo struct pointers */
    ScreenList = (ScreenInfo **) calloc (NumScreens, sizeof (ScreenInfo *));
    numManaged = 0;
    PreviousScreen = DefaultScreen(dpy);
    FirstScreen = TRUE;
    for (scrnum = firstscrn ; scrnum <= lastscrn; scrnum++)
    {
	RedirectError = FALSE;
	XSetErrorHandler(CatchRedirectError);
	XSelectInput(dpy, RootWindow (dpy, scrnum),
	    ColormapChangeMask | EnterWindowMask | PropertyChangeMask | 
	    SubstructureRedirectMask | KeyPressMask |
	    ButtonPressMask | ButtonReleaseMask);
	XSync(dpy, 0);
	XSetErrorHandler(TwmErrorHandler);

	if (RedirectError)
	{
	    fprintf (stderr, "%s:  another window manager is already running",
		     ProgramName);
	    if (MultiScreen && NumScreens > 0)
		fprintf(stderr, " on screen %d?\n", scrnum);
	    else
		fprintf(stderr, "?\n");
	    continue;
	}

	numManaged ++;

	/* Note:  ScreenInfo struct is calloc'ed to initialize to zero. */
	Scr = ScreenList[scrnum] = 
	    (ScreenInfo *) calloc(1, sizeof(ScreenInfo));

	/* initialize list pointers, remember to put an initialization
	 * in InitVariables also
	 */
	Scr->BorderColorL = NULL;
	Scr->IconBorderColorL = NULL;
	Scr->BorderTileForegroundL = NULL;
	Scr->BorderTileBackgroundL = NULL;
	Scr->TitleForegroundL = NULL;
	Scr->TitleBackgroundL = NULL;
	Scr->IconForegroundL = NULL;
	Scr->IconBackgroundL = NULL;
	Scr->NoTitle = NULL;
	Scr->MakeTitle = NULL;
	Scr->AutoRaise = NULL;
	Scr->IconNames = NULL;
	Scr->NoHighlight = NULL;
	Scr->NoStackModeL = NULL;
	Scr->NoTitleHighlight = NULL;
	Scr->DontIconify = NULL;
	Scr->IconMgrNoShow = NULL;
	Scr->IconMgrShow = NULL;
	Scr->IconifyByUn = NULL;
	Scr->IconManagerFL = NULL;
	Scr->IconManagerBL = NULL;
	Scr->IconMgrs = NULL;
	Scr->StartIconified = NULL;
	Scr->SqueezeTitleL = NULL;
	Scr->DontSqueezeTitleL = NULL;
	Scr->WindowRingL = NULL;
	Scr->WarpCursorL = NULL;
	Scr->StickyL = NULL;
	Scr->VirtualBackgroundL = NULL;
	Scr->VirtualForegroundL = NULL;
	Scr->NoIconTitleL = NULL;
	Scr->IconTitleL = NULL;
	/* remember to put an initialization in InitVariables also
	 */

	Scr->screen = scrnum;
	Scr->d_depth = DefaultDepth(dpy, scrnum);
	Scr->d_visual = DefaultVisual(dpy, scrnum);
	Scr->Root = RootWindow(dpy, scrnum);
	XSaveContext (dpy, Scr->Root, ScreenContext, (caddr_t) Scr);
	Scr->VirtualDesktop = None;
	Scr->Panner = None;
	Scr->PannerScale = DEFAULT_PANNER_SCALE;
	Scr->PannerState = NormalState;
	Scr->PannerGeometry = DEFAULT_PANNER_GEOMETRY;

	Scr->TwmRoot.cmaps.number_cwins = 1;
	Scr->TwmRoot.cmaps.cwins =
		(ColormapWindow **) malloc(sizeof(ColormapWindow *));
	Scr->TwmRoot.cmaps.cwins[0] =
		CreateColormapWindow(Scr->Root, True, False);
	Scr->TwmRoot.cmaps.cwins[0]->visibility = VisibilityPartiallyObscured;

	Scr->cmapInfo.cmaps = NULL;
	Scr->cmapInfo.maxCmaps =
		MaxCmapsOfScreen(ScreenOfDisplay(dpy, Scr->screen));
	Scr->cmapInfo.root_pushes = 0;
	InstallWindowColormaps(0, &Scr->TwmRoot);

	Scr->StdCmapInfo.head = Scr->StdCmapInfo.tail = 
	  Scr->StdCmapInfo.mru = NULL;
	Scr->StdCmapInfo.mruindex = 0;
	LocateStandardColormaps();

	Scr->TBInfo.nleft = Scr->TBInfo.nright = 0;
	Scr->TBInfo.head = NULL;
	Scr->TBInfo.border = 1;
	Scr->TBInfo.width = 0;
	Scr->TBInfo.leftx = 0;
	Scr->TBInfo.titlex = 0;

	Scr->MyDisplayWidth = DisplayWidth(dpy, scrnum);
	Scr->MyDisplayHeight = DisplayHeight(dpy, scrnum);
	Scr->MaxWindowWidth = 32767 - Scr->MyDisplayWidth;
	Scr->MaxWindowHeight = 32767 - Scr->MyDisplayHeight;

	Scr->vdtScrollDistanceX = Scr->MyDisplayWidth;
	Scr->vdtScrollDistanceY = Scr->MyDisplayHeight;

	Scr->XORvalue = (((unsigned long) 1) << Scr->d_depth) - 1;

	if (DisplayCells(dpy, scrnum) < 3)
	    Scr->Monochrome = MONOCHROME;
	else
	    Scr->Monochrome = COLOR;

	/* setup default colors */
	Scr->FirstTime = TRUE;
	GetColor(Scr->Monochrome, &black, "black");
	Scr->Black = black;
	GetColor(Scr->Monochrome, &white, "white");
	Scr->White = white;

	if (FirstScreen)
	{
	    SetFocus ((TwmWindow *)NULL);

	    /* define cursors */

	    NewFontCursor(&UpperLeftCursor, "top_left_corner");
	    NewFontCursor(&RightButt, "rightbutton");
	    NewFontCursor(&LeftButt, "leftbutton");
	    NewFontCursor(&MiddleButt, "middlebutton");
	}

	Scr->iconmgr.x = 0;
	Scr->iconmgr.y = 0;
	Scr->iconmgr.width = 150;
	Scr->iconmgr.height = 5;
	Scr->iconmgr.next = NULL;
	Scr->iconmgr.prev = NULL;
	Scr->iconmgr.lasti = &(Scr->iconmgr);
	Scr->iconmgr.first = NULL;
	Scr->iconmgr.last = NULL;
	Scr->iconmgr.active = NULL;
	Scr->iconmgr.scr = Scr;
	Scr->iconmgr.columns = 1;
	Scr->iconmgr.count = 0;
	Scr->iconmgr.name = "TWM";
	Scr->iconmgr.icon_name = "Icons";

	Scr->IconDirectory = NULL;

	Scr->siconifyPm = None;
	Scr->pullPm = None;
	Scr->hilitePm = None;
	Scr->hiliteLeftPm = None;
	Scr->hiliteRightPm = None;
	Scr->tbpm.xlogo = None;
	Scr->tbpm.resize = None;
	Scr->tbpm.question = None;
	Scr->tbpm.menu = None;
	Scr->tbpm.delete = None;

	Scr->rootWeave = XCreatePixmapFromBitmapData(dpy, Scr->Root,
	    root_weave_bits, root_weave_width, root_weave_height,
	    Scr->Black, Scr->White, Scr->d_depth);

	InitVariables();
	InitMenus();

	/* Parse it once for each screen. */
	ParseTwmrc(InitFile);
	if (Scr->SqueezeTitle == -1) Scr->SqueezeTitle = FALSE;
	if (!Scr->HaveFonts) CreateFonts();
	CreateGCs();
	MakeMenus();

	Scr->TitleBarFont.y += Scr->FramePadding + Scr->TitleFontPadding / 2 ;
	Scr->TitleHeight = Scr->TitleBarFont.height + Scr->FramePadding * 2 + Scr->TitleFontPadding ;
	/* make title height be odd so buttons look nice and centered */

	/*
	if (!(Scr->TitleHeight & 1)) Scr->TitleHeight++;
	*/

	InitTitlebarButtons ();		/* menus are now loaded! */

	XGrabServer(dpy);
	XSync(dpy, 0);

	if (Scr->VirtualDesktop) {
	    MakeVirtualDesktop(Scr->vdtWidth, Scr->vdtHeight);
	    MakePanner();
	    SetSWM_VERSION();
	    XMapRaised(dpy, Scr->VirtualDesktop);
	}

	JunkX = 0;
	JunkY = 0;

	XQueryTree(dpy, Scr->Root, &root, &parent, &children, &nchildren);
	CreateIconManagers();
	if (!Scr->NoIconManagers)
	    Scr->iconmgr.twm_win->icon = TRUE;

	/*
	 * weed out icon windows
	 */
	for (i = 0; i < nchildren; i++) {
	    if (children[i]) {
		XWMHints *wmhintsp = XGetWMHints (dpy, children[i]);

		if (wmhintsp) {
		    if (wmhintsp->flags & IconWindowHint) {
			for (j = 0; j < nchildren; j++) {
			    if (children[j] == wmhintsp->icon_window) {
				children[j] = None;
				break;
			    }
			}
		    }
		    XFree ((char *) wmhintsp);
		}
	    }
	}

	/*
	 * map all of the non-override windows
	 */
	for (i = 0; i < nchildren; i++)
	{
	    if (children[i] && MappedNotOverride(children[i]))
	    {
		XUnmapWindow(dpy, children[i]);
		SimulateMapRequest(children[i]);
	    }
	}

	if (Scr->ShowIconManager && !Scr->NoIconManagers)
	{
	    Scr->iconmgr.twm_win->icon = FALSE;
	    if (Scr->iconmgr.count)
	    {
		SetMapStateProp (Scr->iconmgr.twm_win, NormalState);
		MapFrame(Scr->iconmgr.twm_win);
	    }
	}

	
	attributes.border_pixel = Scr->DefaultC.fore;
	attributes.background_pixel = Scr->DefaultC.back;
	attributes.event_mask = (ExposureMask |
				 KeyPressMask | ButtonPressMask);
	attributes.backing_store = NotUseful;
	attributes.cursor = XCreateFontCursor (dpy, XC_hand2);
	valuemask = (CWBorderPixel | CWBackPixel | CWEventMask | 
		     CWBackingStore | CWCursor);
	Scr->InfoWindow = XCreateWindow (dpy, Scr->Root, 0, 0, 
					 (unsigned int) 5, (unsigned int) 5,
					 (unsigned int) BW, 0,
					 (unsigned int) CopyFromParent,
					 (Visual *) CopyFromParent,
					 valuemask, &attributes);

	Scr->SizeStringWidth = XTextWidth (Scr->SizeFont.font,
					   " 88888 x 88888 ", 15);
	valuemask = (CWBorderPixel | CWBackPixel | CWBitGravity);
	attributes.bit_gravity = NorthWestGravity;
	Scr->SizeWindow = XCreateWindow (dpy, Scr->Root, 0, 0, 
					 (unsigned int) Scr->SizeStringWidth,
					 (unsigned int) (Scr->SizeFont.height +
							 SIZE_VINDENT*2),
					 (unsigned int) BW, 0,
					 (unsigned int) CopyFromParent,
					 (Visual *) CopyFromParent,
					 valuemask, &attributes);

	XUngrabServer(dpy);

	FirstScreen = FALSE;
    	Scr->FirstTime = FALSE;
    } /* for */

    if (numManaged == 0) {
	if (MultiScreen && NumScreens > 0)
	  fprintf (stderr, "%s:  unable to find any unmanaged screens\n",
		   ProgramName);
	exit (1);
    }

    RestartPreviousState = False;
    HandlingEvents = TRUE;
    InitEvents();
    HandleEvents();
}

/***********************************************************************
 *
 *  Procedure:
 *	InitVariables - initialize twm variables
 *
 ***********************************************************************
 */

InitVariables()
{
    FreeList(&Scr->BorderColorL);
    FreeList(&Scr->IconBorderColorL);
    FreeList(&Scr->BorderTileForegroundL);
    FreeList(&Scr->BorderTileBackgroundL);
    FreeList(&Scr->TitleForegroundL);
    FreeList(&Scr->TitleBackgroundL);
    FreeList(&Scr->IconForegroundL);
    FreeList(&Scr->IconBackgroundL);
    FreeList(&Scr->IconManagerFL);
    FreeList(&Scr->IconManagerBL);
    FreeList(&Scr->IconMgrs);
    FreeList(&Scr->NoTitle);
    FreeList(&Scr->MakeTitle);
    FreeList(&Scr->AutoRaise);
    FreeList(&Scr->IconNames);
    FreeList(&Scr->NoHighlight);
    FreeList(&Scr->NoStackModeL);
    FreeList(&Scr->NoTitleHighlight);
    FreeList(&Scr->DontIconify);
    FreeList(&Scr->IconMgrNoShow);
    FreeList(&Scr->IconMgrShow);
    FreeList(&Scr->IconifyByUn);
    FreeList(&Scr->StartIconified);
    FreeList(&Scr->IconManagerHighlightL);
    FreeList(&Scr->SqueezeTitleL);
    FreeList(&Scr->DontSqueezeTitleL);
    FreeList(&Scr->WindowRingL);
    FreeList(&Scr->WarpCursorL);
    FreeList(&Scr->StickyL);
    FreeList(&Scr->VirtualBackgroundL);
    FreeList(&Scr->VirtualForegroundL);
    FreeList(&Scr->NoIconTitleL);
    FreeList(&Scr->IconTitleL);

    NewFontCursor(&Scr->FrameCursor, "top_left_arrow");
    NewFontCursor(&Scr->TitleCursor, "top_left_arrow");
    NewFontCursor(&Scr->IconCursor, "top_left_arrow");
    NewFontCursor(&Scr->IconMgrCursor, "top_left_arrow");
    NewFontCursor(&Scr->MoveCursor, "fleur");
    NewFontCursor(&Scr->ResizeCursor, "fleur");
    NewFontCursor(&Scr->MenuCursor, "sb_left_arrow");
    NewFontCursor(&Scr->ButtonCursor, "hand2");
    NewFontCursor(&Scr->WaitCursor, "watch");
    NewFontCursor(&Scr->SelectCursor, "dot");
    NewFontCursor(&Scr->DestroyCursor, "pirate");

    Scr->Ring = NULL;
    Scr->RingLeader = NULL;

    Scr->vdtC.fore = black;
    Scr->vdtC.back = white;
    Scr->PannerC.fore = black;
    Scr->PannerC.back = white;
    Scr->DefaultC.fore = black;
    Scr->DefaultC.back = white;
    Scr->BorderColor = black;
    Scr->BorderTileC.fore = black;
    Scr->BorderTileC.back = white;
    Scr->TitleC.fore = black;
    Scr->TitleC.back = white;
    Scr->MenuC.fore = black;
    Scr->MenuC.back = white;
    Scr->MenuTitleC.fore = black;
    Scr->MenuTitleC.back = white;
    Scr->MenuShadowColor = black;
    Scr->IconC.fore = black;
    Scr->IconC.back = white;
    Scr->IconBorderColor = black;
    Scr->IconManagerC.fore = black;
    Scr->IconManagerC.back = white;
    Scr->IconManagerHighlight = black;
    Scr->VirtualC.back = UNKNOWN_PIXEL;
    Scr->VirtualC.fore = UNKNOWN_PIXEL;

    Scr->MenuLineWidth = 1;
    Scr->FramePadding = 2;		/* values that look "nice" on */
    Scr->TitlePadding = 8;		/* 75 and 100dpi displays */
    Scr->TitleFontPadding = 0;
    Scr->ButtonIndent = 1;
    Scr->SizeStringOffset = 0;
    Scr->BorderWidth = BW;
    Scr->IconBorderWidth = BW;
    Scr->UnknownWidth = 0;
    Scr->UnknownHeight = 0;
    Scr->NumAutoRaises = 0;
    Scr->NoDefaults = FALSE;
    Scr->UsePPosition = PPOS_OFF;
    Scr->FocusRoot = TRUE;
    Scr->Focus = NULL;
    Scr->WarpCursor = FALSE;
    Scr->ForceIcon = FALSE;
    Scr->NoGrabServer = FALSE;
    Scr->NoRaiseMove = FALSE;
    Scr->NoRaiseResize = FALSE;
    Scr->NoRaiseDeicon = FALSE;
    Scr->NoRaiseWarp = FALSE;
    Scr->DontMoveOff = FALSE;
    Scr->DoZoom = FALSE;
    Scr->TitleFocus = TRUE;
    Scr->NoTitlebar = FALSE;
    Scr->DecorateTransients = FALSE;
    Scr->IconifyByUnmapping = FALSE;
    Scr->ShowIconManager = FALSE;
    Scr->IconManagerDontShow =FALSE;
    Scr->BackingStore = TRUE;
    Scr->SaveUnder = TRUE;
    Scr->RandomPlacement = FALSE;
    Scr->OpaqueMove = FALSE;
    Scr->Highlight = TRUE;
    Scr->StackMode = TRUE;
    Scr->TitleHighlight = TRUE;
    Scr->MoveDelta = 1;
    Scr->ZoomCount = 8;
    Scr->SortIconMgr = FALSE;
    Scr->Shadow = TRUE;
    Scr->InterpolateMenuColors = FALSE;
    Scr->NoIconManagers = FALSE;
    Scr->ClientBorderWidth = FALSE;
    Scr->SqueezeTitle = -1;
    Scr->FirstRegion = NULL;
    Scr->LastRegion = NULL;
    Scr->FirstTime = TRUE;
    Scr->HaveFonts = FALSE;		/* i.e. not loaded yet */
    Scr->CaseSensitive = TRUE;
    Scr->WarpUnmapped = FALSE;
    Scr->NoIconTitle = FALSE;
    Scr->StickyAbove = FALSE;
    Scr->StayUpMenus = FALSE;
    Scr->PannerOpaqueScroll = FALSE;
	Scr->ListRings = FALSE;

    /* setup default fonts; overridden by defaults from system.twmrc */
#define DEFAULT_NICE_FONT "variable"
#define DEFAULT_FAST_FONT "fixed"
#define DEFAULT_SMALL_FONT "5x8"

    Scr->TitleBarFont.font = NULL;
    Scr->TitleBarFont.name = DEFAULT_NICE_FONT;
    Scr->MenuFont.font = NULL;
    Scr->MenuFont.name = DEFAULT_NICE_FONT;
    Scr->MenuTitleFont.font = NULL;
    Scr->MenuTitleFont.name = NULL; /* use MenuFont unless set */
    Scr->IconFont.font = NULL;
    Scr->IconFont.name = DEFAULT_NICE_FONT;
    Scr->SizeFont.font = NULL;
    Scr->SizeFont.name = DEFAULT_FAST_FONT;
    Scr->IconManagerFont.font = NULL;
    Scr->IconManagerFont.name = DEFAULT_NICE_FONT;
    Scr->VirtualFont.font = NULL;
    Scr->VirtualFont.name = DEFAULT_SMALL_FONT;
    Scr->DefaultFont.font = NULL;
    Scr->DefaultFont.name = DEFAULT_FAST_FONT;

}


CreateFonts ()
{
    GetFont(&Scr->TitleBarFont);
    GetFont(&Scr->MenuFont);
    if ( Scr->MenuTitleFont.name != NULL )
	{
	GetFont(&Scr->MenuTitleFont);
	}
    GetFont(&Scr->IconFont);
    GetFont(&Scr->SizeFont);
    GetFont(&Scr->IconManagerFont);
    GetFont(&Scr->DefaultFont);
    GetFont(&Scr->VirtualFont);
    Scr->HaveFonts = TRUE;
}


RestoreWithdrawnLocation (tmp)
    TwmWindow *tmp;
{
    int gravx, gravy;
    unsigned int bw, mask;
    XWindowChanges xwc;

    if (XGetGeometry (dpy, tmp->w, &JunkRoot, &xwc.x, &xwc.y, 
		      &JunkWidth, &JunkHeight, &bw, &JunkDepth)) {

	GetGravityOffsets (tmp, &gravx, &gravy);
	if (gravy < 0) xwc.y -= tmp->title_height;

	if (bw != tmp->old_bw) {
	    int xoff, yoff;

	    if (!Scr->ClientBorderWidth) {
		xoff = gravx;
		yoff = gravy;
	    } else {
		xoff = 0;
		yoff = 0;
	    }

	    xwc.x -= (xoff + 1) * tmp->old_bw;
	    xwc.y -= (yoff + 1) * tmp->old_bw;
	}
	if (!Scr->ClientBorderWidth) {
	    xwc.x += gravx * tmp->frame_bw;
	    xwc.y += gravy * tmp->frame_bw;
	}

	mask = (CWX | CWY);
	if (bw != tmp->old_bw) {
	    xwc.border_width = tmp->old_bw;
	    mask |= CWBorderWidth;
	}

	XConfigureWindow (dpy, tmp->w, mask, &xwc);

	if (tmp->wmhints && (tmp->wmhints->flags & IconWindowHint)) {
	    XUnmapWindow (dpy, tmp->wmhints->icon_window);
	}

    }
}


/***********************************************************************
 *
 *  Procedure:
 *	Done - cleanup and exit twm
 *
 *  Returned Value:
 *	none
 *
 *  Inputs:
 *	none
 *
 *  Outputs:
 *	none
 *
 *  Special Considerations:
 *	none
 *
 ***********************************************************************
 */

void Reborder ()
{
    TwmWindow *tmp;			/* temp twm window structure */
    int scrnum;
    extern Atom XA_SWM_ROOT;

    /* put a border back around all windows */

    XGrabServer (dpy);
    for (scrnum = 0; scrnum < NumScreens; scrnum++)
    {
	if ((Scr = ScreenList[scrnum]) == NULL)
	    continue;

	RemoveSWM_VERSION();
	InstallWindowColormaps (0, &Scr->TwmRoot);	/* force reinstall */
	for (tmp = Scr->TwmRoot.next; tmp != NULL; tmp = tmp->next)
	{
	    RestoreWithdrawnLocation (tmp);
	    XMapWindow (dpy, tmp->w);
	}

        /* cleanup all windows on the virtual root */
        if (Scr->VirtualDesktop)
        {
            Window root_return,parent_return,*children;
            unsigned int num_children;
            int i;
            int x, y;
            unsigned int width, height, bw, depth;

	    for (tmp = Scr->TwmRoot.next; tmp != NULL; tmp = tmp->next)
            {
		XGetGeometry(dpy, tmp->frame, &root_return, &x, &y, &width, &height, &bw, &depth);
		if(tmp->sticky && !Scr->StickyAbove)
			XReparentWindow(dpy, tmp->frame, Scr->Root, x - Scr->vdtPositionX, y - Scr->vdtPositionY );
		else
			XReparentWindow(dpy, tmp->frame, Scr->Root, x, y);
		XRemoveFromSaveSet(dpy, tmp->frame);
            }

            XQueryTree(dpy, Scr->VirtualDesktop, &root_return,&parent_return, &children, &num_children);
	    for (i = 0; i < num_children; i++)
	    {
		    XGetGeometry(dpy, children[i], &root_return, &x, &y, &width, &height, &bw, &depth);
		    XReparentWindow(dpy, children[i], Scr->Root, x, y);
		    XRemoveFromSaveSet(dpy, children[i]);
	    }
        }
	for (tmp = Scr->TwmRoot.next; tmp != NULL; tmp = tmp->next)
	{
	    XDeleteProperty(dpy, tmp->w, XA_SWM_ROOT);
	}
    }

    XUngrabServer (dpy);
    SetFocus ((TwmWindow*)NULL);
}

void Done()
{
    Reborder ();
    XCloseDisplay(dpy);
    exit(0);
}


/*
 * Error Handlers.  If a client dies, we'll get a BadWindow error (except for
 * GetGeometry which returns BadDrawable) for most operations that we do before
 * manipulating the client's window.
 */

Bool ErrorOccurred = False;
XErrorEvent LastErrorEvent;

static int TwmErrorHandler(dpy, event)
    Display *dpy;
    XErrorEvent *event;
{
    LastErrorEvent = *event;
    ErrorOccurred = True;

    if (PrintErrorMessages && 			/* don't be too obnoxious */
	event->error_code != BadWindow &&	/* watch for dead puppies */
	(event->request_code != X_GetGeometry &&	 /* of all styles */
	 event->error_code != BadDrawable))
      XmuPrintDefaultErrorMessage (dpy, event, stderr);
    return 0;
}


/* ARGSUSED*/
static int CatchRedirectError(dpy, event)
    Display *dpy;
    XErrorEvent *event;
{
    RedirectError = TRUE;
    LastErrorEvent = *event;
    ErrorOccurred = True;
    return 0;
}


Atom _XA_WM_CHANGE_STATE;
Atom _XA_WM_STATE;
Atom _XA_WM_COLORMAP_WINDOWS;
Atom _XA_WM_PROTOCOLS;
Atom _XA_WM_TAKE_FOCUS;
Atom _XA_WM_SAVE_YOURSELF;
Atom _XA_WM_DELETE_WINDOW;
Atom _XA_TWM_FLAGS;
Atom _XA_TWM_USER_MENU;

InternUsefulAtoms ()
{
    _XA_WM_CHANGE_STATE = XInternAtom (dpy, "WM_CHANGE_STATE", False);
    _XA_WM_STATE = XInternAtom (dpy, "WM_STATE", False);
    _XA_WM_COLORMAP_WINDOWS = XInternAtom (dpy, "WM_COLORMAP_WINDOWS", False);
    _XA_WM_PROTOCOLS = XInternAtom (dpy, "WM_PROTOCOLS", False);
    _XA_WM_TAKE_FOCUS = XInternAtom (dpy, "WM_TAKE_FOCUS", False);
    _XA_WM_SAVE_YOURSELF = XInternAtom (dpy, "WM_SAVE_YOURSELF", False);
    _XA_WM_DELETE_WINDOW = XInternAtom (dpy, "WM_DELETE_WINDOW", False);
    _XA_TWM_FLAGS = XInternAtom (dpy, "_TWM_FLAGS", False);
    _XA_TWM_USER_MENU = XInternAtom (dpy, "_TWM_USER_MENU", False);
}

int FIXED_XmuCompareISOLatin1 (first, second)
    char *first, *second;
{
    register unsigned char *ap, *bp;
    register unsigned char a= *first, b= *second;

    for (ap = (unsigned char *) first, bp = (unsigned char *) second;
	 *ap && *bp; ap++, bp++) {

	if ((a = *ap) != (b = *bp)) {
	    /* try lowercasing and try again */

	    if ((a >= XK_A) && (a <= XK_Z))
	      a += (XK_a - XK_A);
	    else if ((a >= XK_Agrave) && (a <= XK_Odiaeresis))
	      a += (XK_agrave - XK_Agrave);
	    else if ((a >= XK_Ooblique) && (a <= XK_Thorn))
	      a += (XK_oslash - XK_Ooblique);

	    if ((b >= XK_A) && (b <= XK_Z))
	      b += (XK_a - XK_A);
	    else if ((b >= XK_Agrave) && (b <= XK_Odiaeresis))
	      b += (XK_agrave - XK_Agrave);
	    else if ((b >= XK_Ooblique) && (b <= XK_Thorn))
	      b += (XK_oslash - XK_Ooblique);

	    if (a != b) break;
	}
    }
    return (((int)a) - ((int) b));
}
