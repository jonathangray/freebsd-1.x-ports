#include "include/copyright.h"

/*
 *  Include file dependencies:
 */

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <xpm.h>

#include "bitmaps/mainbackground.xpm"
#include "bitmaps/space.xpm"
#include "bitmaps/background.xpm"
#include "bitmaps/background2.xpm"
#include "bitmaps/background3.xpm"
#include "bitmaps/background4.xpm"
#include "bitmaps/background5.xpm"
#include "bitmaps/background6.xpm"
#include "bitmaps/background7.xpm"
#include "bitmaps/background8.xpm"
#include "bitmaps/background9.xpm"
#include "bitmaps/icon.xpm"

#include "include/error.h"
#include "include/blocks.h"
#include "include/sfx.h"
#include "include/ball.h"
#include "include/paddle.h"
#include "include/version.h"
#include "include/init.h"

#include "include/stage.h"

/*
 *  Internal macro definitions:
 */

#define LEFT_OFFSET	    10
#define RIGHT_OFFSET    10
#define TOP_OFFSET      10
#define MIDDLE_OFFSET   10

/*
 *  Internal type declarations:
 */

#if NeedFunctionPrototypes
static Window SetWMIcon(Display *display);
#else
static Window SetWMIcon();
#endif

/*
 *  Internal variable declarations:
 */

Window iconWindow;
Window mainWindow;
Window scoreWindow;
Window levelWindow;
Window playWindow;
Window bufferWindow;
Window messWindow;
Window specialWindow;
Window timeWindow;
Pixmap	mainBackPixmap, iconPixmap, spacePixmap;
Pixmap  back1Pixmap, back2Pixmap, back3Pixmap, back4Pixmap, back5Pixmap;
Pixmap  back6Pixmap, back7Pixmap, back8Pixmap, back9Pixmap;

#if NeedFunctionPrototypes
void InitialiseMainBackPixmap(Display *display, Window window, 
	Colormap colormap)
#else
void InitialiseMainBackPixmap(display, window, colormap)
	Display *display;
	Window window;
	Colormap colormap;
#endif
{
	XpmAttributes   attributes;
	int		    XpmErrorStatus;

	attributes.valuemask = XpmColormap;
	attributes.colormap = colormap;

    /* Create the playfield background pixmaps */

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		mainbackground_xpm, &mainBackPixmap, NULL, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseMainBackPixmap()");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, 
		space_xpm, &spacePixmap, NULL, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseMainBackPixmap(space)");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, background_xpm,
		&back1Pixmap, NULL, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseMainBackPixmap()");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, background2_xpm,
		&back2Pixmap, NULL, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseMainBackPixmap()");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, background3_xpm,
		&back3Pixmap, NULL, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseMainBackPixmap()");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, background4_xpm,
		&back4Pixmap, NULL, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseMainBackPixmap()");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, background5_xpm,
		&back5Pixmap, NULL, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseMainBackPixmap()");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, background6_xpm,
		&back6Pixmap, NULL, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseMainBackPixmap()");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, background7_xpm,
		&back7Pixmap, NULL, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseMainBackPixmap()");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, background8_xpm,
		&back8Pixmap, NULL, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseMainBackPixmap()");

	XpmErrorStatus = XpmCreatePixmapFromData(display, window, background9_xpm,
		&back9Pixmap, NULL, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseMainBackPixmap()");

	/* Free the xpm pixmap attributes */
	XpmFreeAttributes(&attributes);
}

#if NeedFunctionPrototypes
void ClearMainWindow(Display *display, Window window)
#else
void ClearMainWindow(display, window)
	Display *display;
	Window window;
#endif
{
	/* Make sure that it is drawn */
	XSetWindowBackgroundPixmap(display, mainWindow, spacePixmap);
	XClearWindow(display, mainWindow);
}

#if NeedFunctionPrototypes
void CreateAllWindows(Display *display, Colormap colormap,
	char **argv, int argc)
#else
void CreateAllWindows(display, colormap, argv, argc)
	Display *display;
	Colormap colormap;
	char **argv;
	int argc;
#endif
{
    char 			title[80];
	int 			offsetX, offsetY, scoreWidth;
	XWMHints 		wmhints;
	XClassHint 		classhints;
	XSizeHints 		sizehints;
	XTextProperty 	windowName, iconName;
	XSetWindowAttributes winattr;
	unsigned long 	valuemask;

	char *window_Name 	= "- XBoing -";
	char *icon_Name 	= "XBoing";

	offsetX = MAIN_WIDTH / 2;
	offsetY = MAIN_HEIGHT / 2;
	scoreWidth = 224;

	/* Create the main window */
    mainWindow = XCreateSimpleWindow(display,
		RootWindow(display, DefaultScreen(display)), 0, 0,
		PLAY_WIDTH + MAIN_WIDTH + 10, PLAY_HEIGHT + MAIN_HEIGHT + 10, 2, 
		red, black);

	/* Create the score window */
    scoreWindow = XCreateSimpleWindow(display, mainWindow, 
		offsetX, 10, scoreWidth, 42, 0, white, black);

	/* Create the level window */
    levelWindow = XCreateSimpleWindow(display, mainWindow, 
		scoreWidth + offsetX + 25, 5, 
			PLAY_WIDTH + offsetX - 20 - scoreWidth, 52, 0, white, black);

	/* Create the playing area window */
    playWindow = XCreateSimpleWindow(display, mainWindow, 
		offsetX, 60, PLAY_WIDTH, PLAY_HEIGHT, 5, red, black);

	/* Create the buffer playing area window NON VISIBLE */
    bufferWindow = XCreateSimpleWindow(display, mainWindow, 
		offsetX, 60, PLAY_WIDTH, PLAY_HEIGHT, 5, red, black);

	/* Create the message area window */
    messWindow = XCreateSimpleWindow(display, mainWindow, 
		offsetX, 65 + PLAY_HEIGHT + 10, PLAY_WIDTH / 2, MESS_HEIGHT, 4, 
		white, black);

	/* Create the special bonus area window */
    specialWindow = XCreateSimpleWindow(display, mainWindow, 
		offsetX + PLAY_WIDTH / 2 + 10, 65 + PLAY_HEIGHT + 10, 
		180, MESS_HEIGHT + 5, 0, white, black);

	/* Create the timer area window */
    timeWindow = XCreateSimpleWindow(display, mainWindow, 
		offsetX + PLAY_WIDTH / 2 + 10 + 180 + 5, 
		65 + PLAY_HEIGHT + 10, PLAY_WIDTH / 8, MESS_HEIGHT + 5, 
		0, white, black);

    /* Set window manager properties */
	sprintf(title, "XBoing V%d.%d", VERSION, REVNUM + buildNum);

	if (XStringListToTextProperty(&window_Name, 1, &windowName) == 0)
		ShutDown(display, 1, "Cannot create window name resource.");

	if (XStringListToTextProperty(&icon_Name, 1, &iconName) == 0) 
		ShutDown(display, 1, "Cannot create icon name resource.");

	iconWindow = SetWMIcon(display);

	/* Various window manager settings */
    wmhints.initial_state 	= NormalState;
	wmhints.input 			= True;
	wmhints.icon_pixmap 	= iconPixmap;
	wmhints.icon_window 	= iconWindow;
	wmhints.flags = StateHint | InputHint | IconPixmapHint | IconWindowHint;

	/* Set the class for XBoing */
	classhints.res_name		= "XBoing";
	classhints.res_class 	= "XBoing";

	/* Setup the max and minimum size that the window will be */
	sizehints.flags 		= PPosition | PSize | PMinSize | PMaxSize;
	sizehints.min_width 	= PLAY_WIDTH + MAIN_WIDTH + 10;
	sizehints.min_height	= PLAY_HEIGHT + MAIN_HEIGHT + 10;
	sizehints.max_width 	= PLAY_WIDTH + MAIN_WIDTH + 10;
	sizehints.max_height	= PLAY_HEIGHT + MAIN_HEIGHT + 10;

	/* Now set the window manager properties */
	XSetWMProperties(display, mainWindow, &windowName, &iconName,
		argv, argc, &sizehints, &wmhints, &classhints);

	/* Set the current icon as the window's background pixmap */
	XSetWindowBackgroundPixmap(display, iconWindow, iconPixmap);
	XClearWindow(display, iconWindow);

	valuemask = CWColormap;
	winattr.colormap = colormap;

	/* Check if the server allows backing store */
    if (DoesBackingStore(XDefaultScreenOfDisplay(display)) == Always)
	{
		/* Ok we want backing store as it is very useful */
		valuemask |= CWBackingStore;
		winattr.backing_store = Always;
	}

	XChangeWindowAttributes(display, mainWindow, 	valuemask, &winattr);
	XChangeWindowAttributes(display, playWindow, 	valuemask, &winattr);
	XChangeWindowAttributes(display, bufferWindow, 	valuemask, &winattr);
	XChangeWindowAttributes(display, levelWindow, 	valuemask, &winattr);
	XChangeWindowAttributes(display, scoreWindow, 	valuemask, &winattr);
	XChangeWindowAttributes(display, messWindow, 	valuemask, &winattr);
	XChangeWindowAttributes(display, specialWindow, valuemask, &winattr);
	XChangeWindowAttributes(display, timeWindow, 	valuemask, &winattr);
}

#if NeedFunctionPrototypes
void SetBackgrounds(Display *display, Colormap colormap)
#else
void SetBackgrounds(display, colormap)
	Display *display;
	Colormap colormap;
#endif
{
	InitialiseMainBackPixmap(display, mainWindow, colormap);

	ClearMainWindow(display, mainWindow);
	XSetWindowBackgroundPixmap(display, levelWindow, ParentRelative);
	XClearWindow(display, levelWindow);
	XSetWindowBackgroundPixmap(display, scoreWindow, ParentRelative);
	XClearWindow(display, scoreWindow);
	XSetWindowBackgroundPixmap(display, specialWindow, ParentRelative);
	XClearWindow(display, specialWindow);
	XSetWindowBackgroundPixmap(display, timeWindow, ParentRelative);
	XClearWindow(display, timeWindow);
}

#if NeedFunctionPrototypes
void MapAllWindows(Display *display)
#else
void MapAllWindows(display)
	Display *display;
#endif
{
	/* Actually make everything visible */
  	XMapWindow(display, specialWindow);
  	XMapWindow(display, timeWindow);
  	XMapWindow(display, messWindow);
	XMapWindow(display, playWindow);
	XMapWindow(display, levelWindow);
	XMapWindow(display, scoreWindow);
	XMapWindow(display, mainWindow);
	XFlush(display);
}

#if NeedFunctionPrototypes
void RedrawPlayWindow(Display *display, Window window)
#else
void RedrawPlayWindow(display, window)
	Display *display;
	Window window;
#endif
{
	/* Redraw the main playfield */
	XClearWindow(display, playWindow);
	RedrawAllBlocks(display, window);
	RedrawPaddle(display, window);
	RedrawBall(display, window);
}

#if NeedFunctionPrototypes
void FreeBackgroundPixmaps(Display *display)
#else
void FreeBackgroundPixmaps(display)
	Display *display;
#endif
{
	/* Free all the backgound pixmaps */
    if (back1Pixmap)	XFreePixmap(display, back1Pixmap); 
	if (back2Pixmap)	XFreePixmap(display, back2Pixmap);
	if (back3Pixmap)	XFreePixmap(display, back3Pixmap); 
	if (back4Pixmap)	XFreePixmap(display, back4Pixmap);
	if (back5Pixmap)	XFreePixmap(display, back5Pixmap); 
	if (back6Pixmap)	XFreePixmap(display, back6Pixmap); 
	if (back7Pixmap)	XFreePixmap(display, back7Pixmap); 
	if (back8Pixmap)	XFreePixmap(display, back8Pixmap); 
	if (back9Pixmap)	XFreePixmap(display, back9Pixmap); 

	/* Free the icon and main background pixmaps */
	if (iconPixmap)		XFreePixmap(display, iconPixmap);
    if (mainBackPixmap)	XFreePixmap(display, mainBackPixmap); 
    if (spacePixmap)	XFreePixmap(display, spacePixmap); 
}

#if NeedFunctionPrototypes
static Window SetWMIcon(Display *display)
#else
static Window SetWMIcon(display)
	Display *display;
#endif
{
    XpmAttributes   attributes;
	Window	   		win, root;
	Colormap		iconcolormap;
	int		    	XpmErrorStatus;
							
	/* Suss out the root window */
	root = RootWindow(display, DefaultScreen(display));

	if (!(win = XCreateSimpleWindow(display, root,
		0, 0, 50, 50, 0, CopyFromParent, CopyFromParent)))
	{
		/* Well, what a bummer. Just use default icon then. */
		ErrorMessage("Cannot create icon pixmap.");
		return ((Window) NULL);
	}

	/* Create a new colourmap for the icon window */
	iconcolormap = XDefaultColormap(display, XDefaultScreen(display));

	/* Create all xpm pixmap blocks from the files */
	attributes.colormap = iconcolormap;
	attributes.valuemask = XpmColormap;
	XpmErrorStatus = XpmCreatePixmapFromData(display, win, 
		icon_xpm, &iconPixmap, NULL, &attributes);
	HandleXPMError(display, XpmErrorStatus, "InitialiseWMIcon()");

	/* Make the new window have the new colourmap */
	XSetWindowColormap(display, win, iconcolormap);

	/* Free the background pixmap attributes */
	XpmFreeAttributes(&attributes);

	return win;
}

#if NeedFunctionPrototypes
void DrawStageBackground(Display *display, Window window, int stageType,
	int clear)
#else
void DrawStageBackground(display, window, stageType, clear)
	Display *display;
	Window window;
	int stageType;
	int clear;
#endif
{
	char type[20];

	if (debug == True)
	{
		sprintf(type, "Changing background to type %d.", stageType);
		DEBUG(type);
	}

	switch (stageType)
	{
		case BACKGROUND_SPACE:
			XSetWindowBackgroundPixmap(display, window, spacePixmap);
			break;

		case BACKGROUND_SEE_THRU:
			XSetWindowBackgroundPixmap(display, window, ParentRelative);
			break;

		case BACKGROUND_BLACK:
			XSetWindowBackground(display, window, black);
			break;

		case BACKGROUND_WHITE:
			XSetWindowBackground(display, window, white);
			break;

		case BACKGROUND_0:
			XSetWindowBackgroundPixmap(display, window, mainBackPixmap);
			break;

		case BACKGROUND_1:
			XSetWindowBackgroundPixmap(display, window, back1Pixmap);
			break;

		case BACKGROUND_2:
			XSetWindowBackgroundPixmap(display, window, back2Pixmap);
			break;

		case BACKGROUND_3:
			XSetWindowBackgroundPixmap(display, window, back3Pixmap);
			break;
																						case BACKGROUND_4:
			XSetWindowBackgroundPixmap(display, window, back4Pixmap);
			break;

		case BACKGROUND_5:
			XSetWindowBackgroundPixmap(display, window, back5Pixmap);
			break;

		case BACKGROUND_6:
			XSetWindowBackgroundPixmap(display, window, back6Pixmap);
			break;

		case BACKGROUND_7:
			XSetWindowBackgroundPixmap(display, window, back7Pixmap);
			break;

		case BACKGROUND_8:
			XSetWindowBackgroundPixmap(display, window, back8Pixmap);
			break;

		case BACKGROUND_9:
			XSetWindowBackgroundPixmap(display, window, back9Pixmap);
			break;

		default:
			ErrorMessage("Unknown background type requested.");
	}

	/* Flush the new background */
	if (clear == True)
		XClearWindow(display, window);
}
