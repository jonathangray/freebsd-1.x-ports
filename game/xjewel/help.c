/*
**
**	X11 Jewel By David Cooper and Jose Guterman 05/92
**
*/

#ifdef VMS
#include <decw$include/Xlib.h>
#include <decw$include/Xutil.h>
#include <decw$include/Xos.h>
#else
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#endif
 
#include "general.h"
#include "logic.h"
#include "jewel.h"
#include "xhscore.h"
#include "xw.h"


XFontStruct *SymbolFont;
GC SymbolGC;
XFontStruct *HeaderFont;
GC HeaderGC;
#define SYMBOL_FONT "-adobe-symbol-*-*-*-*-18-*-*-*-*-*-adobe-*"
#define HEADER_FONT "-*-*-bold-r-*-*-24-*-*-*-p-*-iso8859-1"
#define HEADER_LOC_Y 100
#define HELP_LEFT_X 100
#define HELP_RIGHT_X (SCREEN_X - HELP_LEFT_X)
char *HeaderString = "\253\253\253 Keys \273\273\273";

#define NUM_HELP 6
static char *HelpStrings[NUM_HELP][3]=
	{
		{ "\254", ", j, 4 ",     " Move Block Left", },
		{ "\255", ", k, 5 ",     " Rotate Block", },
		{ "\256", ", l, 6 ",     " Move Block Right", },
		{ "\257", ", Space, 0 ", " Drop Block", },
		{ " ", "P, p", " Pause/unPause", },
		{ " ", "U, u", " Iconify and Pause", },
	};


void Expose_Help()
	{
	int i, y;
	XCharStruct Sizes;
	int dir, asc, dsc;

	y=HEADER_LOC_Y;

	XTextExtents(HeaderFont,HeaderString,strlen(HeaderString),
		&dir,&asc,&dsc,&Sizes);
	XDrawImageString(xw_display, xw_window, HeaderGC,
		(SCREEN_X - (Sizes.width))/2 , y,
		HeaderString, strlen(HeaderString));

	for (i=0; i< NUM_HELP; i++)
		{
		int len;
		y+=((HeaderFont->ascent+HeaderFont->descent)*2);
		/* draw keypad */
		len=strlen(HelpStrings[i][0]);
		XTextExtents(SymbolFont,HelpStrings[i][0],len,
			&dir,&asc,&dsc,&Sizes);
		XDrawImageString(xw_display, xw_window, SymbolGC, 
			HELP_LEFT_X, y, HelpStrings[i][0],len);
		/* draw keyboard */
		len=strlen(HelpStrings[i][1]);
		XDrawImageString(xw_display, xw_window, HeaderGC,
			(HELP_LEFT_X + Sizes.width), y, HelpStrings[i][1], len);
		/* draw operations */
		len=strlen(HelpStrings[i][2]);
		XTextExtents(HeaderFont,HelpStrings[i][2],len,
			&dir,&asc,&dsc,&Sizes);
		XDrawImageString(xw_display, xw_window, HeaderGC,
			(HELP_RIGHT_X - Sizes.width), y, HelpStrings[i][2], len);
		}

	XTextExtents(VerFont,StartString, strlen(StartString),
		&dir,&asc,&dsc,&Sizes);
	XDrawImageString(xw_display, xw_window, VerGC,
		(SCREEN_X - Sizes.width)/2,
		START_LOC_Y+((VerFont->ascent)*3/2),
		StartString, strlen(StartString));

	XDrawImageString(xw_display, xw_window, VerGC, VER_LOC_X,
		VER_LOC_Y+((VerFont->ascent)*3/2),
		VerString, strlen(VerString));
	XFlush(xw_display);
	}


void Start_Help()
	{
	XClearWindow(xw_display, xw_window);

	Expose_Help();
	JewelState=HELP;
	xw_set_timer(10000L);
	}

void Init_Help()
	{
	XGCValues gcv;
	unsigned long gcvm;

	gcvm=(GCFont | GCGraphicsExposures | GCForeground | GCBackground);

	gcv.graphics_exposures=False;
	gcv.foreground=white;
	gcv.background=black;

	if ( (SymbolFont=XLoadQueryFont(xw_display,SYMBOL_FONT)) == NULL)
		{ xw_fatal("Cannot load SYMBOL font.\n",__LINE__,__FILE__); }
	gcv.font=SymbolFont->fid;
	SymbolGC=XCreateGC(xw_display, xw_window, gcvm, &gcv);

	if ( (HeaderFont=XLoadQueryFont(xw_display,HEADER_FONT)) == NULL)
		{ xw_fatal("Cannot load HEADER font.\n",__LINE__,__FILE__); }
	gcv.font=HeaderFont->fid;
	HeaderGC=XCreateGC(xw_display, xw_window, gcvm, &gcv);
	}
