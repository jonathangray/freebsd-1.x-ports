/* $Header: /a/cvs/386BSD/ports/editor/point/mouse.c,v 1.1 1994/02/15 22:12:38 jkh Exp $ */

#include <stdio.h>
#include "pt.h"
#include <X11/Xatom.h>

/* mouse motion constants (for the arrays) */
#define MM_NOMOTION	0
#define MM_NORTH	1
#define MM_EAST		2
#define MM_SOUTH	3
#define MM_WEST		4

static int mouseMotionActive = 0;
static int currentDirection;

/* variables to handle timing */
static int intervalOn = 0;
static Tk_TimerToken timer_token;
static struct window * mouse_w;

/* global mouse motion variables */
struct mmData * mm_data;
int mm_length[5];
int mm_x, mm_y, mm_row, mm_col;
int mm_origin_x[5];
int mm_origin_y[5];
Offset mm_cp = -1;
GC gc;

static Tk_Uid BeginSelection;
static Tk_Uid BeginExtend;
static Tk_Uid ExtendSel;
static Tk_Uid EndExtending;
static Tk_Uid BeginMouseMenu1;
static Tk_Uid BeginMouseMenu2;
static Tk_Uid ContinueMouseMenu;
static Tk_Uid CancelMouseMenu;
static Tk_Uid EndMouseMenu;

void
InitMouse()
{
	BeginSelection	= Tk_GetUid( "BeginSelection" );
	BeginExtend	= Tk_GetUid( "BeginExtend" );
	ExtendSel	= Tk_GetUid( "ExtendSelection" );
	EndExtending	= Tk_GetUid( "EndExtending" );
	BeginMouseMenu1	= Tk_GetUid( "BeginMouseMenu1" );
	BeginMouseMenu2	= Tk_GetUid( "BeginMouseMenu2" );
	ContinueMouseMenu=Tk_GetUid( "ContinueMouseMenu" );
	CancelMouseMenu	= Tk_GetUid( "CancelMouseMenu" );
	EndMouseMenu	= Tk_GetUid( "EndMouseMenu" );
}

static void DrawInitialMenu();

static void
DrawMenuString( w, direction )
        struct window * w;
	int direction;
{
	extern Display *MainDisplay;

	XDrawImageString( MainDisplay, w->x_window_id, gc,
		mm_origin_x[direction], mm_origin_y[direction],
		mm_data[direction].label, mm_data[direction].length);
}

/*ARGSUSED*/
static void
mmTimeout( client_data )
	ClientData client_data;
{
	intervalOn = 0;
	DrawInitialMenu( mouse_w );
}

void
Mouse( w, cmd, x, y, clicks )
	struct window * w;
	char * cmd;
	int x;
	int y;
	int clicks;
{
	extern struct window *selWindow;
	extern Offset selBegin, selEnd;
	extern int selMode;
	extern int menuDelay;
	extern struct mmData mm1Data[];
	extern struct mmData mm2Data[];
	extern Cursor currentCursor;
	extern Display *MainDisplay;
	extern int mouseSpriteMenu;
	extern int menuTolerance;
	extern char msgBuffer[];
	
	static int lastRow = -1;
	static int lastCol = -1;
	static int anchorCp = 0;

	int row, col, n;
	Offset cp, beginRowCp;
	/* two abbreviations */
	struct fontDataStruct *font = &(w->font);
	int fid = w->fileId;
	Tk_Uid cmd_uid = Tk_GetUid( cmd );

	if( selWindow == 0 )
		return;

	row = (y - w->topMargin) / font->height;
	col = (x - w->leftMargin) / font->width;
	cp = xyToOffset( w, row, col );
	if( cp == fileSize(fid) ) {
		/* we are at or beyond the EOF marker */
		/* so fix up the row and column numbers */
		OffsetToXY( w, cp, &row, &col );
	}
	n = -1;
	beginRowCp = prevLine( fid, cp, &n );

if( cmd_uid == BeginSelection ) {
	Offset oldBegin, oldEnd;

	anchorCp = cp;
	lastRow = row;
	lastCol = col;
	if( mouseMotionActive )
		goto restoreCursor;

	/* determine the proper selection mode */
#ifdef OLDVERSION
	else if( selBegin <= cp && cp <= selEnd && w == selWindow 
			&& cp < fileSize(fid) ) {
		if( selMode++ == SELLINE )
			selMode = SELCHAR;
#else
	else if( clicks != SELCHAR ) {
		selMode = clicks;
#endif
		if( selMode == SELLINE ) {
			sprintf(msgBuffer,"Line %d",LineNumberOfSelection());
			msg( msgBuffer, 0 );
		}
		drawSelection( 1 );
		/* Extend the selection by the selection mode */
		oldBegin = selBegin;
		oldEnd = selEnd;
		modeExtend( selWindow, cp, row, col, beginRowCp );
		/* see if we need to erase the whole selection */
		/* this can happen if you click inside a large, */
		/* drawn-through selection within the double click */
		/* interval. */
		if( oldBegin < selBegin || selEnd < oldEnd )
			drawSelection( 1 );
		if( selMode != SELBLOCK ) {
			int row1, row2, col1, col2;
			Offset begin = selBegin;
			Offset end = selEnd;
			/* restrict begin-end range to what is */
			/* visible on the screen */
			if( begin < selWindow->posTopline )
				begin = selWindow->posTopline;
			if( end >= selWindow->posBotline )
				end = selWindow->posBotline - 1;
			if( begin <= end ) {
				/* at least part of the selection */
				/* is on the screen */
				int n = -1;
				Offset cp = prevLine(selWindow->fileId,
						begin, &n);
				OffsetToXY( selWindow, begin, &row1,
						&col1 );
				OffsetToXY( selWindow, end, &row2,
						&col2 );
				DrawSection( selWindow, cp, row1, col1,
						row2, col2 );
			}
		} else {
			drawWindow( selWindow );
		}

	} else {
		drawSelection( 1 );
		selEnd = selBegin = cp;
		selWindow = w;
		selMode = SELCHAR;
		DrawSection(selWindow, beginRowCp, row, col, row, col);
	}
	AssertSelectionOwnership();

} else if( cmd_uid == BeginExtend ) {
	ExtendSelection( cp, row, col, beginRowCp, anchorCp );

} else if( cmd_uid == ExtendSel ) {
	if( row == lastRow && col == lastCol )
		return;
	lastRow = row;
	lastCol = col;

	/* extend or contract the selection. */
	ExtendSelection( cp, row, col, beginRowCp, anchorCp );

} else if( cmd_uid == EndExtending )
	/*EMPTY*/
{
	/* nothing to do on end selection */

} else if( cmd_uid == BeginMouseMenu1 ) {
	mm_data = mm1Data;
	goto BeginEitherMenu;

} else if( cmd_uid == BeginMouseMenu2 ) {
	mm_data = mm2Data;

BeginEitherMenu:
	/* remember the important parameters */
	mm_x = x;
	mm_y = y;
	mm_row = row;
	mm_col = col;
	mm_cp = cp;
	mouseMotionActive = 1;
	currentDirection = MM_NOMOTION;
	/* record all the label lengths */
	for( n = 0; n < 5; ++n )
		mm_length[n] = strlen(mm_data[n].label);

	if( mouseSpriteMenu ) {
		XDefineCursor( MainDisplay, Tk_WindowId(w->tk_toplevel),
				mm_data[currentDirection].cursor );
	} else {
		intervalOn = 1;
		mouse_w = w;
		timer_token = Tk_CreateTimerHandler( menuDelay, mmTimeout, 0 );
	}

} else if( cmd_uid == ContinueMouseMenu ) {
	int newDirection;

	x -= mm_x;
	y -= mm_y;
	/* make it insensitive to small changes */
	if( abs(x) < menuTolerance && abs(y) < menuTolerance )
		newDirection = MM_NOMOTION;
	else if( x > y ) {
		if( x > -y )
			newDirection = MM_EAST;
		else
			newDirection = MM_NORTH;
	} else {
		if( x > -y )
			newDirection = MM_SOUTH;
		else
			newDirection = MM_WEST;
	}
	if( mouseMotionActive && (currentDirection != newDirection) ) {
		if( mouseSpriteMenu ) {
			XDefineCursor( MainDisplay, Tk_WindowId(w->tk_toplevel),
					mm_data[newDirection].cursor );
		} else if( !intervalOn ) {
			/* unhighlight the old item */
			gc = font->gc_normal;
			DrawMenuString( w, currentDirection );
			/* highlight the new item */
			gc = font->gc_selected;
			DrawMenuString( w, newDirection );
		}
	}
	currentDirection = newDirection;

} else if( cmd_uid == EndMouseMenu ) {
	if( mouseMotionActive ) {
		int row1, row2;
		char * cmd = mm_data[currentDirection].tcl_command;
		if( striccmp(cmd,"ExtendSelection") != 0 ) {
			(void)ExecTclCommand( cmd, NULL );
		} else
			/* extend or contract the selection */
			ExtendSelection( cp, row, col, beginRowCp, anchorCp );
restoreCursor:
		if( mouseSpriteMenu ) {
			XDefineCursor( MainDisplay, Tk_WindowId(w->tk_toplevel),
							currentCursor);
		} else {
			/* redraw the text we wrote over */
			if( intervalOn ) {
				intervalOn = 0;
				Tk_DeleteTimerHandler( timer_token );
			} else {
				if( (row1 = mm_row - 2) < 0 )
					row1 = 0;
				row2 = mm_row + 2;
				drawWindowFast( w, row1, row2, 0, w->nCols, 0 );
			}
		}
		mouseMotionActive = 0;
	}
} else if( cmd_uid == CancelMouseMenu ) {
	goto restoreCursor;
} else {	/* must be a Point command */
	printf("Mouse command `%s' not understood\n", cmd );
}
}

static void
DrawInitialMenu( w )
        struct window * w;
{
	extern Display *MainDisplay;

	int col9;
	/* two abbreviations */
	struct fontDataStruct *font = &(w->font);

	/* write out the circular menu items */

	/* first figure out where everything goes */
	mm_origin_y[MM_NOMOTION] = w->topMargin + font->ascent
						+ mm_row * font->height;
	mm_origin_y[MM_WEST] = mm_origin_y[MM_NOMOTION];
	mm_origin_y[MM_EAST] = mm_origin_y[MM_NOMOTION];
	mm_origin_y[MM_NORTH] = mm_origin_y[MM_NOMOTION] - font->height - 2;
	mm_origin_y[MM_SOUTH] = mm_origin_y[MM_NOMOTION] + font->height + 2;

	col9 = mm_col - mm_length[MM_NOMOTION]/2;
	mm_origin_x[MM_NOMOTION] = w->leftMargin + col9 * font->width;
	mm_origin_x[MM_WEST] = mm_origin_x[MM_NOMOTION]
				- mm_length[MM_WEST] * font->width - 2;
	mm_origin_x[MM_EAST] = mm_origin_x[MM_NOMOTION]
				+ mm_length[MM_NOMOTION] * font->width + 2;
	col9 = mm_col - mm_length[MM_NORTH]/2;
	mm_origin_x[MM_NORTH] = w->leftMargin + col9 * font->width;
	col9 = mm_col - mm_length[MM_SOUTH]/2;
	mm_origin_x[MM_SOUTH] = w->leftMargin + col9 * font->width;

	/* draw rectangles around the strings */
	gc = (w->font).gc_normal;
	XDrawRectangle(MainDisplay, w->x_window_id, gc,
		mm_origin_x[MM_NOMOTION] - 1,
		mm_origin_y[MM_NOMOTION] - font->ascent - 1,
		font->width * mm_length[MM_NOMOTION] + 2,
		font->height + 2);
	XDrawRectangle(MainDisplay, w->x_window_id, gc,
		mm_origin_x[MM_NORTH] - 1,
		mm_origin_y[MM_NORTH] - font->ascent - 1,
		font->width * mm_length[MM_NORTH] + 2,
		font->height + 2);
	XDrawRectangle(MainDisplay, w->x_window_id, gc,
		mm_origin_x[MM_EAST] - 1,
		mm_origin_y[MM_EAST] - font->ascent - 1,
		font->width * mm_length[MM_EAST] + 2,
		font->height + 2);
	XDrawRectangle(MainDisplay, w->x_window_id, gc,
		mm_origin_x[MM_SOUTH] - 1,
		mm_origin_y[MM_SOUTH] - font->ascent - 1,
		font->width * mm_length[MM_SOUTH] + 2,
		font->height + 2);
	XDrawRectangle(MainDisplay, w->x_window_id, gc,
		mm_origin_x[MM_WEST] - 1,
		mm_origin_y[MM_WEST] - font->ascent - 1,
		font->width * mm_length[MM_WEST] + 2,
		font->height + 2);
	/* now write the strings */
	gc = (w->font).gc_normal;
	DrawMenuString( w, MM_NOMOTION );
	DrawMenuString( w, MM_NORTH );
	DrawMenuString( w, MM_WEST );
	DrawMenuString( w, MM_SOUTH );
	DrawMenuString( w, MM_EAST );
	/* this one will be written twice */
	gc = (w->font).gc_selected;
	DrawMenuString( w, currentDirection );
}

/* global variables for making cursor from pixmaps */
static Pixmap pixmap;
static GC mouse_gc;
static XColor black, white;
static int ascent;
extern Display *MainDisplay;

static void
MakeCursors( mm_data )
	struct mmData * mm_data;
{

	int i;

	for( i = 0; i < 5; ++i ) {
		mm_data[i].length = strlen( mm_data[i].label );
		if( mm_data[i].length > 0 ) {
			XDrawImageString( MainDisplay, pixmap, mouse_gc, 2,
				ascent+2, mm_data[i].label, mm_data[i].length );
			mm_data[i].cursor = XCreatePixmapCursor( MainDisplay,
				pixmap, None, &white, &black, 1, 1 );
		} else
			mm_data[i].cursor = (Cursor)NULL;
	}
}

void
MakeMouseMenuCursors()
{
	extern Pixel ptWhitePixel, ptBlackPixel;
	extern struct mmData mm1Data[];
	extern struct mmData mm2Data[];
	extern char * mouseMenuFont;

	XFontStruct *fontInfo;

	/* set up the pixmap and stuff to create the cursors */
	pixmap = XCreatePixmap( MainDisplay, DefaultRootWindow(MainDisplay),
			28, 17, 1 );
	fontInfo = XLoadQueryFont( MainDisplay, mouseMenuFont );
	if( fontInfo == NULL )
		/* we assume every X server has a "fixed" font */
		fontInfo = XLoadQueryFont( MainDisplay, "fixed" );
	ascent = fontInfo->ascent;
	mouse_gc = XCreateGC(MainDisplay, pixmap, 0, NULL);
	XSetFont(MainDisplay, mouse_gc, fontInfo->fid);

	/* clear out the edges of the pixmap */
	XFillRectangle( MainDisplay, pixmap, mouse_gc, 0, 0, 28, 17 );
	black.pixel = ptBlackPixel;
	ptBlackPixel = BlackPixel(MainDisplay, MainDisplay->default_screen);
	XQueryColor( MainDisplay, DefaultColormap(MainDisplay,
				DefaultScreen(MainDisplay)), &black);
	white.pixel = ptWhitePixel;
	XQueryColor( MainDisplay, DefaultColormap(MainDisplay,
				DefaultScreen(MainDisplay)), &white);

	MakeCursors( mm1Data );
	MakeCursors( mm2Data ); 

	XFreeGC( MainDisplay, mouse_gc );
	XFreePixmap( MainDisplay, pixmap );
}

