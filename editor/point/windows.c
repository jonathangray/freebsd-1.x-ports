/* $Header: /a/cvs/386BSD/ports/editor/point/windows.c,v 1.1 1994/02/15 22:12:41 jkh Exp $ */

#include <ctype.h>
#include <string.h>
#include "pt.h"
#include <stdio.h>
#include <sys/file.h>
#include <signal.h>
#ifdef uts
#include <fcntl.h>
#endif

/* the list of active windows, top to bottom */
struct window *windowList = NULL;

/* the active window */
struct window *activeWindow = NULL;

extern struct openFile *files;

void
initWindows()
{
	extern char charTable[];

	charTable['\n'] = 1;
	charTable[BLOCK_EOF] = 2;
	charTable['\t'] = 3;
}

void
MakeWindowActive( w )
	struct window *w;
{
	extern struct window *activeWindow;

	if( w != activeWindow ) {
		/* remember the old active window */
		struct window *oldw = activeWindow;

		activeWindow = w;
		banner( oldw, 0 );
		banner( w, 0 );
	}
}

struct window *
createWindow( w, fileName, geometry )
	struct window * w;
	char * fileName;
	char * geometry;
{
	extern struct window *selWindow;
	extern Offset selBegin, selEnd;
	extern char msgBuffer[];
	extern int autoZoom;
	extern char * textFont;
	extern Display *MainDisplay;
#ifdef HYPERTEXT
	extern DBM *currentDB;
	extern Document currentDocument;
#endif
	extern Tcl_Interp * pointsMainInterp;
	extern BrowserData * mainBrowser;
	extern Window MainWindow;

	int createToo = (w == NULL);
	char * name;

	if( createToo ) {	/* we need to create the window as well as */
				/* load the new file into it */
		w = (struct window *) PtMalloc( sizeof(struct window),
						"Window structure" );
	}

	w->fileId = getFileId(fileName);
	w->realFileId = w->fileId;
	if( w->fileId == -1 ) {
		/* file not found, do not open the window */
		printf( "File %s not found\n", fileName );
		return NULL;
	}
#ifdef HYPERTEXT
	if( hypertextOn && getFileByte(w->fileId,0) == (int)beginMarkerChar ) {
		/* This is a hypertext file */
		extern MapID naturalMap;
		FileID fileID;
		unsigned int flags;
		BlockID blockID;

		w->db = currentDB;
		w->document = currentDocument;
		fileID = LookupFileByName( w->db, w->document,
						files[w->fileId].origName );
		if( fileID == NullObject ) {
			printf(
"ERROR: could not find file %s. Displaying as an ordinary file\n",
				files[w->fileId].origName );
			goto ordinary_file;
		}
		w->file = GetFile( w->db, fileID, ALLOCATE );

		/* get the inital view */
		w->view = NULL;	/* not using views yet */

		/* get the maps from the initial view */
		w->blockMap = GetMap( w->db, naturalMap, ALLOCATE);
		if( w->blockMap == NULL ) {
			printf( "Cannot find natural map in database %s\n",
								fileName );
			return NULL;
		}
		/* Maybe later we can fetch these */
		w->fromLinkMap = NULL;
		w->toLinkMap = NULL;

		/* get the initial block of the initial view */
		(void)ReadBlockMarker( w->fileId, 1, &blockID, &flags );
		w->block = GetBlock( w->db, blockID, ALLOCATE);
		if( w->block == NULL ) {
			printf( "Cannot find initial block in database %s\n",
								fileName );
			return NULL;
		}

		/* Split up the file so that all block markers are in */
		/* separate pieces.  Other code depends on this to be true. */
		SeparateBlockMarkers( w );

		/* fileId already copied to realFileId (above) */
		w->fileId = CreateViewFile( w );
	} else {
	ordinary_file:
		/* This is an ordinary file */
		w->db = NULL;
		w->document = NULL;
		w->view = NULL;
		w->block = NULL;
		w->blockMap = NULL;
		w->fromLinkMap = NULL;
		w->toLinkMap = NULL;
		w->file = NULL;
	}
#endif

	w->posTopline = 0;

	if( createToo ) {
		/* insert w into the doubly linked windowList */
		if( windowList == NULL ) {
			w->prevWindow = NULL;
			w->nextWindow = NULL;
			windowList = w;
		} else {
			windowList->prevWindow = w;
			w->prevWindow = NULL;
			w->nextWindow = windowList;
			windowList = w;
		}
	}

	w->nameOffset = getBaseName(files[w->fileId].origName);

	/* set the default window parameters */
	w->posBotline = 0;
	w->numTopline = 1;
	w->numBotline = -1;
	w->indent = 0;
	
	/* set up the last row cache */
	w->posCurLast = 0;
	w->lastPosTop = 0;
	w->rowCurLast = 0;

	/* set up the margins */
	w->topMargin = 2;
	w->leftMargin = 4;

	/* set up the font name */
	w->font.name = PtMalloc(strlen(textFont)+1, "font name");
	strcpy(w->font.name, textFont);
	/* height = 0 will ensure that the old font will not be reused */
	(w->font).height = 0;
	
	/* create the three GCs needed by each window */
	(w->font).gc_normal = XCreateGC(MainDisplay, MainWindow,
					0, NULL);
	(w->font).gc_selected = XCreateGC(MainDisplay, MainWindow,
					0, NULL);
	(w->font).gc_deselected = XCreateGC(MainDisplay, MainWindow,
					0, NULL);
	(w->font).gc_underline = XCreateGC(MainDisplay, MainWindow,
					0, NULL);

	/* other setups */
	w->x_window_id = (Window)NULL;
	w->oldWidth = 0;
	w->closeInform = NULL;
	w->lineNumbers = 0;
	w->hasMsgLine = 1;
	w->isLogWindow = 0;
	
	/* if there is a file in the window and there is not current */
	/* selection, then move the selection to the new window */
	if( w->fileId != -1 && selWindow == NULL ) {
		/* put the selection at the first char in this window */
		selWindow = w;
		selBegin = 0;
		selEnd = 0;
	}

	if( createToo ) {
		char buffer[100];
		Tk_Window ln_w;
		w->screen_image = NULL;
		/* this will cause a call to WorkspaceResized on the */
		/* first expose event */
		w->nRows = 0;
		/*  create a text window */
		(void)ExecTclCommand( "update", NULL );
		sprintf( buffer, "TextWindow %s", geometry );
		name = ExecTclCommand( buffer, NULL );
		/* window creation failed for some reason, so quit */
		if( name==NULL || name[0]=='\0')
			return NULL;
		w->tk_pathname = Tk_GetUid( name );
		w->tk_toplevel = Tk_NameToWindow( pointsMainInterp,
				w->tk_pathname, mainBrowser->tk_toplevel );
		sprintf( msgBuffer, "%s.vScrollAndText.text", w->tk_pathname );
		w->tk_text = Tk_NameToWindow( pointsMainInterp, msgBuffer,
							w->tk_toplevel);
		w->x_window_id = Tk_WindowId( w->tk_text );
		sprintf( msgBuffer, "%s.vScrollAndText.lineNumbers",
							w->tk_pathname );
		ln_w = Tk_NameToWindow( pointsMainInterp, msgBuffer,
							w->tk_toplevel);
		w->ln_window_id = Tk_WindowId( ln_w );

		(void)ExecTclCommand( "update", NULL );

		/* zoom of necessary */
		if( autoZoom )
			ZoomWindow( w, 0 );
	}

	/* make the new window the active window */
	MakeWindowActive( w );

	/* update the list of open windows */
	NewOpenList();

	return w;
}

int
closeWindow(w, ask)
	register struct window *w;
	int ask;
{
	extern struct window *selWindow;
	extern struct window *activeWindow;
	extern Offset selBegin, selEnd;
	extern char msgBuffer[];

	struct window * w_prev, * w_next;

	/* first destroy the widget tree (the shell is the top) */
	
#ifdef HYPERTEXT
	/* free the objects and close the database */
	if( w->db != NULL ) {
		PutFile( w->db, w->file, RELEASE );
		PutBlock( w->db, w->block, RELEASE );
		PutMap( w->db, w->blockMap, RELEASE );
		PutView( w->db, w->view, RELEASE );
		PutDocument( w->db, w->document, RELEASE );
	}
#endif

	if( closeFile(w->fileId, ask) == -1 )
		return -1;

	/* handle log windows which have an open pty and a child process */
	if( w->isLogWindow ) {
		Tk_DeleteFileHandler( selWindow->toShellFD );
		close( selWindow->toShellFD );
		w->isLogWindow = 0;		/* just to be careful */
		/* Send the child process a software termination signal. */
		/* If it has already exited, we will get an error return */
		/* code but it is easier to just send the signal than to */
		/* check whether it is still alive. */
		(void)kill( selWindow->childPID, SIGTERM );
	}

	/* unlink this window from the window list */
	w_prev = w->prevWindow;
	w_next = w->nextWindow;
	if( w_prev == NULL )
		windowList = w_next;
	else
		w_prev->nextWindow = w_next;
	if( w_next != NULL )
		w_next->prevWindow = w_prev;
	if( windowList == NULL )
		activeWindow = NULL;

	/* is the selection in this window? */
	if( w == selWindow ) {
		/* move the selection to the top window */
		selWindow = windowList;
		if( selWindow != NULL ) {
			selBegin = 0;
			selEnd = selBegin;
		}
	}

	/* is this the active window? */
	if( w == activeWindow ) {
		MakeWindowActive( selWindow );
	}

	(void)ExecTclCommand( "update", NULL );
	Tk_DestroyWindow( w->tk_toplevel );
	(void)ExecTclCommand( "update", NULL );

	/* free any allocated strings */
	if( w->closeInform != NULL ) {
		sprintf( msgBuffer, "catch {send %s %s}", w->closeInform,
							w->tk_pathname );
		(void)ExecTclCommand( msgBuffer, NULL );
		PtFree( w->closeInform );
	}

	PtFree( w->font.name );

	PtFree( w->screen_image );
	
	PtFree( (char *)w );

	/* update the list of open windows */
	NewOpenList();

	return 0;
}

static int timerIsOn = 0;
static Tk_TimerToken timer_token = NULL;
int intervalRows = 0;
int scrollDown = 0;
struct window * scroll_window;

static void
DoOneHScroll()
{
	extern Display *MainDisplay;

	struct fontDataStruct *font = &( scroll_window->font);
	int col1, col2, incr1, incr2, cols, wide;

#ifdef LATERLATER
	if( undoMotion ) {
		/* record in the change history */
		thisChange = GetNewChange( ff );
		thisChange->type = CMOTION;
		thisChange->lineNumber = scroll_window->numTopline;
		thisChange->length = scroll_window->numTopline + intervalRows;
		thisChange->w = scroll_window;
		thisChange->flags = 0;
		RecordChange( ff, thisChange );
	}
#endif
	cols = intervalRows;
	wide = font->width;
	if( scrollDown ) {
		scroll_window->indent += cols;
		incr1 = cols * wide;
		incr2 = 0;
		col1 = (scroll_window->nCols) - cols;
		col2 = scroll_window->nCols - 1;
	} else {
		/* do not allow scrolling off the beginning */
		if( cols > scroll_window->indent )
			cols = scroll_window->indent;
		scroll_window->indent -= cols;
		incr1 = 0;
		incr2 = cols * wide;
		col1 = 0;
		col2 = cols - 1;
	}
	XCopyArea( MainDisplay, scroll_window->x_window_id,
		scroll_window->x_window_id, font->gc_normal,
		scroll_window->leftMargin + incr1, 0,
		(scroll_window->nCols - cols) * wide,
		Tk_Height(scroll_window->tk_toplevel),
		scroll_window->leftMargin + incr2, 0 );
	drawWindowFast( scroll_window, 0, scroll_window->nRows-1, col1, col2,
									1 );
}

/*ARGSUSED*/
static void
repeatHScroll( clientData )
	ClientData clientData;
{
	DoOneHScroll();
	timer_token = Tk_CreateTimerHandler( 100, repeatHScroll, 0 );
}

void
HScroll( w, how, x, button )
	struct window *w;
	int how;
	int x;
	int button;
{
	int top_unit;

	/* how = 0 ==> tk scrolling */
	/* how = 1 ==> button press */
	/* how = 2 ==> button release */
	/* how = 3 ==> button motion */

	scroll_window = w;
	switch( how ) {
	case 0:		/* Tk scrolling */
		top_unit = x;
		intervalRows = top_unit - (w->indent);
		break;
	case 1:		/* button press */
		intervalRows = (x - w->leftMargin) / (w->font).width;
		switch( button ) {
		case 1:
			scrollDown = 0;
			break;
		case 2:
			goto thumbing;
		case 3:
			scrollDown = 1;
			break;
		}
		if( !timerIsOn ) {
			DoOneHScroll();
			timer_token = Tk_CreateTimerHandler( 500,
							repeatHScroll, 0 );
			timerIsOn = 1;
		}
		break;
	case 2:		/* (left or right) button release */
		if( timerIsOn ) {
			timerIsOn = 0;
			Tk_DeleteTimerHandler( timer_token );
		}
		return;
	case 3:		/* (middle) button motion */
	thumbing:
		intervalRows = w->indent
				- (x - w->leftMargin) / (w->font).width;
		DoOneHScroll();
		break;
	}

	/* always scroll at least one column */
	if( intervalRows < 1 )
		intervalRows = 1;
}

int
DoOneVScroll()
{
	extern int undoMotion;
	extern Display *MainDisplay;
	extern Offset selBegin, selEnd;
	extern int keepSelectionVisible;
  
	Offset newSel;
	int selChanged = 0;
	struct fontDataStruct *font = &(scroll_window->font);
	int fid = scroll_window->fileId;
	struct changeItem *thisChange;
	int rowsScrolled, high, incr1, incr2, row1, row2, rows_up;
	struct openFile * ff = &files[fid];
	int copy_height;

	if( undoMotion ) {
		/* record in the change history */
		thisChange = GetNewChange( ff );
		thisChange->type = CMOTION;
		thisChange->lineNumber = scroll_window->numTopline;
		thisChange->length = scroll_window->numTopline + intervalRows;
		thisChange->w = scroll_window;
		thisChange->flags = 0;
		RecordChange( ff, thisChange );
	}

	rowsScrolled = intervalRows;
	high = font->height;
	copy_height = (scroll_window->nRows - intervalRows) * high;
	if( scrollDown ) {
		scroll_window->posTopline = nextLine( fid,
				scroll_window->posTopline, &rowsScrolled );
		if( keepSelectionVisible ) {
			newSel = scroll_window->posTopline;
			if( newSel > selBegin ) {
				drawSelection(1);
				selChanged = 1;
				selBegin = newSel;
				selEnd = newSel;
			}
		}
		scroll_window->numTopline += rowsScrolled;
		scroll_window->numBotline += rowsScrolled;
		incr1 = rowsScrolled * high;
		incr2 = 0;
		row1 = scroll_window->nRows - rowsScrolled;
		if( rowsScrolled < intervalRows )
			row1 = rowsScrolled + 1;
		row2 = scroll_window->nRows-1;
	} else {
		/* do not allow scrolling off the beginning */
		if( scroll_window->posTopline <= 0 )
			return 0;
		scroll_window->posTopline = prevLine( fid,
				scroll_window->posTopline, &rowsScrolled);
		if( keepSelectionVisible ) {
			rows_up = scroll_window->nRows-1;
			newSel = nextLine( fid, scroll_window->posTopline, &rows_up );
			if( newSel < selBegin ) {
				drawSelection(1);
				selChanged = 1;
				selBegin = newSel;
				selEnd = newSel;
			}
		}
		scroll_window->numTopline -= rowsScrolled;
		scroll_window->numBotline -= rowsScrolled;
		row1 = intervalRows - rowsScrolled;
		if( row1 > 0 )
			copy_height += row1 * high;
		incr1 = 0;
		incr2 = rowsScrolled * high;
		row1 = 0;
		row2 = rowsScrolled - 1;
	}
	/* only copy if we can use part of the present window */
	if( intervalRows < scroll_window->nRows ) {
		XCopyArea( MainDisplay, scroll_window->x_window_id,
			scroll_window->x_window_id, font->gc_normal,
			0, scroll_window->topMargin + incr1,
			Tk_Width(scroll_window->tk_toplevel), copy_height,
			0, scroll_window->topMargin + incr2 );
		if( scrollDown && scroll_window->posBotline
					== fileSize(scroll_window->fileId) ) {
			XClearArea( MainDisplay, scroll_window->x_window_id,
				0, copy_height,
				Tk_Width(scroll_window->tk_text),
				Tk_Height(scroll_window->tk_text)-copy_height,
				False
			);
			/*****return rowsScrolled;*****/
		}
	} else {
		/* we went through the above calculations in order to get */
		/* posTopline, numTopline and numBotline updated */
		/* now fix up row1 and row2 */
		row1 = 0;
		row2 = scroll_window->nRows - 1;
	}
	drawWindowFast( scroll_window,row1,row2,0,scroll_window->nCols-1,1 );
	if( selChanged )
		drawSelection(0);
	return rowsScrolled;
}

/*ARGSUSED*/
static void
repeatVScroll( clientData )
	ClientData clientData;
{
	(void)DoOneVScroll();
	timer_token = Tk_CreateTimerHandler( 100, repeatVScroll, 0 );
}

void
VScroll( w, how, y, button )
	struct window *w;
	int how;
	int y;
	int button;
{
	/* how = 0 ==> tk scrolling */
	/* how = 1 ==> button press */
	/* how = 2 ==> button release */
	/* how = 3 ==> button motion */
	extern int button1ScrollsDown;

	int row1, row;
	int fid = w->fileId;
	Offset cp, offset;
	int top_unit;
	int delta, inwindow;
	int scrollbar_height;

	scroll_window = w;
	switch( how ) {
	case 0:		/* Tk scrolling */
		top_unit = y;
		inwindow = w->posBotline - w->posTopline;
		if( inwindow < 1 )
			inwindow = 1;
		delta = top_unit - w->posTopline;
		if( delta < 0 ) {
			delta = -delta;
			scrollDown = 0;
		} else
			scrollDown = 1;
		intervalRows = (w->nRows * delta) / inwindow;
		if( intervalRows < 1 )
			intervalRows = 1;
		(void)DoOneVScroll();
		break;
	case 1:		/* button press */
		intervalRows = (y - w->topMargin) / (w->font).height;
		/* always scroll at least one line */
		if( intervalRows < 1 )
			intervalRows = 1;
		switch( button ) {
		case 1:
			scrollDown = button1ScrollsDown;
			break;
		case 2:
			goto thumbing;
		case 3:
			scrollDown = 1 - button1ScrollsDown;
			break;
		}
		if( !timerIsOn ) {
			(void)DoOneVScroll();
			timer_token = Tk_CreateTimerHandler( 300,
							repeatVScroll, 0 );
			timerIsOn = 1;
		}
		break;
	case 2:		/* (left or right) button release */
		if( timerIsOn ) {
			timerIsOn = 0;
			Tk_DeleteTimerHandler( timer_token );
		}
		return;
	case 3:		/* (middle) button motion */
	thumbing:
		/* we get negative y values when the (grabbed) mouse goes */
		/* above the scroll bar area.  Consider them zero. */
		/* Also adjust for the case where the mouse pointer */
		/* moves below the scrollbar area. */
		scrollbar_height = Tk_Height(w->tk_text);
		if( y < 0 )
			y = 0;
		else if( y > scrollbar_height )
			y = scrollbar_height;
		cp = fileSize( fid );
		offset = (int)( ((double)y * cp) / scrollbar_height );
		if( offset > cp )
			offset = cp;
		cp = w->posTopline;
		row = w->numTopline;
		while( cp < offset ) {
			row1 = 1;
			cp = nextLine( fid, cp, &row1 );
			row += row1;
		}
		while( cp > offset ) {
			row1 = 1;
			cp = prevLine( fid, cp, &row1 );
			row -= row1;
		}
		intervalRows = row - w->numTopline;
		if( intervalRows == 0 )
			return;
		if( intervalRows < 0 ) {
			intervalRows = -intervalRows;
			scrollDown = 0;
		} else
			scrollDown = 1;
		(void)DoOneVScroll();
		break;
	}
}


void
topWindow(w)
	register struct window *w;
{
	MakeWindowActive( w );
}

void
ZoomWindow( w, how )
	struct window *w;
	int how;
{
	extern int display_height;
	extern int display_width;

	if( w->oldWidth == 0 ) {	/* not zoomed */
		/* first save the present geometry */
		w->oldX = Tk_X( w->tk_toplevel );
		w->oldY = Tk_Y( w->tk_toplevel );
		w->oldWidth = Tk_Width( w->tk_toplevel );
		w->oldHeight = Tk_Height( w->tk_toplevel );

		/* now set up to zoomed geometry */
		Tk_MoveResizeWindow( w->tk_toplevel,
			(how ? 4 : w->oldX),
			3,
			(how ? display_width-8 : w->oldWidth),
			display_height-37
		);
	} else {	/* unzoom */
		/* restore the old geometry */
		Tk_MoveResizeWindow( w->tk_toplevel,
			w->oldX, w->oldY-25, w->oldWidth, w->oldHeight );
		/* set up so that it indicates not zoomed */
		w->oldWidth = 0;
	}
}

struct window *
GetNewFile(w, fileName, geometry, doNotAsk )
	struct window * w;
	char * fileName;
	char * geometry;
	int doNotAsk;
{
	extern char msgBuffer[];
	extern char textBuffer[];

	int n;

	if( fileName == NULL ) {
cancelWindow:
		msg("New file cancelled", 0);
		return NULL;
	}
	fileName = noWhiteSpace(fileName);
	strcpy(textBuffer, fileName);
	if( access(textBuffer, 0) == -1 ) {
		if( !doNotAsk ) {
			char * ret;
			sprintf( msgBuffer,
			  "MakeModalYesNo \"%s\" \"%s %s %s\" \"%s\" \"%s\"",
				"Create file?",
				"File", fileName, "does not exist.",
				"Create it",
				"Cancel new file" );
			ret = ExecTclCommand( msgBuffer, NULL );
			if( ret[0] != 'y' ) {
				goto cancelWindow;
			}
		}
		n = open(textBuffer, O_CREAT, 0644);
		if( n < 0 ) {
			sprintf(msgBuffer, "Cannot create %s: ", textBuffer);
			msg(msgBuffer, 1 );
			goto cancelWindow;
		} else
			close(n);
	}
	return createWindow( w, textBuffer, geometry );
}


struct window *
FindWindowByTkName( name )
	char * name;
{
	extern struct window * windowList;

	struct window * w = windowList;
	Tk_Uid uid_of_name = Tk_GetUid( name );

	while( w != NULL ) {
		if( w->tk_pathname == uid_of_name  )
			break;
		w = w->nextWindow;
	}
	return w;
}

void
bottomFile( w )
	struct window *w;
{
	Offset cp;
	int j;
	int i;
	int fid = w->fileId;

	if( w == NULL )
		return;
	/* remember where we came from */
	w->rowLastline = w->numTopline;
	cp = w->posBotline;

	/* find the last line of the file */
	i = 0;
	while( 1 ) {
		j = 1;
		cp = nextLine( fid, cp, &j );
		/* if j==0, we could not go down a line */
		/* so we are at the end */
		if( (cp < 0) || (j == 0) )
			break;
		++i;
	}
	if( i!=0 )
		++i;   /* one more line so EOF mark shows */

	/* now move the window down and redraw it */
	j = i;	/* since i is a register variable, we must use j here */
	w->posTopline = nextLine( fid, w->posTopline, &j );
	w->posBotline = cp;
	w->numTopline += j;
	w->numBotline += j;
	w->indent = 0;
        drawWindow(w);
}

