/* $Header: /a/cvs/386BSD/ports/editor/point/command.c,v 1.1 1994/02/15 22:12:36 jkh Exp $ */

#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include "pt.h"
#include <X11/keysym.h>
#include <X11/StringDefs.h>

/* count commands for autosaving */
static int commands_until_autosave = -1;

/* remember whether to erase the description message */
static int lastOnTopline = 0;

/* remember the last command the user selected (for help) */
static int lastCommand = FDONOTHING;

/* some status (or mode) flags and move/copy pending data */
static int movePending = 0;
static int copyPending = 0;
static struct window *pendWindow = NULL;
static Offset pendPosition;

/* for multiple exposes, keep the maximum bounds */
static int minx = 999999, miny = 999999, maxx = 0, maxy = 0;

char *
command( fn, arg1, arg2, arg3, arg4, arg5, arg6 )
	PointCommand fn;
	char *arg1;
	char *arg2;
	char *arg3;
	char *arg4;
	char *arg5;
	char *arg6;
{
	extern char msgBuffer[];
	extern char textBuffer[];
	extern struct window *selWindow;
	extern Offset selBegin, selEnd;
	extern int selMode;
	extern int lastFn;
	extern struct openFile *files;
	extern struct window *windowList;
	extern long timeOfLastSave;
	extern Offset mm_cp;
	extern Cursor mainCursor;
	extern Cursor currentCursor;
	extern Cursor busyCursor;
	extern Cursor dupCursor;
	extern Display *MainDisplay;
	extern BrowserData *mainBrowser;
	extern BrowserData *activeBrowser;
	extern BrowserData *browserList;
	extern struct window * activeWindow;
#ifdef HYPERTEXT
	extern DBM *currentDB;
	extern Document currentDocument;
	extern int hypertextOn;
#endif
	extern int tkScrolling;
	extern Tcl_Interp * pointsMainInterp;
	extern char * textGeometry;
	extern char * textFont;
	extern char * browserFont;
	extern Tk_Window TkMainWindow;
	extern int scrollDown;
	extern int intervalRows;
	extern int linesOverFind;
	extern int autoSaveCount;
	extern struct window * scroll_window;

	int n, ret, row1, col1, col2;
	Offset cp;
	char ch;
	char *fileName, *str;
	struct window *w2;
	struct window *saveSelWindow;
	char * ret_string = NULL;
	int int1, int2, int3, int4, int5, int6;
	struct window * w = activeWindow;
	BrowserData * browser;
	struct changeItem * last_change;
	struct openFile * ff;
	Offset saveSelBegin, saveSelEnd;
	static int ErrorDisplayed;

/* since we are about to issue a command, we do not need to erase */
/* the command description on the top line (so don't) */
lastOnTopline = 0;

switch( fn ) {

/*********************** EVENT HANDLER COMMANDS ***************************/
case FBARRIER: {
	int x, y;
	struct fontDataStruct *font = &(w->font);
	(void)Tcl_GetInt( pointsMainInterp, arg1, &int1 );
	OffsetToXY( w, int1, &row1, &col1 );
	if( row1 < 0 ) {
		printf("offset %d is not in the window\n", row1);
		break;
	}
	x = w->leftMargin + col1 * font->width;
	y = w->topMargin + (row1+1) * font->height - 2;
	XDrawLine( MainDisplay, w->x_window_id, font->gc_normal,
		x, y, x-3, y+3 );
	XDrawLine( MainDisplay, w->x_window_id, font->gc_normal,
		x, y-1, x-3, y+2 );
	XDrawLine( MainDisplay, w->x_window_id, font->gc_normal,
		x, y, x+3, y+3 );
	XDrawLine( MainDisplay, w->x_window_id, font->gc_normal,
		x, y-1, x+3, y+2 );
	break;
}

case FCONFIGURE:
	w = FindWindowByTkName( arg1 );
	if( w == NULL ) {
		browser = FindBrowserByTkName( arg1 );
		if( browser == NULL ) {
			printf("FCONFIGURE: window %s was not found\n", arg1);
			break;
		}
		NewFilelist( browser );
		break;
	}
	WorkspaceResized( w );
	break;

case FENTERTEXT:
	w2 = FindWindowByTkName( arg1 );
	if( w2 == NULL ) {
#ifdef DEBUG_UPDATE
		printf("FENTERTEXT: window %s not found\n", arg1);
#endif
		break;
	}
	if( w2 != activeWindow ) {
		struct window * oldActiveWindow = activeWindow;
		activeWindow = w2;
		Tk_DefineCursor( activeWindow->tk_text, currentCursor );
		SetWindowNames( oldActiveWindow );
		SetWindowNames( activeWindow );
	}
	break;

case FENTERBROWSER:
	browser = FindBrowserByTkName( arg1 );
	if( browser != activeBrowser ) {
		BrowserData * oldActiveBrowser = activeBrowser;
		activeBrowser = browser;
		chdir( activeBrowser->cwd );
		SetBrowserNames( oldActiveBrowser );
		SetBrowserNames( activeBrowser );
	}
	break;

case FEXPOSE:
	w = FindWindowByTkName( arg1 );
	if( w == NULL ) {
#ifdef DEBUG_UPDATE
		printf("FEXPOSE: window %s not found\n", arg1);
#endif
		break;
	}
	(void)Tcl_GetInt( pointsMainInterp, arg2, &int2 );
	if( int2 < minx )
		minx = int2;
	(void)Tcl_GetInt( pointsMainInterp, arg3, &int3 );
	if( int3 < miny )
		miny = int3;
	(void)Tcl_GetInt( pointsMainInterp, arg4, &int4 );
	int4 += int2;	/* get right x-coord */
	if( int4 > maxx )
		maxx = int4;
	(void)Tcl_GetInt( pointsMainInterp, arg5, &int5 );
	int5 += int3;	/* get bottom y-coord */
	if( int5 > maxy )
		maxy = int5;
	(void)Tcl_GetInt( pointsMainInterp, arg6, &int6 );
	if( int6 > 0 )	/* more expose events yet to come */
		break;
	repaint( w, minx, miny, maxx, maxy );
	maxx = maxy = 0;
	minx = miny = 999999;	/* bigger than any screen ? */
	break;

case FHSCROLL:
	if( w == NULL )
		break;
	if( striccmp(arg1,"press")==0 )
		n = 1;
	else if( striccmp(arg1,"release")==0 )
		n = 2;
	else if( striccmp(arg1,"motion")==0 )
		n = 3;
	else
		n = 0;
	if( n == tkScrolling )
		break;
	(void)Tcl_GetInt( pointsMainInterp, arg2, &int2 );
	if( n == 1 ) {
		(void)Tcl_GetInt( pointsMainInterp, arg3, &int3 );
	}
	HScroll( w, n, int2, int3 );
	break;

case FKEY:
	if( w == NULL ) {
		printf("FKEY: window is NULL\n");
		break;
	}
	(void)Tcl_GetInt( pointsMainInterp, arg1, &int1 );
	(void)Tcl_GetInt( pointsMainInterp, arg2, &int2 );
	HandleKey( int1, int2 );
	break;

case FMOUSE:
	w = FindWindowByTkName( arg1 );
	if( w == NULL ) {
		printf("FMOUSE: window %s not found\n", arg1);
		break;
	}
	(void)Tcl_GetInt( pointsMainInterp, arg3, &int3 );
	(void)Tcl_GetInt( pointsMainInterp, arg4, &int4 );
	(void)Tcl_GetInt( pointsMainInterp, arg5, &int5 );
	if( int5 < SELCHAR || int5 > SELLINE )
		int5 = SELCHAR;
	Mouse( w, arg2, int3, int4, int5 );
	break;

case FVSCROLL:
	if( w == NULL ) {
		printf("FVSCROLL: window is NULL\n");
		break;
	}
	if( striccmp(arg1,"press")==0 )
		n = 1;
	else if( striccmp(arg1,"release")==0 )
		n = 2;
	else if( striccmp(arg1,"motion")==0 )
		n = 3;
	else
		n = 0;
	if( tkScrolling ) {
		if( n != 0 )
			break;
		else
			int3 = 0;
	} else {
		if( n == 0 )
			break;
		else
			(void)Tcl_GetInt( pointsMainInterp, arg3, &int3 );
	}
	(void)Tcl_GetInt( pointsMainInterp, arg2, &int2 );
	VScroll( w, n, int2, int3 );
	break;

/*********************** MACRO COMMANDS ***************************/
case FWAITFORPROCESS:
	(void)Tcl_GetInt( pointsMainInterp, arg1, &int1 );
	waitpid( int1, NULL, 0 );
	break;

case FPOINTSELECTION:
if( striccmp(arg1,"set")==0 ) {
	if( arg2[0] != '\0' ) {
		(void)Tcl_GetInt( pointsMainInterp, arg2, &selBegin );
		if( selBegin < 0 )
			selBegin = 0;
	}
	if( arg3[0] != '\0' ) {
		(void)Tcl_GetInt( pointsMainInterp, arg3, &selEnd );
		cp = fileSize(selWindow->fileId);
		if( selEnd >= cp ) {
			selEnd = cp - 1;
		}
	}
	if( arg4[0] != '\0' ) {
		w = FindWindowByTkName( arg4 );
		if( w == NULL ) {
			printf("FPOINTSELECTION: window %s not found\n",
				arg4);
			break;
		}
		selWindow = w;
	}
	if( arg5[0] != '\0' ) {
		if( strcmp(arg5,"char")==0 )
			selMode = SELCHAR;
		else if( strcmp(arg5,"word")==0 )
			selMode = SELWORD;
		else
			selMode = SELLINE;
	}
} else if( arg1[0] == '\0' || striccmp(arg1,"get")==0 ){
	switch( selMode ) {
		case SELCHAR: str = "char"; break;
		case SELWORD: str = "word"; break;
		case SELLINE: str = "line"; break;
	}
	sprintf( textBuffer, "%d %d %s %s %d", selBegin, selEnd,
		selWindow==NULL ?
			"NoSelection" : selWindow->tk_pathname,
		str, LineNumberOfSelection() );
	ret_string = textBuffer;
	Tcl_SetResult( pointsMainInterp, ret_string, TCL_STATIC );
} else { 	/* must be a 'Sel return' */
	n = 256;
try_again:
	ret_string = (char *)PtMalloc( n, "selection" );
	ret = getSelection( ret_string, 0, n );
	if( !ret ) {	/* did we get the whole selection? */
		/* if not, double the buffer and try again */
		n *= 2;
		PtFree( ret_string );
		goto try_again;
	}
	/* do we need to escape things? */
	if( striccmp(arg1,"escaped")==0 ) {
		/* first count the number of braces */
		row1 = 0;
		str = ret_string;
		while( (ch = *str++) != '\0' ) {
			if( ch == '{' || ch == '}' )
				++row1;
		}
		if( row1 > 0 ) {
			/* allocate space for the escaped version */
			char * new_space = (char *)PtMalloc(
				row1 + (str - ret_string) + 1,
				"selection" );
			/* copy and escape */
			char * from = ret_string;
			char * to = new_space;
			while( 1 ) {
				ch = *from++;
				if( ch == '{' || ch == '}' )
					*to++ = '\\';
				*to++ = ch;
				if( ch == '\0' )
					break;
			}
			PtFree( ret_string );
			ret_string = new_space;
		}
	}
	Tcl_SetResult( pointsMainInterp, ret_string, (Tcl_FreeProc *)PtFree );
}
break;

case FGETFILECHARS:
	(void)Tcl_GetInt( pointsMainInterp, arg1, &int1 );
	(void)Tcl_GetInt( pointsMainInterp, arg2, &int2 );
	if( int1 < 0 )
		int1 = 0;
	if( int2 >= int1 + MSGBUFFERSIZE )
		int2 = MSGBUFFERSIZE - 1;
	if( arg3[0] != '\0' )
		w = FindWindowByTkName( arg3 );
	if( w == NULL ) {
		printf("FGETFILECHARS: window %s not found\n", arg3);
		break;
	}
	n = w->fileId;
	str = textBuffer;
	while( int1 <= int2 )
		*str++ = getFileByte( n, int1++ );
	ret_string = textBuffer;
	Tcl_SetResult( pointsMainInterp, ret_string, TCL_STATIC );
	break;

case FSCROLLWINDOW:
	if( arg3[0] != '\0' )
		w = FindWindowByTkName( arg3 );
	if( w == NULL ) {
		printf("FSCROLLWINDOW: window %s not found\n", arg3);
		break;
	}
	/* default is to scroll down one page */
	/* 'page' or a numerical count ? */
	if(arg2[0] == '\0' || striccmp(arg2,"page")==0 )
		intervalRows = w->nRows - 2;
	else
		intervalRows = atoi( arg2 );
	/* 'up' or 'down' ? */
	if( arg1[0] != '\0' && striccmp(arg1,"up")==0 )
		scrollDown = 0;
	else
		scrollDown = 1;
	scroll_window = w;
	(void)DoOneVScroll();
	break;

case FGETROWCOL:
	if( arg2[0] != '\0' )
		w = FindWindowByTkName( arg2 );
	if( w == NULL ) {
		printf("FGETROWCOL: window %s not found\n", arg2);
		break;
	}
	if( arg1[0] == '\0' )
		cp = selBegin;
	else
		cp = atoi( arg1 );
	OffsetToXY( w, cp, &col1, &col2 );
	sprintf( textBuffer, "%d %d", col1, col2 );
	ret_string = textBuffer;
	Tcl_SetResult( pointsMainInterp, ret_string, TCL_STATIC );
	break;

case FWINDOWNAME:
	if( strcmp(arg1,"set") == 0 ) {
		w = FindWindowByTkName( arg3 );
		if( w == NULL ) {
			printf("FWINDOWNAME: window %s not found\n", arg3);
			break;
		}
		if( strcmp(arg2,"active") == 0 )
			activeWindow = w;
		else
			selWindow = w;
	} else {
		if( strcmp(arg2,"active") == 0 )
			ret_string = activeWindow->tk_pathname;
		else
			ret_string = selWindow->tk_pathname;
		Tcl_SetResult( pointsMainInterp, ret_string, TCL_STATIC );
	}
	break;

case FGETWINDOWINFO:
	if( arg1[0] != '\0' )
		w = FindWindowByTkName( arg1 );
	if( w == NULL ) {
		printf("FGETWINDOWINFO: window %s not found\n", arg1);
		break;
	}
	sprintf( msgBuffer, "%d %d %d %d %d %d %d %d %d %d",
		w->posTopline, w->posBotline,
		w->numTopline, w->numBotline,
		w->indent, w->nRows, w->nCols,
		w->x_window_id, w->tk_toplevel, w->tk_text );
	ret_string = msgBuffer;
	Tcl_SetResult( pointsMainInterp, ret_string, TCL_STATIC );
	break;

case FGETFILEINFO: {
	struct openFile * ff;
	if( arg1[0] != '\0' )
		w = FindWindowByTkName( arg1 );
	if( w == NULL ) {
		printf("FGETFILEINFO: window %s not found\n", arg1);
		break;
	}
	ff = &files[w->fileId];
	sprintf( msgBuffer, "%s %d %d %d",
		ff->origName, ff->fileSize, ff->origFileSize, ff->flags );
	ret_string = msgBuffer;
	Tcl_SetResult( pointsMainInterp, ret_string, TCL_STATIC );
	break;
}

case FINSERTSTRING:
	if( selWindow == NULL )
		break;
	n = strlen( arg1 );
	if( arg2[0] != '\0' && strcmp(arg2,"noupdate")==0 )
		ret = 0;
	else
		ret = 1;
	if( n < 10 && ret ) {
		while( *arg1 != '\0' )
			insChar( *arg1++, ret, 0 );
	} else {
		while( *arg1 != '\0' )
			insertChar( *arg1++ );
		if( ret )
			drawWindow( selWindow );
	}
	break;

/**************************** SEARCH COMMANDS **************************/
case FSEARCHFORS:
	if( arg3[0] != '\0' )
		w = FindWindowByTkName( arg3 );
	if( w == NULL ) {
		printf("FSEARCHFORS: window %s not found\n", arg3);
		break;
	}
	if( arg2[0] == '\0' || striccmp(arg2,"forward")==0 )
		n = 0;
	else
		n = 1;
	if( arg4[0] == '\0' || strcmp(arg4,"update")==0 )
		col1 = UPDATEWINDOWS;
	else
		col1 = NOUPDATE;
	n = searchFor( w, n, arg1, col1, linesOverFind );
	sprintf( textBuffer, "%d", n );
	ret_string = textBuffer;
	Tcl_SetResult( pointsMainInterp, ret_string, TCL_STATIC );
	break;

case FREGEXSEARCH:
	if( arg3[0] != '\0' )
		w = FindWindowByTkName( arg3 );
	if( w == NULL ) {
		printf("FREGEXSEARCH: window %s not found\n", arg3);
		break;
	}
	if( arg2[0] == '\0' || striccmp(arg2,"forward")==0 )
		n = 0;
	else
		n = 1;
	if( arg4[0] == '\0' || strcmp(arg4,"update")==0 )
		col1 = UPDATEWINDOWS;
	else
		col1 = NOUPDATE;
	n = RegexSearch( w, n, arg1, col1, linesOverFind );
	sprintf( textBuffer, "%d", n );
	ret_string = textBuffer;
	Tcl_SetResult( pointsMainInterp, ret_string, TCL_STATIC );
	break;

case FCTAG:
	findCTag( arg1 );
	break;

case FREPEATSEARCH:
	if( arg2[0] != '\0' )
		w = FindWindowByTkName( arg2 );
	if( w == NULL ) {
		printf("FREPEATSEARCH: window %s not found\n", arg2);
		break;
	}
	(void)searchFor( w, (striccmp(arg1,"backward")==0), NULL, 1,
							linesOverFind );
	break;

case FREPLACE:
	if( arg4[0] != '\0' )
		w = FindWindowByTkName( arg4 );
	if( w == NULL ) {
		printf("FREPLACE: window %s not found\n", arg4);
		break;
	}
	if( arg3[0] == '\0' || striccmp(arg3,"inselection")!=0 )
		n = 0;
	else 
		n = 1;
	replaceText( w, arg1, arg2, n );
	break;

case FREGEXREPLACEONE:
	if( arg3[0] != '\0' )
		w = FindWindowByTkName( arg4 );
	if( w == NULL ) {
		printf("FREPLACE: window %s not found\n", arg4);
		break;
	}
	n = RegexReplaceOne( w, arg1, arg2 );
	sprintf( textBuffer, "%d", n );
	ret_string = textBuffer;
	Tcl_SetResult( pointsMainInterp, ret_string, TCL_STATIC );
	break;

case FREGEXREPLACEALL:
	if( arg3[0] == '\0' || striccmp(arg3,"inselection")!=0 )
		n = 0;
	else 
		n = 1;
	RegexReplaceAll( w, arg1, arg2, n );
	break;

case FMATCHCHAR:	/* find the matching character */
	matchChar();
	break;

#define SEARCH_LETTER_BUFFER_SIZE	80

case FSEARCHLETTER:
{
	static char searchString[SEARCH_LETTER_BUFFER_SIZE];
	static int searchStringIndex = 0;
	static int searchMode = 0;
	char ch;

	if( arg3[0] != '\0' )
		w = FindWindowByTkName( arg3 );
	if( w == NULL ) {
		printf("FSEARCHLETTER: window %s not found\n", arg3);
		break;
	}
	(void)Tcl_GetInt( pointsMainInterp, arg1, &int1 );
	if( int1 <= 127 ) {
		ch = (char)int1;
		(void)Tcl_GetInt( pointsMainInterp, arg2, &int2 );
		if( int2 & ControlMask )
			ch &= 0x1f;
	} else {
		int1 &= 0xff;
		/* ignore shift keys */
		if( 0xe1 <= int1 && int1 <= 0xee )
			return ret_string;
		ch = '\0';
	}
	if( ch == '\0' ) {
		searchStringIndex = 0;
		searchMode = 0;
		break;
	} else if( ch == '\b' || ch == '\177' ) {
		if( searchStringIndex > 0 )
			--searchStringIndex;
		else
			searchMode = 1;
		break;
	}
	/* start over on overflows */
	if( searchStringIndex >= SEARCH_LETTER_BUFFER_SIZE )
		searchStringIndex = 0;
	searchString[searchStringIndex++] = ch;
	if( w != NULL ) {
		/* terminate and copy string into the search buffer */
		searchString[searchStringIndex] = '\0';
		if( searchStringIndex > 1 )
			/* we want to find this string again if it matches */
			/* with the additional letter added */
			--selBegin;
		(void)searchFor( w, searchMode, searchString, 1,
							linesOverFind );
	}
	break;
}

/************************** TEXT CHANGING COMMANDS *************************/
case FINSASCII:
	Tcl_GetInt( pointsMainInterp, arg1, &n );
	insChar( (unsigned char)n, 1, 0 );
	break;

case FINSERT:
	if( arg1[0] == '\0' || strcmp(arg1,"update")==0 )
		n = UPDATEWINDOWS;
	else
		n = NOUPDATE;
	if( selWindow == NULL )
		break;
	insScrap( 1, n );
	break;

case FCHANGECASE: {
	int wasChanged;
	Offset sEnd = selEnd;	/* since selEnd will change in the loop */
	int row, col, fid, n;
	int how;
	Offset beginCp;

	if( selWindow == NULL )
		break;
	if( arg1[0] == '\0' || striccmp(arg1,"toggle")==0 )
		how = 2;
	else if( striccmp(arg1,"tolower")==0 )
		how = 1;
	else
		how = 0;
	fid = selWindow->fileId;
	if( files[fid].flags & READ_ONLY ) {
		sprintf(msgBuffer, "File %s is read only", files[fid].origName);
		msg(msgBuffer, 1 );
		break;
	}
	while( selBegin <= sEnd ) {
		ch = (char)getFileByte( fid, selBegin );
		if( !isalpha(ch) ) {
			/* pass over the character if it is not alpha */
			goto redrawSelection;
		}
		wasChanged = 0;
		if( isupper(ch) && how != 0 ) {
			ch = tolower(ch);
			wasChanged = 1;
		} else if( islower(ch) && how != 1 ) {
			ch = toupper(ch);
			wasChanged = 1;
		}
		if( wasChanged ) {
			saveSelBegin = selEnd = selBegin;
			(void)deleteChars(selWindow->fileId, NOUPDATE, 0);
			selEnd = selBegin = saveSelBegin;
			insChar( ch, 1, 1 );
		} else {
	redrawSelection:
			n = -1;
			beginCp = prevLine( selWindow->fileId, selBegin, &n );
			if( ch != '\n' ) {
				OffsetToXY(selWindow, ++selBegin, &row, &col);
				DrawSection( selWindow, beginCp, row, col-1,
								row, col );
			} else {
				OffsetToXY(selWindow, selBegin, &row, &col);
				DrawSection( selWindow, beginCp, row, col,
								row, col );
				OffsetToXY(selWindow, ++selBegin, &row, &col);
				DrawSection( selWindow, beginCp, row, col,
								row, col );
			}
			selEnd = selBegin;
		}
	}
	selEnd = selBegin;
	break;
}

case FJUSTIFY:
	if( w == NULL )
		break;
	justifyLines();
	break;

case FDELETE:
	if( arg1[0] == '\0' || strcmp(arg1,"update")==0 )
		n = UPDATEWINDOWS;
	else
		n = NOUPDATE;
	if( selWindow == NULL )
		break;
	(void)deleteChars(selWindow->fileId, n, 1);
	break;

case FEXCHSCRAP:
	if( selWindow == NULL )
		break;
	exchWithScrap();
	break;

case FCOPYSCRAP:
	if( selWindow == NULL )
		break;
	copyToScrap(selWindow, selBegin, selEnd);
	msg("The selection has been copied to the scrap buffer", 0 );
	break;

case FCOPYTO:	/* copy selection to this point */
	n = COPY;
	goto moveAndCopy;

case FMOVETO:	/* move selection to this point */
	n = MOVE;
moveAndCopy:
	if( w == NULL )
		break;
	/* adjust to the selection mode */
	cp = mm_cp;	/* hidden parameter from userInput */
	/* adjustSelMode always uses selWindow so we have to fool it */
	/* be setting selWindow before the call and resetting it after */
	saveSelWindow = selWindow;
	selWindow = w;
	cp = adjustSelMode( cp );
	selWindow = saveSelWindow;
	drawSelection( 1 );
	copyMove(selWindow, selBegin, selEnd, w, cp, n);
	break;

case FCOPYFROM:
	if( selWindow == NULL )
		break;
	if( copyPending ) {
		drawSelection( 1 );
		copyMove(selWindow, selBegin, selEnd, pendWindow, pendPosition,
									COPY);
		currentCursor = mainCursor;
                command( FCHANGECURSOR, "current", "", "", "", "", "" );
		copyPending = 0;
	} else {
                command( FCHANGECURSOR, "dup", "", "", "", "", "" );
		currentCursor = dupCursor;
		copyPending = 1;
		pendWindow = selWindow;
		pendPosition = selBegin;
	}
	break;

case FMOVEFROM:
	if( selWindow == NULL )
		break;
	if( movePending ) {
		copyMove(selWindow, selBegin, selEnd, pendWindow, pendPosition,
									MOVE);
		movePending = 0;
		msg( NULL, 0 );
	} else {
		msg("Extract mode", 0 );
		movePending = 1;
		pendWindow = selWindow;
		pendPosition = selBegin;
	}
	break;

case FREDO:
	if( arg2[0] != '\0' )
		w = FindWindowByTkName( arg2 );
	if( w == NULL ) {
		printf("FREDO: window %s not found\n", arg1);
		break;
	}
	if( arg1[0] != '\0' )
		(void)Tcl_GetInt( pointsMainInterp, arg1, &int1 );
	else
		int1 = 1;
	redo( &files[w->fileId], int1 );
	break;

case FAGAIN:
	n = 1;
	if( arg1[0] != '\0' ) {
		if( striccmp(arg1,"thisfile")==0 )
			n = 0;
		else
			w = FindWindowByTkName( arg1 );
	}
	if( w == NULL ) {
		printf("FAGAIN: window %s not found\n", arg1);
		break;
	}
	again( &files[w->fileId], n );
	break;

case FUNDO:
	if( arg2[0] != '\0' )
		w = FindWindowByTkName( arg2 );
	if( w == NULL ) {
		printf("FUNDO: window %s not found\n", arg1);
		break;
	}
	ff = &files[w->fileId];
	if( arg1[0] != '\0' ) {
		last_change = GetCurrentChange( ff );
		if( strcmp(arg1,"end")==0 ) {
			last_change->flags |= BLOCK_UNDO_END;
			break;
		} else if( strcmp(arg1,"begin")==0 ) {
			last_change->flags |= BLOCK_UNDO_BEGIN;
			break;
		} else if( strcmp(arg1,"update")==0 ) {
			UpdateUndoList( ff );
			break;
		} else
			(void)Tcl_GetInt( pointsMainInterp, arg1, &int1 );
	} else
		int1 = 1;
	undo( ff, int1 );
	break;

/************************** FILE POSITIONING COMMANDS ***********************/
case FBOTFILE:
	if( arg1[0] != '\0' )
		w = FindWindowByTkName( arg1 );
	if( w == NULL ) {
		printf("FBOTFILE: window %s not found\n", arg1);
		break;
	}
	bottomFile(w);
	break;

case FGOTOSELECTION:
	if( w == NULL )
		break;
	doGoSel(w);
	break;

case FGOBACKTO:
	if( arg1[0] != '\0' )
		w = FindWindowByTkName( arg1 );
	if( w == NULL ) {
		printf("FGOBACKTO: window %s not found\n", arg1);
		break;
	}
	doGoto( w, w->rowLastline, 0 );
	break;

case FGOTOLINE:
	if( arg3[0] != '\0' )
		w = FindWindowByTkName( arg3 );
	if( w == NULL ) {
		printf("FGOTOLINE: window %s not found\n", arg3);
		break;
	}
	if( arg1[0] == '\0' )
		int1 = 1;
	else
		(void)Tcl_GetInt( pointsMainInterp, arg1, &int1 );
	if( arg2[0] == '\0' || striccmp(arg2,"lof")==0 )
		int2 = 1;
	else
		int2 = 0;
	doGoto( w, int1, int2 );
	break;

case FGOTODIGIT:
{
	static int lineNumber = 0;
	if( arg2[0] != '\0' )
		w = FindWindowByTkName( arg2 );
	if( w == NULL ) {
		printf("FGOTODIGIT: window %s not found\n", arg2);
		break;
	}
	if( arg1[0] != '\0' && isdigit( arg1[0] ) )
		lineNumber = 10*lineNumber + arg1[0] - '0';
	else {
		doGoto( w, lineNumber, 1 );
		lineNumber = 0;
	}
	break;
}

case FMOVESEL:
	int3 = 0;
	saveSelBegin = -1;
	if( arg3[0] == '\0' || strcmp(arg3,"update")==0 )
		int3 = 1;
	else if (strcmp(arg3,"nosel")==0 ) {
		saveSelBegin = selBegin;
		saveSelEnd = selEnd;
	}
	(void)cursor( arg1, arg2, int3 );
	if( saveSelBegin >= 0 ) {
		selBegin = saveSelBegin;
		selEnd = saveSelEnd;
	}
	sprintf( msgBuffer, "%d", selBegin );
	Tcl_SetResult( pointsMainInterp, msgBuffer, TCL_STATIC );
	break;

/************************ WINDOW MANAGEMENT COMMANDS ***********************/
case FSETTEXTCOLOR: {
	struct fontDataStruct *fontData = &(w->font);
	GC gc;

	if( arg4[0] != '\0' )
		w = FindWindowByTkName( arg4 );
	if( w == NULL ) {
		printf("FSETTEXTCOLOR: window %s not found\n", arg4);
		break;
	}
	if( arg2[0] == '\0' || striccmp(arg2,"normal")==0 )
		int2 = 1;
	else if( striccmp(arg2,"deselected")==0 )
		int2 = 2;
	else
		int2 = 0;
	if( arg3[0] == '\0' || striccmp(arg3,"foreground")==0 )
		int3 = 1;
	else
		int3 = 0;
	switch( int2 ) {
	case 0:	/* selected text */
		gc = fontData->gc_selected;
		break;
	case 1:	/* normal text */
		gc = fontData->gc_normal;
		break;
	case 2:	/* deselected text */
		gc = fontData->gc_deselected;
		break;
	}
	if( int2 == 1 && int3 == 0 )	/* if normal background */
		int2 = 1;		/* then set window background also */
	else
		int2 = 0;
	SetTextColor( w, int2, int3, arg1, gc );
	drawWindow( w );
	break;
}

case FBROWSER:
	CreateNewBrowser( arg1 );
	ret_string = activeBrowser->tk_pathname;
	Tcl_SetResult( pointsMainInterp, ret_string, TCL_STATIC );
	break;

case FWINDOWFONT:
	if( arg2[0] != '\0' )
		w = FindWindowByTkName( arg2 );
	if( w == NULL ) {
		printf("FWINDOWFONT: window %s not found\n", arg2);
		break;
	}
	if( arg1[0] == '\0' )
		arg1 = textFont;
	/* copy it into the window's font name */
	PtFree( w->font.name );
	w->font.name = PtMalloc(strlen(arg1)+1, "font name");
	strcpy(w->font.name, arg1);
	/* indicate that the font is not yet loaded */
	w->font.height = 0;
	/* redraw the window with the new font */
	WorkspaceResized( w );
	XClearWindow( MainDisplay, w->x_window_id );
	drawWindow( w );
	break;

case FBROWSERFONT:
	if( arg1[0] == '\0' )
		arg1 = browserFont;
	ChangeBrowserFontTo( activeBrowser, arg1 );
	/* redraw the file list */
	NewFilelist( activeBrowser );
	break;

case FLOWER:
	if( arg1[0] != '\0' )
		w = FindWindowByTkName( arg1 );
	if( w == NULL ) {
		printf("FLOWER: window %s not found\n", arg1);
		break;
	}
	XLowerWindow( MainDisplay, Tk_WindowId(w->tk_toplevel) );
	break;

case FRAISE:
	if( arg1[0] != '\0' )
		w = FindWindowByTkName( arg1 );
	if( w == NULL ) {
		if( arg1[0] != '\0' )
			printf("FRAISE: window %s not found\n", arg1);
		break;
	}
	XRaiseWindow( MainDisplay, Tk_WindowId(w->tk_toplevel) );
	break;

case FCD:
	if( arg1[0] == '\0' )
		arg1 = "~";
	{
		Tcl_DString ds;
		n = chdir( Tcl_TildeSubst(pointsMainInterp,arg1,&ds) );
		Tcl_DStringFree( &ds );
	}
	if( n == -1 ) {
		extern int errno;
		extern int sys_nerr;
		extern char *sys_errlist[];
		if( errno < sys_nerr )
			fileName = sys_errlist[errno];
		else
			fileName = "";
		sprintf( msgBuffer, "Change directory failed: %s", fileName );
		msg( msgBuffer, 0  );
	} else {
		if( ErrorDisplayed ) {
			msg( "", 0 );
			ErrorDisplayed = 0;
		}
		(void)ExecTclCommand(
			"if [file exists .ptdirrc] {source .ptdirrc}", NULL );
	}
	NewFilelist( activeBrowser );
	break;

case FCONNECTTOPTY:
	n = ConnectToPty( arg1, arg2, arg3, arg4, arg5, arg6 );
	sprintf( pointsMainInterp->result, "%d", n );
	break;

case FOPENWINDOW:
	if( arg1[0] == '\0' )
		arg1 = "NoFileName";
	FixName( arg1 );
	if( arg2[0] == '\0' )
		arg2 = textGeometry;
	int3 = 0;
	if( arg3[0] != '\0' && striccmp(arg3,"donotask")==0 )
		int3 = 1;
	w = GetNewFile( NULL, arg1, arg2, int3 );
	if( w != NULL )
		ret_string = w->tk_pathname;
	else
		ret_string = "";
	Tcl_SetResult( pointsMainInterp, ret_string, TCL_STATIC );
	break;

case FCLOSEWINDOW:
	if( arg2[0] != '\0' )
		w = FindWindowByTkName( arg2 );
	if( w == NULL ) {
#ifdef DEBUG_UPDATE
		printf("FCLOSEWINDOW: window %s not found\n", arg2);
#endif
		break;
	}
	if( arg1[0] == '\0' || striccmp(arg1,"ask")==0 )
		n = 1;
	else if( striccmp(arg1,"nosave")==0 )
		n = 2;
	else
		n = 0;
	if( w == NULL )
		break;
	Tk_DefineCursor( w->tk_text, busyCursor );
	if( closeWindow(w, n) == -1 )
		Tk_DefineCursor( w->tk_text, currentCursor );
	break;

case FRAISELISTWINDOW:
	if( arg2 == NULL || arg2[0] == '\0' )
		arg2 = textGeometry;
	(void)Tcl_GetInt( pointsMainInterp, arg1, &int1 );
	RaiseListWindow( int1, arg2 );
	break;

case FCLOSEBROWSER: {	/* close the active browser */
	BrowserData *browser = browserList;
	BrowserData *prev_browser, *next_browser;
	
	/* find activeBrowser on the browser list */
	while( browser != activeBrowser && browser != NULL )
		browser = browser->nextBrowser;
	if( browser == NULL ) {
		printf("ERROR: activeBrowser not on browserList\n");
		return ret_string;
	}
	/* do not allow the toplevel window to be deleted */
	if( browser == mainBrowser ) {
		msg( "Cannot close the main browser window", 0 );
		break;
	}

	/* reduce the use count for this FileListData */
	ReduceUseCount( browser->fileListData );

	/* unlink browser from the list of browsers */
	prev_browser = browser->prevBrowser;
	next_browser = browser->nextBrowser;
	if( prev_browser == NULL ) {
		/* activeBrowser is first in the browserList */
		if( next_browser == NULL ) {
			/* deleting last browser -- do not allow this */
			msg("Cannot delete the last browser", 0);
			return ret_string;
		} else {
			/* terminate chain and fix up activeBrowser */
			next_browser->prevBrowser = NULL;
			activeBrowser = next_browser;
			browserList = next_browser;
		}
	} else {
		prev_browser->nextBrowser = next_browser;
		if( next_browser != NULL )
			next_browser->prevBrowser = prev_browser;
		activeBrowser = prev_browser;
		/* browserList points to a previous browser so its okay */
	}
	chdir( activeBrowser->cwd );
	SetBrowserNames( activeBrowser );
	Tk_DestroyWindow( browser->tk_toplevel );
	PtFree( (char *)browser );
	break;
}


case FWRITEFILE:
	if( arg1[0] != '\0' )
		w = FindWindowByTkName( arg1 );
	if( w == NULL ) {
		printf("FWRITEFILE: window %s not found\n", arg1);
		break;
	}
	Tk_DefineCursor( w->tk_text, busyCursor );
	writeFile( w );
	Tk_DefineCursor( w->tk_text, currentCursor );
	break;

case FSAVEFILE:
	if( arg1[0] != '\0' )
		w = FindWindowByTkName( arg1 );
	if( w == NULL ) {
		printf("FSAVEFILE: window %s not found\n", arg1);
		break;
	}
	Tk_DefineCursor( w->tk_text, busyCursor );
	saveFile(w);
	Tk_DefineCursor( w->tk_text, currentCursor );
	break;

case FSAVEALL:
	w2 = windowList;
	while( w2 != NULL ) {
		if( files[w2->fileId].flags & IS_CHANGED ) {
			Tk_DefineCursor(w2->tk_text, busyCursor);
			saveFile(w2);
			Tk_DefineCursor(w2->tk_text, currentCursor);
		}
		w2 = w2->nextWindow;
	}
	timeOfLastSave = time(NULL);
	break;

case FZOOM:
	if( arg1[0] != '\0' )
		w = FindWindowByTkName( arg1 );
	if( w == NULL ) {
		printf("FZOOM: window %s not found\n", arg1);
		break;
	}
	if( arg2[0] == '\0' || strcmp(arg2,"vertical")== 0 )
		n = 0;
	else
		n = 1;
	ZoomWindow( w, n );
	break;

/****************************** OTHER COMMANDS *****************************/
case FCHANGECURSOR: {
	Cursor cursor;
	struct window * w3;
	BrowserData *browser;

	if( striccmp(arg1,"busy")==0 )
		cursor = busyCursor;
	else if( striccmp(arg1,"current")==0 )
		cursor = currentCursor;
	else if( striccmp(arg1,"main")==0 )
		cursor = mainCursor;
	else if( striccmp(arg1,"dup")==0 )
		cursor = dupCursor;
	else {
		cursor = Tk_GetCursor( pointsMainInterp,
				mainBrowser->tk_toplevel, Tk_GetUid(arg1) );
		if( cursor == (Cursor)NULL ) {
			sprintf( "Cursor %s not supported", arg1 );
			msg( msgBuffer, 0 );
			break;
		}
	}
	w3 = windowList;
	while( w3 != NULL ) {
		if( w3->x_window_id != (Window)NULL ) {
			Tk_DefineCursor( w3->tk_text, cursor );
		}
		w3 = w3->nextWindow;
	}
	browser = browserList;
	while( browser != NULL ) {
		Tk_DefineCursor( browser->tk_toplevel, cursor );
		browser = browser->nextBrowser;
	}
	break;
}

case FREADONLY:
	if( arg1[0] != '\0' )
		w = FindWindowByTkName( arg1 );
	if( w == NULL ) {
		printf("FREADONLY: window %s not found\n", arg1);
		break;
	}
	if( files[w->fileId].flags & READ_ONLY ) {
		/* file is readOnly now.  Only allow writing if the */
		/* file being edited has DOS write permission */
		/* check for read and write permissions (6 => RW) */
		if( access(files[w->fileId].origName, 6) == 0 )
			files[w->fileId].flags &= ~READ_ONLY;
	} else
		/* allow any file to be readOnly */
		files[w->fileId].flags |= READ_ONLY;
	break;

case FLINENUMBERS:
	if( arg1[0] != '\0' )
		w = FindWindowByTkName( arg1 );
	if( w == NULL ) {
		printf("FLINENUMBERS: window %s not found\n", arg1);
		break;
	}
	switch( arg2[0] ) {
	case '1':
		w->lineNumbers = 1;
		break;
	case '0':
		w->lineNumbers = 0;
		break;
	default:
		w->lineNumbers = !(w->lineNumbers);
		break;
	}
	if( w->lineNumbers ) {
		Tk_Window ln_w;
		sprintf( msgBuffer,
"pack before %s.vScrollAndText.text %s.vScrollAndText.lineNumbers {left fill}",
			w->tk_pathname, w->tk_pathname );
		(void)ExecTclCommand( msgBuffer, NULL );
		sprintf( msgBuffer, "%s.vScrollAndText.lineNumbers",
							w->tk_pathname );
		ln_w = Tk_NameToWindow(pointsMainInterp, msgBuffer,
							w->tk_toplevel);
		w->ln_window_id = Tk_WindowId( ln_w );
	} else {
		sprintf( msgBuffer,
			"pack unpack %s.vScrollAndText.lineNumbers",
			w->tk_pathname );
		(void)ExecTclCommand( msgBuffer, NULL );
	}
	drawWindow( w );
	break;

case FCANCEL:
	if( copyPending ) {
		currentCursor = mainCursor;
                command( FCHANGECURSOR, "current", "", "", "", "", "" );
		copyPending = 0;
	}
	if( movePending ) {
		movePending = 0;
		msg(NULL, 0);
	}
	break;

case FOPTION:
	if( striccmp(arg1,"set")==0 ) {
		SetPointOption( arg2, arg3 );
	} else {
		ret_string = GetPointOption( arg2 );
		Tcl_SetResult( pointsMainInterp, ret_string, TCL_STATIC );
	}
	break;

case FINFORMONCLOSE:
	w = FindWindowByTkName( arg1 );
	if( w == NULL ) {
		printf("FINFORMONCLOSE: window %s not found\n", arg1);
		break;
	}
	n = strlen( arg2 ) + strlen( arg3 ) + 2;
	w->closeInform = (char *)PtMalloc( n, "string" );
	sprintf( w->closeInform, "%s %s", arg2, arg3 );
	break;

case FQUITPOINT:
	/* first see if any files have changed */
	w2 = windowList;
	while(  w2 != NULL ) {
		/* see if the file has been edited */
		if( files[w2->fileId].flags & IS_CHANGED )
			goto areChanges;
		w2 = w2->nextWindow;
	}
	/* no changes, so quit */
	goto exitPoint;

areChanges:
        if( arg1[0] == '\0' || striccmp(arg1,"ask")==0 )
                n = 1;
	else if( striccmp(arg1,"save")==0 )
		n = 0;
	else
		n = 2;
	while( windowList != NULL ) {
		ret = closeWindow( windowList, n );
		if( ret == -1 ) {
			msg("Window close failed, quit cancelled", 1);
			goto noExit;
		}
	}
	/* closeWindow removes a window from windowList and sets */
	/* windowList to NULL when it is empty -- therefor */
	/* this loop really will terminate */

exitPoint:
#ifdef HYPERTEXT
   	if( hypertextOn )
		CloseHypertext();
#endif
	/* TCL/TK CLEANUP */
	Tk_DestroyWindow( TkMainWindow );
	Tcl_DeleteInterp( pointsMainInterp );
	exit( 0 );
noExit:
	break;

case FREDRAW:
	if( arg1[0] != '\0' )
		w = FindWindowByTkName( arg1 );
	if( w == NULL ) {
		printf("FREDRAW: window %s not found\n", arg1);
		break;
	}
	drawWindow( w );
	break;

case FDONOTHING:
	break;

case FMESSAGELINE:
	msg( arg1, 0 );
	break;

case FSHOWUNDOS:
	if( w == NULL )
		break;
	ShowUndos( &files[w->fileId] );
	break;

case FPRINTSTATS:
	PrintStats( 1 );
	break;

#ifdef HYPERTEXT
case FADDFILETODOCUMENT:
	AddFileToDocument( w );
	sprintf(msgBuffer, "Added file %s into an Anasazi document %s",
		&(files[w->fileId].origName[w->nameOffset]),
		w->document->name);
	msg( msgBuffer, 0 );
/***** debug output *****/
printf( "%s\n", msgBuffer );
printf("file is %s (%d)\nblock is %s (%d)\nmap is %s (%d)\nview is %s (%d)\n",
w->file->name, w->file->this_one, w->block->name, w->block->this_one,
w->blockMap->name, w->blockMap->this_one, w->view->name, w->view->this_one);
/***** debug output *****/
	drawWindow( w );
	break;

case FCREATEDOCUMENT:
	break;

case FCREATEBLOCK:
{
	Block block;
	AttributeID attrID = PickAttribute( w->document,
			"Pick attribute for the new block" );

	if( attrID != NullObject ) {
		extern struct openFile *files;
		int fid;
		struct openFile *ff;
		Attribute attribute;

		block = CreateBlock( w->db, w->document, "block",
						attrID, 0, NullObject );
/***** debug output *****/
attribute = GetAttribute( currentDB, attrID, NO_ALLOCATE);
printf("Created block %s (block ID=%d) with attribute %s (attribute ID=%d)\n",
block->name, block->this_one, attribute->name, attribute->this_one);
/***** debug output *****/

		drawSelection( 1 );	/* erase the selection */

		fid = w->fileId;
		ff = &files[fid];

		/* convert selBegin and selEnd to the underlying (real) file */
		ret = GetRealSelection( ff, 0/*get both selBegin and selEnd*/);

		/* free the piece list of the old view */
		FreeOldViewPieces( ff );

		/* insert the new block into the real file */
		w->fileId = w->realFileId;
		(void)InsertBlock( block->this_one );

		/* create a new piece table for the view */
		w->fileId = CreateViewFile( w );

		/* free the block structure since we are done with it */
		PtFree( (char *)block );
		drawWindow( w );
	}
	break;
}

case FCREATEATTRIBUTE:
{
	Attribute attribute;
	char * ret;

	sprintf( msgBuffer, "MakeModalEntry {%s} {%s} {%s} {%s}",
		"Create Attribute", "Name of attribute to create",
		"Create attribute", "Cancel" );
	ret = ExecTclCommand( msgBuffer, NULL );
	if( strcmp(ret,"XXXcancelXXX") != 0 ) {
		attribute = CreateAttribute( currentDB, currentDocument, ret );
/***** debug output *****/
printf("Created attribute %s with ID=%d\n",attribute->name,attribute->this_one);
/***** debug output *****/
		PtFree( (char *)attribute );
	}
	break;
}

case FCREATEMAP:
{
	Map map;
	char *s, *box_name;
	PickListItem * itemList, *item;

	sprintf( msgBuffer, "MakeModalEntry {%s} {%s} {%s} {%s}",
		"Create Map", "Name of map to create",
		"Create map", "Cancel" );
	s = ExecTclCommand( msgBuffer, NULL );
	if( strcmp(s,"XXXcancelXXX") != 0 ) {
		map = CreateMap( currentDB, currentDocument, ret );
/***** debug output *****/
printf("Created map %s with ID=%d\n",map->name,map->this_one);
/***** debug output *****/
		/* get the list of items */
		itemList = GenerateIDList( currentDocument->firstAttribute,
			AttributeMagic );
		sprintf( msgBuffer, "MakeMapBox" );
		s = ExecTclCommand( msgBuffer, NULL );
		/* make a copy of the name */
		box_name = (char *)PtMalloc( strlen(s)+1, "name" );
		strcpy( box_name, s );
		/* fill the list box */
		item = itemList;
		while( itemList != NULL ) {
			sprintf( msgBuffer, "%s.alist.items insert end {%s}",
						box_name, item->name );
			(void)ExecTclCommand( msgBuffer, NULL );
			item = item->next;
		}
		PtFree( (char *)box_name );

#ifdef XXXXXXXXXXXX
		command( FWAITFORRETURNSTRING, "", "", "", "", "", "" );
		if( strcmp(returnString,"XXXcancelXXX") != 0 ) {
			int i, n, b[MAP_SIZE];
			Attribute attribute;
			for( i = 0; i < MAP_SIZE; ++i )
				b[i] = 0;
			n = sscanf( returnString, "%d %d %d %d %d",
				&b[0], &b[1], &b[2], &b[3], &b[4]);
/***** debug output *****/
printf("This map will show block types:\n");
for( i = 0; i < n; ++i ) {
	attribute = GetAttribute( currentDB, (ID)b[i], NO_ALLOCATE );
	if( attribute == NULL )
		printf("Attribute ID %d is invalid\n", b[i]);
	else {
		printf("\t%s (%d)\n", attribute->name, b[i]);
		map->domain[i] = b[i];
	}
}
printf("\n");
/***** debug output *****/
			PutMap( currentDB, map, 0 );
		}
#endif
		PtFree( (char *)map );
		FreeIDList( itemList );
	}
	break;
}

case FCREATELINK:
	DumpTables();
	break;

case FCREATEVIEW:
	break;

case FCHANGEMAP:
{
	Map map;
	int i;
	MapID mapID = PickMap( w->document, "Pick map to change to" );

	if( mapID != NullObject ) {
		/* get the map */
		map = GetMap( currentDB, mapID, ALLOCATE );
		if( map == NULL ) {
			printf("Map ID %d not found\n", mapID );
			break;
		}

		/* free the old map (if any) and install the new one */
		PtFree( (char *)(w->blockMap) );
		w->blockMap = map;

		printf("Current map will show block types:\n");
		for( i = 0; i < MAP_SIZE && map->domain[i]!=NullObject; ++i ) {
			Attribute attribute;
			attribute = GetAttribute( currentDB,
						map->domain[i], NO_ALLOCATE );
			if( attribute == NULL )
				printf("Attribute ID %d is invalid\n",
						map->domain[i]);
			else
				printf("\t%s (%d)\n", attribute->name,
						attribute->this_one);
		}

		/* free the piece list of the old view */
		FreeOldViewPieces( &(files[w->fileId]) );

		/* create a piece table for the new view */
		w->fileId = CreateViewFile( w );

		drawWindow( w );
	}
	break;
}

case FSHOWATTRIBUTES:
{
	AttributeID attributeID;
	Attribute attribute;

/***** debug output *****/
attributeID = currentDocument->firstAttribute;
printf("\n*** List of all defined attributes ***\n");
while( attributeID != NullObject ) {
	attribute = GetAttribute( currentDB, attributeID, NO_ALLOCATE );
	printf("%s (%d)\n", attribute->name, attribute->this_one );
	attributeID = attribute->next;
}
/***** debug output *****/
	attributeID = PickAttribute( currentDocument, "Show attributes" );
	break;
}

case FSHOWBLOCKS:
{
	BlockID blockID;
	Block block;
	int i;

/***** debug output *****/
blockID = currentDocument->firstBlock;
printf("\n*** List of all defined blocks ***\n");
while( blockID != NullObject ) {
	block = GetBlock( currentDB, blockID, NO_ALLOCATE );
	printf("%s (%d) fileID=%d @ %d, attributes:",
		block->name, block->this_one, block->file, block->hint );
	for( i = 0; i < MAX_ATTRIBUTES; ++i ) {
		if( block->attribute[i] == NullObject )
			break;
		printf(" %d", block->attribute[i]);
	}
	printf("\n");
	blockID = block->next;
}
/***** debug output *****/
	blockID = PickBlock( currentDocument, "Show blocks" );
	break;
}

case FSHOWDOCUMENTS:
	printf("\n*** The current document ***\n");
	printf("%s (%d), next free=%d, initial view=%d\n",
		currentDocument->name, currentDocument->this_one,
		currentDocument->nextFreeID, currentDocument->initialView );
	break;

case FSHOWFILES:
{
	FileID fileID;
	File file;

/***** debug output *****/
fileID = currentDocument->firstFile;
printf("\n*** List of all defined files ***\n");
while( fileID != NullObject ) {
	file = GetFile( currentDB, fileID, NO_ALLOCATE );
	printf("%s (%d)\n", file->name, file->this_one );
	fileID = file->next;
}
/***** debug output *****/
/***** debug output *****/
	fileID = PickFile( currentDocument, "Show files" );
	break;
}

case FSHOWLINKS:
{	LinkID linkID;
	Link link;
	int i;

/***** debug output *****/
linkID = currentDocument->firstLink;
printf("\n*** List of all defined links ***\n");
while( linkID != NullObject ) {
	link = GetLink( currentDB, linkID, NO_ALLOCATE );
	printf("%s (%d) fromID=%d toID=%d, attributes:",
		link->name, link->this_one, link->from, link->to );
	for( i = 0; i < MAX_ATTRIBUTES; ++i ) {
		if( link->attribute[i] == NullObject )
			break;
		printf(" %d", link->attribute[i]);
	}
	printf("\n");
	linkID = link->next;
}
/***** debug output *****/
	linkID = PickLink( currentDocument, "Show links" );
	break;
}

case FSHOWMAPS:
{
	MapID mapID;
	Map map;

/***** debug output *****/
mapID = currentDocument->firstMap;
printf("\n*** List of all defined maps ***\n");
while( mapID != NullObject ) {
	int i;
	map = GetMap( currentDB, mapID, ALLOCATE );
	printf("%s (%d)\n", map->name, map->this_one );
	for( i = 0; i < MAP_SIZE && map->domain[i]!=NullObject; ++i ) {
		Attribute attribute;
		attribute = GetAttribute( currentDB, map->domain[i],
								NO_ALLOCATE );
		if( attribute == NULL )
			printf("Attribute ID %d is invalid\n",
					map->domain[i]);
		else
			printf("\t%s (%d)\n", attribute->name,
					attribute->this_one);
	}
	mapID = map->next;
	PtFree( (char *)map );
}
/***** debug output *****/
	mapID = PickMap( currentDocument, "Show maps" );
	break;
}

case FSHOWTEXTS:
{
	TextID textID;
	Text text;

/***** debug output *****/
textID = currentDocument->firstText;
printf("\n*** List of all defined texts ***\n");
while( textID != NullObject ) {
	text = GetText( currentDB, textID, NO_ALLOCATE );
	printf("%s (%d)\n", text->s, text->this_one );
	textID = text->next;
}
/***** debug output *****/
	textID = PickText( currentDocument, "Show texts" );
	break;
}

case FSHOWVIEWS:
{	ViewID viewID;
	View view;

/***** debug output *****/
viewID = currentDocument->firstView;
printf("\n*** List of all defined views ***\n");
while( viewID != NullObject ) {
	view = GetView( currentDB, viewID, NO_ALLOCATE );
	printf("%s (%d), block=%d, fromLinkMap=%d, toLinkMap=%d, blockMap=%d\n",
		view->name, view->this_one, view->blockID,
		view->fromLinkMap, view->toLinkMap,
		view->blockMap );
	viewID = view->next;
}
/***** debug output *****/
	viewID = PickView( currentDocument, "Show views" );
	break;
}
#endif

default:
	sprintf(msgBuffer, "No action is defined for command %d", fn);
	msg(msgBuffer, 0);
	break;
}
lastFn = fn;
if( fn != FDONOTHING && fn > 0 ) 
	lastCommand = fn;

if( autoSaveCount > 0 && commands_until_autosave <= 0 ) {
	if( commands_until_autosave == 0 ) {
		/*EMPTY*/
		/* auto save */
	}
	commands_until_autosave = autoSaveCount;
}

return ret_string;
}

