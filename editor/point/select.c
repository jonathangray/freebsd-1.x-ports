/* $Header: /a/cvs/386BSD/ports/editor/point/select.c,v 1.1 1994/02/15 22:12:40 jkh Exp $ */

#include <ctype.h>
#include <stdio.h>
#include "pt.h"
#include <X11/Xatom.h>

/* the globals selection */
struct window *selWindow = NULL;
Offset selBegin, selEnd;
Offset selBeginCp;
int selMode;

int ptOwnsSelection = 0;

void 
ExtendSelection( cp, row, col, beginRowCp, anchorCp )
	Offset cp, beginRowCp, anchorCp;
	int row, col;
{
	extern struct window *selWindow;
	extern Offset selBegin, selEnd;

	int saveSelBegin = selBegin;
	int saveSelEnd = selEnd;
	Offset begin, end;

	/* extend according to the selection mode */
	modeExtend( selWindow, cp, row, col, beginRowCp );
		/* this call sets selBegin and selEnd */

	/* figure out what we have to redraw */
	if( saveSelBegin <= selBegin && selEnd < saveSelEnd ) {
		/* the selection is being contracted */
		if( selEnd <= anchorCp ) {
			/* contract from selBegin, selEnd is the anchor */
			begin = saveSelBegin;
			end = selBegin - 1;
			selEnd = saveSelEnd;
		} else {
			/* contract from selEnd, selBegin is the nachor */
			begin = selEnd + 1;
			end = saveSelEnd;
			selBegin = saveSelBegin;
		}
	} else if( selBegin < saveSelBegin ) {
		/* the selection is being extended to the left */
		begin = selBegin;
		end = saveSelBegin - 1;
		selEnd = saveSelEnd;
	} else {
		/* the selection is being entended to the right */
		begin = saveSelEnd + 1;
		end = selEnd;
		selBegin = saveSelBegin;
	}

	if( begin < selWindow->posTopline )
		begin = selWindow->posTopline;
	if( end >= selWindow->posBotline )
		end = selWindow->posBotline - 1;
	if( begin <= end ) {
		int row1, col1, row2, col2;
		int n = -1;
		beginRowCp = prevLine( selWindow->fileId, begin, &n );
		OffsetToXY( selWindow, begin, &row1, &col1 );
		OffsetToXY( selWindow, end, &row2, &col2 );
		DrawSection( selWindow, beginRowCp, row1, col1, row2, col2 );
	}
}

void
drawSelection( erase )
	int erase;
{
	extern struct window *selWindow;
	extern Offset selBegin, selEnd;

	Offset saveSelBegin;
	Offset saveSelEnd;
	Offset begin = selBegin;
	Offset end = selEnd;

	if( selWindow == NULL )
		return;	/* there is no selection to erase */

	/* Turn off the selection, then draw the area.  This will erase */
	/* the selection.  Then restore selBegin. */
	if( erase ) {
		saveSelBegin = selBegin;
		saveSelEnd = selEnd;
		selBegin = 1;	/* so no selection will draw */
		selEnd = 0;
	}

	if( begin < selWindow->posTopline )
		begin = selWindow->posTopline;

	if( end >= selWindow->posBotline && end < fileSize(selWindow->fileId) )
		end = selWindow->posBotline - 1;

	if( begin <= end ) {
		/* some of the selection is showing in the window */
		int n = -1;
		Offset beginCp = prevLine( selWindow->fileId, begin, &n );
		int row1, col1, row2, col2;

		OffsetToXY( selWindow, begin, &row1, &col1 );
		OffsetToXY( selWindow, end, &row2, &col2 );
		DrawSection( selWindow, beginCp, row1, col1, row2, col2);
	}

	if( erase ) {
		selBegin = saveSelBegin;
		selEnd = saveSelEnd;
	}
}	


void
DrawSection( w, beginRowCp, beginRow, beginCol, endRow, endCol )
	struct window *w;
	Offset beginRowCp;
	int beginRow, beginCol, endRow, endCol;
{
	int row, col, y;
	Offset cp;

	/* if the selection is below the window then do nothing */
	if( beginRow >= w->nRows )
		return;

	/* if the selection is above the window then do nothing */
	if( endRow < 0 )
		return;

	/* if part of the selection is above the window, adjust for it */
	if( beginRow < 0 ) {
		beginRow = 0;
		beginCol = 0;
	}

	/* if part of the selection is below the window, adjust for it */
	if( endRow >= w->nRows ) {
		endRow = w->nRows - 1;
		endCol = w->nCols;
	}
	
	/* At this point the entire section (to draw) is in the window */
	/* draw draw the section */

	y = w->topMargin + (w->font).ascent + beginRow * ((w->font).height);
	cp = beginRowCp;

	/* determine the boundaries to draw text in */
	if( beginRow == endRow )
		col = endCol;
	else
		col = w->nCols;
	cp = fillLine( w, cp, beginRow, beginCol, col, y, 0 );
	for( row = beginRow + 1; row < endRow; ++row ) {
		y += (w->font).height;
		cp = fillLine( w, cp, row, 0, w->nCols, y, 0 );
	}
	if( beginRow < endRow ) {
		y += (w->font).height;
		cp = fillLine( w, cp, row, 0, endCol, y, 0 );
	}
}

/*ARGSUSED*/
void
modeExtend(w, cp, row, col, beginRowCp )
	struct window *w;
	Offset cp;
	int row, col;
	Offset beginRowCp;
{
	extern int selMode;
	extern Offset selBegin, selEnd;

	int uch;
	int n;
	Offset cpLine;
	int fid = w->fileId;

	/* determine the initial selection */
	switch( selMode ) {

	case SELCHAR:
		selBegin = cp;
		selEnd = cp;
		break;

	case SELWORD:
	{	Offset cpLow, cpHigh;

		/* test for the special case of the first character of the */
		/* selection not being a digit, letter or '_' */
		uch = getFileByte( fid, cp );
		if( (uch==BLOCK_EOF)
		 || (!isalnum((char)uch) && ((char)uch != '_')) ) {
			selBegin = selEnd = cp;
			break;
		}
		cpLow = cp;
		while( 1 ) {
			uch = getFileByte( fid, cpLow-- );
			/* stop when you: */
			/* 1. go past the beginning of the file */
			if( uch == BLOCK_EOF )
				break;
			/* 2. read a non-word character */
			if( !isalnum((char)uch) && ((char)uch != '_') )
				break;
		}
		/* we went two too far */
		selBegin = cpLow + 2;

		cpHigh = cp;
		while( 1 ) {
			uch = getFileByte( fid, cpHigh++ );
			/* stop when you: */
			/* 1. go past the beginning of the file */
			if( uch == BLOCK_EOF )
				break;
			/* 2. read a non-word character */
			if( !isalnum((char)uch) && ((char)uch != '_') )
				break;
		}
		selEnd = cpHigh - 2;
		break;
	}

	case SELLINE:
		/* find the beginning of the line */
		cpLine = cp;
		n = -1;
		cpLine = prevLine( fid, cpLine, &n );
		selBegin = cpLine;

		/* find the end of the line */
		cpLine = cp;
		n = 1;
		cpLine = nextLine( fid, cpLine, &n );
		selEnd = cpLine - 1;
		break;

	case SELBLOCK:
		/* find the beginning of the block */
	/* DO NOT DO THIS YET */
		selBegin = cp;
		selEnd = cp;
		break;
	}
}

int
indentToShowSelection(selCol)
	int selCol;	/* the column where the first character of */
			/* the selection is */
{
	extern struct window *selWindow;
	extern Offset selBegin, selEnd;

	int dummy, col, windowWidth, indent, endIndent;
	int originalIndent;

	originalIndent = selWindow->indent;

	/* find the column the selection starts in */
	if( selCol == -1 )
		OffsetToXY( selWindow, selBegin, &dummy, &col );
	else
		col = selCol;

	/* sourceToXY subtracts the current indent so add it back in */
	col += selWindow->indent;
	windowWidth = selWindow->nCols;

	/* if the selection is right of the window, change the indent */
	if( col > (windowWidth + selWindow->indent) ) {
		indent = col - (windowWidth>>1);
		/* figure the column of the end of the selection */
		/* 	add window width / 2 to put it in the  */
		/*	middle of the window */
		endIndent = (selEnd - selBegin) + indent;
		if( endIndent < col )
			selWindow->indent = endIndent;
		else
			selWindow->indent = indent;
	} else if( col < selWindow->indent ) {
		/* if the selection is left of the window change the indent */
		/* change the indent back to zero if this will still leave */
		/* the beginning of the selection in the left half of the */
		/* window */
		indent = col - (windowWidth>>1);
		if( indent < 0 )
			indent = 0;
		selWindow->indent = indent;
	}
	/* indicate on return whether you actually changed the indent */
	return (originalIndent != selWindow->indent);
}

Offset
adjustSelMode( cp )
	Offset cp;
{
	extern struct window *selWindow;
	extern int selMode;
	
	int uch, n;
	int fid = selWindow->fileId;

	switch( selMode ) {
	case SELBLOCK:
		break;
	case SELLINE:
		n = -1;
		cp = prevLine( fid, cp, &n );
		break;
	case SELWORD:
		/* only search if cp is inside a word */
		uch = getFileByte( fid, cp );
		if( isalnum((char)uch) || (char)uch == '_' ) {
			while( 1 ) {
				uch = getFileByte( fid, --cp );
				if( !isalnum((char)uch) && (char)uch != '_' )
					break;
			}
			++cp;
		}
		break;
	case SELCHAR:
		/* nothing to adjust in char mode */
		break;
	}
	return cp;
}

/*ARGSUSED*/
int
SupplySelectionToX( clientData, offset, buffer, maxBytes )
	ClientData clientData;
	int offset;
	char * buffer;
	int maxBytes;
{
	extern int ptOwnsSelection;

	ptOwnsSelection = 1;	/* to be safe */
	(void) getSelection( buffer, offset, maxBytes );
	return strlen( buffer );
}

/*ARGSUSED*/
static void
LoseXSelection( clientData )
	ClientData clientData;
{
	extern int ptOwnsSelection;

	ptOwnsSelection = 0;
	drawSelection( 0 );
}

void
AssertSelectionOwnership()
{
	extern int ptOwnsSelection;
	extern BrowserData * mainBrowser;

	if( !ptOwnsSelection ) {
		ptOwnsSelection = 1;
		Tk_OwnSelection( mainBrowser->tk_toplevel, LoseXSelection, 0 );
		drawSelection( 0 );
	}
}

static int selLength;
static char * selString;

/*ARGSUSED*/
static int
ReceiveXSelection( clientData, interp, portion )
	ClientData clientData;
	Tcl_Interp * interp;
	char * portion;
{
	char ch;

	while( (ch = *portion++) != '\0' ) {
		if( --selLength <= 0 )
			break;
		*selString++ = ch;
	}
	return TCL_OK;
}

int
getSelection(s, offset, length)
	char *s;
	int offset;
	int length;
{
	extern Offset selBegin, selEnd;
	extern struct window *selWindow;
	extern int ptOwnsSelection;
	extern BrowserData * mainBrowser;
	extern Tcl_Interp * pointsMainInterp;

	char *p;
	int fid;

	/* If we own the selection then get it */
	if( ptOwnsSelection ) {
		Offset cp = selBegin + offset;

		p = s;
		if( selWindow != NULL )
			fid = selWindow->fileId;
		else {
			msg("No selection, using ~.", 0 );
			s[0] = '~';
			s[1] = '\0';
			return 1;
		}
		while( cp <= selEnd ) {
			*p++ = (char)getFileByte( fid, cp++ );
			/* check for string overflow */
			if( p-s >= length ) {
				--p;
				break;
			}
		}
		*p = '\0';
		return 1;
	}

	/* else get it from X */
	selString = s;
	selLength = length;
	fid = Tk_GetSelection( pointsMainInterp, mainBrowser->tk_toplevel,
		XA_STRING, ReceiveXSelection, 0 );
	*selString = '\0';
	/* did we overflow the space alloted? */
	return (selLength > 0);
}


