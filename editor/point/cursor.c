/* $Header: /a/cvs/386BSD/ports/editor/point/cursor.c,v 1.1 1994/02/15 22:12:36 jkh Exp $ */

#include <ctype.h>
#include "pt.h"

/* keep records so we can move cursor consistently in vertical direction */
int lastFn = 0;
static int lastColumn = -1;
static int lastCursorFn = 0;

#define FSELUP		1
#define FSELDOWN	2
#define FSELRIGHT	3
#define FSELLEFT	4
#define FWORDRIGHT	5
#define FWORDLEFT	6
#define FLINERIGHT	7
#define FLINELEFT	8

int
cursor( how, direction, update )
	char * how;
	char * direction;
	int update;
{
	extern struct window *selWindow;
	extern Offset selBegin, selEnd;
	extern int selMode;
	extern int lastFn;

	Offset cp;
	char ch;
	int uch;
	int row, col;
	int alphaNumeric;
	struct window *w = selWindow;
	int n;
	int fid;
	int fn;

	if( selWindow == NULL )
		return 0;

	fid = selWindow->fileId;

	/* first erase the current selection since the cursor motion */
	/* will move the selection */
	drawSelection( 1 );

	if( selWindow == NULL ) {
		/* get the mouse cursor position */
		printf("No selection\n");
		return 0;
	}

	/* find the position of the current selection */
	cp = selBegin;
	OffsetToXY( selWindow, cp, &row, &col );

	/* adjust it according to the function invoked */
	if( strcmp(how,"char")==0 ) {
		if( strcmp(direction,"left")==0 ) {
			fn = FSELLEFT;
			switch( selMode ) {
				default:
				case SELCHAR:
					break;
				case SELWORD:
					goto doWordLeft;
				case SELLINE:
					goto doCurUp;
			}
			if( col > 0 )
				--col;
			else if( row > 0 || selWindow->numTopline>1 ) {
				--row;
				col = selWindow->nCols - 1;
			}
		} else if( strcmp(direction,"right")==0 ) {
			fn = FSELRIGHT;
			switch( selMode ) {
				default:
				case SELCHAR:
					break;
				case SELWORD:
					goto doWordRight;
				case SELLINE:
					goto doCurDown;
			}
			ch = getFileByte( fid, selBegin );
			if( ch != '\n' ) {
				if( col < selWindow->nCols )
					++col;
			} else {
				++row;
				col = 0;
			}
		} else if( strcmp(direction,"up")==0 ) {
doCurUp:
			/* Special case so that cursor up and down move down*/
			/* the page in a straight line so far as is possible.*/
			if( (lastColumn != -1) && (lastFn == FMOVESEL)
			 && (lastCursorFn==FSELUP||lastCursorFn==FSELDOWN) ) {
				col = lastColumn;
			}

			fn = FSELUP;
			if( row > 0 || selWindow->numTopline>1 )
				--row;
		} else {	/* down */
doCurDown:
			/* Special case so that cursor up and down move down*/
			/* the page in a straight line so far as is possible.*/
			if( (lastColumn != -1) && (lastFn == FMOVESEL)
			 && (lastCursorFn==FSELUP||lastCursorFn==FSELDOWN) ) {
				col = lastColumn;
			}

			fn = FSELDOWN;
			if( row < selWindow->nRows )
				++row;
		}
	} else if( strcmp(how,"word")==0 ) {
		if( strcmp(direction,"left")==0 ) {
doWordLeft:
			fn = FWORDLEFT;
			cp = xyToOffset( w, row, col );
	
			/* skip the white space between words */
			uch = getFileByte( fid, cp );
			while( uch != BLOCK_EOF ) {
				uch = getFileByte( fid, --cp );
				if( !isspace(uch) )
					break;
			}
	
			alphaNumeric = isalnum(uch);
			/* skip the characters in the word */
			ch = 1;
			while( ch != '\0' ) {
				ch = getFileByte( fid, --cp );
				/* if we have switched from alphanumeric to */
				/* not alphanumeric or the reverse, then quit */
				if( alphaNumeric ) {
					if( !isalnum(ch) && ch != '_' ) {
						++cp;
						break;
					}
				} else if(isalnum(ch)||isspace(ch)||ch=='_'){
					++cp;
					break;
				}
			}
finishUp:
			OffsetToXY( w, cp, &row, &col );
			selBegin = cp;
			selEnd = selBegin;
			n = -1;
			cp = prevLine( selWindow->fileId, selBegin, &n );
			modeExtend( selWindow, selBegin, row, col, cp );
			if( indentToShowSelection(-1) ) {
				OffsetToXY( w, cp, &row, &col );
			}
		} else {	/* "right" */
doWordRight:
			fn = FWORDRIGHT;
			cp = xyToOffset( w, row, col );
	
			ch = (char)getFileByte( fid, cp );
			alphaNumeric = isalnum(ch);
	
			/* skip the characters in the word */
			ch = 1;
			while( ch != '\0' ) {
				ch = (char)getFileByte( fid, ++cp );
				/* if we have switched from alphanumeric to */
				/* not alphanumeric or the reverse, then quit */
				if( alphaNumeric ) {
					if( !isalnum(ch) && ch != '_' )
						break;
				} else if( isalnum(ch)||isspace(ch)||ch=='_' )
					break;
			}
	
			/* skip the white space between words */
			while( isspace(ch) )
				ch = (char)getFileByte( fid, ++cp );
	
			goto finishUp;
		}
	} else if( strcmp(how,"line")==0 ) {
		if( strcmp(direction,"right")==0 ) {
			fn = FLINERIGHT;
			cp = xyToOffset( w, row, col );
			while( 1 ) {
				ch = getFileByte( fid, cp++ );
				if( ch == '\n' || ch == '\0' )
					break;
			}
			--cp;	/* we went one past the NL */
			goto finishUp;
		} else {
			fn = FLINELEFT;
			cp = xyToOffset( w, row, col );
			uch = getFileByte( fid, cp-- );
			while( uch != BLOCK_EOF ) {
				uch = getFileByte( fid, cp-- );
				if( (char)uch == '\n' )
					break;
			}
			/* Move up to the first character of the line. */
			/* This is the first character past the '\n' */
			cp += 2;
			if( strcmp(direction,"left")==0 ) {
				/* skip tabs and blanks */
				while( 1 ) {
					ch = getFileByte( fid, cp++ );
					if( ch != ' ' && ch != '\t' )
						break;
				}
				/* undo last increment scanning white space */
				--cp;
			} /* else "left0" */
			goto finishUp;
		}
	} else {
		printf("Unrecognized selection movement: %s\n", how);
		return 0;
	}

	lastCursorFn = fn;
	if( update )
		doScreenUpdate( fn, col, row );
	else {
		switch( fn ) {
			case FSELRIGHT:
			case FSELLEFT:
			case FSELUP:
			case FSELDOWN:
				selBegin = cp;
				selEnd = cp;
				break;
		}
	}
	return 0;
}


void
doScreenUpdate( fn, col, row )
	int fn, col, row;
{
	extern struct window *selWindow;
	extern Offset selBegin, selEnd;
	extern int scrollDown;
	extern int intervalRows;
	extern struct window * scroll_window;

	Offset cp;
	int n, y;
	register struct window *w = selWindow;
	struct fontDataStruct *font = &(w->font);
	int fid = selWindow->fileId;
	int ch;

	lastColumn = col;
	cp = xyToOffset( w, row, col );

	/* This is to prevent the right cursor movement from */
	/* getting stuck on a tab.  It should happen only when */
	/* the selection is on a tab. */
	ch = getFileByte( fid, cp );

	if( ch==BLOCK_EOF && fn==FSELDOWN ) {
		drawSelection(0);
		return;
	}

	if( ch=='\t' && selBegin==cp && selEnd==cp && fn==FSELRIGHT )
		++cp;

	/* see if we scrolled off an edge of the window */
	if( row >= w->nRows ) {
		intervalRows = w->nRows/3;
		scrollDown = 1;
		scroll_window = w;
		n = DoOneVScroll();
		row -= n;
		cp = xyToOffset( w, row, col );
	} else if( row < 0 ) {
		intervalRows = w->nRows/3;
		scrollDown = 0;
		scroll_window = w;
		n = DoOneVScroll();
		row += n;
		cp = xyToOffset( w, row, col );
	}

	switch( fn ) {
	case FSELRIGHT:
	case FSELLEFT:
	case FSELUP:
	case FSELDOWN:
		selBegin = cp;
		selEnd = cp;
		break;
	}
	n = -1;		/* find the beginning of the line */
	cp = prevLine( fid, cp, &n );

	/* figure out where to write line 'row' */
	y = w->topMargin + font->ascent + row*font->height;
	(void) fillLine( w, cp, row, 0, w->nCols, y, 0 );
}

