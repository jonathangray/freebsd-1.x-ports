/* $Header: /a/cvs/386BSD/ports/editor/point/findpos.c,v 1.1 1994/02/15 22:12:37 jkh Exp $ */

#include "pt.h"

Offset
xyToOffset( w, row, col )
	struct window *w;
	int row, col;
{
	extern int tabWidth;

	Offset cp;
	int n, tabStop, rowsDown, iLine;
	int uch;
	int fid = w->fileId;

	rowsDown = row;	/* rows down */

#ifdef XXXXXXX
/* fix this later, maybe keep a flag to tell if the window has moved */
	/* has window moved since we last found a line? */
	doRowCache = 1;
	if( w->lastPosTop == w->posTopline ) {
		/* if so find the new line relative to it */
		cp = w->posCurLast;
		rowsDown -= w->rowCurLast;
		if( rowsDown < 0 ) {
			rowsDown = -rowsDown;
			cp = prevLine( fid, cp, &rowsDown );
		} else if( rowsDown > 0 )
			goto countDown;
	} else {
#endif

		/* just count down from the top line */
		cp = w->posTopline;
#ifdef XXXXXXX
	countDown:
#endif
		n = rowsDown;
		cp = nextLine( fid, cp, &rowsDown );

#ifdef FIX_THIS_LATER
		/* fix so that EOF works */
		if( (n -= rowsDown) > 0 ) {
			*inRow -= n;
			doRowCache = 0;
		}
		/* see if EOF on a line alone */
		if( (cp-1) > 0 &&
		  (char)getFileByte( fid, cp-1 ) != '\n') {
			/* another fix so EOF works */
			--*inRow;
			doRowCache = 0;
		}
	}
	/* if we are near end of file, "row" and the */
	/* "cp" will not really be right.  The +3 is */
	/* superstitious but I want to be sure we don't get */
	/* get off on the counts.  There is little need */
	/* to optimize at the end of the file, we can forgo */
	/* that to be safe */
	if( (cp + 3 < fileSize(w->fileId)) && doRowCache ) {
		/* compute the number of rows down */
		w->rowCurLast = row;
		w->posCurLast = cp;
		w->lastPosTop = w->posTopline;
	} else
		w->lastPosTop = NULL;
#endif

	n = col + w->indent;
	iLine = 0;
	if( w->lineNumbers )
		iLine = 5;
	while( iLine <= n ) {
		uch = getFileByte( fid, cp++ );
		switch( uch ) {
		case '\n':	/* end of line */
		case BLOCK_EOF:	/* end of file */
			/* NOTE: cp could get two past EOF since we post- */
			/* increment above and do not fix it below */
			goto endLoop;
		case '\t':
			tabStop = iLine + tabWidth - (iLine % tabWidth);
			if( tabStop > n )
				goto endLoop;
			else
				iLine = tabStop;
			break;
		default:
			iLine++;
			break;
		}
	}
endLoop:
	return cp-1;
}

void
OffsetToXY( w, offset, outRow, outCol )
	struct window *w;
	Offset offset;
	int *outRow, *outCol;
{
	extern int tabWidth;
	extern int getSpanSize;

	Offset cp;
	int tabStop, row, lastRow, iLine;
	int fid = w->fileId;
        unsigned char *firstByte = (unsigned char *)1;
        unsigned char *lastByte = (unsigned char *)0;

	/* see if offset is above the window */
	if( offset < w->posTopline )
		goto NotFound;

	/* find out which row it is on */
	cp = w->posTopline;
	row = 0;
	iLine = 0;
	lastRow = w->nRows;
	while( row < lastRow ) {
		if( cp == offset ) {
			if( outRow != NULL )
				*outRow = row;
			if( outCol != NULL )
				*outCol = iLine - w->indent;
			return;
		}
                if( firstByte > lastByte ) {
                        if( getSpan( fid, cp, &firstByte, &lastByte, 0 ) )
                                break;
                }
                ++cp;
		switch( *firstByte++ ) {
		case '\n':
			iLine = 0;
			++row;
			break;
		case '\t':
			tabStop = iLine + tabWidth - (iLine % tabWidth);
			iLine = tabStop;
			break;
		default:
			iLine++;
			break;
		}
++getSpanSize;
	}
NotFound:
	if( outRow != NULL )
		*outRow = -1;
	if( outCol != NULL )
		*outCol = -1;
	return;
}

int
OffsetToCol( w, offset, beginRowOffset )
	struct window *w;
	Offset offset, beginRowOffset;
{
	extern int tabWidth;

	Offset cp;
	int tabStop, iLine;
	int uch;
	int fid = w->fileId;

	iLine = 0;
	cp = beginRowOffset;
	while( cp < offset ) {
		uch = getFileByte( fid, cp++ );
		switch( uch ) {
		case BLOCK_EOF:	/* not found */
		case '\n':
			break;
		case '\t':
			tabStop = iLine + tabWidth - (iLine % tabWidth);
			iLine = tabStop;
			break;
		default:
			iLine++;
			break;
		}
	}
	return iLine;
}
