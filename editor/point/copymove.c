/* $Header: /a/cvs/386BSD/ports/editor/point/copymove.c,v 1.1 1994/02/15 22:12:36 jkh Exp $ */

#include <stdio.h>
#include "pt.h"

extern int addHandle;
extern Offset addPosition;

/* the scrap buffer */
struct changeItem scrapBuffer;
int scrapMode = SELCHAR;

void
updateFile( fileId, offset, len, oneLineOnly )
	int fileId;
	Offset offset;
	Offset len;
	int oneLineOnly;
{
	extern struct window *windowList;

	register struct window *w;
	Offset topOffset;
	int n, col, firstRow, lastRow;
	int fid;

	/* redraw the affected windows */
	w = windowList;
	while( w != NULL ) {
		fid = w->fileId;
		if( fid == fileId ) {
			/* repaint the window as necessary */
			if( offset <= w->posBotline )
				w->posBotline += len;
			else
				goto nextWindow;
			topOffset = w->posTopline;
			if( offset < topOffset ) {
				/* remember: len < 0 for a delete */
				/* a delete overlapping this window? */
				if( offset > (topOffset+len) ) {
					n = -1;
					w->posTopline = prevLine( fid,
							topOffset, &n );
				}
				if( !oneLineOnly ) {
					/* recalculate the line number by */
					/* letting prevLine count as far back */
					/* as it can */
					n = 3000000;
					(void)prevLine(fid, w->posTopline, &n);
					w->numTopline = n + 1;
				}
			}
			OffsetToXY( w, offset, &firstRow, &col );
			if( oneLineOnly ) {
				if( len > 1 ) {
					/* len > 0 ==> an insert */
					OffsetToXY( w, offset+len-1, &lastRow,
									&col );
					/* this is the case where we are at */
					/* the end of the file and so */
					/* offset+len-1 is past the EOF */
					if( lastRow < 0 )
						lastRow = firstRow;
				} else {
					lastRow = firstRow;
				}
			} else {
				lastRow = w->nRows - 1;
			}
			if( firstRow != -1 )
				drawWindowFast(w, firstRow, lastRow,
						0, w->nCols-1, !oneLineOnly );
		}
	nextWindow:
		w = w->nextWindow;
	}
}

void
updateTops(fileId, offset, len, oneLineOnly )
	int fileId;
	Offset offset;
	Offset len;
	int oneLineOnly;
{
	extern struct window *windowList;

	register struct window *w;
	int n;
	Offset topOffset;
	int fid;

	/* adjust the data of the affected windows */
	w = windowList;
	while( w != NULL ) {
		fid = w->fileId;
		if( fid == fileId ) {
			if( offset < w->posBotline )
				w->posBotline += len;
			else
				goto nextWindow;
			topOffset = w->posTopline;
			if( offset < topOffset ) {
				/* a delete overlapping this window? */
				if( offset > (topOffset+len) ) {
					n = -1;
					w->posTopline = prevLine( fid,
							topOffset, &n);
				}
				if( !oneLineOnly ) {
					/* recalculate the line number by */
					/* letting prevLine count as far back */
					/* as it can */
					n = 3000000;
					(void)prevLine(fid, w->posTopline, &n);
					w->numTopline = n + 1;
				}
			}
		}
	nextWindow:
		w = w->nextWindow;
	}
}

void
exchWithScrap()
{
	extern char msgBuffer[];
	extern struct changeItem scrapBuffer;
	extern Offset selBegin;
	extern struct window *selWindow;
	extern struct openFile *files;

	Offset length;
	struct changeItem *thisChange;
	Piece firstPiece;
	struct openFile * ff;

	if( selWindow == NULL )
		return;

	ff = &files[selWindow->fileId];

	/* check if this is a readOnly file */
	if( ff->flags & READ_ONLY ) {
		sprintf(msgBuffer, "File %s is read only", ff->origName);
		msg(msgBuffer, 1 );
		return;
	}

	/* remember what was in the scrap */
	firstPiece = scrapBuffer.firstPiece;
	length = scrapBuffer.length;
	/* prevent this from getting freed when the scrap buffer is reused */
	scrapBuffer.firstPiece = NULL;
	
	/* move the selection into the scrap */
	(void)deleteChars( selWindow->fileId, NOUPDATE, 1 );

	/* record in the change history */
	thisChange = GetNewChange( ff );
	thisChange->type = CINSERT;
	thisChange->w = selWindow;
	thisChange->flags = 0;
	thisChange->position = selBegin;
	thisChange->length = length;
	thisChange->lineNumber = selWindow->numTopline;
	thisChange->fileId = selWindow->fileId;
	thisChange->firstPiece = firstPiece;
	RecordChange( ff, thisChange );

	/* copy the old scrap into the file */
	copyPieces( firstPiece, selWindow, selBegin, length, 1, 0 );
	
	/* free the scrap pieces */
	freePieces(firstPiece);
}

void
copyToScrap(fromWindow, fromBegin, fromEnd)
	struct window *fromWindow;
	Offset fromBegin, fromEnd;
{
	extern struct changeItem scrapBuffer;
	extern struct window *selWindow;
	extern int selMode;
	extern int scrapMode;

	int fromId;
	Offset fb, copyLength;
	Piece tempPP;

	fromId = fromWindow->fileId;
	
	/* eliminate the EOF marker from the selection */
	fb = fileSize(fromId);
	if( fromEnd >= fb ) {
		if( fromBegin < fb )
			fromEnd = fb-1;
		else	/* only the EOF symbol is selected */
			return;
	}
	copyLength = fromEnd - fromBegin + 1;

	/* free the old scrap buffer pieces */
	freePieces(scrapBuffer.firstPiece);

	/* record in the scrap buffer */
	scrapBuffer.type = 1;
	scrapBuffer.length = copyLength;
	scrapBuffer.w = selWindow;
	scrapBuffer.flags = 0;
	tempPP = getFreePiece();
	scrapBuffer.firstPiece = tempPP;
	scrapMode = selMode;

	tempPP->file = addHandle;
	tempPP->position = addPosition;
	fb = fromBegin;
	while( fb <= fromEnd ) {
		writeChar( (char)getFileByte(fromId, fb++), addPosition++);
	}
	tempPP->length = copyLength;
}

void
insScrap( doInsert, update )
	int doInsert;
	int update;
{
	extern char msgBuffer[];
	extern struct window *selWindow;
	extern Offset selBegin;
	extern struct changeItem scrapBuffer;
	extern struct openFile *files;
 	extern int selMode;
	extern int scrapMode;
	extern int maxFiles;

	Offset limit, logByte;
	Piece tempPP, oldScrap;
	struct openFile *ff;
	struct changeItem *thisChange;

	if( selWindow == NULL )
		return;

	/* check if this is a readOnly file */
	if( files[selWindow->fileId].flags & READ_ONLY ) {
		sprintf(msgBuffer, "File %s is read only",
			files[selWindow->fileId].origName);
		msg(msgBuffer, 1 );
		return;
	}

	/* See if the text in the scrap buffer is in an edit file rather */
	/* then in the add file.  If it is we need to copy it to the add */
	/* file. */
	if( !scrapBuffer.type ) {
		/* keep a pointer to the old piece list */
		oldScrap = scrapBuffer.firstPiece;
		/* scrapBuffer.length will not change */
		
		/* get a new piece and initialize the fields */
		tempPP = getFreePiece();
		tempPP->file = addHandle;
		tempPP->position = addPosition;
		tempPP->length = scrapBuffer.length;
		
		/* this will be the new scrapBuffer piece list */
		scrapBuffer.type = 1;  /* now it is addFile only type */
		scrapBuffer.firstPiece = tempPP;
		
		/* Now copy the characters into the add file */
		logByte = 0;
		limit = scrapBuffer.length;

		/* simulate a file this was deleted from */
		ff = &files[maxFiles];
		ff->hiLogBuffer = -1;
		ff->loLogBuffer = -1;
		ff->origHandle = scrapBuffer.fileId;
		ff->fileSize = limit;
		ff->logPiece = oldScrap;
		ff->loLogPiece = 0;
		ff->hiLogPiece = oldScrap->length - 1;
		while( logByte < limit ) {
			/* copy the characters in pp to the add file */
			writeChar( (char)getFileByte(maxFiles, logByte++),
				addPosition++);
		}
		
		freePieces(oldScrap);  /* free the old piece chain */
	}
	if( doInsert ) {
		/* record in the change history */
		ff = &files[selWindow->fileId];
		thisChange = GetNewChange( ff );
		thisChange->type = CCOPY;
		selMode = scrapMode;
		selBegin = adjustSelMode( selBegin );
		thisChange->position = selBegin;
		thisChange->length = scrapBuffer.length;
		thisChange->lineNumber = selWindow->numTopline;
		thisChange->fileId = selWindow->fileId;
		thisChange->w = selWindow;
		thisChange->flags = 0;
		thisChange->firstPiece = dupPieces(scrapBuffer.firstPiece);
		RecordChange( ff, thisChange );

		copyPieces( scrapBuffer.firstPiece, selWindow, selBegin,
			scrapBuffer.length, update, 0 );
	}
}

void
copyMove(fromWindow, fromBegin, fromEnd, toWindow, toPosition, mode)
	struct window *fromWindow, *toWindow;
	Offset fromBegin, fromEnd, toPosition;
	int mode;
{
	extern char msgBuffer[];
	extern struct openFile *files;
	extern Offset selBegin, selEnd;
	extern struct window *selWindow;

	int n, fromId, toId;
	Offset  fb, copyLength;
	Piece tempPP;
	struct changeItem *thisChange;
	struct changeItem *currentChange;
	int wasCR;
	char ch;
	struct openFile * ff = &files[toWindow->fileId];

	if( selWindow == NULL )
		return;

	/* check if this is a readOnly file */
	if( ff->flags & READ_ONLY ) {
		sprintf(msgBuffer, "File %s is read only",ff->origName);
		msg(msgBuffer, 1 );
		return;
	}

	/* Is this a move with source and destination ovelapping? */
	if( mode==MOVE && fromBegin<=toPosition && toPosition<=fromEnd
						&& fromWindow==toWindow ) {
		msg("Cannot move text into itself", 1 );
		return;
	}

	fromId = fromWindow->fileId;
	toId = toWindow->fileId;

	/* eliminate the EOF marker from the selection */
	fb = fileSize(fromId);
	if( fromEnd >= fb ) {
		if( fromBegin < fb )
			fromEnd = fb-1;
		else	/* only the EOF symbol is selected */
			return;
	}
	copyLength = fromEnd - fromBegin + 1;

	/* record in the change history */
	thisChange = GetNewChange( ff );
	if( mode == COPY )
		thisChange->type = CCOPY;
	else
		thisChange->type = CMOVE;
	thisChange->position = toPosition;
	thisChange->length = copyLength;
	thisChange->lineNumber = selWindow->numTopline;
	thisChange->fileId = toId;
	thisChange->w = selWindow;
	thisChange->flags = 0;
	tempPP = getFreePiece();
	thisChange->firstPiece = tempPP;
	RecordChange( ff, thisChange );

	tempPP->file = addHandle;
	tempPP->position = addPosition;
	fb = fromBegin;
	wasCR = 0;
	while( fb <= fromEnd ) {
		ch = (char)getFileByte(fromId, fb++);
		if( ch == '\n' )
			wasCR = 1;
		writeChar( ch, addPosition++);
	}
	tempPP->length = copyLength;

	/* if it is a move, then delete the from text */
	if( mode == MOVE ) {
		selWindow = fromWindow;
		selBegin = fromBegin;
		selEnd = fromEnd;
#ifdef XXXXXXXX
		if( fromWindow == toWindow )
			n = NOUPDATE;
		else
#endif
			n = UPDATEWINDOWS;
		(void)deleteChars( fromWindow->fileId, n, 1 );
		/* we're going to change the selection in copyPieces */
		/* to the character after the moved text. */
		drawSelection( 1 );
		/* if we deleted text before the toPosition we have */
		/* to adjust it since the logical text positions change */
		/* after a delete.  We also fix up the position on the */
		/* delete following so it will look like it really did */
		/* take place AFTER the copying in of the moved text */
		if( fromWindow->fileId == toWindow->fileId ) {
			if( toPosition > fromBegin )
				toPosition -= copyLength;
			else {
				currentChange = GetCurrentChange( ff );
				currentChange->position += copyLength;
			}
		}
	}

	copyPieces(thisChange->firstPiece, toWindow, toPosition, copyLength,
						1, !wasCR );
}

void
copyPieces(fromPP, toWindow, toPosition, copyLength, update, oneLineOnly)
	Piece fromPP;
	struct window *toWindow;
	Offset toPosition, copyLength;
	int update;
	int oneLineOnly;
{
	extern char msgBuffer[];
	extern struct openFile *files;
	extern Offset selBegin, selEnd;
	extern int selMode;
	extern struct window *selWindow;
	extern int trace_file;

	int toId;
	Offset offset, toNN;
	Piece nextPP, toPP, lastPP;
	struct openFile *toFF;
	Piece pp2;
	struct window *w;

	if( trace_file > 0 ) {
		sprintf( msgBuffer, "C %2d %5d %5d\n",
			toWindow->fileId, toPosition, copyLength);
		write( trace_file, msgBuffer, strlen(msgBuffer) );
	}
	if( update ) {
		/* just using these variables to save the selection */
		offset = selBegin;
		toNN = selEnd;
		w = selWindow;
		selBegin = selEnd = toPosition;
		selWindow = toWindow;
		selBegin = offset;
		selEnd = toNN;
		selWindow = w;
	}
	
	/* This is a special case to handle a backspace problem.  To wit, */
	/* if we delete text and type in new text, then select other text */
	/* and AGAIN, then the piece from the first change is copied in. */
	/* If we then backspace and type a new character, the piece itself */
	/* is changed and so the orignal edit is changed also.  We have to */
	/* prevent this so we bump addPosition in this case so the */
	/* backspace does not change the piece */
	/* A BETTER SOLUTION would be to make an actual copy of the text */
	/* in the piece.  This would allow further editing that would then */
	/* be able to be repeated again. */
	toPP = fromPP;		/* just using toPP as a convenient variable */
	while( toPP->nextPiece != NULL )
		toPP = toPP->nextPiece;
	if( (toPP->file == addHandle)
	 && (toPP->position+toPP->length == addPosition) )
	 	++addPosition;
		
	/* compute some values for later efficiency */
	toId = toWindow->fileId;
	toFF = &files[toId];

	/* find out what piece 'toBegin' is in */
	toPP = findPiece(toPosition, toFF, &toNN);
	/* special case for the end of the file */
	if( toPosition > toFF->fileSize ) {
		/* leave toPP as the last piece in the file */
		nextPP = NULL;
	} else if( toPosition > toNN ) {
		/* copied text starts inside piece "toPP" */
		/* We split toPP into two pieces (toPP and nextPP) */
		/* at toPosition */
		/* get a new piece */
		nextPP = getFreePiece();
		/* link the new piece in before the piece after toPP */
		pp2 = toPP->nextPiece;
		nextPP->nextPiece = pp2;
		if( pp2 != NULL )
			pp2->prevPiece = nextPP;
		/* adjust the fields of the two pieces */
		offset = toPosition - toNN;
		nextPP->file = toPP->file;
		nextPP->position = toPP->position + offset;
		nextPP->length = toPP->length - offset;
		toPP->length = offset;
	} else {
		nextPP = toPP;
		toPP = toPP->prevPiece;
	}
	
	/* now copy the pieces into the piece list between toPP and nextPP */
	lastPP = toPP;
	while( fromPP != NULL ) {
		pp2 = getFreePiece();
		/* copy the information into the new piece */
		pp2->file = fromPP->file;
		pp2->position = fromPP->position;
		pp2->length = fromPP->length;
		/* link in the new piece */
		pp2->prevPiece = lastPP;
		/* link it into the list */
		if( lastPP != NULL )
			lastPP->nextPiece = pp2;
		else /* new first item on the list */
			toFF->pieceList = pp2;
		/* move to the next piece to copy */
		lastPP = pp2;
		fromPP = fromPP->nextPiece;
	}
	lastPP->nextPiece = nextPP;
	if( nextPP != NULL )
		nextPP->prevPiece = lastPP;

	/* make the cached piece the one after the copied pieces */
	/* unless it is NULL */
	if( nextPP != NULL ) {
		toFF->logPiece = nextPP;
		toFF->loLogPiece = toPosition + copyLength;
		toFF->hiLogPiece = toFF->loLogPiece + nextPP->length - 1;
	} else {	/* make it the first piece in the file */
		toFF->logPiece = toFF->pieceList;
		toFF->loLogPiece = 0;
		toFF->hiLogPiece = toFF->loLogPiece
					+ (toFF->logPiece)->length - 1;
	}

	/* update the length of the "to" file */
	toFF->fileSize += copyLength;

	/* invalidate the buffer cache of the "to" file */
	toFF->hiLogBuffer = -1;
	toFF->loLogBuffer = -1;

	/* record the fact that the file has changed */
	if( !(toFF->flags & IS_CHANGED) ) {
		toFF->flags |= IS_CHANGED;
		NewOpenList();
		banner( toWindow, 0 );
	}

	/* invalidate the last-row-found cache */
	if( toWindow->posCurLast > toPosition )
		toWindow->lastPosTop = -1;
	
	/* make the selection the character past the copied text */
	if( selWindow != toWindow ) {
		drawSelection( 1 );
		selWindow = toWindow;
	}
	selEnd = selBegin = toPosition + copyLength;

	/* change selection mode to character mode */
	selMode = SELCHAR;
	if( update )
		updateFile(toId, toPosition, copyLength, oneLineOnly);
}
