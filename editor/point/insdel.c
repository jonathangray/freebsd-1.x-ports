/* $Header: /a/cvs/386BSD/ports/editor/point/insdel.c,v 1.1 1994/02/15 22:12:38 jkh Exp $ */

#include "pt.h"
#include <stdio.h>

/* the globals additions file */
int addHandle;
Offset addPosition;

void
insertChar(c)
	int c;
{
	extern struct window *selWindow;
	extern Offset selBegin, selEnd;
	extern char msgBuffer[];
	extern struct openFile *files;
	extern Offset addPosition;
	extern int addHandle;
	extern int trace_file;

	struct changeItem *thisChange;
	struct openFile *ff;
	Piece pp, pp2, thispp, nextpp, thirdpp;
	Offset nn, offset;
	unsigned char ch = (unsigned char)c;
	
	if( selWindow == NULL )
		return;

	ff = &files[selWindow->fileId];

#ifdef HYPERTEXT
	if( hypertextOn && selWindow->document != NULL ) {
		int ret;

		/* convert selBegin to the underlying (real) file */
		oldSelBegin = selBegin;
		ret = GetRealSelection( ff, 1 /* get selBegin only */ );

		/* check for insertion in decoration */
		if( ret & 1 ) {
			printf(
"insertChar ERROR: cannot insert into synthetic text\n");
			return;
		}

		/* free the piece list of the old view */
		FreeOldViewPieces( ff );

		/* fix up the fileId and ff */
		selWindow->fileId = selWindow->realFileId;
		ff = &files[selWindow->fileId];
	}
#endif

	/* check if this is a readOnly file */
	if( ff->flags & READ_ONLY ) {
		sprintf(msgBuffer, "File %s is read only", ff->origName);
		msg(msgBuffer, 1 );
		return;
	}

	/* find out what piece 'selBegin' is in */
	pp = findPiece( selBegin, ff, &nn );

	offset = selBegin - nn;
	if( offset > 0 ) {	/* we must split the piece */
		/* we insert two new pieces */
		/* nextpp is for the character we are inserting */
		/* thirdpp is the split of portion of pp */
		
		/* update the fields of the new piece */
		nextpp = getFreePiece();
		nextpp->file = addHandle;
		nextpp->position = addPosition;
		nextpp->length = 1;

		/* update the fields of the split off piece */
		thirdpp = getFreePiece();
		thirdpp->file = pp->file;
		thirdpp->position = pp->position + offset;
		thirdpp->length = pp->length - offset;

		/* update the fields of the original piece */
		/* pp->file remains the same */
		/* pp->position remains the same */
		pp->length = offset;
		
		/* link everything together again */
		nextpp->nextPiece = thirdpp;
		thirdpp->prevPiece = nextpp;
		pp2 = pp->nextPiece;
		thirdpp->nextPiece = pp2;
		if( pp2 != NULL )
			pp2->prevPiece = thirdpp;
		nextpp->prevPiece = pp;
		pp->nextPiece = nextpp;

		/* make the third piece the cached one */
		ff->logPiece = thirdpp;
		ff->loLogPiece = selBegin + 1;
		ff->hiLogPiece = selBegin + thirdpp->length;
	} else {	/* put in front of this piece */
		/* can we put it at the end of the previous piece? */
		thispp = pp->prevPiece;
		/* if (1) there IS a previous piece and */
		/*    (2) it is a piece in the add file and */
		/*    (3) it is just before the piece we are adding */
		/* then we coalsce the two pieces */
		if( (thispp != NULL)	/* pp is not the first piece */
		 && (thispp->file == addHandle)	/* in the add file */
		 && (thispp->position+thispp->length == addPosition)
		) {
			++(thispp->length);	/* simply adjust the length */
			ff->logPiece = pp;
			ff->loLogPiece = nn + 1;
			ff->hiLogPiece = nn + pp->length;
		} else {
			/* create a new piece for this character */
			thispp = getFreePiece();
			thispp->file = addHandle;
			thispp->position = addPosition;
			thispp->length = 1;
			thispp->nextPiece = pp;
			pp2 = pp->prevPiece;
			pp->prevPiece = thispp;
			thispp->prevPiece = pp2;
			if( pp2 != NULL )
				pp2->nextPiece = thispp;
			else
				ff->pieceList = thispp;
			ff->logPiece = thispp;
			ff->loLogPiece = selBegin;
			ff->hiLogPiece = selBegin;
		}
	}

	/* record in the change history */
	/* see if we can add this to the last insert */
	thisChange = GetCurrentChange( ff );
	if( thisChange->type == CINSERT
	 && thisChange->fileId == selWindow->fileId
	 && selBegin == thisChange->position+thisChange->length ) {
		++(thisChange->length);
		++(thisChange->firstPiece->length);
	} else {
		/* record in the change history */
		thisChange = GetNewChange( ff );
		thisChange->type = CINSERT;
		thisChange->position = selBegin;
		thisChange->length = 1;
		thisChange->flags = 0;
		thisChange->lineNumber = selWindow->numTopline;
		thisChange->fileId = selWindow->fileId;
		thisChange->w = selWindow;
		pp = getFreePiece();
		thisChange->firstPiece = pp;
		pp->file = addHandle;
		pp->position = addPosition;
		pp->length = 1;
		RecordChange( ff, thisChange );
	}

	if( trace_file > 0 ) {
		sprintf( msgBuffer, "I %2d %5d %5d\n",
			selWindow->fileId, selBegin, 1 );
		write( trace_file, msgBuffer, strlen(msgBuffer) );
	}

	/* add one character to the file */
	ff->fileSize += 1;
	writeChar(ch, addPosition++);

	/* invalidate the buffer cache */
	ff->hiLogBuffer = -1;
	ff->loLogBuffer = -1;

	/* record the fact that the file has changed */
	if( !(ff->flags & IS_CHANGED) ) {
		ff->flags |= IS_CHANGED;
		NewOpenList();
		banner( selWindow, 0 );
	}

	/* adjust window data even though we do not redraw */
	updateTops( selWindow->fileId, selBegin, 1, (c!='\n') );

	/* invalidate the last-row-found cache */
	if( selWindow->posCurLast > selBegin )
		selWindow->lastPosTop = -1;
	
	/* move the selection to char past new char */
	selEnd = ++selBegin;

#ifdef HYPERTEXT
	if( hypertextOn && selWindow->document != NULL ) {
		/* create a new piece table for the view */
		selBegin = selEnd = ++oldSelBegin;
		selWindow->realFileId = selWindow->fileId;
		selWindow->fileId = CreateViewFile( selWindow );
		drawWindow( selWindow );
	}
#endif
	/* NOTE -- THE SELECTION WILL BE WRONG. FIX THIS LATER */
}

int
delChar()
{
	extern struct window *selWindow;
	extern Offset selBegin, selEnd;
	extern char msgBuffer[];
	extern struct openFile *files;
	extern Offset addPosition;
	extern int addHandle;

	Offset nn, offset;
	struct openFile *ff;
	Piece pp, thispp;
	struct changeItem *currentChange;
	char ch;

	if( selWindow == NULL )
		return 0;

	/* find out what piece 'selBegin' is in */
	ff = &files[selWindow->fileId];
	pp = findPiece(selBegin, ff, &nn);

	/* check if this is a readOnly file */
	if( ff->flags & READ_ONLY ) {
		sprintf(msgBuffer, "File %s is read only", ff->origName);
		msg(msgBuffer, 1 );
		return 0;
	}

	offset = selBegin - nn;
	if( offset > 0 )
		return 0;

	/* get the char deleted (for updateTops) */
	ch = getFileByte( selWindow->fileId, selBegin );

	/* is the char to delete at the end of the previous piece? */
	thispp = pp->prevPiece;
	/* if (1) there IS a previous piece and */
	/*    (2) it is a piece in the add file and */
	/*    (3) it is just before the piece we are adding and */
	/*    (4) we are not deleting the last char in the piece */
	/*    (5) the preceding change was an insert */
	/* then we coalsce the two pieces */
	currentChange = GetCurrentChange( ff );
	if( (thispp != NULL)	/* pp is not the first piece */
	 && (thispp->file == addHandle)	/* in the add file */
	 && (thispp->position+thispp->length == addPosition)
	 && (thispp->length >= 1)
	 && (currentChange->type == CINSERT)
	) {
		--(thispp->length);	/* simply adjust the length */
		ff->logPiece = pp;
		ff->loLogPiece = nn - 1;
		ff->hiLogPiece = nn + pp->length - 1;
	} else
		return 0;

	/* record in the change history */
	/* see if we can add this to the last insert */
	if( currentChange->type == CINSERT
	 && currentChange->fileId == selWindow->fileId
	 && selBegin == currentChange->position+currentChange->length ) {
		--(currentChange->length);
		--(currentChange->firstPiece->length);
	} else {
		++(thispp->length);	/* re-adjust the length */
		return 0;
	}

	/* subtract one character to the file */
	ff->fileSize -= 1;
	--addPosition;

	/* invalidate the buffer cache */
	ff->hiLogBuffer = -1;
	ff->loLogBuffer = -1;

	/* record the fact that the file has changed */
	if( !(ff->flags & IS_CHANGED) ) {
		ff->flags |= IS_CHANGED;
		NewOpenList();
		banner( selWindow, 0 );
	}

	/* adjust window data even though we do not redraw */
	updateTops( selWindow->fileId, selBegin, 1, (ch!='\n') );

	/* invalidate the last-row-found cache */
	if( selWindow->posCurLast > selBegin )
		selWindow->lastPosTop = -1;

	selEnd = --selBegin;
	return 1;
}

int
deleteChars(fileId, update, toScrap)
	/* toScrap=0 --> do not put in the scrap */
	/* toScrap=1 --> do put in the scrap */
	/* toScrap=2 --> do not put in the scrap or the history */
	int fileId, toScrap;
	DoUpdate update;
{
	extern char msgBuffer[];
	extern Offset selBegin, selEnd;
	extern struct window *selWindow;
	extern struct changeItem scrapBuffer;
	extern int selMode;
	extern int scrapMode;
	extern Offset addPosition;
	extern struct openFile *files;
	extern int addHandle;
	extern int trace_file;

	struct openFile *ff = &files[fileId];
	Offset sb, nn, curPos, nextPos, offset, delLength;
	int wasLF;
	Piece pp;
	Piece pp2, firstPP, nextPP, lastPP;
	struct changeItem *thisChange;
	
	if( selWindow == NULL )
		return 0;

#ifdef HYPERTEXT
	if( hypertextOn && selWindow->document != NULL ) {
		int ret;

		/* convert selBegin and selEnd to the underlying (real) file */
		oldSelBegin = selBegin;
		ret = GetRealSelection( ff, 0 /* get selBegin and selEnd */ );

		/* check for insertion in decoration */
		if( ret & 1 ) {
			printf(
"deleteChars ERROR: cannot delete from synthetic text\n");
/* LATER: just adjust to the first real text */
			return 0;
		}

		/* LATER: go through and only delete real text */
		/* NOT any block markers */

		/* free the piece list of the old view */
		FreeOldViewPieces( ff );

		/* fix up the fileId and ff */
		selWindow->fileId = selWindow->realFileId;
		ff = &files[selWindow->fileId];
	}
#endif
	nn = ff->fileSize;

	/* check if this is a readOnly file */
	if( ff->flags & READ_ONLY ) {
		sprintf(msgBuffer, "File %s is read only", ff->origName);
		msg(msgBuffer, 1 );
		return 0;
	}

	/* eliminate the EOF marker from the selection */
	if( selEnd >= nn ) {
		if( selBegin < nn )
			selEnd = nn-1;
		else	/* only the EOF symbol is selected */
			return 0;
	}

	delLength = selEnd - selBegin + 1;

	/* see if we are deleting a newline */
	if( delLength < 100 ) {	/* lines are usally < 100 chars */
		wasLF = 0;
		sb = selBegin;
		while( sb <= selEnd )
			if( (char)getFileByte( fileId, sb++ ) == '\n' ) {
				wasLF = 1;
				break;
			}
	} else	/* assume there is one -- too many to search */
		wasLF = 1;

	/* find out what piece 'selBegin' is in */
	firstPP = findPiece( selBegin, ff, &curPos);

/* first see if we have to split pp */
offset = selBegin - curPos;
if( offset > 0 ) {	/* delete starts inside this piece */
	/* split firstPP at selBegin */
	/* that is, get a new piece and adjust all the fields */
	nextPP = getFreePiece();
	nextPP->file = firstPP->file;
	nextPP->position = firstPP->position + offset;
	nextPP->length = firstPP->length - offset;
	firstPP->length = offset;
	nextPP->nextPiece = firstPP->nextPiece;
	nextPP->prevPiece = firstPP;
	if( (pp = firstPP->nextPiece) != NULL )
		pp->prevPiece = nextPP;
	firstPP->nextPiece = nextPP;
	firstPP = nextPP;
	curPos += offset;
}

/* Now the delete begins at the first byte of firstPP */
/* See where the last piece is */
lastPP = firstPP;
while( 1 ) {
	/* does the selection end in this piece? */
	nextPos = curPos + lastPP->length;
	if( nextPos > selEnd )
		break;
	curPos = nextPos;
	lastPP = lastPP->nextPiece;
}

/* now we see if we have to split this piece */
if( selEnd < nextPos-1 ) {	/* delete ends inside this piece */
	/* split lastPP at selEnd */
	/* that is, get a new piece and adjust all the fields */
	nextPP = getFreePiece();
	nextPP->file = lastPP->file;
	offset = selEnd - curPos + 1;
	nextPP->position = lastPP->position + offset;
	nextPP->length = lastPP->length - offset;
	lastPP->length = offset;
	pp = lastPP->nextPiece;
	nextPP->nextPiece = pp;
	nextPP->prevPiece = lastPP;
	if( pp != NULL )
		pp->prevPiece = nextPP;
	lastPP->nextPiece = nextPP;
}

/* Now the selection has been isolated in pieces firstPP (which begins */
/* with selBegin) and lastPP (which ends with selEnd) */
/* Now just link them out of this piece table */
if( (pp = firstPP->prevPiece) != NULL ) {
	nextPP = lastPP->nextPiece;
	pp->nextPiece = nextPP;
	ff->logPiece = pp;
	ff->loLogPiece = selBegin - pp->length;
	if( nextPP != NULL )
		nextPP->prevPiece = firstPP->prevPiece;
	/* see if we can combine the two pieces now together */
	if( (nextPP != NULL) && (pp->file == nextPP->file)
	 && ((pp->position+pp->length) == nextPP->position) ) {
		pp->length += nextPP->length;	/* add to first piece */
		/* link the second piece out of the list */
		pp2 = nextPP->nextPiece;
		pp->nextPiece = pp2;
		if( pp2 != NULL )
			pp2->prevPiece = pp;
		/* isolate the one piece and free is (as a chain) */
		nextPP->nextPiece = nextPP->prevPiece = NULL;
		freePieces(nextPP);
	}
} else {	/* this delete is at the beginning of the file and there */
		/* are no pieces in front of firstPiece */
	pp = lastPP->nextPiece;
	ff->pieceList = pp;
	if( pp != NULL ) {
		pp->prevPiece = NULL;
		ff->logPiece = pp;
	} else {
		/* the piece table is empty so put a 0 length piece in it */
		/* it is convenient to be able to assume (in other parts */
		/* of the editor) that a piece table always contains at */
		/* least one piece */
		pp = getFreePiece();
		pp->file = addHandle;
		pp->position = addPosition;
		pp->length = 0;
		ff->pieceList = pp;
		ff->logPiece = pp;
	}
	ff->loLogPiece = 0;
}

	/* make this an independent chain */
	firstPP->prevPiece = NULL;
	lastPP->nextPiece = NULL;

	/* put it into the history */
	if( toScrap != 2 ) {
		/* record in the change history */
		thisChange = GetNewChange( ff );
		thisChange->type = CDELETE;
		thisChange->position = selBegin;
		thisChange->length = delLength;
		thisChange->flags = 0;
		thisChange->lineNumber = selWindow->numTopline;
		thisChange->fileId = fileId;
		thisChange->w = selWindow;
		thisChange->firstPiece = firstPP;
		RecordChange( ff, thisChange );
	}

	if( trace_file > 0 ) {
		sprintf( msgBuffer, "D %2d %5d %5d\n",
			fileId, selBegin, delLength );
		write( trace_file, msgBuffer, strlen(msgBuffer) );
	}

	if( toScrap == 1 ) {
		freePieces(scrapBuffer.firstPiece);
		scrapBuffer.length = delLength;
		scrapBuffer.type = 0;
		scrapBuffer.fileId = ff->origHandle;
		scrapBuffer.firstPiece = dupPieces(firstPP);
		scrapMode = selMode;
	} else if( toScrap == 2 )
		freePieces(firstPP);

	/* update the cached piece */
	ff->hiLogPiece = ff->loLogPiece + (ff->logPiece)->length - 1;
	if( ff->hiLogPiece > ff->fileSize )
		/* this could happen if we just deleted the last piece */
		/* in the piece table */
		ff->hiLogPiece = ff->fileSize;

	/* invalidate the buffer cache */
	ff->hiLogBuffer = -1;
	ff->loLogBuffer = -1;

	/* record the fact that the file has changed */
	if( !(ff->flags & IS_CHANGED) ) {
		ff->flags |= IS_CHANGED;
		NewOpenList();
		banner( selWindow, 0 );
	}

	/* invalidate the last-row-found cache */
	if( selWindow->posCurLast > selBegin )
		selWindow->lastPosTop = -1;
		
	/* update the file length */
	ff->fileSize -= delLength;

	/* the character after the deleted characters is the new selection */
	selEnd = selBegin;

	/* entend the selection according to the selection mode */
	{ int row, col, n = -1;
	Offset beginRowCp;
	OffsetToXY( selWindow, selBegin, &row, &col );
	beginRowCp = prevLine( selWindow->fileId, selBegin, &n );
	modeExtend( selWindow, selBegin, row, col, beginRowCp );
	}

#ifdef HYPERTEXT
	if( hypertextOn && selWindow->document != NULL ) {
		/* create a new piece table for the view */
		selBegin = selEnd = oldSelBegin;
		selWindow->realFileId = selWindow->fileId;
		selWindow->fileId = CreateViewFile( selWindow );
		drawWindow( selWindow );
	}
#endif

	if( update == UPDATEWINDOWS ) {
		/* redraw all the affected windows */
		updateFile( fileId, selBegin, -delLength, !wasLF );
	} else {
		/* adjust window data even though we do not redraw */
		updateTops( fileId, selBegin, -delLength, !wasLF );
	}

	return wasLF;
}
