/* $Header: /a/cvs/386BSD/ports/editor/point/piece.c,v 1.1 1994/02/15 22:12:39 jkh Exp $ */

#include "pt.h"

/* piece table */
Piece freePList;
unsigned int bytesLeft;
unsigned int piecesLeft;

Piece 
dupPieces(pp)
	Piece pp;
{
	Piece pp2;
	Piece lastpp, retpp;

	lastpp = retpp = NULL;
	while( pp != NULL ) {
		pp2 = getFreePiece();
		/* link to the previous piece */
		if( lastpp != NULL )
			lastpp->nextPiece = pp2;
		else	/* first time through, remember the first piece */
			retpp = pp2;
		pp2->prevPiece = lastpp;
		/* copy the field values */
		pp2->file = pp->file;
		pp2->position = pp->position;
		pp2->length = pp->length;
		lastpp = pp2;
		pp = pp->nextPiece;
	}
	/* pp2->nextPiece == NULL already since getFreePiece does that */
	return retpp;
}

Piece 
getFreePiece()
{
	extern Piece freePList;
	extern unsigned int piecesLeft;
	extern int piecesRequested;
	extern int piecesAllocated;
	
	Piece pp;
	
++piecesRequested;
	pp = freePList;
	if( pp != NULL ) {
		--piecesLeft;
		freePList = freePList->nextPiece;
	} else { /* we have to allocate a piece structure from the free space */
++piecesAllocated;
		pp = (Piece)PtMalloc(sizeof(struct piece), "piece" );
	}
	pp->nextPiece = pp->prevPiece = NULL;
	pp->flags = 0;
	pp->blockID = NullObject;
	pp->nextClone = pp;
	return pp;
}

void
freePieces(pp)
	Piece pp;
{
	extern Piece freePList;
	extern unsigned int piecesLeft;
	extern int piecesFreed;
	
	Piece pp2;
	
	if( pp == NULL )
		return;
++piecesFreed;
	pp2 = pp;
	while( pp2->nextPiece != NULL ) {
		pp2 = pp2->nextPiece;
		++piecesLeft;
	}
	pp2->nextPiece = freePList;
	freePList = pp;
	++piecesLeft;
}

Piece 
findPiece(logPos, ff, beginLogPos)
	Offset logPos, *beginLogPos;
	struct openFile *ff;
{
	extern int piecesSearchedFor;
	extern int piecesScanned;
	extern int scanned0pieces;
	extern int scanned1pieces;
	extern int scanned2pieces;
	extern int scanned3PlusPieces;

	Piece pp;
	Offset nn, n2;
	int savePiecesScanned;

++piecesSearchedFor;
savePiecesScanned = piecesScanned;
	/* see if we already know what piece it is in */
	pp = ff->logPiece;
	nn = ff->loLogPiece;
	if( ff->loLogPiece <= logPos && logPos <= ff->hiLogPiece )
		/*EMPTY*/
	{
		/* now nn = first logical byte in this piece */
	} else {	/* go through the piece table */
		if( logPos < nn ) {	/* below this piece? */
			if( logPos < 0 ) {  /* error checking to be safe */
				msg("findPiece: byte number < 0", 1 );
				return pp;	/* what else? */
			}
			/* search down for the piece */
			while( logPos < nn ) {
				pp = pp->prevPiece;
++piecesScanned;
				nn -= pp->length;
			}
		} else {	/* must be at or above this piece */
			if( logPos >= ff->fileSize ) {
				/* return the last piece */
				while( pp->nextPiece != NULL ) {
					nn += pp->length;
++piecesScanned;
					pp = pp->nextPiece;
				}
			} else {
				/* search up for the piece */
				while( 1 ) {
					n2 = nn + pp->length;
					if( logPos < n2 )
						break;
					if( pp->nextPiece == NULL )
						break;
					nn = n2;
++piecesScanned;
					pp = pp->nextPiece;
				}
			}
		}
	}
	*beginLogPos = nn;
switch( piecesScanned - savePiecesScanned ) {
case 0: ++scanned0pieces; break;
case 1: ++scanned1pieces; break;
case 2: ++scanned2pieces; break;
default: ++scanned3PlusPieces; break;
}
	return pp;
}
