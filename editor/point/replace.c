/* $Header: /a/cvs/386BSD/ports/editor/point/replace.c,v 1.1 1994/02/15 22:12:39 jkh Exp $ */

#include <ctype.h>
#include <stdio.h>
#include "pt.h"

void
replaceText( w, fromString, toString, inSelection )
	struct window *w;
	char * fromString;
	char * toString;
	int inSelection;
{
	extern struct window *selWindow;
	extern Offset selBegin, selEnd;
	extern int selMode;
	extern char msgBuffer[];
	extern Offset addPosition;
	extern int addHandle;
	extern struct openFile *files;

	struct window *saveSelWindow;
	Offset saveSelBegin, saveSelEnd, repEnd;
	int n, fileId, fromLength;
	int nLines, nReplaces;
	int diffLengths;
	Offset toLength, fSize;
	Offset cp;
	Piece tempPP, newPP;
	struct changeItem *thisChange;
	struct openFile *ff;

	fileId = w->fileId;
	ff = &files[fileId];
	fSize = fileSize(fileId);
	nReplaces = 0;
	
	if( selWindow == NULL )
		return;
	
	/* check if this is a readOnly file */
	if( ff->flags & READ_ONLY ) {
		sprintf(msgBuffer, "File %s is read only", ff->origName);
		msg(msgBuffer, 1 );
		return;
	}

	/* Create a piece for the replacement string and record it in */
	/* the piece buffer.  This makes it easy to insert the replace */
	/* string by copying this piece */
	toLength = strlen(toString);
	tempPP = getFreePiece();

	tempPP->file = addHandle;
	tempPP->position = addPosition;
	for(n = 0; n < (int)toLength; n++ )
		writeChar(toString[n], addPosition++);
	tempPP->length = toLength;

	if( inSelection )
		repEnd = selEnd;
	else
		repEnd = fileSize(w->fileId);

	if( w == selWindow )
		cp = selBegin + 1;
	else
		cp = 0;
	
	/* How will the replace change character counts? */
	/* we need to adjust repEnd after each replace */
	fromLength = strlen(fromString);
	diffLengths = strlen(toString) - fromLength;

	/* set things up so the line counts will be right */
	if( (char)getFileByte(fileId, cp) == '\n' )
		nLines = 1;
	else
		nLines = 0;

	/* save the location of the current selection */
	saveSelWindow = selWindow;
	saveSelBegin = selBegin;
	saveSelEnd = selEnd;

/* while loop to repeat the replace */
while( 1 ) {

	sprintf(msgBuffer, "Replace is %d%% completed",
		(int)( (100*cp) / (fSize + nReplaces*(toLength-fromLength)) )
		);
	msg(msgBuffer, 0);

	/* find the string */
	cp = searchSpans(fileId, cp, repEnd, fromString, fromLength, &n);
	nLines += n;
	if( cp == -1 )
		break;
	if( selWindow != w ) {
		drawSelection( 1 );
		selWindow = w;
	}
	selBegin = cp;
	selEnd = selBegin + fromLength - 1;
	selMode = SELCHAR;

	/* remember where we came from */
	selWindow->rowLastline = selWindow->numTopline;
	
	(void)deleteChars(selWindow->fileId, NOUPDATE, 0);
		
	/* record in the change history */
	thisChange = GetNewChange( ff );
	thisChange->type = CINSERT;
	thisChange->position = selBegin;
	thisChange->length = toLength;
	thisChange->flags = 0;
	thisChange->lineNumber = selWindow->numTopline;
	thisChange->fileId = selWindow->fileId;
	thisChange->w = selWindow;
	newPP = getFreePiece();
	newPP->file = addHandle;
	newPP->position = tempPP->position;
	newPP->length = toLength;
	thisChange->firstPiece = newPP;
	RecordChange( ff, thisChange );
	
	copyPieces( tempPP, selWindow, selBegin, toLength, 0, 0 );
	cp = selBegin;
	/* as we change the length of the text in the file with */
	/* a replacement, we have to adjust repEnd so that we  */
	/* will not quit early */
	repEnd += diffLengths;

	/* we want the final */
	/* selection to be the same as the original */
	/* selection even though we are replaceing */
	/* inside it.  This makes the adjustment */
	saveSelEnd += diffLengths;

	++nReplaces;
}

	/* free the temp piece */
	freePieces(tempPP);

	/* restore the previous selection */
	selWindow = saveSelWindow;
	selBegin = saveSelBegin;
	selEnd = saveSelEnd;
	drawWindow(selWindow);

	sprintf(msgBuffer, "Made %d replacement%s", nReplaces,
		nReplaces==1?"":"s");
	msg(msgBuffer, 0);
}
