/* $Header: /a/cvs/386BSD/ports/editor/point/undoredo.c,v 1.1 1994/02/15 22:12:41 jkh Exp $ */

#include <string.h>
#include <stdio.h>
#include "pt.h"

#define CHANGES_TO_SHOW		30

static struct openFile * mostRecentChange = NULL;

void
initChanges()
{
	extern struct changeItem scrapBuffer;
	extern int addHandle;

	scrapBuffer.firstPiece = getFreePiece();
	scrapBuffer.firstPiece->file = addHandle;
	scrapBuffer.firstPiece->position = 0L;
	scrapBuffer.firstPiece->length = 0L;
}

struct changeItem *
GetCurrentChange( ff )
	struct openFile * ff;
{
	return ff->cmdHistory;
}

static void
SetCurrentChange( ff, new_change )
	struct openFile * ff;
	struct changeItem * new_change;
{
	ff->cmdHistory = new_change;
}

/*ARGSUSED*/
struct changeItem *
GetNewChange( ff )
	struct openFile * ff;
{
	struct changeItem * new_change;

	new_change = (struct changeItem *)
			PtMalloc( sizeof(struct changeItem), "change item" );
	return new_change;
}

void
RecordChange( ff, new_change )
	struct openFile * ff;
	struct changeItem * new_change;
{
	struct changeItem * last_change;
	struct changeItem * next_change;
	struct changeItem * change_to_free;
	
	/* link it into the chain */
	last_change = GetCurrentChange( ff );
	next_change = last_change->next;

	/* free all undone edits */
	while( next_change != NULL ) {
		change_to_free = next_change;
		next_change = next_change->next;
		PtFree( (char *)change_to_free );
	}

	new_change->next = NULL;
	new_change->prev = last_change;
	last_change->next = new_change;
	SetCurrentChange( ff, new_change );
	mostRecentChange = ff;
}

static char *
GetUndoText( thisChange )
	struct changeItem * thisChange;
{
	extern char textBuffer[];
	extern struct openFile *files;
	extern int maxFiles;

	struct openFile *ff;
	int len = thisChange->length;
	Offset limit, logByte;
	int i, ch;

	/* cannot do it if the file is closed */
	if( thisChange->flags & FILE_WAS_CLOSED ) {
		return "File is closed";
	}
	/* simulate a file this was deleted from */
	ff = &files[maxFiles];
	ff->hiLogBuffer = -1;
	ff->loLogBuffer = -1;
	ff->origHandle = thisChange->fileId;
	ff->fileSize = len;
	ff->logPiece = thisChange->firstPiece;
	ff->loLogPiece = 0;
	ff->hiLogPiece = thisChange->firstPiece->length - 1;
	i = 0;
	logByte = 0;
	if( (limit = 15) > len )
		limit = len;
	while( i < limit ) {
		ch = getFileByte(maxFiles, logByte++);
		/* temporay fix because of Tcl parsing of {...} */
		if( ch == '{' )
			ch = '[';
		else if( ch == '}' )
			ch = ']';
		textBuffer[i++] = ch;
	}
	if( len > 33 ) {
		textBuffer[i++] = '.';
		textBuffer[i++] = '.';
		textBuffer[i++] = '.';
		logByte = len - 15;
	}
	while( 1 ) {
		ch = getFileByte(maxFiles, logByte++);
		if( ch == BLOCK_EOF )
			break;
		/* temporay fix because of Tcl parsing of {...} */
		if( ch == '{' )
			ch = '[';
		else if( ch == '}' )
			ch = ']';
		textBuffer[i++] = ch;
	}
	textBuffer[i] = '\0';
	return textBuffer;
}

static char *
flag_msg( flags )
	int flags;
{
	static char buffer[10];

	if( flags & FILE_WAS_CLOSED )
		return "closed ";
	if( flags & CHANGE_WAS_UNDONE )
		strcpy( buffer, "no     " );
	else
		strcpy( buffer, "yes    " );
	if( flags & BLOCK_UNDO_BEGIN )
		buffer[5] = '>';
	else if( flags & BLOCK_UNDO_END )
		buffer[5] = '<';
	return buffer;
}

void
UpdateUndoList( ff )
	struct openFile * ff;
{
	extern char msgBuffer[];
	extern char textBuffer[];
	extern struct openFile *files;

	int i;
	char *s, *str;
	struct changeItem *thisChange;
	struct changeItem *prevChange;
        char buffer[MSGBUFFERSIZE];

	/* empty the current list */
	(void)ExecTclCommand( "catch {.ub.list.list delete 0 end}", NULL );

	/* fill undo box */
	sprintf( msgBuffer,
	    "catch {.ub.list.list insert end {%-7s%-8s%-16s%5s%7s%6s %-20s}}",
	    "Done", "Type", "File", "Line", "Pos", "Len", "Chars" );
	(void)ExecTclCommand( msgBuffer, NULL );
	/* get the current change */
	thisChange = GetCurrentChange( ff );
	/* show up to 10 already undone changes */
	for( i = 0; i < 10; ++i ) {
		if( thisChange->next == NULL )
			break;
		thisChange = thisChange->next;
	}
	i = 0;
	while( thisChange != NULL && i < CHANGES_TO_SHOW ) {
		s = textBuffer;
		prevChange = thisChange->prev;
		switch( thisChange->type ) {
		case CNULL:	/* skip null changes */
			sprintf( s, "%s", "*** End of list ***" );
			break;
		case CINSERT:
                        strcpy(buffer, GetUndoText(thisChange) );
			sprintf( s, "%7s%-8s%-16s%5d%7d%6d \"%s\"",
				flag_msg(thisChange->flags),
				"Insert",
				&((files[thisChange->fileId].origName)
					[thisChange->w->nameOffset]),
				thisChange->lineNumber, thisChange->position,
				thisChange->length,
				buffer );
			break;
		case CDELETE:
			if( prevChange->type == CMOVE )
				str = "MvFrom";
			else
				str = "Delete";
                        strcpy(buffer, GetUndoText(thisChange) );
			sprintf( s, "%7s%-8s%-16s%5d%7d%6d \"%s\"",
				flag_msg(thisChange->flags),
				str,
				&((files[thisChange->fileId].origName)
					[thisChange->w->nameOffset]),
				thisChange->lineNumber, thisChange->position,
				thisChange->length,
				buffer );
			break;
		case CMOVE:
			/* not used -- a combination of CCOPY and CDELETE */
                        strcpy(buffer, GetUndoText(thisChange) );
			sprintf( s, "%7s%-8s%-16s%5d%7d%6d \"%s\"",
				flag_msg(thisChange->flags),
				"MoveTo",
				&((files[thisChange->fileId].origName)
					[thisChange->w->nameOffset]),
				thisChange->lineNumber, thisChange->position,
				thisChange->length,
				buffer );
			break;
		case CCOPY:
                        strcpy(buffer, GetUndoText(thisChange) );
			sprintf( s, "%7s%-8s%-16s%5d%7d%6d \"%s\"",
				flag_msg(thisChange->flags),
				"Copy",
				&((files[thisChange->fileId].origName)
					[thisChange->w->nameOffset]),
				thisChange->lineNumber, thisChange->position,
				thisChange->length,
				buffer );
			break;
		case CREPLACE:
			/* not used -- a combination of CDELETE and CINSERT */
                        strcpy(buffer, GetUndoText(thisChange) );
			sprintf( s, "%7s%-8s%-16s%5d%7d%6d \"%s\"",
				flag_msg(thisChange->flags),
				"Replace",
				&((files[thisChange->fileId].origName)
					[thisChange->w->nameOffset]),
				thisChange->lineNumber, thisChange->position,
				thisChange->length,
				buffer );
			break;
		case CMOTION:
			sprintf( s, "%7s%-8s%-16s%5d%7d%6d",
				flag_msg(thisChange->flags),
				"Motion",
				&((files[thisChange->fileId].origName)
					[thisChange->w->nameOffset]),
				thisChange->lineNumber, thisChange->position,
				thisChange->length );
			break;
		}
		sprintf(msgBuffer, "catch {.ub.list.list insert end {%s}}",s);
		(void)ExecTclCommand( msgBuffer, NULL );
		++i;
		thisChange = prevChange;
	}
}

void
ShowUndos( ff )
	struct openFile * ff;
{
	(void)ExecTclCommand( "MakeUndoBox", NULL );

	UpdateUndoList( ff );
}

void
again( ff, mostRecent )
	struct openFile * ff;
	int mostRecent;
{
	extern char msgBuffer[];
	extern struct window *selWindow;
	extern Offset selBegin;
	extern struct openFile *files;

	struct changeItem *newChange;
	struct changeItem *thisChange, *prevChange;
	int type;

	if( selWindow == NULL )
		return;

	/* check if this is a readOnly file */
	if( files[selWindow->fileId].flags & READ_ONLY ) {
		sprintf(msgBuffer, "File %s is read only",
			files[selWindow->fileId].origName);
		msg(msgBuffer, 1 );
		return;
	}

	/* most recent in this file or over all files? */
	if( mostRecent && mostRecentChange != NULL )
		ff = mostRecentChange;

	thisChange = GetCurrentChange( ff );
	if( thisChange == NULL || thisChange->type == CNULL ) {
noChanges:
		msg("No previous change to repeat", 0 );
		return;
	}

	/* find the change to repeat (not a delete ) */
	while( 1 ) {
		if( thisChange == NULL )
			goto noChanges;
		if( (thisChange->type)!=CDELETE && (thisChange->type)!=CNULL )
			break;
		thisChange = thisChange->prev;
	}

	switch( thisChange->type ) {

	case CINSERT:
		type = CINSERT;
		goto doCopy;

	case CCOPY:
	case CMOVE:
		type = CCOPY;
	doCopy:
		/* see if the previous change was a delete */
		prevChange = thisChange->prev;
		if( thisChange->position == prevChange->position 
		 && prevChange->type == CDELETE )
			/* the delete must go into the history first */
			(void)deleteChars(selWindow->fileId, NOUPDATE, 0);
		/* record the change before copyPieces changes things */
		newChange = GetNewChange( ff );
		newChange->type = type;
		newChange->position = selBegin;
		newChange->length = thisChange->length;
		newChange->lineNumber = selWindow->numTopline;
		newChange->w = thisChange->w;
		newChange->flags = 0;
		newChange->fileId = selWindow->fileId;
		newChange->firstPiece = dupPieces(thisChange->firstPiece);
		copyPieces( thisChange->firstPiece, selWindow, selBegin,
			thisChange->length, 1, 0 );
		RecordChange( ff, newChange );
		break;
	
	case CDELETE:
		(void)deleteChars(selWindow->fileId, UPDATEWINDOWS, 0);
		break;
	}
}


/* make sure the change is visible */
static void
showChange()
{
	extern Offset selBegin, selEnd;
	extern struct window *selWindow;
	extern struct window *windowList;
	
	/* if the selection window is not on top */
	/* or if the selection is not in the window */
	/* then move the window to show the selection */
	if( windowList != selWindow  || selBegin < selWindow->posTopline
					|| selEnd > selWindow->posBotline )
	 	doGoSel(selWindow);
}


void
redo( ff, count )
	struct openFile * ff;
	int count;
{
	extern struct window *selWindow;
	extern Offset selBegin, selEnd;
	extern struct window *windowList;

	struct changeItem *prevChange;
	struct changeItem *thisChange, *nextChange;
	struct window *w1;
	int blockRedo = 0;
	
	if( selWindow == NULL )
		return;

while( count > 0 || blockRedo ) {
	prevChange = GetCurrentChange( ff );
	thisChange = prevChange->next;
	if( thisChange == NULL ) {
		msg("No undone change to redo", 0 );
		return;
	}

	if( (prevChange->flags & BLOCK_UNDO_BEGIN)
	 && (prevChange->type != CNULL) ) {
		blockRedo = 1;
	} else if( (prevChange->flags & BLOCK_UNDO_END) && blockRedo ) {
		blockRedo = 0;
		if( --count <= 0 )
			break;
	} else
		--count;

	/* move up the pointer to the last done change */
	SetCurrentChange( ff, thisChange );

	thisChange->flags &= ~CHANGE_WAS_UNDONE;
	nextChange = thisChange->next;

	/* find a window displaying the file the change was made in */
	if( thisChange->fileId != selWindow->fileId ) {
		w1 = windowList;
		while( w1 != NULL && w1->fileId != thisChange->fileId )
			w1 = w1->nextWindow;
		if( w1 == NULL ) {
			msg("Cannot redo. No windows have that file open.", 1 );
			return;
		} else
			selWindow = w1;
	}

	/* erase the selection because the redo might not do it */
	drawSelection( 1 );

	switch( thisChange->type ) {

	case CMOTION:
		doGoto( (struct window *)(thisChange->firstPiece),
				thisChange->length, 0 );
		break;

	case CCOPY:
	case CMOVE:
		showChange();
		copyPieces( thisChange->firstPiece, selWindow,
			thisChange->position, thisChange->length, 1, 0 );
		/* if this is not a move, stop, we are done */
		if( thisChange->type != CMOVE )
			break;
		/* if it is a move then the delete follows */
		/* else finish redoing the move by dropping through */
		/* to the CDELETE case to delete the MOVEd text */
		nextChange->flags &= ~CHANGE_WAS_UNDONE;
		thisChange = nextChange;
		SetCurrentChange( ff, thisChange );

	case CDELETE:
		selBegin = thisChange->position;
		selEnd = selBegin + thisChange->length - 1;
		selWindow = thisChange->w;
		showChange();
		(void)deleteChars( thisChange->fileId, UPDATEWINDOWS, 2 );
		/* if the next change is an insert at the same position */
		/* then redo it also */
		if( nextChange == NULL || nextChange->type != CINSERT
		 || thisChange->position != nextChange->position )
			break;
		/* else drop through to redo the insert also */
		thisChange = nextChange;
		SetCurrentChange( ff, thisChange );
		thisChange->flags &= ~CHANGE_WAS_UNDONE;

	case CINSERT:
		copyPieces( thisChange->firstPiece, thisChange->w,
				thisChange->position, thisChange->length,
				1, 0 );
		break;
	}
}

}

void
undo( ff, count )
	struct openFile * ff;
	int count;
{
	extern struct window *selWindow;
	extern Offset selBegin, selEnd;
	extern struct window *windowList;

	DoUpdate delAlso;
	struct changeItem *thisChange, *prevChange;
	struct window *w1;
	int blockUndo = 0;
	
	if( selWindow == NULL )
		return;

while( count > 0 || blockUndo ) {
	thisChange = GetCurrentChange( ff );
	if( thisChange == NULL || thisChange->type == CNULL ) {
noChanges:
		msg("No previous change to undo", 0 );
		return;
	}

	/* find the change to undo */
	while( 1 ) {
		if( thisChange == NULL )
			goto noChanges;
		if( (thisChange->type) != CNULL
		 && ( (thisChange->flags)
		 		& (CHANGE_WAS_UNDONE|FILE_WAS_CLOSED) ) == 0 )
			break;
		thisChange = thisChange->prev;
	}

	/* Note that begin and end seem to be reversed here because */
	/* the begin comes with the first edit which is earliter than */
	/* the last edit.  Since we are going backwards in time here */
	/* we encounter the last edit (the BLOCK_UNDO_END) first and */
	/* want to undo all the way to the first edit in the block */
	/* (the BLOCK_UNDO_BEGIN) */
	if( thisChange->flags & BLOCK_UNDO_END ) {
		blockUndo = 1;
	} else if( (thisChange->flags & BLOCK_UNDO_BEGIN) && blockUndo ) {
		blockUndo = 0;
		if( --count <= 0 )
			break;
	} else
		--count;

	thisChange->flags |= CHANGE_WAS_UNDONE;
	prevChange = thisChange->prev;

	/* back up the pointer to the last done change */
	SetCurrentChange( ff, prevChange );

	/* find a window displaying the file the change was made in */
	if( thisChange->fileId != selWindow->fileId ) {
		w1 = windowList;
		while( w1 != NULL && w1->fileId != thisChange->fileId )
			w1 = w1->nextWindow;
		if( w1 == NULL ) {
			msg("Cannot undo. No windows have that file open.",1);
			return;
		} else
			selWindow = w1;
	}
	
	/* erase the selection because the undo might not do it */
	drawSelection( 1 );
	
	switch( thisChange->type ) {

	case CMOTION:
		doGoto( (struct window *)(thisChange->firstPiece),
				thisChange->lineNumber, 0 );
		break;

	case CDELETE:
		showChange();
		copyPieces( thisChange->firstPiece, selWindow,
			thisChange->position, thisChange->length, 1, 0 );
		/* see if this is really the DELETE of a CMOVE */
		if( prevChange->type != CMOVE )
			break;
		/* else finish undoing the move by dropping through */
		/* to the CCOPY case to delete the MOVEd text */
		prevChange->flags |= CHANGE_WAS_UNDONE;
		thisChange = prevChange;
		SetCurrentChange( ff, thisChange->prev );

	case CCOPY:
		selBegin = thisChange->position;
		selEnd = selBegin + thisChange->length - 1;
		showChange();
		(void)deleteChars( thisChange->fileId, UPDATEWINDOWS, 2 );
		break;

	case CINSERT:
		/* delete the characters inserted */
		selBegin = thisChange->position;
		selEnd = selBegin + thisChange->length - 1;
		/* test this first so we can avoid updating the */
		/* screen twice once for the delete and again for */
		/* the copy to follow */
		if( thisChange->position == prevChange->position 
		 && prevChange->type == CDELETE )
			delAlso = NOUPDATE;
		else
			delAlso = UPDATEWINDOWS;
		showChange();
		(void)deleteChars( thisChange->fileId, delAlso, 2 );
		/* see if there is a previous, related delete to undo */
		if( delAlso == NOUPDATE ) {
			prevChange->flags |= CHANGE_WAS_UNDONE;
			SetCurrentChange( ff, prevChange->prev );
			copyPieces( prevChange->firstPiece, selWindow,
				selBegin, prevChange->length, 1, 0 );
		}
		break;
	}
}
}


