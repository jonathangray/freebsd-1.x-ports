/* $Header: /a/cvs/386BSD/ports/editor/point/tags.c,v 1.1 1994/02/15 22:12:41 jkh Exp $ */

#include <stdio.h>
#include "pt.h"

void
findCTag( ctag )
	char *ctag;
{
	extern struct window *selWindow;
	extern Offset selBegin, selEnd;
	extern int selMode;
	extern int ignoreCase;
	extern int linesOverFind;
	extern char msgBuffer[];
	extern struct openFile *files;
	extern char * textGeometry;

	int tags_file;
	char *p, *p_limit, *file_name, *search_string;
	int ctag_len, f_len, line_number;
	int n, len, linesPassed;
	Offset cp;
	char ch;
	int saveIgnoreCase;
	struct window *w;

	ctag_len = strlen( ctag );

	/* open the tags file */
	tags_file = getFileId( "tags" );
	if( tags_file < 0 ) {
		msg("Could not open 'tags' file", 1 );
		return;
	}

	/* search for the selection in the tags file */
	f_len = fileSize( tags_file );
	line_number = 0;
	saveIgnoreCase = ignoreCase;
	ignoreCase = 0;
	cp = -1;
	while( 1 ) {
		cp = searchSpans( tags_file, cp+1, f_len, ctag, ctag_len,
							&line_number);
		if( cp < 0 ) {
			sprintf( msgBuffer,"%s not found in the tags file",
									ctag);
			msg( msgBuffer, 1 );
			goto CleanUp2;
		}
		/* make sure it is at the beginning of a line */
		if( cp == 0 )
			break;
		ch = getFileByte( tags_file, cp-1 );
		if( ch == '\n' )
			break;
	}

	/* extract the file name and the search string for the tag */
	file_name = (char *)PtMalloc( MSGBUFFERSIZE, "file name" );
	search_string = (char *)PtMalloc( MSGBUFFERSIZE, "search string" );
	/* scan to the first tab */
	while( 1 ) {
		ch = getFileByte( tags_file, cp++ );
		if( (char)ch == '\t' )
			break;
	}
	/* copy the file name */
	p = file_name;
	p_limit = p + MSGBUFFERSIZE;
	while( p < p_limit ) {
		ch = (char)getFileByte( tags_file, cp++ );
		if( ch == '\t' )
			break;
		*p++ = ch;
	}
	*p = '\0';	/* terminate the string */
	/* copy the search string */
	cp += 2;	/* skip the '/^' */
	p = search_string;
	p_limit = p + MSGBUFFERSIZE;
	while( p < p_limit ) {
		ch = (char)getFileByte( tags_file, cp++ );
		if( ch == '\n' )
			break;
		*p++ = ch;
	}
	p -= 2;		/* skip the '$/' */
	*p = '\0';	/* terminate the string */

	/* see if the file is already in a window */
	p = makeFullPathname( file_name );
	w = findFilenameWindow( p );
	if( w != NULL )
		topWindow( w );
	else
		w = createWindow( NULL, file_name, textGeometry );

	/* make the string we found the selection */
	/* first erase the old selection */
	if( selWindow != w ) {
		drawSelection( 1 );
		selWindow = w;
	}
	/* then find the search string in the file */
	len = strlen( search_string );
	cp = searchSpans( w->fileId, 0, fileSize(w->fileId),
		search_string, len, &linesPassed);
	if( cp == -1 ) {
		printf("listCB: ERROR: ctag %s not found in file %s\n",
			search_string, files[w->fileId].origName);
		goto CleanUp3;
	}

	/* make it the selection */
	selBegin = cp;
	selEnd = cp + len - 1;
	selMode = SELCHAR;
	AssertSelectionOwnership();

	/* jump the window to show the string we found */
	/* find the number of lines in the window */
	n = selWindow->nRows;
	if( linesOverFind > n )
		/* if linesOverFind would place it outside the */
		/* window then put it in the middle of the window */
		n >>= 1;
	else
		/* otherwise put it linesOverFind lines down */
		n = linesOverFind;
	linesPassed -= n;
	if( linesPassed < 0 )
		linesPassed = 0;
	doGoto( w, linesPassed, 0 );

	/* close the tags file and free the strings */
CleanUp3:
	PtFree( file_name );
	PtFree( search_string );
CleanUp2:
	ignoreCase = saveIgnoreCase;
	closeFile( tags_file, 2 );
}
