/* $Header: /a/cvs/386BSD/ports/editor/point/search.c,v 1.1 1994/02/15 22:12:40 jkh Exp $ */

#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include "pt.h"
#include <X11/StringDefs.h>

static char searchString[STRINGSIZE];

int
searchFor( w, searchMode, str, update, lof )
	struct window *w;
	int searchMode;
	char * str;
	int update;
	int lof;
{
	extern struct window *selWindow;
	extern Offset selBegin, selEnd;
	extern int selMode;
	extern char msgBuffer[];
	extern int ignoreCase;
	extern int underlineSelection;
	extern int wrapAroundSearches;

	int n, i, linesFromTop, fileId, nLines;
	int patLength;
	int saveUnderlineSelection;
	int wrappedAround = 0;
	Offset cp, cp2, startCp, stopCp;
	char ch, *p, *limit;
	int fid = w->fileId;
	Offset newSelBegin, newSelEnd;

	fileId = w->fileId;

        if( str != NULL ) {
                /* get pointers we can change */
                p = searchString;
                limit = searchString + STRINGSIZE;
                while( 1 ) {
                        ch = *str++;
                        /* case insensitive search? */
                        if( isupper(ch) && ignoreCase )
                                ch = tolower(ch);
                        if( (*p++ = ch) == '\0' )
                                break;
                        if( p >= limit ) {
                                *--p = '\0';
                                break;
                        }
                }
        }

	/* do not allow searching for the empty string */
	if( searchString[0] == '\0' ) {
		sprintf(msgBuffer, "Search string is empty");
		goto errorExit;
	}

	/* avoid overflowing msgBuffer */
	n = strlen( searchString );
	if( n > (STRINGSIZE-64) )
		n = STRINGSIZE - 64;
	/* 64 is enough for "Searching circularly for `%s'" */
	sprintf(msgBuffer, "Searching %s for `%s'",
		((searchMode == 0) ? "forwards" : "backwards"),
		searchString);
	msg( msgBuffer, 0 );

	patLength = strlen(searchString);
	nLines = 0;
	cp = fileSize(w->fileId);
	if( searchMode == 1 ) {
		startCp = 0;
		if( w == selWindow )
			stopCp = selBegin - 1;
		else
			stopCp = cp - 1;
	} else {	/* seachMode == 0 */
		stopCp = cp;
		if( w == selWindow ) {
			startCp = selBegin + 1;
			if( startCp >= stopCp ) {	/* already done? */
				if( wrapAroundSearches )
					startCp = 0;
				else
					goto notFound;
			}
		} else
			startCp = 0;
	}
	linesFromTop = 0;
	/* adjust the line number */
	/* just put in the two while loops and forget the IF test */
	/* at most one of the WHILE loops will actually loop */
	/* adjust for backwards searching */
	cp2 = w->posTopline;
	if( searchMode == 1 ) {
		cp = stopCp + 1;
		if( cp > fileSize(w->fileId) )
			--cp;
	} else {
		cp = startCp - 1;
		if( cp < 0 )
			cp = 0;
	}
	/* normalize cp to the beginning of its line */
	i = -1;
	cp = prevLine( fid, cp, &i );
	while( cp2 < cp ) {
		++linesFromTop;
		i = 1;
		cp2 = nextLine( fid, cp2, &i );
	}
	while( cp2 > cp ) {
		--linesFromTop;
		i = 1;
		cp2 = prevLine( fid, cp2, &i );
	}

	if( searchMode == 0 ) {
		cp = searchSpans(fileId, startCp, stopCp,
			searchString, patLength, &n);
		nLines += n;
		if( wrapAroundSearches && cp == -1 ) {
			wrappedAround = 1;
			cp = searchSpans(fileId, 0L, startCp,
				searchString, patLength, &n);
			nLines = n + 1;
			linesFromTop = -(selWindow->numTopline);
		}
	} else {
		cp = searchReverseSpans(fileId, startCp, stopCp,
				searchString, patLength, &n);
		nLines -= n;
		/* a fix so that backwards search from the backwards */
		/* search command is circular if the normal search */
		/* mode is circular */
/********************* DO NOT DO THIS YET ************
 THE PROBLEM IS GETTING THE LINE NUMBERS TO WORK OUT
 FINISH IT LATER
		if( cp == -1 ) {
			wrappedAround = 1;
			cp = searchReverseSpans(fileId, stopCp,
				fileSize(w->fileId) - patLength,
				searchString, patLength, &n);
			nLines = n;
			linesFromTop = -(selWindow->numTopline);
		}
*******************************************************/
	}

	if( cp != -1 ) {
		if( selWindow != w ) {
			drawSelection( 1 );
			selWindow = w;
		}
		newSelBegin = cp;
		newSelEnd = newSelBegin + patLength - 1;
		selMode = SELCHAR;
	} else
		goto notFound;

if( !update ) {
	selBegin = newSelBegin;
	selEnd = newSelEnd;
} else {
	(void)indentToShowSelection(-1);
	saveUnderlineSelection = underlineSelection;
	underlineSelection = 0;
	if( newSelBegin >= selWindow->posBotline
				|| newSelBegin < selWindow->posTopline ) {
		/* remember where we came from */
		selWindow->rowLastline = selWindow->numTopline;
		selBegin = newSelBegin;
		selEnd = newSelEnd;
	 	n = -1;
	 	cp2 = selBegin;
		cp2 = prevLine( fid, cp2, &n);
		/* find the number of lines in the window */
		n = selWindow->nRows;
		if( lof > n )
			/* if lof would place it outside the */
			/* window then put it in the middle of the window */
			n >>= 1;
		else
			/* otherwise put it lof lines down */
			n = lof;
		selWindow->posTopline = cp2;
		selWindow->posTopline = prevLine(fid,selWindow->posTopline,&n);
		selWindow->numTopline += linesFromTop + nLines - n;
		drawWindow(selWindow);
	} else {
		int row1, row2, col1, col2;
		/* the found string is already showing in the window */
		drawSelection( 1 );
		selBegin = newSelBegin;
		selEnd = newSelEnd;
		OffsetToXY( selWindow, selBegin, &row1, &col1 );
		OffsetToXY( selWindow, selEnd, &row2, &col2 );
		drawWindowFast( selWindow, row1, row2, 0, selWindow->nCols, 0 );
	}
	underlineSelection = saveUnderlineSelection;
}

	AssertSelectionOwnership();
	/* this will erase the "Searching for..." msg in the case where */
	/* the selWindow does not extend to the bottom line */
	if( wrappedAround && update )
		msg(
"Circular search has wrapped around the beginning of the file", 0 );
	return selBegin;

notFound:
	/* avoid overflowing msgBuffer */
	n = strlen(searchString);
	if( n > (STRINGSIZE-64) )
		n = STRINGSIZE - 64;
	/* 64 is enough for "String `%s' not found." */
	ch = searchString[n];
	searchString[n] = '\0';
	sprintf(msgBuffer, "`%s' not found.", searchString);
	searchString[n] = ch;
errorExit:
	msg(msgBuffer, 0 );
	return -1;
}


int
RegexSearch( w, searchMode, str, update, lof )
	struct window *w;
	int searchMode;
	char * str;
	int update;
	int lof;
{
	extern struct window *selWindow;
	extern Offset selBegin, selEnd;
	extern int selMode;
	extern char msgBuffer[];
	extern char textBuffer[];
	extern int underlineSelection;
	extern int wrapAroundSearches;
	extern int regex_bopat[];
	extern int regex_eopat[];

	int n, i, linesFromTop, fileId, nLines;
	int saveUnderlineSelection;
	int wrappedAround = 0;
	Offset cp, cp2, startCp, stopCp;
	char ch, *p;
	int fid = w->fileId;
	Offset newSelBegin, newSelEnd;

	fileId = w->fileId;
	
	p = re_comp( str );
	if( p != NULL ) {
		sprintf(msgBuffer, "Regular expression error: %s", p);
		printf("%s\n", msgBuffer);
		goto errorExit;
	}

	/* avoid overflowing msgBuffer */
	sprintf(textBuffer, "Searching %%s for `%%%ds'", STRINGSIZE-64);
	sprintf(msgBuffer, textBuffer,
		((searchMode == 0) ? "forwards" : "backwards"), str);
	msg( msgBuffer, 0  );

	nLines = 0;

	cp = fileSize(w->fileId);
	if( searchMode == 1 ) {
		startCp = 0;
		if( w == selWindow )
			stopCp = selBegin - 1;
		else
			stopCp = cp - 1;
	} else {	/* seachMode == 0 */
		stopCp = cp - 1;
		if( w == selWindow ) {
			startCp = selBegin + 1;
			if( startCp > stopCp ) {	/* already done? */
				if( wrapAroundSearches )
					startCp = 0;
				else
					goto notFound;
			}
		} else
			startCp = 0;
	}
	linesFromTop = 0;
	/* adjust the line number */
	/* just put in the two while loops and forget the IF test */
	/* at most one of the WHILE loops will actually loop */
	/* adjust for backwards searching */
	cp2 = w->posTopline;
	if( searchMode == 1 ) {
		cp = stopCp + 1;
		if( cp > fileSize(w->fileId) )
			--cp;
	} else {
		cp = startCp - 1;
		if( cp < 0 )
			cp = 0;
	}
	/* normalize cp to the beginning of its line */
	i = -1;
	cp = prevLine( fid, cp, &i );
	while( cp2 < cp ) {
		++linesFromTop;
		i = 1;
		cp2 = nextLine( fid, cp2, &i );
	}
	while( cp2 > cp ) {
		--linesFromTop;
		i = 1;
		cp2 = prevLine( fid, cp2, &i );
	}

	if( searchMode == 0 ) {
		i = re_exec( fileId, startCp, stopCp, &n );
		nLines += n;
		if( wrapAroundSearches && i != 1 ) {
			wrappedAround = 1;
			i = re_exec( fileId, 0L, startCp-1, &n );
			nLines = n;
			linesFromTop = -(selWindow->numTopline);
		}
	} else {
		i = re_exec_reversed( fileId, startCp, stopCp, &n );
		nLines -= n;
		/* a fix so that backwards search from the backwards */
		/* search command is circular if the normal search */
		/* mode is circular */
/********************* DO NOT DO THIS YET ************
 THE PROBLEM IS GETTING THE LINE NUMBERS TO WORK OUT
 FINISH IT LATER
		if( cp == -1 ) {
			wrappedAround = 1;
			cp = searchReverseSpans(fileId, stopCp,
				fileSize(w->fileId) - patLength,
				str, patLength, &n);
			nLines = n;
			linesFromTop = -(selWindow->numTopline);
		}
*******************************************************/
	}

	if( i < 1 )
		goto notFound;
	if( selWindow != w ) {
		drawSelection( 1 );
		selWindow = w;
	}
	newSelBegin = regex_bopat[0];
	newSelEnd = regex_eopat[0] - 1;
	selMode = SELCHAR;

if( !update ) {
	selBegin = newSelBegin;
	selEnd = newSelEnd;
} else {
	(void)indentToShowSelection(-1);
	saveUnderlineSelection = underlineSelection;
	underlineSelection = 0;
	if( newSelBegin >= selWindow->posBotline
				|| newSelBegin < selWindow->posTopline ) {
		/* remember where we came from */
		selWindow->rowLastline = selWindow->numTopline;
		selBegin = newSelBegin;
		selEnd = newSelEnd;
	 	n = -1;
	 	cp2 = selBegin;
		cp2 = prevLine( fid, cp2, &n);
		/* find the number of lines in the window */
		n = selWindow->nRows;
		if( lof > n )
			/* if lof would place it outside the */
			/* window then put it in the middle of the window */
			n >>= 1;
		else
			/* otherwise put it lof lines down */
			n = lof;
		selWindow->posTopline = cp2;
		selWindow->posTopline = prevLine(fid,selWindow->posTopline,&n);
		selWindow->numTopline += linesFromTop + nLines - n;
		drawWindow(selWindow);
	} else {
		int row1, row2, col1, col2;
		/* the found string is already showing in the window */
		drawSelection( 1 );
		selBegin = newSelBegin;
		selEnd = newSelEnd;
		OffsetToXY( selWindow, selBegin, &row1, &col1 );
		OffsetToXY( selWindow, selEnd, &row2, &col2 );
		drawWindowFast( selWindow, row1, row2, 0, selWindow->nCols, 0 );
	}
	underlineSelection = saveUnderlineSelection;
}

	AssertSelectionOwnership();
	/* this will erase the "Searching for..." msg in the case where */
	/* the selWindow does not extend to the bottom line */
	if( wrappedAround && update )
		msg(
"Circular search has wrapped around the beginning of the file", 0 );
	return selBegin;

notFound:
	/* avoid overflowing msgBuffer */
	n = strlen(str);
	if( n > (STRINGSIZE-64) )
		n = STRINGSIZE - 64;
	/* 64 is enough for "String `%s' not found." */
	ch = str[n];
	str[n] = '\0';
	sprintf(msgBuffer, "`%s' not found.", str);
	str[n] = ch;
errorExit:
	msg(msgBuffer, 0 );
	return -1;
}


#ifdef OLDOLD
int
OldRegexSearch( w, searchMode, str, update, lof )
	struct window *w;
	int searchMode;
	char * str;
	int update;
	int lof;
{
	extern struct window *selWindow;
	extern Offset selBegin, selEnd;
	extern int selMode;
	extern char msgBuffer[];
	extern int ignoreCase;
	extern int debug;
	extern int underlineSelection;
	extern int wrapAroundSearches;
	extern int regex_bopat[];
	extern int regex_eopat[];

	int n, i, linesFromTop, fileId, fid2, nLines;
	int patLength;
	int saveUnderlineSelection;
	int ret, answer;
	int wrappedAround = 0;
	Offset cp, cp2, startCp, stopCp;
	char ch, *p, *limit, *from, *to;
	int fid = w->fileId;
	Offset newSelBegin, newSelEnd;

	fileId = w->fileId;

	p = re_comp( str );
	if( p != NULL ) {
		sprintf(msgBuffer, "RE ERROR: %s", p);
		printf("%s\n", msgBuffer);
		goto errorExit;
	}

	/* avoid overflowing msgBuffer */
	n = strlen( str );
	if( n > (STRINGSIZE-64) )
		n = STRINGSIZE - 64;
	/* 64 is enough for "Searching circularly for `%s'" */
	sprintf(msgBuffer, "Searching %s for `%s'",
		((searchMode == 0) ? "forwards" : "backwards"),
		str);
	msg( msgBuffer, 0  );

/******** handle backwards, wrap around, etc. ********/

	cp = fileSize(w->fileId);
	stopCp = cp;
	if( w == selWindow ) {
		startCp = selBegin + 1;
		if( startCp >= stopCp ) {	/* already done? */
			if( wrapAroundSearches )
				startCp = 0;
			else
				goto notFound;
		}
	} else
		startCp = 0;

	n = re_exec( fileId, startCp, stopCp, &i);

	if( n < 1 )
		goto notFound;

	if( selWindow != w ) {
		drawSelection( 1 );
		selWindow = w;
	}
	newSelBegin = regex_bopat[0];
	newSelEnd = regex_eopat[0] - 1;
	selMode = SELCHAR;

if( !update ) {
	selBegin = newSelBegin;
	selEnd = newSelEnd;
} else {
	(void)indentToShowSelection(-1);
	saveUnderlineSelection = underlineSelection;
	underlineSelection = 0;
	if( newSelBegin >= selWindow->posBotline
				|| newSelBegin < selWindow->posTopline ) {
		/* remember where we came from */
		selWindow->rowLastline = selWindow->numTopline;
		selBegin = newSelBegin;
		selEnd = newSelEnd;
	 	n = -1;
	 	cp2 = selBegin;
		cp2 = prevLine( fid, cp2, &n);
		/* find the number of lines in the window */
		n = selWindow->nRows;
		if( lof > n )
			/* if lof would place it outside the */
			/* window then put it in the middle of the window */
			n >>= 1;
		else
			/* otherwise put it lof lines down */
			n = lof;
		selWindow->posTopline = cp2;
		selWindow->posTopline = prevLine(fid,selWindow->posTopline,&n);
		selWindow->numTopline += linesFromTop + nLines - n;
		drawWindow(selWindow);
	} else {
		int row1, row2, col1, col2;
		/* the found string is already showing in the window */
		drawSelection( 1 );
		selBegin = newSelBegin;
		selEnd = newSelEnd;
		OffsetToXY( selWindow, selBegin, &row1, &col1 );
		OffsetToXY( selWindow, selEnd, &row2, &col2 );
		drawWindowFast( selWindow, row1, row2, 0, selWindow->nCols, 0 );
	}
	underlineSelection = saveUnderlineSelection;
}

	AssertSelectionOwnership();
	/* this will erase the "Searching for..." msg in the case where */
	/* the selWindow does not extend to the bottom line */
	if( wrappedAround && update )
		msg(
"Circular search has wrapped around the beginning of the file", 0 );
	return selBegin;

notFound:
	/* avoid overflowing msgBuffer */
	n = strlen(str);
	if( n > (STRINGSIZE-64) )
		n = STRINGSIZE - 64;
	/* 64 is enough for "String `%s' not found." */
	ch = str[n];
	str[n] = '\0';
	sprintf(msgBuffer, "`%s' not found.", str);
	str[n] = ch;
errorExit:
	msg(msgBuffer, 0 );
	return -1;
}
#endif

