/* $Header: /a/cvs/386BSD/ports/editor/point/goto.c,v 1.1 1994/02/15 22:12:37 jkh Exp $ */

#include "pt.h"

void
matchChar()
{
	extern struct window *selWindow;
	extern Offset selBegin, selEnd;

	int uch;
	Offset cp;
	char ch1, ch2;
	int n, increment;
	int fid;

	if( selWindow == NULL )
		return;

	fid = selWindow->fileId;

	cp = selBegin;
	uch = getFileByte( fid, cp );
	switch( uch ) {
	case '(': ch1 = '('; ch2 = ')'; increment =  1; break;
	case ')': ch1 = ')'; ch2 = '('; increment = -1; break;
	case '[': ch1 = '['; ch2 = ']'; increment =  1; break;
	case ']': ch1 = ']'; ch2 = '['; increment = -1; break;
	case '{': ch1 = '{'; ch2 = '}'; increment =  1; break;
	case '}': ch1 = '}'; ch2 = '{'; increment = -1; break;
	default:  ch1 = ' '; ch2 = ' '; increment =  1; break;
	}
	if( ch1 == ' ' ) {
		msg(
"Matching s defined for (, ), [, ], {, and } only", 0 );
		return;
	}
	n = 1;	/* n==0 ==> we found the matching character */
	while( 1 ) {
		cp += increment;
		uch = getFileByte( fid, cp );
		if( (char)uch == ch1 )
			++n;
		else if( (char)uch == ch2 )
			--n;
		if( n == 0 || uch == BLOCK_EOF )
			break;
	}
	if( n == 0 ) {
		selBegin = cp;
		selEnd = selBegin;
		/* put the selection on the third line */
		if( selBegin >= selWindow->posBotline
		 || selBegin < selWindow->posTopline ) {
			/* remember where we came from */
			selWindow->rowLastline = selWindow->numTopline;
			n = 4;
			cp = prevLine( fid, cp, &n );
			selWindow->posTopline = cp;
			/* recalculate the line number by letting */
			/* prevLine count as far back as it can */
			n = 3000000;
			cp = prevLine( fid, cp, &n );
			selWindow->numTopline = n + 1;
		}
		drawWindow(selWindow);
	} else
		msg("No matching character was found.", 0 );
}

void
doGoSel(w)
	struct window *w;
{
	extern struct window *selWindow;
	extern Offset selBegin;
	extern int linesOverFind;

	int n;
	int i;
	Offset cp, toCp;
	int fid = w->fileId;

	if( selWindow == NULL )
		return;

	/* remember where we came from */
	w->rowLastline = w->numTopline;

	/* n is the number of lines to move */
	cp = selWindow->posTopline;
	i = linesOverFind;

	/* find the number of lines in the window */
	i = selWindow->nRows;
	if( linesOverFind > i )
		/* if linesOverFind would place it outside the */
		/* window then put it in the middle of the window */
		i >>= 1;
	else
		/* otherwise put it linesOverFind lines down */
		i = linesOverFind;
	toCp = prevLine( fid, selBegin, &i );
	n = selWindow->numTopline;
	if( cp <= toCp ) {
		while( cp < toCp ) {
			i = 1;
			cp = nextLine( fid, cp, &i );
			++n;
		}
	} else {	/* cp > toCp */
		while( cp > toCp ) {
			i = 1;
			cp = prevLine( fid, cp, &i );
			--n;
		}
	}
	selWindow->posTopline = toCp;
	selWindow->numTopline = n;
	(void)indentToShowSelection(-1);
	topWindow(selWindow);
	drawWindow(selWindow);
}

void
doGoto(w, line_number_to_goto, adjustAndSelect)
	register struct window *w;
	int line_number_to_goto;
	int adjustAndSelect;
{
	extern int linesOverFind;
	extern struct window *selWindow;
	extern Offset selBegin, selEnd;

	int n;
	int fid = w->fileId;
	int lof;

	/* remember where we came from */
	w->rowLastline = w->numTopline;

	/* n is the number of lines to move */
	n = line_number_to_goto - w->numTopline;
	if( n > 0 && n < w->nRows ) {
		/* the line is already showing on the screen */
		goto SelectOnly;
	}
	if( adjustAndSelect ) {
		lof = linesOverFind;
		if( lof >= w->nRows )
			lof = w->nRows / 2;
		n -= lof;
	}
	if( n > 0 ) {
		w->posTopline = nextLine( fid, w->posTopline, &n );
		w->numTopline += n;
		w->posBotline = nextLine( fid, w->posBotline, &n );
		w->numBotline += n;
	} else {	/* n < 0 */
		n = -n;
		/* prevLine will stop at line 1 no matter how big n is */
		/* (and return the number of lines actually moved) */
		w->posTopline = prevLine( fid, w->posTopline, &n );
		w->numTopline -= n;
		w->posBotline = prevLine( fid, w->posBotline, &n );
		w->numBotline -= n;
	}
SelectOnly:
	if( adjustAndSelect ) {
		n = line_number_to_goto - w->numTopline;
		selBegin = nextLine( fid, w->posTopline, &n );
		n = 1;
		selEnd =  nextLine( fid, selBegin, &n ) - 1;
	}
	
	w->indent = 0;
	selWindow = w;
	drawWindow(w);
}
