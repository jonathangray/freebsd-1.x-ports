/* $Header: /a/cvs/386BSD/ports/editor/point/display.c,v 1.1 1994/02/15 22:12:37 jkh Exp $ */
#include <math.h>
#include <string.h>
#include <stdio.h>
#include "pt.h"
#include <X11/StringDefs.h>

static int maxLineLength = 1;

/* character table for fillLine (in windtext.c) */
char charTable[257];

/* screen line buffer */
char screenLine[MAXCOLS];

void
drawWindowFast(w, firstRow, lastRow, firstCol, lastCol, doBanner )
	struct window *w;
	int firstRow, lastRow, firstCol, lastCol;
	int doBanner;
{
        fillWindow(w, firstRow, lastRow, firstCol, lastCol );
        if( doBanner )
		banner( w, 1 );
}

void
drawWindow(w)
	register struct window *w;
{
	if( w == NULL )
		return;
	fillWindow(w, 0, w->nRows-1, 0, w->nCols-1);
	banner( w, 1 );
}

static char *
FormatWindowTitle( format, w, ff )
	char * format;
        struct window *w;
        struct openFile *ff;
{
	extern char msgBuffer[];
	extern char textBuffer[];
	extern int pathNames;
	extern int overType;
	extern Offset selBegin;
	extern struct window *selWindow;
	extern struct window * activeWindow;

	char * to = msgBuffer;
	char * copy_in;
	char ch;
	
	if( format == NULL )
		return "";

	while( (ch = *format++) != '\0' ) {
		if( ch != '%' ) {
			*to++ = ch;
			continue;
		}
		/* read a '%' */
		switch( ch = *format++ ) {
		default:	/* anything, including a literal '%' */
			textBuffer[0] = ch;
			textBuffer[1] = '\0';
			copy_in = textBuffer;
			break;
		case 'a':	/* active window */ {
			/* get the active window indicator string */
			char * to = textBuffer;
			char delimiter = *format++;
			while( 1 ) {
				ch = *format++;
				if( ch == delimiter )
					break;
				/* allow for missing delimiters */
				if( ch == '\0' ) {
					--format;
					msg(
"Missing active window flag in title format", 0 );
					break;
				}
				*to++ = ch;
			}
			*to = '\0';
			if( w != activeWindow )
				copy_in = "";
			else
				copy_in = textBuffer;
			break;
		}
		case 'A':	/* Start of selection (line number) */
			if( w == selWindow ) {
				int n = 999999999;
				(void)prevLine( w->fileId, selBegin, &n );
				sprintf( textBuffer, "%d", n);
				copy_in = textBuffer;
			} else
				copy_in = "0";
			break;
		case 'l':	/* beginning line number */
			sprintf( textBuffer, "%d", w->numTopline );
			copy_in = textBuffer;
			break;
		case 'c':	/* changed  flag */
			{/* get the change indicator string */
				char * to = textBuffer;
				char delimiter = *format++;
				while( 1 ) {
					ch = *format++;
					if( ch == delimiter )
						break;
					/* allow for missing delimiters */
					if( ch == '\0' ) {
						--format;
						msg(
"Missing change flag in title format", 0 );
						break;
					}
					*to++ = ch;
				}
				*to = '\0';
			}
			if( ff->flags&IS_CHANGED ) {
				copy_in = textBuffer;
			} else
				copy_in = "";
			break;
		case 'L':	/* ending line number */
			sprintf( textBuffer, "%d", w->numBotline - 1 );
			copy_in = textBuffer;
			break;
#ifdef HYPERTEXT
		case 'M':	/* block map name */
			if( w->blockMap!=NULL && hypertextOn )
				sprintf( textBuffer, "%s", w->blockMap->name );
			else
				textBuffer[0] = '\0';
			copy_in = textBuffer;
			break;
#endif
		case 'n':	/* file name */
			copy_in = &((ff->origName)[pathNames?0:w->nameOffset]);
			break;
		case 'N':	/* full path file name */
			copy_in = &((ff->origName)[0]);
			break;
		case 'o':	/* overType flag */
			{/* get the overtype indicator string */
				char * to = textBuffer;
				char delimiter = *format++;
				while( 1 ) {
					ch = *format++;
					if( ch == delimiter )
						break;
					/* allow for missing delimiters */
					if( ch == '\0' ) {
						--format;
						msg(
"Missing overtype flag in title format", 0 );
						break;
					}
					*to++ = ch;
				}
				*to = '\0';
			}
			if( overType ) {
				copy_in = textBuffer;
			} else
				copy_in = "";
			break;
		case 'p':	/* beginning position number */
			sprintf( textBuffer, "%d", w->posTopline );
			copy_in = textBuffer;
			break;
		case 'P':	/* ending position number */
			sprintf( textBuffer, "%d", w->posBotline - 1 );
			copy_in = textBuffer;
			break;
		case 'r':	/* readOnly flag */
			{/* get the readOnly indicator string */
				char * to = textBuffer;
				char delimiter = *format++;
				while( 1 ) {
					ch = *format++;
					if( ch == delimiter )
						break;
					/* allow for missing delimiters */
					if( ch == '\0' ) {
						--format;
						msg(
"Missing read only flag in title format", 0 );
						break;
					}
					*to++ = ch;
				}
				*to = '\0';
			}
			if( ff->flags&READ_ONLY ) {
				copy_in = textBuffer;
			} else
				copy_in = "";
			break;
		case 's':	/* short file name */
			copy_in = &((ff->origName)[w->nameOffset]);
			break;
		case 'S':	/* file size (in bytes) */
			sprintf( textBuffer, "%d", ff->fileSize );
			copy_in = textBuffer;
			break;
		case 'v':	/* beginning column number */
			sprintf( textBuffer, "%d", w->indent + 1 );
			copy_in = textBuffer;
			break;
		case 'V':	/* ending column number */
			sprintf( textBuffer, "%d", w->indent + w->nCols );
			copy_in = textBuffer;
			break;
		}
		
		/* now copy the string we generated into the title */
		while( *copy_in != '\0' )
			*to++ = *copy_in++;
	}
	*to = '\0';	/* terminate the string */
	
	return msgBuffer;
}

void
SetWindowNames( w )
	struct window *w;
{
	extern char textBuffer[];
	extern char * textTitleFormat;
	extern char * textIconFormat;
	extern struct openFile *files;

	struct openFile *ff;

	if( w == NULL )
		return;

	ff = &(files[w->fileId]);

	/* set the window and icon titles */
	sprintf( textBuffer, "wm title %s {%s}", w->tk_pathname,
			FormatWindowTitle( textTitleFormat, w, ff) );
	(void)ExecTclCommand( textBuffer, NULL );
	sprintf( textBuffer,"wm iconname %s {%s}", w->tk_pathname,
			FormatWindowTitle( textIconFormat, w, ff) );
	(void)ExecTclCommand( textBuffer, NULL );
}

void
banner( w, doSlider )
	struct window *w;
	int doSlider;
{
        extern struct openFile *files;
	extern int msgInTitleLine;
	extern struct openFile *files;

	long lp;
	struct openFile *ff;

	/* remember that we don't have to draw the banner again */
	msgInTitleLine = 0;

	/* guard against calls with a NULL argument */
	if( w == NULL )
		return;

	if( w->fileId == -1 )
		return;

	ff = &(files[w->fileId]);
	lp = ff->fileSize;

	SetWindowNames( w );

	if( doSlider )
		SetSlider( w, lp );
}

void
SetSlider( w, total )
	struct window *w;
	int total;
{
	extern char msgBuffer[];

	int inwindow, first, last;

	/* set vertical scroll bar */
	if( total == 0 ) /* guard again zerodivide */
		total = 1;
	first = w->posTopline;
	last = w->posBotline - 2;
	if( last < first )
		last = first;
	inwindow = last - first;
	if( inwindow < 0 )
		inwindow = 0;
	sprintf( msgBuffer, "%s.vScrollAndText.vScroll set %d %d %d %d",
		w->tk_pathname, total, inwindow, first, last );
	(void)ExecTclCommand( msgBuffer, NULL );

	/* set horizontal scroll bar */
	total = w->nCols;
	if( total < maxLineLength )
		total = maxLineLength;
	if( total <= 0 )
		total = 1;
	inwindow = w->nCols;
	first = w->indent;
	if( first >= total )
		first = total - 1;
	last = w->indent+w->nCols-1;
	if( last < first )
		last = first + 1;
	if( last >= total )
		last = total - 1;
	sprintf( msgBuffer, "%s.splitterAndHScroll.hScroll set %d %d %d %d",
		w->tk_pathname, total, inwindow, first, last );
	(void)ExecTclCommand( msgBuffer, NULL );
}

/*ARGSUSED*/
void
fillWindow(w, firstRow, lastRow, firstCol, lastCol)
	register struct window *w;
	int firstRow, lastRow, firstCol, lastCol;
{
	extern Display *MainDisplay;
	extern struct openFile *files;

	Offset cp;
	int lines_skipped, y, row, lineNumber;
	int col1, col2;
	struct fontDataStruct *font;
	int fid;

	if( w == NULL )
		return;

#ifdef HYPERTEXT
	if( w->file != NULL && !hypertextOn )
		/* a hypertext file but not in hypertext mode */
		fid = w->realFileId;
	else
#endif
		fid = w->fileId;
	
	font = &(w->font);

	maxLineLength = 1;	/* reset this count */

	/* fill the rows one at a time */

	/* initialize */
	cp = w->posTopline;
	if( files[w->fileId].screen_image != NULL )
		cp = 0;
	lineNumber = w->numTopline;
	y = w->topMargin + font->ascent;

	/* skip the rows above the ones we are redrawing */
	lines_skipped = -1;
	for( row = 0; row < firstRow; ++row ) {
		lines_skipped = 1;
		cp = nextLine( fid, cp, &lines_skipped);
		y += font->height;
		++lineNumber;
	}
	
	/* clear the area we are redrawing */
	XClearArea(MainDisplay, w->x_window_id, 0, y - font->ascent,
		Tk_Width(w->tk_text), (lastRow - row + 1)*(font->height),
		False );
		/* 0 width => clear the whole width of the window */
		/* False means: do not generate exposure events */

	/* This prevents extra eof marks from being printed. */
	/* If lines_skipped == 0 then we are past the eof mark */
	if( lines_skipped == 0 )
		return;

	/* rewrite the lines required */
	for( ; row <= lastRow; ++row ) {
		if( row == firstRow )
			col1 = firstCol;
		else
			col1 = 0;
#ifdef REMOVETHIS
/* maybe try to find a better solution later */
		if( row == lastRow )
			col2 = lastCol;
		else
#endif
			col2 = w->nCols - 1;
		cp = fillLine( w, cp, row, col1, col2, y, 1 );
		if( cp == -1 ) {
			++row;	/* at least the EOF marker printed out */
			break;
		}
		y += font->height;
		++lineNumber;
	}

	/* count down through the rows we are not drawing */
	for( ; row < w->nRows; ++row ) {
		if( cp == -1 )
			break;
		/* clear the line numbers of these lines */
		if( w->lineNumbers && w->ln_window_id != (Window)NULL ) {
			XDrawImageString( MainDisplay, w->ln_window_id,
				font->gc_normal, 0, y, "     ", 5 );
		}
		lines_skipped = 1;
		cp = nextLine( fid, cp, &lines_skipped);
		++lineNumber;
		y += font->height;
	}

	/* If we got to the end of the file we want posBotline to be */
	/* the size of the file (which will be one past the last character */
	/* in the file since we start counting at 0) */
	if( cp == -1 )
		cp = fileSize( w->fileId );
	w->posBotline = cp;
	w->numBotline = lineNumber;
}

/* I know I shouldn't use global variables for procedure parameters */
/* but these are called for every single character so I am compromising */
static struct window *w;
static char *sLine;
static GC gc, gc_normal;
static int x, y;
static int inSelection;
static int logicalCol;
static int row;
static int indent;
static Offset cp;
static struct fontDataStruct *font;

void
DrawString()
{
	extern char screenLine[];
	extern int underlineSelection;
	extern Display *MainDisplay;

	int char_len, pix_len;

	*sLine = '\0';
	char_len = sLine - screenLine;
	pix_len = char_len * font->width;
	if( char_len > 0 ) {
		if( inSelection && (underlineSelection > 0) )
			gc = gc_normal;
		XDrawImageString( MainDisplay, w->x_window_id, gc, x, y,
			screenLine, char_len );
		if( inSelection && (underlineSelection > 0) ) {
			int y2 = y + 1;
			XDrawLine(MainDisplay, w->x_window_id, gc, x, y2,
							x+pix_len-1, y2);
			if( underlineSelection == 2 )
				XDrawLine( MainDisplay, w->x_window_id, gc,
						x, y2+1, x+pix_len-1, y2+1 );
		}
		x += pix_len;
	}
	sLine = screenLine;
}

Offset
fillLine(w_in, cp_in, row_in, firstCol, lastCol, y_in, fillToEol)
	struct window *w_in;
	Offset cp_in;
	int row_in, firstCol, lastCol, y_in, fillToEol;
{
	extern struct window *selWindow;
	extern char charTable[];
	extern char screenLine[];
	extern int tabWidth;
	extern int eofChar;
	extern Display *MainDisplay;
	extern Offset selBegin, selEnd;
	extern int getSpanSize;
	extern int ptOwnsSelection;

	int uch;
	int tabStop;
	int fid;
	unsigned char *firstByte;
	unsigned char *lastByte;
	char *sLineMax;
	
	/* make these values available to DrawString and CheckForSelection */
	cp = cp_in;
	w = w_in;
	y = y_in;
	row = row_in;
	font = &(w->font);
	
#ifdef HYPERTEXT
	if( w->file != NULL && !hypertextOn )
		/* a hypertext file but not in hypertext mode */
		fid = w->realFileId;
	else
#endif
		fid = w->fileId;

	/* set up for the loop */
	inSelection = 0;
	gc_normal = font->gc_normal;
	gc = gc_normal;
	logicalCol = 0;
	indent = w->indent;
	lastCol += indent;
	x = w->leftMargin;
	sLine = screenLine;
	sLineMax = sLine + MAXCOLS - 1;

	/* set things up so we will call getSpan first thing */
	firstByte = (unsigned char *)1;
	lastByte = (unsigned char *)0;
	
	/* handle line numbering */
	if( w->lineNumbers && w->ln_window_id != (Window)NULL ) {
		int ln = row + w->numTopline;
		char s[12];
		
		sprintf(s, "%4d:", ln);
		XDrawImageString( MainDisplay, w->ln_window_id,
						gc_normal, 0, y, s, 5 );
	}

	/* loop until we get to the end of the line or the window */
	while( logicalCol <= lastCol ) {
		/****Begin CheckForSelection();****/
		if( selBegin <= cp && cp <= selEnd && w == selWindow ) {
			/* we are inside the selection */
			if( !inSelection ) {
				DrawString();
				if( ptOwnsSelection )
					gc = font->gc_selected;
				else
					gc = font->gc_deselected;
				inSelection = 1;
			}
		} else {
			/* we are not inside the selection */
			if( inSelection ) {
				DrawString();
				gc = font->gc_normal;
				inSelection = 0;
			}
		}
		/****End CheckForSelection();****/
		if( firstByte > lastByte ) {
			if( getSpan( fid, cp, &firstByte, &lastByte, 0 ) ) {
				uch = BLOCK_EOF;
				goto atEOF;
			}
		}
++getSpanSize;
		uch = (unsigned char)(*firstByte++);
		++cp;
		/* check limits of line buffer */
		if( sLine > sLineMax )
			sLine = sLineMax;
		switch( charTable[uch] ) {
		
		case 2:		/* end of file */
	atEOF:
			if( eofChar != -1 )
				*sLine++ = (char)eofChar;
			cp = -1;	/* indicate end of file written */
			goto endLoop;

		case 1:		/* newline -- end of line  */
			*sLine++ = ' ';
			goto endLoop;

		case 3:	/* tab */
			tabStop = tabWidth - (logicalCol % tabWidth)
					+ logicalCol;
			if( (sLine+tabWidth) > sLineMax )
				sLine = sLineMax-tabWidth;
			while( ++logicalCol <= tabStop )
				if( logicalCol >= indent )
					*sLine++ = ' ';
			--logicalCol;	/* one ++ too many above */
			break;

		case 0:	/* other (one char position) character */
			if( logicalCol++ >= indent ) {
				*sLine++ = uch;
			}
			break;

		}
	}
endLoop:
	/* write out the last part of the text */
	DrawString();
	if( fillToEol ) {
		/* clear to the end of the line */
		XClearArea(MainDisplay, w->x_window_id, x, y - font->ascent,
			0, font->height, False );
			/* 0 width => clear the whole width of the window */
			/* False means: do not generate exposure events */
	}


	/* find the end of the line */
	/* only look if we have not already gotten to EOF */
	while( uch != '\n' && uch != BLOCK_EOF ) {
			/* NOTE: */
			/* Since we are postincrementing we could move cp */
			/* two past EOF, check if this is a problem in some */
			/* other place that uses the returned value (cp) */
		uch = getFileByte( fid, cp++ );
		if( uch == '\t' )
			logicalCol += tabWidth - (logicalCol % tabWidth) - 1;
		++logicalCol;
	}

	/* adjust for the NL */
	--logicalCol;
	if( logicalCol > maxLineLength )
		maxLineLength = logicalCol;

	return cp;
}

