/* $Header: /a/cvs/386BSD/ports/editor/point/inschar.c,v 1.1 1994/02/15 22:12:37 jkh Exp $ */

#include <ctype.h>
#include <stdio.h>
#include "pt.h"

void
HandleKey( keysym, state )
	int keysym;
	int state;
{
	extern struct window *selWindow;

	char ch;

	if( selWindow == NULL )
		return;

	if( keysym <= 127 ) {
		ch = (char)keysym;
		if( state & ControlMask )
			ch &= 0x1f;
	} else {
		/* ignore shift keys */
		if( 0xffe1 <= keysym && keysym <= 0xffee )
			return;
		/* make sure it fits in one byte */
		ch = (char)(keysym & 0xff);
	}
	if( ch == '\r' )
		/* convert CRs to NLs */
		ch = '\n';
	else if( ch == '\377' )
		/* convert DELs to BSs */
		ch = '\b';
	if( (ch == '\b') && (state & (ShiftMask|ControlMask|Mod1Mask)) )
		ch = '\177';	/* word erase */
	/* cp = selBegin;	 remember where this character was inserted */
	
	insChar( ch, 1, 1 );
}


static int
doAutoIndent()
{
	extern Offset selBegin;
	extern struct window *selWindow;

	unsigned char uch;
	Offset cp;
	int n, charsInserted;
	int fid = selWindow->fileId;

	n = -1;
	cp = prevLine( fid, selBegin - 1, &n );
	charsInserted = 0;
	while( 1 ) {
		uch = getFileByte( fid, cp++ );
		switch( uch ) {
			case ' ':
			case '\t':
				insertChar( uch );
				++charsInserted;
				break;
			default:
				return charsInserted;
		}
	}
}

void
insChar( c2, update, handleBackspaces )
	int c2;
	int update;
	int handleBackspaces;
{
	extern struct window *selWindow;
	extern Offset selBegin, selEnd;
	extern int selMode;
	extern int overType;
	extern int autoIndent;
	extern int rightMargin;
	extern int insertReplaces;

	Offset cp;
	Offset saveSelBegin, saveSelEnd;
	int insrow1;
	int n, row, col;
	int y;
	char ch;
	int fid = selWindow->fileId;
	unsigned char c = (unsigned char)c2;
	
	if( selWindow == NULL )
		return;

	/* handle log windows specially */
	/* handleBackspaces==2 is a special case to avoid a loop */
	if( selWindow->isLogWindow && handleBackspaces != 2) {
		write( selWindow->toShellFD, &c, 1 );
		return;
	}

	/* erase the old selection */
	drawSelection( 1 );

	/* selection mode is character after an insert */
	selMode = SELCHAR;

	if( c == '\177' && handleBackspaces ) {	/* erase word */
		cp = selBegin;
		/* first scan back over white space */
		while( 1 ) {
			ch = (char)getFileByte( fid, --cp );
			switch( ch ) {
			case ' ':
			case '\t':
			case '\r':
			case '\n':
				break;
			default:
				goto endWhite;	/* break out of while loop */
			}
		}
	endWhite:
		/* now scan over alphanumerics */
		if( isalnum(ch) ) {
			while( isalnum(ch) ) {
				if( cp < 0 )
					break;
				ch = (char)getFileByte( fid, --cp );
			}
		} else {
			while( !isalnum(ch) && !isspace(ch) && !iscntrl(ch) ) {
				if( cp < 0 )
					break;
				ch = (char)getFileByte( fid, --cp );
			}
		}
		/* set up the selection to delete the chars passed over */
		selEnd = selBegin - 1;
		selBegin = cp + 1;
		if( selBegin <= selEnd ) {
			if( deleteChars(selWindow->fileId, NOUPDATE, 0) )
				c = '\n'; /* if so, force a drawWindow */
		} else
			selEnd = selBegin;
	} else if( c == '\b' && handleBackspaces ) {
		/* only backspace if this is NOT the first char in the file */
		if( selBegin > 0 ) {
			if( (char)getFileByte(fid,selBegin-1) == '\n'
							|| !delChar() ) {
				selEnd = --selBegin;
				/* deleted a newline? */
				if( deleteChars(selWindow->fileId,NOUPDATE,0) )
					/* if so, force a drawWindow */
					c = '\n';
			}
		}
	} else {
		if( insertReplaces && selBegin < selEnd )
			deleteChars( fid, update, 1 );
		insertChar((unsigned char)c);
		if( rightMargin < 999 && c != '\n' ) {
			OffsetToXY( selWindow, selBegin, &row, &col );
			if( col > (rightMargin-selWindow->indent+1)) {
				saveSelBegin = selBegin;
				saveSelEnd = selEnd;
				while( 1 ) {
					ch = (char)getFileByte(fid, --selBegin);
					if( ch == ' ' || ch == '\t'
					 || ch=='\n' || selBegin <= 0 )
						break;
				}
				if( ch == ' ' ) {
					selEnd = selBegin;
					deleteChars( fid, 0, 0 );
					n = 0;
				} else {
					selBegin = saveSelBegin - 1;
					n = 1;
				}
				/* no need to set selEnd since */
				/* insertChar does not use it */
				insertChar('\n');
				if( autoIndent )
					n += doAutoIndent();
				updateFile(selWindow->fileId,selBegin-1,1,0);
				selBegin = saveSelBegin + n;
				selEnd = saveSelEnd + n;
			}
		}
		if( c == '\n' && autoIndent ) {
			(void)doAutoIndent();
		}
		if( overType ) {
			selEnd = selBegin;
			ch = (char)getFileByte( fid, selBegin );
			if( ch != '\n' )
				(void)deleteChars(selWindow->fileId,NOUPDATE,0);
		}
	}
	if( !update )
		return;
	if( c == '\n' ) {
#ifdef XXXXXXX
/* I'm not sure now why this was put in, so I'll take it out */
		/* back up one character (to the one before the '\n') */
		--selBegin;
#endif
		OffsetToXY( selWindow, selBegin, &insrow1, NULL );
		/* auto-scroll at the bottom of a page */
		if( insrow1 < 0 ) {
			n = (selWindow->nRows)/3;
			selWindow->posTopline = nextLine( fid,
				selWindow->posTopline, &n);
			selWindow->numTopline += n;
			drawWindow(selWindow);
		}
#ifdef XXXXXX
		/* this is to fix the problem of the very first line of */
		/* a new file not updating correctly */
		if( selWindow->posBotline <= 0 )
			selWindow->posBotline = selBegin;
#endif
		n = indentToShowSelection(-1);
		/* find the '\n' that was inserted */
		cp = selBegin;
		while( cp > 0 && (char)getFileByte( fid, --cp ) != '\n' )
			/*EMPTY*/
			;
		/* handle the special case of inserting at the end */
		/* of the file.  In this case selWindow->posBottomline */
		/* will not have been updated correctly. */
		if( cp == fileSize(fid)-1 ) {
			drawWindow(selWindow);
		}
		updateFile( selWindow->fileId, cp, selBegin-cp, 0 );
		if( n ) {
			drawWindow(selWindow);
			return;
		}
	} else {
		int row, col;

		if( selBegin < selWindow->posTopline )
			doGoSel(selWindow);

		/* if we have to indent, then redraw the screen */
		if( indentToShowSelection(-1) ) {
			drawWindow(selWindow);
			return;
		}
		OffsetToXY( selWindow, selBegin, &row, &col );
		y = selWindow->topMargin + (selWindow->font).ascent
				+ row*((selWindow->font).height);
		updateFile( selWindow->fileId,
			(selBegin>0 ? selBegin-1 : selBegin),
			2, 1 );
	}
}


