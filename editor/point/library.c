/* $Header: /a/cvs/386BSD/ports/editor/point/library.c,v 1.1 1994/02/15 22:12:38 jkh Exp $ */

#include "stdio.h"
#include "pt.h"
#include "ctype.h"
#include "string.h"
#include "stdlib.h"

char scratchName[FILENAMESIZE];

int
LineNumberOfSelection()
{
	extern struct window *selWindow;
	extern Offset selBegin;

	int cp = 0;
	int n;
	int line_number = 1;
	int fid;

	if( selWindow == NULL )
		return 0;

	fid = selWindow->fileId;
	
	while( cp < selBegin ) {
		n = 1;
		cp = nextLine( fid, cp, &n );
		++line_number;
	}
	if( cp > selBegin )
		--line_number;
	return line_number;
}

void
FixName( s )
	char *s;
{
	int l = strlen(s) - 1;
	char ch = s[l];
	if( ch=='@' || ch=='*' || ch=='=' )
		s[l] = '\0';
}

char *
tildefyFilename( buffer )
	char * buffer;
{
	extern char homeDirectory[];

	int home_dir_len = strlen(homeDirectory);
	char *p;

	if( strncmp(buffer, homeDirectory, home_dir_len)==0 ) {
		p = &(buffer[home_dir_len-1]);
		*p = '~';
	} else
		p = buffer;
	return p;
}

char *
findFile(name)
	char *name;
{
	extern char scratchName[];
	extern char *getenv();
	
	char *env;
	int i;

	/* first look in the current directory */
	if( access(name, 0) == 0 )
		return name;
	/* now look in the path or PATH directories */
	env = getenv("path");
	if( env == NULL ) {
		env = getenv("PATH");
		if( env == NULL )
			return NULL;
	}
	while( *env != '\0' ) {
		/* find the next directory name */
		i = 0;
		while( *env != ';' && *env != '\0' )
			scratchName[i++] = *env++;
		if( scratchName[i-1] != '/' )
			scratchName[i++] = '/';
		scratchName[i] = '\0';
		strcat(scratchName, name);
		if( access(scratchName, 0) == 0 )
			return &scratchName[0];
		if( *env != '\0' )
			++env;
	}
	return NULL;
}

/*ARGSUSED*/
char *
PtMalloc( size, purpose )
	int size;
	char *purpose;
{
	char msg_buffer[256];
	char *mem = (char *)malloc( size );
	
	if( mem == NULL ) {
		sprintf( msg_buffer,
"MEMORY ALLOCATION OF %d BYTES FOR %s FAILED. QUIT POINT AND RESTART.",
			size, purpose  );
		msg( msg_buffer, 1 );
		fprintf( stderr, "%s\n", msg_buffer );
	}
	return mem;
}

void
PtFree( s )
	char *s;
{
	if( s != NULL )
		free( s );
}

void
justifyLines()
{
	/* external declarations */
	extern struct window *selWindow;
	extern Offset selBegin, selEnd;
	extern int selMode;
	extern int rightMargin;
	extern int tabWidth;

	Offset cp, selFirst = selBegin, selLast = selEnd;
	int fileId, n, col;
	char ch, lastCh;
	int fid;

	if( selWindow == NULL )
		return;

	fid = selWindow->fileId;

	selMode = SELCHAR;
	fileId = selWindow->fileId;
	n = -1;
	selBegin = prevLine( fileId, selBegin, &n );
	n = 1;
	selEnd = nextLine( fileId, selEnd, &n );
	col = 0;
	lastCh = 0;
	while( 1 ) {
		ch = getFileByte( fid, selBegin );
		if( ch == '\n' ) {
			/* replace the LF with a space */
			selEnd = selBegin;
			(void)deleteChars( fileId, 0, 0 );
			insertChar(' ');
			ch = ' ';
		}
		if( col >= rightMargin ) {
			cp = selBegin;
			while( ch != ' ' && ch != '\n' && ch != '\t' )
				ch = (char)getFileByte(fileId, --cp);
			if( ch == ' ' || ch == '\t' ) {
				selEnd = selBegin = cp;
				(void)deleteChars(fileId, 0, 0);
				insertChar('\n');
			}
			/* else NL is already in place */
			col = 0;
			lastCh = '\n';
		} else {
			if( ch == '\t' ) {
				if( (n = (col % tabWidth)) == 0 )
					n = tabWidth;
				col += n;
			} else
				++col;
			++selBegin;
			lastCh = ch;
		}
		if( selBegin >= selLast )
			break;
	}
	/* see how many spaces and tabs are at the end */
	/* selEnd remembers the end of the string of spaces and tabs */
	selEnd = --selBegin;
	/* loop through the spaces and tabs */
	while( 1 ) {
		ch = getFileByte( fid, selBegin++ );
		if( ch != ' ' && ch != '\t' )
			break;
		--selBegin;
	}
	/* (selBegin went one too) */
	/* if we found some spaces, then delete them */
	if( ++selBegin <= selEnd ) {
		selLast -= (selEnd - selBegin + 1);
		(void)deleteChars(fileId, 0, 0);
	}
	if( lastCh != '\n' ) {
		insertChar('\n');
		selLast += 2;
	}
	selBegin = selFirst;
	selEnd = selLast - 1;
	drawWindow( selWindow );
}

