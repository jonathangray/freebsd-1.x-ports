/* $Header: /a/cvs/386BSD/ports/editor/point/findfiles.c,v 1.1 1994/02/15 22:12:37 jkh Exp $ */

#include <sys/types.h>
#include "pt.h"
#ifdef SYSV
#include <dirent.h>
#else
#include <sys/dir.h>
#endif
#include <ctype.h>
#include <string.h>
#include <stdio.h>

/* scratch file name */
char scratchFileName[FILENAMESIZE];

char *
makeFullPathname(origName)
	char *origName;
{
	extern char scratchFileName[];

	int n;
	register char *p;
	char *fromPtr, *toPtr;

	/* first figure out what we have to do */
	if( origName[0] != '/' ) {
		/* must prepend the current drive and directory */
		(void)getwd(scratchFileName);
		n = strlen(scratchFileName);
		if( scratchFileName[n-1] != '/' ) {
			scratchFileName[n++] = '/';
			scratchFileName[n] = '\0';
		}
	} else
		scratchFileName[0] = '\0';
	strncat(scratchFileName, origName, FILENAMESIZE);
	
	/* now eliminate any ".." components */
	p = scratchFileName;
	while( *p != '\0' ) {
		/* look for a "/../" */
		if( *p=='.' && *(p+1)=='.' && *(p-1)=='/' && *(p+2)=='/' ) {
			/* find the previous path component */
			n = 2;
			while( 1 ) {
				if( (p-n) < scratchFileName )
					/* string is "component/../ ..." */
					break;
				if( *(p-n) == '/' )
					break;
				++n;
			}
			/* eliminate the "component/../" by copying the */
			/* rest of the  string up n character positions */
			fromPtr = p + 3;
			/* *(p-n) is the last character to keep so: */
			toPtr = p - n + 1;
			/* move p to continue the scan at the beginning */
			/* of the moved part of the string */
			p = toPtr;
			while( 1 ) {
				if( (*toPtr++ = *fromPtr++) == '\0' )
					break;
			}
		} else
			++p;
	}

	return scratchFileName;
}

int
striccmp( a, b )
	char *a, *b;
{
	char cha, chb;

	if( a == NULL )
		return -1;
	else if( b == NULL )
		return 1;

	while( 1 ) {
		chb = *b++;
		if( (cha = *a++) == '\0' )
			break;
		/* not end of string for 'a' */
		if( cha != chb ) {
			if( isupper(cha) )
				cha = tolower(cha);
			if( isupper(chb) )
				chb = tolower(chb);
			if( cha != chb )	
				break;
			/* either chb is a letter hence not end of string */
			/* for 'b' or if cha != chb we break */
		} /* else cha == chb hence not end of string for 'b' either */
	}
	return cha - chb;
}

struct window *
findFilenameWindow(filename)
	char *filename;
{
	extern struct window *windowList;
	extern struct openFile *files;

	register struct window *w;

	w = windowList;
	while( w != NULL ) {
		if( striccmp(files[w->fileId].origName, filename) == 0 )
			return w;
		w = w->nextWindow;
	}
	return NULL;
}
