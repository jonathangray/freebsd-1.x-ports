/* $Header: /a/cvs/386BSD/ports/editor/point/lines.c,v 1.1 1994/02/15 22:12:38 jkh Exp $ */

#include <ctype.h>
#include "pt.h"

Offset
readLine( fid, cp, buffer, makeLowerCase )
	int fid;
	Offset cp;
	char *buffer;
	int makeLowerCase;
{
	extern int getSpanSize;

	char *limit = buffer + MSGBUFFERSIZE - 1;
	char ch;
	unsigned char *firstByte = (unsigned char *)1;
	unsigned char *lastByte = (unsigned char *)0;

	while( buffer < limit ) {
                if( firstByte > lastByte ) {
                        if( getSpan( fid, cp, &firstByte, &lastByte, 0 ) )
				break;
                }
		++cp;
		ch = (char)(*firstByte++);
++getSpanSize;
		if( makeLowerCase && isupper(ch) )
			ch = tolower(ch);
		*buffer++ = ch;
		if( ch == '\n' )
			break;
	}
	*buffer = '\0';
	return cp;
}

Offset
nextLine( fid, cp, n)
	int fid;
	Offset cp;
	int *n;
{
	extern int getSpanSize;

	int nLines = 0;
	unsigned char *firstByte = (unsigned char *)1;
	unsigned char *lastByte = (unsigned char *)0;

	while( nLines < *n ) {
                if( firstByte > lastByte ) {
                        if( getSpan( fid, cp, &firstByte, &lastByte, 0 ) )
				break;
                }
		if( (char)(*firstByte++) == '\n' )
			++nLines;
++getSpanSize;
		++cp;
	}
	*n = nLines;
	return cp;
}

/*
 * prevLine backs up 'n' lines or partial lines.  That is, if it starts at the
 * beginning of a line it will not count that line but if it starts in the
 * middle of a line it will count that part of the line.
 *
 * A special case is when n == -1, then prevLine backs up to the beginning
 * of the current line.  If it is already at the beginning of the line it
 * does not change cp, else it moves cp to the first character of the line.
 */
Offset
prevLine( fid, cp, n )
	int fid;
	Offset cp;
	int *n;
{
	int uch;
	int nLines = 0;
	unsigned char *firstByte = (unsigned char *)1;
	unsigned char *lastByte = (unsigned char *)0;

	/* are we already off one of the beginning? */
	if( cp <= 0 ) {
		*n = 0;
		return (Offset)0;
	}

	/* read the characters before the one we are on */
	uch = getFileByte( fid, --cp );

	/* you can't move past the beginning of the text so just return */
	if( uch == BLOCK_EOF ) {
		*n = 0;
		/* Move back to the first character of the file. */
		return (Offset)0;
	}

	/* Now see if we are starting at the beginning of a line */
	if( (char)uch == '\n' ) {
		if( *n == -1 ) {
			*n = 0;
			return cp + 1;
		}
	}

	/* since we handled the beginning of line case above.  The n==-1 */
	/* special case reduces to the case of n==1 */
	if( *n == -1 )
		*n = 1;

	/* Now loop through the lines */
	while( nLines < *n ) {
		--cp;
                if( firstByte > lastByte ) {
                        if( getSpan( fid, cp, &firstByte, &lastByte, 1 ) ) {
                        	/* end of file, count as a line */
				++nLines;
				break;
			}
                }
		if( ((char)(*lastByte--)) == '\n' )
			++nLines;
#ifdef XXXXXX
                uch = getFileByte( fid, --cp );
                if( uch == BLOCK_EOF ) {
                        ++nLines;
                        break;
                }
                if( ((char)uch) == '\n' )
                        ++nLines;
#endif
	}
	/* send back the number of lines we really did back up */
	*n = nLines;

	/* we moved one past the character we are looking for */
	return cp + 1;
}
