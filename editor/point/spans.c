/* $Header: /a/cvs/386BSD/ports/editor/point/spans.c,v 1.1 1994/02/15 22:12:40 jkh Exp $ */

#include <ctype.h>
#include "pt.h"

Offset
searchSpans( fileId, startCp, stopCp, patString, patLength, linesPassed )
	int fileId, patLength, *linesPassed;
	Offset startCp, stopCp;
	char *patString;
{
	extern int ignoreCase;
	extern int findWholeWords;
	extern int getSpanSize;

	char *pat;
	unsigned char *firstChar;
	unsigned char *lastChar;
	unsigned char *p;
	char ch1, ch2, ch3;
	int matched, len, nLines;
	Offset cp, plen;

	/* find the upper and lower case character */
	ch1 = *patString;
	if( !ignoreCase )
		ch2 = ch1;
	else if( 'a' <= ch1 && ch1 <= 'z' )
		ch2 = ch1 - 'a' + 'A';
	else if( 'A' <= ch1 && ch1 <= 'Z' )
		ch2 = ch1 - 'A' + 'a';
	else
		ch2 = ch1;

	/* set up a long version of patLength-1 for comparisons */
	plen = patLength - 1;
	
	nLines = 0;
	
	/* set things up so getSpan is called right away */
	firstChar = (unsigned char *)1;
	lastChar = (unsigned char *)0;

	/* each iteration of this loop scans one span */
	while( 1 ) {
		/* see if there are enough characters left in the */
		/* area we are searching to match the pattern */
		if( (stopCp - startCp) < plen )
			break;

		/* find the first character of the string */
		if( firstChar > lastChar ) {
			if( getSpan(fileId, startCp, &firstChar, &lastChar, 0) )
				/* getSpan says startCp at EOF */
				break;
			/* check to see if the span is longer than the */
			/* area we are supposed to search */
			if( (lastChar-firstChar) > (stopCp-startCp) ) {
				/* if it is too long then adjust lastChar */
				lastChar = firstChar + (stopCp - startCp);
			}
		}
		len = (int)(lastChar - firstChar) + 1;
getSpanSize += len;
		if( ch1 == ch2 )
			p = match2dn(firstChar, len, ch1);
		else
			p = match1dn(firstChar, len, ch1, ch2);
		if( p == NULL ) {
			startCp += len;
			nLines += countnl(firstChar, len);
			firstChar = (unsigned char *)1;
			lastChar = (unsigned char *)0;
			continue;
		}
		/* move startCp up past the matched character */
		len = p - firstChar;
		nLines += countnl(firstChar, len);
		startCp += len;
		
		firstChar = p;

		/* start looking at the second character of the pattern */
		pat = patString + 1;
		matched = 1;	/* 1 character matched so far */
		
		/* search for a match at startCp */
		cp = startCp;
		while( matched < patLength ) {
			/* see if we are still in the span */
			if( firstChar > lastChar ) {
				if(getSpan(fileId,cp,&firstChar,&lastChar,0))
					/* at EOF so not found */
					break;
			}
			/* stop at a mismatch */
			ch3 = *firstChar++;
++getSpanSize;
			++cp;
			if( ignoreCase && 'A' <= ch3 && ch3 <= 'Z' )
				ch3 = ch3 - 'A' + 'a';
			if( *pat++ != ch3 )
				break;
			if( ch3 == '\n' )
				++nLines;
			++matched;
		}
		if( matched == patLength ) {
			/* we found the string */
			/* now does it have to be a whole word? */
			if( findWholeWords ) {
				/* make sure the character before it is */
				/* not alphanumeric or "_" */
				ch3 = (char)getFileByte(fileId, startCp-2);
				matched = !isalnum(ch3) && ch3 != '_';
				if( matched ) {
					/* and the char after it also */
					ch3 = (char)getFileByte(fileId,
						startCp+patLength-1);
					matched = !isalnum(ch3) && ch3 != '_';
				}
			} else
				matched = 1;
			if( matched ) {
				*linesPassed = nLines;
				return startCp - 1;
			}
		}
		firstChar = (unsigned char *)1;
		lastChar = (unsigned char *)0;
	}
	*linesPassed = nLines;
	return (Offset)(-1);
}

Offset
searchReverseSpans(fileId, startCp, stopCp, patString, patLength, linesPassed)
	int fileId, patLength, *linesPassed;
	Offset startCp, stopCp;
	char *patString;
{
	extern int ignoreCase;
	extern int findWholeWords;
	extern int getSpanSize;

	char *pat;
	unsigned char *firstChar;
	unsigned char *lastChar;
	unsigned char *p;
	char ch1, ch2, ch3;
	int matched, len, nLines;
	Offset cp;

	/* find the upper and lower case character */
	ch1 = *patString;
	if( !ignoreCase )
		ch2 = ch1;
	else if( 'a' <= ch1 && ch1 <= 'z' )
		ch2 = ch1 - 'a' + 'A';
	else if( 'A' <= ch1 && ch1 <= 'Z' )
		ch2 = ch1 - 'A' + 'a';
	else
		ch2 = ch1;

	/* set up nLines correctly */
	nLines = 0;
	len = patLength - 1;

	/* set things up so getSpan is called right away */
	firstChar = (unsigned char *)1;
	lastChar = (unsigned char *)0;

	/* each iteration of this loop scans one span */
	while( stopCp >= startCp ) {

		/* find the first character of the string */
		if( firstChar > lastChar ) {
			/* '1' (last argument) means get a reversed span */
			if(getSpan(fileId, stopCp, &firstChar, &lastChar, 1))
				/* at EOF */
				break;
		}
		len = (int)(lastChar - firstChar) + 1;
getSpanSize += len;
		p = match1up(lastChar, len, ch1, ch2);
		if( p == NULL ) {
			stopCp -= len;
			nLines += countnl(firstChar, len);
			firstChar = (unsigned char *)1;
			lastChar = (unsigned char *)0;
			continue;
		}
		/* move stopCp up past the matched character */
		/* (remember that match1up returns a pointer to the */
		/* character BEFORE the one matched) */
		len = lastChar - p;
		stopCp -= len;

		/* p is one before the found character */
		/* we want to match starting one character after */
		/* the found character, so we add 2 */
		firstChar = p + 2;
		nLines += countnl(firstChar, len);
		firstChar = (unsigned char *)1;
		lastChar = (unsigned char *)0;

		/* start looking at the second character of the pattern */
		pat = patString + 1;
		matched = 1;	/* 1 matched character so */
		
		/* search for a match at startCp */
		cp = stopCp + 2;
		while( matched < patLength ) {
			/* see if we are still in the span */
			if( firstChar > lastChar ) {
				if(getSpan(fileId,cp,&firstChar,&lastChar,0))
					/* at EOF so not found */
					break;
			}
			/* stop at a mismatch */
			ch3 = *firstChar++;
++getSpanSize;
			++cp;
			if( ignoreCase && 'A' <= ch3 && ch3 <= 'Z' )
				ch3 = ch3 - 'A' + 'a';
			if( *pat++ != ch3 )
				break;
			++matched;
		}
		if( matched == patLength ) {
			/* we found the string */
			/* now does it have to be a whole word? */
			if( findWholeWords ) {
				/* make sure the character before it is */
				/* not alphanumeric or "_" */
				ch3 = (char)getFileByte(fileId, stopCp);
				matched = !isalnum(ch3) && ch3 != '_';
				if( matched ) {
					/* and the char after it also */
					ch3 = (char)getFileByte(fileId,
						stopCp+patLength+1);
					matched =
						!isalnum(ch3) && ch3 != '_';
				}
			} else
				matched = 1;
			if( matched ) {
				*linesPassed = nLines;
				return stopCp + 1;
			}
		}
		firstChar = (unsigned char *)1;
		lastChar = (unsigned char *)0;
	}
	*linesPassed = nLines;
	return (Offset)(-1);
}

unsigned char *
match1up( start, len, ch1, ch2 )
	unsigned char *start;
	int len;
	int ch1, ch2;
{
	unsigned char ch;

	while( len-- > 0 ) {
		ch = *start--;
		if( ch == ch1 || ch == ch2 )
			return start;
	}
	return (unsigned char *)NULL;
}

unsigned char *
match1dn( start, len, ch1, ch2 )
	unsigned char *start;
	int len;
	int ch1, ch2;
{
	unsigned char ch;

	while( len-- > 0 ) {
		ch = *start++;
		if( ch == (unsigned char)ch1 || ch == (unsigned char)ch2 )
			return start;
	}
	return (unsigned char *)NULL;
}

unsigned char *
match2dn( start, len, ch1 )
	unsigned char *start;
	int len;
	int ch1;
{
	unsigned char ch;

	while( len-- > 0 ) {
		ch = *start++;
		if( ch == (unsigned char)ch1 )
			return start;
	}
	return (unsigned char *)NULL;
}

int
countnl( start, len )
	unsigned char *start;
	int len;
{
	unsigned char ch;
	int count = 0;

	while( len-- > 0 ) {
		ch = *start++;
		if( ch == '\n' )
			++count;
	}
	return count;
}
