/*
 * strdup.c
 * 
 * Simple version of strdup for machines without it (ie DEC Ultrix 4.2)
 *
 * By David Chatterton
 * 29 July 1993
 *
 * You can do anything you like to this... :)
 */

#include <string.h>
#include <stdlib.h>

char* strdup (const char* s1)
{
	char* s2;
	if (s2 = (char*)malloc(strlen(s1)+1))
		strcpy(s2,s1);
	return s2;
}
