/* FvwmWinList Module for Fvwm. 
 *
 *  Copyright 1994,  Mike Finger (mfinger@mermaid.micro.umn.edu or
 *                               Mike_Finger@atk.com)
 *
 * The author makes not guarantees or warantees, either express or
 * implied.  Feel free to use any contained here for any purpose, as long
 * and this and any other applicible copyrights are kept intact.

 * The functions in this source file that are based on part of the FvwmIdent
 * module for Fvwm are noted by a small copyright atop that function, all others
 * are copyrighted by Mike Finger.  For those functions modified/used, here is
 * the full, original copyright:
 *
 * Copyright 1994, Robert Nation and Nobutaka Suzuki.
 * No guarantees or warantees or anything
 * are provided or implied in any way whatsoever. Use this program at your
 * own risk. Permission to use this program for any purpose is given,
 * as long as the copyright is kept intact. */

#include "../../configure.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/time.h>

#ifdef BROKEN_SUN_HEADERS
#include "sun_headers.h"
#endif

#ifdef NEEDS_ALPHA_HEADER
#include "alpha_header.h"
#endif /* NEEDS_ALPHA_HEADER */

extern char *Module;

/******************************************************************************
  safemalloc - safely allocate memory or exit if fails.
    Orginal work from FvwmIdent:
      Copyright 1994, Robert Nation and Nobutaka Suzuki.
******************************************************************************/
char *safemalloc(int length)
{
char *ptr;
  
  if(length <= 0) length = 1;

  ptr = malloc(length);
  if(ptr == (char *)0) {
    fprintf(stderr,"%s:malloc failed",Module);
    exit(1);
  }
  return ptr;
}

/******************************************************************************
  saferealloc - safely reallocate memory or exit if fails. (Doesn't work right)
******************************************************************************/
char *saferealloc(char *ptr, int length)
{
char *newptr;

  if(length <=0) length=1;

  newptr=realloc(ptr,length);
    if (ptr == (char *)0) {
      fprintf(stderr,"%s:realloc failed",Module);
      exit(1);
    }
  return ptr;
}

/******************************************************************************
  sleep_a_little - Sleep for n microseconds
    Orginal work from FvwmIdent:
      Copyright 1994, Robert Nation and Nobutaka Suzuki.
******************************************************************************/
void sleep_a_little(int n)
{
struct timeval value;
  
  if (n <= 0) return;
  
  value.tv_usec = n % 1000000;
  value.tv_sec = n / 1000000;
  
  (void) select(1, 0, 0, 0, &value);
}

/******************************************************************************
  CopyString - A simple routine to copy a string, stripping spaces and
               mallocing space for the new string.
    Orginal work from FvwmIdent:
      Copyright 1994, Robert Nation and Nobutaka Suzuki.
******************************************************************************/
void CopyString(char **dest, char *source)
{
  int len;
  char *start;

  while(((isspace(*source))&&(*source != '\n'))&&(*source != 0)) source++;
  len = 0;
  start = source;
  while(((*source != '\n'))&&(*source != 0)) {
    len++;
    source++;
  }
  *dest = safemalloc(len+1);
  strncpy(*dest,start,len);
  (*dest)[len]=0;	  
}

/******************************************************************************
  CatString2 - Concatenates two strings into a scratch buffer
    Orginal work from FvwmIdent:
      Copyright 1994, Robert Nation and Nobutaka Suzuki.
******************************************************************************/
char *CatString2(char *a, char *b)
{
  char CatS[100];
  if (strlen(a)+strlen(b) > 99) return NULL;

  strcpy(CatS, a);
  strcat(CatS, b);
  return CatS;
}

void UpdateString(char **string,char *value)
{
  if (value==NULL) return;
  if (*string==NULL) *string=(char *)safemalloc(strlen(value)+1);
  else *string=(char *)realloc(*string,strlen(value)+1);
  strcpy(*string,value);
}

/******************************************************************************
  strncasecmp - Needed on some systems
    Orginal work from FvwmIdent:
      Copyright 1994, Robert Nation and Nobutaka Suzuki.
******************************************************************************/
#ifdef NEEDS_STRNCASECMP
int strncasecmp(char *s1,char *s2,int n)
{
  register int c1,c2;
  
  for (;;)
    {
      if (!n) return(0);
      c1 = *s1,c2 = *s2;
      if (!c1 || !c2) return(c1 - c2);
      if (isupper(c1)) c1 = 'a' - 1 + (c1 & 31);
      if (isupper(c2)) c2 = 'a' - 1 + (c2 & 31);
      if (c1 != c2) return(c1 - c2);
      n--,s1++,s2++;
    }
}
#endif
