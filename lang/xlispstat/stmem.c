/* stmem.c - memory allocation routines.                               */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"

#ifdef MACINTOSH
#ifdef MPWC
# include <Memory.h>
# define MemErr MemError()
#else
# undef TRUE
# undef FALSE
# include <MemoryMgr.h>
#endif MPWC
#endif MACINTOSH

extern char *calloc(), *realloc();

#define nil 0L

/**************************************************************************/
/**                                                                      **/
/**             Error Message and Memory Allocation Functions            **/
/**                                                                      **/
/**************************************************************************/

StPerror(s)
	char *s;
{
  xlfail(s);
}

char *StCalloc(n, m)
	int n, m;
{
  char *val;
  
  if ((val = calloc(n, m)) == nil) {
    gc();
    if ((val = calloc(n, m)) == nil)
      StPerror("allocation failed");
  }
  return(val);
}

StFree(p)
	char *p;
{
  if (p != nil) free(p);
}

/**************************************************************************/
/**                                                                      **/
/**                  Reallocatable Data Pointer Package                  **/
/**                                                                      **/
/**  (This is needed on the Mac because the realloc function does not    **/
/**          work and also to help reduce heap fragmentation.)           **/
/**                                                                      **/
/**************************************************************************/

#ifdef MACINTOSH
typedef char **StReallocData;
#else
typedef struct{
  int size;
  char *data;
} realloc_data, *StReallocData;
#endif MACINTOSH

StReallocData StRCalloc(n, m)
	int n, m;
{
#ifdef MACINTOSH
  Handle h;
  long size;
  char *p;
  
  size = ((long) n) * ((long) m);
  if ((h = NewHandle(size)) == nil || MemErr) {
    gc();
    if ((h = NewHandle(size)) == nil || MemErr) 
      StPerror("Allocation Failed");
  }
  for (p = (char *) *h; size > 0; *p++ = 0, size--);
  return ((StReallocData) h);
#else
  StReallocData r;

  r = (StReallocData) StCalloc(sizeof(realloc_data), 1);
  r->size = n * m;
  r->data = StCalloc(r->size, 1);
  return(r);
#endif MACINTOSH
}

StRFree(d)
     StReallocData d;
{
#ifdef MACINTOSH
  if (d != nil) DisposHandle(d);
#else
  if (d != nil) {
    StFree(d->data);
    StFree(d);
  }
#endif MACINTOSH

}

StReallocData StRRealloc(d, n, m)
	StReallocData d;
	int n, m;
{
#ifdef MACINTOSH
  long oldSize, newSize, i;
  char *p;
  
  oldSize = (d != nil) ? GetHandleSize(d) : 0;
  newSize = (long) n * (long) m;
  
  if (d == nil) d = (StReallocData) NewHandle(newSize);
  else SetHandleSize(d, newSize);
  if (d == nil || MemErr) StPerror("Allocation Failed");
  
  for (p = (char *) *d, i = oldSize; i < newSize; p[i] = 0, i++);
  
  return (d);
#else
  int oldsize, size;
  
  if (d == nil) {
    d = StRCalloc(n, m);
    if (d == nil) StPerror("Allocation Failed");
  }
  else {
    size = n * m;
    d->data = realloc(d->data, size);
    if (d->data == nil) StPerror("Allocation Failed");
    oldsize = d->size;
    d->size = size;
    if (size > oldsize) {
      bzero(d->data + oldsize, size - oldsize);
    }
  }
  return(d);
#endif MACINTOSH
}

long StRSize(d)
	StReallocData d;
{
#ifdef MACINTOSH
  return ((d != nil) ? GetHandleSize(d) : 0L);
#else
  return((d != nil) ? d->size : 0);
#endif MACINTOSH
}

char *StRPtr(d)
	StReallocData d;
{
#ifdef MACINTOSH
  return ((d != nil) ? (char *) *d : nil);
#else
  if (d != nil && d->data == nil) xlfail("bad relocatable data");
  return((d != nil) ? d->data : nil);
#endif MACINTOSH
}

StRLock(d)
	StReallocData d;
{
#ifdef MACINTOSH
  HLock(d);
#endif MACINTOSH
}

StRUnlock(d)
	StReallocData d;
{
#ifdef MACINTOSH
  HUnlock(d);
#endif MACINTOSH
}

