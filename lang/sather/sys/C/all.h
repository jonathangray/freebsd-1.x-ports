/* File: sather/sys/C/all.h
 * Author: Stephen M. Omohundro
 * Copyright (C) International Computer Science Institute, 1990, 1991, 1992, 1993 
 *
 * COPYRIGHT NOTICE: This code is provided "AS IS" WITHOUT ANY WARRANTY
 * and is subject to the terms of the SATHER LIBRARY GENERAL PUBLIC
 * LICENSE contained in the file: "sather/doc/license.txt" of the Sather
 * distribution. The license is also available from ICSI, 1947 Center
 * St., Suite 600, Berkeley CA 94704, USA.
 * 
 * Changes: Heinz W. Schmidt (hws@csis.dit.csiro.au)
 * (c) Commonwealth Scientific and Industrial Research Organisation (CSIRO),
 * Australia, 1992, 1993.
 * The modifications are provided "AS IS" WITHOUT ANY WARRANTY and are subject
 * to the terms of the SATHER LIBRARY GENERAL PUBLIC LICENCE referred to above.
 * -- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
 ** FUNCTION: Minimal header file to be included in C files to be linked 
 **           with Sather.
 **
 ** RCS: $Id: all.h,v 1.1 1994/02/12 03:23:35 hsu Exp $
 ** HISTORY:
 ** Last edited: Oct 24 19:08 1993 (hws)
 **  Oct 24 18:35 1993 (hws): various fixes merged from solaris port
 **  Sep  6 17:07 1993 (hws): include redef of malloc for ssather.c 
 **             and external C packages in general
 ** Created: Fri Sep 14 14:23:04 1990
 */

/* Generic pointer */
typedef char *ptr;
/* Sather boolean type */
typedef char bool;
/* and real */
typedef float real;

#ifdef GC_
# ifdef GC_DEBUG
#   define gc_malloc(sz) GC_debug_malloc(sz, __FILE__, __LINE__)
#   define gc_malloc_atomic(sz) GC_debug_malloc_atomic(sz, __FILE__, __LINE__)
#   define gc_realloc(old, sz) GC_debug_realloc(old, sz, __FILE__, __LINE__)
#   define gc_free(p) GC_debug_free(p)
#   define gc_init(p) GC_init(p)
# else
#   define gc_malloc(sz) GC_malloc(sz)
#   define gc_malloc_atomic(sz) GC_malloc_atomic(sz)
#   define gc_realloc(old, sz) GC_realloc(old, sz)
#   define gc_free(p) GC_free(p)
#   define gc_init(p) GC_init(p)
# endif
#endif

/* If GC is on, and user calls "malloc" or "calloc", things may get messed up. 
   NOTE: "malloc", "realloc", "free" are provided in "interface.c". */
#ifdef GC_
#  define alloca gc_malloc
#  define malloc(n) gc_malloc(n)
#  define calloc(m,n) gc_malloc((m)*(n))
#  define realloc(ptr,size) gc_realloc(ptr,size)
#  define free(ptr) gc_free(ptr)
extern char* GC_malloc();
extern char* GC_malloc_atomic();
extern char* GC_realloc();
/* Since allocation of envi vars uses malloc, sather calls setenv_, putenv_.
** sys/C/envivars_ provides a gc_malloc'ing version of these routines if
** GC_ is on. Moreover it provides a non mallocing version of setenv, putenv
** if these are not supported on some hosts. Are they?  (3/92 hws)
*/
#  define putenv(env) (putenv_(env))
#  define setenv(env) (setenv_(env))
#  ifdef __STDC__
extern int putenv_(char *name);
extern int setenv_(char *name, char *value, int ov_write);
extern char *strdup_(char *s);
#  else
extern int putenv_();
extern int setenv_();
extern char *strdup_();
#  endif
#endif

/* bcopy and bzero are not supported on all SYSV platforms */
#if defined(__svr4__) || defined(solaris)
#   define bcopy(x,y,n) memcpy(y,x,n)
#   define bzero(x,n)  memset(x, 0, n)
#endif
