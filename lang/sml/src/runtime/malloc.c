/* malloc.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * Versions of malloc, realloc, calloc, free and cfree that can live with ML.
 */

#ifndef NeXT /* The NeXT can use the system malloc */

#if defined(SGI) || defined(ADVICE)
/* need malloc routines for SGI multi-processor library. */

#define HEAP_SZ		0x10000		/* 64K */

static char mallocbuf[HEAP_SZ], *mallocptr=mallocbuf;

char *malloc (n)
    int		n;
{
    char	*p;

    n = (n+3) & ~3;
    if (mallocptr+n > mallocbuf+HEAP_SZ)
	die ("Too much malloc.");
    p = mallocptr;
    mallocptr += n;
    return p;
}

void free () {}

char *realloc ()
{
    die ("Didn't expect realloc.");
}

char *calloc (n, m)
    int		n,m; 
{
    char	*p = malloc(n*m);
    int		i;

    for(i=0; i<n; i++)
	p[i]=0;
    return p;
}

void cfree () {}

#else
char *malloc () { die ("malloc"); }
char *realloc () { die ("realloc"); }
char *calloc () { die ("calloc"); }
void free () { die ("free"); }
void cfree () { die ("cfree"); }
#endif
#endif !NeXT
