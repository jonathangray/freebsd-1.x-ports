#include <stdio.h>	/* for typedef of void only */

/***********************************************************************
TeXindex uses  the standard  Unix  library function  qsort()  for
record sorting.  Unfortunately, qsort()  is not a stable  sorting
algorithm, so input order is not necessarily preserved for  equal
sort  keys.    This  is   important,  because   the  sorting   is
case-independent, while  the  actual  entries may  not  be.   For
example, the input

\entry{i}{22}{{\CODE{i}}}
\entry{i}{42}{{\CODE{i}}}
\entry{I}{41}{{\CODE{I}}}
\entry{I}{42}{{\CODE{I}}}

produces

\initial {I}
\entry {{\CODE{i}}}{22}
\entry {{\CODE{I}}}{41--42}
\entry {{\CODE{i}}}{42}

instead of the correct

\initial {I}
\entry {{\CODE{i}}}{22, 42}
\entry {{\CODE{I}}}{41--42}

We  therefore  provide  this  stable  shellsort  replacement  for
qsort() based  on the  code  given on  p.  116 of  Kernighan  and
Ritchie, ``The  C Programming  Language'', Prentice-Hall  (1978).
This has  order  N**1.5  average performance,  which  is  usually
slower than qsort().  In the interests of simplicity, we make  no
attempt to handle short sequences by alternative methods.

[07-Nov-86]
***********************************************************************/


#define BASE(i) &base[(i)*width]

void
qsort(base, nel, width, compar)
    char base[];	/* start of data in memory */
    int nel;		/* number of elements to be sorted */
    int width;		/* size (in bytes) of each element */
    int (*compar)();	/* comparison function */
{
    int gap;
    int i;
    int j;

    register int k;	/* inner exchange loop parameters */
    register char* p;
    register char* q;
    register char  c;

    for (gap = nel/2; gap > 0; gap /= 2)
    {
	for (i = gap; i < nel; i++)
	{
	    for (j = i-gap; j >= 0; j -= gap)
	    {
	        p = BASE(j);
		q = BASE(j+gap);
		if ((*compar)(p,q) <= 0)
		    break;	/* exit j loop */
		else
		{
		    for (k = 0; k < width; (++p, ++q, ++k))
		    {
			c = *q;
			*q = *p;
			*p = c;
		    }
		}
	    }
	}
    }
}
