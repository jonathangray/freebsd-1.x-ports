/* sortdata - Sorting, ordering and ranking functions. Uses C qsort.   */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xlisp.h"

extern LVAL mklist(), list2(), coerce_to_list(), arraydata(), peekarg(),
  coerce_to_vector(), displacedarraydim(), xscallsubr1(),
  makedisplacedarray(), elementseq(), makecompound();

extern double *makedouble();

/* comparison routine for qsort */
LOCAL int lcomp(px, py)
     LVAL *px, *py;
{
  LVAL x, y;	
  FIXTYPE ix, iy;
  double fx, fy;
  
  x = *px;
  y = *py;
  
  if (fixp(x) && fixp(y)) {
    ix = getfixnum(x);
    iy = getfixnum(y);
    if (ix < iy) return(-1);
    else if (ix > iy) return(1);
    else return(0);
  }
  else if (floatp(x) || floatp(y)) {
    fx = (floatp(x)) ? getflonum(x) : getfixnum(x);
    fy = (floatp(y)) ? getflonum(y) : getfixnum(y);
    if (fx < fy) return(-1);
    else if (fx > fy) return(1);
    else return(0);
  }
  else if (stringp(x) && stringp(y)) {
    return(strcmp(getstring(x), getstring(y)));
  }
  else xlfail("can't compare these");
}

/* internal sort and order routine. Returns list of list of sorted values */
/* and corresponding indices into original sequence (result of ORDER).    */
LVAL lsort()
{
  LVAL x, sortx, result, nextx, nexti, 
  result_x, result_i;
  int i, n;
  
  /* protect some pointers */
  xlstkcheck(5);
  xlsave(x);
  xlsave(sortx);
  xlsave(result);
  xlsave(result_x);
  xlsave(result_i);
  
  x = xlgetarg();
  x = elementseq(x);
  x = coerce_to_list(x);
  xllastarg();
  
  n = llength(x);
  
  /* copy x and indices to sortx */
  sortx = newvector(2 * n);
  for (i = 0, nextx = x; i < n; i++, nextx = cdr(nextx)) {
    setelement(sortx, 2 * i, car(nextx));
    setelement(sortx, 2 * i + 1, cvfixnum((FIXTYPE) i));
  }
  
  /* sort the data and get the indices */
  qsort(&getelement(sortx, 0), n, 2 * sizeof(LVAL), lcomp);
  
  /* copy the arrays to lists */
  result_x = mklist(n, NIL);
  result_i = mklist(n, NIL);
  for (i = 0, nextx = result_x, nexti = result_i; i < n;
       i++, nextx = cdr(nextx), nexti = cdr(nexti)) {
    rplaca(nextx, getelement(sortx, 2 * i));
    rplaca(nexti, getelement(sortx, 2 * i + 1));
  }
  
  result = list2(result_x, result_i);
  
  /* restore the stack frame */
  xlpopn(5);
  
  return(result);
}

/* Built in SORT-DATA function */
LVAL xssortdata() { return(car(lsort())); }

/* Built in ORDER function */
LVAL xsorder() { return(car(cdr(lsort()))); }

/* Built in RANK function */
LVAL xsrank()
{
  LVAL x, result;
  
  /* create a new stack frame */
  xlstkcheck(2);
  xlsave(x);
  xlsave(result);
  
  x = peekarg(0);
  result = xsorder();
  result = xscallsubr1(xsorder, result);
  result = makecompound(x, result);
  
  /* restore the stack frame */
  xlpopn(2);
  
  return(result);
}
