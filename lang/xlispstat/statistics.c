/* statistics - Basic statistical functions for XLISP-STAT.            */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
# include "xmath.h"
# include "xlisp.h"
#include "complex.h"

# define seqlen(x) ((vectorp(x)) ? getsize(x) : llength(x))
  
/* external variables */

/* external functions */
extern LVAL xssortdata(), xsapplysubr(), xscallsubr1(),
  coerce_to_list(), coerce_to_vector(), compounddataseq(), xsradd(),
  xsrmul(), xsrmin(), xsrmax(), xsreduce(), xsrdiv(), xscount(),
  subr_map_elements(), peekarg(), copyarray(), copylist(), xsrsub(),
  xsrsqrt(), xsrsub1(), makearglist(), reduce(), getnextelement(),
  recursive_subr_map_elements(), copyvector(), xsgetsequence();

extern double makedouble();
FORWARD LVAL datareduce1();

LVAL xssum()  { return(datareduce1(xssum, xsradd, cvfixnum((FIXTYPE) 0), FALSE)); }
LVAL xsprod() { return(datareduce1(xsprod, xsrmul, cvfixnum((FIXTYPE) 1), FALSE)); }
LVAL xsmin() { return(datareduce1(xsmin, xsrmin, NIL, FALSE)); }
LVAL xsmax() { return(datareduce1(xsmax, xsrmax, NIL, FALSE)); }
LVAL xscount()  { return(datareduce1(xscount, xsradd, cvfixnum((FIXTYPE) 0), TRUE)); }

static LVAL datareduce1(f, bf, nullval, count)
	LVAL (*f)(), (*bf)(), nullval;
	int count;
{
  LVAL fcn, x, result;
  
  switch (xlargc) {
  case 0: result = nullval;
  case 1: 
    if (compoundp(peekarg(0))) {
      xlstkcheck(2);
      xlsave(x);
      xlsave(fcn);
      fcn = cvsubr(bf, SUBR, 0);
      x = subr_map_elements(f);
      x = compounddataseq(x);
      result = reduce(fcn, x, FALSE, NIL);
      xlpopn(2);
    }
    else result = (count) ? cvfixnum((FIXTYPE) 1) : xlgetarg();
    break;
  default:
    xlsave1(x);
    x = makearglist(xlargc, xlargv);
    result = xscallsubr1(f, x);
    xlpop();
  }
  return(result);
}

/* check if all elements of the sequence x are simple (i. e. not compound) */
LOCAL all_simple(x)
     LVAL x;
{
  int i, n;

  if (! sequencep(x)) xlerror("not a sequence", x);
  if (listp(x)) {
    for (; consp(x); x = cdr(x)) 
      if (compoundp(car(x))) return(FALSE);
  }
  else {
    n = getsize(x);
    for (i = 0; i < n; i++) 
      if (compoundp(getelement(x, i))) return(FALSE);
  }
  return(TRUE);
}

static LVAL lastcdr(x)
	LVAL x;
{
  LVAL last = NIL;
  
  for (; consp(x); x = cdr(x)) last = x;
  
  return(last);
}

static LVAL elementlist(x)
	LVAL x;
{
  LVAL next, last, result;
  
  if (!compoundp(x)) result = consa(x);
  else {
    xlprot1(x);
    x = compounddataseq(x);
    x = (vectorp(x)) ? coerce_to_list(x) : copylist(x);
    if (all_simple(x)) result = x;
    else {
      for (next = x; consp(next); next = cdr(next))
        rplaca(next, elementlist(car(next)));
      result = car(x);
      last = lastcdr(car(x));
      for (next = cdr(x); consp(next); next = cdr(next)) {
        rplacd(last, car(next));
        last = lastcdr(car(next));
      }
    }
    xlpop();
  }
  return(result);
}
      
LVAL elementseq(x)
	LVAL x;
{
  if (! compoundp(x)) xlerror("not a compound data item", x);
  x = compounddataseq(x);
  if (all_simple(x)) return(x);
  else return(elementlist(x));
}

LVAL xselement_seq() { return(elementseq(xlgetarg())); }

static LVAL data;

static LVAL basequant()
{
  LVAL prob;
  double p, lowelem, highelem;
  int lowk, highk, n;
  
  prob = xlgetarg();
  p = makedouble(prob);
  if ((0.0 > p) || (1.0 < p))
    xlerror("not a probability", prob);
  xllastarg(); 
  
  n = getsize(data);
  lowk = (n - 1) * p;
  highk = (n - 1) - floor((n - 1) * (1 - p));
  lowelem = makedouble(getelement(data, lowk));
  highelem = makedouble(getelement(data, highk));
  return(cvflonum((FLOTYPE) (lowelem + highelem) / 2.0));
}

static LVAL quant() { return(recursive_subr_map_elements(basequant, quant)); }

LVAL xsquantile()
{
  LVAL result;
  
  xlsave1(data);
  data = xlgetarg();
  data = xscallsubr1(xssortdata, data);
  data = coerce_to_vector(data);
  result = quant();
  xlpop();
  return(result);
}

static LVAL base_ifelse()
{
  LVAL a, b, c;
  
  a = xlgetarg();
  b = xlgetarg();
  c = xlgetarg();
  xllastarg();
  
  return((a != NIL) ? b : c);
}

LVAL xsifelse() { return(subr_map_elements(base_ifelse)); }

typedef struct {
  double real, imag;
  int complex;
} Number;

static make_number(num, x)
     Number *num;
     LVAL x;
{
  if (fixp(x) || floatp(x)) {
    num->real = makedouble(x);
    num->imag = 0.0;
    num->complex = FALSE;
  }
  else if (complexp(x)) {
    num->real = makedouble(realpart(x));
    num->imag = makedouble(imagpart(x));
    num->complex = TRUE;
  }
  else xlerror("not a number", x);
}

static base_mean(count, mean, x)
     int *count;
     Number *mean;
     LVAL x;
{
  LVAL y;
  Number num;
  double c, p, q;
  int i, n;

  if (! compoundp(x)) {
    c = *count; p = c / (c + 1.0); q = 1.0 - p;
    make_number(&num, x);
    mean->real = p * mean->real + q * num.real;
    mean->complex = mean->complex || num.complex;
    if (mean->complex) mean->imag = p * mean->imag + q * num.imag;
    (*count)++;
  }
  else {
    x = compounddataseq(x);
    n = seqlen(x);
    for (i = 0; i < n; i++) {
      y = getnextelement(&x, i);
      base_mean(count, mean, y);
    }
  }
}

LVAL xsmean()
{
  Number mean;
  int count;
  LVAL x;

  x = xlgetarg();
  xllastarg();

  mean.real = 0.0; mean.imag = 0.0; mean.complex = FALSE;
  count = 0;
  base_mean(&count, &mean, x);
  if (mean.complex) return(newdcomplex(mean.real,mean.imag));
  else return(cvflonum((FLOTYPE) mean.real));
}

LVAL xssample()
{
  LVAL x, result, temp;
  int n, N, replace, i, j;
  
  x = xsgetsequence();
  n = getfixnum(xlgafixnum());
  N = seqlen(x);
  replace = (moreargs()) ? (xlgetarg() != NIL) : FALSE;
  
  xlstkcheck(2);
  xlprotect(x);
  xlsave(result);
  x = (listp(x)) ? coerce_to_vector(x) : copyvector(x);
  result = NIL;
  if (N > 0 && n > 0) {
    for (i = 0; i < n; i++) {
      j = (replace) ? osrand(N) : i + osrand(N - i);
      result = cons(getelement(x, j), result);
      if (! replace) {           /* swap elements i and j */
        temp = getelement(x, i);
        setelement(x, i, getelement(x, j));
        setelement(x, j, temp);
      }
    }
  }
  xlpopn(2);
  return(result);
}
