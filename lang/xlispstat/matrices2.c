/* matrices2 - Linear algebra routines and Multreg Style Sweep         */
/* Operator. Tolerance should be determined from a keyword variable;   */
/* default tolerance from a global variable.                           */ 
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xmath.h"
#include "xlisp.h"
#include "statfloat.h"

#define seqlen(x) ((vectorp(x)) ? getsize(x) : llength(x))

/* external functions */
extern LVAL list2(), coerce_to_list(), coerce_to_vector(), copyarray(),
  newarray(), arraydata(), xsgetmatrix(), xsgetsequence();
extern double macheps();

/* Built in MAKE-SWEEP-MATRIX function */
LVAL xsmakesweepmatrix()
{
  LVAL x, x_data, y, w, x_mean, y_mean, pp, dim, result, result_data;

  int n, p, i, j, k;
  double val, dxi, dyi, dv, dw, sum_w, dxik, dxjk, dyj,
  dx_meani, dx_meanj, dy_mean;
  
  /* protect some pointers */
  xlstkcheck(8);
  xlsave(x);
  xlsave(y);
  xlsave(w);
  xlsave(x_mean);
  xlsave(pp);
  xlsave(y_mean);
  xlsave(dim);
  xlsave(result);
  
  x = xsgetmatrix();
  y = xlgetarg();
  w = (moreargs()) ? xlgetarg() : NIL;
  xllastarg();
  
  y = coerce_to_vector(y);
  if (numrows(x) != getsize(y)) xlfail("dimensions do not match");
  if (w != NIL) {
    w = coerce_to_vector(w);
    if (numrows(x) != getsize(w)) xlfail("dimensions do not match");
  }
  x_data = arraydata(x);

  /* find dimensions */
  n = getsize(y);
  p = numcols(x);
  
  /* make the result matrix and the mean vector */
  pp = cvfixnum((FIXTYPE) p + 2);
  dim = list2(pp, pp);
  result = newarray(dim, NIL, NIL);
  result_data = arraydata(result);
  x_mean = newvector(p);
  
  /* find the mean of y */
  for (val = 0.0, sum_w = 0.0, i = 0; i < n; i++) {
    dyi = makedouble(getelement(y, i));
    if (w != NIL) {
      dw = makedouble(getelement(w, i));
      sum_w = f_plus(sum_w, dw);
      dyi = f_times(dyi, dw);
    }
    val = f_plus(val, dyi);
  }
  if (w == NIL) sum_w = n;
  if (sum_w <= 0) xlfail("non positive sum of weights");
  y_mean = cvflonum((FLOTYPE) val / sum_w);
  
  /* find the column means */
  for (j = 0; j < p; j++) {
    for (val = 0.0, i = 0; i < n; i++) {
      dxi = makedouble(getelement(x_data, p * i + j));
      if (w != NIL) {
	dw = makedouble(getelement(w, i));
	dxi = f_times(dxi, dw);
      }
      val = f_plus(val, dxi);
    }
    setelement(x_mean, j, cvflonum((FLOTYPE) val / sum_w));
  }
  
  /* put 1/sum_w in topleft, means on left, minus means on top */
  setelement(result_data, 0, cvflonum((FLOTYPE) 1.0 / sum_w));
  for (i = 0; i < p; i++) {
    setelement(result_data, i + 1, 
	       cvflonum((FLOTYPE) - getflonum(getelement(x_mean, i))));
    setelement(result_data, (i + 1) * (p + 2), getelement(x_mean, i));
  }
  setelement(result_data, p + 1, cvflonum((FLOTYPE) - getflonum(y_mean)));
  setelement(result_data, (p + 1) * (p + 2), y_mean);
  
  /* put sums of adjusted cross products in body */
  for (i = 0; i < p; i ++) {
    for (j = i; j < p; j++) {
      for (val = 0.0, k = 0; k < n; k++) {
	dxik = makedouble(getelement(x_data, p * k + i));
	dxjk = makedouble(getelement(x_data, p * k + j));
	dx_meani = getflonum(getelement(x_mean, i));
	dx_meanj = getflonum(getelement(x_mean, j));
	dv = f_times(f_minus(dxik, dx_meani), f_minus(dxjk, dx_meanj));
	if (w != NIL) {
	  dw = makedouble(getelement(w, k));
	  dv = f_times(dv, dw);
	}
	val = f_plus(val, dv);
      }
      setelement(result_data, (i + 1) * (p + 2) + (j + 1), 
		 cvflonum((FLOTYPE) val));
      setelement(result_data, (j + 1) * (p + 2) + (i + 1), 
		 cvflonum((FLOTYPE) val));
    }
    for (val = 0.0, j = 0; j < n; j++) {
      dxik = makedouble(getelement(x_data, p * j + i));
      dyj = makedouble(getelement(y, j));
      dx_meani = getflonum(getelement(x_mean, i));
      dy_mean = getflonum(y_mean);
      dv = f_times(f_minus(dxik, dx_meani), f_minus(dyj, dy_mean));
      if (w != NIL) {
	dw = makedouble(getelement(w, j));
	dv = f_times(dv, dw);
      }
      val = f_plus(val, dv);
    }
    setelement(result_data, (i + 1) * (p + 2) + (p + 1), 
	       cvflonum((FLOTYPE) val));
    setelement(result_data, (p + 1) * (p + 2) + (i + 1), 
	       cvflonum((FLOTYPE) val));
  }
  for (val = 0.0, j = 0; j < n; j++) {
    dyj = makedouble(getelement(y, j));
    dy_mean = getflonum(y_mean);
    dv = f_times(f_minus(dyj, dy_mean), f_minus(dyj, dy_mean));
    if (w != NIL) {
      dw = makedouble(getelement(w, j));
      dv = f_times(dv, dw);
    }
    val = f_plus(val, dv);
  }
  setelement(result_data, (p + 1) * (p + 2) + (p + 1), 
	     cvflonum((FLOTYPE) val));
  
  /* restore the stack frame */
  xlpopn(8);
  
  return(result);
}

/* Internal sweeping algorithm */
sweep_in_place(a, k, tol)
     LVAL a;
     int k;
     double tol;
{
  LVAL data;
  int rows, cols, i, j;
  double pivot, aij, aik, akj, akk, meps;
  
  if (! matrixp(a)) xlerror("not a matrix", a);
  
  if ((k < 0) || (k >= numrows(a)) || (k >= numcols(a)))
    xlfail("index out of range");
  
  meps = macheps();
  if (tol < meps) tol = meps;
  
  rows = numrows(a);
  cols = numcols(a);
  data = arraydata(a);

  pivot = makedouble(getelement(data, cols * k + k));
  
  if ((pivot > tol) || (pivot < -tol)) {
    for (i = 0; i < rows; i++)
      for (j = 0; j < cols; j++)
	if ((i != k) && (j != k)) {
	  aij = makedouble(getelement(data, cols * i + j));
	  aik = makedouble(getelement(data, cols * i + k));
	  akj = makedouble(getelement(data, cols * k + j));
	  aij = aij - f_divide(f_times(aik, akj), pivot);
	  setelement(data, cols * i + j, cvflonum((FLOTYPE) aij));
	}
    for (i = 0; i < rows; i++) {
      aik = makedouble(getelement(data, cols * i + k));
      if (i != k) {
	aik = f_divide(aik, pivot);
	setelement(data, cols * i + k, cvflonum((FLOTYPE) aik));
      }
    }
    for (j = 0; j < cols; j++) {
      akj = makedouble(getelement(data, cols * k + j));
      if (j != k) {
	akj = -f_divide(akj, pivot);
	setelement(data, cols * k + j, cvflonum((FLOTYPE) akj));
      }
    }
    akk = f_divide(1.0, pivot);
    setelement(data, cols * k + k, cvflonum((FLOTYPE) akk));
    return (1);
  }
  else
    return (0);
}

/* Built in SWEEP-OPERATOR function. Should allow in place sweep as option */
LVAL xssweepoperator()
{
  LVAL a, columns, sweptcolumns, tolerances, col;
  LVAL result;
  int c;
  double tol = .000001;
  
  /* create a new stack frame */
  xlstkcheck(3);
  xlsave(sweptcolumns);
  xlsave(tolerances);
  xlsave(result);
  
  a = xsgetmatrix();
  columns = xsgetsequence();
  columns = coerce_to_list(columns);
  tolerances = (moreargs()) ? xsgetsequence() : NIL;
  tolerances = coerce_to_list(tolerances);
  xllastarg();
  
  result = copyarray(a);
  sweptcolumns = NIL;
  
  for (; consp(columns); 
       columns = cdr(columns), 
       tolerances = (consp(tolerances)) ? cdr(tolerances) : NIL) {
    col = car(columns);
    if (! fixp(col)) xlerror("not an integer", col);
    c = getfixnum(col);
    tol = (consp(tolerances))  ? makedouble(car(tolerances)) : tol;
    if (sweep_in_place(result, c, tol)) sweptcolumns = cons(col, sweptcolumns);
  }
  result = list2(result, sweptcolumns);
  
  /* restore the previous stack frame */
  xlpopn(3);
  
  return(result);
}
