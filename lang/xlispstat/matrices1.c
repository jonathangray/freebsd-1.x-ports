/* matrices1 - Elementary matrix operations                            */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
# include "xmath.h"
# include "xlisp.h"
# include "statfloat.h"

# define seqlen(x) ((vectorp(x)) ? getsize(x) : llength(x))

/* external variables */
extern LVAL s_ielement, s_icontents;

/* external functions */
extern LVAL list2(), mklist(), displacedarraydim(), peekarg(), xsrmul(),
  coerce_to_vector(), getnextelement(), checknonnegint(), newarray(),
  arraydata(), xspermutearray(), coerce_to_list(), integer_list_2(),
  makearglist(), reduce(), xscallsubr2(), xsfuncall2();
 
/*************************************************************************/
/**                                                                     **/
/**                       Matrix Data Type                              **/
/**                                                                     **/
/*************************************************************************/

/* Many routines here assume that displacedarraydim returns a vector.    */

/* is a a matrix? */
matrixp(a)
     LVAL a;
{
  return(displacedarrayp(a) && (arrayrank(a) == 2));
}

/* get a matrix from the argument stack */
LVAL xsgetmatrix()
{
  LVAL m;

  m = xlgetarg();
  if (! matrixp(m)) xlerror("not a matrix", m);
  return(m);
}

/* number of rows in a matrix */
numrows(a)
     LVAL a;
{
  return((int) getfixnum(getelement(displacedarraydim(a), 0)));
}

/* number of columns in a matrix */
numcols(a)
     LVAL a;
{
  return((int) getfixnum(getelement(displacedarraydim(a), 1)));
}

/*************************************************************************/
/**                                                                     **/
/**                   Matrix Multiplication Functions                   **/
/**                                                                     **/
/*************************************************************************/

/* numerical inner product. Result is always of type FLOAT*/
LVAL innerproduct(x, y)
     LVAL x, y;
{
  int n, i;
  double val, xi, yi;
  int complex = FALSE;
  Complex cval, cxi, cyi;
  
  if (! vectorp(x)) xlerror("not a vector", x);
  if (! vectorp(y)) xlerror("not a vector", y);
  if (getsize(x) != getsize(y)) xlfail("vector lengths do not match");
  if (getsize(x) == 0) xlfail("vectors are too short");
  n = getsize(x);

  /* check for a complex argument */
  for (i = 0; i < n && ! complex; i++) {
    if (complexp(getelement(x, i))) complex = TRUE;
    if (complexp(getelement(y, i))) complex = TRUE;
  }
  
  if (complex) {
    cval = cart2complex(0.0, 0.0);
    for (i = 0; i < n; i++) {
      cxi = makecomplex(getelement(x, i));
      cyi = makecomplex(getelement(y, i));
      cval = cadd(cval, cmul(cxi, cyi));
    }
    return(cvcomplex(cval));
  }
  else {
    for (val = 0.0, i = 0; i < n; i++) {
      xi = makedouble(getelement(x, i));
      yi = makedouble(getelement(y, i));
      val = f_plus(val, f_times(xi, yi));
    }
    return(cvflonum((FLOTYPE) val));
  }
}

/* copy row r of matrix a into the vector x */
copy_row(a, r, x)
     LVAL a, x;
     int r;
{
  int cols, i;
  LVAL data;

  if (! matrixp(a)) xlerror("not a matrix", a);
  if (! vectorp(x)) xlerror("not a vector", x);
  if (numcols(a) != getsize(x)) xlfail("dimensions do not match");
	
  cols = numcols(a);
  data = arraydata(a);
	
  for (i = 0; i < cols; i++)
    setelement(x, i, getelement(data, cols * r + i));
}

/* copy column c of matrix a into vector x */
copy_column(a, c, x)
     LVAL a, x;
     int c;
{
  int rows, cols, i;
  LVAL data;

  if (! matrixp(a)) xlerror("not a matrix", a);
  if (! vectorp(x)) xlerror("not a vector", x);
  if (numrows(a) != getsize(x)) xlfail("dimensions do not match");
	
  rows = numrows(a);
  cols = numcols(a);
  data = arraydata(a);
	
  for (i = 0; i < rows; i++)
    setelement(x, i, getelement(data, cols * i + c));
}

/* Built in MATMULT function. Result is always of type FLOAT */
static LVAL matmult()
{
  LVAL x, y, val, dim, nn, row, col, mm, result, result_data;
  int i, j, n, m;
  int list_arg = FALSE;
	
  /* protect some pointers */
  xlstkcheck(9);
  xlsave(x);
  xlsave(y);
  xlsave(dim);
  xlsave(nn);
  xlsave(mm);
  xlsave(val);
  xlsave(row);
  xlsave(col);
  xlsave(result);

  /* scalar multiplication */
  if (numberp(peekarg(0)) || numberp(peekarg(1))) result = xsrmul();
  else {
    x = xlgetarg();
    y = xlgetarg();
    xllastarg();
	
    /* coerce lists to vectors and check */
    if (! arrayp(x) && consp(x)) { list_arg = TRUE; x = coerce_to_vector(x); }
    if (! arrayp(y) && consp(y)) { list_arg = TRUE; y = coerce_to_vector(y); }
    if ((! vectorp(x)) && (! matrixp(x))) xlbadtype(x);
    if ((! vectorp(y)) && (! matrixp(y))) xlbadtype(y);

    /* simple inner product */
    if (simplevectorp(x) && simplevectorp(y)) {
      if (getsize(x) != getsize(y))
	xlfail("dimensions do not match");
      result = innerproduct(x, y);
    }
    else {

      /* check dimensions */
      if (simplevectorp(x) && matrixp(y) && (getsize(x) != numrows(y)))
	xlfail("dimensions do not match");
      if (matrixp(x) && simplevectorp(y) && (numcols(x) != getsize(y)))
	xlfail("dimensions do not match");
      if (matrixp(x) && matrixp(y) && (numcols(x) != numrows(y)))
	xlfail("dimensions do not match");

      /* compute result dimensions */
      nn = (simplevectorp(x)) ? cvfixnum((FIXTYPE) 1)
	                      : getelement(displacedarraydim(x), 0);
      mm = (simplevectorp(y)) ? cvfixnum((FIXTYPE) 1)
                              : getelement(displacedarraydim(y), 1);
      n = getfixnum(nn);
      m = getfixnum(mm);
      dim = list2(nn, mm);
      result = newarray(dim, NIL, NIL);
	
      /* set up the vectors for holding rows and columns for innerproduct */   
      row = (simplevectorp(x)) ? x : newvector(numcols(x));
      col = (simplevectorp(y)) ? y : newvector(numrows(y));
	
      /* compute the result matrix */
      result_data = arraydata(result);
      for (i = 0; i < n; i++)
	for (j = 0; j < m; j++) {
	  if (! simplevectorp(x)) copy_row(x, i, row);
	  if (! simplevectorp(y)) copy_column(y, j, col);
	  val = innerproduct(row, col);
	  setelement(result_data, m * i + j, val);
	}
    }
  }
  
  /* reformat result if one of inputs was a sequence, one a matrix */
  if (matrixp(result) && (simplevectorp(x) || simplevectorp(y))) {
    result = (list_arg) ? coerce_to_list(arraydata(result))
                        : arraydata(result);
  }
  
  /* restore the stack frame */
  xlpopn(9);

  return(result);
}

LVAL xsmatmult()
{
  LVAL args, fcn, result;

  if (xlargc <= 2) result = matmult();
  else {
    xlstkcheck(2);
    xlsave1(args);
    xlsave1(fcn);
    fcn = cvsubr(matmult, SUBR, 0);
    args = makearglist(xlargc, xlargv);
    result = reduce(fcn, args, FALSE, NIL);
    xlpopn(2);
  }
  return(result);
}
    
/* Built in CROSS-PRODUCT function */
LVAL xscrossproduct()
{
  LVAL x, val, dim, nn, col_i, col_j, result, result_data;
  int i, j, n;
	
  x = xlgetarg();
  xllastarg();

  if (simplevectorp(x)) result = innerproduct(x, x);
  else if (matrixp(x)) {

    /* save some pointers */
    xlstkcheck(6);
    xlsave(dim);
    xlsave(nn);
    xlsave(val);
    xlsave(col_i);
    xlsave(col_j);
    xlsave(result);

    /* determine dimensions and set up result */
    nn = getelement(displacedarraydim(x), 1);
    n = getfixnum(nn);
    dim = list2(nn, nn);
    result = newarray(dim, NIL, NIL);
	
    /* set up vectors for holding columns for inner product */
    col_i = newvector(numrows(x));
    col_j = newvector(numrows(x));

    /* compute the result */
    result_data = arraydata(result);
    for (i = 0; i < n; i++)
      for (j = 0; j < n; j++) {
	copy_column(x, i, col_i);
	copy_column(x, j, col_j);
	val = innerproduct(col_i, col_j);
	setelement(result_data, n * i + j, val);
    }
		
    /* restore the stack frame */
    xlpopn(6);
  }
  else xlbadtype(x);

  return(result);
}

/* Built in OUTER-PRODUCT finction */
LVAL xsouterproduct()
{
  LVAL x, y, f, dim, val, result, result_data, xe, ye;
  int i, j, n, m;
	
  /* protect some pointers */
  xlstkcheck(5);
  xlsave(x);
  xlsave(y);
  xlsave(dim);
  xlsave(val);
  xlsave(result);

  x = xlgetarg();
  x = coerce_to_vector(x);
  y = xlgetarg();
  y = coerce_to_vector(y);
  f = (moreargs()) ? xlgetarg() : NIL;
  if (! simplevectorp(x)) xlerror("not a simple vector", x);
  if (! simplevectorp(y)) xlerror("not a simple vector", y);
  xllastarg();
	
  n = getsize(x);
  m = getsize(y);
  dim = integer_list_2(n, m);
  result = newarray(dim, NIL, NIL);
	
  result_data = arraydata(result);
  for (i = 0; i < n; i++)
    for (j = 0; j < m; j++) {
      xe = getelement(x, i);
      ye = getelement(y, j);
      val = (f == NIL) ? xscallsubr2(xsrmul, xe, ye) : xsfuncall2(f, xe, ye);
      setelement(result_data, m * i + j, val);
    }
		
  /* restore the stack frame */
  xlpopn(5);

  return(result);
}

/*************************************************************************/
/**                                                                     **/
/**           Matrix Construction and Decomposition Functions           **/
/**                                                                     **/
/*************************************************************************/

/* Internal version of DIAGONAL function. Given a matrix returns the */
/* diagonal; given a sequence returns a diagonal matrix.             */
LVAL diagonal(arg)
     LVAL arg;
{
  LVAL next, dim, val, data, result, result_data;
  int n, m, i;

  /* protect some pointers */
  xlstkcheck(3);
  xlsave(dim);
  xlsave(val);
  xlsave(result);
	
  if (matrixp(arg)) {

    /* extract diagonal from a matrix */
    n = (numrows(arg) < numcols(arg)) ? numrows(arg) : numcols(arg);
    m = numcols(arg);
    result = mklist(n, NIL);
    data = arraydata(arg);
    for (i = 0, next = result; i < n; i++, next = cdr(next))
      rplaca(next, getelement(data, m * i + i));
  }
  else if (sequencep(arg)) {

    /* construct a diagonal matrix */
    n = (vectorp(arg)) ? getsize(arg) : llength(arg);
    dim = cvfixnum((FIXTYPE) n);
    dim = list2(dim, dim);
    val = cvfixnum((FIXTYPE) 0);
    result = newarray(dim, s_ielement, val);
    result_data = arraydata(result);
    for (i = 0; i < n; i++)
      setelement(result_data, n * i + i, getnextelement(&arg, i));
  }
  else xlbadtype(arg);
	
  /* restore the stack frame */
  xlpopn(3);

  return(result);
}

/* Built in DIAGONAL function */
LVAL xsdiagonal()
{
  LVAL arg;

  arg = xlgetarg();
  xllastarg();

  return(diagonal(arg));
}

/* Internal IDENTITY-MATRIX function */
LVAL identitymatrix(n)
     int n;
{
  LVAL result, val;

  /* protect some pointers */
  xlstkcheck(2);
  xlsave(val);
  xlsave(result);

  val = cvfixnum((FIXTYPE) 1);
  result = mklist(n, val);
  result = diagonal(result);
  
  /* restore the stack frame */
  xlpopn(2);

  return(result);
}

/* Built in IDENTITY-MATRIX function */
LVAL xsidentitymatrix()
{
  int n;
	
  n = getfixnum(checknonnegint(xlgetarg()));
  xllastarg();
	
  return(identitymatrix(n));
}

/* Return a list of rows or columns of a matrix read from the stack */
LVAL facelist(face)
     int face;
{
  LVAL a, result, next, vect, data;
  int rows, cols, i, j;
	
  a = xsgetmatrix();
  xllastarg();

  rows = numrows(a);
  cols = numcols(a);
	
  /* protect some pointers */
  xlsave1(result);

  data = arraydata(a);
  switch(face) {
  case 0: /* rows */
    result = mklist(rows, NIL);
    for (next = result, i = 0; i < rows; i++, next = cdr(next)) {
      vect = newvector(cols);
      rplaca(next, vect);
      for (j = 0; j < cols; j++) 
	setelement(vect, j, getelement(data, cols * i + j));
    }
    break;
  case 1: /* columns */
    result = mklist(cols, NIL);
    for (next = result, j = 0; j < cols; j++, next = cdr(next)) {
      vect = newvector(rows);
      rplaca(next, vect);
      for (i = 0; i < rows; i++) 
	setelement(vect, i, getelement(data, cols * i + j));
    }
    break;
  default:
    xlfail(" bad face selector");
  }
    
  /* restore the stack frame */
  xlpop();

  return(result);
}

/* Built in ROW-LIST and COLUMN-LIST functions */
LVAL xsrowlist() { return(facelist(0)); }
LVAL xscolumnlist() { return(facelist(1)); }

/* Bind list of sequences or matrices along rows or columns */
LVAL xsbindfaces(face)
     int face;
{
  LVAL next, data, dim, result, result_data;
  int totalsize, rows, cols, c, r, n, i, j;
  
  /* protect some pointers */
  xlstkcheck(3);
  xlsave(data);
  xlsave(dim);
  xlsave(result);
  
  /* Check the first argument and establish size of the binding face */
  next = peekarg(0);
  switch (face) {
  case 0:
    if (matrixp(next)) cols = numcols(next);
    else if (sequencep(next)) cols = seqlen(next);
    else if (! compoundp(next)) cols = 1;
    else xlbadtype(next);
    break;
  case 1:
    if (matrixp(next)) rows = numrows(next);
    else if (sequencep(next)) rows = seqlen(next);
    else if (! compoundp(next)) rows = 1;
    else xlbadtype(next);
    break;
  }

  /* Pass through the arguments on the stack to determine the result size */
  n = xlargc;
  for (i = 0, totalsize = 0; i < n; i++) {
    next = peekarg(i);
    if (matrixp(next)) {
      c = numcols(next);
      r = numrows(next); 
    }
    else if (sequencep(next))
      switch (face) {
      case 0:  c = seqlen(next); r = 1; break;
      case 1:  c = 1; r = seqlen(next); break;
      }
    else if (! compoundp(next)) {
      c = 1;
      r = 1;
    }
    else xlbadtype(next);

    switch (face) {
    case 0:
      if (c != cols) xlfail("dimensions do not match");
      else totalsize += r;
      break;
    case 1:
      if (r != rows) xlfail("dimensions do not match");
      else totalsize += c;
    }
  }
  
  /* set up the result matrix */
  dim = newvector(2);
    switch (face) {
    case 0:
      setelement(dim, 0, cvfixnum((FIXTYPE) totalsize));
      setelement(dim, 1, cvfixnum((FIXTYPE) cols));
      break;
    case 1:
      setelement(dim, 0, cvfixnum((FIXTYPE) rows));
      setelement(dim, 1, cvfixnum((FIXTYPE) totalsize));
      break;
    }
  result = newarray(dim, NIL, NIL);
  result_data = arraydata(result);

  /* compute the result */
  for (r = 0, c = 0; moreargs();) {
    next = xlgetarg();
    if (matrixp(next)) {
      rows = numrows(next);
      cols = numcols(next);
      data = arraydata(next);
    }
    else {
      switch (face) {
      case 0: rows = 1; break;
      case 1: cols = 1; break;
      }
      data = coerce_to_vector(next);
    }
    switch (face) {
    case 0:
      for (i = 0; i < rows; i++, r++) 
	for (j = 0; j < cols; j++)
	  setelement(result_data, cols * r + j,
		     getelement(data, cols * i + j));
      break;
    case 1:
      for (j = 0; j < cols; j++, c++)
	for (i = 0; i < rows; i++) 
	  setelement(result_data, totalsize * i + c,
		     getelement(data, cols * i + j));
      break;
    }
  }
  
  /* restore the stack frame */
  xlpopn(3);
  
  return(result);
}

/* Built in BIND-ROWS and BIND-COLUMNS functions */
LVAL xsbindrows() { return(xsbindfaces(0)); }
LVAL xsbindcols() { return(xsbindfaces(1)); }

/* Built in TRANSPOSE function */
FORWARD LVAL transpose_list();
LVAL xstranspose()
{
  LVAL m, perm, result;

  if (consp(peekarg(0))) return(transpose_list());
  
  m = xsgetmatrix();
  xllastarg();

  xlsave1(perm);
  perm = newvector(2);
  setelement(perm, 0, cvfixnum((FIXTYPE) 1));
  setelement(perm, 1, cvfixnum((FIXTYPE) 0));
  result = xscallsubr2(xspermutearray, m, perm);
  xlpop();

  return(result);
}

extern LVAL copylist();

LOCAL LVAL transpose_list()
{
  LVAL list, result, nextr, row, nextl;
  int m, n;
  
  list = xlgalist();
  xllastarg();
  
  xlstkcheck(2);
  xlsave(result);
  xlprotect(list);
  
  list = copylist(list);
  m = llength(list);
  if (! consp(car(list))) xlerror("not a list", car(list));
  n = llength(car(list));
  
  result = mklist(n, NIL);
  for (nextr = result; consp(nextr); nextr = cdr(nextr)) {
    row = mklist(m, NIL);
    rplaca(nextr, row);
    for (nextl = list; consp(nextl); nextl = cdr(nextl)) {
      if (!consp(car(nextl))) xlerror("not a list", car(nextl));
      rplaca(row, car(car(nextl)));
      row = cdr(row);
      rplaca(nextl, cdr(car(nextl)));
    }
  }
  
  xlpopn(2);
  return(result);
}
