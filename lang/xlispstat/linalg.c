/* linalg - Lisp interface for basic linear algebra routines           */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "linalg.h"
extern double rcondest();
extern LVAL xsgetsequence(), xsgetmatrix();

extern LVAL sk_width, sk_type;

/************************************************************************/
/**                                                                    **/
/**                 Storage Allocation Functions                       **/
/**                                                                    **/
/************************************************************************/

static char *allocate(n, m)
	unsigned n, m;
{
  char *p = calloc(n, m);
  if (p == nil) xlfail("allocation failed");
  return(p);
}

static free_alloc(p)
	char *p;
{
  if (p != nil) free(p);
}

IVector ivector(n)
	unsigned n;
{
  return((IVector) allocate(n, sizeof(int)));
}

RVector rvector(n)
	unsigned n;
{
  return((RVector) allocate(n, sizeof(double)));
}

CVector cvector(n)
	unsigned n;
{
  return((CVector) allocate(n, sizeof(Complex)));
}

free_vector(v) Vector v; { free_alloc(v); }

IMatrix imatrix(n, m)
	unsigned n, m;
{
  int i;
  IMatrix mat = (IMatrix) allocate(n, sizeof(IVector));
  for (i = 0; i < n; i++) mat[i] = (IVector) allocate(m, sizeof(int));
  return(mat);
}

RMatrix rmatrix(n, m)
	unsigned n, m;
{
  int i;
  RMatrix mat = (RMatrix) allocate(n, sizeof(RVector));
  for (i = 0; i < n; i++) mat[i] = (RVector) allocate(m, sizeof(double));
  return(mat);
}

CMatrix cmatrix(n, m)
	unsigned n, m;
{
  int i;
  CMatrix mat = (CMatrix) allocate(n, sizeof(CVector));
  for (i = 0; i < n; i++) mat[i] = (CVector) allocate(m, sizeof(Complex));
  return(mat);
}

free_matrix(mat, n)
	Matrix mat;
	int n;
{
  int i;
  
  if (mat != nil) for (i = 0; i < n; i++) free_alloc(mat[i]);
  free_alloc(mat);
}

/************************************************************************/
/**                                                                    **/
/**             Lisp to/from C Data Translation Functions              **/
/**                                                                    **/
/************************************************************************/

static baddata(data)
	LVAL data;
{
  xlerror("bad linear algebra data", data);
}

static badmatrix(data)
	LVAL data;
{
  xlerror("not a matrix", data);
}

static badsquarematrix(data)
	LVAL data;
{
  xlerror("not a square matrix", data);
}

static badsequence(data)
	LVAL data;
{
  xlerror("not a sequence", data);
}

static data_mode(data)
	LVAL data;
{
  LVAL item;
  int i, n;
  int mode = IN;
  
  if (! consp(data) && ! vectorp(data) && ! displacedarrayp(data))
    baddata(data);
    
  if (! consp(data)) data = arraydata(data);
  n = seqlen(data);
  for (i = 0; i < n; i++) {
    item = getnextelement(&data, i);
    if (! realp(item) && ! complexp(item)) baddata(item);
    if (floatp(item) && mode == IN) mode = RE;
    else if (complexp(item)) mode = CX;
  }
  return(mode);
}

static Vector data_to_vector(data, mode)
	LVAL data;
	int mode;
{
  LVAL item;
  int i, n;
  Vector v;
  IVector iv;
  RVector rv;
  CVector cv;
  
  if (! sequencep(data)) baddata(data);
  n = seqlen(data);
  
  switch (mode) {
  case IN: iv = ivector(n); v = (Vector) iv; break;
  case RE: rv = rvector(n); v = (Vector) rv; break;
  case CX: cv = cvector(n); v = (Vector) cv; break;
  }
  
  for (i = 0; i < n; i++) {
    item = getnextelement(&data, i);
    switch (mode) {
    case IN: iv[i] = getfixnum(item);   break;
    case RE: rv[i] = makedouble(item);  break;
    case CX: cv[i] = makecomplex(item); break;
    }
  }
  return(v);
}

static Matrix data_to_matrix(data, mode)
	LVAL data;
	int mode;
{
  LVAL item;
  int i, j, n, m;
  Matrix mat;
  IMatrix imat;
  RMatrix rmat;
  CMatrix cmat;
  
  if (! matrixp(data)) baddata(data);
  n = numrows(data); m = numcols(data);
  
  switch (mode) {
  case IN: imat = imatrix(n, m); mat = (Matrix) imat; break;
  case RE: rmat = rmatrix(n, m); mat = (Matrix) rmat; break;
  case CX: cmat = cmatrix(n, m); mat = (Matrix) cmat; break;
  }
  
  data = arraydata(data);
  for (i = 0; i < n; i++) {
    for (j = 0; j < m; j++) {
      item = getelement(data, i * m + j);
      switch (mode) {
      case IN: imat[i][j] = getfixnum(item);   break;
      case RE: rmat[i][j] = makedouble(item);  break;
      case CX: cmat[i][j] = makecomplex(item); break;
      }
    }
  }
  return(mat);
}

static LVAL vector_to_data(v, n, mode, as_list)
	Vector v;
	int n, mode, as_list;
{
  LVAL data;
  int i;
  IVector iv = (IVector) v;
  RVector rv = (RVector) v;
  CVector cv = (CVector) v;
  
  xlsave1(data);
  data = newvector(n);
  for (i = 0; i < n; i++) {
    switch (mode) {
    case IN: setelement(data, i, cvfixnum((FIXTYPE) iv[i])); break;
    case RE: setelement(data, i, cvflonum((FLOTYPE) rv[i])); break;
    case CX: setelement(data, i, cvcomplex(cv[i]));          break;
    }
  }
  if (as_list) data = coerce_to_list(data);
  xlpop();
  return(data);
}

static LVAL matrix_to_data(mat, n, m, mode)
	Matrix mat;
	int n, m, mode;
{
  LVAL data, dim, data_matrix;
  int i, j;
  IMatrix imat = (IMatrix) mat;
  RMatrix rmat = (RMatrix) mat;
  CMatrix cmat = (CMatrix) mat;
  
  xlstkcheck(2);
  xlsave(dim);
  xlsave(data_matrix);
  
  dim = integer_list_2(n, m);
  data_matrix = newarray(dim, NIL, NIL);
  data = arraydata(data_matrix);
  for (i = 0; i < n; i++) {
    for (j = 0; j < m; j++) {
      switch (mode) {
      case IN: setelement(data, i * m + j, cvfixnum((FIXTYPE) imat[i][j])); break;
      case RE: setelement(data, i * m + j, cvflonum((FLOTYPE) rmat[i][j])); break;
      case CX: setelement(data, i * m + j, cvcomplex(cmat[i][j]));          break;
      }
    }
  }
  xlpopn(2);
  return(data_matrix);
}

/************************************************************************/
/**                                                                    **/
/**                  Machine Epsilon Determination                     **/
/**                                                                    **/
/************************************************************************/

double macheps()
{
  static int calculated = FALSE;
  static double epsilon = 1.0;
  
  if (! calculated)
    while (1.0 + epsilon / 2.0 != 1.0) epsilon = epsilon / 2.0;
  calculated = TRUE;
  return(epsilon);
}

/************************************************************************/
/**                                                                    **/
/**            Lisp Interfaces to Linear Algebra Routines              **/
/**                                                                    **/
/************************************************************************/

LVAL xslu_decomp()
{
  LVAL data, result, temp;
  Matrix mat;
  IVector iv;
  double d;
  int n, m, mode, singular;
  
  data = xlgetarg();
  xllastarg();
  
  if (! matrixp(data)) badmatrix(data);
  n = numrows(data);
  m = numcols(data);
  if (n != m || n <= 0 || m <= 0) badsquarematrix(data);
  
  xlsave1(result);
  result = mklist(4, NIL);
  temp = result;
  
  mode = data_mode(data);
  if (mode == IN) mode = RE;
  mat = data_to_matrix(data, mode);
  iv = ivector(n);
  singular = crludcmp(mat, n, iv, mode, &d);
  rplaca(temp, matrix_to_data(mat, n, n, mode));    temp = cdr(temp);
  rplaca(temp, vector_to_data(iv, n, IN, FALSE));   temp = cdr(temp);
  rplaca(temp, cvflonum((FLOTYPE) d));              temp = cdr(temp);
  rplaca(temp, (singular) ? s_true : NIL);

  free_matrix(mat, n);
  free_vector(iv);
  xlpop();
  return(result);
}

LVAL xslu_solve()
{
  LVAL ludecomp, la, lindx, lb, result;
  Matrix a;
  Vector b, indx;
  int n, m, a_mode, b_mode, mode, singular;
  
  ludecomp = xlgalist();
  lb = xlgetarg();
  xllastarg();
  
  la = (consp(ludecomp)) ? car(ludecomp) : NIL;
  lindx = (consp(cdr(ludecomp))) ? car(cdr(ludecomp)) : NIL;
  if (! matrixp(la)) badmatrix(la);
  n = numrows(la);
  m = numcols(la);
  
  if (n != m || n <= 0 || m <= 0) badsquarematrix(la);
  
  if (! sequencep(lindx)) badsequence(lindx);
  if (data_mode(lindx) != IN) xlerror("not an integer sequence", lindx);
  if (! sequencep(lb)) badsequence(lb);
  if (seqlen(lb) != n) xlerror("bad sequence length", lb);
  
  a_mode = data_mode(la);
  b_mode = data_mode(lb);
  mode = max(a_mode, b_mode);
  if (mode == IN) mode = RE;
  
  a = data_to_matrix(la, mode);
  indx = data_to_vector(lindx, IN);
  b = data_to_vector(lb, mode);
  
  singular = crlubksb(a, n, indx, b, mode);
  result = vector_to_data(b, n, mode, listp(lb));
  free_matrix(a, n);
  free_vector(indx);
  free_vector(b);
  
  if (singular) xlfail("matrix is (numerically) singular");
  return(result);
}

LVAL xslu_determinant()
{
  LVAL data, result;
  Matrix mat;
  CMatrix cmat;
  RMatrix rmat;
  IVector iv;
  double d, rd1, d2, magn;
  Complex cd1;
  int m, n, i, mode;
  
  data = xlgetarg();
  xllastarg();
  
  if (! matrixp(data)) badmatrix(data);
  m = numrows(data);
  n = numcols(data);
  if (n != m || n <= 0 || m <= 0) badsquarematrix(data);
    
  mode = data_mode(data);
  if (mode == IN) mode = RE;
  mat = data_to_matrix(data, mode);
  iv = ivector(n);
  crludcmp(mat, n, iv, mode, &d);

  switch (mode) {
  case RE: 
    rmat = (RMatrix) mat;
    rd1 = d;
    d2 = 0.0;
    for (i = 0; i < n; i++) {
      if ((magn = fabs(rmat[i][i])) == 0.0) {
        rd1 = 0.0;
        break;
      }
      rd1 *= rmat[i][i] / magn;
      d2 += log(magn);
    }
    result = cvflonum((FLOTYPE) rd1 * exp(d2));
    break;
  case CX:
    cmat = (CMatrix) mat;
    cd1 = cart2complex(d, 0.0);
    d2 = 0.0;
    for (i = 0; i < n; i++) {
      if ((magn = modulus(cmat[i][i])) == 0.0) {
        cd1 = cart2complex(0.0, 0.0);
        break;
      }
      cd1 = polar2complex(modulus(cd1), phase(cd1) + phase(cmat[i][i]));
      d2 += log(magn);
    }
    result = cvcomplex(cmul(cd1, cart2complex(exp(d2), 0.0)));
    break;
  }
  
  free_matrix(mat, n);
  free_vector(iv);
  return(result);
}

LVAL xslu_inverse()
{
  LVAL data, result;
  Matrix mat, inv;
  CMatrix cinv;
  RMatrix rinv;
  CVector cv;
  RVector rv;
  Vector v;
  IVector iv;
  double d;
  int m, n, i, j, mode, singular;
  
  data = xlgetarg();
  xllastarg();
  
  if (! matrixp(data)) badmatrix(data);
  m = numrows(data);
  n = numcols(data);
  if (n != m || n <= 0 || m <= 0) badsquarematrix(data);
    
  mode = data_mode(data);
  if (mode == IN) mode = RE;
  mat = data_to_matrix(data, mode);
  iv = ivector(n);
  inv = (mode == RE) ? (Matrix) rmatrix(n,n) : (Matrix) cmatrix(n,n);
  rinv = (RMatrix) inv;
  cinv = (CMatrix) inv;
  v = (mode == RE) ? (Vector) rvector(n) : (Vector) cvector(n);
  rv = (RVector) v;
  cv = (CVector) v;
  
  singular = crludcmp(mat, n, iv, mode, &d);

  if (! singular) {
    for (j = 0; j < n; j++) {
      for (i = 0; i < n; i++) {
        if (mode == RE) rv[i] = 0.0;
        else cv[i] = cart2complex(0.0, 0.0);
      }
      if (mode == RE) rv[j] = 1.0;
      else cv[j] = cart2complex(1.0, 0.0);
      
      singular = singular || crlubksb(mat, n, iv, v, mode);
      
      for (i = 0; i < n; i++) {
        if (mode == RE) rinv[i][j] = rv[i];
        else cinv[i][j] = cv[i];
      }
    }
    result = matrix_to_data(inv, n, n, mode);
  }
  
  free_matrix(mat, n);
  free_matrix(inv, n);
  free_vector(iv);
  free_vector(v);
  if (singular) xlfail("matrix is (numerically) singular");
  return(result);
}

LVAL xssv_decomp()
{
  LVAL data, result, temp;
  Matrix mat, v;
  Vector w;
  int n, m, mode, converged;
  
  data = xlgetarg();
  xllastarg();
  
  if (! matrixp(data)) badmatrix(data);
  m = numrows(data);
  n = numcols(data);
  if (n <= 0 || m <= 0) baddata(data);
  if (m < n) xlfail("number of rows less than number of columns");
  
  xlsave1(result);
  result = mklist(4, NIL);
  temp = result;
  
  mode = data_mode(data);
  if (mode == IN) mode = RE;
  if (mode == CX) xlfail("complex SVD not available yet");
  
  mat = data_to_matrix(data, mode);
  w = (Vector) rvector(n);
  v = (Matrix) rmatrix(n, n);
  
  converged = svdcmp(mat, m, n, w, v);
  rplaca(temp, matrix_to_data(mat, m, n, mode));     temp = cdr(temp);
  rplaca(temp, vector_to_data(w, n, mode, FALSE));   temp = cdr(temp);
  rplaca(temp, matrix_to_data(v, n, n, mode));       temp = cdr(temp);
  rplaca(temp, (converged) ? s_true : NIL);

  free_matrix(mat, m);
  free_matrix(v, n);
  free_vector(w);
  xlpop();
  return(result);
}

LVAL xsqr_decomp()
{
  LVAL data, result, temp;
  Matrix mat, v;
  Vector jpvt;
  int n, m, mode, pivot;
  
  data = xlgetarg();
  pivot = (moreargs()) ? (xlgetarg() != NIL) : FALSE;
  xllastarg();
  
  if (! matrixp(data)) badmatrix(data);
  m = numrows(data);
  n = numcols(data);
  if (n <= 0 || m <= 0) baddata(data);
  if (m < n) xlfail("number of rows less than number of columns");
  
  xlsave1(result);
  result = mklist((pivot) ? 3 : 2, NIL);
  temp = result;
  
  mode = data_mode(data);
  if (mode == IN) mode = RE;
  if (mode == CX) xlfail("complex QR decomposition not available yet");
  
  mat = data_to_matrix(data, mode);
  v = (Matrix) rmatrix(n, n);
  jpvt = (Vector) ivector(n);

  qrdecomp(mat, m, n, v, jpvt, pivot);
  rplaca(temp, matrix_to_data(mat, m, n, mode)); temp = cdr(temp);
  rplaca(temp, matrix_to_data(v, n, n, mode)); temp = cdr(temp);
  if (pivot) rplaca(temp, vector_to_data(jpvt, n, IN, TRUE));

  free_matrix(mat, m);
  free_matrix(v, n);
  free_vector(jpvt);

  xlpop();
  return(result);
}

LVAL xschol_decomp()
{
  LVAL data, result;
  Matrix mat;
  int n, m, mode;
  double maxadd, maxoffl;
  
  data = xlgetarg();
  if (moreargs()) maxoffl = makedouble(xlgetarg());
  else maxoffl = 0.0;
  xllastarg();
  
  if (! matrixp(data)) badmatrix(data);
  m = numrows(data);
  n = numcols(data);
  if (n != m || n <= 0 || m <= 0) badsquarematrix(data);
    
  mode = data_mode(data);
  if (mode == IN) mode = RE;
  if (mode == CX) xlfail("complex Cholesky not available yet");
  
  mat = data_to_matrix(data, mode);

  choldecomp(mat, n, maxoffl, &maxadd);

  xlsave1(result);
  result = mklist(2, NIL);
  rplaca(result, matrix_to_data(mat, n, n, mode));
  rplaca(cdr(result), cvflonum((FLOTYPE) maxadd));

  free_matrix(mat, m);
  xlpop();
  return(result);
}

LVAL xsrcondest()
{
  LVAL data, result;
  Matrix mat;
  int n, m, mode;
  double est;
  
  data = xlgetarg();
  xllastarg();
  
  if (! matrixp(data)) badmatrix(data);
  m = numrows(data);
  n = numcols(data);
  if (n != m || n <= 0 || m <= 0) badsquarematrix(data);
    
  mode = data_mode(data);
  if (mode == IN) mode = RE;
  if (mode == CX) xlfail("complex condition estimate not available yet");
  
  mat = data_to_matrix(data, mode);

  est = rcondest(mat, n);
  result = cvflonum((FLOTYPE) est);

  free_matrix(mat, m);
  return(result);
}

LVAL xsmake_rotation()
{
  LVAL s1, s2, result;
  Vector x, y;
  Matrix rot;
  double alpha;
  int n, use_alpha = FALSE;
  
  s1 = xsgetsequence();
  s2 = xsgetsequence();
  if (moreargs()) {
    use_alpha = TRUE;
    alpha = makedouble(xlgetarg());
  }
  xllastarg();
  
  n = seqlen(s1);
  if (seqlen(s2) != n) xlfail("sequences not the same length");
  
  if (data_mode(s1) == CX || data_mode(s2) == CX)
    xlfail("complex data not supported yet");
  
  x = data_to_vector(s1, RE);
  y = data_to_vector(s2, RE);
  
  rot = (Matrix) rmatrix(n,n);
  make_rotation(n, rot, x, y, use_alpha, alpha);
  result = matrix_to_data(rot, n, n, RE);
  
  free_vector(x);
  free_vector(y);
  free_matrix(rot, n);
  return(result);
}

#define NS_DEFAULT 30

static get_smoother_data(px, py, pn, pxs, pys, pns, is_reg)
     Vector *px, *py, *pxs, *pys;
     int *pn, *pns, is_reg;
{
  LVAL s1, s2, arg, sk_xvals = xlenter(":XVALS");
  int n, ns, i, supplied;
  double xmin, xmax, *x, *xs;

  s1 = xsgetsequence();
  if (is_reg) s2 = xsgetsequence();

  if (xlgetkeyarg(sk_xvals, &arg)) {
    if (! sequencep(arg) && ! fixp(arg)) xlbadtype(arg);
  }
  else arg = NIL;

  ns = (fixp(arg)) ? getfixnum(arg) : seqlen(arg);
  if (ns < 2) ns = NS_DEFAULT;
  supplied = (sequencep(arg) && arg != NIL) ? TRUE : FALSE;

  n = seqlen(s1);
  if (n <= 0) xlfail("sequence too short");
  if (is_reg && seqlen(s2) != n) xlfail("sequences not the same length");
  if (supplied && data_mode(arg) == CX) xlfail("data must be real");
  if (data_mode(s1) == CX || (is_reg && data_mode(s2) == CX))
    xlfail("data must be real");
  
  *px = data_to_vector(s1, RE);
  *py = (is_reg) ? data_to_vector(s2, RE) : nil;
  *pxs = (supplied) ? data_to_vector(arg, RE) : (Vector) rvector(ns);
  *pys = (Vector) rvector(ns);
  *pn = n;
  *pns = ns;

  if (! supplied) {
    x = (double *) *px;
    xs = (double *) *pxs;
    for (xmax = xmin = x[0], i = 1; i < *pn; i++) {
      if (x[i] > xmax) xmax = x[i];
      if (x[i] < xmin) xmin = x[i];
    }
    for (i = 0; i < *pns; i++)
      xs[i] = xmin + (xmax - xmin) * ((double) i) / ((double) (*pns - 1));
  }
}

LVAL xsspline()
{
  LVAL result;
  Vector x, y, work, xs, ys;
  int n, ns, error;

  get_smoother_data(&x, &y, &n, &xs, &ys, &ns, TRUE);

  work = (Vector) rvector(2 * n);

  error = fit_spline(n, x, y, ns, xs, ys, work);

  xlsave1(result);
  result = mklist(2, NIL);
  rplaca(result, vector_to_data(xs, ns, RE, TRUE));
  rplaca(cdr(result), vector_to_data(ys, ns, RE, TRUE));
  xlpop();

  free_vector(x);
  free_vector(y);
  free_vector(xs);
  free_vector(ys);
  free_vector(work);

  if (error) xlfail("bad data for splines");
  return(result);
}

static LVAL kernel(is_reg)
     int is_reg;
{
  LVAL warg, targ, result;
  Vector x, y, xs, ys, wts, wds;
  int n, ns, error, ktype;
  double width;

  get_smoother_data(&x, &y, &n, &xs, &ys, &ns, is_reg);
  if (! xlgetkeyarg(sk_width, &warg)) warg = NIL;
  if (! xlgetkeyarg(sk_type, &targ)) targ = NIL;

  width = (fixp(warg) || floatp(warg)) ? makedouble(warg) : -1.0;
  wts = (Vector) nil;
  wds = (Vector) nil;
  if (targ == xlenter("U")) ktype = 'U';
  else if (targ == xlenter("T")) ktype = 'T';
  else if (targ == xlenter("G")) ktype = 'G';
  else ktype = 'B';

  error = kernel_smooth(x, y, n, width, nil, nil, xs, ys, ns, ktype);

  xlsave1(result);
  result = mklist(2, NIL);
  rplaca(result, vector_to_data(xs, ns, RE, TRUE));
  rplaca(cdr(result), vector_to_data(ys, ns, RE, TRUE));
  xlpop();

  free_vector(x);
  free_vector(y);
  free_vector(xs);
  free_vector(ys);

  if (error) xlfail("bad data for splines");
  return(result);
}

LVAL xskernel_smooth() { return(kernel(TRUE));  }
LVAL xskernel_dens()   { return(kernel(FALSE)); }

LVAL xsbase_lowess() 
{
  LVAL s1, s2, result;
  int n, nsteps, error;
  double f, delta;
  Vector x, y, ys, rw, res;

  s1 = xsgetsequence();
  s2 = xsgetsequence();
  f = makedouble(xlgetarg());
  nsteps = getfixnum(xlgafixnum());
  delta = makedouble(xlgetarg());
  xllastarg();

  n = seqlen(s1);
  if (n <= 0) xlfail("sequence too short");
  if (seqlen(s2) != n) xlfail("sequences not the same length");
  if (data_mode(s1) == CX || data_mode(s2) == CX) xlfail("data must be real");
  x = data_to_vector(s1, RE);
  y = data_to_vector(s2, RE); 
  ys = (Vector) rvector(n);
  rw = (Vector) rvector(n);
  res = (Vector) rvector(n);

  error = lowess(x, y, n, f, nsteps, delta, ys, rw, res);
  result = vector_to_data(ys, n, RE, TRUE);

  free_vector(x);
  free_vector(y);
  free_vector(ys);
  free_vector(rw);
  free_vector(res);

  if (error) xlfail("bad data for splines");
  return(result);
}

static LVAL add_contour_point(i, j, k, l, x, y, z, v, result)
	int i, j, k, l;
	RVector x, y;
	RMatrix z;
	double v;
	LVAL result;
{
  LVAL pt;
  double p, q;
  
  if ((z[i][j] <= v && v < z[k][l]) || (z[k][l] <= v && v < z[i][j])) {
    xlsave(pt);
    pt = mklist(2, NIL);
    p = (v - z[i][j]) / (z[k][l] - z[i][j]);
    q = 1.0 - p;
    rplaca(pt, cvflonum((FLOTYPE) (q * x[i] + p * x[k])));
    rplaca(cdr(pt), cvflonum((FLOTYPE) (q * y[j] + p * y[l])));
    result = cons(pt, result);
    xlpop();
  }
  return(result);
}

LVAL xssurface_contour()
{
  LVAL s1, s2, mat, result;
  RVector x, y;
  RMatrix z;
  double v;
  int i, j, n, m;
  
  s1 = xsgetsequence();
  s2 = xsgetsequence();
  mat = xsgetmatrix();
  v = makedouble(xlgetarg());
  xllastarg();
    
  n = seqlen(s1); m = seqlen(s2);
  if (n != numrows(mat) || m != numcols(mat)) xlfail("dimensions do not match");
  if (data_mode(s1) == CX || data_mode(s2) == CX || data_mode(mat) == CX)
    xlfail("data must be real");
  
  x = (RVector) data_to_vector(s1, RE);
  y = (RVector) data_to_vector(s2, RE);
  z = (RMatrix) data_to_matrix(mat, RE);
  
  xlsave1(result);
  result = NIL;
  for (i = 0; i < n - 1; i++) {
  	for (j = 0; j < m - 1; j++) {
  	  result = add_contour_point(i, j, i, j+1, x, y, z, v, result);
  	  result = add_contour_point(i, j+1, i+1, j+1, x, y, z, v, result);
  	  result = add_contour_point(i+1, j+1, i+1, j, x, y, z, v, result);
  	  result = add_contour_point(i+1, j, i, j, x, y, z, v, result);
  	}
  }
  xlpop();
  
  free_vector(x);
  free_vector(y);
  free_matrix(z, n);
  
  return(result);
}

LVAL xsfft()
{
  LVAL data, result;
  CVector x;
  RVector work;
  int n, isign, as_list;
  
  data = xsgetsequence();
  isign = (moreargs() && xlgetarg() != NIL) ? -1.0 : 1.0; 
  xllastarg();
  
  /* check and convert the data */
  n = seqlen(data);
  if (n <= 0) xlfail("not enough data");
  data_mode(data); /* checks that data are numbers */
  as_list = (listp(data)) ? TRUE : FALSE;
  x = (CVector) data_to_vector(data, CX);
  work = rvector(4 * n + 15);

  cfft(n, x, work, isign);

  /* free internal data and return result */
  result = vector_to_data(x, n, CX, as_list);
  free_vector(x);
  free_vector(work);
  
  return(result);
}
