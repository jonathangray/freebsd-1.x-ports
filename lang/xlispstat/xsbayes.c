/* xsbayes - Lisp interface to laplace approximation stuff             */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "linalg.h"

extern char *calloc(), *realloc();
extern LVAL xsgetsequence(), mklist(), integer_list_2(), newarray(), 
  arraydata(), slot_value(), newvector(), make_string(),
  compounddataseq(), xsgetmatrix();
extern double makedouble();

extern LVAL s_true;

/************************************************************************/
/**                                                                    **/
/**                      Definitions and Globals                       **/
/**                                                                    **/
/************************************************************************/

#define MAXALLOC 20

static char  *mem[MAXALLOC], memcount;
static LVAL arg;

typedef struct {
  int n, m, k, itnlimit, backtrack, verbose, vals_suppl, exptilt;
  int count, termcode;
} MaxIPars;

typedef struct {
  double typf, h, gradtol, steptol, maxstep, dflt, tilt, newtilt, hessadd;
} MaxDPars;

typedef struct {
  MaxIPars max;
  int full, covar;
} MomIPars;

typedef struct {
  MaxDPars max;
  double mgfdel;
} MomDPars;

/************************************************************************/
/**                                                                    **/
/**                 Fake Replacements for S Interface                  **/
/**                                                                    **/
/************************************************************************/

static meminit() 
{
  static inited = FALSE;
  int i;

  if (! inited) {
    for (i = 0; i < MAXALLOC; i++) mem[i] = nil;
    inited = TRUE;
  }

  memcount = 0;
}

static makespace(pptr, size)
     char **pptr;
     int size;
{
  if (size <= 0) return;
  if (*pptr == nil) *pptr = calloc(size, 1);
  else *pptr = realloc(*pptr, size);
  if (size > 0 && *pptr == nil) xlfail("memory allocation failed");
}

#ifdef SBAYES
char *S_alloc(n, m)
     int n, m;
{
  char *result;
  if (memcount >= MAXALLOC) result = nil;
  else {
    makespace(mem + memcount, n * m);
    result = mem[memcount];
    memcount++;
  }
  return(result);
}
#endif /* SBAYES */

call_S(fun, narg, args, mode, length, names, nvals, values)
     char *fun, **args, **mode, **names, **values;
     long narg, nvals, *length;
{
  int n = length[0], derivs;
  static double *fval = nil, *grad = nil, *hess = nil;

  makespace(&fval, 1 * sizeof(double));
  makespace(&grad, n * sizeof(double));
  makespace(&hess, n * n * sizeof(double));

  callLminfun(fun, n, args[0], fval, grad, hess, &derivs);
  
  values[0] = (char *) fval;
  values[1] = (derivs > 0) ? (char *) grad : nil;
  values[2] = (derivs > 1) ? (char *) hess : nil;
}

Recover(s, w)
     char *s, *w;
{
  xlfail(s);
}

/************************************************************************/
/**                                                                    **/
/**                   Callback and Copying Function                    **/
/**                                                                    **/
/************************************************************************/

static callLminfun(fun, n, x, fval, grad, hess, derivs)
     LVAL fun;
     int n, *derivs;
     RVector x, grad, hess;
     double *fval;
{
  LVAL result, seq;
  
  /* arg does not realy have to containt the function; this is a carryover */
  double2list(n, x, car(cdr(arg)));
  rplaca(arg, fun);
  result = xlapply(pushargs(car(arg), cdr(arg)));

  if (realp(result)) {
    *fval = makedouble(result);
    *derivs = 0;
  }
  else if (consp(result)) {
    *fval = makedouble(car(result));
    *derivs = 0;
    if (consp(cdr(result))) {
      seq = compounddataseq(car(cdr(result)));
      if (seqlen(seq) != n) xlerror("bad gradient", car(cdr(result)));
      seq2double(n, seq, grad);
      *derivs = 1;
      if (consp(cdr(cdr(result)))) {
	seq = compounddataseq(car(cdr(cdr(result))));
	if (seqlen(seq) != n * n)
	  xlerror("bad hessian", car(cdr(cdr(result))));
	seq2double(n * n, seq, hess);
	*derivs = 2;
      }
    }
  }
  else xlerror("bad function result", result);
}	   
  
static LVAL makecallarg(n)
     int n;
{
  LVAL carg;

  xlsave1(carg);
  carg = mklist(n, NIL);
  carg = consa(carg);
  carg = cons(NIL, carg);
  xlpop();
  return(carg);
}

static seq2pchar(n, x, p)
     int n;
     LVAL x;
     char **p;
{
  int i;

  for (i = 0; i < n; i++) 
    p[i] = (char *) getnextelement(&x, i);
}

static seq2double(n, x, dx)
     int n;
     LVAL x;
     double *dx;
{
  int i;

  for (i = 0; i < n; i++) 
    dx[i] = makedouble(getnextelement(&x, i));
}

static double2list(n, dx, x)
     int n;
     double *dx;
     LVAL x;
{
  int i;

  for (i = 0; i < n && consp(x) ; i++, x = cdr(x))
    rplaca(x, cvflonum((FLOTYPE) dx[i]));
}

/************************************************************************/
/**                                                                    **/
/**                      Numerical Derivatives                         **/
/**                                                                    **/
/************************************************************************/

static LVAL numderiv(order)
     int order;
{
  LVAL f, x, scale, result, data;
  double h, fval;
  static double *dx = nil, *grad = nil, *hess = nil, *typx = nil;
  int n, i, all;

  f = xlgetarg();
  x = xsgetsequence();
  scale = (moreargs()) ? xsgetsequence() : NIL;
  h = (moreargs()) ? makedouble(xlgetarg()) : -1.0;
  all = (moreargs() && xlgetarg() != NIL) ? TRUE : FALSE;
  
  n = seqlen(x);
  makespace(&dx, n * sizeof(double));
  makespace(&grad, n * sizeof(double));
  if (order > 1) makespace(&hess, n * n * sizeof(double));
  makespace(&typx, n * sizeof(double));

  seq2double(n, x, dx);
  if (scale != NIL) seq2double(n, scale, typx);
  else for (i = 0; i < n; i++) typx[i] = 1.0;

  xlstkcheck(2);
  xlsave(arg);
  xlsave(result);
  arg = makecallarg(n);

  evalfront(&f, &n, dx, &fval, grad, (order > 1) ? hess : nil, &h, typx);
  
  if (order == 1) {
    result = mklist(n, NIL);
    double2list(n, grad, result);
  }
  else {
    result = integer_list_2(n, n);
    result = newarray(result, NIL, NIL);
    data = arraydata(result);
    for (i = 0; i < n * n; i++)
      setelement(data, i, cvflonum((FLOTYPE) hess[i]));
	if (all) {
	  result = consa(result);
	  result = cons(mklist(n, NIL), result);
	  double2list(n, grad, car(result));
      result = cons(cvflonum((FLOTYPE) fval), result);
	}
  }
  xlpopn(2);

  return(result);
}

LVAL xsnumgrad() { return(numderiv(1)); }
LVAL xsnumhess() { return(numderiv(2)); }

/************************************************************************/
/**                                                                    **/
/**                      Maximization Interface                        **/
/**                                                                    **/
/************************************************************************/

#define INTLEN 12
#define F_POS 0
#define G_POS 1
#define C_POS 2
#define X_POS 3
#define SCALE_POS 4
#define FVALS_POS 5
#define CVALS_POS 6
#define CTARG_POS 7
#define IPARS_POS 8
#define DPARS_POS 9
#define TSCAL_POS 10
#define MULT_POS 11

#define getffun(i) getelement(i, F_POS)
#define getgfuns(i) getelement(i, G_POS)
#define getcfuns(i) getelement(i, C_POS)
#define getx(i) getelement(i, X_POS)
#define getscale(i) getelement(i, SCALE_POS)
#define getfvals(i) getelement(i, FVALS_POS)
#define getcvals(i) getelement(i, CVALS_POS)
#define getctarget(i) getelement(i, CTARG_POS)
#define getipars(i) getelement(i, IPARS_POS)
#define getdpars(i) getelement(i, DPARS_POS)
#define gettscale(i) getelement(i, TSCAL_POS)
#define getmults(i) getelement(i, MULT_POS)
#define getderivstep(i) getflonum(getelement(getdpars(i), 1))

#define setx(i, v) setelement(i, X_POS, v)
#define setscale(i, v) setelement(i, SCALE_POS, v)
#define setmults(i, v) setelement(i, MULT_POS, v)
#define setfvals(i, v) setelement(i, FVALS_POS, v)
#define setcvals(i, v) setelement(i, CVALS_POS, v)
#define setipars(i, v) setelement(i, IPARS_POS, v)
#define setdpars(i, v) setelement(i, DPARS_POS, v)
#define setderivstep(i, v) setelement(getdpars(i), 1, cvflonum((FLOTYPE) (v)))

static LVAL getinternals(minfo)
     LVAL minfo;
{
  return(slot_value(minfo, xlenter("INTERNALS")));
}

static setinternals(minfo, internals)
     LVAL minfo, internals;
{
  set_slot_value(minfo, xlenter("INTERNALS"), internals);
}

static LVAL newipars(ip)
     MaxIPars ip;
{
  LVAL result;

  xlsave1(result);
  result = newvector(10);
  setelement(result, 0, cvfixnum((FIXTYPE) ip.n));
  setelement(result, 1, cvfixnum((FIXTYPE) ip.m));
  setelement(result, 2, cvfixnum((FIXTYPE) ip.k));
  setelement(result, 3, cvfixnum((FIXTYPE) ip.itnlimit));
  setelement(result, 4, cvfixnum((FIXTYPE) ip.backtrack));
  setelement(result, 5, cvfixnum((FIXTYPE) ip.verbose));
  setelement(result, 6, cvfixnum((FIXTYPE) ip.vals_suppl));
  setelement(result, 7, cvfixnum((FIXTYPE) ip.exptilt));
  setelement(result, 8, cvfixnum((FIXTYPE) ip.count));
  setelement(result, 9, cvfixnum((FIXTYPE) ip.termcode));
  xlpop();
  return(result);
}

static LVAL newdpars(dp)
     MaxDPars dp;
{
  LVAL result;

  xlsave1(result);
  result = newvector(9);
  setelement(result, 0, cvflonum((FLOTYPE) dp.typf));
  setelement(result, 1, cvflonum((FLOTYPE) dp.h));
  setelement(result, 2, cvflonum((FLOTYPE) dp.gradtol));
  setelement(result, 3, cvflonum((FLOTYPE) dp.steptol));
  setelement(result, 4, cvflonum((FLOTYPE) dp.maxstep));
  setelement(result, 5, cvflonum((FLOTYPE) dp.dflt));
  setelement(result, 6, cvflonum((FLOTYPE) dp.tilt));
  setelement(result, 7, cvflonum((FLOTYPE) dp.newtilt));
  setelement(result, 8, cvflonum((FLOTYPE) dp.hessadd));
  xlpop();
  return(result);
}

static MaxIPars getMaxIPars(internals)
     LVAL internals;
{
  LVAL ipars = getipars(internals);
  MaxIPars ip;

  ip.n = getfixnum(getelement(ipars, 0));
  ip.m = getfixnum(getelement(ipars, 1));
  ip.k = getfixnum(getelement(ipars, 2));
  ip.itnlimit = getfixnum(getelement(ipars, 3));
  ip.backtrack = getfixnum(getelement(ipars, 4));
  ip.verbose = getfixnum(getelement(ipars, 5));
  ip.vals_suppl = getfixnum(getelement(ipars, 6));
  ip.exptilt = getfixnum(getelement(ipars, 7));
  ip.count = getfixnum(getelement(ipars, 8));
  ip.termcode = getfixnum(getelement(ipars, 9));

  return(ip);
}

static MaxDPars getMaxDPars(internals)
     LVAL internals;
{
  LVAL dpars = getdpars(internals);
  MaxDPars dp;

  dp.typf = getflonum(getelement(dpars, 0));
  dp.h = getflonum(getelement(dpars, 1));
  dp.gradtol = getflonum(getelement(dpars, 2));
  dp.steptol = getflonum(getelement(dpars, 3));
  dp.maxstep = getflonum(getelement(dpars, 4));
  dp.dflt = getflonum(getelement(dpars, 5));
  dp.tilt = getflonum(getelement(dpars, 6));
  dp.newtilt = getflonum(getelement(dpars, 7));
  dp.hessadd = getflonum(getelement(dpars, 8));

  return(dp);
}

static LVAL newinternals(f, x, scale, h)
     LVAL f, x, scale;
     double h;
{
  LVAL result;
  int n, i;
  MaxIPars ip;
  MaxDPars dp;

  n = seqlen(x);
  ip.n = n; ip.k = 0; ip.m = 0; ip.itnlimit = -1; ip.backtrack = TRUE;
  ip.verbose = 0; ip.vals_suppl = FALSE; ip.exptilt = TRUE;
  ip.count = 0; ip.termcode = 0;

  dp.typf = 1.0; dp.h = h; dp.gradtol = -1.0; dp.steptol = -1.0;
  dp.maxstep = -1.0; dp.dflt = 0.0; dp.tilt = 0.0; dp.newtilt = 0.0;
  dp.hessadd = 0.0;

  xlsave1(result);
  result = newvector(INTLEN);
  setelement(result, F_POS, f);
  setelement(result, X_POS, x);
  if (scale != NIL) setelement(result, SCALE_POS, scale);
  else {
    scale = newvector(n);
    setelement(result, SCALE_POS, scale);
    for (i = 0; i < n; i++) setelement(scale, i, cvflonum((FLOTYPE) 1.0));
  }
  setelement(result, IPARS_POS, newipars(ip));
  setelement(result, DPARS_POS, newdpars(dp));
  xlpop();
  
  return(result);
}

LVAL xsminfo_isnew()
{
  LVAL myinfo, f, x, scale, step;
  double h;

  myinfo = xlgaobject();
  f = xlgetarg();
  x = xsgetsequence();
  if (! xlgetkeyarg(xlenter(":SCALE"), &scale)) scale = NIL;
  if (xlgetkeyarg(xlenter(":DERIVSTEP"), &step)) h = makedouble(step);
  else h = -1.0;

  setinternals(myinfo, newinternals(f, x, scale, h));
  return(NIL);
}

LVAL xsminfo_maximize()
{
  LVAL minfo, internals, f, g, c, x, scale, verbose, tiltscale, result, mults; 
  MaxIPars ip;
  MaxDPars dp;
  int n, m, k;
  static double *dx = nil, *typx = nil, *fvals = nil, *tscale = nil;
  static double *cvals = nil, *ctarget = nil;
  static char *gg = nil, *cc = nil;
  char *msg;

  minfo = xlgaobject();
  verbose = (moreargs()) ? xlgafixnum() : NIL;

  internals = getinternals(minfo);
  f = getffun(internals);
  g = getgfuns(internals);
  c = getcfuns(internals);
  x = getx(internals);
  scale = getscale(internals);
  ip = getMaxIPars(internals);
  dp = getMaxDPars(internals);
  tiltscale = gettscale(internals);
  mults = getmults(internals);

  m = ip.m;
  k = ip.k;
  if (verbose != NIL) ip.verbose = getfixnum(verbose);
  n = seqlen(x);
  ip.n = n;
  makespace(&gg, m * sizeof(char *));
  makespace(&cc, k * sizeof(char *));
  makespace(&dx, (n + k) * sizeof(double));
  makespace(&typx, n * sizeof(double));
  makespace(&fvals, (1 + n + n * n) * sizeof(double));
  makespace(&tscale, m * sizeof(double));
  makespace(&cvals, (k + k * n) * sizeof(double));
  makespace(&ctarget, k * sizeof(double));

  seq2pchar(m, g, gg);
  seq2pchar(k, c, cc);
  seq2double(n, x, dx);
  seq2double(n, scale, typx);
  if (ip.vals_suppl) {
    seq2double(1 + n + n * n, getfvals(internals), fvals);
    seq2double(k + k * n, getcvals(internals), cvals);
  }
  seq2double(m, tiltscale, tscale);
  seq2double(k, getctarget(internals), ctarget);
  seq2double(k, mults, dx + n);

  xlsave1(arg);
  arg = makecallarg(n);

  meminit();
  maxfront(&f, gg, cc, dx, typx, fvals, nil, cvals, ctarget, &ip, &dp, tscale, &msg);

  result = mklist(n, NIL);
  setx(internals, result);
  double2list(n, dx, result);
  result = mklist(1 + n + n * n, NIL);
  setfvals(internals, result);
  double2list(1 + n + n * n, fvals, result);
  if (k > 0) {
    result = mklist(k + k * n, NIL);
    setcvals(internals, result);
    double2list(k + k * n, cvals, result);
    result = mklist(k, NIL);
    setmults(internals, result);
    double2list(k, dx + n, result);
  }
  setipars(internals, newipars(ip));
  setdpars(internals, newdpars(dp));

  xlpop();
  
  return(make_string(msg));
}

/************************************************************************/
/**                                                                    **/
/**                        Laplace Interface                           **/
/**                                                                    **/
/************************************************************************/

LVAL xsminfo_loglap()
{
  LVAL minfo, internals;
  MaxIPars ip;
  MaxDPars dp;
  int n, k, detonly;
  double val;
  static double *fvals = nil, *cvals = nil;
  
  minfo = xlgaobject();
  detonly = (moreargs()) ? (xlgetarg() != NIL) : FALSE;

  internals = getinternals(minfo);
  ip = getMaxIPars(internals);
  dp = getMaxDPars(internals);

  n = ip.n;
  k = ip.k;

  makespace(&fvals, (1 + n + n * n) * sizeof(double));
  makespace(&cvals, (k + k * n) * sizeof(double));
  seq2double(1 + n + n * n, getfvals(internals), fvals);
  seq2double(k + k * n, getcvals(internals), cvals);

  loglapdet(fvals, cvals, &ip, &dp, &val, &detonly);

  return(cvflonum((FLOTYPE) val));
}

/************************************************************************/
/**                                                                    **/
/**            Scaled Evaluation and Numerical Derivatives             **/
/**                                                                    **/
/************************************************************************/

#ifdef DODO
/**** function objects; exact derivatives; NIL values ****/

static struct scaled_Lfun_info {
  LVAL arglist, value;
  RVector scaling, z;
  int dim, in_range, num_derivs, cached;
  double cached_value;
} Lfun_info;

static LVAL scaled_eval(order)
	int order;
{
  struct scaled_Lfun_info old_info;
  LVAL Lfun, Lx, Lscaling, Lvalue, space, hspace, Ldata;
  RVector x, fsum, grad;
  RMatrix hess;
  double value, h;
  int k, i, j, all;
  
  old_info = Lfun_info;
  
  Lfun = xlgetarg();
  Lx = xsgetsequence();
  Lscaling = xsgetsequence();
  if (order > 0) h = makedouble(xlgetarg());
  if (order == 2) all = (moreargs() && xlgetarg() != NIL) ? TRUE : FALSE;
  xllastarg();
  
  xlstkcheck(5);
  xlsave(space);
  xlsave(hspace);
  xlsave(Lfun_info.arglist);
  xlsave(Lfun_info.value);
  xlsave(Lvalue);
  
  k = Lfun_info.dim = seqlen(Lx);
  space = newadata(sizeof(double), 2 + k * (2 * k + 5), FALSE);
  Lfun_info.scaling = (RVector) getadaddr(space);
  Lfun_info.z = Lfun_info.scaling + 2 + k * (k + 1);
  x = Lfun_info.scaling + 2 + k * (k + 2);
  fsum = Lfun_info.scaling + 2 + k * (k + 3);
  grad = Lfun_info.scaling + 2 + k * (k + 4);
  Lfun_info.arglist = mklist(2, NIL);
  rplaca(Lfun_info.arglist, Lfun);
  rplaca(cdr(Lfun_info.arglist), mklist(k, NIL));
  Lfun_info.in_range = TRUE;
  if (order == 2) {
    hspace = newadata(sizeof(double *), k, FALSE);
    hess = (RMatrix) getadaddr(hspace);
	for (i = 0; i < k; i++)
	  hess[i] = Lfun_info.scaling + 2 + k * (k + 5 + i);
  }
  else hess = nil;
  Lfun_info.cached = FALSE;
  Lfun_info.num_derivs = 0;
  
  seq2double(2 + k * (k + 1), Lscaling, Lfun_info.scaling);
  seq2double(k, Lx, x);

  call_scaled_Lfun(x, &value, grad, hess);
  switch (order) {
  case 0:
    if (Lfun_info.in_range) Lvalue = cvflonum((FLOTYPE) value);
    else Lvalue = Lfun_info.value;
    break;
  case 1:
    if (Lfun_info.num_derivs < 1)
	  numergrad(k, x, grad, fsum, call_scaled_Lfun, h, nil);
	if (Lfun_info.in_range) {
	  Lvalue = mklist(k, NIL);
	  double2list(k, grad, Lvalue);
	}
	else Lvalue = NIL;
    break;
  case 2:
    /* currently uses second differences even for analytic gradient */
    if (Lfun_info.num_derivs < 2) {
      numergrad(k, x, grad, fsum, call_scaled_Lfun, h, nil);
	  /* Lfun_info.cached_value = value;
	  Lfun_info.cached = TRUE;*/
      numerhess(k, x, hess, value, fsum, call_scaled_Lfun, h, nil);
	}
	if (Lfun_info.in_range) {
      Lvalue = integer_list_2(k, k);
      Lvalue = newarray(Lvalue, NIL, NIL);
      Ldata = arraydata(Lvalue);
      for (i = 0; i < k; i++)
        for (j = 0; j < k; j++)
          setelement(Ldata, i * k + j, cvflonum((FLOTYPE) hess[i][j]));
	  if (all) {
	    Lvalue = consa(Lvalue);
		Lvalue = cons(mklist(k, NIL), Lvalue);
		double2list(k, grad, car(Lvalue));
		Lvalue = cons(cvflonum((FLOTYPE) value), Lvalue);
	  }
	}
	else Lvalue = NIL;
    break;
  }
  
  xlpopn(5);
  Lfun_info = old_info;
  return(Lvalue);
}

static call_scaled_Lfun(x, value, grad, hess)
	RVector x, grad, hess;
	double *value;
{
  LVAL temp, Lvalue, Lgrad, Lhess;
  double center, scale;
  RVector mean, sigma, z;
  int i, j, k;
  
  /* cheat to avoid recalculating function value in Hessian evaluation */
  if (Lfun_info.cached && Lfun_info.num_derivs == 0) {
    Lfun_info.cached = FALSE;
	*value = Lfun_info.cached_value;
	return;
  }
  
  k = Lfun_info.dim;
  center = Lfun_info.scaling[0];
  scale = Lfun_info.scaling[1];
  if (scale == 0.0) xlfail("division by zero");
  mean = Lfun_info.scaling + 2;
  sigma = Lfun_info.scaling + 2 + k;
  z = Lfun_info.z;
  
  for (i = 0; i < k; i++) {
	z[i] = mean[i];
    for (j = 0; j <= i; j++)
	  z[i] += sigma[i * k + j] * x[j];
  }
  
  double2list(k, z, car(cdr(Lfun_info.arglist)));

  temp = xlapply(pushargs(car(Lfun_info.arglist), cdr(Lfun_info.arglist)));
  
  Lfun_info.num_derivs = (consp(temp)) ? seqlen(temp) - 1 : 0;
  
  Lvalue = (consp(temp)) ? car(Lvalue) : temp;
  Lgrad = (consp(temp) && consp(cdr(temp))) ? car(cdr(temp)) : NIL;
  Lhess = (consp(temp) && consp(cdr(temp)) && consp(cdr(cdr(temp)))) 
        ? car(cdr(cdr(temp))) : NIL;
  
  if (fixp(Lvalue) || floatp(Lvalue)) {
  	*value = (makedouble(Lvalue) - center) / scale;
	if (grad != nil && Lgrad != NIL) {
	  seq2double(k, Lgrad, grad);
	  for (i = 0; i < k; i++) {
	    z[i] = 0.0;
	    for (j = i; j < k; j++) z[i] += sigma[j * k + i] * grad[j];
	    grad[i] = z[i] / scale;
	  }
	}
	if (hess != nil && Lhess != NIL) {
	  if (! matrixp(Lhess)) xlerror("not an a matrix", Lhess);
	  seq2double(k * k, arraydata(Lhess), hess);
	  /* rescale */
	}
  }
  else {
    *value = 0.0;
	Lfun_info.in_range = FALSE;
    Lfun_info.value = Lvalue;
  }
}

LVAL xsscaled_c2_eval() { return(scaled_eval(0)); }
LVAL xsscaled_c2_grad() { return(scaled_eval(1)); }
LVAL xsscaled_c2_hess() { return(scaled_eval(2)); }
#endif DODO

LVAL xsaxpy()
{
  LVAL result, next, tx, a, x, y;
  int i, j, n, start, end, lower;
  double val;
  
  a = arraydata(xsgetmatrix());
  x = xsgetsequence();
  y = xsgetsequence();
  lower = (moreargs() && xlgetarg() != NIL) ? TRUE : FALSE;
  
  n = seqlen(x);
  if (n * n != seqlen(a) || n != seqlen(y)) xlfail("dimensions do not match");
  
  xlsave(result);
  result = mklist(n, NIL);
  for (i = 0, start = 0, next = result; i < n; i++, start += n, next = cdr(next)) {
  	val = makedouble(getnextelement(&y, i));
	end = (lower) ? i : n - 1;
    for (j = 0, tx = x; j <= end; j++) {
	  val += makedouble(getnextelement(&tx, j)) 
           * makedouble(getelement(a, start + j));
    }
	rplaca(next, cvflonum((FLOTYPE) val));
  }
  xlpop();
  return(result);
}
