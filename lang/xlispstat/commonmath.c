/* commonmath - xlisp math functions modified and augmented to         */
/* correspond more closely to Common Lisp standard                     */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "xmath.h"
#include "statfloat.h"

/*
 * The stuff enclosed in NEWFIXARITH defines an attempt to provide
 * extended precision by having integer arithmetic overflow into
 * floating point when necessary. It is not true extended precision,
 * since subsequent results that fit in the fixed point range are not
 * demoted. The system assumes a FIXTYPE (C long) is 32 bits, and
 * two's complement integer arithmetic. Systems with other sizes for 
 * FIXTYPE may be handled by changing the definitions of
 * MOST_NEGATIVE_FIX, MOST_POSITIVE_FIX and ROOT_BIG_FIX (which should
 * be such that MOST_NEGATIVE_FIX = (-2 * ROOT_BIG_FIX * ROOT_BIG_FIX)
 * is representable as a FIXTYPE). Changes can be made through the
 * UCFLAGS in the Makefile or in statfloat.h.
 *
 * The system for handling addition and subtraction is adapted from
 * kcl. It should slow these operations a little but not too much.
 * Division only requires a check for -MOST_NEGATIVE_FIX / -1 (I think).
 * The system used for multiplication is a hack. Some pretests are used
 * to determine if the result will be representable as a FIXTYPE. If
 * these tests fail, the result is computed in floating point and
 * checked to see if it falls between MOST_NEGATIVE_FIX and
 * MOST_POSITIVE_FIX compared. If so, then the result is an integer.
 * Otherwise, the arguments are promoted to floating point and the
 * result is re-computed in floating point. (If I got things right, the
 * pretests should only fail if the result needs to be promoted or if
 * the result would be equal to MOST_NEGATIVE_FIX.
 */

#ifndef OLDFIXARITH
#define NEWFIXARITH
#endif

#ifdef NEWFIXARITH
#ifndef MOST_NEGATIVE_FIX
#define MOST_NEGATIVE_FIX -2147483648
#define MOST_POSITIVE_FIX  2147483647
#endif
#ifndef ROOT_BIG_FIX
#define ROOT_BIG_FIX 32768
#endif
#endif

/* external variables */
extern LVAL true;

/* external functions */
extern double uni();

#define IN 0
#define FL 1
#define CI 2
#define CF 3

typedef struct {
  int mode;
  FIXTYPE val, crval, cival;
  FLOTYPE fval, cfrval, cfival;
} Number;

/* forward declarations */
FORWARD LVAL unary();
FORWARD LVAL binary();
FORWARD LVAL binary1();
FORWARD LVAL logbinary();
FORWARD LVAL predicate();
FORWARD LVAL compare();
FORWARD LVAL ccompare();
FORWARD FLOTYPE logarithm();
FORWARD FLOTYPE arcsin();
FORWARD FLOTYPE arccos();

/* binary functions */
LVAL xadd()    { return (binary('+')); } /* + */
LVAL xsub()    { return (binary('-')); } /* - */
LVAL xmul()    { return (binary('*')); } /* * */
LVAL xdiv()    { return (binary('/')); } /* / */
LVAL xmin()    { return (binary('m')); } /* min */
LVAL xmax()    { return (binary('M')); } /* max */

LVAL xlogand() { return (logbinary('&')); } /* logand */
LVAL xlogior() { return (logbinary('|')); } /* logior */
LVAL xlogxor() { return (logbinary('^')); } /* logxor */

static LVAL readnumber(num)
	Number *num;
{
  LVAL arg = xlgetarg(), real, imag;
  
  if (fixp(arg)) {
    num->mode = IN;
    num->val = getfixnum(arg);
  }
  else if (floatp(arg)) {
    num->mode = FL;
    num->fval = getflonum(arg);
  }
  else if (complexp(arg)) {
    real = realpart(arg);
    imag = imagpart(arg);
    if (fixp(real)) {
      num->mode = CI;
      num->crval = getfixnum(real);
      num->cival = getfixnum(imag);
    }
    else {
      num->mode = CF;
      num->cfrval = makedouble(real);
      num->cfival = makedouble(imag);
    }
  }
  else xlerror("not a number", arg);
  return(arg);
}

static setmode(x, mode)
	Number *x;
	int mode;
{
  switch (mode) {
  case FL:
    if (x->mode != IN) return;
    x->mode = mode;
    x->fval = x->val;
    break;
  case CI:
    if (x->mode != IN) return;
    x->mode = mode;
    x->crval = x->val;
    x->cival = 0;
    break;
  case CF:
    switch (x->mode) {
    case IN:
      x->mode = mode;
      x->cfrval = x->val;
      x->cfival = 0.0;
      break;
    case FL:
      x->mode = mode;
      x->cfrval = x->fval;
      x->cfival = 0.0;
      break;
    case CI:
      x->mode = mode;
      x->cfrval = x->crval;
      x->cfival = x->cival;
      break;
    }
    break;
  }
}

static matchmodes(x, y)
	Number *x, *y;
{
  int mode = x->mode;
  switch (mode) {
  case IN: mode = y->mode; break;
  case FL: if (y->mode == CI || y->mode == CF) mode = CF; break;
  case CI: if (y->mode == FL || y->mode == CF) mode = CF; break;
  case CF: break;
  }
  if (x->mode != mode) setmode(x, mode);
  if (y->mode != mode) setmode(y, mode);
}

static LVAL lispnumber(x)
	Number *x;
{
  switch (x->mode) {
  case IN: return(cvfixnum(x->val));
  case FL: return(cvflonum(x->fval));
  case CI: return(newicomplex(x->crval, x->cival));
  case CF: return(newdcomplex(x->cfrval, x->cfival));
  }
}

static badarg(arg)
	LVAL arg;
{
  xlerror("bad argument type", arg);
}

#ifdef NEWFIXARITH
#define goodsum(a,b,c) (((a)==0)||(((a)>0)&&(((b)<0)||((c)>0)))||(((a)<0)&&(((b)>0)||((c)<0))))
#define gooddif(a,b,c) (((b)!=MOST_NEGATIVE_FIX)&&((a)==0)||(((a)>0)&&(((b)>0)||((c)>0)))||(((a)<0)&&(((b)<0)||((c)<0))))
#define smallfac(x) (((-ROOT_BIG_FIX<(x))&&((x)<ROOT_BIG_FIX)))
#endif

#ifdef NEWFIXARITH
static int_add(x, y, good)
	FIXTYPE *x, *y;
	int *good;
{
  FIXTYPE temp;
  
  temp = *x + *y;
  if (goodsum(*x, *y, temp)) {
    *good = TRUE;
	*x = temp;
  }
  else *good = FALSE;
}

static int_sub(x, y, good)
	FIXTYPE *x, *y;
	int *good;
{
  FIXTYPE temp;
  
  temp = *x - *y;
  if (gooddif(*x, *y, temp)) {
    *good = TRUE;
	*x = temp;
  }
  else *good = FALSE;
}

static int_mul(x, y, good)
	FIXTYPE *x, *y;
	int *good;
{    
  FIXTYPE temp, xt, yt, c, d;
  double ftemp;
  int small_x, small_y, sgn;
  
  small_x = smallfac(*x);
  small_y = smallfac(*y);
  
  /* handle cases where result is "obviously" going to be OK */
  if (small_x && small_y) {
    *x *= *y;
    *good = TRUE;
    return;
  }
  else if (small_x) {
    temp = *y / (2 * ROOT_BIG_FIX);
    temp = *x * temp;
    if (smallfac(temp)) {
      *x *= *y;
      *good = TRUE;
      return;
    }
  }
  else if (small_y) {
    temp = *x / (2 * ROOT_BIG_FIX);
    temp = *y * temp;
    if (smallfac(temp)) {
      *x *= *y;
      *good = TRUE;
      return;
    }
  }
  else {
    sgn = ((*x < 0 && *y < 0) || (*x > 0 && *y > 0)) ? 1 : -1;
    xt = (*x > 0) ? *x : -(*x);
    yt = (*y > 0) ? *y : -(*y);
    if (xt < 2 * ROOT_BIG_FIX && yt < 2 * ROOT_BIG_FIX) {
      xt -= ROOT_BIG_FIX;
      yt -= ROOT_BIG_FIX;
      c = ROOT_BIG_FIX * (xt + yt);
      d = xt * yt;
      if (d < ((ROOT_BIG_FIX * ROOT_BIG_FIX) - c)) {
        temp = (ROOT_BIG_FIX * ROOT_BIG_FIX) + c + d;
	*x = (sgn == 1) ? temp : -temp;
	*good = TRUE;
	return;
      }
    }
  }
  
  /* Drop through to here if none of the cases is matched. */
  /* Should only happen on overflow and when result is     */
  /* MOST_NEGATIVE_FIX                                     */
  ftemp = ((double) *x) * ((double) *y);
  if (MOST_NEGATIVE_FIX <= ftemp && ftemp <= MOST_POSITIVE_FIX) {
    *x = ftemp;
    *good = TRUE;
  }
  else *good = FALSE;
}
#endif /* NEWFIXARITH */

LVAL binary(which)
	int which;
{
  LVAL larg;
  Number val, arg;
  FIXTYPE rtemp, itemp;
  FLOTYPE frtemp, fitemp, magn;
#ifdef NEWFIXARITH
  FIXTYPE temp;
  int good;
#endif /* NEWFIXARITH */
  
  if (xlargc == 1 && (which == '-' || which == '/')) {
    val.mode = IN;
    switch (which) {
    case '-': val.val = 0; break;
    case '/': val.val = 1; break;
    }
  }
  else larg = readnumber(&val);
  while (moreargs()) {
    larg = readnumber(&arg);
    matchmodes(&val, &arg);
    switch (which) {
    case '+':
      switch (val.mode) {
#ifdef NEWFIXARITH
      case IN:
        int_add(&val.val, &arg.val, &good);
        if (good) break;
        /* else */
        setmode(&val, FL);
        setmode(&arg, FL);
        /* drop through */
#else
      case IN: val.val   += arg.val;  break;
#endif /* NEWFIXARITH */
      case FL: val.fval  += arg.fval; break;
#ifdef NEWFIXARITH
      case CI:
	    rtemp = val.crval; itemp = val.cival;
	    int_add(&rtemp, &arg.crval, &good);
	    if (good) {
	      int_add(&itemp, &arg.cival, &good);
		  if (good) {
	        val.crval = rtemp;
	        val.cival = itemp;
	        break;
		  }
	    }
	    /* else */
	    setmode(&val, CF);
	    setmode(&arg, CF);
	    /* drop through */
#else
      case CI: val.crval += arg.crval;   val.cival += arg.cival;   break;
#endif /* NEWFIXARITH */
      case CF: val.cfrval += arg.cfrval; val.cfival += arg.cfival; break;
      }
      break;
    case '-':
      switch (val.mode) {
#ifdef NEWFIXARITH
      case IN:
	    int_sub(&val.val, &arg.val, &good);
        if (good) break;
        /* else */
        setmode(&val, FL);
        setmode(&arg, FL);
        /* drop through */
#else
      case IN: val.val   -= arg.val;  break;
#endif /* NEWFIXARITH */
      case FL: val.fval  -= arg.fval; break;
#ifdef NEWFIXARITH
      case CI:
        rtemp = val.crval; itemp = val.cival;
        int_sub(&rtemp, &arg.crval, &good);
        if (good) {
          int_sub(&itemp, &arg.cival, &good);
          if (good) {
            val.crval = rtemp;
            val.cival = itemp;
            break;
          }
        }
        /* else */
        setmode(&val, CF);
        setmode(&arg, CF);
        /* drop through */
#else
      case CI: val.crval -= arg.crval;   val.cival -= arg.cival;   break;
#endif /* NEWFIXARITH */
      case CF: val.cfrval -= arg.cfrval; val.cfival -= arg.cfival; break;
      }
      break;
    case '*':
      switch (val.mode) {
#ifdef NEWFIXARITH
      case IN:
	    int_mul(&val.val, &arg.val, &good);
        if (good) break;
        /* else */
        setmode(&val, FL);
        setmode(&arg, FL);
        /* drop through */
#else
      case IN: val.val   *= arg.val;  break;
#endif /* NEWFIXARITH */
      case FL: val.fval  *= arg.fval; break;
      case CI:
#ifdef NEWFIXARITH
        temp = val.crval;
        int_mul(&temp, &arg.crval, &good);
        if (good) {
          rtemp = temp;
          temp = val.cival;
          int_mul(&temp, &arg.cival, &good);
          if (good) {
            int_sub(&rtemp, &temp, &good);
            if (good) {
              temp = val.cival;
              int_mul(&temp, &arg.crval, &good);
              if (good) {
                itemp = temp;
                temp = val.crval;
                int_mul(&temp, &arg.cival, &good);
                if (good) {
                  int_add(&itemp, &temp, &good);
                  if (good) {
                    val.crval = rtemp;
                    val.cival = itemp;
                    break;
				  }
				}
			  }
			}
		  }
        }
        /* else */
        setmode(&val, CF);
        setmode(&arg, CF);
        /* drop through */
#else
        rtemp = val.crval * arg.crval - val.cival * arg.cival;
        itemp = val.cival * arg.crval + val.crval * arg.cival;
        val.crval = rtemp; val.cival = itemp;
        break;
#endif /* NEWFIXARITH */
      case CF:
        frtemp = val.cfrval * arg.cfrval - val.cfival * arg.cfival;
        fitemp = val.cfival * arg.cfrval + val.cfrval * arg.cfival;
        val.cfrval = frtemp; val.cfival = fitemp;
        break;
      }
      break;
    case '/':
      switch (val.mode) {
      case IN:
        checkizero(arg.val);
#ifdef NEWFIXARITH
        if ((val.val != MOST_NEGATIVE_FIX || arg.val != -1)
	    && val.val % arg.val == 0) {
          val.val /= arg.val;
          break;
        }
#else
        if (val.val % arg.val == 0) {
          val.val /= arg.val;
          break;
        }
#endif /* NEWFIXARITH */
        else {
          setmode(&val, FL);
          setmode(&arg, FL);
        }
        /* drop through */
      case FL:
        checkfzero(arg.fval);
        val.fval /= arg.fval;
        break;
      case CI:
        setmode(&val, CF);
        setmode(&arg, CF);
        /* drop through */
      case CF:
        magn = arg.cfrval * arg.cfrval + arg.cfival * arg.cfival;
        checkfzero(magn);
        frtemp = (val.cfrval * arg.cfrval + val.cfival * arg.cfival) / magn;
        fitemp = (val.cfival * arg.cfrval - val.cfrval * arg.cfival) / magn;
        val.cfrval = frtemp; val.cfival = fitemp;
        break;
      }
      break;
    case 'M':
      switch (val.mode) {
      case IN: val.val  = (val.val > arg.val)   ? val.val  : arg.val;  break;
      case FL: val.fval = (val.fval > arg.fval) ? val.fval : arg.fval; break;
      default: badarg(larg);
      }
      break;
    case 'm':
      switch (val.mode) {
      case IN: val.val  = (val.val < arg.val)   ? val.val  : arg.val;  break;
      case FL: val.fval = (val.fval < arg.fval) ? val.fval : arg.fval; break;
      default: badarg(larg);
      }
      break;
    }
  }
  return(lispnumber(&val));
}

static get_rem_arg(fval, mode)
	FLOTYPE *fval;
	int *mode;
{
  LVAL arg;
  
  arg = xlgetarg();
  if (fixp(arg)) {
    *fval = getfixnum(arg);
    *mode = IN;
  }
  else if (floatp(arg)) {
    *fval = getflonum(arg);
    *mode = FL;
  }
  else badarg(arg);
}

static LVAL xremmod(which)
     int which;
{
  int mode1, mode2;
  FLOTYPE fval1, fval2, fres, rat, sgn;
  
  get_rem_arg(&fval1, &mode1);
  get_rem_arg(&fval2, &mode2);
  xllastarg();

  checkfzero(fval2);
  switch (which) {
  case 'R':
    rat = fval1 / fval2;
    sgn = (rat > 0) ? 1.0 : -1.0;
    fres = fval1 - fval2 * sgn * floor(fabs(rat));
    break;
  case 'M': fres = fval1 - fval2 * floor(fval1 / fval2); break;
  }
  return((mode1 == IN && mode2 == IN) ? cvfixnum((FIXTYPE) fres)
                                      : cvflonum((FLOTYPE) fres));
}

LVAL xrem() { return(xremmod('R')); }
LVAL xmod() { return(xremmod('M')); }

LVAL logbinary(which)
	int which;
{
  int val, arg;
  
  switch (which) {
  case '&': val = -1; break;
  case '|': val =  0; break;
  case '^': val =  0; break;
  }
  while (moreargs()) {
    arg = getfixnum(xlgafixnum());
    switch (which) {
    case '&': val &= arg; break;
    case '|': val |= arg; break;
    case '^': val ^= arg; break;
    }
  }
  return(cvfixnum((FIXTYPE) val));
}

LVAL xexpt()
{
  LVAL base, power;
  int bsign, psign;
  FIXTYPE b, p, val;
  FLOTYPE fb, fp, fval;
  
  base = xlgetarg();
  power = xlgetarg();
  xllastarg();
  
  if (fixp(base) && fixp(power)) {
    b = getfixnum(base);
    p = getfixnum(power);
    if (p == 0) return(cvfixnum((FIXTYPE) 1));
    if (b == 0 && p > 0) return(cvfixnum((FIXTYPE) 0));
    checkizero(b);
    bsign = (b > 0) ? 1 : -1;
    psign = (p > 0) ? 1 : -1;
    b = (b > 0) ? b : -b;
    p = (p > 0) ? p : -p;
    fval = floor(f_expt((double) b, (double) p) + 0.1); /* to get integer right */
    if (bsign == -1 && p % 2 == 1) fval = -fval;
    if (psign == 1) {
      val = fval;
      if (val == fval) return(cvfixnum((FIXTYPE) val));
      else return(cvflonum((FLOTYPE) fval));    /* to handle precision for large results */
    }
    else {
      checkfzero(fval);
      return(cvflonum((FLOTYPE) 1.0 / fval));
    }
  } 
  else if (floatp(base) && fixp(power)) {
    fb = getflonum(base);
    p = getfixnum(power);
    if (p == 0) return(cvfixnum((FIXTYPE) 1));
    if (fb == 0.0 && p > 0) return(cvflonum((FLOTYPE) 0.0));
    checkfzero(fb);
    bsign = (fb > 0) ? 1 : -1;
    psign = (p > 0) ? 1 : -1;
    fb = (fb > 0) ? fb : -fb;
    p = (p > 0) ? p : -p;
    fval = f_expt((double) fb, (double) p);
    if (bsign == -1 && p % 2 == 1) fval = -fval;
    if (psign == 1) return(cvflonum((FLOTYPE) fval));
    else {
      checkfzero(fval);
      return(cvflonum((FLOTYPE) 1.0 / fval));
    }
  }
  else if ((fixp(base) || floatp(base)) && floatp(power)) {
    fb = makedouble(base);
    fp = getflonum(power);
    if (fp == 0.0) return(cvflonum((FLOTYPE) 1.0));
    if (fb == 0.0 && fp > 0.0) return(cvflonum((FLOTYPE) 0.0));
    if (fb < 0.0)
      return(cvcomplex(cexpt(makecomplex(base), makecomplex(power))));
    psign = (fp > 0) ? 1 : -1;
    fb = (fb > 0) ? fb : -fb;
    fp = (fp > 0) ? fp : -fp;
    fval = f_expt((double) fb, (double) fp);
    if (psign == 1) return(cvflonum((FLOTYPE) fval));
    else {
      checkfzero(fval);
      return(cvflonum((FLOTYPE) 1.0 / fval));
    }    
  }
  else if (complexp(base) || complexp(power))
    return(cvcomplex(cexpt(makecomplex(base), makecomplex(power))));
  else xlfail("bad argument type(s)");
}

LVAL xlog()
{
  LVAL arg, base;
  int base_supplied = FALSE;
  double fx, fb;
  
  arg = xlgetarg();
  if (moreargs()) {
    base_supplied = TRUE;
    base = xlgetarg();
  }
  if (base_supplied) {
    if (numberp(arg) && numberp(base)) {
      fx = makedouble(arg);
      fb = makedouble(base);
      if (fx <= 0.0 || fb <= 0.0)
        return(cvcomplex(cdiv(clog(makecomplex(arg)), clog(makecomplex(base)))));
      else return(cvflonum((FLOTYPE) logarithm(fx, fb, TRUE)));
    }
    else if ((numberp(arg) && complexp(base))
             || (complexp(arg) && numberp(base))
             || (complexp(arg) && complexp(base)))
      return(cvcomplex(cdiv(clog(makecomplex(arg)), clog(makecomplex(base)))));
    else xlfail("bad argument type(s)");
  }
  else {
    if (numberp(arg)) {
      fx = makedouble(arg);
      if (fx <= 0.0) return(cvcomplex(clog(makecomplex(arg))));
      else return(cvflonum((FLOTYPE) logarithm(fx, 0.0, FALSE)));
    }
    else if (complexp(arg)) 
      return(cvcomplex(clog(makecomplex(arg))));
    else xlfail("bad argument type(s)");
  }
}

/* xgcd - greatest common divisor */
LVAL xgcd()
{
  FIXTYPE m,n,r;
  LVAL arg;

  if (!moreargs())                  /* check for identity case */
    return (cvfixnum((FIXTYPE)0));
  arg = xlgafixnum();
  n = getfixnum(arg);
  if (n < (FIXTYPE)0) n = -n;		/* absolute value */
  while (moreargs()) {
    arg = xlgafixnum();
    m = getfixnum(arg);
    if (m == 0 || n == 0) xlfail("zero argument");
    if (m < (FIXTYPE)0) m = -m;	    /* absolute value */
    for (;;) {                      /* euclid's algorithm */
      r = m % n;
      if (r == (FIXTYPE) 0)
        break;
      m = n;
      n = r;
    }
  }
  return (cvfixnum(n));
}

/* checkizero - check for integer division by zero */
checkizero(iarg)
  FIXTYPE iarg;
{
  if (iarg == 0)
  xlfail("illegal zero argument");
}

/* checkfzero - check for floating point division by zero or log of zero */
checkfzero(farg)
  FLOTYPE farg;
{
  if (farg == 0.0)
  xlfail("illegal zero argument");
}

#ifdef NEWFIXARITH
/* return FIXTYPE or FLOTYPE if too large for FIXTYPE */
static LVAL cvfixorflo(x)
     double x;
{
  FIXTYPE temp = x;
  if (x == temp) return(cvfixnum((FIXTYPE) temp));
  else return(cvflonum((FLOTYPE) x));
}
#endif /* NEWFIXARITH */

/* unary functions */
LVAL xlognot() { return (unary('~')); } /* lognot */
LVAL xabs()    { return (unary('A')); } /* abs */
LVAL xadd1()   { return (unary('+')); } /* 1+ */
LVAL xsub1()   { return (unary('-')); } /* 1- */
LVAL xsin()    { return (unary('S')); } /* sin */
LVAL xcos()    { return (unary('C')); } /* cos */
LVAL xtan()    { return (unary('T')); } /* tan */
LVAL xexp()    { return (unary('E')); } /* exp */
LVAL xsqrt()   { return (unary('R')); } /* sqrt */
LVAL xfix()    { return (unary('I')); } /* truncate */
LVAL xfloat()  { return (unary('F')); } /* float */
LVAL xrand()   { return (unary('?')); } /* random */
LVAL xfloor()  { return (unary('_')); } /* floor */
LVAL xceil()   { return (unary('^')); } /* ceiling */
LVAL xround()  { return (unary('r')); } /* round */
LVAL xasin()   { return (unary('s')); } /* asin */
LVAL xacos()   { return (unary('c')); } /* acos */
LVAL xatan()   { return (unary('t')); } /* atan */
LVAL xphase()  { return (unary('P')); } /* phase */

/* unary - handle unary operations */
LOCAL LVAL unary(which)
	int which;
{
  FLOTYPE fval;
  FIXTYPE ival, itemp;
  Complex cval;
  LVAL arg, real, imag;
  int mode;
  
  /* get the argument */
  arg = xlgetarg();
  if (which == 'F' && moreargs()) xlgaflonum();
  xllastarg();

  /* check its type */
  if (fixp(arg)) {
    ival = getfixnum(arg);
    mode = IN;
  }
  else if (floatp(arg)) {
    fval = getflonum(arg);
    mode = FL;
  }
  else if (complexp(arg)) {
    cval = makecomplex(arg);
    real = realpart(arg);
    imag = imagpart(arg);
    if (fixp(realpart(arg))) mode = CI;
    else mode = CF;
  }
  else xlerror("not a number", arg);

  switch (which) {
  case '~':
    if (mode == IN) return(cvfixnum((FIXTYPE) ~ival));    
    else badiop();
    break;
  case 'A':
    switch (mode) {
#ifdef NEWFIXARITH
    case IN: 
      itemp = ival < 0 ? -ival : ival;
      if (0 <= itemp) return(cvfixnum((FIXTYPE) itemp));
      else fval = ival;
      /* drop through */
#else
    case IN: return(cvfixnum((FIXTYPE) (ival < 0   ? -ival : ival)));
#endif /* NEWFIXARITH */
    case FL: return(cvflonum((FLOTYPE) (fval < 0.0 ? -fval : fval)));
    case CI:
    case CF: return(cvflonum((FLOTYPE) modulus(cval)));
    }
    break;
  case '+':
    switch (mode) {
#ifdef NEWFIXARITH
    case IN: 
      itemp = ival + 1;
      if (ival < 0 || itemp > 0) return(cvfixnum((FIXTYPE) itemp));
      else fval = ival;
      /* drop through */
#else
    case IN: return(cvfixnum((FIXTYPE) ival + 1));
#endif /* NEWFIXARITH */
    case FL: return(cvflonum((FLOTYPE) fval + 1.0));
#ifdef NEWFIXARITH
    case CI:
      ival = getfixnum(real);
      itemp = ival + 1;
      if (ival < 0 || itemp > 0)
	return(newicomplex(itemp, getfixnum(imag)));
      /* else drop through */
    case CF: return(newdcomplex(makedouble(real) + 1.0, makedouble(imag)));
#else
    case CI: return(newicomplex(getfixnum(real) + 1, getfixnum(imag)));
    case CF: return(newdcomplex(getflonum(real) + 1.0, getflonum(imag)));
#endif /* NEWFIXARITH */
    }
    break;
  case '-':
    switch (mode) {
#ifdef NEWFIXARITH
    case IN: 
      itemp = ival - 1;
      if (ival != MOST_NEGATIVE_FIX && (ival > 0 || itemp < 0))
	return(cvfixnum((FIXTYPE) itemp));
      else fval = ival;
      /* drop through */
#else
    case IN: return(cvfixnum((FIXTYPE) ival - 1));
#endif /* NEWFIXRITH */
    case FL: return(cvflonum((FLOTYPE) fval - 1.0));
#ifdef NEWFIXARITH
    case CI:
      ival = getfixnum(real);
      itemp = ival - 1;
      if (ival != MOST_NEGATIVE_FIX && (ival > 0 || itemp < 0))
	return(newicomplex(itemp, getfixnum(imag)));
      /* else drop through */
    case CF: return(newdcomplex(makedouble(real) - 1.0, makedouble(imag)));
#else
    case CI: return(newicomplex(getfixnum(real) - 1, getfixnum(imag)));
    case CF: return(newdcomplex(getflonum(real) - 1.0, getflonum(imag)));
#endif /* NEWFIXARITH */
    }
    break;
  case 'S':
    switch (mode) {
    case IN: return(cvflonum((FLOTYPE) sin((double) ival)));
    case FL: return(cvflonum((FLOTYPE) sin((double) fval)));
    case CI:
    case CF: return(cvcomplex(csin(cval)));
    }
  case 'C':
    switch (mode) {
    case IN: return(cvflonum((FLOTYPE) cos((double) ival)));
    case FL: return(cvflonum((FLOTYPE) cos((double) fval)));
    case CI:
    case CF: return(cvcomplex(ccos(cval)));
    }
  case 'T': 
    switch (mode) {
    case IN: return(cvflonum((FLOTYPE) tan((double) ival)));
    case FL: return(cvflonum((FLOTYPE) tan((double) fval)));
    case CI:
    case CF: return(cvcomplex(ctan(cval)));
    }
  case 'E':
    switch (mode) {
    case IN: return(cvflonum((FLOTYPE) f_exp((double) ival)));
    case FL: return(cvflonum((FLOTYPE) f_exp((double) fval)));
	case CI: 
	case CF: return(cvcomplex(cexp(cval)));
	}
	break;
  case 'R':
    switch (mode) {
    case IN:
      if (ival < 0) return(cvcomplex(csqrt(makecomplex(arg))));
	  else return(cvflonum((FLOTYPE) f_sqrt((double) ival)));
	case FL:
      if (fval < 0) return(cvcomplex(csqrt(makecomplex(arg))));
      else return(cvflonum((FLOTYPE) f_sqrt(fval)));
    case CI:
    case CF: return(cvcomplex(csqrt(cval)));
    }
    break;
  case 'I':
    switch (mode) {
    case IN: return (cvfixnum((FIXTYPE) ival));
#ifdef NEWFIXARITH
    case FL: return (cvfixorflo((fval < 0.0) ? ceil(fval) : floor(fval)));
#else
    case FL: return (cvfixnum((FIXTYPE) fval));
#endif /* NEWFIXARITH */
	default: badcop();
	}
	break;
  case 'F': 
    switch (mode) {
    case IN: return (cvflonum((FLOTYPE) ival));
    case FL: return (cvflonum((FLOTYPE) fval));
	default: badcop();
	}
	break;
  case '?':
    switch (mode) {
    case IN: 
	  if (ival <= 0) badiop();
	  return (cvfixnum((FIXTYPE) osrand((int) ival)));
    case FL: 
	  if (fval <= 0.0) badfop();
	  return (cvflonum((FLOTYPE) fval * uni()));
	default: badcop();
	}
	break;
  case '_':
    switch (mode) {
    case IN: return (cvfixnum((FIXTYPE) ival));
#ifdef NEWFIXARITH
    case FL: return (cvfixorflo(floor(fval)));
#else
    case FL: return (cvfixnum((FIXTYPE) floor(fval)));
#endif /* NEWFIXARITH */
	default: badcop();
	}
	break;
  case '^':
    switch (mode) {
    case IN: return (cvfixnum((FIXTYPE) ival));
#ifdef NEWFIXARITH
    case FL: return (cvfixorflo(ceil(fval)));
#else
    case FL: return (cvfixnum((FIXTYPE) ceil(fval)));
#endif /* NEWFIXARITH */
	default: badcop();
	}
	break;
  case 'r':
    switch (mode) {
    case IN: return (cvfixnum((FIXTYPE) ival));
#ifdef NEWFIXARITH
    case FL: return (cvfixorflo(floor(fval + 0.5)));
#else
    case FL: return (cvfixnum((FIXTYPE) floor(fval + 0.5)));
#endif /* NEWFIXARITH */
	default: badcop();
	}
	break;
  case 's':
    switch (mode) {
    case IN:
      fval = ival;
      /* drop through */
    case FL:
      if (fval > 1.0 || fval < -1.0) 
        return(cvcomplex(casin(makecomplex(arg))));
      else return(cvflonum((FLOTYPE) asin(fval)));
	case CI:
	case CF: return(cvcomplex(casin(cval)));
	}
	break;
  case 'c':
    switch (mode) {
    case IN:
      fval = ival;
      /* drop through */
    case FL:
      if (fval > 1.0 || fval < -1.0) 
        return(cvcomplex(cacos(makecomplex(arg))));
      else return(cvflonum((FLOTYPE) acos(fval)));
	case CI:
	case CF: return(cvcomplex(cacos(cval)));
	}
	break;
  case 't':
    switch (mode) {
    case IN: fval = ival; /* drop through */
    case FL: return(cvflonum((FLOTYPE) atan(fval)));
	case CI:
	case CF: return(cvcomplex(catan(cval)));
	}
	break;
  case 'P':
    switch (mode) {
    case IN: return(cvflonum((FLOTYPE) (ival >= 0) ? 0.0 : PI));
	case FL: return(cvflonum((FLOTYPE) (fval >= 0.0) ? 0.0 : PI));
	case CI:
	case CF: return(cvflonum((FLOTYPE) phase(cval)));
	}
	break;
  default: xlfail("unsupported operation");
  }
}

/* unary predicates */
LVAL xminusp() { return (predicate('-')); } /* minusp */
LVAL xzerop()  { return (predicate('Z')); } /* zerop */
LVAL xplusp()  { return (predicate('+')); } /* plusp */
LVAL xevenp()  { return (predicate('E')); } /* evenp */
LVAL xoddp()   { return (predicate('O')); } /* oddp */


/* predicate - handle a predicate function */
LOCAL LVAL predicate(fcn)
  int fcn;
{
  FLOTYPE fval;
  FIXTYPE ival;
  LVAL arg;

  /* get the argument */
  arg = xlgetarg();
  xllastarg();

  /* check the argument type */
  if (fixp(arg)) {
    ival = getfixnum(arg);
    switch (fcn) {
    case '-': ival = (ival < 0); break;
    case 'Z': ival = (ival == 0); break;
    case '+': ival = (ival > 0); break;
    case 'E': ival = ((ival & 1) == 0); break;
    case 'O': ival = ((ival & 1) != 0); break;
    default:  badiop();
    }
  }
  else if (floatp(arg)) {
    fval = getflonum(arg);
    switch (fcn) {
    case '-': ival = (fval < 0); break;
    case 'Z': ival = (fval == 0); break;
    case '+': ival = (fval > 0); break;
    default:  badfop();
    }
  }
  else
    badarg(arg);

  /* return the result value */
  return (ival ? true : NIL);
}

/* comparison functions */
LVAL xlss() { return (compare('<'));  } /* < */
LVAL xleq() { return (compare('L'));  } /* <= */
LVAL xequ() { return (ccompare('=')); } /* = */
LVAL xneq() { return (ccompare('#')); } /* /= */
LVAL xgeq() { return (compare('G'));  } /* >= */
LVAL xgtr() { return (compare('>'));  } /* > */

/* compare - common compare function */
LOCAL LVAL compare(fcn)
  int fcn;
{
    FIXTYPE icmp,ival,iarg;
    FLOTYPE fcmp,fval,farg;
    LVAL arg;
    int mode;

    /* get the first argument */
    arg = xlgetarg();

    /* set the type of the first argument */
    if (fixp(arg)) {
	ival = getfixnum(arg);
	mode = 'I';
    }
    else if (floatp(arg)) {
	fval = getflonum(arg);
	mode = 'F';
    }
    else
      badarg(arg);

    /* handle each remaining argument */
    for (icmp = TRUE; icmp && moreargs(); ival = iarg, fval = farg) {

	/* get the next argument */
	arg = xlgetarg();

	/* check its type */
	if (fixp(arg)) {
	    switch (mode) {
	    case 'I':
	        iarg = getfixnum(arg);
	        break;
	    case 'F':
	        farg = (FLOTYPE)getfixnum(arg);
		break;
	    }
	}
	else if (floatp(arg)) {
	    switch (mode) {
	    case 'I':
	        fval = (FLOTYPE)ival;
		farg = getflonum(arg);
		mode = 'F';
		break;
	    case 'F':
	        farg = getflonum(arg);
		break;
	    }
	}
	else
	  badarg(arg);

	/* compute result of the compare */
	switch (mode) {
	case 'I':
	    icmp = ival - iarg;
	    switch (fcn) {
	    case '<':	icmp = (icmp < 0); break;
	    case 'L':	icmp = (icmp <= 0); break;
	    case '=':	icmp = (icmp == 0); break;
	    case '#':	icmp = (icmp != 0); break;
	    case 'G':	icmp = (icmp >= 0); break;
	    case '>':	icmp = (icmp > 0); break;
	    }
	    break;
	case 'F':
	    fcmp = fval - farg;
	    switch (fcn) {
	    case '<':	icmp = (fcmp < 0.0); break;
	    case 'L':	icmp = (fcmp <= 0.0); break;
	    case '=':	icmp = (fcmp == 0.0); break;
	    case '#':	icmp = (fcmp != 0.0); break;
	    case 'G':	icmp = (fcmp >= 0.0); break;
	    case '>':	icmp = (fcmp > 0.0); break;
	    }
	    break;
	}
    }

    /* return the result */
    return (icmp ? true : NIL);
}

LVAL ccompare(which)
	int which;
{
  LVAL larg;
  Number val, arg;
  int icmp;
  
  switch (which) {
  case '=': icmp = TRUE;  break;
  case '#': icmp = FALSE; break;
  }
  larg = readnumber(&val);  
  while (moreargs()) {
    larg = readnumber(&arg);
    matchmodes(&val, &arg);
    switch (which) {
    case '=':
      switch (val.mode) {
      case IN: icmp = icmp && val.val  == arg.val;  break;
      case FL: icmp = icmp && val.fval == arg.fval; break;
      case CI: icmp = icmp && val.crval == arg.crval && val.cival == arg.cival; break;
      case CF: icmp = icmp && val.cfrval == arg.cfrval && val.cfival == arg.cfival; break;
      }
      break;
    case '#':
      switch (val.mode) {
      case IN: icmp = icmp || val.val  != arg.val;  break;
      case FL: icmp = icmp || val.fval != arg.fval; break;
      case CI: icmp = icmp || val.crval != arg.crval || val.cival != arg.cival; break;
      case CF: icmp = icmp || val.cfrval != arg.cfrval || val.cfival != arg.cfival; break;
      }
      break;
    }
  }
  return((icmp) ? true : NIL);
}

/* badiop - bad integer operation */
LOCAL badiop()
{
  xlfail("bad integer operation");
}

/* badfop - bad floating point operation */
LOCAL badfop()
{
  xlfail("bad floating point operation");
}

/* badcop - bad complex number operation */
LOCAL badcop()
{
  xlfail("bad complex number operation");
}

/* two argument logarithm */
double logarithm(x, base, base_supplied)
     FLOTYPE x, base;
     int base_supplied;
{
  double lbase;
  if (x <= 0.0) xlfail("logarithm of a nonpositive number");
  if (base_supplied) {
    if (base <= 0.0) xlfail("logarithm to a nonpositive base");
    else {
      lbase = f_log(base);
      if (lbase == 0.0) xlfail("logarith to a unit base");
      else return((f_log(x)/lbase));
    }
  }
  else return (f_log(x));
}
