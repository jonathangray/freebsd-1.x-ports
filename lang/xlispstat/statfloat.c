/* statfloat - Floating point operations with error checking           */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

# include "xmath.h"
# include "xlisp.h"

# ifdef MACINTOSH
extern long flog(), fexp(), fsqrt(), ftimes(), fdivid(), fexpt();
# endif

double f_plus(x, y)
     double x, y;
{
  return(x + y);
}

double f_minus(x, y)
     double x, y;
{
  return(x - y);
}

double f_times(x, y)
     double x, y;
{
# ifdef DODO /* MACINTOSH */
  short double sa, sx, sy;
  double a;

  sx = x;
  sy = y;
  f77sub(ftimes, &sa, &sx, &sy);
  a = sa;
  return(a);
# else
  return(x * y);
# endif
}

double f_divide(x, y)
     double x, y;
{
#ifdef DODO /* MACINTOSH */
  short double sa, sx, sy;
  double a;

  if (y == 0.0) xlfail("division by zero");

  sx = x;
  sy = y;
  f77sub(fdivid, &sa, &sx, &sy);
  a = sa;
  return(a);
#else
  return(x / y);
#endif
}

double f_log(x)
     double x;
{
# ifdef DODO /* MACINTOSH */
  short double sa, sx;
  double a;
	
  if (x <= 0) xlfail("logarithm of nonpositive number");
  sx = x;
  f77sub(flog, &sa, &sx);
  a = sa;
  return(a);
# else
  return(log(x));
# endif
}

double f_exp(x)
     double x;
{
# ifdef DODO /* MACINTOSH */
  short double sa, sx;
  double a;
	
  sx = x;
  f77sub(fexp, &sa, &sx);
  a = sa;
  return(a);
# else
  return(exp(x));
# endif 
}

double f_sqrt(x)
     double x;
{
# ifdef DODO /* MACINTOSH */
  short double sa, sx;
  double a;

  if (x < 0) xlfail("square root of negative number");
  sx = x;
  f77sub(fsqrt, &sa, &sx);
  a = sa;
  return(a);
# else
  return(sqrt(x));
# endif
}

double f_expt(x, y)
     double x, y;
{
# ifdef DODO /* MACINTOSH */
  short double sa, sx, sy;
  double a;

  sx = x;
  sy = y;
  f77sub(fexpt, &sa, &sx, &sy);
  a = sa;
  return(a);
# else
  return(pow(x, y));
#endif  
}

