/* complex - Complex number functions                                  */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "xmath.h"
#include "statfloat.h"

Complex makecomplex(x)
	LVAL x;
{
  Complex c;
  
  if (numberp(x)) {
    c.real = makedouble(x);
    c.imag = 0.0;
  }
  else if (complexp(x)) {
    c.real = makedouble(realpart(x));
    c.imag = makedouble(imagpart(x));
  }
  else xlerror("not a number", x);
  return(c);
}

double phase(c)
	Complex c;
{
  double phi;
  
  if (c.real == 0.0) {
    if (c.imag > 0.0) phi = PI / 2;
    else if (c.imag == 0.0) phi = 0.0;
    else phi = -PI / 2;
  }
  else {
    phi = atan(c.imag / c.real);
    if (c.real < 0.0) {
      if (c.imag > 0.0) phi += PI;
      else if (c.imag < 0.0) phi -= PI;
      else phi = PI;
    }
  }
  return(phi);
}

double modulus(c)
	Complex c;
{
  return(sqrt(c.real * c.real + c.imag * c.imag));
}

Complex cart2complex(real, imag)
	double real, imag;
{
  Complex val;
  val.real = real;
  val.imag = imag;
  return(val);
}

LVAL cvcomplex(c)
	Complex c;
{
  return(newdcomplex(c.real, c.imag));
}

Complex polar2complex(mod, phi)
	double mod, phi;
{
  Complex val;
  double cs, sn;
  
  if (phi == 0) {
    cs = 1.0;
    sn = 0.0;
  }
  else if (phi == PI / 2) {
    cs = 0.0;
    sn = 1.0;
  }
  else if (phi == PI) {
    cs = -1.0;
    sn = 0.0;
  }
  else if (phi == -PI / 2) {
    cs = 0.0;
    sn = -1.0;
  }
  else {
    cs = cos(phi);
    sn = sin(phi);
  }
  val.real = mod * cs;
  val.imag = mod * sn;
  return(val);
}
 
Complex csqrt(c)
	Complex c;
{
  return(polar2complex(sqrt(modulus(c)), phase(c) / 2));
}

Complex cexp(c)
	Complex c;
{
  return(polar2complex(f_exp(c.real), c.imag));
}

Complex clog(c)
	Complex c;
{
  double mod;
  
  mod = modulus(c);
  checkfzero(mod);
  return(cart2complex(f_log(mod), phase(c)));
}

Complex cexpt(cb, cp)
	Complex cb, cp;
{
  if (modulus(cp) == 0.0) return(cart2complex(1.0, 0.0));
  else  return(cexp(cmul(clog(cb), cp)));
}

Complex cadd(c1, c2)
	Complex c1, c2;
{
  return(cart2complex(c1.real + c2.real, c1.imag + c2.imag));
}

Complex csub(c1, c2)
	Complex c1, c2;
{
  return(cart2complex(c1.real - c2.real, c1.imag - c2.imag));
}

Complex cmul(c1, c2)
	Complex c1, c2;
{
  double m1, m2, p1, p2;
  
  m1 = modulus(c1);
  p1 = phase(c1);
  m2 = modulus(c2);
  p2 = phase(c2);
  return(polar2complex(m1 * m2, p1 + p2));
}

Complex cdiv(c1, c2)
	Complex c1, c2;
{
  double m1, m2, p1, p2;
  
  m1 = modulus(c1);
  p1 = phase(c1);
  m2 = modulus(c2);
  p2 = phase(c2);
  checkfzero(m2);
  return(polar2complex(m1 / m2, p1 - p2));
}

Complex csin(c)
	Complex c;
{
  Complex x1, x2, val;
  
  x1 = cart2complex(-c.imag, c.real);
  x2 = cart2complex(c.imag, -c.real);
  val = csub(cexp(x1), cexp(x2));
  return(cart2complex(val.imag / 2.0, -val.real / 2.0));
}

Complex ccos(c)
	Complex c;
{
  Complex x1, x2, val;
  
  x1 = cart2complex(-c.imag, c.real);
  x2 = cart2complex(c.imag, -c.real);
  val = cadd(cexp(x1), cexp(x2));
  return(cart2complex(val.real / 2.0, val.imag / 2.0));
}

Complex ctan(c)
	Complex c;
{
  Complex e1, e2, val;
  
  e1 = cexp(cart2complex(-c.imag, c.real));
  e2 = cexp(cart2complex(c.imag, -c.real));
  val = cdiv(csub(e1, e2), cadd(e1, e2));
  return(cart2complex(val.imag, -val.real));
}

Complex casin(c)
	Complex c;
{
  Complex sx, ix, val;
  
  sx = cmul(c, c);
  sx = csqrt(cart2complex(1.0 - sx.real, - sx.imag));
  ix = cart2complex(-c.imag, c.real);
  val = clog(cadd(ix, sx));
  return(cart2complex(val.imag, -val.real));
}

Complex cacos(c)
	Complex c;
{
  Complex sx, val;
  
  sx = cmul(c, c);
  sx = csqrt(cart2complex(1.0 - sx.real, - sx.imag));
  sx = cart2complex(-sx.imag, sx.real);
  val = clog(cadd(c, sx));
  return(cart2complex(val.imag, -val.real));
}

Complex catan(c)
	Complex c;
{
  Complex sx, ix, val, one;
  
  sx = cmul(c, c);
  sx = cart2complex(1.0 + sx.real, sx.imag);
  one = cart2complex(1.0, 0.0);
  sx = csqrt(cdiv(one, sx));
  ix = cadd(one, cart2complex(-c.imag, c.real));
  val = clog(cmul(ix, sx));
  return(cart2complex(val.imag, -val.real));
}
