/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

typedef struct {
  double real, imag;
} Complex;
 
#ifndef PI
#define PI 3.141592653589793
#endif PI

# define numberp(x) (floatp(x) || fixp(x))

extern Complex makecomplex();
extern Complex cart2complex(), polar2complex();
extern Complex csqrt(), cexp(), clog(), cexpt(), csin(), ccos(), ctan();
extern Complex casin(), cacos(), catan();
extern Complex cadd(), csub(), cmul(), cdiv();
extern double phase(), modulus();
extern LVAL cvcomplex(), newdcomplex(), newicomplex();
extern LVAL realpart(), imagpart();
