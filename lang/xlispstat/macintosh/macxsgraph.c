/* macxsgraph - Macintosh lisp low level graphics functions            */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#ifdef MPWC
# include <Windows.h>
# include <Picker.h>
#else
# include <WindowMgr.h>
# include <Color.h>
#endif MPWC
# include "xlisp.h"

# define nil 0L
# define MULTIPLIER 62535

/* external variables */

/* external functions */
extern double makedouble();
extern LVAL mklist();

static RGBColor ListToRGB(x)
	LVAL x;
{
  RGBColor color;

  if (! consp(x) || llength(x) != 3) xlerror("not a color list", x);
  color.red   = MULTIPLIER * makedouble(car(x)); x = cdr(x);
  color.green = MULTIPLIER * makedouble(car(x)); x = cdr(x);
  color.blue  = MULTIPLIER * makedouble(car(x));
  return(color);
}
  
static LVAL RGBToList(color)
	RGBColor color;
{
  LVAL result, rp;
  
  xlsave1(result);
  result = rp = mklist(3, NIL);
  rplaca(rp, cvflonum((FLOTYPE) (((double) color.red)   / MULTIPLIER)));
  rp = cdr(rp);
  rplaca(rp, cvflonum((FLOTYPE) (((double) color.green) / MULTIPLIER)));
  rp = cdr(rp);
  rplaca(rp, cvflonum((FLOTYPE) (((double) color.blue)  / MULTIPLIER)));
  xlpop();
  return(result);
}

LVAL xspick_color()
{
  Point where;
  char *prompt;
  RGBColor in_color, out_color;
  int ok;
  LVAL arg, sk_initial = xlenter(":INITIAL");
  
  if (! StScreenHasColor()) return(NIL);
  
  in_color.red = 0; 
  in_color.green = 0;
   in_color.blue = 0;
  if (moreargs()) {
    prompt = (char *) getstring(xlgastring());
    if (xlgetkeyarg(sk_initial, &arg)) in_color = ListToRGB(arg);
  }
  else prompt = "Pick a color";
  
  where.h = 0; where.v = 0;
  CtoPstr(prompt);
  ok = GetColor(where, prompt, &in_color, &out_color);
  PtoCstr(prompt);
  
  return((ok) ? RGBToList(out_color) : NIL);
}
