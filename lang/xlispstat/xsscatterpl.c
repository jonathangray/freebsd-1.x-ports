/* xsscatterplot - XLISP interface to IVIEW dynamic graphics package.  */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xmath.h"
#include "xlisp.h"
#include "iview.h"

# define sequencep(x) (listp(x) || simplevectorp(x))

/* external variables */
extern LVAL sk_draw, sk_resize, sk_redraw, s_true;
extern LVAL s_scale_type;

/* external functions */
extern LVAL list2(), slot_value();
extern IVIEW_WINDOW GETIVIEWADDRESS();
extern char *IViewVariableLabel();

static adjust_variable(w, var, ticks, labeled) 
	IVIEW_WINDOW w;
	int var, *ticks, *labeled;
{
  double low, high;
  char *label;
  
  IViewGetRange(w, var, &low, &high);
  GetNiceRange(&low, &high, ticks);
  IViewSetRange(w, var, low, high);
  label = IViewVariableLabel(w, var);
  *labeled = (label != nil && strlen(label) > 0) ? TRUE : FALSE;
}

LVAL iview_plot2d_adjust_to_data()
{
  LVAL object;
  IVIEW_WINDOW w;
  char *gwinfo;
  int x, y, ticks, labeled, scaled;
  LVAL arg;
  
  object = xlgaobject();
  w = GETIVIEWADDRESS(object);
  gwinfo = StGWObWinInfo(object);
  
  if (w != nil) {
    scaled = (slot_value(object, s_scale_type) != NIL) ? TRUE : FALSE;
    StGrGetContentVariables(gwinfo, &x, &y);
    StGrObAdjustToData(object, FALSE);
    ticks = 4;
    if (! scaled) adjust_variable(w, x, &ticks, &labeled);
    IViewSetXaxis(w, ! scaled, labeled, ticks);
    ticks = 4;
    if (! scaled) adjust_variable(w, y, &ticks, &labeled);
    IViewSetYaxis(w, ! scaled, labeled, ticks);

    if (! xlgetkeyarg(sk_draw, &arg)) arg = s_true;
    if (arg != NIL) send_message(object, sk_resize);
    if (arg != NIL) send_message(object, sk_redraw);  
  }
  return(NIL);
}

static LVAL plot2d_add_data(which)
     int which;
{
  IVIEW_WINDOW w;
  int old_n, n;
  LVAL x, y, data, object;
  
  object = xlgaobject();
  w = GETIVIEWADDRESS(object);
  
  if (w == nil) return(NIL);
    
  xlsave1(data);
  x = xlgetarg();
  if (fixp(x) || (consp(x) && sequencep(car(x)))) data = x;
  else {
    y = xlgetarg();
    data = list2(x, y);
  }
  switch (which) {
  case 'P':
    old_n = IViewNumPoints(w);
    internal_iview_add_points(w, object, data);
    n = IViewNumPoints(w);
    break;
  case 'L':
    old_n = IViewNumLines(w);
    internal_iview_add_lines(w, object, data);
    n = IViewNumLines(w);
    break;
#ifdef USESTRINGS
  case 'S':
    old_n = IViewNumStrings(w);
    internal_iview_add_strings(w, object, data);
    n = IViewNumStrings(w);
    break;
#endif /* USESTRINGS */
  } 
  xlpop();
  
  check_add_to_screen(object, which, old_n, n, FALSE);
  
  return(NIL);
}

LVAL iview_plot2d_add_points()   { return(plot2d_add_data('P')); }
LVAL iview_plot2d_add_lines()    { return(plot2d_add_data('L')); }
#ifdef USESTRINGS
LVAL iview_plot2d_add_strings()  { return(plot2d_add_data('S')); }
#endif /* USESTRINGS */
