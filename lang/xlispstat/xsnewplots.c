/* xsnewplots - XLISP interface to IVIEW dynamic graphics package.     */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xmath.h"
#include "xlisp.h"
#include "iview.h"

# define numberp(x) (floatp(x) || fixp(x))
# define sequencep(x) (listp(x) || simplevectorp(x))
# define seqlen(x) ((listp(x)) ? llength(x) : getsize(x))

/* external functions */
extern LVAL makearglist(), list2(), slot_value(), apply_send();
extern IVIEW_WINDOW GETIVIEWADDRESS();
extern double IViewScale(), IViewShift();

/* external variables */
extern LVAL s_true, sk_new, s_histogram_proto, sk_add_points, sk_add_lines,
  sk_draw, s_scatterplot_proto, sk_adjust_to_data, s_spin_proto,
  s_scatmat_proto, s_name_list_proto, sk_point_labels, s_fixed, s_variable,
  sk_resize, sk_redraw, s_scale_type, sk_show, sk_show_window;

static set_scale_shift(w, var, scale, shift)
     IVIEW_WINDOW w;
     int var;
     double scale, shift;
{
  double old_scale, old_shift;

  old_scale = IViewScale(w, var);
  old_shift = IViewShift(w, var);
  if (scale != 0.0 && old_scale != 0.0) {
    scale = scale / old_scale;
    shift = shift - scale * old_shift;
    IViewApplyScaleShift(w, var, scale, shift);
  }
}

StGrObAdjustToData(object, draw)
     LVAL object;
     int draw;
{
  IVIEW_WINDOW w;
  double low, high, range, center;
  int i, vars;
  LVAL scale_type;
  
  w = GETIVIEWADDRESS(object);
  if (w != nil) {
    scale_type = slot_value(object, s_scale_type);
    vars = IViewNumVariables(w);
    high = 1.0; low = -high;
    if (scale_type == s_variable) {
      high = sqrt((double) vars); low = - high;
      for (i = 0; i < vars; i++) {
	IViewScaleToRange(w, i, -1.0, 1.0);
	IViewSetScaledRange(w, i, low, high);
      }
    }
    else if (scale_type == s_fixed) {
      if (vars > 0) {
	IViewGetVisibleRange(w, 0, &low, &high);
	set_scale_shift(w, 0, 1.0, -(high + low) / 2.0);
	range = high - low;
	for (i = 1; i < vars; i++) {
	  IViewGetVisibleRange(w, i, &low, &high);
	  set_scale_shift(w, i, 1.0, -(high + low) / 2.0);
	  if (high - low > range) range = high - low;
	}
	range = sqrt((double) vars) * range / 2;
	for (i = 0; i < vars; i++) {
	  center = -IViewShift(w, i);
	  IViewSetRange(w, i, center - range, center + range);
	}
      }
    }
    else {
      for (i = 0; i < vars; i++) {
	IViewApplyScaleShift(w, i, 1.0, 0.0);
	IViewGetVisibleRange(w, i, &low, &high);
	IViewSetRange(w, i, low, high);
      }
    }
    if (draw) {
      send_message(object, sk_resize);
      send_message(object, sk_redraw);
    }
  }
}

LVAL iview_adjust_to_data()
{
  LVAL object;
  LVAL arg;
  int draw;

  object = xlgaobject();
  if (! xlgetkeyarg(sk_draw, &arg)) arg = s_true;
  draw = (arg != NIL) ? TRUE : FALSE;
  StGrObAdjustToData(object, draw);
  return(NIL);
}

static LVAL make_iview_object(which, vars, rest)
	int which, vars;
	LVAL rest;
{
  LVAL proto, object, args;
  
  switch (which) {
  case 'H': proto = getvalue(s_histogram_proto); break;
  case 'P': 
  case 'L': proto = getvalue(s_scatterplot_proto); break;
  case 'R': proto = getvalue(s_spin_proto); break;
  case 'S': proto = getvalue(s_scatmat_proto); break;
  case 'N': proto = getvalue(s_name_list_proto); break;
  default:  xlfail("unknown iview proto");
  }
  
  xlsave1(args);
  args = cons(NIL, rest);
  args = cons(sk_show, args);
  /* cons protects its arguments, so the new fixnum should be safe */
  args = cons(cvfixnum((FIXTYPE) vars), args);
  object = apply_send(proto, sk_new, args);
  xlpop();
  return(object);
}

static get_data(which, data, vars, rest, show)
	int which, *vars, *show;
	LVAL *data, *rest;
{
  LVAL x, y;
  int n;
  
  if (data == nil || vars == nil) return;
  
  switch (which) {
  case 'H':
    *data = xlgetarg();
    *vars = (consp(*data) && sequencep(car(*data))) ? seqlen(*data) : 1;
	*show = xsboolkey(sk_show, TRUE);
    *rest = makearglist(xlargc, xlargv);
    break;
  case 'P':
  case 'L':
    x = xlgetarg();
    if (consp(x) && sequencep(car(x))) *data = x;
    else {
      y = xlgetarg();
      *data = list2(x, y);
    }
    *vars = (consp(*data) && sequencep(car(*data))) ? seqlen(*data) : 1;
	*show = xsboolkey(sk_show, TRUE);
    *rest = makearglist(xlargc, xlargv);
    break;
  case 'R':
  case 'S':
    *data = xlgalist();
    *vars = seqlen(*data);
	*show = xsboolkey(sk_show, TRUE);
    *rest = makearglist(xlargc, xlargv);
    break;
  case 'N':
    *vars = 0;
    *data = xlgetarg();
	*show = xsboolkey(sk_show, TRUE);
    *rest = makearglist(xlargc, xlargv);
    if (! numberp(*data)) {
      n = seqlen(*data);
      *rest = cons(*data, *rest);
      *rest = cons(sk_point_labels, *rest);
      *data = cvfixnum((FIXTYPE) n);
    }
    break;
  default:  xlfail("unknown iview proto");
  }    
}

static check_data(which, data)
	int which;
	LVAL data;
{
  switch (which) {
  case 'H': break;
  case 'P':
  case 'L':
  case 'R':
  case 'S': 
    if (! consp(data)) xlerror("not a list of sequences", data);
    for (; consp(data); data = cdr(data)) 
      if (! sequencep(car(data))) xlerror("not a sequence", car(data));
    break;
  case 'N': break;
  default:  xlfail("unknown iview proto");
  }
}

static add_data(which, object, data, rest)
	int which;
	LVAL object, data, rest;
{
  LVAL args, message;
  
  xlsave1(args);
  args = cons(NIL, rest);
  args = cons(sk_draw, args);
  args = cons(data, args);

  switch (which) {
  case 'H':
  case 'P': 
  case 'R': 
  case 'S':
  case 'N': message = sk_add_points; break;
  case 'L': message = sk_add_lines;  break;
  default:  xlfail("unknown iview proto");
  }
  
  apply_send(object, message, args);
  xlpop();
}
  
static adjust_plot_to_data(object, rest)
	LVAL object, rest;
{
  LVAL args;
  
  xlsave1(args);
  args = cons(NIL, rest);
  args = cons(sk_draw, args);
  apply_send(object, sk_adjust_to_data, args);
  xlpop();
}

static LVAL newplot(which)
	int which;
{
  int vars, show;
  LVAL object, data, rest, args;
  
  if (! StHasWindows()) xlfail("not available without windows");

  xlstkcheck(4);
  xlsave(object);
  xlsave(data);
  xlsave(args);
  xlsave(rest);
  
  get_data(which, &data, &vars, &rest, &show);
  check_data(which, data);
  object = make_iview_object(which, vars, rest);
  add_data(which, object, data, rest);
  adjust_plot_to_data(object, rest);
  
  xlpopn(4);
  
  if (show) send_message(object, sk_show_window);
  
  return(object);
}

LVAL xshistogram()          { return(newplot('H')); }
LVAL xsplot_points()        { return(newplot('P')); }
LVAL xsplot_lines()         { return(newplot('L')); }
LVAL xsspin_plot()          { return(newplot('R')); }
LVAL xsscatterplot_matrix() { return(newplot('S')); }
LVAL xsnamelist()           { return(newplot('N')); }
