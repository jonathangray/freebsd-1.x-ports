/* xsiview - XLISP interface to IVIEW dynamic graphics package.        */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "iview.h"

# define numberp(x) (floatp(x) || fixp(x))
# define sequencep(x) (listp(x) || simplevectorp(x))
# define seqlen(x) ((listp(x)) ? llength(x) : getsize(x))

/* external functions */
extern LVAL getnextelement();
extern IVIEW_WINDOW get_iview_address();
extern double makedouble();

/* external variables */
extern char buf[];
extern LVAL s_true;
extern LVAL s_solid, sk_point_labels, sk_draw, sk_redraw,
  sk_redraw_background, sk_redraw_content, sk_redraw_overlays,
  sk_resize_overlays, sk_scale, sk_type, sk_color, sk_width, sk_symbol;


/**************************************************************************/
/**                                                                      **/
/**                    General IView Data Functions                      **/
/**                                                                      **/
/**************************************************************************/

static check_locations(data, vars)
	LVAL data;
	int vars;
{
  LVAL seq, x;
  int i, j, n;
  
  if (fixp(data)) {
    if (getfixnum(data) <= 0) xlerror("too few points", data);
    else return(getfixnum(data));
  }
  
  if (! consp(data)/* || llength(data) != vars */)
    xlerror("bad variable list", data);
  
  if (! sequencep(car(data))) xlerror("not a sequence", car(data));
  n = seqlen(car(data));
  
  for (i = 0; i < vars && consp(data); i++, data = cdr(data)) {
    if (! sequencep(car(data))) xlerror("not a sequence", car(data));
    if (seqlen(car(data)) != n) xlfail("sequences of different lengths");
    for (j = 0, seq = car(data); j < n; j++) {
      x = getnextelement(&seq, j);
      if (! numberp(x)) xlerror("not a number", x);
    }
  }
  return(n);
}

static set_locations(w, object, type, data, vars, oldn, n)
    IVIEW_WINDOW w;
	LVAL object, data;
    int type;
	int  vars, oldn, n;
{
  LVAL seq, arg;
  int i, j;
  double x;

  if (fixp(data)) return;
  if (! xlgetkeyarg(sk_scale, &arg)) arg = s_true;
  
  for (i = 0; i < vars && consp(data); i++, data = cdr(data)) {
    for (j = oldn, seq = car(data); j < n; j++) {
      x = makedouble(getnextelement(&seq, j - oldn));
      switch (type) {
      case 'P': IViewSetPointValue(w, i, j, x); break;
      case 'L': IViewSetLineValue(w, i, j, x); break;
#ifdef USESTRINGS
      case 'S': IViewSetStringValue(w, i, j, x); break;
#endif /* USESTRINGS */
      }
    }
  }
}

static check_strings(n, strings)
	int n;
	LVAL strings;
{
  int i;
  LVAL element;
  
  if (! sequencep(strings)) xlerror("not a sequence", strings);
  if (n != seqlen(strings)) xlerror("wrong sequence length", strings);
  for (i = 0; i < n; i++) {
    element = getnextelement(&strings, i);
    if (! stringp(element)) xlerror("not a string", element);
  }
}

static LVAL clear_data(which)
	int which;
{
  IVIEW_WINDOW w;
  LVAL object;
  
  object = xlgaobject();
  w = get_iview_address(object);
  
  switch(which) {
  case 'P': IViewClearPoints(w);  break;
  case 'L': IViewClearLines(w);   break;
#ifdef USESTRINGS
  case 'S': IViewClearStrings(w); break;
#endif /* USESTRINGS */
  }
  check_redraw(object, TRUE, TRUE);
  return(NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                      IView Point Data Functions                      **/
/**                                                                      **/
/**************************************************************************/

internal_iview_add_points(w, object, data)
	IVIEW_WINDOW w;
	LVAL object, data; 
{
  LVAL labels, arg;
  int i, n, oldn, vars, sym, hsym;
  char *str;
  char *gwinfo = IViewWindowWinInfo(w);

  if (! xlgetkeyarg(sk_point_labels, &labels)) labels = NIL;
  
  vars = IViewNumVariables(w);
  oldn = IViewNumPoints(w);
  n = check_locations(data, vars);
  
  IViewAddPoints(w, n);
  n = IViewNumPoints(w);
  set_locations(w, object, 'P', data, vars, oldn, n);
  
  if (labels != NIL) {
    check_strings(n - oldn, labels);
    for (i = oldn; i < n; i++) {
      str = (char *) getstring(getnextelement(&labels, i - oldn));
      IViewSetPointLabel(w, i, str);
    }
  }
  if (xlgetkeyarg(sk_color, &arg)) {
    StGWSetUseColor(gwinfo, TRUE);
    for (i = oldn; i < n; i++)
      IViewSetPointColor(w, i, decode_lisp_color(arg));
  }
  if (xlgetkeyarg(sk_symbol, &arg)) {
    decode_point_symbol(arg, &sym, &hsym);
    for (i = oldn; i < n; i++)
      IViewSetPointSymbol(w, i, sym, hsym);
  }
}

LVAL iview_add_points()
{
  IVIEW_WINDOW w;
  LVAL object, data;
  int old_n, n;
  
  object = xlgaobject();
  w = get_iview_address(object);
  data = xlgetarg();
  
  old_n = IViewNumPoints(w);
  internal_iview_add_points(w, object, data);
  n = IViewNumPoints(w);
  check_add_to_screen(object, 'P', old_n, n, FALSE);
  return(NIL);
}

LVAL iview_clear_points() { return(clear_data('P')); }

/**************************************************************************/
/**                                                                      **/
/**                      IView Line Data Functions                       **/
/**                                                                      **/
/**************************************************************************/

internal_iview_add_lines(w, object, data)
    IVIEW_WINDOW w;
    LVAL object, data;
{
  int i, n, oldn, vars, width;
  char *gwinfo = IViewWindowWinInfo(w);
  LVAL arg;
  
  vars = IViewNumVariables(w);
  oldn = IViewNumLines(w);
  n = check_locations(data, vars);
  
  IViewAddLines(w, n);
  n = IViewNumLines(w);
  set_locations(w, object, 'L', data, vars, oldn, n);

  if (xlgetkeyarg(sk_type, &arg) && arg != s_solid)
    for (i = oldn; i < n; i++) IViewSetLineType(w, i, 1);
  if (xlgetkeyarg(sk_color, &arg)) {
    StGWSetUseColor(gwinfo, TRUE);
    for (i = oldn; i < n; i++)
      IViewSetLineColor(w, i, decode_lisp_color(arg));
  }
  if (xlgetkeyarg(sk_width, &arg) && fixp(arg)) {
    width = getfixnum(arg);
    for (i = oldn; i < n; i++)
      IViewSetLineWidth(w, i, width);
  }
}

LVAL iview_add_lines()
{
  IVIEW_WINDOW w;
  LVAL object, data;
  int n, oldn;
  
  object = xlgaobject();
  w = get_iview_address(object);
  data = xlgetarg();

  oldn = IViewNumLines(w);
  internal_iview_add_lines(w, object, data);
  n = IViewNumLines(w);
  check_add_to_screen(object, 'L', oldn, n, FALSE);
  return(NIL);
}
  
LVAL iview_clear_lines() { return(clear_data('L')); }

#ifdef USESTRINGS
/**************************************************************************/
/**                                                                      **/
/**                     IView String Data Functions                      **/
/**                                                                      **/
/**************************************************************************/

internal_iview_add_strings(w, object, data)
	IVIEW_WINDOW w;
	LVAL object, data;
{
  LVAL strings;
  int i, n, oldn, vars;
  char *str;
  
  strings = xlgetarg();
  
  vars = IViewNumVariables(w);
  oldn = IViewNumStrings(w);
  n = check_locations(data, vars);
  check_strings(n, strings);
  
  IViewAddStrings(w, n);
  n = IViewNumStrings(w);
  set_locations(w, object, 'S', data, vars, oldn, n);
  
  for (i = oldn; i < n; i++) {
    str = (char *) getstring(getnextelement(&strings, i - oldn));
    IViewSetStringString(w, i, str);
  }
}

LVAL iview_add_strings()
{
  IVIEW_WINDOW w;
  LVAL object, data;
  int n, oldn;
  
  object = xlgaobject();
  w = get_iview_address(object);
  data = xlgetarg();

  oldn = IViewNumStrings(w);
  internal_iview_add_strings(w, object, data);
  n = IViewNumStrings(w);
  check_add_to_screen(object, 'S', oldn, n, FALSE);
  return(NIL);
}

LVAL iview_clear_strings() { return(clear_data('S')); }
#endif /* USESTRINGS */

/**************************************************************************/
/**                                                                      **/
/**                     Standard Callback Functions                      **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_std_resize()
{
  IVIEW_WINDOW w;

  w = get_iview_address(xlgaobject());
  IViewStdResize(w);
  return(NIL);
}

LVAL iview_std_redraw()
{
  IVIEW_WINDOW w;

  w = get_iview_address(xlgaobject());
  IViewStdRedraw(w);
  return(NIL);
}

LVAL iview_std_redraw_background()
{
  IVIEW_WINDOW w;

  w = get_iview_address(xlgaobject());
  IViewStdRedrawBackground(w);
  return(NIL);
}

LVAL iview_std_redraw_content()
{
  IVIEW_WINDOW w;

  w = get_iview_address(xlgaobject());
  IViewStdRedrawContent(w);
  return(NIL);
}

IViewRedrawBackground(w)
	IVIEW_WINDOW w;
{
  send_message((LVAL) IViewWindowGetObject(w), sk_redraw_background);
}

IViewRedrawContent(w)
	IVIEW_WINDOW w;
{
  send_message((LVAL) IViewWindowGetObject(w), sk_redraw_content);
}

IViewRedrawOverlays(w)
	IVIEW_WINDOW w;
{
  send_message((LVAL) IViewWindowGetObject(w), sk_redraw_overlays);
}

IViewResizeOverlays(w)
	IVIEW_WINDOW w;
{
  send_message((LVAL) IViewWindowGetObject(w), sk_resize_overlays);
}

/**************************************************************************/
/**                                                                      **/
/**                        Miscellaneous Functions                       **/
/**                                                                      **/
/**************************************************************************/

check_add_to_screen(object, which, old_n, n, redraw)
	LVAL object;
	int old_n, n, redraw;
{
  IVIEW_WINDOW w;
  char *gwinfo;
  int x, y;
  LVAL adjust;
  
  w = get_iview_address(object);
  gwinfo = StGWObWinInfo(object);
  if (! xlgetkeyarg(sk_draw, &adjust)) adjust = s_true;
  
  if (adjust != NIL) {
    if (redraw || old_n == 0) send_message(object, sk_redraw_content);
    else {
      StGrGetContentVariables(gwinfo, &x, &y);
      switch(which) {
      case 'P': IViewDrawDataPoints(w, x, y, old_n, n);   break;
      case 'L': IViewDrawDataLines(w, x, y, old_n, n);    break;
#ifdef USESTRINGS
      case 'S': IViewDrawDataStrings(w, x, y, old_n, n);  break;
#endif /* USESTRINGS */
      }
    }
  }
}

check_redraw(object, deflt, content_only)
	LVAL object;
	int deflt, content_only;
{
  LVAL arg, msg;
  int redraw;
  
  if (xlgetkeyarg(sk_draw, &arg)) redraw = (arg != NIL);
  else redraw = deflt;
  
  msg = (content_only) ? sk_redraw_content : sk_redraw;
  if (redraw) send_message(object, msg);
}

draw_key_arg(deflt)
	int deflt;
{
  int value = deflt, n, i;
  
  for (n = xlargc - 1, i = 0; i < n; i++) {
    if (sk_draw == xlargv[i]) {
	  value = (xlargv[i+1] != NIL) ? TRUE : FALSE;
	}
  }
  return(value);
}
