/* xsscatmat - XLISP interface to IVIEW dynamic graphics package.      */
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

#define SCAT_PLOT_GAP 1
#define SCAT_INSET 4
#define LABEL_OFFSET 5
#define MAX_NUM_VARS 10

#define VAR_FORMAT "var %d"
#define POINT_FORMAT "%d"

extern IVIEW_WINDOW IViewNew(), GETIVIEWADDRESS();
extern char *IViewVariableLabel(), *IViewPointLabel();
extern MouseMode IViewMouseMode();
extern PointState IViewPointState(), IViewPointScreenState();
extern LVAL slot_value();
extern LVAL s_scale_type, sk_show, sk_show_window;
extern PointState decode_point_state();

static int pdata[MAX_NUM_VARS];

static struct {
  int x, y, left, top, size, bottom;
} current;

/**************************************************************************/
/**                                                                      **/
/**                        Plot Creation Functions                       **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_scatmat_allocate()
{  
  LVAL object;
  int i, vars, show;
  IVIEW_WINDOW w;
  char s[100];

  object = xlgaobject();
  show = xsboolkey(sk_show, TRUE);

  get_iview_ivars(object, &vars);
  if (vars < 2) xlfail("Too few variables for scatmat");
  if (vars > MAX_NUM_VARS) xlfail("Too many variables for scatmat");
  
  w = IViewNew(object);
  
  /* should replace this by something lisp-based +++++++ */
  for (i = 0; i < vars; i++) {
    sprintf(s, VAR_FORMAT, i);
    IViewSetVariableLabel(w, i, s);
    IViewSetRange(w, i, 0.0, 1.0);
  }
  
  initialize_iview(w, object);
  /* use StShowWindow to show (map) window but NOT send :resize or :redraw */
  if (show) StShowWindow(w);
  return(object);
}

/**************************************************************************/
/**                                                                      **/
/**                            Data Functions                            **/
/**                                                                      **/
/**************************************************************************/

static LVAL scatmat_add_data(which)
     int which;
{
  IVIEW_WINDOW w;
  LVAL data, object;
  int old_n, n;
  
  object = xlgaobject();
  w = GETIVIEWADDRESS(object);
  if (w == nil) return(NIL);
  data = xlgetarg();

  switch(which) {
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
  
  check_add_to_screen(object, which, old_n, n, TRUE);
  
  return(NIL);
}

LVAL iview_scatmat_add_points()  { return(scatmat_add_data('P')); }
LVAL iview_scatmat_add_lines()   { return(scatmat_add_data('L')); }
#ifdef USESTRINGS
LVAL iview_scatmat_add_strings() { return(scatmat_add_data('S')); }
#endif /* USESTRINGS */
 
/**************************************************************************/
/**                                                                      **/
/**                    Drawing and Resizing Functions                    **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_scatmat_resize()
{
  int vars;
  int i, top, left, subsize, low, high;
  IVIEW_WINDOW w;
  LVAL object;
  
  object = xlgaobject();
  xllastarg();
  
  w = GETIVIEWADDRESS(object);
  if (w == nil) return(NIL);
  
  vars = IViewNumVariables(w);
  IViewSetFixedAspect(w, TRUE);
  IViewStdResize(w);

  get_plot_layout(w, &left, &top, &subsize);

  for (i = 0; i < vars; i++) {
    low = i * (subsize + SCAT_PLOT_GAP) + 2 * SCAT_INSET + SCAT_PLOT_GAP;
    high = low + subsize - 3 * SCAT_INSET;
    IViewSetScreenRange(w, i, low, high);
  }
  return(NIL);
}

LVAL iview_scatmat_redraw_content()
{
  int vars;
  int left, top, subleft, subtop, subsize;
  int viewleft, viewtop, viewwidth, viewheight;
  int i, j;
  double low, high;
  char s[100];
  IVIEW_WINDOW w;
  LVAL object, scale_type;
  char *gwinfo;
  
  object = xlgaobject();
  xllastarg();
  
  gwinfo = StGWObWinInfo(object);
  w = GETIVIEWADDRESS(object);
  if (w == nil) return(NIL);
    
  vars = IViewNumVariables(w);
  if (IViewMouseMode(w) == brushing) IViewEraseBrush(w);
  StGWGetViewRect(gwinfo, &viewleft, &viewtop, &viewwidth, &viewheight);
  get_plot_layout(w, &left, &top, &subsize);
  scale_type = slot_value(object, s_scale_type);

  StGWStartBuffering(gwinfo);
  StGWEraseRect(gwinfo, viewleft, viewtop, viewwidth, viewheight);
  for (i = 0; i < vars; i++) 
    for (j = 0; j < vars; j++) {
      subleft = left + j * (SCAT_PLOT_GAP + subsize);
      subtop = top + (vars - i - 1) * (SCAT_PLOT_GAP + subsize);
      StGWFrameRect(gwinfo, subleft, subtop, subsize, subsize);
      if (i == j && scale_type == NIL) {
        if (IViewVariableLabel(w, i) != 0)
          StGWDrawText(gwinfo, IViewVariableLabel(w, i), 
                       subleft + subsize / 2, subtop + subsize / 2, 1, 0);
        IViewGetRange(w, i, &low, &high);
        sprintf(s, "%.3lg", high);
        StGWDrawText(gwinfo, s, 
		     subleft + subsize - SCAT_INSET,
		     subtop + SCAT_INSET, 2, 1);
        sprintf(s, "%.3lg", low);
        StGWDrawText(gwinfo, s, 
		     subleft + SCAT_INSET,
		     subtop + subsize - SCAT_INSET, 0, 0);
      }
      else if (i != j) {
        IViewDrawDataPoints(w, i, j, 0, IViewNumPoints(w));
        IViewDrawDataLines(w, i, j, 0, IViewNumLines(w));
#ifdef USESTRINGS
        IViewDrawDataStrings(w, i, j, 0, IViewNumStrings(w));
#endif /* USESTRINGS */
      }
    }
  StGWBufferToScreen(gwinfo, viewleft, viewtop, viewwidth, viewheight);
  if (IViewMouseMode(w) == brushing) IViewDrawBrush(w);
  IViewResetScreenStates(w);
  return(NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                           Mouse Functions                            **/
/**                                                                      **/
/**************************************************************************/

extern LVAL peekarg();

static LVAL iview_scatmat_mouse(click)
     int click;
{
  IVIEW_WINDOW w;
  LVAL object;
  int x, y;
  
  object = xlgaobject();
  w = GETIVIEWADDRESS(object);

  if (w != nil) {
    x = fixp(peekarg(0)) ? getfixnum(peekarg(0)) : 0;
    y = fixp(peekarg(1)) ? getfixnum(peekarg(1)) : 0;
    find_current_plot(w, x, y);
    if (click) IViewDoClick(object);
    else IViewDoMotion(object);
  }
  return(NIL);
}

LVAL iview_scatmat_click() { return(iview_scatmat_mouse(TRUE)); }
LVAL iview_scatmat_motion() { return(iview_scatmat_mouse(FALSE)); }

LVAL iview_scatmat_adjust_screen_point()
{
  LVAL object;
  int point;
  IVIEW_WINDOW w;
  PointState state, screen_state;
  
  object = xlgaobject();
  point = getfixnum(xlgafixnum());
  xllastarg();

  w = GETIVIEWADDRESS(object);
  if (w == nil) return(NIL);
  
  if (! IViewPointMasked(w, point)) {
    state = IViewPointState(w, point);
    screen_state = IViewPointScreenState(w, point);
	if (state == pointInvisible || screen_state == pointInvisible) {
	  StGrSetDirty(StGWObWinInfo(object), TRUE);
	}
    else {
      scat_draw_point(w, point, state);
    }
  }
  
  return(NIL);
}

LVAL iview_scatmat_adjust_points_in_rect()
{
  int var1, var2, x, y, px, py;
  int  i, n, in_rect, right, bottom, left, top, width, height;
  PointState point_state, state;
  IVIEW_WINDOW w;
  LVAL object;
  char *gwinfo;
  
  object = xlgaobject();
  w = GETIVIEWADDRESS(object);
  left = getfixnum(xlgafixnum());
  top = getfixnum(xlgafixnum());
  width = getfixnum(xlgafixnum());
  height = getfixnum(xlgafixnum());
  state = decode_point_state(xlgetarg());
  xllastarg();
  
  if (w == nil) return(NIL);
  gwinfo = StGWObWinInfo(object);
  
  IViewCheckLinks(w);
  n = IViewNumPoints(w);
  bottom = top + height;
  right = left + width;
  StGrGetContentVariables(gwinfo, &var1, &var2);
  StGrGetContentOrigin(gwinfo, &x, &y);
  
  if (IViewMouseMode(w) == brushing) IViewEraseBrush(w);

  for (i = 0; i < n; i++) {
    point_state = IViewPointState(w, i);
    if (! IViewPointMasked(w, i) && point_state != pointInvisible) {
      px = x + IViewPointScreenValue(w, var1, i);
      py = y - IViewPointScreenValue(w, var2, i);
      in_rect = (var1 != var2 
		 && px >= left && px <= right && py >= top && py <= bottom);
      if (in_rect && (int) point_state < (int) state) {
        IViewSetPointState(w, i, state);
      }
      else if (! in_rect 
	       && state == pointHilited && point_state == pointHilited) {
        IViewSetPointState(w, i, pointNormal);
      }
    }
  }
  IViewAdjustScreens(w);
  if (IViewMouseMode(w) == brushing) IViewDrawBrush(w);
  
  return(NIL);
}

LVAL iview_scatmat_mark_points_in_rect()
{
  int var1, var2;
  int left, top, width, height;
  IVIEW_WINDOW w;
  LVAL object;
  char *gwinfo;
  
  object = xlgaobject();
  w = GETIVIEWADDRESS(object);
  left = getfixnum(xlgafixnum());
  top = getfixnum(xlgafixnum());
  width = getfixnum(xlgafixnum());
  height = getfixnum(xlgafixnum());
  xllastarg();
  
  if (w == nil) return(NIL);
  gwinfo = StGWObWinInfo(object);
  
  StGrGetContentVariables(gwinfo, &var1, &var2);
  if (var1 == var2)   IViewClearPointMarks(w);
  else IViewStdMarkPointsInRect(w, left, top, width, height);
  return(NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                          Internal Functions                          **/
/**                                                                      **/
/**************************************************************************/

static get_plot_layout(w, subleft, subtop, subsize)
     IVIEW_WINDOW w;
     int *subleft, *subtop, *subsize;
{
  int vars, left, top, width, height, delta;
  char *gwinfo = IViewWindowWinInfo(w);

  vars = IViewNumVariables(w);
  StGrGetContentRect(gwinfo, &left, &top, &width, &height);

  if (subleft != nil && subtop != nil && subsize != nil) {
    *subsize = (width - SCAT_PLOT_GAP * (vars + 1)) / vars;
    if (*subsize < 0) *subsize = 0;

    delta = (width - (vars * *subsize + (vars - 1) * SCAT_PLOT_GAP)) / 2;
    if (delta < 0) delta = 0;

    *subleft = left + delta;
    *subtop = top + delta;
  }
}

static ScatDrawPoint(w, point, state, screen_state)
	IVIEW_WINDOW w;
	int point;
	PointState state, screen_state;
{
  int vars = IViewNumVariables(w);
  int left, bottom;
  int oldwidth, oldheight, newwidth, newheight;
  int i, j;
  int x, y, oldsym, newsym, sym, hsym, replace;
  char *gwinfo = IViewWindowWinInfo(w);
  int color, oldcolor, use_color = StGWUseColor(gwinfo);

  IViewGetPointSymbol(w, point, &sym, &hsym);
  oldsym = (screen_state == pointNormal) ? sym : hsym;
  newsym = (state == pointNormal) ? sym : hsym;
  if (state == pointInvisible) return;
  
  StGWGetSymbolSize(oldsym, &oldwidth, &oldheight);
  StGWGetSymbolSize(newsym, &newwidth, &newheight);
  replace = (oldwidth > newwidth || oldheight > newheight);

  IViewGetScreenPointValues(w, point, pdata);

  StGrGetContentOrigin(gwinfo, &left, &bottom);
  if (use_color) {
    oldcolor = StGWDrawColor(gwinfo);
    color = IViewPointColor(w, point);
    if (color >= 0) StGWSetDrawColor(gwinfo, color);
  }
  for (i = 0; i < vars; i++) {
    y = bottom - pdata[i];
    for (j = 0; j < vars; j++)
      if (i != j) {
        x = left + pdata[j];
        if (replace) StGWReplaceSymbol(gwinfo, oldsym, newsym, x, y);
        else StGWDrawSymbol(gwinfo, newsym, x, y);
      }
  }
  if (use_color && color >= 0) StGWSetDrawColor(gwinfo, oldcolor);
}

static DrawLabel(w, point)
	IVIEW_WINDOW w;
	int point;
{
  int vars = IViewNumVariables(w);
  int left, bottom;
  int i, j;
  int x, y;
  char *label;
  char *gwinfo = IViewWindowWinInfo(w);
  int mode = StGWDrawMode(gwinfo);

  label = IViewPointLabel(w, point);
  if (label == nil) return;
    
  for (i = 0; i < vars; i++) pdata[i] = IViewPointScreenValue(w, i, point);
  StGrGetContentOrigin(gwinfo, &left, &bottom);
  for (i = 0; i < vars; i++) {
    y = bottom - pdata[i] - LABEL_OFFSET;
    for (j = 0; j < vars; j++)
      if (i != j) {
        x = left + pdata[j] + LABEL_OFFSET;
        StGWSetDrawMode(gwinfo, 1);
        StGWDrawString(gwinfo, label, x, y);
        StGWSetDrawMode(gwinfo, mode);
      }
  }
}

static find_current_plot(w, x, y)
     IVIEW_WINDOW w;
     int x, y;
{
  int width, height;
  int left, top, subsize, vars;
  char *gwinfo = IViewWindowWinInfo(w);
  
  vars = IViewNumVariables(w);
  
  if (IViewMouseMode(w) == brushing) {
    IViewGetBrush(w, nil, nil, &width, &height);
    x -= width / 2;
    y -= height / 2;
  }
  
  get_plot_layout(w, &left, &top, &subsize);
  subsize += SCAT_PLOT_GAP;
  current.x = (x - left) / subsize;
  current.y = (top + vars * subsize - y - SCAT_PLOT_GAP) / subsize;
  if (current.x < 0 || current.x >= vars 
      || current.y < 0 || current.y >= vars) {
    current.x = 0;
    current.y = 0;
  }
  current.size = vars * subsize - SCAT_PLOT_GAP;
  current.left = left;
  current.top = top;
  current.bottom = current.left + current.top + current.size;
  StGrSetContentVariables(gwinfo, current.x, current.y);
}

static scat_draw_point(w, i, state)
	IVIEW_WINDOW w;
	int i;
	PointState state;
{
  int showingLabels = IViewShowingLabels(w);
  
  if (state == pointNormal && showingLabels) DrawLabel(w, i); /* to erase */
  ScatDrawPoint(w, i, state, state);
  if (state != pointNormal && showingLabels) DrawLabel(w, i); /* to draw */
  IViewSetPointScreenState(w, i, state);
}

