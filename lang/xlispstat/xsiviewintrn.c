/* xsiviewinter - XLISP interface to IVIEW dynamic graphics package.   */
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
extern LVAL list2(), slot_value(), getnextelement(), xscallsubr2(),
  coerce_to_vector(), integer_list_2(), integer_list_4(), xmsend(),
  xscallsubrvec();
extern LVAL send_message_stk(), peekarg(), mklist(), send_message();
extern IVIEW_WINDOW get_iview_address(), GETIVIEWADDRESS();
extern double makedouble();
extern MouseMode IViewMouseMode();
extern PointState IViewPointState();

/* external variables */
extern char buf[];
extern LVAL s_true, sk_adjust_points_in_rect, sk_unselect_all_points;

/* symbol pointers */
extern LVAL s_selecting, s_brushing, s_user, sk_mark_points_in_rect,
  sk_adjust_screen, s_number_of_variables, sk_variable_labels,
  s_variable_labels, sk_allocate, sk_showing_labels, s_showing_labels,
  s_fixed_aspect, s_mouse_mode, sk_overlay_click, sk_new_menu,
  s_invisible, s_normal, s_hilited, s_selected, sk_show, sk_show_window,
  sk_adjust_screen_point, s_hardware_address, s_mode_list, s_scale_type,
  sk_scale, s_fixed, s_variable, sk_draw, sk_resize, sk_redraw;

/**************************************************************************/
/**                                                                      **/
/**                       IView Creation Functions                       **/
/**                                                                      **/
/**************************************************************************/

/* :ISNEW message for GRAPH-PROTO */
LVAL iview_isnew()
{
  LVAL object, vars, args;
  int show;
  
  object = xlgaobject();
  vars = xlgafixnum();
  if (getfixnum(vars) < 0) xlerror("not a nonnegative integer", vars);
  else set_slot_value(object, s_number_of_variables, vars);
  show = (xsboolkey(sk_show, TRUE)) ? TRUE : FALSE;

  object_isnew(object);
  get_iview_keys(object);
  initialize_graph(object);
  send_message(object, sk_new_menu);

  xlsave1(args);
  if (show) args = consa(s_true);
  else args = consa(NIL);
  args = cons(sk_show, args);
  apply_send(object, sk_allocate, args);
  xlpop();
  
  return(object);
}

initialize_graph(object)
	LVAL object;
{
  initialize_graph_window(object);
}

/* :ALLOCATE message for GRAPH-PROTO */
LVAL iview_allocate()
{
  LVAL object;
  IVIEW_WINDOW w;
  int show;
  
  object = xlgaobject();
  show = xsboolkey(sk_show, TRUE);

  w = IViewNew(object);
  initialize_iview(w, object);
  /* use StShowWindow to show (map) window but NOT send :resize or :redraw */
  if (show) StShowWindow(w);
  
  return(object);
}

get_iview_keys(object)
	LVAL object;
{
  LVAL arg;
  
  if (xlgetkeyarg(sk_variable_labels, &arg))
    set_slot_value(object, s_variable_labels, coerce_to_vector(arg));
  if (xlgetkeyarg(sk_scale, &arg)) {
    if (arg != s_fixed && arg != s_variable) arg = NIL;
    set_slot_value(object, s_scale_type, arg);
  }
}

#ifdef OLDLINKS
IViewAdjustOwnScreen(w)
     IVIEW_WINDOW w;
{
  LVAL object = (LVAL) IViewWindowGetObject(w);
  
  if (mobject_p(object))
    send_message(object, sk_adjust_screen);
}
#endif /* OLDLINKS */

IViewMarkPointsInRect(w, left, top, width, height)
	IVIEW_WINDOW w;
	int left, top, width, height;
{
  LVAL argv[6], Lleft, Ltop, Lwidth, Lheight;
  
  xlstkcheck(4);
  xlsave(Lleft);
  xlsave(Ltop);
  xlsave(Lwidth);
  xlsave(Lheight);
  argv[0] = (LVAL) IViewWindowGetObject(w);
  argv[1] = sk_mark_points_in_rect;
  argv[2] = Lleft = cvfixnum((FIXTYPE) left);
  argv[3] = Ltop = cvfixnum((FIXTYPE) top);
  argv[4] = Lwidth = cvfixnum((FIXTYPE) width);
  argv[5] = Lheight = cvfixnum((FIXTYPE) height);
  xscallsubrvec(xmsend, 6, argv);
  xlpopn(4);
}

LVAL encode_point_state(state)
	PointState state;
{
  switch (state) {
  case pointInvisible: return(s_invisible);
  case pointNormal:    return(s_normal);
  case pointHilited:   return(s_hilited);
  case pointSelected:  return(s_selected);
  default: xlfail("unknown point state");
  }
}

IViewAdjustPointsInRect(w, left, top, width, height, state)
	IVIEW_WINDOW w;
	int left, top, width, height;
	PointState state;
{
  LVAL argv[7], Lleft, Ltop, Lwidth, Lheight;
  
  xlstkcheck(4);
  xlsave(Lleft);
  xlsave(Ltop);
  xlsave(Lwidth);
  xlsave(Lheight);
  argv[0] = (LVAL) IViewWindowGetObject(w);
  argv[1] = sk_adjust_points_in_rect;
  argv[2] = Lleft = cvfixnum((FIXTYPE) left);
  argv[3] = Ltop = cvfixnum((FIXTYPE) top);
  argv[4] = Lwidth = cvfixnum((FIXTYPE) width);
  argv[5] = Lheight = cvfixnum((FIXTYPE) height);
  argv[6] = encode_point_state(state);
  xscallsubrvec(xmsend, 7, argv);
  xlpopn(4);
}

IViewAdjustOwnScreenPoint(w, point)
	IVIEW_WINDOW w;
	int point;
{
  LVAL object = (LVAL) IViewWindowGetObject(w);

  if (mobject_p(object))
    send_message1(object, sk_adjust_screen_point, point);
}

IViewUnselectAllPoints(w)
	IVIEW_WINDOW w;
{
  LVAL object = (LVAL) IViewWindowGetObject(w);

  if (mobject_p(object))
    send_message(object, sk_unselect_all_points);
}

initialize_iview(w, object)
	IVIEW_WINDOW w;
	LVAL object;
{
  int i, vars, len;
  LVAL labels;
  MouseMode mode;
  
  labels = slot_value(object, s_variable_labels);
  if (vectorp(labels)) {
    len = getsize(labels);
    vars = IViewNumVariables(w);
    for (i = 0; i < len && i < vars; i++) {
      if (stringp(getelement(labels, i)))
        IViewSetVariableLabel(w, i, (char *) getstring(getelement(labels, i)));
    }
  }
  else set_slot_value(object, s_variable_labels, NIL);
  
  if (slot_value(object, s_showing_labels) != NIL)
    IViewSetShowingLabels(w, TRUE);

  IViewSetFixedAspect(w, (slot_value(object, s_fixed_aspect) != NIL));

  if (slot_value(object, s_mouse_mode) == s_brushing) mode = brushing;
  else if (slot_value(object, s_mouse_mode) == s_selecting) mode = selecting;
  else mode = usermode;
  IViewSetMouseMode(w, mode);
}

get_iview_ivars(object, vars)
	LVAL object;
	int *vars;
{
  LVAL num_variables;
  
  num_variables = slot_value(object, s_number_of_variables);
  if (! fixp(num_variables) || getfixnum(num_variables) < 0)
	xlerror("number of variables is not a nonnegative integer", num_variables);
  *vars = getfixnum(num_variables);
}

/* :SHOW-WINDOW message for GRAPH-PROTO */
LVAL iview_window_show_window()
{
  LVAL object = xlgaobject();
  IVIEW_WINDOW w = GETWINDOWADDRESS(object);
  char *gwinfo = StGWObWinInfo(object);

  if (w == nil) send_message(object, sk_allocate);
  StGWShowWindow(gwinfo);
  return(NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                 IView State Accessors and Mutators                   **/
/**                                                                      **/
/**************************************************************************/

static LVAL state_access(which)
	int which;
{
  char *gwinfo;
  LVAL list, arg, object;
  int a, b, c, d, set = FALSE;
  
  object = xlgaobject();
  gwinfo = StGWObWinInfo(object);
  if (moreargs()) {
    set = TRUE;
    a = getfixnum(xlgafixnum());
    b = getfixnum(xlgafixnum());
    if (which == 'R' || which == 'M') {
      c = getfixnum(xlgafixnum());
      d = getfixnum(xlgafixnum());
    }
  }
  
  if (set)
    switch (which) {
    case 'R': StGrSetContentRect(gwinfo, a, b, c, d); break;
    case 'O': StGrSetContentOrigin(gwinfo, a, b);     break;
    case 'V': StGrSetContentVariables(gwinfo, a, b);  break;
    case 'C': StGrSetClickRange(gwinfo, a, b);        break;
    case 'M': 
      StGrSetMargin(gwinfo, a, b, c, d);
      if (! xlgetkeyarg(sk_draw, &arg)) arg = s_true;
      if (arg != NIL) send_message(object, sk_resize);
      if (arg != NIL) send_message(object, sk_redraw);  
      break;
    }
    
  switch (which) {
  case 'R': StGrGetContentRect(gwinfo, &a, &b, &c, &d); break;
  case 'O': StGrGetContentOrigin(gwinfo, &a, &b);       break;
  case 'V': StGrGetContentVariables(gwinfo, &a, &b);    break;
  case 'C': StGrGetClickRange(gwinfo, &a, &b);          break;
  case 'M': StGrGetMargin(gwinfo, &a, &b, &c, &d);      break;
  }
  
  if (which == 'R' || which == 'M') list = integer_list_4(a, b, c, d);
  else list = integer_list_2(a, b);
  
  return(list);
}

LVAL iview_content_rect()      { return(state_access('R')); }
LVAL iview_content_origin()    { return(state_access('O')); }
LVAL iview_content_variables() { return(state_access('V')); }
LVAL iview_click_range()       { return(state_access('C')); }

LVAL iview_mouse_mode()
{
  IVIEW_WINDOW w;
  LVAL modesym, object;
  MouseMode mode;
  int set = FALSE;
  
  object = xlgaobject();
  w = get_iview_address(object);
  if (moreargs()) {
    set = TRUE;
    modesym = xlgasymbol();
  }
  xllastarg();
  
  if (set) {
    if (modesym == s_selecting) mode = selecting;
    else if (modesym == s_brushing) mode = brushing;
    else mode = usermode;
    set_slot_value(object, s_mouse_mode, modesym);
    IViewSetMouseMode(w, mode);
    set_mode_cursor(object);
  }
  
  switch (IViewMouseMode(w)) {
  case selecting: modesym = s_selecting; break;
  case brushing:  modesym = s_brushing;  break;
  default: modesym = slot_value(object, s_mouse_mode);
  }
  
  return(modesym);
}

LVAL iview_showing_labels()
{
  IVIEW_WINDOW w;
  int set = FALSE, show;
  LVAL object;
  
  object = xlgaobject();
  w = get_iview_address(object);
  if (moreargs()) {
    set = TRUE;
    show = (xlgetarg() != NIL) ? TRUE : FALSE;
  }
  xllastarg();
  
  if (set)  IViewSetShowingLabels(w, show);
  return((IViewShowingLabels(w)) ? s_true : NIL);
}

LVAL iview_margin()      { return(state_access('M')); }

LVAL iview_fixed_aspect()
{
  IVIEW_WINDOW w;
  LVAL object;
  int set = FALSE, fixed;
  
  object = xlgaobject();
  if (moreargs()) {
    set = TRUE;
    fixed = (xlgetarg() != NIL) ? TRUE : FALSE;
  }
  xllastarg();
  w = GETIVIEWADDRESS(object);
  
  if (w != nil) {
    if (set) {
      set_slot_value(object, s_fixed_aspect, (fixed) ? s_true : NIL);
      IViewSetFixedAspect(w, fixed);
      StGWObResize(object);
      StGWObRedraw(object);
    }
    return((IViewFixedAspect(w)) ? s_true : NIL);
  }
  else {
    if (set) set_slot_value(object, s_fixed_aspect, (fixed) ? s_true : NIL);
    return(slot_value(object, s_fixed_aspect));
  }
}

LVAL iview_dirty()
{
  char *gwinfo;
  LVAL object;
  int set;
  
  object = xlgaobject();
  set = moreargs();
  gwinfo = StGWObWinInfo(object);
  if (set) StGrSetDirty(gwinfo, (xlgetarg() != NIL) ? TRUE : FALSE);
  return(StGrDirty(gwinfo) ? s_true : NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                            Axis Functions                            **/
/**                                                                      **/
/**************************************************************************/

static LVAL iview_axis(which)
	int which;
{
  IVIEW_WINDOW w;
  int showing, labeled, ticks, set = FALSE, draw;
  LVAL object, temp, result;
  
  object = xlgaobject();
  w = get_iview_address(object);
  if (moreargs()) {
    set = TRUE;
    showing = (xlgetarg() != NIL) ? TRUE : FALSE;
    labeled = (moreargs() && xlgetarg() != NIL) ? TRUE : FALSE;
    ticks = (moreargs()) ? getfixnum(xlgafixnum()) : 4;
  }
  
  if (set) {
    switch (which) {
    case 'X': IViewSetXaxis(w, showing, labeled, ticks); break;
    case 'Y': IViewSetYaxis(w, showing, labeled, ticks); break;
    }
    draw = draw_key_arg(TRUE);
    StGWObResize(object);
    check_redraw(object, draw, FALSE);
  }
  
  switch(which) {
  case 'X': IViewGetXaxis(w, &showing, &labeled, &ticks); break;
  case 'Y': IViewGetYaxis(w, &showing, &labeled, &ticks); break;
  }
  
  xlstkcheck(2);
  xlsave(result);
  xlsave(temp);
  temp = cvfixnum((FIXTYPE) ticks); result = consa(temp);
  temp = (labeled) ? s_true : NIL; result = cons(temp, result);
  temp = (showing) ? s_true : NIL; result = cons(temp, result);
  xlpopn(2);

  return(result);
}

LVAL iview_x_axis() { return(iview_axis('X')); }
LVAL iview_y_axis() { return(iview_axis('Y')); }

/**************************************************************************/
/**                                                                      **/
/**                           Brush Functions                            **/
/**                                                                      **/
/**************************************************************************/

static LVAL brush(which)
	int which;
{
  IVIEW_WINDOW w;
  int x, y, width, height, set = FALSE;
  LVAL result;
  
  w = get_iview_address(xlgaobject());
  if (which == 'B' && moreargs()) {
    set = TRUE;
    x = getfixnum(xlgafixnum());
    y = getfixnum(xlgafixnum());
    width = getfixnum(xlgafixnum());
    height = getfixnum(xlgafixnum());
  }
  else if (which == 'M') {
    x = getfixnum(xlgafixnum());
    y = getfixnum(xlgafixnum());
  }
  xllastarg();
  
  if (set) IViewSetBrush(w, x, y, width, height);
  
  switch (which) {
  case 'B': IViewGetBrush(w, &x, &y, &width, &height); break;
  case 'E': IViewEraseBrush(w); break;
  case 'D': IViewDrawBrush(w); break;
  case 'M': IViewMoveBrush(w, x, y); break;
  }
  
  if (which == 'B') result = integer_list_4(x, y, width, height);
  else result = NIL;
  
  return(result);
}

LVAL iview_brush()       { return(brush('B')); }
LVAL iview_erase_brush() { return(brush('E')); }
LVAL iview_draw_brush()  { return(brush('D')); }
LVAL iview_move_brush()  { return(brush('M')); }

LVAL iview_resize_brush()
{
  IVIEW_WINDOW w;
  int x, y, width, height, new_width, new_height, changed;
  
  w = get_iview_address(xlgaobject());
  changed = IViewGetNewBrushSize(w, &new_width, &new_height);
  if (changed) {
    IViewGetBrush(w, &x, &y, &width, &height);
    IViewSetBrush(w, x, y, new_width, new_height);
  }
  return((changed) ? s_true : NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                      Mouse Action Functions                          **/
/**                                                                      **/
/**************************************************************************/

static LVAL get_mouse_selector(object, type)
     LVAL object;
     MouseEventType type;
{
  LVAL selector, mode, list, entry;
  
  mode = slot_value(object, s_mouse_mode);
  list = slot_value(object, s_mode_list);
  for (selector = NIL; consp(list); list = cdr(list)) {
    entry = car(list);
    if (consp(entry) && car(entry) == mode) {
      if (type == MouseClick) {
	if (llength(entry) >= 4) selector = car(cdr(cdr(cdr(entry))));
      }
      else {
	if (llength(entry) >= 5) selector = car(cdr(cdr(cdr(cdr(entry)))));
      }
      break;
    }
  }
  return(selector);
}
  
IViewDoClick(object)
     LVAL object;
{
  LVAL selector = get_mouse_selector(object, MouseClick);

  if (! OverlayMouse(object) && symbolp(selector))
    send_message_stk(object, selector);
}

IViewDoMotion(object)
     LVAL object;
{
  LVAL selector = get_mouse_selector(object, MouseMove);

  if (symbolp(selector))
    send_message_stk(object, selector);
}

static OverlayMouse(object)
	LVAL object;
{
  LVAL argv[6], result;
  int i;

  argv[0] = object;
  argv[1] = sk_overlay_click;
  for (i = 0; i < 4 && i < xlargc; i++)
    argv[i + 2] = xlargv[i];
  result = xscallsubrvec(xmsend, 6, argv);
  return(result != NIL);  
}

LVAL iview_do_click()
{
  LVAL object;
  
  object = xlgaobject();
  IViewDoClick(object);
  return(NIL);
}

LVAL iview_do_motion()
{
  LVAL object;
  
  object = xlgaobject();
  IViewDoMotion(object);
  return(NIL);
}

static LVAL iview_std_mouse(type)
     MouseEventType type;
{
  IVIEW_WINDOW w;
  int x, y;
  MouseClickModifier mods;
  
  w = get_iview_address(xlgaobject());
  x = getfixnum(xlgafixnum());
  y = getfixnum(xlgafixnum());
  if (type == MouseClick) {
    mods = (MouseClickModifier) ((xlgetarg() != NIL) ? 1 : 0);
    if (xlgetarg() != NIL) mods = (MouseClickModifier) (((int) mods) + 2);
  }
  xllastarg();
  
  IViewStdMouseAction(w, x, y, type, mods);
  return(NIL);
}

LVAL iview_std_click()  { return(iview_std_mouse(MouseClick)); }
LVAL iview_std_motion() { return(iview_std_mouse(MouseMove));  }

static LVAL multi_state_set(which)
	int which;
{
  IVIEW_WINDOW w;
  int result = FALSE;
  
  w = get_iview_address(xlgaobject());
  xllastarg();
  
  switch (which) {
  case 'U': IViewStdUnselectAllPoints(w); break;
  case 'E': IViewEraseSelection(w);               break;
  case 'M': IViewMaskSelection(w);                break;
  case 'u': IViewUnmaskAllPoints(w);              break;
  case 'S': IViewShowAllPoints(w);                break;
  case 'A': result = IViewAllPointsShowing(w);    break;
  case 'a': result = IViewAllPointsUnmasked(w);   break;
  case 's': result = IViewAnyPointsSelected(w);   break;
  }
  return((result) ? s_true : NIL);
}

LVAL iview_unselect_all_points() { return(multi_state_set('U')); }
LVAL iview_erase_selection()     { return(multi_state_set('E')); }
LVAL iview_mask_selection()      { return(multi_state_set('M')); }
LVAL iview_unmask_all_points()   { return(multi_state_set('u')); }
LVAL iview_show_all_points()     { return(multi_state_set('S')); }
LVAL iview_all_points_showing()  { return(multi_state_set('A')); }
LVAL iview_all_points_unmasked() { return(multi_state_set('a')); }
LVAL iview_any_points_selected() { return(multi_state_set('s')); }

/*************************************************************************/
/**                                                                     **/
/**                      IView Linking Functions                        **/
/**                                                                     **/
/*************************************************************************/

#ifdef OLDLINKS
LVAL iview_linked()
{
  IVIEW_WINDOW w;
  int set = FALSE, linked;
  
  w = get_iview_address(xlgaobject());
  if (moreargs()) {
    set = TRUE;
    linked = (xlgetarg() != NIL) ? TRUE : FALSE;
  }
  xllastarg();
  
  if (set) {
    if (linked) IViewLinkWindow(w);
    else IViewUnlinkWindow(w);
  }
  return(IViewIsLinked(w) ? s_true : NIL);
}

LVAL iview_unlink_all_windows()
{
  xllastarg();
  IViewUnlinkAllWindows();
  return(NIL);
}
#else
extern LVAL s_linked_plots, sk_links, sk_linked;
extern LVAL xremove();
extern long IViewGetLinks();

IViewInternalIsLinked(w)
     IVIEW_WINDOW w;
{
  return (IViewIsLinked(w));
}

LVAL iview_linked()
{
  LVAL object, temp;
  IVIEW_WINDOW w;
  int set = FALSE, linked, i, n;
  
  object = xlgaobject();
  w = get_iview_address(object);
  if (moreargs()) {
    set = TRUE;
    linked = (xlgetarg() != NIL) ? TRUE : FALSE;
  }
  xllastarg();
  
  if (set) {
    if (linked) {
	  setvalue(s_linked_plots, cons(object, getvalue(s_linked_plots)));
      IViewCheckLinks(w);
	  n = IViewNumPoints(w);
      for (i = 0; i < n; i++) IViewMatchPointState(w, i);
      IViewAdjustScreens(w);
	}
    else {
      temp = xscallsubr2(xremove, object, getvalue(s_linked_plots));
      setvalue(s_linked_plots, temp);
      IViewCheckLinks(w);
	}
  }
  else IViewCheckLinks(w);
  
  return(IViewIsLinked(w) ? s_true : NIL);
}

LVAL iview_links()
{
  LVAL object, links, next;
  
  object = xlgaobject();
  links = getvalue(s_linked_plots);
  for (next = links; consp(next); next = cdr(next))
    if (object == car(next)) return(links);
  return(NIL);
}

IViewUnlinkWindow(w)
     IVIEW_WINDOW w;
{
  LVAL object = (LVAL) IViewWindowGetObject(w);
  
  if (mobject_p(object)) send_message_1L(object, sk_linked, NIL);
  IViewCheckLinks(w);
}

LVAL iview_unlink_all_windows()
{
  LVAL links = getvalue(s_linked_plots);
  
  xllastarg();
  for (; consp(links); links = cdr(links))
    send_message_1L(car(links), sk_linked, NIL);
  return(NIL);
}

IViewMatchPointState(w, p)
     IVIEW_WINDOW w;
     unsigned p;
{
  IVIEW_WINDOW lw;
  LVAL links, object;
  
  for (links = (LVAL) IViewGetLinks(w); consp(links); links = cdr(links)) {
    object = car(links);
    lw = GETIVIEWADDRESS(object);
    if (w != lw && IViewPointState(w, p) != IViewPointState(lw, p)) {
	  IViewSetPointScreenState(lw, p, IViewPointState(lw, p));
      IViewDataSetPointState(IViewData(lw), p, IViewPointState(w, p));
      send_message1(object, sk_adjust_screen_point, p);
	  IViewSetPointScreenState(lw, p, IViewPointState(lw, p));
	}
  }
}

IViewAdjustScreens(w)
	IVIEW_WINDOW w;
{
  LVAL links, object;

  object = (LVAL) IViewWindowGetObject(w);
  if (mobject_p(object)) send_message(object, sk_adjust_screen);
  if (IViewIsLinked(w)) {
    for (links = (LVAL) IViewGetLinks(w); consp(links); links = cdr(links)) {
      object = car(links);
      if (mobject_p(object)) send_message(object, sk_adjust_screen);
	}
  }
}

IViewCheckLinks(w)
  IVIEW_WINDOW w;
{
  LVAL links;

  links = send_message(IViewWindowGetObject(w), sk_links);
  IViewSetLinks(w, links);
}
#endif OLDLINKS

/*************************************************************************/
/**                                                                     **/
/**                      Miscellaneous Functions                        **/
/**                                                                     **/
/*************************************************************************/

static set_mode_cursor(object)
	LVAL object;
{
  send_message(object, xlenter(":SET-MODE-CURSOR"));
}
