/* xsnamelist - XLISP interface to IVIEW dynamic graphics package.     */
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

#define LIST_BOTTOM_GAP 3
#define LIST_STRINGS_VISIBLE 15
#define LIST_STRING_TEMPLATE "123456789012345678901234567890"
#define LIST_PAGE_INC 10
#define LIST_CLICK_WIDTH 1
#define LIST_CLICK_HEIGHT 1

/* external functions */
extern IVIEW_WINDOW IViewNew();
extern char *IViewPointLabel();
extern MouseMode IViewMouseMode();
extern PointState IViewPointState(), IViewPointScreenState();
extern PointState decode_point_state();
extern IViewStdMouseAction();
extern LVAL slot_value();

/* external variables */
extern LVAL s_hardware_address, sk_show, sk_show_window;

extern IVIEW_WINDOW GETIVIEWADDRESS();

static list_space(gwinfo)
	char *gwinfo;
{
  return(StGWTextAscent(gwinfo) + StGWTextDescent(gwinfo) + LIST_BOTTOM_GAP);
}

/**************************************************************************/
/**                                                                      **/
/**                        List Creation Functions                       **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_list_allocate()
{
  LVAL object;
  int width, height, space;
  IVIEW_WINDOW w;
  char *gwinfo;
  int show;

  object = xlgaobject();
  show = xsboolkey(sk_show, TRUE);
  w = IViewNew(object);
  gwinfo = StGWObWinInfo(object);
  initialize_iview(w, object);

  space = list_space(gwinfo);
  width = StGWTextWidth(gwinfo, LIST_STRING_TEMPLATE);
  height = LIST_STRINGS_VISIBLE * space;
  StGWSetSize(gwinfo, width, height, FALSE); 
  StGWSetHasVscroll(gwinfo, TRUE, height);
  StGWSetVscrollIncs(gwinfo, space, LIST_PAGE_INC * space);
  StGrSetClickRange(gwinfo, LIST_CLICK_WIDTH, LIST_CLICK_HEIGHT);

  /* use StShowWindow to show (map) window but NOT send :resize or :redraw */
  if (show) StShowWindow(w);

  return(object);
}

/**************************************************************************/
/**                                                                      **/
/**                            Data Functions                            **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_list_add_points()
{
  IVIEW_WINDOW w;
  int old_n, n;
  LVAL object, data;
  int space;
  char *gwinfo;
  
  object = xlgaobject();
  w = GETIVIEWADDRESS(object);
  gwinfo = StGWObWinInfo(object);
  if (w == nil || gwinfo == nil) return(NIL);
  
  old_n = IViewNumPoints(w);
  data = xlgetarg();
  internal_iview_add_points(w, object, data);
  n = IViewNumPoints(w);
  
  space = list_space(gwinfo);
  check_add_to_screen(object, 'P', old_n, n, TRUE);
  StGWSetHasVscroll(gwinfo, TRUE, n * space);
  
  return(NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                    Drawing and Resizing Functions                    **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_list_redraw_content()
{
  int i, n;
  int left, top, width, height;
  PointState state;
  IVIEW_WINDOW w;
  LVAL object;
  char *gwinfo;
  
  object = xlgaobject();
  xllastarg();
  
  gwinfo = StGWObWinInfo(object);
  w = GETIVIEWADDRESS(object);
  if (w == nil) return(NIL);
  
  n = IViewNumPoints(w);
  StGWGetViewRect(gwinfo, &left, &top, &width, &height);
  StGWStartBuffering(gwinfo);
  StGWEraseRect(gwinfo, left, top, width, height);
  
  for (i = 0; i < n; i++) {
    state = IViewPointState(w, i);
    if (! IViewPointMasked(w, i) && state != pointInvisible)
      list_draw_point(w, i, state);   
  }
  IViewResetScreenStates(w);
  StGWBufferToScreen(gwinfo, left, top, width, height);
  return(NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                           Mouse Functions                            **/
/**                                                                      **/
/**************************************************************************/

static list_draw_point(w, i, state)
	IVIEW_WINDOW w;
	int i;
	PointState state;
{
  int left, top, width, height, space;
  int color, old_color;
  char *gwinfo;

  gwinfo = IViewWindowWinInfo(w);

  StGWGetViewRect(gwinfo, &left, &top, &width, &height);
  space = list_space(gwinfo);

  old_color = StGWDrawColor(gwinfo);
  color = IViewPointColor(w, i);
  if (color >= 0) StGWSetDrawColor(gwinfo, color);
  if (state == pointNormal) {
    StGWEraseRect(gwinfo, 0, space * i, width, space);
    StGWDrawString(gwinfo, IViewPointLabel(w, i), LIST_BOTTOM_GAP,
                           (i + 1) * space - LIST_BOTTOM_GAP);
  }
  else {
    StGWPaintRect(gwinfo, 0, space * i, width, space);
    StGWSetDrawColor(gwinfo, StGWBackColor(gwinfo));
    StGWDrawString(gwinfo, IViewPointLabel(w, i), LIST_BOTTOM_GAP,
                           (i + 1) * space - LIST_BOTTOM_GAP);
  }
  StGWSetDrawColor(gwinfo, old_color);
  IViewSetPointScreenState(w, i, state);
}

LVAL iview_list_adjust_screen_point()
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
      list_draw_point(w, point, state);
    }  
  }
  
  return(NIL);
}

LVAL iview_list_adjust_points_in_rect()
{
  int  i, n, in_rect, space, bottom, left, top, height, width;
  PointState point_state, state;
  IVIEW_WINDOW w;
  LVAL object;
  char *gwinfo;
  
  object = xlgaobject();
  w = GETIVIEWADDRESS(object);
  gwinfo = StGWObWinInfo(object);
  left = getfixnum(xlgafixnum());
  top = getfixnum(xlgafixnum());
  width = getfixnum(xlgafixnum());
  height = getfixnum(xlgafixnum());
  state = decode_point_state(xlgetarg());
  xllastarg();
  
  if (w == nil) return(NIL);

  IViewCheckLinks(w);
  n = IViewNumPoints(w);
  space = list_space(gwinfo);
  bottom = top + height;
  
  if (IViewMouseMode(w) == brushing) IViewEraseBrush(w);

  for (i = 0; i < n; i++) {
    point_state = IViewPointState(w, i);
    if (! IViewPointMasked(w, i) && point_state != pointInvisible) {
      in_rect = (top < (i + 1) * space && bottom > i * space);
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

LVAL iview_list_mark_points_in_rect()
{
  int  i, n, in_rect, space, bottom, left, top, width, height;
  PointState point_state;
  IVIEW_WINDOW w;
  LVAL object;
  char *gwinfo;
  
  object = xlgaobject();
  w = GETIVIEWADDRESS(object);
  gwinfo = StGWObWinInfo(object);
  left = getfixnum(xlgafixnum());
  top = getfixnum(xlgafixnum());
  width = getfixnum(xlgafixnum());
  height = getfixnum(xlgafixnum());
  xllastarg();
  
  if (w == nil) return(NIL);

  n = IViewNumPoints(w);
  space = list_space(gwinfo);
  bottom = top + height;
  
  for (i = 0; i < n; i++) {
    point_state = IViewPointState(w, i);
    if (! IViewPointMasked(w, i) && point_state != pointInvisible) {
      in_rect = (top < (i + 1) * space && bottom > i * space);
      IViewSetPointMark(w, i, in_rect);
    }
  }

  return(NIL);
}
