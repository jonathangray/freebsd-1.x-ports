/* windows - general window functions                                  */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
# include "xlisp.h"
# include "iview.h"

/* external variables */
extern LVAL s_true, s_title, s_size, s_location, sk_allocate;
extern IVIEW_WINDOW GETWINDOWADDRESS();

/* external functions */
extern LVAL integer_list_2(), slot_value();

/**************************************************************************/
/**                                                                      **/
/**                           Utility Functions                          **/
/**                                                                      **/
/**************************************************************************/

get_window_bounds(object, left, top, width, height)
	LVAL object;
	int *left, *top, *width, *height;
{
  LVAL size, location;
  
  size = slot_value(object, s_size);
  location = slot_value(object, s_location);
  if (consp(size) && fixp(car(size)) && consp(cdr(size)) && fixp(car(cdr(size)))) {
    *width = getfixnum(car(size));
    *height = getfixnum(car(cdr(size)));
  }
  if (consp(location) && fixp(car(location)) 
      && consp(cdr(location)) && fixp(car(cdr(location)))) {
    *left = getfixnum(car(location));
    *top = getfixnum(car(cdr(location)));
  }
}

/***********************************************************************/
/**                                                                   **/
/**                  General Window Methods Functions                 **/
/**                                                                   **/
/***********************************************************************/

LVAL xsshowwindow()
{
  LVAL object = xlgaobject();
  IVIEW_WINDOW w = GETWINDOWADDRESS(object);
  
  if (w == nil) {
    send_message(object, sk_allocate);
    w = GETWINDOWADDRESS(object);
  }
  StShowWindow(w);
  return(NIL);
}

LVAL xshidewindow()
{
  LVAL object = xlgaobject();
  IVIEW_WINDOW w = GETWINDOWADDRESS(object);
  
  if (w != nil) StHideWindow(w);
  return(NIL);
}

LVAL xswindow_title()
{
  IVIEW_WINDOW w;
  LVAL object, title;
  char *str;
  
  object = xlgaobject();
  w = GETWINDOWADDRESS(object);
  if (moreargs()) {
    title = xlgastring();
    set_slot_value(object, s_title, title);
    if (w != nil) {
      str = (char *) getstring(title);
      StWSetTitle(w, str);
    }
  }
  return(slot_value(object, s_title));
}
  
static LVAL window_dimensions(which, frame)
	int which, frame;
{
  LVAL object, slot;
  IVIEW_WINDOW w;
  int a, b, set = FALSE;
  
  object = xlgaobject();
  if (moreargs()) {
    set = TRUE;
    a = getfixnum(xlgafixnum());
    b = getfixnum(xlgafixnum());
  }
  xllastarg();
  
  w = GETWINDOWADDRESS(object);
  slot = (which == 'L') ? s_location : s_size;
  
  if (set) {
    if (! frame) set_slot_value(object, slot, integer_list_2(a, b));
    if (w != nil) {
      switch (which) {
      case 'L': StWSetLocation(w, a, b, frame); break;
      case 'S': StWSetSize(w, a, b, frame);     break;
      }
    }
  }
  if (w != nil) {
    switch (which) {
    case 'L': 
      StWGetLocation(w, &a, &b, FALSE);
      set_slot_value(object, slot, integer_list_2(a, b));
      if (frame) StWGetLocation(w, &a, &b, TRUE);
      break;
    case 'S': 
      StWGetSize(w, &a, &b, FALSE);
      set_slot_value(object, slot, integer_list_2(a, b));
      if (frame) StWGetSize(w, &a, &b, TRUE);
      break;
    }
    return(integer_list_2(a, b));
  }
  else return(slot_value(object, slot));
}

LVAL xswindow_location()       { return(window_dimensions('L', FALSE)); }
LVAL xswindow_size()           { return(window_dimensions('S', FALSE)); }
LVAL xswindow_frame_location() { return(window_dimensions('L', TRUE));  }
LVAL xswindow_frame_size()     { return(window_dimensions('S', TRUE));  }

/**************************************************************************/
/**                                                                      **/
/**                         Screen Info Functions                        **/
/**                                                                      **/
/**************************************************************************/

LVAL xsscreen_size()
{
  int width, height;

  StGetScreenSize(&width, &height);
  return(integer_list_2(width, height));
}

LVAL xsscreen_has_color()
{
  return((StScreenHasColor()) ? s_true : NIL);
}

LVAL xsflush_graphics()
{
  StFlushGraphics();
  return(NIL);
}
