/* macwindows - Macintosh window functions                             */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#ifdef MPWC
# include <Windows.h>
# include <Menus.h>
# include <OSUtils.h>
# include <Strings.h>
# include <Script.h>
# define screenBits qd.screenBits
# define MBarHeight GetMBarHeight()
CtoPstr(s) char *s; {c2pstr(s);}
PtoCstr(s) char *s; {p2cstr(s);}
#else
# include <WindowMgr.h>
# include <MenuMgr.h>
# include <OSUtil.h>
# include <Color.h>
# include <strings.h>
#endif MPWC
# include "xlisp.h"
# include "windows.h"

# define nil 0L

/* external variables */
extern LVAL s_true, sk_update, sk_close, sk_activate;

/**************************************************************************/
/**                                                                      **/
/**                           Utility Functions                          **/
/**                                                                      **/
/**************************************************************************/

static GrafPtr current_port()
{
  GrafPtr port;
  
  GetPort(&port);
  return(port);
}

/**************************************************************************/
/**                                                                      **/
/**                        Window Data Functions                         **/
/**                                                                      **/
/**************************************************************************/

LVAL get_window_object(w)
	WindowPtr w;
{
  WindowData data;
  
  data = (WindowData) get_window_data(w);
  if (data == nil || ! objectp((LVAL) data->object)) return(NIL);
  else return((LVAL) data->object);
}

set_window_object(w, object)
	WindowPtr w;
	LVAL object;
{
  WindowData data;
  
  data = (WindowData) get_window_data(w);
  if (data == nil) return;
  else data->object = (char *) object;
}

/**************************************************************************/
/**                                                                      **/
/**                       Standard Event Handlers                        **/
/**                                                                      **/
/**************************************************************************/

mac_update_action(resized)
	int resized;
{
  LVAL object = get_window_object(current_port());

  if (! mobject_p(object)) return;
  send_message_1L(object, sk_update, (resized) ? s_true : NIL);
}

mac_activate_action(active)
	int active;
{
  LVAL object = get_window_object(current_port());
  
  if (! mobject_p(object)) return;
  send_message_1L(object, sk_activate, (active) ? s_true : NIL);
}

mac_close_action()
{
  LVAL object = get_window_object(current_port());
  
  if (! mobject_p(object)) return;
  send_message(object, sk_close);
}

/**************************************************************************/
/**                                                                      **/
/**                 General Window Information Functions                 **/
/**                                                                      **/
/**************************************************************************/

StShowWindow(w)
	WindowPtr w;
{
  MyShowWindow(w);
}

StHideWindow(w)
	WindowPtr w;
{
  HideWindow(w);
}

StWSetTitle(w, str)
	WindowPtr w;
	char *str;
{
  CtoPstr(str);
  SetWTitle(w, str);
  PtoCstr(str);
}

/**************************************************************************/
/**                                                                      **/
/**                         Screen Info Functions                        **/
/**                                                                      **/
/**************************************************************************/

StGetScreenSize(width, height)
	int *width, *height;
{
  if (width != nil) *width = screenBits.bounds.right - screenBits.bounds.left;
  if (height != nil) *height = screenBits.bounds.bottom - screenBits.bounds.top - MBarHeight;
}

StScreenHasColor()
{
  SysEnvRec r;
  GDHandle gd;
  static inited = FALSE, has_color = FALSE;

  if (! inited) {
    SysEnvirons(1, &r);
    if ((int) r.hasColorQD) {
      gd = GetGDevice();
      has_color = ((*(*gd)->gdPMap)->pixelSize > 1) ? TRUE : FALSE;
    }
    else has_color = FALSE;
    inited = TRUE;
  }
  return(has_color);
}

StHasWindows() { return(TRUE); }
StFlushGraphics(){}
