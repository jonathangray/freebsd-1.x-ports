/* xsiviewwin - XLISP interface to IVIEW dynamic graphics package.     */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

#include "xlisp.h"
#include "iview.h"

#define IVIEW_WINDOW_TITLE  "Graph Window"
#ifdef MACINTOSH
#define IVIEW_WINDOW_LEFT 10
#define IVIEW_WINDOW_TOP 40
#define IVIEW_WINDOW_WIDTH  250
#define IVIEW_WINDOW_HEIGHT 250
#else
#define IVIEW_WINDOW_LEFT 50
#ifdef AMIGA
#define IVIEW_WINDOW_TOP 0
#else
#define IVIEW_WINDOW_TOP 50
#endif AMIGA
#define IVIEW_WINDOW_WIDTH  400
#define IVIEW_WINDOW_HEIGHT 400
#endif

/* external functions */
extern LVAL list2(), slot_value(), integer_list_2(), xmsend();

/* external variables */
extern LVAL s_true;
extern LVAL sk_allocate, sk_dispose, sk_resize, sk_redraw, sk_do_idle,
  sk_do_click, sk_do_motion, sk_do_key, sk_install, sk_remove, s_title,
  s_go_away, s_menu, s_hardware_address, s_black_on_white, s_has_h_scroll,
  s_has_v_scroll, s_internals, sk_show, sk_show_window;
  
/**************************************************************************/
/**                                                                      **/
/**                       Window Creation Functions                      **/
/**                                                                      **/
/**************************************************************************/

/* :ISNEW message for IVIEW-WINDOW-CLASS */
LVAL iview_window_isnew()
{
  LVAL object = xlgaobject();
  int show = xsboolkey(sk_show, TRUE);

  object_isnew(object);
  initialize_graph_window(object);
  if (show) send_message(object, sk_allocate);
  return(object);
}

/* :ALLOCATE message for IVIEW-WINDOW-CLASS */
LVAL iview_window_allocate()
{
  LVAL object;
  IVIEW_WINDOW w;
  
  object = xlgaobject();
  
  w = IViewWindowNew(object, TRUE);
  /* use StShowWindow to show (map) window but NOT send :resize or :redraw */
  if (xsboolkey(sk_show, TRUE)) StShowWindow(w);
  
  return(object);
}

StGWGetAllocInfo(object, title, left, top, width, height, goAway)
	LVAL object;
	char **title;
	int *left, *top, *width, *height, *goAway;
{
  LVAL window_title;
  
  if (slot_value(object, s_hardware_address) != NIL)
  	send_message(object, sk_dispose);
  
  window_title = slot_value(object, s_title);
  if (!stringp(window_title)) {
  	window_title = newstring(strlen(IVIEW_WINDOW_TITLE) + 1);
  	strcpy((char *) getstring(window_title), IVIEW_WINDOW_TITLE);
  	set_slot_value(object, s_title, window_title);
  }
  *title = (char *) getstring(window_title);
  
  *left = IVIEW_WINDOW_LEFT;
  *top = IVIEW_WINDOW_TOP;
  *width = IVIEW_WINDOW_WIDTH;
  *height = IVIEW_WINDOW_HEIGHT;
  get_window_bounds(object, left, top, width, height);
  
  *goAway = slot_value(object, s_go_away) != NIL;
}

StGWObDoClobber(object)
  LVAL object;
{
  standard_hardware_clobber(object);
}

StGWObResize(object)
  LVAL object;
{
  send_message(object, sk_resize);
}

StGWObRedraw(object)
	LVAL object;
{
  send_message(object, sk_redraw);
}
	

/* idle action. incall is used to detect longjmp's on errors and to    */
/* turn off idle calling if the call is generating an error.           */
StGWObDoIdle(object)
    LVAL object;
{
  static int incall = FALSE;
  
  if (incall) {
    StGWSetIdleOn(StGWObWinInfo(object), FALSE);
    incall = FALSE;
    return;
  }
  else {
    incall = TRUE;
    send_message(object, sk_do_idle);
    incall = FALSE;
  }
}

StGWObDoMouse(object, x, y, type, mods)
     LVAL object;
     int x, y;
     MouseEventType type;
     MouseClickModifier mods;
{
  LVAL Lx, Ly, argv[6];
  int extend, option;
  
  xlstkcheck(2);
  xlsave(Lx);
  xlsave(Ly);
  argv[0] = object;
  argv[2] = Lx = cvfixnum((FIXTYPE) x);
  argv[3] = Ly = cvfixnum((FIXTYPE) y);

  if (type == MouseClick) {
	extend = ((int) mods) % 2;
	option = ((int) mods) / 2;
    argv[1] = sk_do_click;
	argv[4] = (extend) ? s_true : NIL;
	argv[5] = (option) ? s_true : NIL;
    xscallsubrvec(xmsend, 6, argv);
  }
  else {
    argv[1] = sk_do_motion;
    xscallsubrvec(xmsend, 4, argv);
  }
  xlpopn(2);
}

StGWObDoKey(object, key, shift, opt)
	LVAL object;
	unsigned char key;
	int shift, opt;
{
  LVAL argv[5], ch;
  
  xlsave1(ch);
  ch = cvchar(key);
  argv[0] = object;
  argv[1] = sk_do_key;
  argv[2] = ch;
  argv[3] = shift ? s_true : NIL;
  argv[4] = opt ? s_true : NIL;
  xscallsubrvec(xmsend, 5, argv);
  xlpop();
}
  
char *StGWObWinInfo(object)
	LVAL object;
{
  LVAL internals = slot_value(object, s_internals);
  
  if (! consp(internals) || ! adatap(car(internals)) 
      || getadaddr(car(internals)) == nil) 
    xlfail("bad internal data");
  else return((char *) getadaddr(car(internals)));
}

initialize_graph_window(object)
	LVAL object;
{
  LVAL internals, value;
  int v, width, height, size;
  char *gwinfo;
  ColorCode bc,dc; /* added JKL */
  
  internals = newadata(StGWWinInfoSize(), 1, FALSE);
  set_slot_value(object, s_internals, consa(internals));
  StGWInitWinInfo(object);
  
  gwinfo = StGWObWinInfo(object);
  if (gwinfo == nil) return;
  
  StGWSetObject(gwinfo, object);
  
  if (slot_value(object, s_black_on_white) == NIL) {
    bc = StGWBackColor(gwinfo);         /* this seems better for color */
    dc = StGWDrawColor(gwinfo);         /* machines - 0 and 1 are not  */
    StGWSetDrawColor(gwinfo, bc);       /* the default draw and back   */
    StGWSetBackColor(gwinfo, dc);       /* colors on the Amiga   JKL   */
  }
  
  StGetScreenSize(&width, &height);
  size = (width > height) ? width : height;
  if ((value = slot_value(object, s_has_h_scroll)) != NIL) {
    v =  (fixp(value)) ? getfixnum(value) : size;
    StGWSetHasHscroll(gwinfo, TRUE, v);
  }
  if ((value = slot_value(object, s_has_v_scroll)) != NIL) {
    v =  (fixp(value)) ? getfixnum(value) : size;
    StGWSetHasVscroll(gwinfo, TRUE, v);
  }
}

LVAL xsiview_window_update()
{
#ifdef MACINTOSH
  LVAL object;
  int resized;
  
  object = xlgaobject();
  resized = (xlgetarg() != NIL);
  xllastarg();
  
  graph_update_action(StGWObWinInfo(object), resized);
#endif MACINTOSH
  return(NIL);
}

LVAL xsiview_window_activate()
{
#ifdef MACINTOSH
  LVAL object, menu;
  int active;
  
  object = xlgaobject();
  active = (xlgetarg() != NIL);
  xllastarg();
  
  graph_activate_action(StGWObWinInfo(object), active);
  menu = slot_value(object, s_menu);
  if (menu_p(menu)) {
    if (active) send_message(menu, sk_install);
    else send_message(menu, sk_remove);
  } 
#endif MACINTOSH
  return(NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                     Idle Installation Functions                      **/
/**                                                                      **/
/**************************************************************************/

LVAL iview_window_idle_on()
{
  char *gwinfo;
  int on, set = FALSE;
  
  gwinfo = StGWObWinInfo(xlgaobject());
  if (gwinfo == nil) return(NIL);
  
  if (moreargs()) {
    set = TRUE;
    on = (xlgetarg() != NIL) ? TRUE : FALSE;
  }
  xllastarg();

  if (set) StGWSetIdleOn(gwinfo, on);
  return((StGWIdleOn(gwinfo)) ? s_true : NIL);
}

/**************************************************************************/
/**                                                                      **/
/**                 Menu Installation and Access Functions               **/
/**                                                                      **/
/**************************************************************************/

extern LVAL get_menu_by_hardware();
extern IVIEW_MENU get_hardware_menu();

LVAL iview_window_menu()
{
  LVAL object, menu;
  int set = FALSE;
  
  object = xlgaobject();
  if (moreargs()) {
    set = TRUE;
    menu = xlgetarg();
  }
  xllastarg();

  if (set) {
    if (menu_p(menu)) set_slot_value(object, s_menu, menu);
    else if (menu == NIL) set_slot_value(object, s_menu, NIL);
    else xlerror("not a menu", menu);
  }
  
  return(slot_value(object, s_menu));
}
