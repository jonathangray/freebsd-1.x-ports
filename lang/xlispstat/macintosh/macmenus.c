/* macmenus - Low Level Menu Objects for Macintosh                     */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
/***********************************************************************/
/**                                                                   **/
/**                    General Includes and Definitions               **/
/**                                                                   **/
/***********************************************************************/

#ifdef MPWC
# include <Quickdraw.h>
# include <Windows.h>
# include <Menus.h>
# include <Events.h>
# include <Dialogs.h>
# include <Desk.h>
# include <ToolUtils.h>
# include <OSEvents.h>
# define SPRINTF_ADJUST 0  /* needed because of THINK_C bug? */
#else
# include <QuickDraw.h>
# include <WindowMgr.h>
# include <MenuMgr.h>
# include <EventMgr.h>
# define SPRINTF_ADJUST 1  /* needed because of THINK_C bug? */
# define ModalFilterProcPtr ProcPtr
#endif MPWC

#define IVIEW_MENU MenuHandle
#define IVIEW_WINDOW WindowPtr
#define nil 0L

#include "xlisp.h"
#include "version.h"

/* external variables */
extern LVAL s_true, s_title, s_items, s_enabled, s_id, s_menu_list, s_key,
  s_mark, s_style, s_action, s_menu, s_menu_proto, s_apple_menu_proto,
  s_menu_item_proto, sk_select, sk_update, sk_do_action, s_bold, s_italic,
  s_underline, s_outline, s_shadow, s_condense, s_extend, sk_enabled,
  s_hardware_address, sk_allocate, sk_dispose;

extern char buf[];

/* external functions */
extern LVAL peekarg(), slot_value(), xsmenu_isnew(), xsmenu_select();
extern IVIEW_MENU get_menu_address();
extern IVIEW_WINDOW GETWINDOWADDRESS();

/***********************************************************************/
/**                                                                   **/
/**                       MENU-PROTO Definitions                      **/
/**                                                                   **/
/***********************************************************************/

# define get_menu_id(m) ((int) getfixnum(slot_value(m, s_id)))

FORWARD Style get_item_style();

/***********************************************************************/
/**                                                                   **/
/**                     MENU-ITEM-PROTO Definitions                   **/
/**                                                                   **/
/***********************************************************************/

FORWARD char *get_item_string();

/***********************************************************************/
/**                                                                   **/
/**                        Support Function                           **/
/**                                                                   **/
/***********************************************************************/

LOCAL LVAL GetMenuList()
{
  return(slot_value(getvalue(s_menu_proto), s_menu_list));
}

/* find the position of the item in the menu */
static get_item_position(menu, item)
	LVAL menu, item;
{
  int i;
  LVAL items;
  
  for (items = slot_value(menu, s_items), i = 1;
       consp(items) && car(items) != item; i++, items = cdr(items))
    ;
  if (item != car(items)) xlfail("item not in the menu");
  return(i);
}

/***********************************************************************/
/**                                                                   **/
/**                            Menu Functions                         **/
/**                                                                   **/
/***********************************************************************/

StMObInstalled(m)
	LVAL m;
{
  return(StMObAllocated(m) && GetMHandle(get_menu_id(m)) != NIL);
}

/* find menu object with given hardware address */
LVAL get_menu_by_hardware(m)
	IVIEW_MENU m;
{
  LVAL menu = NIL, next;
  
  for (next = GetMenuList();
       menu == NIL && consp(next); next = cdr(next)) 
    if (StMObAllocated(car(next)) && m == get_menu_address(car(next)))
      menu = car(next);
  
  if (menu == NIL) xlfail("can't find menu with this handle");
  return(menu);
}

/* find lisp menu with a specified macintosh menuID */
static LVAL get_menu_by_id(m)
	int m;
{
  return(get_menu_by_hardware(GetMHandle(m)));
}

/* menu select function for SkelMenu. Sends :SELECT message to the menu. */
static LispMenuSelect(i, m)
	int i, m;
{
  /* Unhilite the menu bar */
  HiliteMenu(0);
  
  send_message1(get_menu_by_id(m), sk_select, i);
}

/* send an installed menu the :UPDATE message */
static LispMenuUpdate(m)
	int m;
{
  send_message(get_menu_by_id(m), sk_update);
}

/* allocate a macintosh internal menu */
static id_in_use(id)
	int id;
{
  LVAL next;
  
  for (next = GetMenuList(); consp(next); next = cdr(next)) {
    if (id == get_menu_id(car(next))) return(TRUE);
  }
  return(FALSE);
}
  
static unique_id()
{
  static int id = 2000;
  
  if (id > 32000) id = 2000;
  id++;
  
  while (id_in_use(id)) id++;
  return(id);
}

StMObAllocateMach(menu)
	LVAL menu;
{
  MenuHandle theMenu;
  LVAL title;
  int menuID;
  
  title = slot_value(menu, s_title);
  
  menuID = unique_id();
  
  CtoPstr((char *) getstring(title));
  theMenu = NewMenu(menuID, getstring(title));
  PtoCstr((char *) getstring(title));
  if (theMenu == NULL) xlfail("menu allocation failed");
  set_menu_address(theMenu, menu);
  set_slot_value(menu, s_id, cvfixnum((FIXTYPE) menuID));
  
  if (kind_of_p(menu, getvalue(s_apple_menu_proto)))
    AddResMenu (theMenu, 'DRVR');
}

/* dispose of a macintosh menu */
StMObDisposeMach(menu)
	LVAL menu;
{
  if (StMObAllocated(menu)) SkelRmveMenu(get_menu_address(menu));
  if (StMObAllocated(menu)) DisposeMenu(get_menu_address(menu));
}

/* add items to a macintosh internal menu */
StMObAppendItems(menu, items)
	LVAL menu, items;
{
  LVAL item;
  char *s;
  int i;
  MenuHandle theMenu;
  
  if (StMObAllocated(menu)) {
    theMenu = get_menu_address(menu);
    i = llength(slot_value(menu, s_items)) - llength(items);
    if (i < 0) xlfail("append list should not exceed item list");
    
    for (; consp(items); items = cdr(items), i++) {
      item = car(items);
      s = get_item_string(item);
      CtoPstr(s);
      InsMenuItem(theMenu, s, i);
      PtoCstr(s);
      SetItemStyle(theMenu, i, get_item_style(item));
    }
  }
}

/* remove item from a macintosh menu */
StMObDeleteItem(menu, item)
	LVAL menu, item;
{
  if (StMObAllocated(menu)) 
    DelMenuItem(get_menu_address(menu), get_item_position(menu, item));
}

/* install a macintosh menu */
StMObInstall(menu)
	LVAL menu;
{
  if (! StMObInstalled(menu)) {
    if (! StMObAllocated(menu)) StMObAllocate(menu);
    SkelMenu(get_menu_address(menu), LispMenuSelect, NULL);
    SkelMenuUpdateProc(get_menu_address(menu), LispMenuUpdate);
  }
}

/* remove a macintosh menu */
StMObRemove(menu)
	LVAL menu;
{
  if (StMObAllocated(menu)) SkelRmveMenu(get_menu_address(menu));
  if (StMObAllocated(menu)) StMObDispose(menu);
}

/* enable or disable a macintosh menu */
StMObEnable(menu, enable)
	LVAL menu;
	int enable;
{
  if (StMObAllocated(menu)) {
    if (enable) EnableItem(get_menu_address(menu), 0);
    else DisableItem(get_menu_address(menu), 0);
    if (StMObInstalled(menu)) DrawMenuBar();
  }
  set_slot_value(menu, s_enabled, (enable) ? s_true : NIL);
}

StMObPopup(menu, left, top, window)
	LVAL menu, window;
	int left, top;
{
  IVIEW_MENU theMenu;
  IVIEW_WINDOW w;
  int item, menuID;
  GrafPtr SavePort;
  Point pt;
  
  StMObAllocate(menu);
  theMenu = get_menu_address(menu);
  menuID = get_menu_id(menu);
  if (window != NIL && (w = GETWINDOWADDRESS(window)) != nil) {
    GetPort(&SavePort);
    SetPort(w);
    pt.h = left; pt.v = top;
    LocalToGlobal(&pt);
    left = pt.h; top = pt.v;
    SetPort(SavePort);
  }
  if (! StillDown()) {
    while (! Button()) ;
    FlushEvents(mDownMask | mUpMask, 0);
  }
  InsertMenu(theMenu, -1);
  item = LoWord(PopUpMenuSelect(theMenu, top, left, 1));
  DeleteMenu(menuID);
  StMObDispose(menu);
  return(item);
}
  
/***********************************************************************/
/**                                                                   **/
/**                         Menu Item Functions                       **/
/**                                                                   **/
/***********************************************************************/

/* Get a string for use by AppendMenu. Style info is not encoded. */
static char *get_item_string(item)
	LVAL item;
{
  LVAL title, key, mark, enabled;
  static char *s;
    
  if (! menu_item_p(item)) xlerror("not a menu item", item);
  
  title = slot_value(item, s_title);
  if (! stringp(title)) xlerror("title is not a string", title);
  key = slot_value(item, s_key);
  mark = slot_value(item, s_mark);
  enabled = slot_value(item, s_enabled);
  
  s = buf;
  if (enabled == NIL)
    s += sprintf(s, "(") - SPRINTF_ADJUST;
  if (charp(key))
    s += sprintf(s, "/%c", getchcode(key)) - SPRINTF_ADJUST;
  if (mark == s_true)
    s += sprintf(s, "!%c", 0x12) - SPRINTF_ADJUST;
  else if (charp(mark))
    s += sprintf(s, "!%c", getchcode(key)) - SPRINTF_ADJUST;
  sprintf(s, "%s", getstring(title));
  return(buf);
}

/* Convert style symbol to Style value */
static Style style_value(sym)
	LVAL sym;
{
  if (sym == NIL) return(0);
  else if (! symbolp(sym)) xlerror("not a symbol", sym);
  else if (sym == s_bold) return(bold);
  else if (sym == s_italic) return(italic);
  else if (sym == s_underline) return(underline);
  else if (sym == s_outline) return(outline);
  else if (sym == s_shadow) return(shadow);
  else if (sym == s_condense) return(condense);
  else if (sym == s_extend) return(extend);
  else xlerror("unknown style symbol", sym);
}

/* compute the style value for a style symbol or list using bit-or */
static Style get_item_style(item)
	LVAL item;
{
  LVAL style;
  Style s;
  
  style = slot_value(item, s_style);
  if (consp(style)) {
    for (s = 0; consp(style); style = cdr(style))
      s = s | style_value(car(style));
    return(s);
  }
  else return (style_value(style));
}
	
/* adjust internal implementation of allocated menu to new instance value */ 
StMObSetItemProp(item, which)
	LVAL item;
	int which;
{
  char *s, ch;
  MenuHandle theMenu;
  LVAL menu;
  int i;
  
  menu = slot_value(item, s_menu);
  if (menu != NIL && StMObAllocated(menu)) {
    theMenu = get_menu_address(menu);
    i = get_item_position(menu, item);
    switch (which) {
    case 'T': {
                LVAL title = slot_value(item, s_title);
                if (! stringp(title))
                  xlerror("title is not a string", title);
                s = (char *) getstring(title); 
                CtoPstr(s);
                SetItem(theMenu, i, s);
                PtoCstr(s);
                break;
              }
    case 'K': DelMenuItem(theMenu, i);
              s = get_item_string(item);
              CtoPstr(s);
              InsMenuItem(theMenu, s, i - 1);
              PtoCstr(s);
              SetItemStyle(theMenu, i, get_item_style(item));
              break;
    case 'M': {
                LVAL mark = slot_value(item, s_mark);
                CheckItem(theMenu, i, FALSE);
                if (mark == s_true) ch = 0x12;
                else if (charp(mark)) ch = getchcode(mark);
                else break; 
                SetItemMark(theMenu, i, ch);
                break;
              }
    case 'S': SetItemStyle(theMenu, i, get_item_style(item)); break;
    case 'A': break;
    case 'E': if (slot_value(item, s_enabled) != NIL) 
                EnableItem(theMenu, i);
              else DisableItem(theMenu, i);
              break;
    default:  xlfail("unknown item instance variable");
    }
  }
}

/***********************************************************************/
/***********************************************************************/
/**                                                                   **/
/**                    APPLE-MENU-PROTO Methods                       **/
/**                                                                   **/
/***********************************************************************/
/***********************************************************************/

LVAL xsapple_menu_isnew() { return(xsmenu_isnew()); }

LVAL xsapple_menu_select()
{
  LVAL menu = peekarg(0), item = peekarg(1);
  int i, n;
  GrafPtr SavePort;
  
  if (! menu_p(menu)) xlerror("not a menu", menu);
  if (! fixp(item)) xlerror("not an integer", item);

  i = getfixnum(item);
  n = llength(slot_value(menu, s_items));
  
  if (i <= n) return(xsmenu_select());
  else {
    menu = xlgetarg();
    i = getfixnum(xlgetarg());
    xllastarg();
    
    if (StMObAllocated(menu)) {
      GetPort (&SavePort);
      GetItem (get_menu_address(menu), i, buf);  /* get DA name */
      OpenDeskAcc(buf);                          /* open it     */
      SetPort (SavePort);
    }
    return(NIL);
  }
}

/* about alert for the */
# define	aboutAlrt		1000
#ifdef MPWC
#define COMPILER "\pMPW C, V3.0"
#else
#define COMPILER "\pLightspeedª C, V3.0"
#endif MPWC
LVAL xsabout_xlisp_stat() 
{
  char *vers = XLISPSTAT_VERSION;
  
  xllastarg();
  CtoPstr(vers);
  ParamText(vers, COMPILER, "\p", "\p");
  Alert (aboutAlrt, (ModalFilterProcPtr) NULL);
  PtoCstr(vers);
  return(NIL);
}
