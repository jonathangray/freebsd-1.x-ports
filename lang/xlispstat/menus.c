/* menus - Hardware Independent Menu Objects                           */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
/***********************************************************************/
/**                                                                   **/
/**                    General Includes and Definitions               **/
/**                                                                   **/
/***********************************************************************/

#include "xlisp.h"

/* external variables */
extern LVAL s_true, s_title, s_items, s_enabled, s_id, s_menu_list, s_key, 
  s_mark, s_style, s_action, s_menu, s_menu_proto, s_apple_menu_proto, 
  s_menu_item_proto, sk_select, sk_update, sk_do_action, s_bold, s_italic,
  s_underline, s_outline, s_shadow, s_condense, s_extend, sk_enabled, 
  s_hardware_address, sk_allocate, sk_dispose;

extern char buf[];

/* external functions */
extern LVAL xremove(), makearglist();
extern LVAL list2(), xsadjoin(), slot_value(), xscallsubr2();

/***********************************************************************/
/**                                                                   **/
/**                        MENU-PROTO Definitions                     **/
/**                                                                   **/
/***********************************************************************/

# define menu_enabled_p(m) (slot_value(m, s_enabled) != NIL)

/***********************************************************************/
/**                                                                   **/
/**                     MENU-ITEM-PROTO Definitions                   **/
/**                                                                   **/
/***********************************************************************/

# define item_installed_p(i) (slot_value(i, s_menu) != NIL)

/***********************************************************************/
/**                                                                   **/
/**                          Utility Functions                        **/
/**                                                                   **/
/***********************************************************************/

/* append item to the end of list and return result. Cons item to NIL */
/* if list is NIL.                                                    */
LOCAL LVAL rplac_end(list, item)
	LVAL list, item;
{
  LVAL next; 
  if (list == NIL) return(consa(item));
  else if (listp(list)) {
    for (next = list; consp(cdr(next)); next = cdr(next))
      ;
    rplacd(next, consa(item));
    return(list);
  }
  else xlerror("not a list", list);
}

LOCAL LVAL remove_from_list(item, list)
	LVAL item, list;
{
  return(xscallsubr2(xremove, item, list));
}

/***********************************************************************/
/**                                                                   **/
/**                         Menu List Functions                       **/
/**                                                                   **/
/***********************************************************************/

LOCAL LVAL GetMenuList()
{
  return(slot_value(getvalue(s_menu_proto), s_menu_list));
}

LOCAL SetMenuList(list)
	LVAL list;
{
  set_slot_value(getvalue(s_menu_proto), s_menu_list, list);
}

/***********************************************************************/
/***********************************************************************/
/**                                                                   **/
/**                          MENU-PROTO Methods                       **/
/**                                                                   **/
/***********************************************************************/
/***********************************************************************/

/***********************************************************************/
/**                                                                   **/
/**                      Hardware Address Functions                   **/
/**                                                                   **/
/***********************************************************************/

/* check if menu is currently allocated. */
StMObAllocated(menu)
	LVAL menu;
{
  return(valid_menu_address(slot_value(menu, s_hardware_address)));
}  

/***********************************************************************/
/**                                                                   **/
/**               Predicate and Argument Access Function              **/
/**                                                                   **/
/***********************************************************************/

/* Is this a menu? */
menu_p(x)
	LVAL x;
{
  return (kind_of_p(x, getvalue(s_menu_proto)));
}

/* get and check a menu from the argument list */
LVAL xsgetmenu()
{
  LVAL menu;
  menu = xlgaobject();
  if (! menu_p(menu)) xlerror("not a menu", menu);
  return(menu);
}

/***********************************************************************/
/**                                                                   **/
/**                         Support Functions                         **/
/**                                                                   **/
/***********************************************************************/

/* append list of items to the menu */
static append_items(menu, new_items)
	LVAL menu, new_items;
{
  LVAL next, item, item_list;
  
  /* Check all items are menu items and not installed */
  for (next = new_items; consp(next); next = cdr(next)) {
    item = car(next);
    if (! menu_item_p(item)) xlerror("not a menu item", item);
    if (item_installed_p(item)) xlerror("item already installed", item);
  }
  
  /* add items to the item list and set items menus to menu */
  for (next = new_items; consp(next); next = cdr(next)) {
    item = car(next);
    item_list = rplac_end(slot_value(menu, s_items), item);
    set_slot_value(menu, s_items,item_list);
    set_slot_value(item, s_menu, menu);
  }
            
  if (StMObAllocated(menu)) StMObAppendItems(menu, new_items);
}

/* delete item from the list */
static delete_menu_item(menu, item)
	LVAL menu, item;
{
  LVAL item_list;
   
  StMObDeleteItem(menu, item);
  
  item_list = slot_value(menu, s_items);
  item_list = remove_from_list(item, item_list);
  set_slot_value(menu, s_items,item_list);
  set_slot_value(item, s_menu, NIL);
}
   
/* allocate a menu and enter it into the list of allocated menus */
StMObAllocate(menu)
	LVAL menu;
{
  LVAL menu_list;
  
  StMObDispose(menu);

  StMObAllocateMach(menu);
  
  StMObEnable(menu, menu_enabled_p(menu));
  StMObAppendItems(menu, slot_value(menu, s_items));
    
  menu_list = GetMenuList();
  menu_list = xscallsubr2(xsadjoin, menu, menu_list);
  SetMenuList(menu_list);
}

/* send :UPDATE message to menu items */
static update_menu(menu)
	LVAL menu;
{
  LVAL list;
  
  for (list = slot_value(menu, s_items); consp(list); list = cdr(list))
    send_message(car(list), sk_update);
}

/* dispose of a menu */
StMObDispose(menu)
	LVAL menu;
{
  LVAL menu_list;
  
  if (StMObAllocated(menu)) StMObDisposeMach(menu);
  standard_hardware_clobber(menu);

  menu_list = GetMenuList();
  menu_list = remove_from_list(menu, menu_list);
  SetMenuList(menu_list);
}

/* handle simple imperative messages with no arguments */
static LVAL simple_menu_message(which)
	int which;
{
  LVAL menu;
  LVAL arg;
  int set = FALSE;
	
  menu = xlgaobject();
  if (which == 'E') {
    if (moreargs()) {
      set = TRUE;
      arg = (xlgetarg() != NIL) ? s_true : NIL;
    }
  }
  xllastarg();
  
  switch (which) {
  case 'A': StMObAllocate(menu); break;
  case 'D': StMObDispose(menu); break;
  case 'E': if (set) {
              set_slot_value(menu, s_enabled, arg);
              StMObEnable(menu, (arg != NIL));
            }
            return(slot_value(menu, s_enabled));
  case 'I': StMObInstall(menu); break;
  case 'R': StMObRemove(menu); break;
  case 'U': update_menu(menu); break;
  default:  xlfail("unknown message");
  }
  
  return(NIL);
}

/* handle instance variable selectors/status inquiries */
static LVAL menu_selector_message(which)
	int which;
{
  LVAL menu, result;
  
  menu = xlgaobject();
  xllastarg();

  switch (which) {
  case 'A': result = (StMObAllocated(menu)) ? s_true : NIL; break;
  case 'I': result = slot_value(menu, s_items); break;
  case 'i': result = (StMObInstalled(menu)) ? s_true : NIL; break;
  default:  xlfail("unknown message");
  }
  return(result);
}

/***********************************************************************/
/**                                                                   **/
/**                              Methods                              **/
/**                                                                   **/
/***********************************************************************/

/* :ISNEW Method */
LVAL xsmenu_isnew()
{
  LVAL menu, title;
	
  menu = xlgaobject();
  title = xlgastring();
  xllastarg();

  if (strlen(getstring(title)) <= 0) xlerror("title is too short", title);
  
  object_isnew(menu);
  set_slot_value(menu, s_title, title);
  set_slot_value(menu, s_enabled, s_true);

  return(menu);
}

LVAL xsallocate_menu() { return(simple_menu_message('A')); }
LVAL xsdispose_menu()  { return(simple_menu_message('D')); }
LVAL xsupdate_menu()   { return(simple_menu_message('U')); }
LVAL xsallocated_p()  { return(menu_selector_message('A')); }
LVAL xsmenu_items()   { return(menu_selector_message('I')); }

LVAL xsinstall_menu()  { return(simple_menu_message('I')); }
LVAL xsremove_menu()   { return(simple_menu_message('R')); }
LVAL xsinstalled_p()  { return(menu_selector_message('i')); }

LVAL xsmenu_enabled()   { return(simple_menu_message('E')); }

/* :APPEND-ITEMS Method */
LVAL xsappend_items()
{
  LVAL menu, new_items;
	
  xlsave1(new_items);
  menu = xlgaobject();
  new_items = makearglist(xlargc, xlargv);
  append_items(menu, new_items);
  xlpop();
  return(NIL);
}

/* :DELETE-ITEMS Method */
LVAL xsdelete_items()
{
  LVAL menu;
	
  menu = xlgaobject();
  while (moreargs())
    delete_menu_item(menu, xlgaobject());
  return(NIL);
}

/* :SELECT Method */
LVAL xsmenu_select()
{
  LVAL menu, item, next;
  int i;
  
  menu = xsgetmenu();
  i = getfixnum(xlgafixnum());
  xllastarg();
  
  for (next = slot_value(menu, s_items);
       i > 1 && consp(next); i--, next = cdr(next))
    ;
  if (! consp(next)) xlfail("no item with this index in the menu");
  else item = car(next);
  
  send_message(item, sk_do_action);
  
  return(NIL);
}

LVAL xsmenu_title()
{
  LVAL menu, title;

  menu = xlgaobject();
  if (moreargs()) {
    title = xlgastring();
    if (strlen(getstring(title)) <= 0)
      xlerror("title is too short", title);
    if (StMObAllocated(menu))
      xlfail("can't change title of an allocated menu");
    set_slot_value(menu, s_title, title);
  }
  return(slot_value(menu, s_title));
}

LVAL xsmenu_popup()
{
  LVAL menu, window;
  int left, top, item;
  
  menu = xsgetmenu();
  left = getfixnum(xlgafixnum());
  top = getfixnum(xlgafixnum());
  window = (moreargs()) ? xlgaobject() : NIL;
  xllastarg();
  
  send_message(menu, sk_update);
  item = StMObPopup(menu, left, top, window);
  if (item > 0) send_message1(menu, sk_select, item);
  return(cvfixnum((FIXTYPE) item));
}
	

/***********************************************************************/
/***********************************************************************/
/**                                                                   **/
/**                     MENU-ITEM-PROTO Methods                       **/
/**                                                                   **/
/***********************************************************************/
/***********************************************************************/

/***********************************************************************/
/**                                                                   **/
/**              Predicate and Argument Access Function               **/
/**                                                                   **/
/***********************************************************************/

/* is this a menu item ? */
menu_item_p(x)
	LVAL x;
{
  return(kind_of_p(x, getvalue(s_menu_item_proto)));
}

/* get and check a menu item from the argument stack */
LVAL xsgetmenuitem()
{
	LVAL item;
	
	item = xlgaobject();
	if (! menu_item_p(item)) xlerror("not a menu item", item);
	return(item);
}

/***********************************************************************/
/**                                                                   **/
/**                        Support Function                           **/
/**                                                                   **/
/***********************************************************************/

/* check an item instance variable */
static LVAL check_item_ivar(which, value)
	int which;
	LVAL value;
{
  int good;
  
  switch (which) {
  case 'T': good = (stringp(value) && strlen(getstring(value)) > 0); break;
  case 'K': good = (charp(value) || value == NIL); break;
  case 'M': good = (charp(value) || value == NIL || value == s_true); break;
  case 'S': good = (symbolp(value) || listp(value)); break;
  case 'A': good = (value == NIL || symbolp(value) || closurep(value) || subrp(value)); break;
  case 'E': good = TRUE; value = (value != NIL) ? s_true : NIL; break;
  default:  xlfail("unknown item instance variable");
  }
  if (! good) xlerror("bad instance variable value", value);
  return(value);
}

/* set an item instance variable; item and value supplied or on the stack */
static LVAL set_item_ivar(which, item, value)
	int which;
	LVAL item, value;
{
  value = check_item_ivar(which, value);
  
  switch (which) {
  case 'T': set_slot_value(item, s_title, value); break;
  case 'K': set_slot_value(item, s_key, value); break;
  case 'M': set_slot_value(item, s_mark, value); break;
  case 'S': set_slot_value(item, s_style, value); break;
  case 'A': set_slot_value(item, s_action, value); break;
  case 'E': set_slot_value(item, s_enabled, value); break;
  default:  xlfail("unknown item instance variable");
  }
  
  StMObSetItemProp(item, which);
  return(value);
}

/* get an item instance variable; item and value supplied or on the stack */
static LVAL get_item_ivar(which, item)
	int which;
	LVAL item;
{
  LVAL value;
    
  switch (which) {
  case 'T': value = slot_value(item, s_title); break;
  case 'K': value = slot_value(item, s_key); break;
  case 'M': value = slot_value(item, s_mark); break;
  case 'S': value = slot_value(item, s_style); break;
  case 'A': value = slot_value(item, s_action); break;
  case 'E': value = slot_value(item, s_enabled); break;
  default:  xlfail("unknown item instance variable");
  }
  return(check_item_ivar(which, value));
}

static LVAL item_ivar(which)
	int which;
{
  LVAL item;
  
  item = xlgaobject();
  if (moreargs()) set_item_ivar(which, item, xlgetarg());
  return(get_item_ivar(which, item));
}

/***********************************************************************/
/**                                                                   **/
/**                            Methods                                **/
/**                                                                   **/
/***********************************************************************/

/* :ISNEW Method */
LVAL xsitem_isnew()
{ 
  LVAL item, title, value;
  
  item = xlgaobject();
  title = xlgastring();
  
  set_item_ivar('T', item, title);
  object_isnew(item);
  
  if (xlgetkeyarg(sk_enabled, &value)) set_item_ivar('E', item, value);
  else set_item_ivar('E', item, s_true);
  return(NIL);  /* to keep compilers happy - L. Tierney */
}

LVAL xsitem_title()       { return(item_ivar('T')); }
LVAL xsitem_key()         { return(item_ivar('K')); }
LVAL xsitem_mark()        { return(item_ivar('M')); }
LVAL xsitem_style()       { return(item_ivar('S')); }
LVAL xsitem_action()      { return(item_ivar('A')); }
LVAL xsitem_enabled()     { return(item_ivar('E')); }

/* :INSTALLED-P Method */
LVAL xsitem_installed_p() 
{
  LVAL item;
  item = xsgetmenuitem();
  xllastarg();
  
  return((item_installed_p(item)) ? s_true :  NIL);
  
}

LVAL xsitem_update()      { return(NIL); }

/* :DO-ACTION Method */
LVAL xsitem_do_action()
{ 
  LVAL item, action, result;
  item = xsgetmenuitem();
  xllastarg();
  
  action = slot_value(item, s_action);
  result = (action != NIL) ? xlapply(pushargs(action, NIL)) : NIL;
  return(result);
}
