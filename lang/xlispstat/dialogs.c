/* dialogs - General Dialog Objects                                    */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */
 
#include "xlisp.h"

#ifdef MACINTOSH
#undef FALSE
#undef TRUE
#ifdef MPWC
# include  <Quickdraw.h> /* needed for definition of Point */
#else 
#include <WindowMgr.h>
#endif MPWC
#define FALSE 0
#define TRUE 1
#else
typedef struct {
  int h, v;
} Point;
#endif

/* external variables */
extern LVAL s_title, s_items, s_true, s_text, s_location, s_size,
  s_action, s_dialog, s_min_value, s_max_value, s_page_increment,
  s_editable, s_value, s_list_data, s_columns, s_dialog_proto,
  s_dialog_item_proto, s_button_item_proto, s_toggle_item_proto,
  s_text_item_proto, sk_editable, s_choice_item_proto,
  s_scroll_item_proto, sk_min_value, sk_max_value, sk_page_increment,
  s_list_item_proto, sk_columns, s_modeless, sk_allocate, s_modal,
  s_hardware_address, sk_show;

/* external functions */
extern LVAL slot_value(), make_string(), xsfuncall1(), integer_list_2(), 
  coerce_to_vector(), copyarray(), arraydata(), send_message_1L();

extern LVAL DialogGetModalItem(), DialogToggleItemValue(),
  DialogTextItemText(), DialogChoiceItemValue(), DialogScrollItemValue(),
  DialogScrollItemMax(), DialogScrollItemMin(), DialogListItemSelection();

/* layout definitions */
# define ITEM_GAP 10
# define SCROLL_MIN 0
# define SCROLL_MAX 100
# define SCROLL_PAGE 5

# define has_item_location(i) (slot_value(i, s_location) != NIL)
# define has_item_size(i) (slot_value(i, s_size) != NIL)
# define check_dialog_address(d) valid_dialog_address(slot_value(d, s_hardware_address))

/***********************************************************************/
/**                                                                   **/
/**                          Utility Functions                        **/
/**                                                                   **/
/***********************************************************************/

check_point_list(x)
	LVAL x;
{
  return(listp(x) && llength(x) == 2 && fixp(car(x)) && fixp(car(cdr(x))));
}

Point ListToPoint(list)
	LVAL list;
{
  Point pt;
  
  if (! check_point_list(list)) xlerror("not a point", list);
  pt.h = getfixnum(car(list));
  pt.v = getfixnum(car(cdr(list)));
  return(pt);
}

LVAL PointToList(pt)
	Point pt;
{
  return(integer_list_2((int) pt.h, (int) pt.v));
}

/***********************************************************************/
/***********************************************************************/
/**                                                                   **/
/**                         DIALOG-PROTO Methods                      **/
/**                                                                   **/
/***********************************************************************/
/***********************************************************************/

/***********************************************************************/
/**                                                                   **/
/**                         Support Functions                         **/
/**                                                                   **/
/***********************************************************************/

/* Is this a dialog? */
dialog_p(x)
	LVAL x;
{
  return (kind_of_p(x, getvalue(s_dialog_proto)));
}

/* get a dialog from the stack */
LVAL xsgetdialog()
{
  LVAL x;
  x = xlgetarg();
  if (! dialog_p(x)) xlerror("not a dialog", x);
  return(x);
}

static Point calc_item_size(dialog, item, left, top)
	LVAL dialog, item;
	int left, top;
{
  Point sz, loc;
  
  if (! dialog_item_p(item)) xlerror("not a dialog item", item);
  if (slot_value(item, s_dialog) != NIL
      && check_dialog_address(slot_value(item, s_dialog))) 
    xlfail("item is already installed in a dialog");
  sz = ListToPoint(slot_value(item, s_size));
  if (has_item_location(item))
    loc = ListToPoint(slot_value(item, s_location));
  else {
    loc.h = left;
    loc.v = top;
    set_slot_value(item, s_location, PointToList(loc));
  }
  sz.h += loc.h - left;
  sz.v += loc.v - top;
  sz.h = max((int) sz.h, 0);
  sz.v = max((int) sz.v, 0);
  set_slot_value(item, s_dialog, dialog);
  return(sz);
} 

static Point calc_item_list_size(dialog, items, left, top, as_column)
	LVAL dialog, items;
	int left, top, as_column;
{
  LVAL item;
  Point sz, pt;
  
  for (sz.h = 0, sz.v = 0; consp(items); items = cdr(items)) {
    item = car(items);
    if consp(item)
      pt = calc_item_list_size(dialog, item, left, top, ! as_column);
    else pt = calc_item_size(dialog, item, left, top);
    if (as_column) {
      sz.h = max((int) sz.h, (int) pt.h);
      sz.v += pt.v + ITEM_GAP;
      top += pt.v + ITEM_GAP;
    }
    else {
      sz.h += pt.h + ITEM_GAP;
      left += pt.h + ITEM_GAP;
      sz.v = max((int) sz.v, (int) pt.v);
    }
  }
  if (as_column) sz.v = max((int) (sz.v - ITEM_GAP), 0);
  else sz.h = max((int) (sz.h - ITEM_GAP), 0);
  
  return(sz);
}

static calc_size(dialog)
	LVAL dialog;
{
  Point sz;
  LVAL size = slot_value(dialog, s_size);
  LVAL items = slot_value(dialog, s_items);
  
  sz = calc_item_list_size(dialog, items, ITEM_GAP, ITEM_GAP, TRUE);
  sz.h += 2 * ITEM_GAP;
  sz.v += 2 * ITEM_GAP;
  if (! check_point_list(size)) {
    set_slot_value(dialog, s_size, PointToList(sz));
  }
}

static calc_location(dialog)
	LVAL dialog;
{
  Point screen, size, location;
  int left, top;
  LVAL loc = slot_value(dialog, s_location);
  
  if (! check_point_list(loc)) {
    StGetScreenSize(&left, &top);
	screen.h = left; screen.v = top; /* needed since components may be shorts */
    size = ListToPoint(slot_value(dialog, s_size));
    location.h = (screen.h - size.h) / 2;
    location.v = (screen.v - size.v) / 2;
    set_slot_value(dialog, s_location, PointToList(location));
  }
}
 
static LVAL simple_dialog_method(which)
	int which;
{
  LVAL dialog, result = NIL;
  
  dialog = xsgetdialog();
  xllastarg();
   
  switch (which) {
  case 'R': DialogRemove(dialog); break;
  case 'A': calc_size(dialog);
            calc_location(dialog);
            DialogAllocate(dialog);
            break;
  case 'a': result = (check_dialog_address(dialog)) ? s_true : NIL; break;
  }
  return(result);
}

extern LVAL s_text_item_proto, sk_new;
extern LVAL copylist();

static LVAL make_text_item(string)
	LVAL string;
{
  LVAL result;
  
  result = send_message_1L(getvalue(s_text_item_proto), sk_new, string);
  return(result);
}

static LVAL process_items(items)
	LVAL items;
{
  LVAL next;
  
  xlprot1(items);
  items = copylist(items);
  for (next = items; consp(next); next = cdr(next)) {
    if (stringp(car(next))) rplaca(next, make_text_item(car(next)));
    else if (consp(car(next))) rplaca(next, process_items(car(next)));
  }
  xlpop();
  return(items);
}
  
/***********************************************************************/
/**                                                                   **/
/**                              Methods                              **/
/**                                                                   **/
/***********************************************************************/

/* :ISNEW Method */
LVAL xsdialog_isnew()
{
  LVAL dialog, items;
	
  dialog = xsgetdialog();
  
  items = xlgalist();
  items = process_items(items);
  set_slot_value(dialog, s_items, items);
  
  object_isnew(dialog);
  if (! stringp(slot_value(dialog, s_title)))
    set_slot_value(dialog, s_title, make_string("Dialog"));
    
  if (xsboolkey(sk_show, TRUE)) send_message(dialog, sk_allocate);
  
  return(dialog);
}

LVAL xsdialog_allocate()     { return(simple_dialog_method('A')); }
LVAL xsdialog_remove()       { return(simple_dialog_method('R')); }
LVAL xsdialog_allocated_p()  { return(simple_dialog_method('a')); }

LVAL xsdialog_default_button()
{
  LVAL dialog,  item;

  dialog = xsgetdialog();
  item = xlgetarg();
  xllastarg();

  DialogSetDefaultButton(dialog, item);
  
  return (item); 
}

/* :MODAL-DIALOG method */
LVAL xsdialog_modal()
{
  LVAL dialog;
    
  dialog = xsgetdialog();
  xllastarg();
  
  return(DialogGetModalItem(dialog));
}

/***********************************************************************/
/***********************************************************************/
/**                                                                   **/
/**                      DIALOG-ITEM-PROTO Methods                    **/
/**                                                                   **/
/***********************************************************************/
/***********************************************************************/

/* Is this a dialog-item? */
dialog_item_p(x)
	LVAL x;
{
  return (kind_of_p(x, getvalue(s_dialog_item_proto)));
}

/* get a dialog item from the stack */
LVAL xsgetdialogitem()
{
  LVAL x;
  x = xlgetarg();
  if (! dialog_item_p(x)) xlerror("not a dialog item", x);
  return(x);
}

static get_initial_item_values(item, get_first)
	LVAL item;
	int get_first;
{
  LVAL text;
  
  if (get_first) {
    text = xlgastring();
    set_slot_value(item, s_text, text);
  }
  object_isnew(item);
}

/* :DO-ACTION Method */
LVAL xsdialog_item_do_action()
{ 
  LVAL item, action, result;
  item = xsgetdialogitem();
  xllastarg();
  
  action = slot_value(item, s_action);
  result = (action != NIL) ? xlapply(pushargs(action, NIL)) : NIL;
  return(result);
}

/* :ACTION Method */
LVAL xsdialog_item_action()
{
  LVAL item, action;
  int set;
  
  item = xsgetdialogitem();
  set = moreargs();
  if (set) action = xlgetarg();
  xllastarg();
  
  if (set) set_slot_value(item, s_action, action);
  return(slot_value(item, s_action));
}

/***********************************************************************/
/***********************************************************************/
/**                                                                   **/
/**                      BUTTON-ITEM-PROTO Methods                    **/
/**                                                                   **/
/***********************************************************************/
/***********************************************************************/

/* Is this a button-item? */
button_item_p(x)
	LVAL x;
{
  return (kind_of_p(x, getvalue(s_button_item_proto)));
}

/* get a button item from the stack */
LVAL xsgetbuttonitem()
{
  LVAL x;
  x = xlgetarg();
  if (! button_item_p(x)) xlerror("not a button item", x);
  return(x);
}

/* :ISNEW Method */
LVAL xsbutton_item_isnew()
{
  LVAL item;
  int width, height;
  
  item = xsgetbuttonitem();
  
  get_initial_item_values(item, TRUE);
    
  if (! has_item_size(item)) {
    DialogButtonGetDefaultSize(item, &width, &height);
    set_slot_value(item, s_size, integer_list_2(width, height));
  }
  
  return(item);
}

/***********************************************************************/
/***********************************************************************/
/**                                                                   **/
/**                      TOGGLE-ITEM-PROTO Methods                    **/
/**                                                                   **/
/***********************************************************************/
/***********************************************************************/

/* Is this a toggle-item? */
toggle_item_p(x)
	LVAL x;
{
  return (kind_of_p(x, getvalue(s_toggle_item_proto)));
}

/* get a toggle item from the stack */
LVAL xsgettoggleitem()
{
  LVAL x;
  x = xlgetarg();
  if (! toggle_item_p(x)) xlerror("not a toggle item", x);
  return(x);
}

/* :ISNEW Method */
LVAL xstoggle_item_isnew()
{
  LVAL item;
  int width, height;
  
  item = xsgettoggleitem();
  
  get_initial_item_values(item, TRUE);
    
  if (! has_item_size(item)) {
    DialogToggleGetDefaultSize(item, &width, &height);
    set_slot_value(item, s_size, integer_list_2(width, height));
  }
  
  return(item);
}

/* :VALUE Method */
LVAL xstoggle_item_value()
{
  LVAL item, value;
  int set;
   
  item = xsgettoggleitem();
  set = moreargs();
  if (set) value = xlgetarg();
  xllastarg();
  
  return(DialogToggleItemValue(item, set, value));
}

/***********************************************************************/
/***********************************************************************/
/**                                                                   **/
/**                       TEXT-ITEM-PROTO Methods                     **/
/**                                                                   **/
/***********************************************************************/
/***********************************************************************/

/* Is this a text-item? */
text_item_p(x)
	LVAL x;
{
  return (kind_of_p(x, getvalue(s_text_item_proto)));
}

/* get a text item from the stack */
LVAL xsgettextitem()
{
  LVAL x;
  x = xlgetarg();
  if (! text_item_p(x)) xlerror("not a text item", x);
  return(x);
}

/* :ISNEW Method */
LVAL xstext_item_isnew()
{
  LVAL item, edit;
  int width, height;
  
  item = xsgettextitem();

  get_initial_item_values(item, TRUE);
    
  if (xlgetkeyarg(sk_editable, &edit) && edit != NIL)
    set_slot_value(item, s_editable, s_true);
    
  if (! has_item_size(item)) {
    DialogTextGetDefaultSize(item, &width, &height);
    set_slot_value(item, s_size, integer_list_2(width, height));
  }
  
  return(item);
}

/* :TEXT Method */
LVAL xstext_item_text()
{
  LVAL item;
  int set;
  char *text;
  
  item = xsgettextitem();
  set = moreargs();
  if (set) text = (char *) getstring(xlgastring());
  xllastarg();
  
  return(DialogTextItemText(item, set, text));
}

/***********************************************************************/
/***********************************************************************/
/**                                                                   **/
/**                      CHOICE-ITEM-PROTO Methods                    **/
/**                                                                   **/
/***********************************************************************/
/***********************************************************************/

/* Is this a choice-item? */
choice_item_p(x)
	LVAL x;
{
  return (kind_of_p(x, getvalue(s_choice_item_proto)));
}

/* get a choice item from the stack */
LVAL xsgetchoiceitem()
{
  LVAL x;
  x = xlgetarg();
  if (! choice_item_p(x)) xlerror("not a choice item", x);
  return(x);
}

/* :ISNEW Method */
LVAL xschoice_item_isnew()
{
  LVAL item, text, next;
  int width, height;
  
  item = xsgetchoiceitem();
  text = xlgalist();
  for (next = text; consp(next); next = cdr(next))
    if (! stringp(car(next))) xlerror("not a string", car(next));
  set_slot_value(item, s_text, text);
  
  get_initial_item_values(item, FALSE);
    
  if (! has_item_size(item)) {
    DialogChoiceGetDefaultSize(item, &width, &height);
    set_slot_value(item, s_size, integer_list_2(width, height));
  }
  if (! fixp(slot_value(item, s_value)))
    set_slot_value(item, s_value, cvfixnum((FIXTYPE) 0));
  return(item);
}

/* :VALUE Method */
LVAL xschoice_item_value()
{
  LVAL item;
  int value, set;
  
  item = xsgetchoiceitem();
  set = moreargs();
  if (set) value = getfixnum(xlgafixnum());
  xllastarg();
  
  return(DialogChoiceItemValue(item, set, value));
}

/***********************************************************************/
/***********************************************************************/
/**                                                                   **/
/**                      SCROLL-ITEM-PROTO Methods                    **/
/**                                                                   **/
/***********************************************************************/
/***********************************************************************/

/* Is this a scroll-item? */
scroll_item_p(x)
	LVAL x;
{
  return (kind_of_p(x, getvalue(s_scroll_item_proto)));
}

/* get a scroll item from the stack */
LVAL xsgetscrollitem()
{
  LVAL x;
  x = xlgetarg();
  if (! scroll_item_p(x)) xlerror("not a scroll item", x);
  return(x);
}

/* :ISNEW Method */
LVAL xsscroll_item_isnew()
{
  LVAL item, low, high, page;
  int width, height;
  
  item = xsgetscrollitem();
  
  get_initial_item_values(item, FALSE);
  
  if (! xlgetkeyarg(sk_min_value, &low) || ! fixp(low))
    low = cvfixnum((FIXTYPE) SCROLL_MIN);
  set_slot_value(item, s_min_value, low);
  if (! fixp(slot_value(item, s_value)))
    set_slot_value(item, s_value, low);
  if (! xlgetkeyarg(sk_max_value, &high) || ! fixp(high))
    high = cvfixnum((FIXTYPE) SCROLL_MAX);
  set_slot_value(item, s_max_value, high);
  if (! xlgetkeyarg(sk_page_increment, &page) || ! fixp(page))
    page = cvfixnum((FIXTYPE) SCROLL_PAGE);
  set_slot_value(item, s_page_increment, page);
    
  if (! has_item_size(item)) {
    DialogScrollGetDefaultSize(item, &width, &height);
    set_slot_value(item, s_size, integer_list_2(width, height));
  }
  
  return(item);
}

/* :VALUE Method */
LVAL xsscroll_item_value()
{
  LVAL item;
  int set, value;
  
  item = xsgetscrollitem();
  set = moreargs();
  if (set) value = getfixnum(xlgafixnum());
  xllastarg();
  
  return(DialogScrollItemValue(item, set, value));
}

/* :MAX Method */
LVAL xsscroll_item_max()
{
  LVAL item;
  int set, value;
  
  item = xsgetscrollitem();
  set = moreargs();
  if (set) value = getfixnum(xlgafixnum());
  xllastarg();
  
  return(DialogScrollItemMax(item, set, value));
}

/* :MIN Method */
LVAL xsscroll_item_min()
{
  LVAL item;
  int set, value;
  
  item = xsgetscrollitem();
  set = moreargs();
  if (set) value = getfixnum(xlgafixnum());
  xllastarg();
  
  return(DialogScrollItemMin(item, set, value));
}

/* :SCROLL-ACTION Method */
LVAL xsscroll_item_action()
{ 
  LVAL item, action, result;
  
  item = xsgetdialogitem();
  xllastarg();
  
  action = slot_value(item, s_action);
  result = (action != NIL) ? xlapply(pushargs(action, NIL)) : NIL;
  return(result);
}

/***********************************************************************/
/***********************************************************************/
/**                                                                   **/
/**                       LIST-ITEM-PROTO Methods                     **/
/**                                                                   **/
/***********************************************************************/
/***********************************************************************/

/* Is this a list-item? */
list_item_p(x)
	LVAL x;
{
  return (kind_of_p(x, getvalue(s_list_item_proto)));
}

/* get a list item from the stack */
LVAL xsgetlistitem()
{
  LVAL x;
  x = xlgetarg();
  if (! list_item_p(x)) xlerror("not a list item", x);
  return(x);
}

/* :ISNEW Method */
LVAL xslist_item_isnew()
{
  LVAL item, data, columns;
  int width, height;
  
  item = xsgetlistitem();
  data = xlgetarg();
  
  if (listp(data)) data = coerce_to_vector(data);
  else data = copyarray(data);
  set_slot_value(item, s_list_data, data);
  
  get_initial_item_values(item, FALSE);
  
  if (! xlgetkeyarg(sk_columns, &columns) || ! fixp(columns)
      || getfixnum(columns) < 1) columns = cvfixnum((FIXTYPE) 1);
  set_slot_value(item, s_columns, columns);
  
  if (! has_item_size(item)) {
    DialogListGetDefaultSize(item, &width, &height);
    set_slot_value(item, s_size, integer_list_2(width, height));
  }
  
  return(item);
}

/* :DO-ACTION Method */
LVAL xslist_item_action()
{ 
  LVAL item, action, double_click, result;
  item = xsgetlistitem();
  double_click = (moreargs() && xlgetarg() != NIL) ? s_true : NIL;
  xllastarg();
  
  action = slot_value(item, s_action);
  result = (action != NIL) ? xsfuncall1(action, double_click) : NIL;
  return(result);
}

/* :SET-TEXT Method */
LVAL xslist_item_text()
{
  LVAL item, data, index, value;
  char *text;
    
  item = xsgetlistitem();
  index = xlgetarg();
  value = xlgastring();
  text = (char *) getstring(value);
  xllastarg();

  data = slot_value(item, s_list_data);
  if (simplevectorp(data))
    setelement(data, rowmajorindex(data, consa(index), FALSE), value);
  else if (arrayp(data))
    setelement(arraydata(data), rowmajorindex(data, index, FALSE), value);
  else xlerror("not an array", data);
  DialogListItemSetText(item, index, text);
  return(NIL);
}

/* :SELECTION Method */
LVAL xslist_item_selection()
{
  LVAL item, index;
  int set;
  
  item = xsgetlistitem();
  set = moreargs();
  if (set) index = xlgetarg();
  xllastarg();
  
  return(DialogListItemSelection(item, set, index));
}
