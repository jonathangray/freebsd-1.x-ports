/* hardwareobjects - Lisp representation of physical machine objects.  */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

/*
Physical machine objects, such as windows or menus, allocated in a 
session are represented as xlisp objects. A list of such objects is
kept in the variable *HARDWARE-OBJECTS*. Every such object must 
understand the :ALLOCATE and :DISPOSE messages. When an object is
created it is entered into the list; when it is disposed it must 
remove itself from the list. The object is assumed to have an
instance variable HARDWARE-ADDRESS that will be set to the 
representation described below when the object is allocated and to
NIL otherwise.

Representations for the machine address include information to allow
determination of whether an address is valid or not. This is as
protection from referencing objects from a previous session after a 
restore. The restore function will be modified to deallocate all
objects in the old *HARDWARE-OBJECTS* list before the restore and 
reallocate objects in this list after the restore. The menu bar may
require special handling.
*/

#include "xlisp.h"

/* external variables */
extern LVAL s_true;
extern long time_stamp;
extern LVAL s_hardware_address, s_hardware_objects, sk_clobber;

/* external function */
extern LVAL list2(), xdelete(), slot_value(), xscallsubr2();

#ifndef nil
#define nil 0L
#endif

#define NONE -1
#define WINDOW 0
#define IVIEWWINDOW 1
#define IVIEW 2
#define SPINNER 3
#define SCATMAT 4
#define HISTOGRAM 5
#define NAMELIST 6

#define MENU 7
#define APPLEMENU 8
#define DIALOG 9
#define EDIT 10
#define DISPLAY 11

static int window_data[] = { WINDOW, NONE };
static int *window = window_data;
static int iview_window_data[] = { WINDOW, IVIEWWINDOW, NONE };
static int *iview_window = iview_window_data;
static int iview_data[] = { WINDOW, IVIEWWINDOW, IVIEW, NONE };
static int *iview = iview_data;
static int menu_data[] = { MENU, NONE };
static int *menu = menu_data;
static int apple_menu_data[] = { MENU, APPLEMENU, NONE };
static int *apple_menu = apple_menu_data;
static int dialog_data[] = { WINDOW, DIALOG, NONE };
static int *dialog = dialog_data;
static int edit_window_data[] = { WINDOW, EDIT, NONE };
static int *edit_window = edit_window_data;
static int display_window_data[] = { WINDOW, DISPLAY, NONE };
static int *display_window = display_window_data;

/**************************************************************************/
/**                                                                      **/
/**                       General Address Functions                      **/
/**                                                                      **/
/**************************************************************************/

/*
Addresses are stored in a list of the form

  (time-stamp address xlisp-object ....)
  
Additional entries give information about the specific type of the object.
*/

LOCAL valid_hardware_address(addr, type)
	LVAL addr;
	int *type;
{
  LVAL val;

  if (! consp(addr)) return(FALSE);
  val = car(addr);
  if (! fixp(val) || time_stamp != getfixnum(val)) return(FALSE);
  addr = cdr(addr);
  if (! consp(addr) || ! fixp(car(addr))) return(FALSE);
  addr = cdr(addr);
  if (! consp(addr) || ! objectp(car(addr))) return(FALSE);
  addr = cdr(addr);

  for (; *type != NONE; type++, addr = cdr(addr)) {
    if (! consp(addr)) return(FALSE);
    val = car(addr);
    if (! fixp(val) || getfixnum(val) != *type) return(FALSE);
  }
  return(TRUE);
}

LOCAL set_hardware_address(ptr, object, type)
	char *ptr;
	LVAL object;
	int *type;
{
  LVAL t, p, last, result, oblistsym, newoblist;
  
  if (! objectp(object)) xlerror("not an object", object);
  
  oblistsym = s_hardware_objects;
  if (! consp(getvalue(oblistsym))) setvalue(oblistsym, NIL);
  
  xlstkcheck(4);
  xlsave(t);
  xlsave(p);
  xlsave(result);
  xlsave(newoblist);
  
  t = cvfixnum((FIXTYPE) time_stamp);
  p = cvfixnum((FIXTYPE) ptr);
  result = last = consa(object);
  result = cons(p, result);
  result = cons(t, result);
  
  newoblist = cons(result, getvalue(oblistsym));
  setvalue(oblistsym, newoblist);
  set_slot_value(object, s_hardware_address, result);
  
  for (;*type != NONE; type++, last = cdr(last)) {
    t = cvfixnum((FIXTYPE) *type);
    t = consa(t);
    rplacd(last, t);
  }
  xlpopn(4);
}

standard_hardware_clobber(object)
	LVAL object;
{
  LVAL addr, oblistsym, oblist;
  
  if (! objectp(object)) xlerror("not an object", object);
  
  addr = slot_value(object, s_hardware_address);
  
  oblistsym = xlenter("*HARDWARE-OBJECTS*");
  oblist = getvalue(oblistsym);
  if (! listp(oblist)) xlerror("not a list", oblist);
  
  setvalue(oblistsym, xscallsubr2(xdelete, addr, oblist));
  set_slot_value(object, s_hardware_address, NIL);
  
  send_message(object, sk_clobber);
}

LVAL get_hardware_object_by_address(ptr)
	char *ptr;
{
  LVAL oblist = getvalue(s_hardware_objects);
  LVAL result, addr;
  
  for (result = NIL; result == NIL && consp(oblist); oblist = cdr(oblist)) {
    addr = car(oblist);
    if (ptr == (char *) getfixnum(car(cdr(addr)))) result = car(cdr(cdr(addr)));
  }
  return(result);
}
    
/**************************************************************************/
/**                                                                      **/
/**                       Window Address Functions                       **/
/**                                                                      **/
/**************************************************************************/

valid_window_address(addr)
	LVAL addr;
{
  return(valid_hardware_address(addr, window));
}

set_window_address(ptr, object)
	char *ptr;
	LVAL object;
{
  set_hardware_address(ptr, object, window);
}

char *GETWINDOWADDRESS(object)
	LVAL object;
{
  LVAL addr = slot_value(object, s_hardware_address);
  if (addr == NIL) return(nil);
  if (! valid_window_address(addr))
    xlfail("not a valid window address - try reallocating the object");
  return((char *) getfixnum(car(cdr(addr))));
}

/**************************************************************************/
/**                                                                      **/
/**                    IView Window Address Functions                    **/
/**                                                                      **/
/**************************************************************************/

valid_iview_window_address(addr)
	LVAL addr;
{
  return(valid_hardware_address(addr, iview_window));
}

set_iview_window_address(ptr, object)
	char *ptr;
	LVAL object;
{
  set_hardware_address(ptr, object, iview_window);
}

char *GETIVIEWWINDOWADDRESS(object)
	LVAL object;
{
  LVAL addr = slot_value(object, s_hardware_address);
  if (addr == NIL) return(nil);
  else if (! valid_iview_window_address(addr))
    xlfail("not a valid graph window address - try reallocating the object");
  return((char *) getfixnum(car(cdr(addr))));
}

/**************************************************************************/
/**                                                                      **/
/**                        IView Address Functions                       **/
/**                                                                      **/
/**************************************************************************/

valid_iview_address(addr)
	LVAL addr;
{
  return(valid_hardware_address(addr, iview));
}

set_iview_address(ptr, object)
	char *ptr;
	LVAL object;
{
  set_hardware_address(ptr, object, iview);
}

char *get_iview_address(object)
	LVAL object;
{
  LVAL addr;
  
  addr = slot_value(object, s_hardware_address);
  if (! valid_iview_address(addr))
    xlfail("not a valid graph address - try reallocating the object");
  return((char *) getfixnum(car(cdr(addr))));
}

char *GETIVIEWADDRESS(object)
	LVAL object;
{
  LVAL addr = slot_value(object, s_hardware_address);
  if (addr == NIL) return(nil);
  else if (! valid_iview_address(addr))
    xlfail("not a valid graph address - try reallocating the object");
  else return((char *) getfixnum(car(cdr(addr))));
}

/**************************************************************************/
/**                                                                      **/
/**                        Menu Address Functions                        **/
/**                                                                      **/
/**************************************************************************/

valid_menu_address(addr)
	LVAL addr;
{
  return(valid_hardware_address(addr, menu));
}

set_menu_address(ptr, object)
	char *ptr;
	LVAL object;
{
  set_hardware_address(ptr, object, menu);
}

char *get_menu_address(object)
	LVAL object;
{
  LVAL addr;
  
  addr = slot_value(object, s_hardware_address);
  if (! valid_menu_address(addr))
    xlfail("not a valid menu address - try reallocating the object");
  return((char *) getfixnum(car(cdr(addr))));
}
#ifdef MACINTOSH
/**************************************************************************/
/**                                                                      **/
/**                    Apple Menu Address Functions                      **/
/**                                                                      **/
/**************************************************************************/

valid_apple_menu_address(addr)
	LVAL addr;
{
  return(valid_hardware_address(addr, apple_menu));
}

set_apple_menu_address(ptr, object)
	char *ptr;
	LVAL object;
{
  set_hardware_address(ptr, object, apple_menu);
}

char *get_apple_menu_address(object)
	LVAL object;
{
  LVAL addr;
  
  addr = slot_value(object, s_hardware_address);
  if (! valid_apple_menu_address(addr))
    xlfail("not a valid apple menu address - try reallocating the object");
  return((char *) getfixnum(car(cdr(addr))));
}
#endif MACINTOSH
#ifdef AMIGA
/**************************************************************************/
/**                                                                      **/
/**                    Amiga Menu Address Functions                      **/
/**                                                                      **/
/**************************************************************************/
/*
int valid_amiga_menu_address(addr)
	LVAL addr;
{
  return(valid_hardware_address(addr, amiga_menu));
}

void set_amiga_menu_address(ptr, object)
	void *ptr;
	LVAL object;
{
  set_hardware_address(ptr, object, amiga_menu);
}

void *get_amiga_menu_address(object)
	LVAL object;
{
  LVAL addr;
  
  addr = slot_value(object, s_hardware_address);
  if (! valid_amiga_menu_address(addr))
    xlfail("not a valid Amiga menu address - try reallocating the object");
  return((void *)getfixnum(car(cdr(addr))));
}

void set_amiga_menu_window(ptr, object)
	void *ptr;
	LVAL object;
{
  set_hardware_address(ptr, object, window);
}

void *get_amiga_menu_window(object)
	LVAL object;
{
  LVAL addr;
  
  addr = slot_value(object, s_hardware_address);
  if (! valid_window_address(addr))
    xlfail("not a valid window address - try reallocating the object");
  return((void *)getfixnum(car(cdr(addr))));
}*/
#endif AMIGA
/**************************************************************************/
/**                                                                      **/
/**                        Dialog Address Functions                      **/
/**                                                                      **/
/**************************************************************************/

valid_dialog_address(addr)
	LVAL addr;
{
  return(valid_hardware_address(addr, dialog));
}

set_dialog_address(ptr, object)
	char *ptr;
	LVAL object;
{
  set_hardware_address(ptr, object, dialog);
}

char *GETDIALOGADDRESS(object)
	LVAL object;
{
  LVAL addr = slot_value(object, s_hardware_address);
  if (addr == NIL) return(nil);
  if (! valid_dialog_address(addr))
    xlfail("not a valid dialog address - try reallocating the object");
  return((char *) getfixnum(car(cdr(addr))));
}

#ifdef MACINTOSH
/**************************************************************************/
/**                                                                      **/
/**                      Edit Window Address Functions                   **/
/**                                                                      **/
/**************************************************************************/

valid_edit_window_address(addr)
	LVAL addr;
{
  return(valid_hardware_address(addr, edit_window));
}

set_edit_window_address(ptr, object)
	char *ptr;
	LVAL object;
{
  set_hardware_address(ptr, object, edit_window);
}

char *get_edit_window_address(object)
	LVAL object;
{
  LVAL addr;
  
  addr = slot_value(object, s_hardware_address);
  if (! valid_edit_window_address(addr))
    xlfail("not a valid edit window address - try reallocating the object");
  return((char *) getfixnum(car(cdr(addr))));
}

/**************************************************************************/
/**                                                                      **/
/**                    Display Window Address Functions                  **/
/**                                                                      **/
/**************************************************************************/

valid_display_window_address(addr)
	LVAL addr;
{
  return(valid_hardware_address(addr, display_window));
}

set_display_window_address(ptr, object)
	char *ptr;
	LVAL object;
{
  set_hardware_address(ptr, object, display_window);
}

char *get_display_window_address(object)
	LVAL object;
{
  LVAL addr;
  
  addr = slot_value(object, s_hardware_address);
  if (! valid_display_window_address(addr))
    xlfail("not a valid display window address - try reallocating the object");
  return((char *) getfixnum(car(cdr(addr))));
}
#endif MACINTOSH

