/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

# include "xlisp.h"

#ifdef MACINTOSH
#ifdef MPWC
# include <Quickdraw.h>
# include <Windows.h>
# include <Controls.h>
# include <Menus.h>
# include <Dialogs.h>
# include <Events.h>
# include <OSEvents.h>
# include <Lists.h>
# include <Memory.h>
# include <Fonts.h>
# include <Script.h>
# define thePort qd.thePort
# define arrow qd.arrow
# define MBarHeight GetMBarHeight()
# define MemErr MemError()
# define RETURNCHAR '\n'
#else
# undef FALSE /* to avoid some conflicts */
# undef TRUE
# include <WindowMgr.h>	/* pulls in MacTypes, QuickDraw */
# include <ControlMgr.h>
# include <MenuMgr.h>
# include <DialogMgr.h>
# include <EventMgr.h>
# include <ListMgr.h>
# include <MemoryMgr.h>
# include <FontMgr.h>
# include <pascal.h>
# define FALSE 0
# define TRUE 1
# define ModalFilterProcPtr ProcPtr
# define RETURNCHAR '\r'
#endif MPWC
#else
#ifdef SUNVIEW
#undef SUNVIEW  /* to avoid a conflict in include files */
#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <suntool/panel.h>
#include <pixrect/pixrect_hs.h>

typedef struct {
  int h, v;
} Point;

#else
typedef struct {
  int h, v;
} Point;
typedef struct {
  int left, right, width, height;
} Rect;
#endif 
#endif

#ifndef nil
#define nil 0L
#endif nil

extern LVAL s_title, s_type, s_go_away, s_items, s_default_button, 
  s_true, s_text, s_location, s_size, s_action, s_dialog,
  s_hardware_address, s_multiple, s_min_value, s_max_value, 
  s_page_increment, s_editable, s_value, s_list_data, s_columns,
  sk_do_action, sk_scroll_action, s_dialog_proto, s_dialog_item_proto,
  sk_action, s_button_item_proto, s_toggle_item_proto, s_text_item_proto,
  sk_editable, s_choice_item_proto, s_scroll_item_proto,
  sk_min_value, sk_max_value, sk_page_increment, s_list_item_proto,
  sk_columns, s_modeless, sk_allocate, s_modal, sk_close, sk_activate;
extern char buf[];

extern LVAL xlclass(), xremove(), xfuncall(), xmsend(), slot_value(),
  make_string(), integer_list_2();
extern LVAL list2(), xsapplysubr(), xsadjoin(),
	displacedarraydim(), arraydata(), getnextelement(), list3();
extern Rect ListToRect();
extern long GETDIALOGADDRESS();
extern Point ListToPoint();
extern LVAL PointToList();
extern LVAL DialogGetModalItem(), DialogToggleItemValue(),
  DialogTextItemText(), DialogChoiceItemValue(), DialogScrollItemValue(),
  DialogScrollItemMax(), DialogScrollItemMin(), DialogListItemSelection();

/***********************************************************************/
/**                                                                   **/
/**                    DIALOG-ITEM-PROTO Definitions                  **/
/**                                                                   **/
/***********************************************************************/

/* dialog item types */
# define NULL_ITEM        0
# define BUTTON_ITEM      1
# define TOGGLE_ITEM      2
# define CHOICE_ITEM      3
# define MESSAGE_ITEM     4
# define TEXT_ITEM        5
# define SCROLL_ITEM      6
# define REAL_SCROLL_ITEM 7
# define LIST_ITEM        8
# define ITEM_LIST        9

# define has_item_location(i) (slot_value(i, s_location) != NIL)
# define has_item_size(i) (slot_value(i, s_size) != NIL)
# define check_dialog_address(d) valid_dialog_address(slot_value(d, s_hardware_address))

/***********************************************************************/
/*                    Machine Dependent Definitions                    */
/***********************************************************************/

#ifdef MACINTOSH
typedef struct {
  LVAL object;
  int count;
} DialogData;

typedef struct {
  int type;
  int itemNumber, clusterLeader, clusterSize;
  Handle itemHandle;
  LVAL object;
} DialogItemData;

#define DialogItemCount(d) (**(short **)(((DialogPeek) d)->items) + 1)
#define GetDialogObject(d) ((*((DialogData **) GetWRefCon(d)))->object)
#define SetDialogObject(d, obj) (GetDialogObject(d) = obj)
#define GetDialogData(d) ((DialogData **) GetWRefCon(d))

extern DialogItemData *GetDialogItemData(), FindItemData();
#endif MACINTOSH

