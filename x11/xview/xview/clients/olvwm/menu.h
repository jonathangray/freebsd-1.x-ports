/*
 *      (c) Copyright 1989 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ident	"@(#)menu.h	26.24	91/09/14 SMI"

/* a handy typedef for pointers to functions returning int */
typedef int (*FuncPtr) ();

#define NOBUTTON	-1	/* no button is active */
#define PINBUTTON	-2	/* the pin is active */

/* Button stacks (menus) are implemented as lists of buttons.
 * Each button in a stack may in turn be stacked, this is indicated by
 * the stacked flag of the button. If this flag is True then the buttonAction
 * is a pointer to a new stack. Otherwise it is the function to be called
 * after the menu has been dispatched.
 */
typedef struct {
    FuncPtr     callback;	/* if not stacked; call this */
    struct _menu *submenu;	/* Menu hasn't been defined yet */
}           ButtonAction;

/*
 * Button Label:  a Label may be either a string or a pixmap or both
 *     Items which don't have a type are not active (like the 2nd choice
 *	of a non-toggle item)
 */

typedef enum {NoType, StringLabel, ImageLabel, ComboLabel} ButtonLabelKind;

typedef struct _buttonlabel {
    ButtonLabelKind	kind;
    char		*string;
    Pixlabel		*pixlabel;
} ButtonLabel;

/*
 * Button
 */
typedef struct _button {
    ButtonLabel label[2];	/* displayed label, alternate label */
    char       *helpstring[2];	/* help for that button, alternate help */
    int		which;		/* which label to display */
    Bool        stacked;	/* True if this is a button stack */
    Bool	enabled;	/* Enabled/Disabled */
    Bool	visible;	/* is this item visible at all? */
    ButtonAction action;	/* proc or submenu */
    FuncPtr	generate_func;	/* If !NULL, called when this button is
				 * hilighted to re-initialize action.submenu */
    char	*generate_args;	/* args for generate function */
}           Button;

/*
 * Menu
 */
typedef struct _menu {
    char       *title;
    Button    **buttons;
    int         buttonCount;
    int         buttonDefault;
    Bool        hasPushPin;
    Bool	menudirty;	/* needs size recalced */
    char       *helpstring;
    int		btnPerCol;	/* Number buttons per column */
    int		maxLabWidth;	/* Maximum label width */
    int		maxLabHeight;	/* Maximum button/label height */
    int		prefColSize;		/* # of preferred columns */
}           Menu;

#define basewin_openitem	0
#define basewin_zoomitem	(basewin_openitem + 1)
#define basewin_moveitem	(basewin_zoomitem + 1)
#define basewin_resizeitem	(basewin_moveitem + 1)
#define basewin_propsitem	(basewin_resizeitem + 1)
#define basewin_backitem	(basewin_propsitem + 1)
#define basewin_refreshitem	(basewin_backitem + 1)
#define basewin_stickyitem	(basewin_refreshitem + 1)
#define basewin_quititem	(basewin_stickyitem + 1)
#define basewin_ctbuttons	(basewin_quititem + 1)

#define popup_dismissitem	0
#define popup_moveitem		(popup_dismissitem + 1)
#define popup_resizeitem	(popup_moveitem + 1)
#define popup_backitem		(popup_resizeitem + 1)
#define popup_refreshitem	(popup_backitem + 1)
#define popup_quititem		(popup_refreshitem + 1)
#define popup_ctbuttons		(popup_quititem + 1)

#define limit_dismissthisitem	0
#define limit_dismissallitem	(limit_dismissthisitem + 1)
#define limit_ctbuttons		(limit_dismissallitem + 1)

/*
 * Global array of menus
 *
 * We need screen specific menus to get pixmaps in them, so this table
 * has been moved to screen.h
extern Menu *MenuTable[NUM_MENUS];
 */
extern Bool flDoSetDefault;	/*is the ctrl key down (only used in winpinmenu.c)*/

/*
 * constants used by ShowStandardMenuSync()
 */
#define SYNC_DONE		0
#define SYNC_CHANGECLICK	(SYNC_DONE + 1)

/*
 * ButtonInfo
 */
typedef struct _buttonInfo {
    Button     *button;
    int         buttonX, buttonY;
    int		buttonHeight;
    Bool	flDirty;	/*damaged*/
    Bool	framed;		/* True if button should be framed */
    struct _menuInfo *subMenu;
}           ButtonInfo;

/*
 * MenuInfo
 */
typedef struct _menuInfo {
    int         depth;
    Menu       *menu;
    ButtonInfo *buttons;
    int         notitleOffset, buttonOffset;
    int         menuX, menuY, menuWidth, menuHeight;
    int         titleX, titleY, titleHeight, titleWidth;
    int         pushPinX, pushPinY;
    int		maxbuttonWidth;
    struct _wingeneric *menuWin;
    struct _menuInfo *origmenuInfo;
    struct _menuInfo *pinnedBrother;
    Bool        childActive;
    Bool        pinIn;
    int         litButton;
    Bool        ignoreNextExpose;
    SemanticAction action;
    int         ringedButton;
    int		numColumns;
}           MenuInfo;

/*
 * MenuCache - per screen
 */
typedef struct _menuCache {
    MenuInfo   **menuInfoList;	/* dynamic */
    int         nextSlot;
    int		maxSlots;	/* how big is menuInfoList */ 
    struct _winmenu **menuWinList;
    int         maxDepth;
}           MenuCache;

typedef enum _menuTrackMode {
    MODE_DRAG,	/* Press-Drag-Release */
    MODE_CLICK	/* Click-Move-Click */
}           MenuTrackMode;

typedef enum _menuLocation {
    ML_BUTTON,	/* On a button */
    ML_PIN,/* On the pin */
    ML_MENU,	/* Elsewhere on the menu */
    ML_OFFMENU,	/* Outside the menu entirely */
    ML_BUTTONDISABLED /*on a disabled button*/
}           MenuLocation;

#define BUTTON_INDEX_OK(mi,idx) ((idx)>=0 && (idx)<(mi)->menu->buttonCount)

#ifdef notdef
/*
 * Default button list
 */
typedef struct _defaults {
    char        Name[80];
    int         DefaultButton;
    MenuInfo   *mInfo;
    struct _defaults *next;
}           Defaults, *DefaultsP;
#endif

/*****************************************/


/*
 *	 External functions
 */
void SetMenuDefault();
Menu *NewNamedMenu();
Bool AppendMenuItem();
Menu *CreateMenu();
Menu *GetEnabledMenu();
MenuInfo *MenuInfoCreate();
void ShowStandardMenu();
void ShowStandardMenuSync();
void SetClickCallback();

extern void InitMenus();
extern MenuCache *InitScreenMenus( /* Display *dpy, ScreenInfo *scrInfo */ );
extern void MenuCreate( /* dpy, menu */ );
extern void MenuShow( /* dpy, WinGeneric, menu, event */ );
extern void SetButton( /* dpy, menu, bindex, Bool */ );
extern void ExecButtonAction( /* dpy, winInfo, menu, btn, Bool */ );
extern void DrawMenu( /* dpy, menu */ );
extern int  PointInRect( /* x, y, rx, ry, rw, rh */ );


/*
 * generically useful region code that happens to live in menu.c
 */

void InitRegions();
void EmptyRegion();
void RectRegion();
void AppendExposeDamage();
void MakeExposeDamage();


/*
 *	WinMenu Functions (from winmenu.c)
 */
extern struct _winmenu *
MakeMenu( /* Display *dpy, 
	     WinRoot *winInfo */ 
	 );
extern void
MapMenuWindow(/* Display *dpy, 
		 WinMenu *winInfo, 
		 MenuInfo *menuInfo */ 
	      );
extern void
UnmapMenuWindow(/* Display *dpy, 
		   WinMenu *winInfo, 
		   MenuInfo *menuInfo */ 
		);


int MenuEventExpose();
int MenuEventDrawMenu();


/*
 *	WinPinMenu Functions (from winpinmenu.c)
 */
extern struct _winpinmenu *
MakePinMenu(/* Display *dpy, 
	       WinRoot *winInfo, 
	       MenuInfo *menuInfo */ 
	    );


/*
 * macros for setting menu items
 */

#define DirtyMenu(pmenu)	(pmenu)->menudirty = True
#define _menuset(p,i,q,x)	do { \
				   Button *pb = p->buttons[i];	\
				   if (pb->q != (x)) {		\
				      DirtyMenu(p);		\
				      pb->q = (x);		\
				      }				\
				} while(0)
#define ToggleVisible(p,i,x)	_menuset(p,i,visible,(x))
#define ToggleItem(p,i,x)	_menuset(p,i,which,(((x) == 0)? False : True))
#define ToggleEnabled(p,i,x)	_menuset(p,i,enabled,(x))
#define SetMenuTitle(m,t)	do {  \
				   if (m->title != t) {		\
				      m->title = t;		\
				      DirtyMenu(m);		\
				   }				\
			 	} while (0)

/*
 * This stuff really belongs in the olgx library (in olgx.h, in fact).
 * But I don't want to ship that whole library, so I put it here and
 * included only ol_button.c from olgx
 */

#define OLGX_LABEL_IS_COMB	0x0400	/* Label ix pixmap/string combination */

typedef struct _comblabel {
    Pixlabel	pixlabel;
    char	*strlabel;
} Comblabel;

/*
 * eventX, eventY, eventTime
 *
 * Extract the xroot, yroot, or timestamp fields from an event, assuming it's
 * a MotionNotify, ButtonPress, or ButtonRelease.
 */

#define eventX(e)       ((e)->type == MotionNotify ? (e)->xmotion.x_root \
						   : (e)->xbutton.x_root )

#define eventY(e)       ((e)->type == MotionNotify ? (e)->xmotion.y_root \
						   : (e)->xbutton.y_root )

#define eventTime(e)    ((e)->type == MotionNotify ? (e)->xmotion.time \
						   : (e)->xbutton.time )
