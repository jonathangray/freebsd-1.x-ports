/*
 * Copyright 1992, 1993 The University of Newcastle upon Tyne
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose other than its commercial exploitation
 * is hereby granted without fee, provided that the above copyright
 * notice appear in all copies and that both that copyright notice and
 * this permission notice appear in supporting documentation, and that
 * the name of The University of Newcastle upon Tyne not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission. The University of
 * Newcastle upon Tyne makes no representations about the suitability of
 * this software for any purpose. It is provided "as is" without express
 * or implied warranty.
 * 
 * THE UNIVERSITY OF NEWCASTLE UPON TYNE DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF
 * NEWCASTLE UPON TYNE BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 * 
 * Author:  Jim Wight (j.k.wight@newcastle.ac.uk)
 *          Department of Computing Science
 *          University of Newcastle upon Tyne, UK
 */

#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/IntrinsicP.h>	
#include <X11/StringDefs.h>	
#include <X11/cursorfont.h>	
#include <X11/Xaw/Label.h>
#include <AxeWindow.h>
#include <X11/Xp/Table.h>
#include <AxeIntrinsic.h>
#include <AxeMenuBtn.h>
#include <AxeSimMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#include <AxeSmeBSB.h>
#include <AxeCommand.h>
#include <AxeTextDeck.h>
#include <AxeText.h>
#include <AxeiiText.h>
#include <Confirmer.h>
#include <FileNom.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/errno.h>
#include <signal.h>
extern int errno;
#ifndef __BSD_4_4__
extern char *sys_errlist[];
#endif
#include <stdio.h>

#ifdef EXTENSION
#include "Language.h"
#endif

#include "AxeEditorP.h"
extern char *getenv();

#include "regexp.h"
#include "util.h"

#include "version.h"

#include "axeLogo.xbm"
#include "fwdArrow.xbm"
#include "bwdArrow.xbm"
#include "vtCentre.xbm"
#include "hzCentre.xbm"
#include "tickMark.xbm"
#include "modMark.xbm"

static Boolean validAction;

/*
 * The names in the first column must be the same
 *  as those in the defaultMenuList that follows
 */
static MenuRecord chosenMenus[] = {
    "move",     False,
    "search",   False,
    "insert",   False,
    "delete",   False,
    "help",     False,
    "misc",     False,
    "font",     False,
    "window",   False,
    "buffer",   False,
    "show",     False,
    "file",     False,
    "quit",     False,
    0,          False,
};

#define defaultMenuList "move search insert delete help misc font window \
                         buffer show file quit logo"

#define defaultFontList "Small:6x13  Medium:8x13  Large:9x15  Huge:10x20"

#define defaultMiniMenu "print:lpr spell nroff mail | \
                         exec:mini-commit abort:mini-abort | \
                        print<RET>:lpr\n spell<RET>:spell\n nroff<RET>:nroff\n"

#define Offset(field) XtOffsetOf(AxeEditorRec, axeEditor.field)

static XtResource resources[] = {
    {XtNfile, XtCFile, XtRString, sizeof(String),
         Offset(file), XtRString, NULL},
    {XtNmenuList, XtCMenuList, XtRString, sizeof(String),
         Offset(menu_list), XtRString, defaultMenuList},
    {XtNbuttons, XtCButtons, XtRBoolean, sizeof(Boolean),
         Offset(buttons), XtRImmediate, (XtPointer) False},
    {XtNbuttonList, XtCButtonList, XtRString, sizeof(String),
         Offset(button_list), XtRString, NULL},
    {XtNinfoTimeout, XtCInfoTimeout, XtRInt, sizeof(int),
         Offset(info_timeout), XtRImmediate, (XtPointer) 0},
    {XtNfocusToText, XtCFocusToText, XtRBoolean, sizeof(Boolean),
         Offset(focus_to_text), XtRImmediate, (XtPointer) False},
    {XtNsuppressFilename, XtCSuppressPane, XtRBoolean, sizeof(Boolean),
         Offset(suppressFilename), XtRImmediate, (XtPointer) False},
    {XtNsuppressInfobar, XtCSuppressPane, XtRBoolean, sizeof(Boolean),
         Offset(suppressInfobar), XtRImmediate, (XtPointer) False},
    {XtNsuppressMinibuffer, XtCSuppressPane, XtRBoolean, sizeof(Boolean),
         Offset(suppressMinibuffer), XtRImmediate, (XtPointer) False},
    {XtNfontList, XtCFontList, XtRString, sizeof(String),
         Offset(font_list), XtRString, defaultFontList},
    {XtNinternalBorderWidth, XtCBorderWidth, XtRDimension, sizeof(Dimension),
         Offset(internal_border_width), XtRImmediate, (XtPointer) 1},
    {XtNfullPathnames, XtCFullPathnames, XtRBoolean, sizeof(Boolean),
         Offset(full_pathnames), XtRImmediate, (XtPointer) False},
    {XtNchangeCallback, XtCCallback, XtRCallback, sizeof(XtPointer), 
         Offset(change_callbacks), XtRCallback, (XtPointer) NULL},
};

#undef Offset

#define INCREMENT 5
#define MAXSTRLEN 16
#define CHUNK 10

#define EDITORS axeEditorClassRec.axeEditor_class.editors
#define NUMBEROFEDITORS axeEditorClassRec.axeEditor_class.numberOfEditors
#define MAXEDITORS axeEditorClassRec.axeEditor_class.maxEditors

#define CLASS(field) axeEditorClassRec.axeEditor_class.field
#define PRIVATE(w,field) (((AxeEditorWidget) w)->axeEditor.field)

static void ClassInitialize(), PseudoClassInitialize(), Initialize();
static void CalculateSize(), Resize(), Realize();
static void Destroy();
static XtGeometryResult GeometryManager();
static void InsertChild(), DeleteChild();

static XpTableLoc MakeButtonsLayout();
static void NewEditor(), DeckChange(), SetKeyboardFocus();
static Widget MakeMenu();
static void MakeOneMenu();
static void MakeFontMenu(), MakeShowMenu(), MakeLogoMenu(), MakeMiniMenu();
static void MakeButtons();

static void RePopulateShowMenu(), FreeFontList();
static FontListStruct *ParseFontList();

static void UpdateFileName(), UpdateTitles();
static Widget AxeEditorOf(), ShellOf();
static String HomeFile();
String AxeEditorExpandName();

static void Help();
static void NewWindow(), CloseWindow(), CloseAll(), IconifyAll(), Restack();
static void NewBuffer(), CloseBuffer();
static void SaveAndClose(), SaveAll();
static void miniSearch(), miniInsert(), miniSaveAs(), miniLoad(), miniPipe();
static void miniClear(), miniSelect(), miniAbort(), MiniLeave(), MiniCommit();
static void miniShell(), miniStuff();
#ifdef EXTENSION
static int MiniInterp();
#endif
static void MiniUninstallAccelerators();
static void startMacro(), endMacro(), execMacro(), repeatMacro();
static void UpdateInfo(), UpdateInfoBar();

static char commandButtonsLayout[] = "\
  mbeg       0 1 wW; sbtext     1 1 wW;    ifile      2 1 wW; \
  mend       0 2 wW; sftext     1 2 wW;    isel       2 2 wW; \
  mup        0 3 wW; sline      1 3 wW;    ipaste     2 3 wW; \
  mdown      0 4 wW; scaret     1 4 wW;    ictrl      2 4 wW; \
  mtop       0 5 wW; sbsel      1 5 wW; \
  mbot       0 6 wW; sfsel      1 6 wW; \
\
  dword      3 1 wW; hgnrl      4 1 wW;    mundo      5 1 wW; \
  dline      3 2 wW; hbind      4 2 wW;    mwhere     5 2 wW; \
  dsel       3 3 wW; hcust      4 3 wW;    mform      5 3 wW; \
  dcut       3 4 wW; hpop       4 4 wW;    mcentre    5 4 wW; \
                     hextn      4 5 wW;    mhcentr    5 5 wW; \
                     hchng      4 6 wW;    mpref      5 6 wW; \
\
  wnew       7 1 wW; bempty     8 1 wW;    fsvex     10 1 wW; \
  wfull      7 2 wW; bfull      8 2 wW;    fsvall    10 2 wW; \
  wclose     7 3 wW; bclear     8 3 wW;    fsave     10 3 wW; \
  wclall     7 4 wW; bclose     8 4 wW;    fsvas     10 4 wW; \
  wicon      7 5 wW;                       fload     10 5 wW; \
  wdeicon    7 6 wW;                       frvrt     10 6 wW; \
\
  qquit     11 1 wW; \
  qsvex     11 2 wW; \
  qsvclw    11 3 wW; \
  qsvclb    11 4 wW;";

static ButtonRecord buttonActions[] = {
"mMove",  NULL,
        "mbeg",    "beginning-of-line",
        "mend",    "end-of-line",
        "mup",     "previous-page",
        "mdown",   "next-page",
        "mtop",    "beginning-of-file",
        "mbot",    "end-of-file",

"mSearch", NULL,
        "sbtext",  "search(backward)",
        "sftext",  "search(forward)",
        "sline",   "goto-line",
        "scaret",  "search-caret",
        "sbsel",   "backward-search-selection",
        "sfsel",   "forward-search-selection",

"mInsert", NULL,
        "ifile",   "include-file",
        "isel",    "include-selection",
        "ipaste",  "paste",
        "ictrl",   "insert-control",

"mDelete", NULL,
        "dword",   "delete-word",
        "dline",   "delete-line",
        "dsel",    "delete-selection",
        "dcut",    "kill-selection",

"mHelp",  NULL,
        "hgnrl",   "help(Introduction)",
        "hbind",   "help(Bindings)",
        "hcust",   "help(Customise)",
        "hpop",    "help(Popups)",
#ifdef EXTENSION
        "hextn",   "help(Language)",
#endif
        "hchng",   "help(Changes)",

"mMisc",  NULL,
        "mundo",   "undo",
        "mwhere",  "where",
        "mform",   "form-paragraph",
        "mcentre", "redraw-display",
        "mhcentr", "centre-line",
        "mpref",   "set-preferences",

"mWindow", NULL,
        "wnew",    "new-window",
        "wfull",   "new-window(load)",
        "wclose",  "close-window",
        "wclall",  "close-all",
        "wicon",   "iconify-all(I)",
        "wdeicon", "iconify-all(D)",

"mBuffer", NULL,
        "bempty",  "new-buffer",
        "bfull",   "new-buffer(load)",
        "bclear",  "clear-buffer",
        "bclose",  "close-buffer",

"mFile",  NULL,
        "fsvex",   "save-and-close",
        "fsvall",  "save-all",
        "fsave",   "save-file",
        "fsvas",   "save-as",
        "fload",   "load-file",
        "frvrt",   "reload-file",

"mQuit",  NULL,
        "qquit",   "close-all",
        "qsvex",   "save-and-close",
        "qsvclw",  "save-and-close(window)",
        "qsvclb",  "save-and-close(buffer)",
};

static char miniTranslationsTable[] = "\
                        <Enter>: display-caret(on) update-info(\"search /)fwd ?)bwd; [0-9][0-9]*)goto;  r)ead;  w)rite;  e)dit;  |)pipe;  !)shell\")\n\
                        <Leave>: mini-leave() update-info(\" \")\n\
                        Ctrl<Key>G: mini-abort() \n\
                        <Key>Escape: mini-abort() \n\
                        <Key>Return: mini-commit()";

static char miniAcceleratorsTable[] = "#override \n\
                        Ctrl<Key>A: beginning-of-line() \n\
                        Ctrl<Key>B: backward-character() \n\
                        Ctrl<Key>D: delete-next-character() \n\
                        Ctrl<Key>E: end-of-line() \n\
                        Ctrl<Key>F: forward-character() \n\
                        Ctrl<Key>G: mini-abort() \n\
                        Ctrl<Key>K: kill-to-end-of-line() \n\
                        <Key>Delete: delete-previous-character() \n\
                        <Key>Escape: mini-abort() \n\
                        <Key>Return: mini-commit() \n\
                        <Key>: insert-char()";

static XtActionsRec actions[] = {
    "help",                 Help,

    "new-window",           NewWindow,
    "close-window",         CloseWindow,
    "close-all",            CloseAll,
    "iconify-all",          IconifyAll,
    "deiconify-all",        IconifyAll,
    "restack",              Restack,

    "new-buffer",           NewBuffer,
    "close-buffer",         CloseBuffer,

    "save-and-exit ",       SaveAndClose,  /* backwards compatability */
    "save-and-close",       SaveAndClose,
    "save-all",             SaveAll,

    "mini-search",          miniSearch,
    "mini-insert",          miniInsert,
    "mini-saveas",          miniSaveAs,
    "mini-load",            miniLoad,
    "mini-pipe",            miniPipe,
    "mini-clear",           miniClear,
    "mini-select",          miniSelect,
    "mini-abort",           miniAbort,
    "mini-leave",           MiniLeave,
    "mini-commit",          MiniCommit,
    "mini-shell",           miniShell,
    "mini-stuff",           miniStuff,

    "start-macro",          startMacro,
    "end-macro",            endMacro,
    "exec-macro",           execMacro,
    "repeat-macro",         repeatMacro,

    "update-info",          UpdateInfo,
};

AxeEditorClassRec axeEditorClassRec = {
    /* Core class part */
  {
    /* superclass	     */	(WidgetClass) &compositeClassRec,
    /* class_name	     */ "AxeEditor",
    /* widget_size	     */ sizeof(AxeEditorRec),
    /* class_initialize      */ ClassInitialize,
    /* class_part_initialize */ NULL,
    /* class_inited          */	FALSE,
    /* initialize	     */	Initialize,
    /* initialize_hook       */	NULL,
    /* realize		     */	Realize,
    /* actions		     */	actions,
    /* num_actions	     */	XtNumber(actions),
    /* resources	     */	resources,
    /* num_resources	     */	XtNumber(resources),
    /* xrm_class	     */	NULLQUARK,
    /* compress_motion	     */	TRUE,
    /* compress_exposure     */	XtExposeCompressMultiple,
    /* compress_enterleave   */	TRUE,
    /* visible_interest	     */	FALSE,
    /* destroy		     */	Destroy,
    /* resize		     */	Resize,
    /* expose		     */	XtInheritExpose,
    /* set_values	     */	NULL,
    /* set_values_hook       */	NULL,			
    /* set_values_almost     */	XtInheritSetValuesAlmost,  
    /* get_values_hook       */	NULL,
    /* accept_focus	     */	NULL,
    /* version		     */	XtVersion,
    /* callback offsets      */	NULL,
    /* tm_table              */	NULL,
    /* query_geometry	     */	XtInheritQueryGeometry,
    /* display_accelerator   */	NULL,
    /* extension	     */	NULL,
  },
  {
    /* composite class fields */
    /* geometry_manager   */   GeometryManager,
    /* change_managed     */   XtInheritChangeManaged,
    /* insert_child       */   InsertChild,
    /* delete_child       */   DeleteChild,
    /* extension          */   NULL
  },
   /* AxeEditor class part */
  {
    /* extension            */   NULL
  }
};

WidgetClass axeEditorWidgetClass = (WidgetClass) &axeEditorClassRec;

static void
ClassInitialize()
{
    char *dir;

#if defined(SYSV) || defined(SVR4)
    extern char *getcwd();
#define getwd(buf) getcwd(buf,MAXPATHLEN)
#else
    extern char *getwd();
#endif

    extern char *getenv();

    EDITORS = NULL;
    NUMBEROFEDITORS = 0;
    MAXEDITORS = 0;

    CLASS(show_menu) = (Widget) 0;

    CLASS(mini_translations) = XtParseTranslationTable(miniTranslationsTable);
    CLASS(mini_accelerators) = XtParseAcceleratorTable(miniAcceleratorsTable);

    if ( (dir = getenv("HOME")) )
    {
        CLASS(home_dir) = XtNewString(dir);
    }
    else
    {
        CLASS(home_dir) = NULL;
    }
    CLASS(home_dir_len) = strlen(CLASS(home_dir));

    CLASS(current_dir) = XtMalloc(MAXPATHLEN);
    if (!getwd(CLASS(current_dir)))
    {
	if (CLASS(home_dir))
	{
	    strcpy(CLASS(current_dir), CLASS(home_dir));
	}
	else
	{
	    CLASS(current_dir)[0] = '\0';
	}
    }

    strcat(CLASS(current_dir), "/");
    CLASS(current_dir_len) = strlen(CLASS(current_dir));

    CLASS(show_menu_callbacks) = 0;
    CLASS(show_menu_callback_list) = 0;

    CLASS(default_mini_menu) = 0;
    CLASS(mini_menu) = 0;

    CLASS(macro_id) = (XtActionHookId) 0;
    CLASS(macro_size) = 0;
    CLASS(max_macro_size) = 0;
    CLASS(in_macro_string) = False;
    CLASS(macro_commands) = (MacroList) 0;

    CLASS(show_work_proc) = False;
}
    
static void
PseudoClassInitialize(w)
     Widget w;
{
    Display *display;
    Window rootWindow;
    Widget app;

    if (!CLASS(axeLogo))
    {
	display = XtDisplay(w);
	rootWindow = XDefaultRootWindow(display);

	CLASS(axeLogo) =
	    XCreateBitmapFromData(display, rootWindow, (char *) axeLogo_bits,
				                axeLogo_width, axeLogo_height);

	CLASS(fwdArrow) =
	    XCreateBitmapFromData(display, rootWindow, (char *) fwdArrow_bits,
				              fwdArrow_width, fwdArrow_height);

	CLASS(bwdArrow) =
	    XCreateBitmapFromData(display, rootWindow, (char *) bwdArrow_bits, 
				              bwdArrow_width, bwdArrow_height);

	CLASS(vtCentre) =
	    XCreateBitmapFromData(display, rootWindow, (char *) vtCentre_bits, 
				              vtCentre_width, vtCentre_height);

	CLASS(hzCentre) =
	    XCreateBitmapFromData(display, rootWindow, (char *) hzCentre_bits, 
				              hzCentre_width, hzCentre_height);

	CLASS(tickMark) =
	    XCreateBitmapFromData(display, rootWindow, (char *) tickMark_bits, 
				              tickMark_width, tickMark_height);

	CLASS(modMark) =
	    XCreateBitmapFromData(display, rootWindow, (char *) modMark_bits, 
				                modMark_width, modMark_height);

	while ((app = XtParent(w)))
	{
	    w = app;
	}
	CLASS(menu_parent) = w;
    }
}

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
    Widget req, new;
    ArgList args;
    Cardinal *num_args;
{
    AxeEditorWidget aew = (AxeEditorWidget) new;
    FontListStruct *buttonList = NULL;
    XpTableLoc tableLoc;

    int menu;
    char menuName[32];
    Widget fileName, infoBar, buttons, miniBuffer;
    Widget menuButton, logoButton, miniMenu;

    PseudoClassInitialize(aew);

    NewEditor(aew);

    fileName = PRIVATE(new,file_name) =
	XtVaCreateWidget("fileName", labelWidgetClass, new, NULL);
    if (!PRIVATE(new,suppressFilename))
    {
	XtManageChild(fileName);
    }

    infoBar = PRIVATE(new,info_bar) =
	XtVaCreateWidget("infoBar", labelWidgetClass, new,
			 XtNlabel, QVERSION,
			 NULL);
    PRIVATE(new,info_timer) = (XtIntervalId) 0;
    if (!PRIVATE(new,suppressInfobar))
    {
	XtManageChild(infoBar);
    }

    tableLoc = MakeButtonsLayout(new, &buttonList);
    buttons = PRIVATE(new,menu_bar) =
	XtVaCreateManagedWidget("buttons", xpTableWidgetClass, new,
				XtNlayout, tableLoc,
				NULL);
    XpTableLocFree(tableLoc);

    for (menu = 0;  chosenMenus[menu].name;  ++menu)
    {
	if (chosenMenus[menu].chosen)
	{
	    sprintf(menuName, "%sMenu", chosenMenus[menu].name);
	    menuButton 
		= XtVaCreateManagedWidget(menuName,
				      axeMenuButtonWidgetClass, buttons, NULL);

	    if (strcmp(menuName, "showMenu") == 0 && !CLASS(show_menu))
	    {
		CLASS(show_menu) = MakeMenu(menuButton, CLASS(menu_parent),
					                  MakeShowMenu, False);
	    }
	    else if (strcmp(menuName, "fontMenu") == 0)
	    {
		(void) MakeMenu(menuButton, new, MakeFontMenu, False);
	    }
	    else
	    {
		(void) MakeMenu(menuButton, CLASS(menu_parent), MakeOneMenu,
				                                        False);
	    }
	}
    }
    PRIVATE(new,font_table) = (FontListStruct *) 0;

    logoButton
	= XtVaCreateManagedWidget("axeLogo",
				  axeMenuButtonWidgetClass, buttons,
				  XtNbitmap, CLASS(axeLogo),
				  XtNhelp, QVERSION,
				  NULL);
    (void) MakeMenu(logoButton, CLASS(menu_parent), MakeLogoMenu, False);

    if (PRIVATE(new,buttons) || PRIVATE(new,button_list))
    {
	MakeButtons(buttons, buttonList);
	if (buttonList)
	{
	    FreeFontList(XtDisplay(new), buttonList);
	}
    }

    PRIVATE(new,ed_deck) =
	XtVaCreateManagedWidget("edDeck", axeTextDeckWidgetClass, new,
				XtNfile, PRIVATE(new,file),
				NULL);
    XtAddCallback(PRIVATE(new,ed_deck), XtNchangeCallback, DeckChange,
		                                              (XtPointer) new);
    XtVaGetValues(AxeTextDeckTop(PRIVATE(new,ed_deck)),
		  XtNtranslations, &(PRIVATE(new,defaultTextTranslations)),
		  NULL);

    miniBuffer = PRIVATE(new,mini_buffer) =
	XtVaCreateWidget("miniBuffer", asciiTextWidgetClass, new,
			 XtNaccelerators, CLASS(mini_accelerators),
			 NULL);
    XtOverrideTranslations(PRIVATE(new,mini_buffer), CLASS(mini_translations));

    miniMenu = PRIVATE(new,mini_button) =
	XtVaCreateWidget("miniMenu", axeMenuButtonWidgetClass, new, NULL);
    (void) MakeMenu(miniMenu, CLASS(menu_parent), MakeMiniMenu, False);

    if (!PRIVATE(new,suppressMinibuffer))
    {
	XtManageChild(miniBuffer);
	XtManageChild(miniMenu);
    }
    PRIVATE(new,accelerateMini) = False;

    PRIVATE(new,confirmer) =
	XtVaCreatePopupShell("confirmer", confirmerWidgetClass, new, NULL);

    SetKeyboardFocus(new, AxeTextDeckTop(PRIVATE(new,ed_deck)));

    /* The best we can do to keep Realize happy */
    CalculateSize(aew, &aew->core.width, &aew->core.height);
}

static void
CalculateSize(aew, width, height)
     AxeEditorWidget aew;
     Dimension *width, *height;
{
    Cardinal child;
    Dimension wide, maxWidth, high, totalHeight = 0;
    
    for (child = 0;  child < aew->composite.num_children;  ++child)
    {
	if (XtIsManaged(aew->composite.children[child]))
	{
	    XtVaGetValues(aew->composite.children[child],
			  XtNwidth, &wide,
			  XtNheight, &high,
			  NULL);

	    if (XtIsSubclass(aew->composite.children[child],
			     axeTextDeckWidgetClass))
	    {
		maxWidth = wide;
	    }

	    if (XtIsSubclass(aew->composite.children[child],
			     axeMenuButtonWidgetClass))
	    {
		continue;
	    }

	    totalHeight += high + PRIVATE(aew,internal_border_width);
	}
    }
    *width = maxWidth;
    *height = totalHeight - PRIVATE(aew,internal_border_width);;
}

static void
Configure(aew, deltaHeight, initiator)
     AxeEditorWidget aew;
     Dimension deltaHeight;
     Widget initiator;
{
    /*
     * This routine needs looking at; the addition of the
     * minibuffer MenuButton was done in a bit of a hurry.
     */

    Cardinal child;
    Widget kid;
    Dimension width, height, border;
    Position x = -PRIVATE(aew,internal_border_width);
    Position y = -PRIVATE(aew,internal_border_width);
    Position atx, aty;
    
    for (child = 0;  child < aew->composite.num_children;  ++child)
    {
	kid = aew->composite.children[child];
	if (XtIsManaged(kid))
	{
	    XtVaGetValues(kid,
			  XtNheight, &height,
			  NULL);
    
	    if (XtIsSubclass(kid, axeTextDeckWidgetClass))
	    {
		height += deltaHeight;
	    }

	    if (kid == initiator)
	    {
		kid->core.y = y;
	    }
	    else
	    {
		aty = y;
		border = PRIVATE(aew,internal_border_width);
		if (XtIsSubclass(kid, asciiTextWidgetClass)) /* minibuffer */
		{
		    atx = PRIVATE(aew,mini_button)->core.width;
		    width = aew->core.width - atx + 1;
		}
		else
		{
		    atx = x;
		    if (XtIsSubclass(kid, axeMenuButtonWidgetClass))
		    {
			width = kid->core.width;
			aty = y + PRIVATE(aew,internal_border_width);
			height = PRIVATE(aew,mini_buffer)->core.height;
			border = 0;
		    }
		    else
		    {
			width = aew->core.width;
		    }
		}
		XtConfigureWidget(kid, atx, aty, width, height, border);
	    }
	    if (!XtIsSubclass(kid, asciiTextWidgetClass)) /* order assumed */
	    {
		y += height + PRIVATE(aew,internal_border_width);
	    }
	}
    }
}    

static void
Resize(aew)
    AxeEditorWidget aew;
{
    Dimension  oldWidth, oldHeight;
    
    CalculateSize(aew, &oldWidth, &oldHeight);

    Configure(aew, aew->core.height - oldHeight, (Widget) 0);
}    

/*
 * This routine shouldn't be necessary, but it is because the size could
 * not be calculated in Initialize since the Table widget does not set its
 * core width and height fields in its Initialize procedure as it should.
 */
static void 
Realize(w, valueMask, attributes)
Widget w;
Mask *valueMask;
XSetWindowAttributes *attributes;
{
    AxeEditorWidget aew = (AxeEditorWidget) w;
    Cardinal child;
    Dimension width, height;

    (*((CompositeWidgetClass)
       (axeEditorWidgetClass->core_class.superclass))->core_class.realize) (w, valueMask, attributes);

    for (child = 0;  child < aew->composite.num_children;  ++child)
    {
	if (XtIsManaged(aew->composite.children[child]))
	{
	    XtRealizeWidget(aew->composite.children[child]);
	}
    }

    CalculateSize(aew, &width, &height);
    XtVaSetValues(w,
		  XtNwidth, width,
		  XtNheight, height,
		  NULL);
    /*
     * This is done here and not in Initialize for the benefit of AxeWindow.
     * UpdateTitles calls the change_callbacks list but AxeWindow can't
     * add to the list until after it has created its AxeEditor component,
     * hence if fileTitle were operative then the title bar would not be 
     * correct until a change causing an update occurred.
     */
    UpdateTitles(aew);
}

static void
Destroy(w)
     Widget w;
{
    int editor;

    for (editor = 0;  editor < NUMBEROFEDITORS;  ++editor)
    {
	if (w == EDITORS[editor])
	{
	    EDITORS[editor] = NULL;
	}
    }

    if (PRIVATE(w,info_timer))
    {
	XtRemoveTimeOut(PRIVATE(w,info_timer));
    }

    if (PRIVATE(w,font_table))
    {
	FreeFontList(XtDisplay(w), PRIVATE(w,font_table));
    }
}

static void
NewEditor(aew)
     Widget aew;
{
    int editor;
    Boolean spareSlot = False;
    WidgetList oldList;

    for (editor = 0;  editor < NUMBEROFEDITORS;  ++editor)
    {
	if (!EDITORS[editor])
	{
	    spareSlot = True;
	    break;
	}
    }

    if (!spareSlot)
    {
	++NUMBEROFEDITORS;
	if (NUMBEROFEDITORS > MAXEDITORS)
	{
	    MAXEDITORS += INCREMENT;
	    oldList = EDITORS;
	    EDITORS = (WidgetList) XtRealloc((char *) oldList,
					     MAXEDITORS * sizeof(Widget));
	}
	editor = NUMBEROFEDITORS - 1;
    }

    EDITORS[editor] = aew;
}

/* ARGSUSED */
static XtGeometryResult
GeometryManager(w, desired, allowed)
     Widget w;
     XtWidgetGeometry *desired, *allowed;
{
    AxeEditorWidget aew = (AxeEditorWidget) XtParent(w);
    Dimension saveWidth, saveHeight, newWidth, newHeight;
    XtWidgetGeometry request;
    XtGeometryResult result;

#define REQUESTS(flag) (desired->request_mode & flag)

    if (!XtIsSubclass(w, axeTextDeckWidgetClass))
    {
	return XtGeometryNo;
    }

    if (REQUESTS(CWWidth))
    {
	saveWidth = w->core.width;
	w->core.width = desired->width;
    }
    
    if (REQUESTS(CWHeight))
    {
	saveHeight = w->core.height;
	w->core.height = desired->height;
    }    
    CalculateSize(aew, &newWidth, &newHeight);

    request.request_mode = desired->request_mode;
    request.width = newWidth;
    request.height = newHeight;

    result = XtMakeGeometryRequest((Widget) aew, &request,
			      (XtWidgetGeometry *) 0);

    if (REQUESTS(XtCWQueryOnly) || result != XtGeometryYes)
    {
	if(REQUESTS(CWWidth))
	{
	    w->core.width = saveWidth;
	}

	if (REQUESTS(CWHeight))
	{
	    w->core.height = saveHeight;
	}

	return result;
    }
	
    if (result == XtGeometryYes)
    {
	Configure(aew, 0, w);
	
	return XtGeometryYes;
    }

    return XtGeometryNo;

#undef REQUESTS
}

static void
InsertChild(w)
     Widget w;
{
    CompositeWidget parent = (CompositeWidget) XtParent(w);

    /* Silently disallow unknown children; the 6 we know of are fileName */
    /*          infoBar, buttons, ed_deck, miniMenu, miniBuffer          */
    if (parent->composite.num_children < 6)
    {
	(*((CompositeWidgetClass)
	   (axeEditorWidgetClass->
	    core_class.superclass))->composite_class.insert_child) (w);
    }
}

static void
DeleteChild(w)
     Widget w;
{
    Widget parent = XtParent(w);

    if (w == PRIVATE(parent,ed_deck))
    {
	XtDestroyWidget(parent);
    }
}

static void
SetKeyboardFocus(axeEditor, ed)
     Widget axeEditor, ed;
{
    if (PRIVATE(axeEditor,focus_to_text))
    {
	XtSetKeyboardFocus(PRIVATE(axeEditor,file_name), ed);
	XtSetKeyboardFocus(PRIVATE(axeEditor,info_bar), ed);
	XtSetKeyboardFocus(PRIVATE(axeEditor,menu_bar), ed);
    }
}

/* ARGSUSED */
static void
DeckChange(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    Widget axeEditor = (Widget) client_data;
    Widget ed = AxeTextDeckTop(PRIVATE(axeEditor,ed_deck));

    SetKeyboardFocus(axeEditor, ed);
    if (PRIVATE(axeEditor,accelerateMini))
    {
	MiniUninstallAccelerators(w);
    }
    UpdateTitles(axeEditor);

    RePopulateShowMenu(XtWidgetToApplicationContext(axeEditor));
}

static Boolean
CreatePopupLater(client_data)
     XtPointer client_data;
{
    Widget menu = (Widget) client_data;
    XtCreatePopupChildProc create;

    XtVaGetValues(menu,
		  XtNcreatePopupChildProc, &create,
		  NULL);

    if (create)
    {
	(*create) (menu);
    }

    XtVaSetValues(menu,
		  XtNcreatePopupChildProc, (XtCreatePopupChildProc) 0,
		  NULL);

    return True;
}

static Widget
MenuMade(menuName, menuParent)
     String menuName;
     Widget menuParent;
{
    int n;
    
    for (n = 0;  n < menuParent->core.num_popups;  n++)
    {
        if (strcmp(XtName(menuParent->core.popup_list[n]), menuName) == 0)
        {
            return menuParent->core.popup_list[n];
        }
    }
    return (Widget) 0;
}

static Widget
MakeMenu(menuButton, menuParent, childProc, always)
     Widget menuButton, menuParent;
     XtCreatePopupChildProc childProc;
     Boolean always;
{
    String menuName;
    Widget menu;

    XtVaGetValues(menuButton,
		  XtNmenuName, &menuName,
		  NULL);

    if (!(menu = MenuMade(menuName, menuParent)))
    {
	menu = XtVaCreatePopupShell(menuName, axeSimpleMenuWidgetClass,
				    menuParent,
				    NULL);
	XtVaSetValues(menu,
		      XtNcreatePopupChildProc, childProc,
		      NULL);

	AxeSimpleMenuStorePopperUpper(menu, AxeEditorParentOf(menuButton));

	/*
	 * No longer applies to any menu; retained just in case
	 */
	if (always)
	{
	    XtAppAddWorkProc(XtWidgetToApplicationContext(menu),
			     CreatePopupLater,
			     (XtPointer) menu);
	}

	return menu;
    }

    return (Widget) 0;
}

static void
MakeEvent(widget, event)
     Widget widget;
     XEvent *event;
{
    Window root, child;
    unsigned int mask;

    XQueryPointer(XtDisplay(widget), XtWindow(widget), &root, &child,
		  &event->xbutton.x_root, &event->xbutton.y_root,
		  &event->xbutton.x, &event->xbutton.y, &mask);

    event->xbutton.type = ButtonRelease;
}

static void
GenericAction(widget, client_data)
     Widget widget;
     XtPointer client_data;
{
    XEvent event;
    String action = (String) client_data, params[1], p, q, proc;
    char routine[32], param[128];
    Cardinal num_params;
    
    if ( (p = index(action, '(')) )
    {
	strncpy(routine, action, p - action);
	routine[p - action] = '\0';
	p += 1;
	q = index(p, ')');
	strncpy(param, p, q - p);
	param[q - p] = '\0';
	proc = routine;
	params[0] = param;
	num_params = strlen(param) == 0 ? 0 : 1;
    }
    else
    {
	params[0] = NULL;
	proc = action;
	num_params = 0;
    }

    MakeEvent(widget, &event);
    XtCallActionProc(widget, proc, &event, params, num_params);
}


/* ARGSUSED */
static void
DoAction(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    GenericAction(AxeTextDeckTop(PRIVATE(AxeSimpleMenuFetchPopperUpper(XtParent(widget)),ed_deck)), client_data);
}

static void
AddBitmaps(widget, pixmap)
     Widget widget;
     Pixmap pixmap;
{
    Pixmap lbitmap, rbitmap;

    XtVaGetValues(widget,
                  XtNleftBitmap, &lbitmap,
                  XtNrightBitmap, &rbitmap,
                  NULL);

    if (!lbitmap)
    {
        XtVaSetValues(widget,
                      XtNleftBitmap, pixmap,
                      NULL);
    }

    if (!rbitmap)
    {
        XtVaSetValues(widget,
                      XtNrightBitmap, pixmap, 
                      NULL);
    }
}

static void
AddObjectBitmaps(obj, name)
     Widget obj;
     String name;
{
    Pixmap bitmap;

    if (strcmp(name, "sbtext") == 0)
    {
	XtVaGetValues(obj, XtNleftBitmap, &bitmap, NULL);
	if (!bitmap)
	{
	    XtVaSetValues(obj,
			  XtNleftBitmap, CLASS(bwdArrow),
			  NULL);
	}
    }
    else if (strcmp(name, "sftext") == 0)
    {
	XtVaGetValues(obj, XtNrightBitmap, &bitmap, NULL);
	if (!bitmap)
	{
	    XtVaSetValues(obj,
			  XtNrightBitmap, CLASS(fwdArrow),
			  NULL);
	}
    }
    else if (strcmp(name, "sbsel") == 0)
    {
	XtVaGetValues(obj, XtNleftBitmap, &bitmap, NULL);
	if (!bitmap)
	{
	    XtVaSetValues(obj,
			  XtNleftBitmap, CLASS(bwdArrow),
			  NULL);
	}
    }
    else if (strcmp(name, "sfsel") == 0)
    {
	XtVaGetValues(obj, XtNrightBitmap, &bitmap, NULL);
	if (!bitmap)
	{
	    XtVaSetValues(obj,
			  XtNrightBitmap, CLASS(fwdArrow),
			  NULL);
	}
    }
    else if (strcmp(name, "mcentre") == 0)
    {
	AddBitmaps(obj, CLASS(vtCentre));
    }
    else if (strcmp(name, "mhcentr") == 0)
    {
	AddBitmaps(obj, CLASS(hzCentre));
    }
}

static void
MakeOneDefaultMenu(menu, menuName)
     Widget menu;
     String menuName;
{
    Widget obj;
    int button;

    for	(button = 0;  button < XtNumber(buttonActions);  ++button)
    {
	if (!buttonActions[button].action
	               && strcmp(menuName, buttonActions[button].name) == 0)
	{
	    break;
	}
    }

    for (button += 1;  button < XtNumber(buttonActions);  ++button)
    {
	if (!buttonActions[button].action)
	{
	    break;
	}

	obj = XtVaCreateManagedWidget(buttonActions[button].name,
				             axeSmeBSBObjectClass, menu, NULL);
	XtAddCallback(obj, XtNcallback, DoAction,
		                     (XtPointer) buttonActions[button].action);
	AddObjectBitmaps(obj, buttonActions[button].name);
    }
}

static void
MakeOneMenu(menu)
     Widget menu;
{
    String menuName = XtName(menu);
    char resourceName[32];
    String menuList;
    XtResource resource;
    Cardinal numChildren;

    /*
     * We don't know which button is popping up the menu, but we know the
     * menu name so we will generate something from that. Thus for menu
     * mMove we generate moveMenu and look for a resource moveMenuList.
     */
    sprintf(resourceName, "%sMenu", menuName);
    resourceName[1] = tolower(resourceName[1]);

    resource.resource_name   = &resourceName[1];
    resource.resource_class  = "Menu";
    resource.resource_type   = XtRString;
    resource.resource_size   = sizeof(String);
    resource.resource_offset = 0;
    resource.default_type    = XtRString;
    resource.default_addr    = 0;

    XtVaGetApplicationResources(AxeSimpleMenuFetchPopperUpper(menu),
				(XtPointer) &menuList, &resource, 1, NULL);
    if (!menuList)
    {
	MakeOneDefaultMenu(menu, menuName);
    }
    else
    {
	FontListStruct *m, *userMenu = ParseFontList(menuList);
	int item;
	Widget obj;

	for (m = userMenu + 1;  m->name;  ++m)
	{
	    if (*m->label == '+')
	    {
		MakeOneDefaultMenu(menu, menuName);
		continue;
	    }

	    if (*m->label == '|')
	    {
		XtVaCreateManagedWidget("line", smeLineObjectClass, menu,
					NULL);
		continue;
	    }

	    for (item = 0;  item < XtNumber(buttonActions);  ++item)
	    {
                if (strcmp(buttonActions[item].name, m->name) == 0)
                {
                    obj = XtVaCreateManagedWidget(buttonActions[item].name,
				             axeSmeBSBObjectClass, menu, NULL);
		    XtAddCallback(obj, XtNcallback, DoAction,
		                       (XtPointer) buttonActions[item].action);
		    AddObjectBitmaps(obj, buttonActions[item].name);
                    break;
		}
	    }

        }

	XtVaGetValues(menu, XtNnumChildren, &numChildren, NULL);
	if (numChildren == 0)
	{
	    char warning[64];
	    
	    sprintf(warning,
		    "The value of %s is invalid", &resourceName[1]);
	    XtAppWarning(XtWidgetToApplicationContext(menu), warning);

	    /*
	     * Prevents permature exit if menu popped
	     */
	    XtVaCreateManagedWidget("line", smeLineObjectClass, menu,
					NULL);
	}

	FreeFontList(XtDisplay(menu), userMenu);
    }
}

/* ARGSUSED */
static void
Help(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    if (*num_params)
    {
	AxeEditorHelpWindow(params[0], CLASS(menu_parent));
    }
    else
    {
	AxeEditorHelpWindow((String) 0, CLASS(menu_parent));
    }
}

/* ARGSUSED */
static void
ChooseFont(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Pixmap leftBitmap;
    WidgetList children;
    Cardinal numChildren, child, choice;
    Widget axeEditor = AxeSimpleMenuFetchPopperUpper(XtParent(widget));
    Widget lastFont;
    XFontStruct *fontStruct;

    XtVaGetValues(widget,
		  XtNleftBitmap, &leftBitmap,
		  NULL);

    if (leftBitmap)
    {
	return;
    }

    XtVaGetValues(XtParent(widget),
		  XtNchildren, &children,
		  XtNnumChildren, &numChildren,
		  NULL);

    for (child = 0;  child < numChildren;  ++child)
    {
	XtVaGetValues(children[child],
		      XtNleftBitmap, &leftBitmap,
		      NULL);

	if (leftBitmap)
	{
	    lastFont = children[child];
	}

	if (widget == children[child])
	{
	    choice = child;
	}
    }

    if (!(fontStruct = PRIVATE(axeEditor,font_table)[choice].fontStruct))
    {
	fontStruct
	    = PRIVATE(axeEditor,font_table)[choice].fontStruct
	    = XLoadQueryFont(XtDisplay(axeEditor),
			     PRIVATE(axeEditor,font_table)[choice].name);
    }
    
    if (fontStruct)
    {
	XtVaSetValues(lastFont,
		      XtNleftBitmap, (Pixmap) 0,
		      NULL);
	
	XtVaSetValues(widget,
		      XtNleftBitmap, CLASS(tickMark),
		      NULL);
	
	XtVaSetValues(PRIVATE(axeEditor,ed_deck),
		      XtNfont, fontStruct,
		      NULL);
    }
    else
    {
	UpdateInfoBar(axeEditor, "Invalid font");
    }
}

static void
FreeFontList(display, list)
     Display *display;
     FontListStruct *list;
{
    FontListStruct *l;

    for (l = list;  l->name;  ++l)
    {
	if (l->name != l->label)
	{
	    XtFree(l->name);
	}
	XtFree(l->label);

	/* NB We didn't cause creation of the Default entry's XFontStruct */
	if (l != list && l->fontStruct)
	{
	    XFreeFont(display, l->fontStruct);
	}
    }
    XtFree((char *) list);
}

static FontListStruct*
ParseFontList(fontList)
     String fontList;
{
    int n, numberOfFonts, listLen = strlen(fontList);
    String i, fontString, parsed, p;
    Boolean skipping, quoting;
    char quoteChar;
    FontListStruct *fontTable;

    for (n = 0, i = fontList, skipping = True, quoting = False;
	 i < fontList + strlen(fontList);
	 ++i)
    {
	if (*i == '"' || *i == '\'')
	{
	    if (quoting && (*i == quoteChar))
	    {
		quoting = False;
	    }
	    else if (!quoting)
	    {
		quoting = True;
		quoteChar = *i;
	    }
	}
	else if (*i == ' ' || *i == '\t')
	{
	    if (!skipping)
	    {
		++n;
		skipping = !skipping;
	    }
	}
	else if (skipping)
	{
	    skipping = !skipping;
	}
    }

    numberOfFonts = n + 3; /* Default, no trailing space, NULL terminator */
    fontTable
	= (FontListStruct *) XtMalloc(numberOfFonts * sizeof(FontListStruct));

    fontString = (String) XtMalloc(listLen + 2);
    strcpy(fontString, fontList);
    strcat(fontString, " ");

    fontTable[0].label = XtNewString("Default");
    fontTable[0].name = fontTable[0].label;
    parsed = (String) XtMalloc(listLen + 1);

    for (i = fontString, p = parsed, skipping = False, quoting = False, n = 0;
	 i < fontString + strlen(fontList) + 1;
	 ++i)
    {
	if (*i == '"' || *i == '\'')
	{
	    if (quoting && (*i == quoteChar))
	    {
		quoting = False;
	    }
	    else if (!quoting)
	    {
		quoting = True;
		quoteChar = *i;
	    }
	}
	else if (!quoting && (*i == ' ' || *i == '\t'))
	{
	    if (skipping)
	    {
		*p = '\0';
		if (!fontTable[n].label)
		{
		    fontTable[n].label = parsed;
		}
		fontTable[n].name = parsed;
		fontTable[n].fontStruct = (XFontStruct *) 0;
		parsed = (String) XtMalloc(listLen + 1);
		p = parsed;
	    }
	    skipping = False;
	}
	else if (!quoting && *i == ':')
	{
	    *p = '\0';
	    fontTable[n].label = parsed;
	    parsed = (String) XtMalloc(listLen + 1);
	    p = parsed;
	}
	else
	{
	    *p++ = *i;
	    if (!skipping)
	    {
		skipping = !skipping;
		++n;
		fontTable[n].label = 0;
	    }
	}
    }
    fontTable[++n].name = (String) 0;

    XtFree(fontString);
    XtFree(parsed);

    return fontTable;
}

static void
MakeFontMenu(menu)
     Widget menu;
{
    Widget axeEditor = XtParent(menu);
    String fontList;
    Widget obj;
    FontListStruct *parseFontList, *p;

    XtVaGetValues(XtParent(menu),
		  XtNfontList, &fontList,
		  NULL);

    parseFontList = PRIVATE(axeEditor,font_table) = ParseFontList(fontList);

    XtVaGetValues(AxeTextDeckTop(PRIVATE(XtParent(menu),ed_deck)),
		  XtNfont, &parseFontList[0].fontStruct,
		  NULL);

    for (p = parseFontList;  p->name;  ++p)
    {
	obj = XtVaCreateManagedWidget(p->name, axeSmeBSBObjectClass, menu,
				      XtNlabel, p->label,
				      XtNleftBitmap, p == parseFontList ?
				                     CLASS(tickMark) :
				                     (Pixmap) 0,
				      NULL);
	XtAddCallback(obj, XtNcallback, ChooseFont, (XtPointer) 0);
    }
}

/* ARGSUSED */
static void
NewWindow(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Widget window
	= AxeEditorCreateWindow(widget, CLASS(menu_parent), (String) 0);
    WidgetList children;

    XtPopup(window, XtGrabNone);

    if (*num_params > 0)
    {
	XtVaGetValues(window, XtNchildren, &children, NULL);

	XtCallActionProc(AxeEditorEdWidget(children[0]), "load-file",
					   event, (String *) 0, (Cardinal) 0);
    }
}

/* ARGSUSED */
static void
ForceClose(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    XtDestroyWidget((Widget) client_data);

    RePopulateShowMenu(XtWidgetToApplicationContext(widget));
}

static Boolean
WindowClosed(w)
     Widget w;
{
    Boolean closed;

    if (w && XtIsSubclass(XtParent(w), axeWindowWidgetClass))
    {
	if ( (closed = AxeTextDeckIterate(PRIVATE(w,ed_deck), AxeSafeClose)) )
	{
	    XtDestroyWidget(w);
	}
	return closed;
    }
    else
    {
	return False;
    }
}

/* ARGSUSED */
static void
CloseWindow(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Widget axeEditor = AxeEditorParentOf(widget);

    if (!WindowClosed(axeEditor))
    {
	ConfirmerRequestConfirmation(PRIVATE(axeEditor,confirmer),
				     "There are unsaved changes",
				     "close unsaved", ForceClose,
				     "unused", NULL,
				     (XtPointer) axeEditor);
    }
    {
	RePopulateShowMenu(XtWidgetToApplicationContext(widget));
    }
}

static Boolean
AllWindowsClosed(w)
     Widget w;
{
    Boolean result = False;

    if (w && XtIsSubclass(XtParent(w), axeWindowWidgetClass))
    {
	result = AxeEditorIterate(AxeSafeClose);
    }
    return result;
}

/* ARGSUSED */
static void
ForceCloseAll(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    AxeEditorIterate(AxeForceClose);

    RePopulateShowMenu(XtWidgetToApplicationContext(widget));
}

/* ARGSUSED */
static void
CloseAll(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Widget axeEditor= AxeEditorParentOf(widget);
    int editor;

    if (!AllWindowsClosed(axeEditor))
    {
	/* NB axeEditor may have been deleted, so find another. */
	/*    being_destroyed field is used since only Phase 1  */
	/*    of XtDestroyWidget for any windows destroyed will */
	/*    have been executed at this juncture.              */
	for (editor = 0;  editor < NUMBEROFEDITORS;  ++editor)
	{
	    axeEditor = EDITORS[editor];
	    if (axeEditor && !axeEditor->core.being_destroyed)
	    {
		ConfirmerRequestConfirmation(PRIVATE(axeEditor,confirmer),
					     "There are unsaved changes",
					     "close unsaved", ForceCloseAll,
					     "unused", NULL,
					     (XtPointer) axeEditor);
		break;
	    }
	}
    }
    /*
     * This catches the case where the confirmer is necessary but
     * the user selects "cancel", but it also means that the menu
     * is generated twice if "close unsaved" is chosen - once from
     * this call and once from the ForceClassAll callback.
     */
    RePopulateShowMenu(XtWidgetToApplicationContext(widget));
}

/* ARGSUSED */
static void
IconifyAll(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Boolean iconifying = (strcmp(params[0], "I") == 0);
    Display *display;
    Widget shell;
    Window window;
    int editor;

    for (editor = 0;  editor < NUMBEROFEDITORS;  ++editor)
    {
	if (EDITORS[editor])
	{
	    shell = ShellOf(EDITORS[editor]);
	    display = XtDisplay(shell);
	    window = XtWindow(shell);
	    if (iconifying)
	    {
		XIconifyWindow(display, window, XDefaultScreen(display));
            }
            else
            {
                XMapWindow(display, window);
            }
	}
    }
}

/* ARGSUSED */
static void
Restack(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    printf("Restack not implemented\n");
}

/* ARGSUSED */
static void
NewBuffer(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Widget axeEditor = AxeEditorParentOf(widget), ed;
    XFontStruct *font;
    char *assocFile, assocDir[MAXPATHLEN];

    XtVaGetValues(PRIVATE(axeEditor,ed_deck),
		  XtNfont, &font,
		  NULL);
	
    ed = XtVaCreateManagedWidget("ed", axeTextWidgetClass,
				                   PRIVATE(axeEditor,ed_deck),
				 XtNfont, font,
				 NULL);

    if ( (assocFile = AxeTextGetAssociatedFile(widget)) )
    {
	strcpy(assocDir, assocFile);
	assocDir[rindex(assocDir, '/') - assocDir] = '\0';
	XtVaSetValues(ed, XtNassociatedDirectory, assocDir, NULL);
    }

    if (*num_params > 0)
    {
	XtCallActionProc(ed, "load-file", event, (String *) 0, (Cardinal) 0);
    }
}

/* ARGSUSED */
static void
ForceCloseTopBuffer(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget axeEditor = (Widget) client_data;

    AxeTextDeckCloseTop(PRIVATE(axeEditor,ed_deck));

    RePopulateShowMenu(XtWidgetToApplicationContext(axeEditor));
}

/* ARGSUSED */
static void
CloseBufferSaved(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget axeEditor = (Widget) client_data;

    if (!AxeTextSaveFile(AxeTextDeckTop(PRIVATE(axeEditor,ed_deck))))
    {
	ConfirmerRequestConfirmation(PRIVATE(axeEditor,confirmer),
				     "There are unsavable changes",
				     "acknowledge", ConfirmerPopdown,
				     "unused", NULL,
				     (XtPointer) axeEditor);
    }
    else
    {
	ForceCloseTopBuffer((XtPointer) 0, (XtPointer) axeEditor,
			                                        (XtPointer) 0);
    }
}

/* ARGSUSED */
static void
CloseBuffer(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Widget axeEditor = AxeEditorParentOf(widget);
    Widget deck = PRIVATE(axeEditor,ed_deck);
    Widget buffer = AxeTextDeckTop(deck);
    Cardinal numChildren;
    
    XtVaGetValues(deck, XtNnumChildren, &numChildren, NULL);
    if (numChildren == 1)
    {
	CloseWindow(widget, event, params, num_params);

	return;
    }

    if (AxeTextIsModified(buffer))
    {
	if (AxeTextGetAssociatedFile(buffer))
	{
	    ConfirmerRequestConfirmation(PRIVATE(axeEditor,confirmer),
					 "There are unsaved changes",
					 "close unsaved", ForceCloseTopBuffer,
					 "close saved", CloseBufferSaved,
					 (XtPointer) axeEditor);
					 
	}
	else
	{
	    ConfirmerRequestConfirmation(PRIVATE(axeEditor,confirmer),
					 "There are unsavable changes",
					 "close unsaved", ForceCloseTopBuffer,
					 "unused", NULL,
					 (XtPointer) axeEditor);
	}
    }
    else
    {
	ForceCloseTopBuffer((XtPointer) 0, (XtPointer) axeEditor,
			                                        (XtPointer) 0);
    }
}

static Widget
ShellOf(axeEditor)
     Widget axeEditor;
{
    Widget w;
    
    for (w = XtParent(axeEditor)  ;;  w = XtParent(w))
    {
	if (XtIsShell(w))
	{
	    break;
	}
    }

    return w;
}

/* ARGSUSED */
static void
RaiseWindow(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget axeEditor = AxeEditorOf((Widget) client_data);
    Widget shell = ShellOf(axeEditor);

    AxeTextDeckRaise(PRIVATE(axeEditor,ed_deck), (Widget) client_data);

    XMapRaised(XtDisplay(shell), XtWindow(shell));
}

static void
CopyShowMenuChildren(children, numChildren)
     WidgetList *children;
     Cardinal *numChildren;
{
    WidgetList copyChildren;
    Cardinal child;

    XtVaGetValues(CLASS(show_menu),
		  XtNchildren, &copyChildren,
		  XtNnumChildren, numChildren,
		  NULL);

    *children = (WidgetList) XtMalloc(*numChildren * sizeof(Widget));
    for (child = 0;  child < *numChildren;  ++child)
    {
	(*children)[child] = copyChildren[child];
    }
}

/* ARGSUSED */
static Boolean
PopulateShowMenu(client_data)
     XtPointer client_data;
{
    WidgetList copyChildren;
    Cardinal numChildren;
    int child, editor, windows, n;
    AxeTextDeckList list, lst;
    String homeFile;
    Pixmap leftBitmap;
    Widget obj, line = 0;
    AxeEditorWidgetClass wc = (AxeEditorWidgetClass) axeEditorWidgetClass;

    if (!CLASS(show_menu))
    {
	return True;
    }

    CopyShowMenuChildren(&copyChildren, &numChildren);

    /*
     * I'm not sure that I understand this, but if I deleted the old menu
     * objects here first, then app crashed with X_ConfigureWindow error,
     * although it was fine when this routine was a callback routine that
     * was invoked via the triggering of the MenuButton's notify action.
     */

    for (windows = 0, editor = 0;  editor < NUMBEROFEDITORS;  ++editor)
    {
	if (EDITORS[editor])
	{
	    list = AxeTextDeckListOf(PRIVATE(EDITORS[editor],ed_deck));
	    for (lst = list;  (*lst).file;  ++lst)
	    {
		leftBitmap
		    = AxeTextIsModified((*lst).widget) ? CLASS(modMark) 
			                               : (Pixmap) 0;

		homeFile = HomeFile((*lst).file);
		obj = XtVaCreateManagedWidget(homeFile,
					      axeSmeBSBObjectClass,
					      CLASS(show_menu),
					      XtNleftBitmap, leftBitmap,
					      NULL);
		XtAddCallback(obj, XtNcallback, RaiseWindow,
			                            (XtPointer) (*lst).widget);
		if (homeFile != (*lst).file)
		{
		    XtFree(homeFile);
		}
		XtFree((char *) (*lst).file);
	    }
	    XtFree((char *) list);

            line = XtVaCreateManagedWidget("line", smeLineObjectClass,
					               CLASS(show_menu), NULL);
	    ++windows;
	}
    }
    if (line)
    {
	/* If the menu is non-empty then the last line was one too many */
	XtDestroyWidget(line);
    }
    else
    {
	/* Can't have an empty menu so give it a dummy entry */
	XtVaCreateManagedWidget("line", smeLineObjectClass,
				                       CLASS(show_menu), NULL);
    }

    /* See comment above */
    for (child = 0;  child < numChildren;  ++child)
    {
	XtDestroyWidget(copyChildren[child]);
    }
    XtFree((char *) copyChildren);

    CLASS(show_work_proc) = False;

    for (n = 0;  n < CLASS(show_menu_callbacks);  ++n)
    {
	(*CLASS(show_menu_callback_list)[n].cbr.callback)
	    (CLASS(show_menu_callback_list)[n].widget,
	     CLASS(show_menu_callback_list)[n].cbr.closure,
	     (XtPointer) windows);
    }

    return True;
}

static void
RePopulateShowMenu(app)
     XtAppContext app;
{
    if (!CLASS(show_work_proc))
    {
	(void) XtAppAddWorkProc(app, PopulateShowMenu, (XtPointer) 0);

	CLASS(show_work_proc) = True;
    }
}

/* ARGSUSED */
static void
OnShowPopup(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget axeEditor = AxeSimpleMenuFetchPopperUpper(widget);
    Widget ed;
    WidgetList children;
    Cardinal numChildren, child;
    XtCallbackList callback;
    
    if (axeEditor)
    {
	ed = AxeEditorEdWidget(axeEditor);
    }

    XtVaGetValues(CLASS(show_menu),
		  XtNchildren, &children,
		  XtNnumChildren, &numChildren,
		  NULL);

    for (child = 0;  child < numChildren;  ++child)
    {
	if (!XtIsSubclass(children[child], axeSmeBSBObjectClass))
	{
	    continue;
	}

	XtVaGetValues(children[child], XtNcallback, &callback, NULL);
	XtVaSetValues(children[child],
		      XtNrightBitmap,
		      axeEditor ? (((Widget) callback[0].closure ==  ed) ?
				   CLASS(tickMark) : (Pixmap) 0) : (Pixmap) 0,
		      NULL);
    }
}

static void
MakeShowMenu(menu)
     Widget menu;
{
    Cardinal numChildren;

    XtAddCallback(menu, XtNpopupCallback, OnShowPopup, (XtPointer) 0);

    XtVaGetValues(menu, XtNnumChildren, &numChildren, NULL);

    if (numChildren == 0)
    {
	PopulateShowMenu((XtPointer) 0);
    }
}

static Boolean
BuffersSaved(w)
     Widget w;
{
	return AxeTextDeckIterate(PRIVATE(w,ed_deck), AxeSave);
}

/* ARGSUSED */
static void
SaveAndClose(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Widget axeEditor = AxeEditorParentOf(widget);
    Widget axeText;
    int editor;
    Cardinal numChildren;
    
    XtVaGetValues(PRIVATE(axeEditor,ed_deck),
		  XtNnumChildren, &numChildren,
		  NULL);

    if (*num_params == 1)
    {
	if (strcmp(params[0], "window") == 0
	    || (strcmp(params[0], "buffer") == 0 && numChildren == 1))
	{
	    (void) BuffersSaved(axeEditor);
	    if (!WindowClosed(axeEditor))
	    {
		ConfirmerRequestConfirmation(PRIVATE(axeEditor,confirmer),
					     "There are unsavable buffers",
					     "acknowledge", ConfirmerPopdown,
					     "unused", NULL,
					     (XtPointer) axeEditor);
	    }
	}
	else if (strcmp(params[0], "buffer") == 0)
	{
	    axeText = AxeEditorEdWidget(axeEditor);
	    if (AxeTextSaveFile(axeText))
	    {
		XtDestroyWidget(axeText);
	    }
	    else
	    {
		ConfirmerRequestConfirmation(PRIVATE(axeEditor,confirmer),
					     "There are unsavable changes",
					     "acknowledge", ConfirmerPopdown,
					     "unused", NULL,
					     (XtPointer) axeEditor);
	    }
	}
    }
    else if (*num_params == 0 && !AxeEditorIterate(AxeSaveAndClose))
    {
	(void) AxeEditorIterate(AxeSafeClose);

	/* NB axeEditor may have been deleted, so find another. */
	/*    being_destroyed field is used since only Phase1   */
	/*    of XtDestroyWidget for any windows destroyed will */
	/*    have been executed at this juncture.              */
	for (editor = 0;  editor < NUMBEROFEDITORS;  ++editor)
	{
	    axeEditor = EDITORS[editor];
	    if (!axeEditor->core.being_destroyed)
	    {
		ConfirmerRequestConfirmation(PRIVATE(axeEditor,confirmer),
					     "There are unsavable buffers",
					     "acknowledge", ConfirmerPopdown,
					     "unused", NULL,
					     (XtPointer) axeEditor);
		break;
	    }
	}
    }
    RePopulateShowMenu(XtWidgetToApplicationContext(widget));
}

/* ARGSUSED */
static void
SaveAll(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Widget axeEditor = AxeEditorParentOf(widget);;

    if (!AxeEditorIterate(AxeSave))
    {
	ConfirmerRequestConfirmation(PRIVATE(axeEditor,confirmer),
				     "There are unsavable buffers",
				     "acknowledge", ConfirmerPopdown,
				     "unused", NULL,
				     (XtPointer) axeEditor);
    }
    else
    {
	UpdateInfoBar(axeEditor, "All buffers saved");
    }
}

static void
MakeLogoMenu(menu)
     Widget menu;
{
    XtVaCreateManagedWidget(QVERSION,
			    axeSmeBSBObjectClass, menu, NULL);
    XtVaCreateManagedWidget("by",
			    axeSmeBSBObjectClass, menu, NULL);
    XtVaCreateManagedWidget("Jim Wight",
			    axeSmeBSBObjectClass, menu, NULL);
    XtVaCreateManagedWidget("Department of Computing Science",
			    axeSmeBSBObjectClass, menu, NULL);
    XtVaCreateManagedWidget("University of Newcastle upon Tyne",
			    axeSmeBSBObjectClass, menu, NULL);
    XtVaCreateManagedWidget("<j.k.wight@newcastle.ac.uk>",
			    axeSmeBSBObjectClass, menu, NULL);
}

static void
MakeMiniSubMenu(menu, menuList)
     Widget menu;
     FontListStruct *menuList;
{
    char action[80];
    Widget obj;
    FontListStruct *p;

    for (p = menuList + 1;  p->name;  ++p)
    {
	if (*p->label == '+')
	{
	    if (!CLASS(default_mini_menu))
	    {
		CLASS(default_mini_menu) = ParseFontList(defaultMiniMenu);
	    }
	    MakeMiniSubMenu(menu, CLASS(default_mini_menu));
	    continue;
	}

	if (*p->label == '|')
	{
	    XtVaCreateManagedWidget("line", smeLineObjectClass, menu, NULL);
	    continue;
	}

	obj = XtVaCreateManagedWidget(p->label, axeSmeBSBObjectClass, menu,
				      NULL);

	if (strncmp(p->name, "mini-", 5) != 0)
	{
	    sprintf(action, "mini-shell(%s)", p->name);
	    if (p->name != p->label)
	    {
		XtFree(p->name);
	    }
	    p->name = XtNewString(action);
	}
	XtAddCallback(obj, XtNcallback, DoAction, (XtPointer) p->name);
    }
}    

static void
MakeMiniMenu(menu)
     Widget menu;
{
    String miniMenuList;
    FontListStruct *miniMenu;
    static XtResource resource = 
        {XtNminiMenu, XtCMiniMenu, XtRString, sizeof(String),
             (Cardinal) 0, XtRString, defaultMiniMenu};

    /*
     * The miniMenu resource is an application resource, not an AxeEditor
     * resource. We do it this way because we can't get a handle onto the
     * invoking AxeEditor widget.
     */
    XtVaGetApplicationResources(menu, (XtPointer) &miniMenuList,
				&resource, 1,
				NULL);

    CLASS(mini_menu) = ParseFontList(miniMenuList);

    MakeMiniSubMenu(menu, CLASS(mini_menu));
}

/* ARGSUSED */
static void
ButtonAction(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    GenericAction(AxeEditorEdWidget(AxeEditorParentOf(widget)), client_data);
}

static void
MakeButtons(parent, buttonList)
     Widget parent;
     FontListStruct *buttonList;
{
    FontListStruct *p;
    Widget butt;
    int len, button;

    if (PRIVATE(XtParent(parent),button_list))
    {
	for (p = buttonList + 1;  p->name;  ++p)
	{
	    if (*(p->name) == '-' || *(p->name) == ';')
	    {
		continue;
	    }
	    else if (p->name[(len = strlen(p->name)) - 1] == ';')
	    {
		len -= 1;
	    }
	    
	    for (button = 0;  button < XtNumber(buttonActions);  ++button)
	    {
		if (strncmp(buttonActions[button].name, p->name, len) == 0)
		{
		    butt = XtVaCreateManagedWidget(buttonActions[button].name,
					  axeCommandWidgetClass, parent, NULL);

		    XtAddCallback(butt, XtNcallback, ButtonAction,
			             (XtPointer) buttonActions[button].action);
		    break;
		}
	    }
	    
	    if (button >= XtNumber(buttonActions))
	    {
		printf("Invalid button name %s in buttonList\n", p->name);
	    }
	}
    }
    else
    {
	for (button = 0; button < XtNumber(buttonActions); ++button)
	{
	    if (buttonActions[button].action)
	    {
		butt = XtVaCreateManagedWidget(buttonActions[button].name,
					  axeCommandWidgetClass, parent, NULL);

		XtAddCallback(butt, XtNcallback, ButtonAction,
			             (XtPointer) buttonActions[button].action);
	    }
	}
    }
}

static String
ButtonListLayout(widget, menuButtonsLayout, list)
     Widget widget;
     char *menuButtonsLayout;
     FontListStruct **list;
{
    String buttonLayout = (String) XtMalloc(strlen(menuButtonsLayout) +
					    strlen(commandButtonsLayout) + 1);
    FontListStruct
	*buttonList = ParseFontList(PRIVATE(widget,button_list)), *p;

    int col = 0, row = 1, len;
    Boolean newRow = False;
    char buf[16];
    
    strcpy(buttonLayout, menuButtonsLayout);
    for (p = buttonList + 1;  p->name;  ++p)
    {
	if (*(p->name) == '-' )
	{
	    ++col;
	    continue;
	}
	else if (*(p->name) == ';')
	{
	    ++row;
	    col = 0;
	    continue;
	}
	else if (p->name[(len = strlen(p->name)) - 1] == ';')
	{
	    strncat(buttonLayout, p->name, len - 1);
	    newRow = True;
	}
	else
	{
	    strcat(buttonLayout, p->name);
	}
	sprintf(buf, " %d %d wW;", col, row);
	strcat(buttonLayout, buf);
	if (newRow)
	{
	    ++row;
	    col = 0;
	    newRow = False;
	}
	else
	{
	    ++col;
	}
    }

    *list = buttonList;
    return buttonLayout;
}

static XpTableLoc
MakeButtonsLayout(widget, buttonList)
     Widget widget;
     FontListStruct **buttonList;
{
    String layoutString;
    XpTableLoc tableLoc;
    Boolean toFree = True;
    FontListStruct
	*menuList = ParseFontList(PRIVATE(widget,menu_list)), *p;
    char menuButtonsLayout[1024];
    char *mbl;
    int n, m;
    Boolean axeLogoSeen = False, matched;

    menuButtonsLayout[0] = '\0';
    mbl = menuButtonsLayout;
    for (n = 0, p = menuList + 1;  p->name;  ++p, ++n)
    {
	matched = False;
	for (m = 0;  chosenMenus[m].name;  ++m)
	{
	    if (strcmp(p->name, chosenMenus[m].name) == 0)
	    {
		chosenMenus[m].chosen = True;
		matched = True;
		sprintf(mbl, "%sMenu %d 0 wW; ", p->name, n);
		mbl += strlen(mbl);
		break;
	    }
	}
	if (!matched && strcmp(p->name, "logo") == 0)
	{
	    axeLogoSeen = True;
	    sprintf(mbl, "axeLogo %d 0 1 8; ", n);
	    mbl += strlen(mbl);
	}
    }
    if (!axeLogoSeen)
    {
	sprintf(mbl, "axeLogo %d 0 1 8; ", n);
    }

    if (PRIVATE(widget,button_list))
    {
	layoutString = ButtonListLayout(widget, menuButtonsLayout, buttonList);
    }
    else if (PRIVATE(widget,buttons))
    {
	layoutString = (String) XtMalloc(strlen(menuButtonsLayout) +
					 strlen(commandButtonsLayout) + 1);
	strcpy(layoutString, menuButtonsLayout);
	strcat(layoutString, commandButtonsLayout);
    }
    else
    {
	layoutString = menuButtonsLayout;
	toFree = False;
    }

    tableLoc = XpTableLocParse(layoutString);

    if (toFree)
    {
	XtFree(layoutString);
    }
    FreeFontList(XtDisplay(widget), menuList);

    return tableLoc;
}

/*************************************************************
 *
 *                         utilities
 *
 *************************************************************/

static Widget
AxeEditorOf(widget)
     Widget widget;
{
    Widget w;

    for (w = widget;  w;  w = XtParent(w))
    {
	if (XtIsSubclass(w, axeEditorWidgetClass))
	{
	    return w;
	}
    }

    return (Widget) 0;
}

static void
UpdateFileName(aew, file)
     Widget aew;
     String file;
{
    if (PRIVATE(aew,suppressFilename))
    {
	return;
    }

    XtVaSetValues(PRIVATE(aew,file_name), XtNlabel, file, NULL);
}


static void
UpdateTitles(aew)
     Widget aew;
{
    Widget top = AxeTextDeckTop(PRIVATE(aew,ed_deck));
    String file, fileName, homeFile;
    Boolean freeFile = False;

    if (!(file = AxeTextGetAssociatedFile(top)))
    {
	file = XtNewString(noname);
	freeFile = True;
    }
    
    homeFile = HomeFile(file);
    fileName = XtMalloc(strlen(homeFile) + 2 + 1);
    *fileName = '\0';
    if (AxeTextIsModified(top))
    {
	strcat(fileName, "* ");
    }
    strcat(fileName, homeFile);

    UpdateFileName(aew, fileName);
    XtCallCallbackList(aew, PRIVATE(aew,change_callbacks),
		                                         (XtPointer) fileName);
    XtFree(fileName);
    if (homeFile != file)
    {
	XtFree(homeFile);
    }
    if (freeFile)
    {
	XtFree(file);
    }
}    

/*ARGSUSED*/
static void
InfoBarTimeout(client_data, id)
     XtPointer client_data;
     XtIntervalId *id;
{
    Widget axeEditor = (Widget) client_data;

    PRIVATE(axeEditor,info_timer) = (XtIntervalId) 0;

    XtVaSetValues(PRIVATE(axeEditor,info_bar),
                  XtNlabel, QVERSION,
                  NULL);
}

/* ARGSUSED */
static void
UpdateInfo(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    UpdateInfoBar(AxeEditorOf(w), params[0]);
}

static void
UpdateInfoBar(w, info)
     Widget w;
     String info;
{
    if (PRIVATE(w,suppressInfobar))
    {
	return;
    }

    if (PRIVATE(w,info_timer))
    {
	XtRemoveTimeOut(PRIVATE(w,info_timer));
    }

    XtVaSetValues(PRIVATE(w,info_bar),
		  XtNlabel, info ? info : QVERSION,
		  NULL);

    if (PRIVATE(w,info_timeout))
    {
	PRIVATE(w,info_timer) =
	    XtAppAddTimeOut(XtWidgetToApplicationContext(w),
			    (unsigned long) PRIVATE(w,info_timeout) * 1000,
			                        InfoBarTimeout, (XtPointer) w);
    }
}

static Atom
AxeisUserAtom(display)
     Display *display;
{
    char propName[1024], *user;

    strcpy(propName, "AXE_AXINFO");

    if ( (user = getenv("USER")) )
    {
        strcat(propName, "_");
        strcat(propName, user);
    }

    return XInternAtom(display, propName, False);
}

/*************************************************************
 *
 *                         Minibuffer
 *
 *************************************************************/

/* Caller must free return value */
static String
CurrentDir(w)
     Widget w;
{
    Widget axeEditor = AxeEditorOf(w);
    String assocFile, dir, filePart;

    if (assocFile
	= AxeTextGetAssociatedFile(AxeTextDeckTop(PRIVATE(axeEditor,ed_deck))))
    {
	dir = XtNewString(assocFile);
	filePart = rindex(dir, '/');
	if (filePart)
	{
	    *(filePart + 1) = '\0';
	}
	return dir;
    }
    else
    {
	return XtNewString(CLASS(current_dir));
    }
}

/* NB Caller must free return value if different from input */
static String
HomeFile(file)
     String file;
{
    String new;

    if (strlen(file) > CLASS(home_dir_len)
        &&  strncmp(file, CLASS(home_dir), CLASS(home_dir_len)) == 0)
    {
        new = XtMalloc(strlen(file) + 3);
        strcpy(new, "~");
        strcat(new, &file[CLASS(home_dir_len)]);
        return new;
    }

    return file;
}

static void
MiniDisplayCaret(w, On)
     Widget w;
     Boolean On;
{
    XtVaSetValues(PRIVATE(AxeEditorOf(w),mini_buffer),
                  XtNdisplayCaret, On,
                  NULL);
}    

static void
MiniEnable(w, prompt)
     Widget w;
     String prompt;
{
    Widget axeEditor = AxeEditorOf(w);

    if (!PRIVATE(axeEditor,suppressMinibuffer))
    {
        Widget input = PRIVATE(axeEditor,mini_buffer);

        XtVaSetValues(input,
                      XtNdisplayCaret, True,
                      XtNstring, prompt,
                      NULL);

        XawTextSetInsertionPoint(input, strlen(prompt));
    }
}

static XawTextPosition
MiniAppend(w, text)
    Widget w;
    String text;
{
    XawTextBlock textBlock;
    XawTextPosition textPos, textRtn;
    Widget input = PRIVATE(AxeEditorOf(w),mini_buffer);

    textPos = XawTextGetInsertionPoint(input);
    textBlock.firstPos = 0;
    textBlock.length = strlen(text);
    textBlock.ptr = text;
    textBlock.format = FMT8BIT;
    if ((textRtn = XawTextReplace(input, textPos, textPos, &textBlock))
         == XawEditDone)
    {
        XawTextSetInsertionPoint(input, textPos + textBlock.length);
    }

    return textRtn;
}

static void
MiniInstallAccelerators(w)
     Widget w;
{
    Widget axeEditor = AxeEditorOf(w);

    if (!PRIVATE(axeEditor,suppressMinibuffer))
    {
        XtVaSetValues(w,
                      XtNdisplayCaret, False,
                      NULL);

        XtInstallAccelerators(w, PRIVATE(axeEditor,mini_buffer));

        PRIVATE(axeEditor,accelerateMini) = True;
    }
}

static void
MiniUninstallAccelerators(w)
     Widget w;
{
    Widget axeEditor = AxeEditorOf(w);

    if (!PRIVATE(axeEditor,suppressMinibuffer))
    {
	XtVaSetValues(AxeTextDeckTop(PRIVATE(axeEditor,ed_deck)),
		      XtNtranslations,
		                    PRIVATE(axeEditor,defaultTextTranslations),
		      XtNdisplayCaret, True,
		      NULL);
    
	XtVaSetValues(PRIVATE(axeEditor,mini_buffer),
		      XtNdisplayCaret, False,
		      NULL);

	PRIVATE(axeEditor,accelerateMini) = False;
    }
}

static void
MiniInitInput(w)
     Widget w;
{
    String dir = CurrentDir(w);
    String home = HomeFile(dir);
    
    (void) MiniAppend(w, home);

    if (home != dir)
    {
        XtFree(home);
    }
    XtFree(dir);
}
 
static String 
MiniInput(w, skip)
     Widget w;
     Boolean skip;
{
    String input, p;

    XtVaGetValues(PRIVATE(AxeEditorOf(w),mini_buffer),
                  XtNstring, &input,
                  NULL);

    if (skip)
    {
        for (p = &input[1];  *p == ' ';  ++p) {}
        return p;
    }
    else
    {
	return input;
    }
}

/*ARGSUSED*/
static void
miniSelect(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    if (!PRIVATE(AxeEditorOf(w),suppressMinibuffer))
    {
        MiniDisplayCaret(w, True);
        MiniInstallAccelerators(w);
    }
}

/*ARGSUSED*/
static void
miniClear(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    if (!PRIVATE(AxeEditorOf(w),suppressMinibuffer))
    {
        MiniEnable(w, "");
        MiniInstallAccelerators(w);
    }
}

/*ARGSUSED*/
static void
miniSearch(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    if (!PRIVATE(AxeEditorOf(w),suppressMinibuffer))
    {
        if (*num_params > 0 && (strcmp(params[0], "backward") == 0))
        {
            MiniEnable(w, "?");
        }
        else
        {
            MiniEnable(w, "/");
        }
        MiniInstallAccelerators(w);
    }
}

static void
MiniSearch(w, searchDirection)
     Widget w;
     XawTextScanDirection searchDirection;
{
    Widget text = AxeTextDeckTop(PRIVATE(AxeEditorOf(w),ed_deck));
    regexp *compexp;
    String nomatch, regexpr = MiniInput(w, False);
    XawTextPosition nobegin, noend;
    Boolean found;

    compexp = 0;
    if (compexp = regcomp(&regexpr[1]))
    {
	nomatch = 0;
	found = AxeiiTextReSearch(text, (searchDirection == XawsdRight),
			          &regexpr[1], compexp, &nomatch,
				  &nobegin, &noend);
	if (!RegError && !found)
	{
	    UpdateInfoBar(AxeEditorOf(w), "Search string not found");
	}

	XtFree(nomatch);
    }

    if (RegError)
    {
	UpdateInfoBar(AxeEditorOf(w), RegError);
	XBell(XtDisplay(w), 100);
	RegError = 0;
    }

    XtFree((char *) compexp);
}

static void
MiniGoto(w)
     Widget w;
{
    AxeTextGotoLine(AxeTextDeckTop(PRIVATE(AxeEditorOf(w),ed_deck)),
	                                            atoi(MiniInput(w, False)));
}

/*ARGSUSED*/
static void
miniInsert(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    if (!PRIVATE(AxeEditorOf(w),suppressMinibuffer))
    {
        MiniEnable(w, "r ");
        MiniInitInput(w);
        MiniInstallAccelerators(w);
    }
}

static void
MiniInsert(w)
     Widget w;
{
    Widget axeEditor = AxeEditorOf(w);
    Widget axe = AxeTextDeckTop(PRIVATE(axeEditor,ed_deck));    
    String insertFile = AxeEditorExpandName(MiniInput(w, True));
    Boolean undo;
    void (*proc)();

    XtVaGetValues(axe, XtNundo, &undo, NULL);

    if ( (proc = AxeiiTextUndoPreInsert(axe)) )
    {
        (*proc)(axe);
    }

    if (!InsertFileNamed(axe, insertFile))
    {
        UpdateInfoBar(axeEditor, "Error inserting file");
        XBell(XtDisplay(w), 100);
    }

    XtFree(insertFile);
}

/*ARGSUSED*/
static void
miniSaveAs(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    if (!PRIVATE(AxeEditorOf(w),suppressMinibuffer))
    {
        MiniEnable(w, "w ");
        MiniInitInput(w);
        MiniInstallAccelerators(w);
    }
}

static void
MiniSaveAs(w)
     Widget w;
{
    Widget axeEditor = AxeEditorOf(w);
    String saveAs = AxeEditorExpandName(MiniInput(w, True));
    FileNominatorStruct fnomStruct;

    AxeTextFileToNominatorStruct(saveAs, &fnomStruct);

    if (!fnomStruct.directoryPart)
    {
	UpdateInfoBar(axeEditor, "Invalid pathname");
	XBell(XtDisplay(w), 100);
    }
    else if (!fnomStruct.filenamePart)
    {
	UpdateInfoBar(axeEditor, "Pathname is a directory");
	XBell(XtDisplay(w), 100);
    }
    else
    {
	(void) AxeTextSaveAsFile(AxeEditorEdWidget(axeEditor), &fnomStruct);
    }

    XtFree(saveAs);

    AxeTextFreeNominatorStruct(&fnomStruct);
}

/*ARGSUSED*/
static void
miniLoad(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    if (!PRIVATE(AxeEditorOf(w),suppressMinibuffer))
    {
        MiniEnable(w, "e ");
        MiniInitInput(w);
        MiniInstallAccelerators(w);
    }
}

/* ARGSUSED */
static void
MiniForceLoad(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data, call_data;
{
    Widget mini = (Widget) client_data;
    Widget axeEditor = AxeEditorOf(mini);
    String loadFile;
    FileNominatorStruct fnomStruct;
    loadFile = AxeEditorExpandName(MiniInput(mini, True));

    AxeTextFileToNominatorStruct(loadFile, &fnomStruct);

    if (!fnomStruct.directoryPart)
    {
	UpdateInfoBar(axeEditor, "Invalid pathname");
	XBell(XtDisplay(mini), 100);
    }
    else if (!fnomStruct.filenamePart)
    {
	UpdateInfoBar(axeEditor, "Pathname is a directory");
	XBell(XtDisplay(mini), 100);
    }
    else
    {
	AxeTextLoadFile(AxeEditorEdWidget(axeEditor), &fnomStruct);
    }
	
    XtFree(loadFile);

    AxeTextFreeNominatorStruct(&fnomStruct);
}

static void
MiniLoad(w)
     Widget w;
{
    Widget axeEditor = AxeEditorOf(w);

    if (AxeTextIsModified(AxeTextDeckTop(PRIVATE(axeEditor,ed_deck))))
    {
	ConfirmerRequestConfirmation(PRIVATE(axeEditor,confirmer),
				     "There are unsaved changes",
				     "Discard changes", MiniForceLoad,
				     "unused", NULL,
				     (XtPointer) w);
    }
    else
    {
	MiniForceLoad((XtPointer) 0, (XtPointer) w, (XtPointer) 0);
    }
}

static void
MiniPipeOrShell(w, cmd, params, num_params)
     Widget w;
     String cmd, *params;
     Cardinal *num_params;
{
    int i, len;
    char param[80];
    Boolean commit = False;

    if (!PRIVATE(AxeEditorOf(w),suppressMinibuffer))
    {
        MiniEnable(w, cmd);

        for (i = 0;  i < *num_params;  ++i)
        {
	    if (i == (*num_params) - 1 && (len = strlen(params[i])) >= 1 &&
		params[i][len - 1] == '\n')
	    {
		strcpy(param, params[i]);
		param[len - 1] = '\0';
		MiniAppend(w, param);
		commit = True;
	    }
	    else
	    {
		MiniAppend(w, params[i]);
		MiniAppend(w, " ");
	    }
        }

	MiniInstallAccelerators(w);

	if (commit)
	{
	    MiniCommit(w, (XEvent *) 0, (String *) 0, (Cardinal *) 0);
	}
    }
}

/*ARGSUSED*/
static void
miniPipe(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    MiniPipeOrShell(w, "| ", params, num_params);
}

static void
ClosePipes(output, input, errput)
     int *output, *input, *errput;
{
    if (output[0] != -1) close(output[0]);
    if (output[1] != -1) close(output[1]);
    if (input[0] != -1) close(input[0]);
    if (input[1] != -1) close(input[1]);
    if (errput[0] != -1) close(errput[0]);
    if (errput[1] != -1) close(errput[1]);
}	

static Boolean
ExecSubProcess(widget, command, output, input, errput)
     Widget widget;
     String command;
     int *output, *input, *errput;
{
    output[0] = output[1] = input[0] = input[1] = errput[0] = errput[1] = -1;

    if (pipe(output) == -1 || pipe(input) == -1 || pipe(errput) == -1)
    {
	ClosePipes(output, input, errput);
	return False;
    }

    switch(fork())
    {
    case -1:
	ClosePipes(output, input, errput);
	return False;
    case 0: /* Child */
	(void) close(XConnectionNumber(XtDisplay(widget)));
	if (close(0) == 0          &&
	    dup(output[0]) == 0    &&
	    close(output[0]) == 0  &&
	    close(output[1]) == 0  &&
	    close(1) == 0          &&
	    dup(input[1]) == 1     &&
	    close(input[0]) == 0   &&
	    close(input[1]) == 0   &&
	    close(2) == 0          &&
	    dup(errput[1]) == 2    &&
	    close(errput[0]) == 0  &&
	    close(errput[1]) == 0)
	{
	    char *assocFile, assocDir[MAXPATHLEN];
	    

	    if ( (assocFile = AxeTextGetAssociatedFile(widget)) )
	    {
		strcpy(assocDir, assocFile);
		assocDir[rindex(assocDir, '/') - assocDir] = '\0';
		
		(void) chdir(assocDir);
	    }
	    (void) execl("/bin/sh", "sh", "-c", command, (char *) 0);
	}
	ClosePipes(output, input, errput);
	exit(0);
    default: /* Parent */
	if (close(output[0]) == -1 ||
	    close(input[1]) == -1  ||
	    close(errput[1]) == -1)
	{
	    ClosePipes(output, input, errput);
	    return False;
	}
    }

    return True;
}

static void
ChildInput(client_data, source, id)
     XtPointer client_data;
     int *source;
     XtInputId *id;
{
    struct childInfo *ci = (struct childInfo*) client_data;
    char buffer[1024];
    int nread;
    XawTextBlock block;
    XawTextPosition pos;
    void (*proc)();

    nread = read(*source, buffer, sizeof(buffer));

    if (nread == -1)
    {
	UpdateInfoBar(AxeEditorOf(ci->ed),
		      "Output from command is possibly incomplete");
	XBell(XtDisplay(ci->ed), 100);
    }

    if (nread == 0 || nread == -1)
    {
	if (!ci->first && !ci->replace)
	{
	    XtVaSetValues(ci->ed, XtNinsertPosition, 0, NULL);
	    XtPopup(ci->window, XtGrabNone);
	}
	else if (ci->replace && (ci->selStart != ci->selEnd))
	{
	    XawTextSetSelection(ci->ed, ci->selStart,
				             XawTextGetInsertionPoint(ci->ed));
	}

        XtRemoveInput(*id);
        close(*source);
	XtFree((char *) ci);
	wait((int *) 0);
    }
    else
    {
        block.firstPos = 0;
        block.length = nread;
        block.ptr = buffer;
        block.format = FMT8BIT;

	if (ci->first)
	{
	    char first[2];
	    String f = first, *ff = &f;

	    ci->first = False;
	    if (ci->replace)
	    {
		XawTextBlock notext;

		notext.firstPos = 0;
		notext.length = 0;
		notext.ptr = 0;
		notext.format = FMT8BIT;
		XawTextSetInsertionPoint(ci->ed, ci->selStart);
		if (ci->selStart != ci->selEnd)
		{
		    if ( (proc = AxeiiTextUndoPreReplace(ci->ed)) )
		    {
			(*proc)(ci->ed);
		    }
		}
		else
		{
		    if ( (proc = AxeiiTextUndoPreInsert(ci->ed)) )
		    {
			(*proc)(ci->ed);
		    }
		}
		(void) XawTextReplace(ci->ed, ci->selStart, ci->selEnd, &notext);
		/*
		 * This shouldn't be necessary, but for some reason the
		 * insertion point is incorrect following the replace.
		 */
		XawTextSetInsertionPoint(ci->ed, ci->selStart);
	    }
	    else
	    {
		ci->window = AxeEditorCreateWindow(ci->ed, CLASS(menu_parent),
						  (String) 0);
		ci->ed = AxeEditorEdWidget(AxeWindowAxeEditor(ci->window));
		XtRealizeWidget(ci->window);
		/*
		 * Silly kludge to get round problem of caret behaving oddly
		 * if text is inserted into empty widget using XawTextReplace
		 */
		first[0] = buffer[0];
		first[1] = '\0';
		XtCallActionProc(ci->ed, "insert-string", (XEvent *) 0,
				 ff, (Cardinal) 1);
		
		block.firstPos = 1;
		block.length = nread - 1;
	    }
        }

        pos = XawTextGetInsertionPoint(ci->ed);
        if (XawTextReplace(ci->ed, pos, pos, &block) != XawEditDone)
        {
	    UpdateInfoBar(AxeEditorOf(ci->ed), "Text insertion failure");
        }
        else
        {
            XawTextSetInsertionPoint(ci->ed, pos + block.length);
        }
    }
}

static void
MiniPopen(w, replace)
     Widget w;
     Boolean replace;
{

    Widget axeEditor = AxeEditorOf(w);
    Widget text = AxeTextDeckTop(PRIVATE(axeEditor,ed_deck));
    Widget asciiSrc;
    XawTextPosition selStart, selEnd, from;
    XawTextBlock textBlock;
    Cursor busyCursor = 0, menuCursor, textCursor, miniCursor;
    int length, toread, piece;
    int tochild[2], fromchild[2], errchild[2];
    struct childInfo *ci, *ce;

    /*
     * The selection or the entire buffer is fed to the command in the
     * minibuffer. The output from the command replaces the selection or
     * is inserted into the buffer if replace is True (| - mini-pipe),
     * otherwise it is inserted into a new window (! - mini-shell). Nothing
     * happens if there is no standard output from the command. Error output
     * is popped up in a separate standard aXe window.
     */

    if (!ExecSubProcess(text, MiniInput(w, True), tochild, fromchild, errchild))
    {
	UpdateInfoBar(axeEditor, "Unable to execute command");
	XBell(XtDisplay(w), 100);
	return;
    }

    asciiSrc = XawTextGetSource(text);
    XtVaGetValues(asciiSrc, XtNpieceSize, &piece, NULL);
    XawTextGetSelectionPos(text, &selStart, &selEnd);

    if ((length = selEnd - selStart) == 0)
    {
	if (replace)
	{
	    selStart = XawTextGetInsertionPoint(text);
	    selEnd = selStart;
	}
	else
	{
	    selStart = selEnd = 0;
	    length = piece;
	}
    }

    XtVaGetValues(PRIVATE(axeEditor,menu_bar), XtNcursor, &menuCursor, NULL);
    XtVaGetValues(text, XtNcursor, &textCursor, NULL);
    XtVaGetValues(PRIVATE(axeEditor,mini_buffer), XtNcursor, &miniCursor, NULL);

    busyCursor  = XCreateFontCursor(XtDisplay(axeEditor), XC_watch);

    XtVaSetValues(PRIVATE(axeEditor,menu_bar), XtNcursor, busyCursor, NULL);
    XtVaSetValues(text, XtNcursor, busyCursor, NULL);
    XtVaSetValues(PRIVATE(axeEditor,mini_buffer), XtNcursor, busyCursor, NULL);

    XFlush(XtDisplay(axeEditor));

    ci = XtNew(struct childInfo);
    ci->first = True;
    ci->replace = replace;
    ci->ed = text;
    ci->window = 0;
    ci->selStart = selStart;
    ci->selEnd = selEnd;
    (void) XtAppAddInput(XtWidgetToApplicationContext(w), fromchild[0],
			 (XtPointer) XtInputReadMask, ChildInput,
			 (XtPointer) ci);

    ce = XtNew(struct childInfo);
    ce->first = True;
    ce->replace = False;
    ce->ed = text;          
    ce->window = 0;
    ce->selStart = 0;    /* Not used */
    ce->selEnd = 0;      /* Not used */
    (void) XtAppAddInput(XtWidgetToApplicationContext(w), errchild[0],
			 (XtPointer) XtInputReadMask, ChildInput,
			 (XtPointer) ce);

    signal(SIGPIPE, SIG_IGN);
    textBlock.format = FMT8BIT;
    for(toread = length, from = selStart;
        toread > 0;
        toread = (selEnd != selStart ? toread - textBlock.length : piece),
	                                       from = from + textBlock.length) 
    {
        (void) XawTextSourceRead(asciiSrc, from, &textBlock, toread);
	if (textBlock.length == 0)
	{
	    break;
	}
	if (write(tochild[1], textBlock.ptr, textBlock.length) == -1)
	{
	    UpdateInfoBar(axeEditor, "Pipe output error");
	    XBell(XtDisplay(w), 100);
	    break;
	}
    }
    close(tochild[1]);
    signal(SIGPIPE, SIG_DFL);

    if (busyCursor)
    {
	XtVaSetValues(PRIVATE(axeEditor,menu_bar), XtNcursor, menuCursor,NULL);
	XtVaSetValues(text, XtNcursor, textCursor, NULL);
	XtVaSetValues(PRIVATE(axeEditor,mini_buffer), XtNcursor, miniCursor, NULL);
    }
}

#ifdef EXTENSION
/*ARGSUSED*/
static int
MiniInterp(w)
     Widget w;
{
#if TCL_MAJOR_VERSION < 7
    return Tcl_Eval(interpreter.interp, MiniInput(w, False), 0, NULL);
#else
    return Tcl_Eval(interpreter.interp, MiniInput(w, False));
#endif
}
#endif

/*ARGSUSED*/
static void
miniShell(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    MiniPipeOrShell(w, "! ", params, num_params);
}

/*ARGSUSED*/
static void
miniStuff(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    MiniPipeOrShell(w, "", params, num_params);    
}

/*ARGSUSED*/
static void
miniAbort(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Widget axeEditor = AxeEditorOf(w);

    if (!PRIVATE(axeEditor,suppressMinibuffer))
    {
        UpdateInfoBar(axeEditor, "Minibuffer abort");
        XBell(XtDisplay(w), 100);

        if (PRIVATE(axeEditor,accelerateMini))
        {
            MiniUninstallAccelerators(w);
        }
    }
}

/*ARGSUSED*/
static void
TrapWarningMsgss(name, type, class, defaultp, params, num_params)
     String name, type, class, defaultp, *params;
     Cardinal *num_params;
{
    validAction = False;
}

/*ARGSUSED*/
static void
TrapWarnings(message)
     String message;
{
    validAction = False;
}

/*ARGSUSED*/
static void
MiniCommit(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Widget axeEditor = AxeEditorOf(w);
    String input;
    char command;
    Boolean done;

    XtVaGetValues(PRIVATE(axeEditor,mini_buffer),
                  XtNstring, &input,
                  NULL);

    command = input[0];
    switch (command)
    {
    case '/':
        MiniSearch(w, XawsdRight);
        break;
    case '?':
        MiniSearch(w, XawsdLeft);
        break;
    case '|':
        MiniPopen(w, True);
        break;
    case '!':
        MiniPopen(w, False);
        break;
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
        MiniGoto(w);
        break;
    case 'r':
    case 'R':
    case 'w':
    case 'W':
    case 'e':
    case 'E':
	done = False;
	if (input[1] == ' ')
	{
	    switch (input[0])
	    {
	    case 'r':
	    case 'R':
		MiniInsert(w);
		done = True;
		break;
	    case 'w':
	    case 'W':
		MiniSaveAs(w);
		done = True;
		break;
	    case 'e':
	    case 'E':
		MiniLoad(w);
		done = True;
		break;
	    default:
		break;
	    }
	}
	if (done)
	{
	    break;
	}
	else
	{
	    /* Drop-though; not r|w|e<space>; try action or Tcl command */
	}
    default:
        {
	    XtAppContext app = XtWidgetToApplicationContext(w);

	    XtErrorMsgHandler oldWarningMsgHandler =
		XtAppSetWarningMsgHandler(app, TrapWarningMsgss);
	    XtErrorHandler oldWarningHandler =
		XtAppSetWarningHandler(app, TrapWarnings);

	    validAction = True;
	    GenericAction(AxeEditorEdWidget(axeEditor),
			                      (XtPointer) MiniInput(w, False));
	    
	    XtAppSetWarningMsgHandler(app, oldWarningMsgHandler);
	    XtAppSetWarningHandler(app, oldWarningHandler);
	    if (validAction)
	    {
		break;
	    }
	}
#ifdef EXTENSION
	if (MiniInterp(w) == TCL_OK)
	{
	    break;
	}
	else
	{
	    ConfirmerRequestConfirmation(PRIVATE(axeEditor,confirmer),
					 interpreter.interp->result,
					 "acknowledge", ConfirmerPopdown,
					 "unused", NULL,
					 (XtPointer) axeEditor);
#else
	if (True)
	{
	    ConfirmerRequestConfirmation(PRIVATE(axeEditor,confirmer),
					 "Invalid command",
					 "acknowledge", ConfirmerPopdown,
					 "unused", NULL,
					 (XtPointer) axeEditor);
#endif
	    command = '$';
	    break;
	}
    }

    switch (command)
    {
    case '/':
    case '?':
    case '$':
        break;
    default:
        if (PRIVATE(axeEditor,accelerateMini))
        {
            MiniUninstallAccelerators(w);
        }
    }
}

/*ARGSUSED*/
static void
MiniLeave(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    if (!PRIVATE(AxeEditorOf(w),accelerateMini))
    {
        MiniDisplayCaret(w, False);
    }
}

/*************************************************************
 *
 *                       Keyboard Macro
 *
 *************************************************************/

static void
AddToMacro(w, client_data, action_name, event, params, num_params)
     Widget w;
     XtPointer client_data;
     String action_name;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    char buffer[MAXSTRLEN + 1];
    int bytes;

    if (w != (Widget) client_data)
    {
        return;
    }

    if (strcmp(action_name, "insert-char") == 0)
    {
        bytes = XLookupString((XKeyEvent *) event, buffer, MAXSTRLEN,
                              (KeySym *) 0, (XComposeStatus *) 0);
        if (bytes != 0)
        {
            buffer[bytes] = '\0';
            if (!CLASS(in_macro_string))
            {
                CLASS(macro_commands)[CLASS(macro_size)].name
                                           = XrmStringToQuark("insert-string");
                CLASS(macro_commands)[CLASS(macro_size)].numArgs = 1;
                CLASS(macro_commands)[CLASS(macro_size)].arg
		                                = (String) XtMalloc(bytes + 1);
                strncpy(CLASS(macro_commands)[CLASS(macro_size)].arg,
			                                    buffer, bytes + 1);
                CLASS(in_macro_string) = True;
            }
            else
            {
                CLASS(macro_commands)[CLASS(macro_size)].arg
                    = (String) XtRealloc(CLASS(macro_commands)[CLASS(macro_size)].arg,
                                         strlen(CLASS(macro_commands)[CLASS(macro_size)].arg) + bytes + 1);
                strncat(CLASS(macro_commands)[CLASS(macro_size)].arg, buffer, bytes);
            }
        }
    }
    else
    {
        if (CLASS(in_macro_string))
        {
            ++CLASS(macro_size);
            CLASS(in_macro_string) = False;
        }
        CLASS(macro_commands)[CLASS(macro_size)].name
	                                       = XrmStringToQuark(action_name);
        CLASS(macro_commands)[CLASS(macro_size)].numArgs
	                                       = (*num_params) ? 1 : 0;
        if (*num_params != 0)
        {
            CLASS(macro_commands)[CLASS(macro_size)].arg
		                                      = XtNewString(params[0]);
        }
        else
        {
            CLASS(macro_commands)[CLASS(macro_size)].arg = NULL;
        }
        ++CLASS(macro_size);
    }

    if (CLASS(macro_size) == CLASS(max_macro_size))
    {
        CLASS(max_macro_size) += CHUNK;
        CLASS(macro_commands)
	    = (MacroList) XtRealloc((char *) CLASS(macro_commands),
				    CLASS(max_macro_size) * sizeof(MacroCommand));
    }
}

/*ARGSUSED*/
static void
startMacro(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Widget axeEditor = AxeEditorOf(w);
    int m;

    if (!CLASS(macro_id))
    {
        CLASS(macro_id) = XtAppAddActionHook(XtWidgetToApplicationContext(w),
                                                    AddToMacro, (XtPointer) w);
        UpdateInfoBar(axeEditor, "Defining keyboard macro");

        if (CLASS(macro_size) == 0)
        {
            CLASS(macro_commands)
		= (MacroList) XtMalloc(CHUNK * sizeof(MacroCommand));
            CLASS(max_macro_size) = CHUNK;
        }
        else
        {
            for (m = 0;  m < CLASS(macro_size);  ++m)
            {
                if (CLASS(macro_commands)[m].numArgs != 0)
                {
                    XtFree(CLASS(macro_commands)[m].arg);
                }
            }
            CLASS(macro_size) = 0;
        }
        CLASS(in_macro_string) = False;
    }
    else
    {
        UpdateInfoBar(axeEditor, "Already defining keyboard macro");
        XBell(XtDisplay(w), 100);
    }
}

/*ARGSUSED*/
static void
endMacro(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Widget axeEditor = AxeEditorOf(w);

    if (CLASS(macro_id))
    {
        --CLASS(macro_size); /* removes end-macro itself */
        XtRemoveActionHook(CLASS(macro_id));
        CLASS(macro_id) = (XtActionHookId) 0;
        UpdateInfoBar(axeEditor, "Keyboard macro defined");
    }
    else
    {
        UpdateInfoBar(axeEditor, "Not defining keyboard macro");
        XBell(XtDisplay(w), 100);
    }
}

static void
ExecMacro(w, event)
     Widget w;
     XEvent *event;
{
    int c;

    for (c = 0;  c < CLASS(macro_size);  ++c)
    {
        XtCallActionProc(w,
			 XrmQuarkToString(CLASS(macro_commands)[c].name),
                         event, &CLASS(macro_commands)[c].arg,
                         CLASS(macro_commands)[c].numArgs);
    }
}

/*ARGSUSED*/
static void
execMacro(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    if (CLASS(macro_size) == 0)
    {
        UpdateInfoBar(AxeEditorOf(w), "No macro defined");
        XBell(XtDisplay(w), 100);
        return;
    }

    ExecMacro(w, event);
}

/*ARGSUSED*/
static void
repeatMacro(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Widget axeEditor = AxeEditorOf(w);
    int repeat, repeats, times;

    if (CLASS(macro_size) == 0)
    {
        UpdateInfoBar(axeEditor, "No macro defined");
        XBell(XtDisplay(w), 100);
        return;
    }

    if (*num_params > 0)
    {
	repeats = atoi(params[0]);
    }
    else
    {
	if (!PRIVATE(axeEditor,suppressMinibuffer))
	{
	    repeats = ((times = atoi(MiniInput(w, False))) == 0) ? 1 : times;
	}
	else
	{
	    repeats = 1;
	}
    }

    for (repeat = 1;  repeat <= repeats;  ++repeat)
    {
        ExecMacro(w, event);
    }
}

/*************************************************************
 *
 *                    Semi-public functions
 *
 *************************************************************/

/*
 * This routine is made public for the benefit of AxeTextDeck, but it is
 * not advertised as such in AxeEditor.h. If there are multiple files it
 * is easier to do the expansions after splitting in AxeTextDeck.
 *
 * NB caller must free returned string
 */
String
AxeEditorExpandName(name)
     String name;
{
    String expandedName;

    if (name[0] == '~')
    {
        expandedName = XtMalloc(CLASS(home_dir_len) + strlen(name) + 1);
        strcpy(expandedName, CLASS(home_dir));
        strcat(expandedName, "/");
        strcat(expandedName, &name[2]);
    }
#ifdef __apollo
    else if (strncmp (name, "`node_data", 10) == 0)
    {
	expandedName = XtNewString(name);
    }
#endif
    else if (name[0] == '/')
    {
        expandedName = XtNewString(name);
    }
    else
    {
	expandedName = XtMalloc(CLASS(current_dir_len) + strlen(name) + 1);
        strcpy(expandedName, CLASS(current_dir));
        strcat(expandedName, name);
    }

    return expandedName;
}

/*************************************************************
 *
 *                      Public functions
 *
 *************************************************************/

int
AxeEditorNextEditor()
{
    int editor;

    /*
       AxeWindows are named according to the slot number of the child
       AxeEditor. But the AxeWindow has to be created, and hence named,
       before the AxeEditor is created. This makes sure a correct slot
       number is returned in the first case
     */
    XtInitializeWidgetClass(axeEditorWidgetClass);

    for (editor = 0;  editor < NUMBEROFEDITORS;  ++editor)
    {
	if (!EDITORS[editor])
	{
	    break;
	}
    }

    return editor;
}

Widget
AxeEditorParentOf(widget)
     Widget widget;
{
    Widget axeEditor;
    
    for (axeEditor = XtParent(widget);
	 axeEditor;
	 axeEditor = XtParent(axeEditor))
    {
	if (XtIsSubclass(axeEditor, axeEditorWidgetClass))
	{
	    break;
	}
    }
    return axeEditor;
}

Widget
AxeEditorFileNameWidget(w)
     Widget w;
{
    return (XtIsSubclass(w, axeEditorWidgetClass)) ? PRIVATE(w,file_name)
	                                           : (Widget) 0;
}

Widget
AxeEditorInfoBarWidget(w)
     Widget w;
{
    return (XtIsSubclass(w, axeEditorWidgetClass)) ? PRIVATE(w,info_bar)
	                                           : (Widget) 0;
}

Widget
AxeEditorMenuBarWidget(w)
     Widget w;
{
    return (XtIsSubclass(w, axeEditorWidgetClass)) ? PRIVATE(w,menu_bar)
	                                           : (Widget) 0;
}

Widget
AxeEditorEdWidget(w)
     Widget w;
{
    return (XtIsSubclass(w, axeEditorWidgetClass))
	                 ? AxeTextDeckTop(PRIVATE(w,ed_deck)) : (Widget) 0;
}

Widget
AxeEditorMiniBufferWidget(w)
     Widget w;
{
    return (XtIsSubclass(w, axeEditorWidgetClass)) ? PRIVATE(w,mini_buffer)
	                                           : (Widget) 0;
}

void
AxeEditorUpdateInfoBar(w, info)
     Widget w;
     String info;
{
    if (w && XtIsSubclass(w, axeEditorWidgetClass))
    {
	UpdateInfoBar(w, info);
    }
}

Boolean
AxeEditorIterate(iterator)
     AxeIterationType iterator;
{
    Widget axeEditor;
    int editor;
    Boolean result = True;

    XtInitializeWidgetClass(axeEditorWidgetClass);

    for (editor = 0;  editor < NUMBEROFEDITORS;  ++editor)
    {
	axeEditor = EDITORS[editor];

	if (!axeEditor)
	{
	    continue;
	}

       switch (iterator)
	{
	case AxeSafeClose:
	    result &= WindowClosed(axeEditor);
	    break;
	case AxeForceClose:
	    XtDestroyWidget(axeEditor);
	    break;
	case AxeSave:
	    result &= BuffersSaved(axeEditor);
	    break;
	case AxeSaveAndClose:
	    (void) BuffersSaved(axeEditor);
	    result &= WindowClosed(axeEditor);
	    break;
	default:
	    ;
	}
    }

    return result;
}

Widget
AxeEditorCreateWindow(widget, parent, file)
     Widget widget;
     Widget parent;
     String file;
{
    Widget window, deck;
    char name[3], assocDir[MAXPATHLEN], *assocFile;

    RePopulateShowMenu(XtWidgetToApplicationContext(parent));

    sprintf(name, "%02d", AxeEditorNextEditor());
    window = XtVaCreatePopupShell(name, axeWindowWidgetClass, parent,
				  XtNfile, file,
				  NULL);

    if (XtIsSubclass(widget, axeTextWidgetClass))
    {
	if ( (assocFile = AxeTextGetAssociatedFile(widget)) )
	{
	    strcpy(assocDir, assocFile);
	    assocDir[rindex(assocDir, '/') - assocDir] = '\0';
	}

	deck = PRIVATE(AxeWindowAxeEditor(window),ed_deck);
	AxeTextDeckSetAssociatedDirectory (deck,
					   assocFile ? assocDir : assocFile);
    }

    return window;
}

static XtResource appResource[] =
{
    { "display", "Display", XtRString, sizeof(String),
          (Cardinal) 0, XtRImmediate, (XtPointer) 0
    }
};

void
AxeEditorHelpWindow(helpName, parent)
     String helpName;
     Widget parent;
{
    Atom reqAtom = XA_WINDOW, gotAtom;
    int gotFormat;
    unsigned long gotItems, moreBytes; 
    unsigned char *propValue;
    Display *display = XtDisplay(parent);
    Window propWindow;
    char command[1024], property;
    Boolean startAxinfo = True;
    String dpy;

    /*
     * Check if axinfo is already running. First look to
     * see if expected root window property is present.
     */
    if (XGetWindowProperty(display,
                           XDefaultRootWindow(display),
                           AxeisUserAtom(display),
                           0, 1,
                           False,
                           reqAtom,
                           &gotAtom,
                           &gotFormat,
                           &gotItems,
                           &moreBytes,
                           &propValue) == Success)
    {
	/*
	 * The property does exist. The window that is the value of the
	 * property should have the AXE_AXINFO_user property attached.
	 */
        if (gotAtom == reqAtom)
        {
            propWindow = *((Window *) propValue);
            XFree((char *) propValue);
	    
	    TrapErrors(display);

	    if (XGetWindowProperty(display,
				   propWindow,
				   AxeisUserAtom(display),
				   0, 0,
				   False,
				   XA_STRING,
				   &gotAtom,
				   &gotFormat,
				   &gotItems,
				   &moreBytes,
				   &propValue) == Success)
	    {
		if (gotAtom == None)
		{
		    xerror = BadAtom;
		}
		XFree((char *) propValue);
	    }

	    DontTrapErrors(display);
	    
	    /*
	     * The window id from the root window property seems to be OK.
	     * So go ahead and notify it by changing its AXE_AXINFO property
	     * with the tag of the node that is to be displayed. If that
	     * succeeds then the work is done. 
	     */
	    if (xerror == Success)
	    {
		TrapErrors(display);

		XChangeProperty(display,
				propWindow,
				XInternAtom(display, "AXE_AXINFO", False),
				XA_STRING,
				8,
				PropModeReplace,
				(unsigned char *) helpName,
				strlen(helpName));

		if (xerror == Success)
		{
		    startAxinfo = False;
		}

		DontTrapErrors(display);
	    }
        }
    }

    if (startAxinfo)
    {
	XtVaGetApplicationResources(parent, (XtPointer) &dpy,
				    appResource, (Cardinal) 1,
				    NULL);
	if (dpy)
	{
	    sprintf(command, "axinfo -node %s -display %s &",
		                           (helpName ? helpName : "Top"), dpy);
	}
	else
	{
	    sprintf(command,
		            "axinfo -node %s &",(helpName ? helpName : "Top"));
	}
	system(command);
    }
}    

void
AxeEditorAddShowMenuCallback(widget, proc, client_data)
     Widget widget;
     XtCallbackProc proc;
     XtPointer client_data;
{
    AxeEditorWidgetClass wc;
    int n;

    XtInitializeWidgetClass(axeEditorWidgetClass);

    wc = (AxeEditorWidgetClass) axeEditorWidgetClass;
    n = ++CLASS(show_menu_callbacks);

    CLASS(show_menu_callback_list) = (ShowMenuList)
	     XtRealloc((char *) CLASS(show_menu_callback_list),
		       (Cardinal) (n * sizeof(ShowMenuRec))); 

    CLASS(show_menu_callback_list)[n - 1].widget = widget;
    CLASS(show_menu_callback_list)[n - 1].cbr.callback = proc;
    CLASS(show_menu_callback_list)[n - 1].cbr.closure = client_data;
}

#undef PRIVATE
#undef CLASS

#undef MAXEDITORS
#undef NUMBEROFEDITORS
#undef EDITORS
