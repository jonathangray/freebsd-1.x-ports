/*
 * Copyright 1991, 1992, 1993 The University of Newcastle upon Tyne
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
#include <X11/IntrinsicP.h>	
#include <X11/StringDefs.h>	
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/AsciiText.h>
#include <ScrollText.h>
#include "tickMark.xbm"

#if defined(XtSpecificationRelease) && XtSpecificationRelease > 4
#include <Viewlist.h>
#else
#include <X11/Xaw/Viewport.h>
#endif

#include <X11/Xaw/Command.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <errno.h>
extern int errno;
#include <sys/file.h>
#include <pwd.h>

#ifdef NODIRENT
#include <sys/dir.h>
#define dirent direct
#else
#include <dirent.h>
#endif

#include <stdio.h>

extern char *getenv();

#include "FileNomP.h"

#define Offset(field) XtOffsetOf(FileNominatorRec, fileNominator.field)

static XtResource resources[] = {
    {XtNinitialDirectory, XtCInitialDirectory, XtRString, sizeof(String),
	 Offset(initial_directory), XtRString, (XtPointer) NULL},
    {XtNselectCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
	 Offset(select_callback), XtRCallback, (XtPointer) NULL},
    {XtNpathList, XtCPathList, XtRString, sizeof(String),
	 Offset(path_list), XtRString, (XtPointer) NULL},
    {XtNcancelCallback, XtCCallback, XtRCallback, sizeof(XtCallbackList),
	 Offset(cancel_callback), XtRCallback, (XtPointer) NULL},
    {XtNmargin, XtCMargin, XtRDimension, sizeof(Dimension),
	 Offset(margin), XtRImmediate, (XtPointer) 10},
    {XtNnumberRows, XtCNumberStrings, XtRInt, sizeof(int),
	 Offset(number_rows), XtRImmediate, (XtPointer) 12},
    {XtNshowDotFiles, XtCShowDotFiles, XtRBoolean, sizeof(Boolean),
	 Offset(show_dot_files), XtRImmediate, (XtPointer) True},
    {XtNbellLevel, XtCBellLevel, XtRInt, sizeof(int),
	 Offset(bell_level), XtRImmediate, (XtPointer) 100},
    {XtNfilter, XtCFilter, XtRString, sizeof(String),
	 Offset(filter), XtRImmediate, (XtPointer) "*"},
    {XtNfilterDirectoryNames, XtCFilterDirectoryNames, XtRBoolean, sizeof(Boolean),
	 Offset(filter_directory_names), XtRImmediate, (XtPointer) True},
    {XtNuserData, XtCUserData, XtRPointer, sizeof(XtPointer),
         Offset(user_data), XtRPointer, (XtPointer) NULL},
};

#undef Offset

#define PRIVATE(w,field) (((FileNominatorWidget) w)->fileNominator.field)
#define CLASS(field) fileNominatorClassRec.fileNominator_class.field

#ifdef __STDC__
#define Child(w,child) (((FileNominatorWidget) w)->fileNominator.child##_widget)
#else
#define Child(w,child) (((FileNominatorWidget) w)->fileNominator.child/**/_widget)
#endif
#define List(w) (((FileNominatorWidget) w)->fileNominator.listList)
#define Rows(w) (((FileNominatorWidget) w)->fileNominator.number_rows)
#define CurrentDir(w) (((FileNominatorWidget) w)->fileNominator.currentDir)
#define WatchingChanges(w) (((FileNominatorWidget) w)->fileNominator.watchingChanges)
#define Nomination(w) (((FileNominatorWidget) w)->fileNominator.nomination)
#define ShowDotFiles(w) (((FileNominatorWidget) w)->fileNominator.show_dot_files)
#define BellLevel(w) (((FileNominatorWidget) w)->fileNominator.bell_level)

static char pathTranslations[] =
    "<BtnDown>:reset() MakeMenu() XawPositionSimpleMenu(menu) MenuPopup(menu)";

static char listTranslations[] =
    "<Btn1Up>(2):         Set() Nominate() Unset()\n\
     <Btn1Down>,<Btn1Up>: Set() Notify() \n\
     <Btn2Up>:            Set() Notify() Nominate() Unset()";

static char FilterTranslations[] = 
    "<BtnDown>:           reset() MakeFilterMenu() PopupMenu()";

static char filterTranslations[] = 
    "<Enter>:             FocusToFilter() \n\
     <Leave>:             FocusToFilter(False) \n\
     <Key>Return:         Filter()";

static char filenameTranslations[] =
     "<Key>Return: Nominate() \n\
      <Key>Escape: ToggleDotFiles()";

static char selectTranslations[] =
    "<Btn1Up>: Nominate() unset()";

static void MakeMenu(), FocusToFilter(), Filter(), MakeFilterMenu(), Nominate();
static void ToggleDotFiles(), ToggleFilterDirectories(), ScrollOnMove();

static XtActionsRec actions[] = {
    "scroll-on-movement",   ScrollOnMove,
};

static XtActionsRec pathActions[] = {
    "MakeMenu",                MakeMenu,
    "FocusToFilter",           FocusToFilter,
    "Filter",                  Filter,
    "MakeFilterMenu",          MakeFilterMenu,
    "Nominate",                Nominate,
    "ToggleDotFiles",          ToggleDotFiles,
    "ToggleFilterDirectories", ToggleFilterDirectories,
};

static void                     CancelCallback();
static void                     ChangeDir();
static void                     SelectDir();
static void                     FillWindow();
static void                     ReplaceFilename();
static void                     AsciiSourceChanged();
static void                     WatchForChanges();
static void                     DontWatchForChanges();
static void                     CollapsePath();

static void ClassInitialize(), Initialize();
static void PositionChildren(), Realize(), Destroy();
static Boolean SetValues();

FileNominatorClassRec fileNominatorClassRec = {
    /* Core class part */
  {
    /* superclass	     */	(WidgetClass) &widgetClassRec,
    /* class_name	     */ "FileNominator",
    /* widget_size	     */ sizeof(FileNominatorRec),
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
    /* resize		     */	PositionChildren,
    /* expose		     */	NULL,
    /* set_values	     */	SetValues,
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
   /* FileNominator class part */
  {
    /* extension	     */	NULL,
  }
};

WidgetClass fileNominatorWidgetClass = (WidgetClass) &fileNominatorClassRec;

static void
CalculateSize(fnw, width, height)
    FileNominatorWidget fnw;
    Dimension *width, *height;
{
    Dimension max, wide, high;

    max = 0;
    wide = Child(fnw,Filter)->core.width +
	2 * Child(fnw,Filter)->core.border_width +
	    Child(fnw,filter)->core.width +
		2 * Child(fnw,filter)->core.border_width +
		    3 *fnw->fileNominator.margin;
    if (wide > max) {max = wide;}

    wide = Child(fnw,text)->core.width +
	2 * Child(fnw,filename)->core.border_width +
	    2 * fnw->fileNominator.margin;
    if (wide > max) {max = wide;}

    wide = Child(fnw,select)->core.width + 
	2 * Child(fnw,select)->core.border_width +
	    Child(fnw,path)->core.width +
		2 * Child(fnw,path)->core.border_width +
		    Child(fnw,cancel)->core.width +
			2 * Child(fnw,cancel)->core.border_width +
			    4 * fnw->fileNominator.margin;
    if (wide > max) {max = wide;}
    *width = max;

    *height = 0;
    max = 0;
    high = Child(fnw,Filter)->core.height +
	2 * Child(fnw,Filter)->core.border_width;
    if (high > max) {max = high;};

    high = Child(fnw,filter)->core.height +
	2 * Child(fnw,filter)->core.border_width;
    if (high > max) {max = high;};
    *height += max;

    max = 0;
    high = Child(fnw,select)->core.height +
	2 * Child(fnw,select)->core.border_width;
    if (high > max) {max = high;};

    high = Child(fnw,path)->core.height +
	2 * Child(fnw,path)->core.border_width;
    if (high > max) {max = high;};

    high = Child(fnw,cancel)->core.height +
	2 * Child(fnw,cancel)->core.border_width;
    if (high > max) {max = high;};
    *height += max;

    *height += Child(fnw,viewport)->core.height +
	Child(fnw,text)->core.height +
	    2 * (Child(fnw,viewport)->core.border_width +
		 Child(fnw,filename)->core.border_width) +
		     5 * fnw->fileNominator.margin;
}

static void
PositionChildren(fnw)
    FileNominatorWidget fnw;
{
    XtConfigureWidget(Child(fnw,viewport),
		      fnw->fileNominator.margin,
		      fnw->fileNominator.margin,
	              fnw->core.width -
                      2 * fnw->fileNominator.margin -
                      2 * Child(fnw,viewport)->core.border_width,
		      fnw->core.height -
		      2 * Child(fnw,viewport)->core.border_width -
		      Child(fnw,filter)->core.height -
		      2 * Child(fnw,filter)->core.border_width -
		      Child(fnw,text)->core.height -
		      2 * Child(fnw,filename)->core.border_width -
		      Child(fnw,select)->core.height -
		      2 * Child(fnw,select)->core.border_width -
		      5 * fnw->fileNominator.margin,
		      Child(fnw,viewport)->core.border_width);

    XtConfigureWidget(Child(fnw,Filter), fnw->fileNominator.margin,
		      fnw->core.height -
		      Child(fnw,select)->core.height -
		      2 * Child(fnw,select)->core.border_width -
		      Child(fnw,text)->core.height -
		      2 * Child(fnw,filename)->core.border_width -
		      Child(fnw,Filter)->core.height -
		      2 * Child(fnw,Filter)->core.border_width -
		      3 * fnw->fileNominator.margin,
		      Child(fnw,Filter)->core.width,
		      Child(fnw,Filter)->core.height,
		      Child(fnw,Filter)->core.border_width);

    XtConfigureWidget(Child(fnw,filter),
		      2 * fnw->fileNominator.margin +
		      Child(fnw,Filter)->core.width +
		      2 * Child(fnw,Filter)->core.border_width,
		      fnw->core.height -
		      Child(fnw,select)->core.height -
		      2 * Child(fnw,select)->core.border_width -
		      Child(fnw,text)->core.height -
		      2 * Child(fnw,filename)->core.border_width -
		      Child(fnw,filter)->core.height -
		      2 * Child(fnw,filter)->core.border_width -
		      3 * fnw->fileNominator.margin,
		      fnw->core.width -
		      3 * fnw->fileNominator.margin -
		      Child(fnw,Filter)->core.width - 
		      2 * Child(fnw,Filter)->core.border_width -
		      2 * Child(fnw,filter)->core.border_width,
		      Child(fnw,filter)->core.height,
		      Child(fnw,filter)->core.border_width);

    XtConfigureWidget(Child(fnw,filename), fnw->fileNominator.margin,
		      fnw->core.height -
		      Child(fnw,select)->core.height -
		      2 * Child(fnw,select)->core.border_width -
		      Child(fnw,text)->core.height -
		      2 * Child(fnw,filename)->core.border_width -
		      2 * fnw->fileNominator.margin,
		      fnw->core.width -
		      2 * fnw->fileNominator.margin -
		      2 * Child(fnw,filename)->core.border_width,
		      Child(fnw,text)->core.height,
		      Child(fnw,filename)->core.border_width);

    XtMoveWidget(Child(fnw,select), fnw->fileNominator.margin,
		 fnw->core.height -
		 fnw->fileNominator.margin -
		 2 * Child(fnw,select)->core.border_width -
		 Child(fnw,select)->core.height);

    XtMoveWidget(Child(fnw,path),
		 Child(fnw,select)->core.width +
		 2 * Child(fnw,select)->core.border_width +
		 2 * fnw->fileNominator.margin,
		 fnw->core.height -
		 fnw->fileNominator.margin -
		 2 * Child(fnw,select)->core.border_width -
		 Child(fnw,select)->core.height);

    XtMoveWidget(Child(fnw,cancel), 
		 Child(fnw,path)->core.width +
		 2 * Child(fnw,path)->core.border_width +
		 Child(fnw,select)->core.width +
		 2 * Child(fnw,select)->core.border_width +
		 3 * fnw->fileNominator.margin,
		 fnw->core.height -
		 fnw->fileNominator.margin -
		 2 * Child(fnw,select)->core.border_width -
		 Child(fnw,select)->core.height);
}

static void
ClassInitialize()
{
    CLASS(pathTranslations) = XtParseTranslationTable(pathTranslations);
    CLASS(listTranslations) = XtParseTranslationTable(listTranslations);
    CLASS(FilterTranslations) = XtParseTranslationTable(FilterTranslations);
    CLASS(filterTranslations) = XtParseTranslationTable(filterTranslations);
    CLASS(filenameTranslations) =
	                      XtParseTranslationTable(filenameTranslations);
    CLASS(selectTranslations) = XtParseTranslationTable(selectTranslations);
}

/* ARGSUSED */
static void
Initialize(req, new, args, num_args)
    Widget req, new;
    ArgList args;
    Cardinal *num_args;
{
    FileNominatorWidget fnw = (FileNominatorWidget) new;
    struct stat fstats;
    int status;
    String dir;
    Dimension width, height;

#if defined(SYSV) || defined(SVR4)
    extern char *getcwd();
#define getwd(buf) getcwd(buf,MAXPATHLEN)
#else
    extern char *getwd();
#endif

    List(new) = NULL;
    Nomination(new).directoryPart = NULL;
    Nomination(new).filenamePart = NULL;

    *(CurrentDir(new)) = '\0';
    if (PRIVATE(new,initial_directory))
    {
	status = stat(PRIVATE(new,initial_directory), &fstats);
	if (status != -1 && (fstats.st_mode & S_IFDIR)
	                 && access(PRIVATE(new,initial_directory), R_OK) == 0)
	{
	    strcpy(CurrentDir(new), PRIVATE(new,initial_directory));
	}
    }

    if (!(*(CurrentDir(new))))
    {
	if (!getwd(CurrentDir(new)))
	{
	    if ( (dir = getenv("HOME")) )
	    {
		strcpy(CurrentDir(new), dir);
	    }
	    else
	    {
		strcpy(CurrentDir(new), "/");
	    }
	}
    }

    if (CurrentDir(new)[strlen(CurrentDir(new)) - 1] != '/')
    {
	strcat(CurrentDir(new), "/");
    }

    WatchingChanges(new) = False;

#if defined(XtSpecificationRelease) && XtSpecificationRelease > 4
    Child(fnw,viewport)
	= XtVaCreateWidget("viewport", viewlistWidgetClass, new,
			   XtNallowVert, True,
			   NULL);

    XtVaGetValues(Child(fnw,viewport),
		  XtNlistWidget, &(Child(fnw,list)),
		  NULL);
#else
   /*
    * I haven't a clue why the layout is messed up when a Viewlist is used
    * instead of a Viewport, and I haven't time to spend investigating it, so
    * those still at R4 will have to continue to put with the old undesirable
    * redrawing behaviour that the Viewlist widget improves upon.
    */
    Child(fnw,viewport)
	= XtVaCreateWidget("list", viewportWidgetClass, new,
			   XtNallowVert, True,
			   NULL);

    Child(fnw,list)
	= XtVaCreateManagedWidget("list", listWidgetClass, Child(fnw,viewport),
				  NULL);
#endif    

    XtVaSetValues(Child(fnw,list),
		  XtNdefaultColumns, 1,
		  XtNforceColumns, True,
		  NULL);
    XtOverrideTranslations(Child(fnw,list), CLASS(listTranslations));
    XtAddCallback(Child(fnw,list), XtNcallback, ReplaceFilename, NULL); 

    Child(fnw,Filter)
	= XtVaCreateWidget("Filter", menuButtonWidgetClass, new,
			   NULL);
    XtOverrideTranslations(Child(fnw,Filter),CLASS(FilterTranslations));
    PRIVATE(fnw,filter_menu) = (Widget) 0;

    Child(fnw,filter)
	= XtVaCreateWidget("filter", scrollingTextWidgetClass, new, NULL);
    {
	Widget text;

	XtVaGetValues(Child(fnw,filter), XtNtextWidget, &text, NULL);
	XtVaSetValues(text,
		      XtNeditType, XawtextEdit,
		      XtNstring, PRIVATE(new,filter),
		      XtNdisplayCaret, False,
		      NULL);
	XtOverrideTranslations(text, CLASS(filterTranslations));
    }

    Child(fnw,filename)
	= XtVaCreateWidget("filename", scrollingTextWidgetClass, new, NULL);
    {
	Widget text;

	XtVaGetValues(Child(fnw,filename), XtNtextWidget, &text, NULL);
	XtVaSetValues(text, XtNeditType, XawtextEdit, NULL);
	Child(fnw,text) = text;
    }

    XtOverrideTranslations(Child(fnw,text), CLASS(filenameTranslations));
    XtSetKeyboardFocus(new, Child(fnw,text));
    WatchForChanges(fnw);

    Child(fnw,select)
	= XtVaCreateWidget("select", commandWidgetClass, new,
			   NULL);
    XtOverrideTranslations(Child(fnw,select), CLASS(selectTranslations));

    Child(fnw,path)
	= XtVaCreateWidget("path", menuButtonWidgetClass, new,
			   NULL);
    XtOverrideTranslations(Child(fnw,path), CLASS(pathTranslations));
    XtAppAddActions(XtWidgetToApplicationContext(new),
		                          pathActions, XtNumber(pathActions));
    XawSimpleMenuAddGlobalActions(XtWidgetToApplicationContext(new));

    Child(fnw,cancel)
	= XtVaCreateWidget("cancel", commandWidgetClass, new, NULL);
    XtAddCallback(Child(fnw,cancel), XtNcallback, CancelCallback, NULL); 

    FillWindow(fnw);
    CalculateSize(fnw, &width, &height);
    fnw->core.width = fnw->core.width ? fnw->core.width : width;
    fnw->core.height = fnw->core.height ? fnw->core.height : height;
    PositionChildren(fnw);
}

static void 
Realize(w, valueMask, attributes)
    Widget w;
    XtValueMask *valueMask;
    XSetWindowAttributes *attributes;
{
    (*fileNominatorWidgetClass->core_class.superclass->core_class.realize)
	(w, valueMask, attributes);

    XtRealizeWidget(Child(w,viewport));
    XtRealizeWidget(Child(w,Filter));
    XtRealizeWidget(Child(w,filter));
    XtRealizeWidget(Child(w,filename));
    XtRealizeWidget(Child(w,select));
    XtRealizeWidget(Child(w,path));
    XtRealizeWidget(Child(w,cancel));

    XMapSubwindows(XtDisplay(w), XtWindow(w));
}

static void Destroy(w)
    Widget w;
{
    int idx;
    FileNominatorWidget fnw = (FileNominatorWidget) w;
    
    XtSetKeyboardFocus(w, (Widget) 0);

    XtDestroyWidget(Child(fnw,viewport));
    XtDestroyWidget(Child(fnw,Filter));
    XtDestroyWidget(Child(fnw,filter));
    XtDestroyWidget(Child(fnw,filename));
    XtDestroyWidget(Child(fnw,select));
    XtDestroyWidget(Child(fnw,path));
    XtDestroyWidget(Child(fnw,cancel));

    idx = 0;
    while (List(fnw)[idx])
    {
	XtFree((char *) List(fnw)[idx++]);
    }
    XtFree((char *) List(fnw)[idx]);
    XtFree((char *) List(fnw));

    XtFree(Nomination(fnw).directoryPart);
    XtFree(Nomination(fnw).filenamePart);
}

/* ARGSUSED */
static Boolean
SetValues(old, request, new, args, num_args)
     Widget old, request, new;
     ArgList args;
     Cardinal *num_args;
{
    FileNominatorWidget oldfnw = (FileNominatorWidget) old;
    FileNominatorWidget newfnw = (FileNominatorWidget) new;
    Widget filter;
    Boolean redisplay = False, refilter = False;

#define NE(field) (oldfnw->field != newfnw->field)

    XtVaGetValues(Child(new,filter), XtNtextWidget, &filter, NULL);

    if (NE(fileNominator.filter))
    {
	XtVaSetValues(filter, XtNstring, PRIVATE(new,filter), NULL);

	refilter = True;
    }

    if (NE(fileNominator.filter_directory_names) && PRIVATE(new,filter_menu))
    {
	XtVaSetValues(PRIVATE(new,apply_dirs),
		      XtNleftBitmap, PRIVATE(new,filter_directory_names) ?
		                     CLASS(tickMark) : (Pixmap) 0,
		      NULL);
	refilter = True;
    }

    if (NE(fileNominator.show_dot_files))
    {
	if(PRIVATE(new,filter_menu))
	{
	    XtVaSetValues(PRIVATE(new,apply_dots),
			  XtNleftBitmap, PRIVATE(new,show_dot_files) ?
			                         CLASS(tickMark) : (Pixmap) 0,
			  NULL);
	}
	refilter = True;
    }

    /*
     * This doesn't work yet; the list grows in width as margin is changed
     *
    if (NE(fileNominator.margin))
    {
	CalculateSize(new, &(new->core.width), &(new->core.height));
	PositionChildren(new);
	redisplay = True;
    }
    */

#undef NE

    if (refilter)
    {
	Filter(filter, (XEvent *) 0, (String *) 0, (Cardinal *) 0);
    }

    return redisplay;
}

/* ARGSUSED */
static void
ScrollOnMove(widget, event, params, num_params)
     Widget widget;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    XtVaSetValues(Child(widget,filename),
		  XtNscrollOnMovement, *params[0] == 'T' ? True : False,
		  NULL);

    XtVaSetValues(Child(widget,filter),
		  XtNscrollOnMovement, *params[0] == 'T' ? True : False,
		  NULL);
}

/* ARGSUSED */
static void 
CancelCallback(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    XtCallCallbacks(XtParent(w), XtNcancelCallback, NULL);
}

/* ARGSUSED */
static void 
MenuFilter(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    Widget fnw;

    if (!client_data)
    {
	return;
    }

    for (fnw = XtParent(w);
	 !XtIsSubclass(fnw, fileNominatorWidgetClass);
	 fnw = XtParent(fnw))
	;

    XtCallActionProc(Child(fnw,Filter), (String) client_data, (XEvent *) 0, (String *) 0, (Cardinal) 0);
}

/* ARGSUSED */
static void
MakeFilterMenu(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    FileNominatorWidget fnw = (FileNominatorWidget) XtParent(w);
    String menuName;
    Widget menu, menuEntry;

    if (PRIVATE(fnw,filter_menu))
    {
	return;
    }

    if (!CLASS(tickMark))
    {
        Display *display = XtDisplay(w);

        CLASS(tickMark)
	    = XCreateBitmapFromData(display, XDefaultRootWindow(display),
				    (char *) tickMark_bits,
				    tickMark_width, tickMark_height);
    }

    XtVaGetValues(w,
		  XtNmenuName, &menuName,
		  NULL);
    
    menu = XtVaCreatePopupShell(menuName,simpleMenuWidgetClass, w, NULL);

    menuEntry =	XtVaCreateManagedWidget("Apply",
					smeBSBObjectClass, menu,
					XtNleftMargin, 16,
					XtNrightMargin, 16,
					NULL);
    XtAddCallback(menuEntry, XtNcallback, MenuFilter, (XtPointer) "Filter");

    XtVaCreateManagedWidget("line", smeLineObjectClass, menu, NULL);

    menuEntry = PRIVATE(fnw,apply_dirs)
	= XtVaCreateManagedWidget("Apply to directories",
				  smeBSBObjectClass, menu,
				  XtNleftMargin, 16,
				  XtNrightMargin, 16,
				  XtNleftBitmap,
				          PRIVATE(fnw,filter_directory_names) ?
				          CLASS(tickMark) : (Pixmap) 0,
				  NULL);
    XtAddCallback(menuEntry, XtNcallback, MenuFilter, (XtPointer) "ToggleFilterDirectories"); 
    
    menuEntry = PRIVATE(fnw,apply_dots)
	= XtVaCreateManagedWidget("Apply to \".\" files",
				  smeBSBObjectClass, menu,
				  XtNleftMargin, 16,
				  XtNrightMargin, 16,
				  XtNleftBitmap,
				          PRIVATE(fnw,show_dot_files) ?
				          CLASS(tickMark) : (Pixmap) 0,
				  NULL);
    XtAddCallback(menuEntry, XtNcallback, MenuFilter, (XtPointer) "ToggleDotFiles");

    PRIVATE(fnw,filter_menu) = menu;
}

/* ARGSUSED */
static void
DestroyMenu(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    XtDestroyWidget(w);
}

/* ARGSUSED */
static void
MakeMenu(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    FileNominatorWidget fnw = (FileNominatorWidget) XtParent(w);
    String menuName, menuList;
    Widget menu, menuEntry;
    char *where, *p1, *p2, *p, *q, *TopLevelDir;
    int menuItem = 1, len;

    XtVaGetValues(w,
		  XtNmenuName, &menuName,
		  NULL);
    
    menu = XtVaCreatePopupShell(menuName,
				simpleMenuWidgetClass, w,
				NULL);
    XtAddCallback(menu, XtNpopdownCallback, DestroyMenu, NULL);

    where = XtNewString(CurrentDir(fnw));
#ifdef __apollo
    if (strncmp (where, "//", 2) == 0)
    {
        p1 = where + 2;  /* leading // on pathname, if there */
        TopLevelDir = "//";
    }
    else
    {
        p1 = where + 1;  /* else skip the single leading slash */
        TopLevelDir = "/";
    }
#else
    p1 = where + 1;
    TopLevelDir = "/";
#endif

    menuEntry = XtVaCreateManagedWidget(TopLevelDir,
                                        smeBSBObjectClass, menu, NULL);
    XtAddCallback(menuEntry, XtNcallback, ChangeDir, (XtPointer) menuItem++);

    len = strlen(where);
    while (p1 < &where[len])
    {
	if (!(p2 = index(p1, '/')))
	{
	    p2 = &where[len];
	}
	*p2 = '\0';
	menuEntry = XtVaCreateManagedWidget(p1,
					    smeBSBObjectClass, menu, NULL);
	XtAddCallback(menuEntry, XtNcallback, ChangeDir,
		      (XtPointer) menuItem++);
	p1 = p2 + 1;
    }

    if (!fnw->fileNominator.path_list)
    {
	XtVaSetValues(menu,
		      XtNpopupOnEntry, menuEntry,
		      NULL);
    }
    else
    {
	menuEntry =
	    XtVaCreateManagedWidget("sep", smeLineObjectClass, menu, NULL);

	XtVaSetValues(menu,
		      XtNpopupOnEntry, menuEntry,
		      NULL);
    
	menuList = XtNewString(fnw->fileNominator.path_list);
	for (p = menuList;  (q = index(p, ':'));  p = q + 1)
	{
	    *q = '\0';
	    menuEntry = XtVaCreateManagedWidget(p,
						smeBSBObjectClass, menu,
						NULL);
	    XtAddCallback(menuEntry, XtNcallback, SelectDir, NULL);
	}
	menuEntry = XtVaCreateManagedWidget(p,
					    smeBSBObjectClass, menu,
					    NULL);
	XtAddCallback(menuEntry, XtNcallback, SelectDir, NULL);

	XtFree(menuList);
    }

    XtFree(where);
}

static void
ChangeDirectory(fnw, position)
     FileNominatorWidget fnw;
     int position;
{
    String p;
    int m;

    if (position > 0)
    {
	p = CurrentDir(fnw);
#ifdef __apollo
        /* skip first slash, if there of //nodename so we get the right dir */
	if (strncmp (p, "//", 2) == 0)
            p++;
#endif
	for (m = 0;  m < position;  ++m)
	{
	    while(*p++ != '/')
		;
	}
	*p = '\0';
    }

    XtVaSetValues(Child(fnw, text),
		  XtNstring, "",
		  NULL);

    FillWindow(fnw);

    PositionChildren(fnw);
}
	 
/* ARGSUSED */
static void
ChangeDir(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    FileNominatorWidget fnw
        = (FileNominatorWidget) XtParent(XtParent(XtParent(w)));

    ChangeDirectory(fnw, (int) client_data);
}

/* ARGSUSED */
static void
SelectDir(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    FileNominatorWidget fnw
        = (FileNominatorWidget) XtParent(XtParent(XtParent(w)));
    String label;

    XtVaGetValues(w,
		  XtNlabel, &label,
		  NULL);

    XtVaSetValues(Child(fnw,text),
		  XtNstring, label,
		  NULL);

    Nominate(Child(fnw,select), NULL, NULL, NULL);
}

static char*
GetEnv(name)
     char *name;
{
    char *r, *env, *e;

    env = XtNewString(name);
    if ((r = index(env, '/')))
    {
	*r = '\0';
    }
    e = getenv(env);

    XtFree(env);
    
    return e;
}

static char*
GetUser(name)
     char *name;
{
    char *i, *user, *u;
    struct passwd *gotUser;

    user = XtNewString(name);
    if ((i = index(user, '/')))
    {
	*i = '\0';
    }

    if ((gotUser = getpwnam(user)))
    {
	u = gotUser->pw_dir;
    }
    else
    {
	u = (char *) 0;
    }

    XtFree(user);
    
    return u;
}

/* ARGSUSED */
static void
Nominate(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Widget fnw;
    char *nomination, *home, *user, *envVar, selection[MAXPATHLEN], *newPath, *r;
    struct stat fstats;
    int status, len;

    for (fnw = XtParent(w);
	 !XtIsSubclass(fnw, fileNominatorWidgetClass);
	 fnw = XtParent(fnw))
	;

    XtVaGetValues(Child(fnw,text),
		  XtNstring, &nomination,
		  NULL);

    selection[0] = '\0';
    if (*nomination == '/')
    {
	strcpy(selection, nomination);
    }
    else if (*nomination == '~')
    {
	if ((*(nomination + 1) == '/' || *(nomination + 1) == '\0')
                                                  && (home = getenv("HOME")))
	{
	    strcpy(selection, home);
	    strcat(selection, &nomination[1]);
	}
	else if ((user = GetUser(&nomination[1])))
	{
	    strcpy(selection, user);
	    if ((r = index(&nomination[1], '/')))
	    {
		strcat(selection, r);
	    }
	}
    }
    else if (*nomination == '$' && (envVar = GetEnv(&nomination[1])))
    {
	strcpy(selection, envVar);
	if ((r = index(&nomination[1], '/')))
	{
	    strcat(selection, r);
	}
    }
#ifdef __apollo
    else if (*nomination == '`' &&
	                          (strncmp("`node_data", nomination, 10) == 0))
    {
        strcat(selection, nomination);
    }
#endif

    if (strlen(selection) == 0)
    {
	if (strlen(CurrentDir(fnw)) > 1)
	{
	    strcpy(selection, CurrentDir(fnw));
	}
	strcat(selection, "/");
	strcat(selection, nomination);
    }

    len = strlen(selection);
    if (len != 0)
    {
	newPath = (char *) XtMalloc(len + 2);
	CollapsePath(selection, newPath);
	status = stat(newPath, &fstats);
	if (status != -1 && fstats.st_mode & S_IFDIR)
	{
	    if (access(newPath, R_OK) == 0)
	    {
		if (newPath[strlen(newPath) - 1] != '/')
		{
		    strcat(newPath, "/");
		}
		strcpy(CurrentDir(fnw), newPath);
		ChangeDirectory(fnw, 0);
	    }
	    else
	    {
		XBell(XtDisplay(fnw), BellLevel(fnw));
	    }
	}
	else if (status == 0 || (status == -1 && errno == ENOENT))
	{
	    r = rindex(newPath, '/');
	    XtFree(Nomination(fnw).filenamePart);
	    Nomination(fnw).filenamePart = XtNewString(r + 1);
	    if (status == 0)
	    {
		status |=
		    (access(newPath, R_OK) == 0) ? FileNominatorReadable : 0;
		status |=
		    (access(newPath, W_OK) == 0) ? FileNominatorWritable : 0;
		status |=
		    (access(newPath, X_OK) == 0) ? FileNominatorExecutable : 0;
	    }
	    else
	    {
		status = FileNominatorNonexistent;
	    }
	    Nomination(fnw).filenameStatus = status;
	    *(r + 1) = '\0';
	    XtFree(Nomination(fnw).directoryPart);
	    status = 0;
	    status |=
		(access(newPath, R_OK) == 0) ? FileNominatorReadable : 0;
	    status |=
		(access(newPath, W_OK) == 0) ? FileNominatorWritable : 0;
	    status |=
		(access(newPath, X_OK) == 0) ? FileNominatorExecutable : 0;
	    if (strcmp(newPath, CurrentDir(fnw)) != 0 &&
		                              (status & FileNominatorReadable))
	    {
		strcpy(CurrentDir(fnw), newPath);
		ChangeDirectory(fnw, 0);
		Nomination(fnw).directoryPart = XtNewString(CurrentDir(fnw));
	    }
	    else
	    {
		Nomination(fnw).directoryPart = XtNewString(newPath);
	    }
	    Nomination(fnw).directoryStatus = status;
	    XtCallCallbacks((Widget) fnw, XtNselectCallback,
			                        (XtPointer) &Nomination(fnw));
	}
	else
	{
            XBell(XtDisplay(fnw), BellLevel(fnw));
	}
	XtFree(newPath);
    }
}

static void
FillWindow(fnw)
     Widget fnw;
{
    XFontStruct *font;
    Dimension height, internalHeight, rowSpacing;
    int num, newNum, idx;
    struct dirent **namelist;
    extern int alphasort();
    char buf[MAXPATHLEN], *bp;
    String name;
    struct stat fstats;
    Widget text, vscroller;
    float top = 0.0;
    String p, m, empty = "*";
    int i;
    extern Boolean match();

    XtVaGetValues(Child(fnw,filter), XtNtextWidget, &text, NULL);
    XtVaGetValues(text, XtNstring, &p, NULL);
    if (!*p)
    {
	p = empty;
    }
    m = XtNewString(p);
    for(i=0;  p[i]; ++i)
    {
	switch (p[i]) {
	case '?':
	case '*':
	case '[':
	    m[i] = 1;
	    break;
	default:
	    m[i] = 0;
	}
    }    

    num = scandir(CurrentDir(fnw), &namelist, (int(*)())0, alphasort);
    if (num <= 0)
    {
	return;
    }

    if (List(fnw))
    {
        idx = 0;
        while (List(fnw)[idx])
        {
            XtFree((char *) List(fnw)[idx++]);
        }
	XtFree((char *) List(fnw)[idx]);
	XtFree((char *) List(fnw));
    }
    List(fnw) = (String *) XtMalloc((num + 1) * sizeof(String));

    strcpy(buf, CurrentDir(fnw));
    bp = buf + strlen(buf);
    for(idx = 0, newNum = 0; idx < num;  idx++)
    {
	name = namelist[idx]->d_name;
	if (ShowDotFiles(fnw) || (!ShowDotFiles(fnw) &&
				  (*name != '.' 
				  || ((strcmp(name, ".") == 0) ||
				      (strcmp(name, "..") == 0)))))
	{
	    if (match(p, m, name))
	    {
		List(fnw)[newNum] = XtMalloc(strlen(name) + 2);
		strcpy(List(fnw)[newNum], name);
		strcpy(bp, name);
		(void) stat(buf, &fstats);
		if (fstats.st_mode & S_IFDIR)
		{
		    strcat(List(fnw)[newNum], "/");
		}
		++newNum;
	    }
	    else if (!PRIVATE(fnw,filter_directory_names))
	    {
		strcpy(bp, name);
		(void) stat(buf, &fstats);
		if (fstats.st_mode & S_IFDIR)
		{
		    List(fnw)[newNum] = XtMalloc(strlen(name) + 2);
		    strcpy(List(fnw)[newNum], name);
		    strcat(List(fnw)[newNum], "/");
		    ++newNum;
		}
	    }
	}
    }

    for(idx = 0; idx < num;  idx++)
    {
	XtFree((char *) namelist[idx]);
    }
    XtFree((char *) namelist);

    XtFree(m);

    List(fnw)[newNum] = NULL;

    XtVaGetValues(Child(fnw,list),
                  XtNfont, &font,
                  XtNinternalHeight, &internalHeight,
		  XtNrowSpacing, &rowSpacing,
                  NULL);

    height = Rows(fnw) * (font->max_bounds.ascent +
		     font->max_bounds.descent + rowSpacing) -
			 rowSpacing + 2 * internalHeight; 

    XtVaSetValues(Child(fnw,viewport),
                  XtNheight, height,
                  NULL);

    XawListChange(Child(fnw,list), List(fnw), newNum, -1, True);

    if (XtIsRealized(Child(fnw,viewport)))
    {
#if defined(XtSpecificationRelease) && XtSpecificationRelease > 4
	XawViewportSetLocation(Child(fnw,viewport), 0.0, 0.0);
#else
	/* Kludge to force scroll to top of new list */
	if ((vscroller = XtNameToWidget(Child(fnw,viewport), "vertical")))
	{
	    XtCallCallbacks(vscroller, XtNjumpProc, (XtPointer) &top);
	}
#endif
    }
}

/* ARGSUSED */
static void
FocusToFilter(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Boolean on = (*num_params == 0);
    Widget fnw = XtParent(w);

    while (!XtIsSubclass(fnw, fileNominatorWidgetClass))
    {
	fnw = XtParent(fnw);
    }

    XtSetKeyboardFocus(fnw, on ? (Widget) 0 : Child(fnw,text));
    XtVaSetValues(w, XtNdisplayCaret, on, NULL);
    XtVaSetValues(Child(fnw,text), XtNdisplayCaret, !on, NULL);
}

/* ARGSUSED */
static void
Filter(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Widget fnw = XtParent(w);

    while (!XtIsSubclass(fnw, fileNominatorWidgetClass))
    {
	fnw = XtParent(fnw);
    }

    FillWindow(fnw);
    PositionChildren(fnw);
}

/* ARGSUSED */
static void
ToggleFilterDirectories(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Boolean filterDirs;
    Widget fnw = XtParent(w);

    while (!XtIsSubclass(fnw, fileNominatorWidgetClass))
    {
	fnw = XtParent(fnw);
    }

    XtVaGetValues(fnw,
		  XtNfilterDirectoryNames, &filterDirs,
		  NULL);

    XtVaSetValues(fnw,
		  XtNfilterDirectoryNames, !filterDirs,
		  NULL);
}

/* ARGSUSED */
static void
ToggleDotFiles(w, event, params, num_params)
     Widget w;
     XEvent *event;
     String *params;
     Cardinal *num_params;
{
    Boolean showDotFiles;
    Widget fnw = XtParent(w);

    while (!XtIsSubclass(fnw, fileNominatorWidgetClass))
    {
	fnw = XtParent(fnw);
    }

    XtVaGetValues(fnw,
		  XtNshowDotFiles, &showDotFiles,
		  NULL);

    XtVaSetValues(fnw,
		  XtNshowDotFiles, !showDotFiles,
		  NULL);
}

/* ARGSUSED */
static void
ReplaceFilename(w, client_data, call_data)
     Widget w;
     XtPointer client_data, call_data;
{
    FileNominatorWidget fnw
	= (FileNominatorWidget) XtParent(XtParent(w));

    XawListReturnStruct *list = XawListShowCurrent(Child(fnw,list));

    XtVaSetValues(Child(fnw,text),
		  XtNstring, list->string,
		  NULL);

    XawTextSetInsertionPoint(Child(fnw,text),
			     (XawTextPosition) strlen(list->string));

    XtFree((char *) list);

    WatchForChanges(fnw);
}

/* ARGSUSED */
static void
AsciiSourceChanged(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    FileNominatorWidget fnw = (FileNominatorWidget) client_data;

    DontWatchForChanges(fnw);

    XawListUnhighlight(Child(fnw,list));
}

static void
WatchForChanges(fnw)
     Widget fnw;
{
    if (!WatchingChanges(fnw))
    {
	XtAddCallback(XawTextGetSource(Child(fnw,text)), XtNcallback,
		      AsciiSourceChanged, (XtPointer) fnw);

	WatchingChanges(fnw) = True;
    }
}

static void
DontWatchForChanges(fnw)
     Widget fnw;
{
    XtRemoveCallback(XawTextGetSource(Child(fnw,text)), XtNcallback,
		     AsciiSourceChanged, (XtPointer) fnw);

    WatchingChanges(fnw) = False;
}

static void 
CollapsePath(in, out)
     char *in, *out;
{
    char *p = in, *q = out, *pend = p + strlen(p);
#ifdef __apollo
    int m = 0;
#endif 
    
    while (p < pend)
    {
	if (*p != '/')
	{
	    *q++ = *p++;
	}
	else if (p + 1 < pend && *(p + 1) == '/')
	{
#ifdef __apollo
	    if (m == 0) /* copy the // at the beginning of an apollo path */
		*q++ = *p++;
	    else
#endif
              ++p;
	}
	else if ( (p + 2 == pend && *(p + 1) == '.') || 
		  (p + 2 < pend && *(p + 1) == '.' && *(p + 2) == '/') )
	{
	    p += 2;
	}
	else if ( (p + 3 == pend && *(p + 1) == '.' && *(p + 2) == '.') ||
		 (p + 3 < pend && *(p + 1) == '.'
		                      && *(p + 2) == '.' && *(p + 3) == '/') )
	{
	    while (q > out && *--q != '/')
		;
	    p += 3;
	}
	else
	{
	    *q++ = *p++;
	}
#ifdef __apollo
	m++;
#endif
    }
    if (q == out)
    {
	*q++ = '/';
    }

    while (q > out)
    {
	if (*--q != '/')
	    break;
    }
    *++q = '\0';
}

String
FileNominatorGetDirectory(fnw)
     Widget fnw;
{
    if (XtIsSubclass(fnw, fileNominatorWidgetClass))
    {
	return CurrentDir(fnw);
    }
    else
    {
	return NULL;
    }
}

void
FileNominatorSetDirectory(fnw, dir)
     Widget fnw;
     String dir;
{
    if (!XtIsSubclass(fnw, fileNominatorWidgetClass))
    {
	return;
    }

    /* Should do more checks */
    strcpy(CurrentDir(fnw), dir);
    if (dir[strlen(dir) - 1] != '/')
    {
	strcat(CurrentDir(fnw), "/");
    }

    ChangeDirectory(fnw, 0);
}

void
FileNominatorClearName(widget)
     Widget(widget);
{
    if (!XtIsSubclass(widget, fileNominatorWidgetClass))
    {
	return;
    }

    XtVaSetValues(Child(widget,text), XtNstring, "", NULL);
    XawListUnhighlight(Child(widget,list));
}

Widget
FileNominatorViewportWidget(widget)
     Widget(widget);
{
    if (XtIsSubclass(widget, fileNominatorWidgetClass))
    {
	return Child(widget,viewport);
    }
    else
    {
	return (Widget) 0;
    }
}

Widget
FileNominatorListWidget(widget)
     Widget(widget);
{
    if (XtIsSubclass(widget, fileNominatorWidgetClass))
    {
	return Child(widget,list);
    }
    else
    {
	return (Widget) 0;
    }
}

#undef CLASS
