/*
 * main.c -- Main routine for ghostview.
 * Copyright (C) 1992  Timothy O. Theisen
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *   Author: Tim Theisen           Systems Programmer
 * Internet: tim@cs.wisc.edu       Department of Computer Sciences
 *     UUCP: uwvax!tim             University of Wisconsin-Madison
 *    Phone: (608)262-0438         1210 West Dayton Street
 *      FAX: (608)262-9777         Madison, WI   53706
 */

#include <X11/Intrinsic.h>
#include <X11/cursorfont.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#ifdef VMS
#include <X11/bitmaps/dot.>
#include <X11/bitmaps/menu16.>
#include <X11/bitmaps/tie_fighter.>
#else
#include <X11/bitmaps/dot>
#include <X11/bitmaps/menu16>
#include <X11/bitmaps/tie_fighter>
#endif

#include <X11/Xaw/Cardinals.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Command.h>

#include "Ghostview.h"
#include "gv.h"
#include "ps.h"

extern char *getenv();

static String version = "Ghostview, version 1.5";

static XtResource resources[] = {
    {"showTitle", "Labels", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, show_title), XtRImmediate, (XtPointer)True},
    {"showDate", "Labels", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, show_date), XtRImmediate, (XtPointer)True},
    {"showLocator", "Labels", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, show_locator), XtRImmediate, (XtPointer)True},
    {"autoCenter", "AutoCenter", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, auto_center), XtRImmediate, (XtPointer)True},
    {"horizontalMargin", "Margin", XtRInt, sizeof(int),
     XtOffsetOf(AppResources, wm_horiz_margin), XtRImmediate, (XtPointer)20},
    {"verticalMargin", "Margin", XtRInt, sizeof(int),
     XtOffsetOf(AppResources, wm_vert_margin), XtRImmediate, (XtPointer)44},
    {"minimumMagstep", "Magstep", XtRInt, sizeof(int),
     XtOffsetOf(AppResources, minimum_magstep), XtRImmediate, (XtPointer)-5},
    {"maximumMagstep", "Magstep", XtRInt, sizeof(int),
     XtOffsetOf(AppResources, maximum_magstep), XtRImmediate, (XtPointer)5},
    {"magstep", "Magstep", XtRInt, sizeof(int),
     XtOffsetOf(AppResources, magstep), XtRImmediate, (XtPointer)0},
    {"orientation", "Orientation", XtRPageOrientation,
     sizeof(XtPageOrientation), XtOffsetOf(AppResources, orientation),
     XtRImmediate, (XtPointer) XtPageOrientationPortrait},
    {"page", "Page", XtRString, sizeof(String),
     XtOffsetOf(AppResources, page), XtRImmediate, NULL},
    {"pageMedia", "PageMedia", XtRString, sizeof(String),
     XtOffsetOf(AppResources, pagemedia), XtRImmediate, "Letter"},
    {"forceOrientation", "Force", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, force_orientation), XtRImmediate,
     (XtPointer)False},
    {"forcePageMedia", "Force", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, force_pagemedia), XtRImmediate, (XtPointer)False},
    {"swapLandscape", "SwapLandscape", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, swap_landscape), XtRImmediate, (XtPointer)False},
#ifndef VMS
#if defined(SVR4) || defined(SYSV) || defined(USG)
    {"printCommand", "PrintCommand", XtRString, sizeof(String),
     XtOffsetOf(AppResources, print_command), XtRImmediate, "lp"},
    {"printerVariable", "PrinterVariable", XtRString, sizeof(String),
     XtOffsetOf(AppResources, printer_variable), XtRImmediate, "LPDEST"},
#else
    {"printCommand", "PrintCommand", XtRString, sizeof(String),
     XtOffsetOf(AppResources, print_command), XtRImmediate, "lpr"},
    {"printerVariable", "PrinterVariable", XtRString, sizeof(String),
     XtOffsetOf(AppResources, printer_variable), XtRImmediate, "PRINTER"},
#endif
    {"printPrompt", "PrintPrompt", XtRString, sizeof(String),
     XtOffsetOf(AppResources, print_prompt), XtRImmediate, "Printer Name:"},
#else /* VMS */
    {"printCommand", "PrintCommand", XtRString, sizeof(String),
     XtOffsetOf(AppResources, print_command), XtRImmediate, "Print_dcl/Delete"},
    {"printerVariable", "PrinterVariable", XtRString, sizeof(String),
     XtOffsetOf(AppResources, printer_variable), XtRImmediate, "GV_PRINT_QUAL"},
    {"printPrompt", "PrintPrompt", XtRString, sizeof(String),
     XtOffsetOf(AppResources, print_prompt), XtRImmediate, "Print Qualifiers:"},
#endif /* VMS */
    {"defaultPrinter", "DefaultPrinter", XtRString, sizeof(String),
     XtOffsetOf(AppResources, default_printer), XtRImmediate, NULL},
    {"printFail", "printFail", XtRString, sizeof(String),
     XtOffsetOf(AppResources, print_fail), XtRImmediate,
     "\"%s\" command failed."},
    {"openPrompt", "OpenPrompt", XtRString, sizeof(String),
     XtOffsetOf(AppResources, open_prompt), XtRImmediate, "Open File:"},
    {"openFail", "OpenFail", XtRString, sizeof(String),
     XtOffsetOf(AppResources, open_fail), XtRImmediate, "Cannot open file: "},
    {"savePrompt", "SavePrompt", XtRString, sizeof(String),
     XtOffsetOf(AppResources, save_prompt), XtRImmediate, "Save File:"},
    {"saveFail", "SaveFail", XtRString, sizeof(String),
     XtOffsetOf(AppResources, save_fail), XtRImmediate, "Cannot save file: "},
    {"openWindows", "OpenWindows", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, openwindows), XtRImmediate, (XtPointer)False},
    {"ncdwm", "Ncdwm", XtRBoolean, sizeof(Boolean),
     XtOffsetOf(AppResources, ncdwm), XtRImmediate, (XtPointer)False},
};

static XrmOptionDescRec options[] = {
    {"-monochrome", "*Ghostview.palette", XrmoptionNoArg, "Monochrome"},
    {"-grayscale", "*Ghostview.palette", XrmoptionNoArg, "Grayscale"},
    {"-color", "*Ghostview.palette", XrmoptionNoArg, "Color"},
    {"-page", ".page", XrmoptionSepArg, NULL},
    {"-title", ".showTitle", XrmoptionNoArg, "True"},
    {"-notitle", ".showTitle", XrmoptionNoArg, "False"},
    {"-date", ".showDate", XrmoptionNoArg, "True"},
    {"-nodate", ".showDate", XrmoptionNoArg, "False"},
    {"-locator", ".showLocator", XrmoptionNoArg, "True"},
    {"-nolocator", ".showLocator", XrmoptionNoArg, "False"},
    {"-center", ".autoCenter", XrmoptionNoArg, "True"},
    {"-nocenter", ".autoCenter", XrmoptionNoArg, "False"},
    {"-labels", ".Labels", XrmoptionNoArg, "True"},
    {"-nolabels", ".Labels", XrmoptionNoArg, "False"},
    {"-quiet", "*Ghostview.quiet", XrmoptionNoArg, "True"},
    {"-noquiet", "*Ghostview.quiet", XrmoptionNoArg, "False"},
    {"-safer", "*Ghostview.safer", XrmoptionNoArg, "True"},
    {"-nosafer", "*Ghostview.safer", XrmoptionNoArg, "False"},
    {"-arguments", "*Ghostview.arguments", XrmoptionSepArg, NULL},
    {"-xdpi", "*Ghostview.xdpi", XrmoptionSepArg, NULL},
    {"-ydpi", "*Ghostview.ydpi", XrmoptionSepArg, NULL},
    {"-dpi", "*Ghostview.Resolution", XrmoptionSepArg, NULL},
    {"-resolution", "*Ghostview.Resolution", XrmoptionSepArg, NULL},
    {"-magstep", ".magstep", XrmoptionSepArg, NULL},
    {"-portrait", ".orientation", XrmoptionNoArg, "Portrait"},
    {"-landscape", ".orientation", XrmoptionNoArg, "Landscape"},
    {"-upsidedown", ".orientation", XrmoptionNoArg, "Upside-down"},
    {"-seascape", ".orientation", XrmoptionNoArg, "Seascape"},
    {"-forceorientation", ".forceOrientation", XrmoptionNoArg, "True"},
    {"-letter", ".pageMedia", XrmoptionNoArg, "Letter"},
    {"-tabloid", ".pageMedia", XrmoptionNoArg, "Tabloid"},
    {"-ledger", ".pageMedia", XrmoptionNoArg, "Ledger"},
    {"-legal", ".pageMedia", XrmoptionNoArg, "Legal"},
    {"-statement", ".pageMedia", XrmoptionNoArg, "Statement"},
    {"-executive", ".pageMedia", XrmoptionNoArg, "Executive"},
    {"-a3", ".pageMedia", XrmoptionNoArg, "A3"},
    {"-a4", ".pageMedia", XrmoptionNoArg, "A4"},
    {"-a5", ".pageMedia", XrmoptionNoArg, "A5"},
    {"-b4", ".pageMedia", XrmoptionNoArg, "B4"},
    {"-b5", ".pageMedia", XrmoptionNoArg, "B5"},
    {"-folio", ".pageMedia", XrmoptionNoArg, "Folio"},
    {"-quarto", ".pageMedia", XrmoptionNoArg, "Quarto"},
    {"-10x14", ".pageMedia", XrmoptionNoArg, "10x14"},
    {"-forcemedia", ".forcePageMedia", XrmoptionNoArg, "True"},
    {"-force", ".Force", XrmoptionNoArg, "True"},
    {"-swap", ".swapLandscape", XrmoptionNoArg, "True"},
    {"-noswap", ".swapLandscape", XrmoptionNoArg, "False"},
    {"-openwindows", ".openWindows", XrmoptionNoArg, "True"},
    {"-noopenwindows", ".openWindows", XrmoptionNoArg, "False"},
    {"-ncdwm", ".ncdwm", XrmoptionNoArg, "True"},
    {"-noncdwm", ".ncdwm", XrmoptionNoArg, "False"},
};

static XtActionsRec actions[] = {
    {"GhostviewCopyright",	gv_copyright},
    {"GhostviewQuit",		gv_quit},
    {"GhostviewOpen",		gv_open},
    {"GhostviewReopen",		gv_reopen},
    {"GhostviewSave",		gv_save},
    {"GhostviewPrintWhole",	gv_print_whole},
    {"GhostviewPrintMarked",	gv_print_marked},
    {"GhostviewPrevious",	gv_prev},
    {"GhostviewShow",		gv_show},
    {"GhostviewNext",		gv_next},
    {"GhostviewCenter",		gv_center},
    {"GhostviewMark",		gv_mark},
    {"GhostviewUnmark",		gv_unmark},
    {"GhostviewSetMagstep",	gv_set_magstep},
    {"GhostviewIncreaseMagstep",gv_increase_magstep},
    {"GhostviewDecreaseMagstep",gv_decrease_magstep},
    {"GhostviewSetOrientation",	gv_set_orientation},
    {"GhostviewSwapLandscape",	gv_swap_landscape},
    {"GhostviewSetPageMedia",	gv_set_pagemedia},
    {"GhostviewDefault",	gv_default},
    {"GhostviewForce",		gv_force},
    {"GhostviewDeleteWindow",	gv_delete_window},
    {"GhostviewDeleteZoom",	gv_delete_zoom},
    {"GhostviewDismiss",	gv_dismiss},
    {"GhostviewScrollUp",	gv_scroll_up},
    {"GhostviewScrollDown",	gv_scroll_down},
    {"GhostviewScrollLeft",	gv_scroll_left},
    {"GhostviewScrollRight",	gv_scroll_right},
    {"GhostviewEraseLocator",	gv_erase_locator},
    {"GhostviewCheckFile",	gv_check_file},
};

String fallback_resources[] = {
#   include "app-defaults.h"
    NULL
};

#define ROWS 22
#define COLS 68
static String copyright = "\
Ghostview -- An X11 user interface for ghostscript.\n\
Copyright (C) 1992  Timothy O. Theisen\n\
\n\
This program is free software; you can redistribute it and/or modify\n\
it under the terms of the GNU General Public License as published by\n\
the Free Software Foundation; either version 2 of the License, or\n\
(at your option) any later version.\n\
\n\
This program is distributed in the hope that it will be useful,\n\
but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
GNU General Public License for more details.\n\
\n\
You should have received a copy of the GNU General Public License\n\
along with this program; if not, write to the Free Software\n\
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.\n\
\n\
  Author: Tim Theisen           Systems Programmer\n\
Internet: tim@cs.wisc.edu       Department of Computer Sciences\n\
    UUCP: uwvax!tim             University of Wisconsin-Madison\n\
   Phone: (608)262-0438         1210 West Dayton Street\n\
     FAX: (608)262-9777         Madison, WI   53706";

static void Syntax();

float	default_xdpi;	/* default xdpi from ghostview widget */
float	default_ydpi;	/* default ydpi from ghostview widget */

int	num_ghosts;	/* number of ghostview widgets active */
FILE	*psfile;	/* file to display */
String	filename;	/* its filename */
String	oldfilename;	/* previous filename */
int	current_page;	/* current page being displayed */
int	current_magstep;/* current magnification */
XtPageOrientation	current_orientation; /* current orientation */
int	default_pagemedia;	/* index into document media or papersizes */
int	current_pagemedia;	/* index into document media or papersizes */
Boolean	force_document_media;	/* Whether to force a document media */
int	document_media;		/* document media being forced */
int	current_llx;		/* current bounding box */
int	current_lly;
int	current_urx;
int	current_ury;
int	base_papersize;		/* tells where papersizes begins */
Boolean	info_up;		/* tells if information window is up */
int	force_setting;		/* tells if new setting override %%comments */
Pixmap	dot_bitmap;		/* bitmap used to mark default setting */
Pixmap	menu16_bitmap;		/* bitmap used to mark document setting */
Pixmap	tie_fighter_bitmap;	/* bitmap used to mark forced setting */
String	toc_text;	/* page labels (Table of Contents) */
int	toc_length;	/* length of page label text */
int	toc_entry_length; /* length of one entry */
int	info_length;	/* number of bytes in infotext window */
int	mode;	/* tells what type of popup */
time_t	mtime;		/* last modified time of input file */
struct document *doc;	/* document structure */
struct document *olddoc;	/* document structure */
Atom	wm_delete_window;	/* Atom sent to destroy a window */
XErrorHandler	old_Xerror;	/* standard error handler */
Boolean	dying;		/* whether an X error caused our exit */
XErrorEvent	bomb;	/* what the error was */

XtAppContext app_con;
AppResources app_res;

/* Widgets used.  Indentation indicates hierarchy */
        Widget toplevel;
        Widget     form;
        Widget         titlebutton;
        Widget             titlemenu;
        Widget         datebutton;
        Widget             datemenu;
        Widget         locator;
        Widget         box;
        Widget             filebutton;
        Widget                 filemenu;
        Widget                     openbutton;
        Widget                     reopenbutton;
        Widget                     printwholebutton;
        Widget                     printmarkedbutton;
        Widget                     savebutton;
        Widget                     copyrightbutton;
        Widget                     quitbutton;
        Widget             pagebutton;
        Widget                 pagemenu;
        Widget                     nextbutton;
        Widget                     showbutton;
        Widget                     prevbutton;
        Widget                     centerbutton;
        Widget                     markbutton;
        Widget                     unmarkbutton;
        Widget             magstepbutton;
        Widget                 magstepmenu;
        Widget                     *magstepentry;
        Widget             orientationbutton;
        Widget                 orientationmenu;
        Widget                     portraitbutton;
        Widget                     landscapebutton;
        Widget                     upsidedownbutton;
        Widget                     seascapebutton;
        Widget                     swapbutton;
        Widget             pagemediabutton;
        Widget                 pagemediamenu;
        Widget                     *pagemediaentry;
        Widget         toc;
        Widget         pageview;
        Widget             page;

/* Popup children */
        Widget infopopup;
        Widget     infoform;
        Widget         infotext;
        Widget         infodismiss;
        Widget copyrightpopup;
        Widget     copyrightform;
        Widget         copyrighttext;
        Widget         copyrightdismiss;
        Widget dialogpopup;
        Widget     dialog;

int
main(argc, argv)
int argc;
char *argv[];
{
    struct stat sbuf;
    Display *dpy;
    Screen *scr;
    Arg args[20];
    Cardinal num_args;
    Widget above_toc;
    Widget left_of_page;
    Widget line;
    int i;
    Boolean set_vert_dist;
    String s1, s2;
    XFontStruct *font;
    XTextProperty nameprop;
    Dimension bottomMargin, leftMargin, rightMargin, topMargin;
    Dimension width, height;
    Dimension button_width;
    static String nothing = "";
    static XawTextSelectType sarry[] =
	    {XawselectLine, XawselectAll, XawselectNull};

    XtToolkitInitialize();
    XtSetTypeConverter(XtRString, XtRPageOrientation,
		       XmuCvtStringToPageOrientation, NULL, 0,
		       XtCacheAll, NULL);
    app_con = XtCreateApplicationContext();
    XtAppAddActions(app_con, actions, XtNumber(actions));
    XtAppSetFallbackResources(app_con, fallback_resources);
    dpy = XtOpenDisplay(app_con, NULL, NULL, "Ghostview",
			options, XtNumber(options), &argc, argv);
    if (dpy == NULL) {
	fprintf(stderr, "%s: cannot open DISPLAY.\n", argv[0]);
	exit(1);
    }
    if (argc > 2) Syntax(argv[0]);
    if (argc == 2) {
	filename = XtNewString(argv[1]);
	if (strcmp(filename, "-")) {
#ifdef VMS
	    if ((psfile = fopen(argv[1], "r", "mbc=100")) == NULL) {
#else
	    if ((psfile = fopen(argv[1], "r")) == NULL) {
#endif
		fprintf(stderr, "Cannot open ");
		perror(argv[1]);
		exit(1);
	    }
	    stat(filename, &sbuf);
	    mtime = sbuf.st_mtime;
	}
    }

    old_Xerror = XSetErrorHandler(catch_Xerror);
    scr = DefaultScreenOfDisplay(dpy);
    wm_delete_window = XInternAtom(dpy, "WM_DELETE_WINDOW", False);

    toplevel = XtAppCreateShell(NULL, "Ghostview", applicationShellWidgetClass,
				dpy, NULL, ZERO);

    XtGetApplicationResources(toplevel, (XtPointer) &app_res,
			      resources, XtNumber(resources), NULL, ZERO);
    if (s1 = getenv(app_res.printer_variable)) app_res.default_printer = s1;

    /* Open Windows sometimes hands me a bad bitmap */
    if (app_res.openwindows) {
	dot_bitmap = menu16_bitmap = tie_fighter_bitmap = None;
    } else {
	dot_bitmap = XCreateBitmapFromData(dpy, RootWindowOfScreen(scr),
					   dot_bits, dot_width, dot_height);
	menu16_bitmap = XCreateBitmapFromData(dpy, RootWindowOfScreen(scr),
					      menu16_bits, menu16_width,
					      menu16_height);
	tie_fighter_bitmap = XCreateBitmapFromData(dpy, RootWindowOfScreen(scr),
						   tie_fighter_bits,
						   tie_fighter_width,
						   tie_fighter_height);
    }
    
    /* Instantiate Popup children */
    infopopup = XtCreatePopupShell("information", topLevelShellWidgetClass,
				   toplevel, NULL, ZERO);

    infoform = XtCreateManagedWidget("form", formWidgetClass,
				     infopopup, NULL, ZERO);

							num_args = 0;
    XtSetArg(args[num_args], XtNtop, XtChainTop);	num_args++;
    XtSetArg(args[num_args], XtNbottom, XtChainBottom);	num_args++;
    XtSetArg(args[num_args], XtNleft, XtChainLeft);	num_args++;
    XtSetArg(args[num_args], XtNright, XtChainRight);	num_args++;
    XtSetArg(args[num_args], XtNscrollHorizontal, XawtextScrollWhenNeeded);
							num_args++;
    XtSetArg(args[num_args], XtNscrollVertical, XawtextScrollWhenNeeded);
							num_args++;
    XtSetArg(args[num_args], XtNdisplayCaret, False);	num_args++;
    infotext = XtCreateManagedWidget("text", asciiTextWidgetClass,
				     infoform, args, num_args);

							num_args = 0;
    XtSetArg(args[num_args], XtNfromVert, infotext);	num_args++;
    XtSetArg(args[num_args], XtNtop, XtChainBottom);	num_args++;
    XtSetArg(args[num_args], XtNbottom, XtChainBottom);	num_args++;
    XtSetArg(args[num_args], XtNleft, XtChainLeft);	num_args++;
    XtSetArg(args[num_args], XtNright, XtChainRight);	num_args++;
    infodismiss = XtCreateManagedWidget("dismiss", commandWidgetClass,
				       infoform, args, num_args);
    XtAddCallback(infodismiss, XtNcallback, dismiss, (XtPointer)infopopup);

							num_args = 0;
    XtSetArg(args[num_args], XtNfont, &font);		num_args++;
    XtSetArg(args[num_args], XtNbottomMargin, &bottomMargin);num_args++;
    XtSetArg(args[num_args], XtNleftMargin, &leftMargin);num_args++;
    XtSetArg(args[num_args], XtNrightMargin, &rightMargin);num_args++;
    XtSetArg(args[num_args], XtNtopMargin, &topMargin);	num_args++;
    XtGetValues(infotext, args, num_args);

    width = font->max_bounds.width * 80 + leftMargin + rightMargin;
    height = (font->ascent + font->descent) * ROWS + topMargin + bottomMargin;

    XtSetArg(args[0], XtNwidth, width);
    XtSetArg(args[1], XtNheight, height);
    XtSetValues(infotext, args, TWO);
    XtSetValues(infodismiss, args, ONE);
    XtRealizeWidget(infopopup);
    XSetWMProtocols(dpy, XtWindow(infopopup), &wm_delete_window, 1);

    copyrightpopup = XtCreatePopupShell("copyright", topLevelShellWidgetClass,
					toplevel, NULL, ZERO);

    copyrightform = XtCreateManagedWidget("form", formWidgetClass,
					  copyrightpopup, NULL, ZERO);

							num_args = 0;
    XtSetArg(args[num_args], XtNtop, XtChainTop);	num_args++;
    XtSetArg(args[num_args], XtNbottom, XtChainBottom);	num_args++;
    XtSetArg(args[num_args], XtNleft, XtChainLeft);	num_args++;
    XtSetArg(args[num_args], XtNright, XtChainRight);	num_args++;
    XtSetArg(args[num_args], XtNscrollHorizontal, XawtextScrollWhenNeeded);
							num_args++;
    XtSetArg(args[num_args], XtNscrollVertical, XawtextScrollWhenNeeded);
							num_args++;
    XtSetArg(args[num_args], XtNdisplayCaret, False);	num_args++;
    XtSetArg(args[num_args], XtNuseStringInPlace, True);num_args++;
    XtSetArg(args[num_args], XtNlength, strlen(copyright));num_args++;
    XtSetArg(args[num_args], XtNstring, copyright);	num_args++;
    copyrighttext = XtCreateManagedWidget("text", asciiTextWidgetClass,
					  copyrightform, args, num_args);

							num_args = 0;
    XtSetArg(args[num_args], XtNfromVert, copyrighttext);
							num_args++;
    XtSetArg(args[num_args], XtNtop, XtChainBottom);	num_args++;
    XtSetArg(args[num_args], XtNbottom, XtChainBottom);	num_args++;
    XtSetArg(args[num_args], XtNleft, XtChainLeft);	num_args++;
    XtSetArg(args[num_args], XtNright, XtChainRight);	num_args++;
    copyrightdismiss = XtCreateManagedWidget("dismiss", commandWidgetClass,
					     copyrightform, args, num_args);
    XtAddCallback(copyrightdismiss, XtNcallback, dismiss,
		  (XtPointer)copyrightpopup);

							num_args = 0;
    XtSetArg(args[num_args], XtNfont, &font);		num_args++;
    XtSetArg(args[num_args], XtNbottomMargin, &bottomMargin);num_args++;
    XtSetArg(args[num_args], XtNleftMargin, &leftMargin);num_args++;
    XtSetArg(args[num_args], XtNrightMargin, &rightMargin);num_args++;
    XtSetArg(args[num_args], XtNtopMargin, &topMargin);	num_args++;
    XtGetValues(copyrighttext, args, num_args);

    width = font->max_bounds.width * COLS + leftMargin + rightMargin;
    height = (font->ascent + font->descent) * ROWS + topMargin + bottomMargin;

    XtSetArg(args[0], XtNwidth, width);
    XtSetArg(args[1], XtNheight, height);
    XtSetValues(copyrighttext, args, TWO);
    XtSetValues(copyrightdismiss, args, ONE);
    XtRealizeWidget(copyrightpopup);
    XSetWMProtocols(dpy, XtWindow(copyrightpopup), &wm_delete_window, 1);

    dialogpopup = XtCreatePopupShell("popup", transientShellWidgetClass,
				     toplevel, NULL, ZERO);

    dialog = CreateDialog(dialogpopup, "dialog", okay, dismiss);
    XtRealizeWidget(dialogpopup);
    XSetWMProtocols(dpy, XtWindow(dialogpopup), &wm_delete_window, 1);


    /* Instantiate Widgets */

    form = XtCreateManagedWidget("form", formWidgetClass,
				 toplevel, NULL, ZERO);

    above_toc = NULL;
    left_of_page = NULL;
    set_vert_dist = False;
    if (app_res.show_title) {
							num_args = 0;
	XtSetArg(args[num_args], XtNfromVert, above_toc);num_args++;
	XtSetArg(args[num_args], XtNtop, XtChainTop);	num_args++;
	XtSetArg(args[num_args], XtNbottom, XtChainTop);num_args++;
	XtSetArg(args[num_args], XtNleft, XtChainLeft);	num_args++;
	XtSetArg(args[num_args], XtNright, XtChainLeft);num_args++;
	XtSetArg(args[num_args], XtNresizable, True);	num_args++;
	XtSetArg(args[num_args], XtNborderWidth, 0);	num_args++;
	XtSetArg(args[num_args], XtNresize, False);	num_args++;
	XtSetArg(args[num_args], XtNlabel, " ");	num_args++;
	titlebutton = XtCreateManagedWidget("titleButton",
					    menuButtonWidgetClass,
				            form, args, num_args);
	above_toc = titlebutton;
	if (!left_of_page) left_of_page = titlebutton;
	set_vert_dist = True;
    }

    if (app_res.show_date) {
							num_args = 0;
	XtSetArg(args[num_args], XtNfromVert, above_toc);num_args++;
	if (set_vert_dist) {
	    XtSetArg(args[num_args], XtNvertDistance, 0);num_args++;
	}
	XtSetArg(args[num_args], XtNtop, XtChainTop);	num_args++;
	XtSetArg(args[num_args], XtNbottom, XtChainTop);num_args++;
	XtSetArg(args[num_args], XtNleft, XtChainLeft);	num_args++;
	XtSetArg(args[num_args], XtNright, XtChainLeft);num_args++;
	XtSetArg(args[num_args], XtNresizable, True);	num_args++;
	XtSetArg(args[num_args], XtNborderWidth, 0);	num_args++;
	XtSetArg(args[num_args], XtNresize, False);	num_args++;
	XtSetArg(args[num_args], XtNlabel, " ");	num_args++;
	datebutton = XtCreateManagedWidget("dateButton", menuButtonWidgetClass,
					   form, args, num_args);
	above_toc = datebutton;
	if (!left_of_page) left_of_page = datebutton;
	set_vert_dist = True;
    }

    if (app_res.show_locator) {
							num_args = 0;
	XtSetArg(args[num_args], XtNfromVert, above_toc);num_args++;
	if (set_vert_dist) {
	    XtSetArg(args[num_args], XtNvertDistance, 0);num_args++;
	}
	XtSetArg(args[num_args], XtNtop, XtChainTop);	num_args++;
	XtSetArg(args[num_args], XtNbottom, XtChainTop);num_args++;
	XtSetArg(args[num_args], XtNleft, XtChainLeft);	num_args++;
	XtSetArg(args[num_args], XtNright, XtChainLeft);num_args++;
	XtSetArg(args[num_args], XtNresizable, True);	num_args++;
	XtSetArg(args[num_args], XtNborderWidth, 0);	num_args++;
	XtSetArg(args[num_args], XtNresize, False);	num_args++;
	XtSetArg(args[num_args], XtNlabel, " ");	num_args++;
	locator = XtCreateManagedWidget("locator", labelWidgetClass,
					form, args, num_args);
	above_toc = locator;
	if (!left_of_page) left_of_page = locator;
    }

							num_args = 0;
    XtSetArg(args[num_args], XtNfromVert, above_toc);	num_args++;
    XtSetArg(args[num_args], XtNtop, XtChainTop);	num_args++;
    XtSetArg(args[num_args], XtNbottom, XtChainTop);	num_args++;
    XtSetArg(args[num_args], XtNleft, XtChainLeft);	num_args++;
    XtSetArg(args[num_args], XtNright, XtChainLeft);	num_args++;
    XtSetArg(args[num_args], XtNresizable, True);	num_args++;
    XtSetArg(args[num_args], XtNallowVert, True);	num_args++;
    box = XtCreateManagedWidget("box", boxWidgetClass, form, args, num_args);

    XtSetArg(args[0], XtNresize, False);
    filebutton = XtCreateManagedWidget("fileButton", menuButtonWidgetClass,
				       box, args, ONE);

    filemenu = XtCreatePopupShell("menu", simpleMenuWidgetClass,
				  filebutton, NULL, ZERO);

    openbutton = XtCreateManagedWidget("open", smeBSBObjectClass,
				       filemenu, NULL, ZERO);
    XtAddCallback(openbutton, XtNcallback, popup_dialog, (XtPointer)OPEN);

    reopenbutton = XtCreateManagedWidget("reopen", smeBSBObjectClass,
				         filemenu, NULL, ZERO);
    XtAddCallback(reopenbutton, XtNcallback, reopen_file, (XtPointer)NULL);

    printwholebutton = XtCreateManagedWidget("printwhole", smeBSBObjectClass,
				             filemenu, NULL, ZERO);
    XtAddCallback(printwholebutton, XtNcallback, popup_dialog,
		  (XtPointer)PRINT_WHOLE);

    printmarkedbutton = XtCreateManagedWidget("printmarked", smeBSBObjectClass,
				              filemenu, NULL, ZERO);
    XtAddCallback(printmarkedbutton, XtNcallback, popup_dialog,
		  (XtPointer)PRINT_MARKED);

    savebutton = XtCreateManagedWidget("save", smeBSBObjectClass,
				       filemenu, NULL, ZERO);
    XtAddCallback(savebutton, XtNcallback, popup_dialog, (XtPointer)SAVE);

    line = XtCreateManagedWidget("line", smeLineObjectClass,
				 filemenu, NULL, ZERO);

    copyrightbutton = XtCreateManagedWidget("copyright", smeBSBObjectClass,
					    filemenu, NULL, ZERO);
    XtAddCallback(copyrightbutton, XtNcallback, popup,
		  (XtPointer)copyrightpopup);

    quitbutton = XtCreateManagedWidget("quit", smeBSBObjectClass,
				       filemenu, NULL, ZERO);
    XtAddCallback(quitbutton, XtNcallback, quit_ghostview, (XtPointer)0);

    XtSetArg(args[0], XtNresize, False);
    pagebutton = XtCreateManagedWidget("pageButton", menuButtonWidgetClass,
				       box, args, 1);

    pagemenu = XtCreatePopupShell("menu", simpleMenuWidgetClass,
				  pagebutton, NULL, ZERO);

    nextbutton = XtCreateManagedWidget("next", smeBSBObjectClass,
				       pagemenu, NULL, ZERO);
    XtAddCallback(nextbutton, XtNcallback, next_page, (XtPointer)0);

    showbutton = XtCreateManagedWidget("show", smeBSBObjectClass,
				       pagemenu, NULL, ZERO);
    XtAddCallback(showbutton, XtNcallback, this_page, (XtPointer)0);

    prevbutton = XtCreateManagedWidget("prev", smeBSBObjectClass,
				       pagemenu, NULL, ZERO);
    XtAddCallback(prevbutton, XtNcallback, prev_page, (XtPointer)0);

    line = XtCreateManagedWidget("line", smeLineObjectClass,
				 pagemenu, NULL, ZERO);

    centerbutton = XtCreateManagedWidget("center", smeBSBObjectClass,
				       pagemenu, NULL, ZERO);
    XtAddCallback(centerbutton, XtNcallback, center_page, (XtPointer)0);

    line = XtCreateManagedWidget("line", smeLineObjectClass,
				 pagemenu, NULL, ZERO);

    markbutton = XtCreateManagedWidget("mark", smeBSBObjectClass,
				       pagemenu, NULL, ZERO);
    XtAddCallback(markbutton, XtNcallback, mark_page, (XtPointer)0);

    unmarkbutton = XtCreateManagedWidget("unmark", smeBSBObjectClass,
				         pagemenu, NULL, ZERO);
    XtAddCallback(unmarkbutton, XtNcallback, unmark_page, (XtPointer)0);

    XtSetArg(args[0], XtNresize, False);
    magstepbutton = XtCreateManagedWidget("magstepButton",
					  menuButtonWidgetClass,
					  box, args, ONE);

    magstepmenu = XtCreatePopupShell("menu", simpleMenuWidgetClass,
				     magstepbutton, NULL, ZERO);

    magstepentry = (Widget *) XtMalloc(
	    (app_res.maximum_magstep - app_res.minimum_magstep + 1) *
	    sizeof(Widget));
    XtSetArg(args[0], XtNleftMargin, 20);
    for (i = app_res.minimum_magstep; i <= app_res.maximum_magstep; i++) {
	char buf[16];
	sprintf(buf, "%d", i);
	magstepentry[i-app_res.minimum_magstep] =
		XtCreateManagedWidget(buf, smeBSBObjectClass,
				      magstepmenu, args, 1);
	XtAddCallback(magstepentry[i-app_res.minimum_magstep], XtNcallback,
		      set_magstep, (XtPointer)i);
    }

    XtSetArg(args[0], XtNresize, False);
    orientationbutton = XtCreateManagedWidget("orientationButton",
					      menuButtonWidgetClass,
					      box, args, ONE);

    orientationmenu = XtCreatePopupShell("menu", simpleMenuWidgetClass,
				         orientationbutton, NULL, ZERO);

    XtSetArg(args[0], XtNleftMargin, 20);
    portraitbutton = XtCreateManagedWidget("portrait", smeBSBObjectClass,
				           orientationmenu, args, ONE);
    XtAddCallback(portraitbutton, XtNcallback,
		  set_orientation, (XtPointer)XtPageOrientationPortrait);

    landscapebutton = XtCreateManagedWidget("landscape", smeBSBObjectClass,
				            orientationmenu, args, ONE);
    XtAddCallback(landscapebutton, XtNcallback,
		  set_orientation, (XtPointer)XtPageOrientationLandscape);

    upsidedownbutton = XtCreateManagedWidget("upsidedown", smeBSBObjectClass,
				             orientationmenu, args, ONE);
    XtAddCallback(upsidedownbutton, XtNcallback,
		  set_orientation, (XtPointer)XtPageOrientationUpsideDown);

    seascapebutton = XtCreateManagedWidget("seascape", smeBSBObjectClass,
				             orientationmenu, args, ONE);
    XtAddCallback(seascapebutton, XtNcallback,
		  set_orientation, (XtPointer)XtPageOrientationSeascape);

    line = XtCreateManagedWidget("line", smeLineObjectClass,
				 orientationmenu, NULL, ZERO);

    swapbutton = XtCreateManagedWidget("swap", smeBSBObjectClass,
				       orientationmenu, args, ONE);
    XtAddCallback(swapbutton, XtNcallback, swap_landscape, (XtPointer)0);

    if (app_res.swap_landscape) {
	XtSetArg(args[0], XtNlabel, &s1);
	XtGetValues(landscapebutton, args, ONE);
	s1 = XtNewString(s1);
	XtSetArg(args[0], XtNlabel, &s2);
	XtGetValues(seascapebutton, args, ONE);
	s2 = XtNewString(s2);
	XtSetArg(args[0], XtNlabel, s2);
	XtSetValues(landscapebutton, args, ONE);
	XtSetArg(args[0], XtNlabel, s1);
	XtSetValues(seascapebutton, args, ONE);
	XtFree(s1);
	XtFree(s2);
    }

    XtSetArg(args[0], XtNresize, False);
    pagemediabutton = XtCreateManagedWidget("pagemediaButton",
					    menuButtonWidgetClass,
					    box, args, ONE);

    default_pagemedia = 0;
    for (i = 0; papersizes[i].name; i++) {
        if (!strcmp(app_res.pagemedia, papersizes[i].name)) {
	    default_pagemedia = i;
            break;
        }
    }

#ifndef max
#define max(a,b) ((a)>(b)?(a):(b))
#endif

    /* Line up all the menu buttons */
    XtSetArg(args[0], XtNwidth, &width);
    XtGetValues(filebutton, args, ONE);
    button_width = width;
    XtGetValues(pagebutton, args, ONE);
    button_width = max(button_width, width);
    XtGetValues(magstepbutton, args, ONE);
    button_width = max(button_width, width);
    XtGetValues(orientationbutton, args, ONE);
    button_width = max(button_width, width);
    XtGetValues(pagemediabutton, args, ONE);
    button_width = max(button_width, width);

    XtSetArg(args[0], XtNwidth, button_width);
    XtSetValues(filebutton, args, ONE);
    XtSetValues(pagebutton, args, ONE);
    XtSetValues(magstepbutton, args, ONE);
    XtSetValues(orientationbutton, args, ONE);
    XtSetValues(pagemediabutton, args, ONE);

							num_args = 0;
    XtSetArg(args[num_args], XtNfromVert, above_toc);	num_args++;
    XtSetArg(args[num_args], XtNfromHoriz, box);	num_args++;
    XtSetArg(args[num_args], XtNtop, XtChainTop);	num_args++;
    XtSetArg(args[num_args], XtNbottom, XtChainBottom);	num_args++;
    XtSetArg(args[num_args], XtNleft, XtChainLeft);	num_args++;
    XtSetArg(args[num_args], XtNright, XtChainLeft);	num_args++;
    XtSetArg(args[num_args], XtNresizable, True);	num_args++;
    XtSetArg(args[num_args], XtNdisplayCaret, False);	num_args++;
    XtSetArg(args[num_args], XtNuseStringInPlace, True);num_args++;
    XtSetArg(args[num_args], XtNlength, 0);		num_args++;
    XtSetArg(args[num_args], XtNstring, nothing);	num_args++;
    XtSetArg(args[num_args], XtNselectTypes, sarry);	num_args++;
    XtSetArg(args[num_args], XtNscrollVertical, XawtextScrollAlways);num_args++;
    toc = XtCreateManagedWidget("toc", asciiTextWidgetClass,
				form, args, num_args);
    if (!left_of_page) left_of_page = toc;

							num_args = 0;
    XtSetArg(args[num_args], XtNfromHoriz, left_of_page);num_args++;
    XtSetArg(args[num_args], XtNtop, XtChainTop);	num_args++;
    XtSetArg(args[num_args], XtNbottom, XtChainBottom);	num_args++;
    XtSetArg(args[num_args], XtNleft, XtChainLeft);	num_args++;
    XtSetArg(args[num_args], XtNright, XtChainRight);	num_args++;
    XtSetArg(args[num_args], XtNresizable, True);	num_args++;
    XtSetArg(args[num_args], XtNallowHoriz, True);	num_args++;
    XtSetArg(args[num_args], XtNallowVert, True);	num_args++;
    pageview = XtCreateManagedWidget("pageview", viewportWidgetClass,
				     form, args, num_args);

    page = XtCreateManagedWidget("page", ghostviewWidgetClass,
				 pageview, NULL, ZERO);
    num_ghosts++;
    XtAddCallback(page, XtNcallback, track_and_zoom, (XtPointer)0);
    XtAddCallback(page, XtNdestroyCallback, destroy_ghost, (XtPointer)page);
    XtAddCallback(page, XtNmessageCallback, message, (XtPointer)page);
    XtAddCallback(page, XtNoutputCallback, output, (XtPointer)0);
							num_args = 0;
    XtSetArg(args[num_args], XtNxdpi, &default_xdpi);	num_args++;
    XtSetArg(args[num_args], XtNydpi, &default_ydpi);	num_args++;
    XtGetValues(page, args, num_args);

    /* This sets most of the window sizes.  This keeps X server traffic
     * down during realization.
     */
    GhostviewDisableInterpreter(page);
    setup_ghostview();
    i = find_page(app_res.page);

    /* Coerce page number to fall in range */
    if (toc_text) {
	if (i >= doc->numpages) i = doc->numpages - 1;
	if (i < 0) i = 0;
    }
    /* Coerce magstep to fall in range */
    if (app_res.magstep < app_res.minimum_magstep)
	app_res.magstep = app_res.minimum_magstep;
    if (app_res.magstep > app_res.maximum_magstep)
	app_res.magstep = app_res.maximum_magstep;
    set_new_magstep();
    set_new_orientation(i);
    set_new_pagemedia(i);
    layout_ghostview();

    XtSetMappedWhenManaged(toplevel, False);
    XtRealizeWidget(toplevel);
    XtSetArg(args[0], XtNtransientFor, toplevel);
    XtSetValues(dialogpopup, args, ONE);
    XSetWMProtocols(dpy, XtWindow(toplevel), &wm_delete_window, 1);
    if (XStringListToTextProperty(&version, 1, &nameprop)) {
	XSetWMName(dpy, XtWindow(toplevel), &nameprop);
    }

    /* This sets the sizes on widget that were created during the realize. */
    layout_ghostview();
    XtMapWidget(toplevel);

    show_page(i);
    XtAppMainLoop(app_con);

    /* should never get here */
    return 1;
}

static void
Syntax(call)
char *call;
{
    XtDestroyApplicationContext(app_con);
    fprintf(stderr, "Usage: %s\n", call);
    fprintf(stderr,
		"    [-monochrome] [-grayscale] [-color]\n");
    fprintf(stderr,
		"    [-[no]title] [-[no]date] [-[no]locator] [-[no]labels]\n");
    fprintf(stderr,
		"    [-resolution <dpi>] [-dpi <dpi>]\n");
    fprintf(stderr,
		"    [-xdpi <dpi>] [-ydpi <dpi>] [-magstep <n>]\n");
    fprintf(stderr,
		"    [-[no]safer] [-[no]quiet] [-arguments <arguments>]\n");
    fprintf(stderr,
		"    [-[no]center]\n");
    fprintf(stderr,
		"    [-portrait] [-landscape] [-upsidedown] [-seascape] [-[no]swap]\n");
    fprintf(stderr,
		"    [-letter] [-tabloid] [-ledger] [-legal] [-statement]\n");
    fprintf(stderr,
		"    [-executive] [-a3] [-a4] [-a5] [-b4] [-b5]\n");
    fprintf(stderr,
		"    [-folio] [-quarto] [-10x14]\n");
    fprintf(stderr,
		"    [-force] [-forceorientation] [-forcemedia]\n");
    fprintf(stderr,
		"    [-[no]openwindows] [-[no]ncdwm]\n");
    fprintf(stderr,
		"    [-page <label>] [toolkitoption]\n");
    fprintf(stderr,
		"    [filename]\n");
    exit(1);
}
