/*
 * Copyright (C) 1990 Regents of the University of California.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of the University of
 * California not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission.  the University of California makes no representations
 * about the suitability of this software for any purpose.  It is provided
 * "as is" without express or implied warranty.
 */

# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Shell.h>
# include <X11/Xaw/Form.h>

# include <stdio.h>
# include <string.h>

# include "cdrom_globs.h"
# include "debug.h"
# include "logo.xbm"

XtAppContext	appc;
char		*file;
char		*device;
Boolean		debug		= False;
Boolean		display_timer	= True;
float		volbase		= 187.0;
float		volpcent	= .267;
int		replayThreshold	= 4;
int		pauseSkipInterval	= 15;
int		scanSkipInterval	= 1;
float		scanPauseInterval	= .062;
float		pausePauseInterval	= .500;
char		*cdInfoDir = NULL;

extern char	*getenv();

static Widget	top_shell;
Widget	top_form_widget;

static XrmOptionDescRec options[] = {
{ "-file",	".file",	XrmoptionSepArg,	(caddr_t) NULL },
{ "-device",	".device",	XrmoptionSepArg,	(caddr_t) NULL },
{ "-debug",	".debug",	XrmoptionNoArg,		(caddr_t) "True" },
{ "-displayTimer", ".displayTimer", XrmoptionNoArg,	(caddr_t) "True" },
{ "-volBase",	".volBase",	XrmoptionSepArg,	(caddr_t) 0 },
{ "-volPcent",	".volPcent",	XrmoptionSepArg,	(caddr_t) 0 },
{ "-cdInfoDir",	".cdInfoDir",	XrmoptionSepArg,	(caddr_t) NULL },
};

static XtResource	resources[] = {
	{ "file", "File", XtRString, sizeof(String), (Cardinal) &file,
		  XtRString, (caddr_t) NULL },
	{ "device", "Device", XtRString, sizeof(String), (Cardinal) &device,
		  XtRString, (caddr_t) NULL },
	{ "debug", "Debug", XtRBoolean, sizeof(Boolean), (Cardinal) &debug,
		  XtRBoolean, (caddr_t) &debug },
	{ "displayTimer", "DisplayTimer", XtRBoolean, sizeof(Boolean),
		  (Cardinal) &display_timer, XtRBoolean,
		  (caddr_t) &display_timer },
	{ "volBase", "VolBase", XtRFloat, sizeof(float),
		  (Cardinal) &volbase, XtRFloat, (caddr_t) &volbase },
	{ "volPcent", "VolPcent", XtRFloat, sizeof(float),
		  (Cardinal) &volpcent, XtRFloat, (caddr_t) &volpcent },
	{ "replayThreshold", "replayThreshold", XtRInt, sizeof(int),
		  (Cardinal) &replayThreshold, XtRInt, 
		  (caddr_t) &replayThreshold },
	{ "scanSkipInterval", "scanSkipInterval", XtRInt, sizeof(int),
		  (Cardinal) &scanSkipInterval, XtRInt, 
		  (caddr_t) &scanSkipInterval },
	{ "scanPauseInterval", "scanPauseInterval", XtRFloat, sizeof(float),
		  (Cardinal) &scanPauseInterval, XtRFloat, 
		  (caddr_t) &scanPauseInterval },
	{ "pauseSkipInterval", "pauseSkipInterval", XtRInt, sizeof(int),
		  (Cardinal) &pauseSkipInterval, XtRInt, 
		  (caddr_t) &pauseSkipInterval },
	{ "pausePauseInterval", "pausePauseInterval", XtRFloat, sizeof(float),
		  (Cardinal) &pausePauseInterval, XtRFloat, 
		  (caddr_t) &pausePauseInterval },
	{ "cdInfoDir", "cdInfoDir", XtRString, sizeof(String), 
		  (Cardinal) &cdInfoDir, XtRString, (caddr_t) NULL },
};

Widget
top_setup(argc, argv)
	int		argc;
	char		**argv;
{
	Display		*dpy;

	Pixmap		icon_pixmap;
	Arg		arg;
	char		*s;

	XtToolkitInitialize();

	appc = XtCreateApplicationContext();

	dpy = XtOpenDisplay(appc, NULL, "xcdplayer", "XCdplayer",
			    options, XtNumber(options),
			    (Cardinal *) &argc, argv);

	if (dpy == NULL) {
		fprintf(stderr, "can't open display\n");
		exit(1);
	}

	top_shell = XtAppCreateShell("xcdplayer", "XCdplayer",
				     applicationShellWidgetClass,
				     dpy, (ArgList) NULL, 0);

	icon_pixmap = XCreateBitmapFromData(XtDisplay(top_shell),
				RootWindowOfScreen(XtScreen(top_shell)),
				logo_bits, logo_width, logo_height);
	
	XtSetArg(arg, XtNiconPixmap, icon_pixmap);
	XtSetValues(top_shell, &arg, 1);
	

	(void) XtGetApplicationResources(top_shell, (caddr_t) NULL,
					 resources, XtNumber(resources),
					 (ArgList) NULL, 0);

	if ((cdInfoDir == NULL) && ((s=getenv("XCDINFODIR")) != NULL))
	    cdInfoDir = strdup(s);

	top_form_widget = XtCreateWidget("mainForm", formWidgetClass,
						 top_shell,
						 (ArgList) NULL, 0);

	return(top_form_widget);
}

void
top_start() {
	XtManageChild(top_form_widget);
	XtRealizeWidget(top_shell);
}
