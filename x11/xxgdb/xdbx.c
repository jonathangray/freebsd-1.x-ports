/*****************************************************************************
 *
 *  xdbx - X Window System interface to the dbx debugger
 *
 *  Copyright 1989 The University of Texas at Austin
 *  Copyright 1990 Microelectronics and Computer Technology Corporation
 *
 *  Permission to use, copy, modify, and distribute this software and its
 *  documentation for any purpose and without fee is hereby granted,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name of The University of Texas
 *  and Microelectronics and Computer Technology Corporation (MCC) not be 
 *  used in advertising or publicity pertaining to distribution of
 *  the software without specific, written prior permission.  The
 *  University of Texas and MCC makes no representations about the 
 *  suitability of this software for any purpose.  It is provided "as is" 
 *  without express or implied warranty.
 *
 *  THE UNIVERSITY OF TEXAS AND MCC DISCLAIMS ALL WARRANTIES WITH REGARD TO
 *  THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 *  FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TEXAS OR MCC BE LIABLE FOR
 *  ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 *  RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
 *  CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 *  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *  Author:  	Po Cheung
 *  Created:   	March 10, 1989
 * 
 *****************************************************************************
 * 
 *  xxgdb - X Window System interface to the gdb debugger
 *  
 * 	Copyright 1990 Thomson Consumer Electronics, Inc.
 *  
 *  Permission to use, copy, modify, and distribute this software and its
 *  documentation for any purpose and without fee is hereby granted,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name of Thomson Consumer
 *  Electronics (TCE) not be used in advertising or publicity pertaining
 *  to distribution of the software without specific, written prior
 *  permission.  TCE makes no representations about the suitability of
 *  this software for any purpose.  It is provided "as is" without express
 *  or implied warranty.
 *
 *  TCE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 *  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT
 *  SHALL TCE BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES
 *  OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 *  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 *  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 *  SOFTWARE.
 *
 *  Adaptation to GDB:  Pierre Willard
 *  XXGDB Created:   	December, 1990
 *
 *****************************************************************************/

/*  xdbx.c
 *
 *    Contain main program and initialization, command line options handling,
 *    and resource database management.
 *
 *    Syntax():		Print an error message if xdbx is invoked with an
 *			incorrect number of arguments.
 *    main_init():	Initialization routine.
 *    dbxoptions():	Construct command line arguments for dbx.
 *    main():		Main program.
 */

#ifdef GDB
#define XGDBVERSION	"1.06"
#endif

#ifdef SYSV 
#   include <stdio.h>
#   include <sys/param.h>
#endif

#include "global.h"
#include "bitmaps.h"
#include "patchlevel.h"

#define VERSION	"2.1"
#define Offset(field) (XtOffset(XdbxResources *, field))

char *progname;

char            cwd[MAXPATHLEN];        /* The current working directory */
XtAppContext  	app_context; 		/* application context */
Widget  	toplevel; 		/* top level widget */
Display		*display;		/* connection to X server */
Cursor		watch;			/* XC_watch cursor */
XdbxResources 	app_resources;		/* application resources of xdbx */
char 		xdbxinit[LINESIZ];	/* initialization file name */
Boolean		Tstartup = False;	/* if True, remove xdbxinit */
Boolean		debug = False;		/* debug mode for xdbx */


static XtResource resources[] = {
    {"bell", "Bell", XtRBoolean, sizeof(Boolean), 
	Offset(bell), XtRImmediate, (caddr_t)False},
    {"displayWindow", "DisplayWindow", XtRBoolean, sizeof(Boolean), 
	Offset(displayWindow), XtRImmediate, (caddr_t)False},
    {"prompt", "Prompt", XtRString, sizeof(char *), 
	Offset(prompt), XtRImmediate, (caddr_t)NULL},
	
/* CRL mod 4 3/15/91 GWC - added two new application resources */
    {"db_name", "Prompt", XtRString, sizeof(char *), 
	Offset(db_name), XtRImmediate, (caddr_t)NULL},
    {"db_prompt", "Prompt", XtRString, sizeof(char *), 
	Offset(db_prompt), XtRImmediate, (caddr_t)NULL},

    {"nx", "nx", XtRBoolean, sizeof(Boolean), 
	Offset(nx), XtRImmediate, (caddr_t)False},

    {"delimiters", "Delimiters", XtRString, sizeof(char *), 
	Offset(delimiters), XtRImmediate, (caddr_t)NULL},
    {"stop_color", "StopColor", XtRPixel, sizeof(Pixel), 
	Offset(stop_color), XtRString, "Red"},
    {"arrow_color", "ArrowColor", XtRPixel, sizeof(Pixel), 
	Offset(arrow_color), XtRString, "Blue"},
    {"updown_color", "UpdownColor", XtRPixel, sizeof(Pixel), 
	Offset(updown_color), XtRString, "Blue"},
    {"bomb_color", "bombColor", XtRPixel, sizeof(Pixel), 
	Offset(bomb_color), XtRString, "Red"},
    {"dataDpyMaxHeight", "DataDpyMaxHeight", XtRDimension, sizeof(Dimension), 
	Offset(dataDpyMaxHeight), XtRString, "300"},
    {"dataDpyMaxWidth", "DataDpyMaxWidth", XtRDimension, sizeof(Dimension), 
	Offset(dataDpyMaxWidth), XtRString, "600"},
    {"bigicon", "Xdbxoptions", XtRBoolean, sizeof(Boolean), 
	Offset(bigicon), XtRImmediate, (caddr_t)False},
    {"debug", "Xdbxoptions", XtRBoolean, sizeof(Boolean), 
	Offset(debug), XtRImmediate, (caddr_t)False},
    {"dbxopt_r", "Dbxoptions", XtRBoolean, sizeof(Boolean), 
	Offset(dbxopt_r), XtRImmediate, (caddr_t)False},
    {"dbxopt_i", "Dbxoptions", XtRBoolean, sizeof(Boolean), 
	Offset(dbxopt_i), XtRImmediate, (caddr_t)False},
    {"includeDir", "Dbxoptions", XtRString, sizeof(char *), 
	Offset(includeDir), XtRImmediate, (caddr_t)NULL},
    {"dbxopt_k", "Dbxoptions", XtRBoolean, sizeof(Boolean), 
	Offset(dbxopt_k), XtRImmediate, (caddr_t)False},
    {"cfile", "Dbxoptions", XtRString, sizeof(char *), 
	Offset(cfile), XtRImmediate, (caddr_t)NULL},
    {"dbxopt_kbd", "Dbxoptions", XtRBoolean, sizeof(Boolean), 
	Offset(dbxopt_kbd), XtRImmediate, (caddr_t)False},
    {"fcount", "Dbxoptions", XtRString, sizeof(char *), 
	Offset(fcount), XtRImmediate, (caddr_t)NULL},
    {"startup", "Dbxoptions", XtRString, sizeof(char *), 
	Offset(startup), XtRImmediate, (caddr_t)NULL},
    {"tstartup", "Dbxoptions", XtRString, sizeof(char *), 
	Offset(tstartup), XtRImmediate, (caddr_t)NULL},
};


String fallback_resources[] = {
    "*allowShellResize:                 True",
    "*borderWidth:			1",
    "*font:                             fixed",
    "*vpane.width:                      550",
    "*fileWindow*font:     		variable",
    "*fileLabel.width:     		500",
    "*lineLabel.width:     		50",
    "*sourceForm.preferredPaneSize:     320",
    "*sourceWindow.leftMargin:          35",
    "*sourceWindow.scrollHorizontal:	whenNeeded",
    "*sourceWindow.translations:	#override \\n\
        <Btn1Down>:             SelectStart() SelectWord() \\n\
        Shift<Btn1Up>:          Update() SelectEnd() PrintSelection() \\n\
        <Btn1Up>:               Update() SelectEnd() \\n",
    "*messageWindow*font:  		variable",
    "*messageWindow.min:  		30",
    "*messageWindow.max:  		30",
    "*dialogWindow.preferredPaneSize:	200",
    "*dialogWindow.resizeToPreferred:	True",
    "*dialogWindow.translations:	#override \\n\
        <Btn1Down>:     SelectStart() SelectWord() \\n\
        Shift<Btn1Up>:  SelectEnd() PrintSelection() \\n\
        <Btn1Up>:       SelectEnd() \\n",
    "*commandWindow.preferredPaneSize:  106",
    "*commandWindow.skipAdjust:		True",
    "*commandWindow.hSpace:		14",
    "*commandWindow.vSpace:		10",
    "*Command.height:                   20",
    "*Command.width:                    60",
    "*List.columnSpacing:               10",
    "*displayWindow.preferredPaneSize:  50",
    "*displayWindow.skipAdjust:         True",
    "*displayWindow.scrollVertical:	whenNeeded",
    "*displayWindow.scrollHorizontal:	whenNeeded",
    "*displayWindow.translations:	#override \\n\
        <Btn1Down>:             SelectStart() SelectWord() \\n\
        Shift<Btn1Up>:          SelectEnd() PrintSelection() \\n\
        <Btn1Up>:               SelectEnd() \\n",
    "*popup*showGrip:  			False",
    NULL,
};

static XrmOptionDescRec options[] = {
    {"-bigicon","bigicon",	XrmoptionNoArg, "True"},
    {"-debug",	"debug",	XrmoptionNoArg, "True"},

/* CRL mod 4 4/1/91 GWC - command line options for db_name and db_prompt */
    {"-db_name","db_name",      XrmoptionSepArg, NULL},
    {"-db_prompt","db_prompt",  XrmoptionSepArg, NULL},

#ifdef GDB
    {"-d",	"includeDir",	XrmoptionSepArg, NULL},
    {"-nx",	"nx",			XrmoptionNoArg, "True"},
#else
    {"-r",	"dbxopt_r",	XrmoptionNoArg, "True"},
    {"-i",	"dbxopt_i",	XrmoptionNoArg, "True"},
    {"-I",	"includeDir",	XrmoptionSepArg, NULL},
    {"-k",	"dbxopt_k",	XrmoptionNoArg, "True"},
#ifdef BSD   /* Berkeley dbx */
    {"-c",	"cfile",	XrmoptionSepArg, NULL},
#else	     /* Sun dbx */
    {"-kbd",	"dbxopt_kbd",	XrmoptionNoArg, "True"},
    {"-f",	"fcount",	XrmoptionSepArg, NULL},
    {"-s",	"startup",	XrmoptionSepArg, NULL},
    {"-sr",	"tstartup",	XrmoptionSepArg, NULL},
#endif
#ifdef MIPS  /* Mips dbx */
    {"-pixie",  "pixie",	XrmoptionNoArg, "True"},
#endif
#endif	/* not GDB */
};

XtActionsRec xdbx_actions[] = {
    {"SelectStart",	(XtActionProc) SelectStart},
    {"SelectEnd",	(XtActionProc) SelectEnd},
    {"SelectWord",	(XtActionProc) SelectWord},
    {"PrintSelection",	(XtActionProc) PrintSelection},
    {"Update",		(XtActionProc) Update},
    {"DeleteWord",	(XtActionProc) DeleteWord},
    {"DeleteLine",	(XtActionProc) DeleteLine},
    {NULL, NULL}
};

static void Syntax(call)
char *call;
{
    fprintf(stderr,
#ifdef GDB
	    "Usage: %s [-toolkitoptions] [-gdboptions] [objfile [corefile]]\n",
#else
	    "Usage: %s [-toolkitoptions] [-dbxoptions] [objfile [corefile]]\n",
#endif	/* not GDB */
	    call);
    exit(1);
}

/*  Set window manager hints to indicate display accepts input.
 *  Initialize routines in source.c, signs.c and parser.c.
 *  Disable window resize of fileWindow.
 *  Get the name of the dbx command initialization file.
 */
static void main_init()
{
    XWMHints	wmhints;
    char	title[100];

    display = XtDisplay(toplevel);
    watch = XCreateFontCursor(display, XC_watch);

#ifdef GDB
    sprintf(title, "xxgdb %s", XGDBVERSION);
    XStoreName(display, XtWindow(toplevel), title);
    XSetIconName(display, XtWindow(toplevel), "xxgdb");
#else
    sprintf(title, "xdbx %s (patch level %d)", VERSION, PATCHLEVEL);
    XStoreName(display, XtWindow(toplevel), title);
    XSetIconName(display, XtWindow(toplevel), "xdbx");
#endif	/* not GDB */
    wmhints.input = True;
    if (app_resources.bigicon)
	wmhints.icon_pixmap = XCreateBitmapFromData(display, XtWindow(toplevel),
	    xdbx64_bits, xdbx64_width, xdbx64_height);
    else
	wmhints.icon_pixmap = XCreateBitmapFromData(display, XtWindow(toplevel),
	    xdbx48_bits, xdbx48_width, xdbx48_height);
    wmhints.flags = IconPixmapHint | InputHint;
    XSetWMHints(display, XtWindow(toplevel), &wmhints);

    if (!app_resources.delimiters || 
	strcmp(app_resources.delimiters, "") == NULL)
	app_resources.delimiters = XtNewString(DELIMITERS);
    if (app_resources.prompt && strcmp(app_resources.prompt, "") != NULL)
	xdbxprompt = app_resources.prompt;
    else
	xdbxprompt = XtNewString(XDBXPROMPT);
    debug = app_resources.debug;
    DisableWindowResize(fileWindow);

#ifdef GDB
	if (app_resources.nx)
		strcpy(xdbxinit, "");
	else
		{
	    strcpy(xdbxinit, ".gdbinit");
	    if (access(xdbxinit, R_OK) == -1)
	    	{
	    	sprintf(xdbxinit, "%s/%s", (char *) getenv("HOME"), ".gdbinit");
    		if (access(xdbxinit, R_OK) == -1)
	    		strcpy(xdbxinit, "");
	    	}
    	}
#else
    strcpy(xdbxinit, ".dbxinit");
    if (access(xdbxinit, R_OK) == -1) {
    	sprintf(xdbxinit, "%s/%s", (char *) getenv("HOME"), ".dbxinit");
    	if (access(xdbxinit, R_OK) == -1)
	    	strcpy(xdbxinit, "");
    }
#endif	/* not GDB */
    source_init();
    signs_init();
    parser_init();
}


/*  Reconstruct command line arguments for calling dbx.
 *  Return the argument list for dbx and new value of argc.
 */
static char **dbxoptions(argc, argv, app_resources)
    int  *argc;
    char **argv;
    XdbxResources *app_resources;
{
    char **dbxargv;
#ifndef GDB
    char *temp = "xdbx.XXXXXX";
#endif
    int  i=0;

    dbxargv = (char **) XtMalloc (MAXARGS * sizeof(char *));
    for (i=0; i < *argc; i++)
	dbxargv[i] = argv[i];

#ifdef GDB
	dbxargv[i++] = "-fullname";	/* see gdb_regex.h */
#endif	/* GDB */

    if (app_resources->dbxopt_r)
	dbxargv[i++] = "-r";
    if (app_resources->dbxopt_i)
	dbxargv[i++] = "-i";
    if (app_resources->includeDir) {
#ifdef GDB
	dbxargv[i++] = "-d";
#else
	dbxargv[i++] = "-I";
#endif	/* not GDB */
	dbxargv[i++] = app_resources->includeDir;
    }
    if (app_resources->dbxopt_k)
	dbxargv[i++] = "-k";
    if (app_resources->cfile) {
	dbxargv[i++] = "-c";
	dbxargv[i++] = app_resources->cfile;
    }
    if (app_resources->dbxopt_kbd)
	dbxargv[i++] = "-kbd";
    if (app_resources->fcount) {
	dbxargv[i++] = "-f";
	dbxargv[i++] = app_resources->fcount;
    }
    /*  If .dbxinit exists in the local or home directory, include the option
     *  -c (Berkeley dbx) or -s (Sun dbx) and a dummy filename as the option 
     *	argument.  This will prevent dbx from reading the user's command
     *	initialization file.  Xdbx will read each line and pass it to dbx
     *  instead.
     */
     
#ifdef GDB
     /* for GDB, always use option -nx */
	dbxargv[i++] = "-nx";
#else
    if (strcmp(xdbxinit, "")) {		/* .dbxinit or ~/.dbxinit exists */
#ifdef BSD
	dbxargv[i++] = "-c";
#else
	dbxargv[i++] = "-s";
#endif
	dbxargv[i++] = (char *) mktemp(temp);
    }
#endif	/* not GDB */
    if (app_resources->startup) {	/* overwrites dbxinit */
	Tstartup = False;
	strcpy(xdbxinit, app_resources->startup);
    }
    if (app_resources->tstartup) {	/* overwrites dbxinit */
	Tstartup = True;
	strcpy(xdbxinit, app_resources->tstartup);
    }
#ifndef GDB
#ifdef MIPS
    if (app_resources->pixie) {		/* pixie output */
	dbxargv[i++] = "-pixie";
    }
#endif
#endif	/* not GDB */
    dbxargv[i] = NULL;
    *argc = i;
    return dbxargv;
}

void main(argc, argv)
int argc;
char **argv;
{
    char 	**dbxargv;
    char *p;
    
    progname = argv[0];			/* (MJH) */
        
#ifdef SYSV 
    getcwd((char *)cwd, MAXPATHLEN);
#endif
    trap_signals();

    toplevel = XtAppInitialize(&app_context, "XDbx", options, XtNumber(options),
			       &argc, argv, fallback_resources, NULL, 0);
    if (argc > 3) Syntax(argv[0]);
    
    XtGetApplicationResources(toplevel, &app_resources, resources,
                              XtNumber(resources), NULL, 0);
    XtAppAddActions(app_context, xdbx_actions, XtNumber(xdbx_actions));
    CreateSubWindows(toplevel);
    XtRealizeWidget(toplevel);

    main_init();
    
#ifdef GDB
    AppendDialogText("XXGDB comes with ABSOLUTELY NO WARRANTY.\n");
#endif
   
    dbxargv = dbxoptions(&argc, argv, &app_resources);
    calldbx(argc, dbxargv);

    XtAppMainLoop(app_context);
}
