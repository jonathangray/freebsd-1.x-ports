/*****************************************************************************
 *
 *  xdbx - X Window System interface to dbx
 *
 *  Copyright 1989, 1990 The University of Texas at Austin
 *
 *  Permission to use, copy, modify, and distribute this software and its
 *  documentation for any purpose and without fee is hereby granted,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name of The University of Texas
 *  not be used in advertising or publicity pertaining to distribution of
 *  the software without specific, written prior permission.  The
 *  University of Texas makes no representations about the suitability of
 *  this software for any purpose.  It is provided "as is" without express
 *  or implied warranty.
 *
 *  THE UNIVERSITY OF TEXAS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
 *  SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 *  FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TEXAS BE LIABLE FOR ANY
 *  SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 *  RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
 *  CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 *  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *  Author:  	Po Cheung, The University of Texas at Austin
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

/*  windows.c:
 *
 *    CreateTitleBar() :	Create title bar.
 *    CreateFileLabel() :	Create file label in file window.
 *    CreateLineLabel() :	Create line label in file window.
 *    CreateFileWindow() :	Create file window.
 *    CreateMessageWindow() :	Create message window.
 *    CreateDisplayWindow() :	Create display window.
 *    CreateSubWindows() :	Create the subwindows.
 *    UpdateFileLabel() :	Update file label.
 *    UpdateLineLabel() :	Update line label.
 *    UpdateMessageWindow() :	Update message window.
 */

#include "global.h"

Widget	fileWindow,			/* parent of fileLabel and lineLabel */
	messageWindow,			/* window for displaying messages */
	separator,			/* separator in vpane */
	displayWindow;			/* area for displaying variables */

static Widget 	fileLabel,		/* filename of displayed text */
		lineLabel;		/* line number of caret position */

/*
 *  Private routines for creating various subwindows for xdbx.
 */

static void CreateFileLabel(parent)
Widget parent;
{
    Arg 	args[MAXARGS];
    Cardinal 	n;

    n = 0;
    XtSetArg(args[n], XtNlabel, (XtArgVal) "No Source File");           n++;
    XtSetArg(args[n], XtNborderWidth, (XtArgVal) 0);           		n++;
    fileLabel = XtCreateManagedWidget("fileLabel", labelWidgetClass, 
				      parent, args, n);
}

static void CreateLineLabel(parent)
Widget parent;
{
    Arg 	args[MAXARGS];
    Cardinal 	n;

    n = 0;
    XtSetArg(args[n], XtNlabel, (XtArgVal) "");           		n++;
    XtSetArg(args[n], XtNborderWidth, (XtArgVal) 0);           		n++;
    XtSetArg(args[n], XtNfromHoriz, (XtArgVal) fileLabel);          	n++;
    XtSetArg(args[n], XtNhorizDistance, (XtArgVal) 0);          	n++;
    lineLabel = XtCreateManagedWidget("lineLabel", labelWidgetClass, 
				      parent, args, n);
}

static void CreateFileWindow(parent)
Widget parent;
{
    Arg 	args[MAXARGS];
    Cardinal 	n;

    n = 0;
    XtSetArg(args[n], XtNshowGrip, (XtArgVal) False);			n++;
    fileWindow = XtCreateManagedWidget("fileWindow", formWidgetClass, 
				       parent, args, n);
    CreateFileLabel(fileWindow);
    CreateLineLabel(fileWindow);
}

static void CreateMessageWindow(parent)
Widget parent;
{
    Arg 	args[MAXARGS];
    Cardinal 	n;

    n = 0;
    XtSetArg(args[n], XtNlabel, (XtArgVal) ""); 			n++;
    XtSetArg(args[n], XtNjustify, (XtArgVal) XtJustifyLeft);          	n++;
    XtSetArg(args[n], XtNshowGrip, (XtArgVal) False);			n++;
    messageWindow = XtCreateManagedWidget("messageWindow", labelWidgetClass,
					  parent, args, n);
}

/*  Create a window for displaying variables as specified by the display
 *  command in dbx.
 */
static void CreateDisplayWindow(parent)
Widget parent;
{
    Arg 	args[MAXARGS];
    Cardinal 	n;

    n = 0;
    XtSetArg(args[n], XtNborderWidth, (XtArgVal) 0);                    n++;
    XtSetArg(args[n], XtNmin, (XtArgVal) 2); 				n++;
    XtSetArg(args[n], XtNmax, (XtArgVal) 2); 				n++;
    XtSetArg(args[n], XtNshowGrip, (XtArgVal) False);			n++;
    separator = XtCreateWidget("", labelWidgetClass, parent, args, n);

    n = 0;
    XtSetArg(args[n], XtNeditType, (XtArgVal) XawtextRead);		n++;
    displayWindow = XtCreateWidget("displayWindow", asciiTextWidgetClass, 
				   parent, args, n);

    if (app_resources.displayWindow) {
	XtManageChild(separator);
	XtManageChild(displayWindow);
    }
}


/*  PUBLIC ROUTINES */
/*
 *  Top level function for creating all the xdbx subwindows.
 */
void CreateSubWindows(parent)
Widget parent;
{
    Widget	vpane;		/* outer widget containing various subwindows */
    Arg 	args[MAXARGS];
    Cardinal 	n;

    n = 0;
    vpane = XtCreateManagedWidget("vpane", panedWidgetClass, parent, args, n);

    CreateFileWindow(vpane);
    CreateSourceWindow(vpane);
    CreateMessageWindow(vpane);
    CreateCommandPanel(vpane);
    CreateDialogWindow(vpane);
#ifdef GDB
    CreateDisplayWindow(vpane);
#else
#ifndef BSD
    CreateDisplayWindow(vpane);
#endif
#endif	/* not GDB */
} 

/*
 *  Routines for updating fields for the filename and line number
 *  in the file window, and the execution status in the message window.
 */

void UpdateFileLabel(string)
char *string;
{
    Arg 	args[MAXARGS];
    Cardinal 	n;

    n = 0;
    XtSetArg(args[n], XtNlabel, (XtArgVal) string);        		n++;
    XtSetValues(fileLabel, args, n);
}

void UpdateLineLabel(line)
Cardinal line;
{
    Arg 	args[MAXARGS];
    Cardinal 	n;
    char 	string[10];

    n = 0;
    if (line > 0)
    	sprintf(string, "%d", line);
    else
	strcpy(string, "");
    XtSetArg(args[n], XtNlabel, (XtArgVal) string);        	n++;
    XtSetValues(lineLabel, args, n);
}

/*--------------------------------------------------------------------------+
|																			|
|	Note : UpdateMessageWindow assumes that the format string				|
|			can only contain one %s specifier.								|
|		arg is either NULL or is a string.									|
|		format is a string (never NULL).									|
|																			|
+--------------------------------------------------------------------------*/
void UpdateMessageWindow(format, arg)
char *format, *arg;
{
	char *message;
	char string[LINESIZ];
	int fulllength;
    Arg 	args[MAXARGS];
    Cardinal 	n;

	fulllength = strlen ("  ") + strlen (format) + 1 + ((arg == NULL) ? 0 : strlen (arg));
	
	if (fulllength > LINESIZ)
		message = (char*) XtMalloc (fulllength);
	else
		message = string;
		
    strcpy(message, "  ");
    sprintf(message + strlen(message), format, arg);
    
    n = 0;
    XtSetArg(args[n], XtNlabel, (XtArgVal) message);		n++;
    XtSetValues(messageWindow, args, n);
    
	if (fulllength > LINESIZ)
		XtFree(message);
}

void ClearMessageWindow()
{
    Arg 	args[MAXARGS];
    Cardinal 	n;

    n = 0;
    XtSetArg(args[n], XtNlabel, (XtArgVal) "");			n++;
    XtSetValues(messageWindow, args, n);
}

