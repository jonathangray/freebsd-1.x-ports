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

/*  dialog.c
 *
 *    Create the dialogue window where the user enter dbx commands, and
 *    provide action procs to make a text widget behave like a terminal.
 *
 *    InsertSpace():	Prevent user from deleting past the prompt (action proc
 *			for DELETE or BACKSPACE).
 *    DeleteWord():	Word delete in dialog window. (action proc for Ctrl-w).
 *    DeleteLine():	Line delete in dialog window. (action proc for Ctrl-u).
 *    Dispatch():	Send an input command line to dbx. (action proc for CR).
 *    SigInt():		Send SIGINT to dbx (action proc for Ctrl-C).
 *    SigEof():		Send an EOF signal to dbx (action proc for Ctrl-D).
 *    SigQuit():	Send SIGQUIT to dbx (action proc for Ctrl-\).
 *    CreateDialogWindow(): Create dialog window and install action table.
 *    AppendDialogText():       Append string to dialog window.
 */

#include <signal.h>
#include "global.h"

#define	DIALOGSIZE	100000		/* max size of dialogue window buffer */

Widget	dialogWindow;			/* text window as a dbx terminal */
Boolean FalseSignal = FALSE;		/* set to TRUE before self-generated
					   interrupt/quit signals */
static char DialogText[DIALOGSIZE];	/* text buffer for widget */
static XawTextPosition  StartPos;      	/* starting position of input text */


/*  This procedure prevents the user from deleting past the prompt, or
 *  any text appended by AppendDialogText() to the dialog window.
 *  It checks the last position of text, if it matches StartPos, set
 *  by AppendDialogText(), it inserts a space so that delete-previous-
 *  character() can only delete the space character.
 */
/* ARGSUSED */
static void InsertSpace(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    XawTextBlock    textblock;
    XawTextPosition lastPos;

    if (XawTextGetInsertionPoint(w) <= StartPos) {
    	lastPos = TextGetLastPos(w);
	if (lastPos == StartPos) {
	    textblock.firstPos = 0;
	    textblock.length   = 1;
	    textblock.ptr      = " ";
	    XawTextReplace(w, lastPos, lastPos, &textblock);
	    XawTextSetInsertionPoint(w, lastPos+1);
	}
    }
}

/*  Erases the preceding word.
 *  Simulates the action of the WERASE character (ctrl-W).
 */
/* ARGSUSED */
void DeleteWord(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    XawTextBlock    	textblock;
    XawTextPosition	pos;
    Cardinal	 	i;

    textblock.firstPos = 0;
    textblock.length   = 0;
    textblock.ptr      = "";

    pos = XawTextGetInsertionPoint(w); 
    if (pos <= StartPos)
        pos = TextGetLastPos(w); 
    for (i=pos; i > StartPos && DialogText[i-1] == ' '; i--);
    for (; i > StartPos && DialogText[i-1] != ' '; i--);
    XawTextReplace(w, i, pos, &textblock);
    XawTextSetInsertionPoint(w, i);
}


/*  Deletes the entire current input line.
 *  simulates the action of the KILL character (ctrl-U).
 */
/* ARGSUSED */
void DeleteLine(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    XawTextBlock    	textblock;
    XawTextPosition 	pos, beginPos;
    Cardinal	 	i;
    char		*s;

    textblock.firstPos = 0;
    textblock.length   = 0;
    textblock.ptr      = "";

    pos = XawTextGetInsertionPoint(w); 
    if (w == dialogWindow) {
	s = DialogText;
	beginPos = StartPos;
	if (pos <= beginPos)
    	    pos = TextGetLastPos(w);
    }
    for (i=pos; i > beginPos && s[i-1] != '\n'; i--);
    XawTextReplace(w, i, pos, &textblock);
    XawTextSetInsertionPoint(w, i);
}


/*  Dispatch() is invoked on every <CR>.
 *  It collects text from the dialog window and sends it to dbx.
 *  If the string is a command to dbx (Prompt would be TRUE),
 *  it is stored in the global variable, Command.
 */
/* ARGSUSED */
static void Dispatch(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params; 
{
#ifdef GDB
	/* 
	For GDB, '\n' means exec previous command again.
	default command is space+CR, so that we never send
	CR to gdb (the repeat is managed here)
	*/
    static char gdb_command[LINESIZ] = " \n";
#endif
char s[LINESIZ];

    strcpy(s, DialogText + StartPos);
#if 1
	/* (PW)18DEC90 : bug xdbx : without the following line,
	xdbx sends several times the same lines when Prompt is false */
    StartPos = TextGetLastPos(dialogWindow);
#endif

    if (Prompt)
    	{
#ifdef GDB
		/* When we send \n to gdb, it executes the last command,
		so better tell xxgdb what gdb is doing */
	    if (strcmp(s, "\n"))
			strcpy(gdb_command,s);	/* if not "\n" ! */
	    else
	    	{
	    	/* copy previous command in new command, and
	    	echo the command in the dialog window. */
			strcpy(s,gdb_command);
			AppendDialogText(gdb_command);
			}
#endif /* GDB */
		send_command (s);
		}
	else
		/* this string is for the application, not for gdb */
    	write_dbx(s);
}


/*  Sends an interrupt signal, SIGINT, to dbx.
 *  Simulates the action of the INTR character (ctrl-C).
 */
/* ARGSUSED */
static void SigInt(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
#ifndef GDB
    FalseSignal = TRUE;
   killpg(dbxpid, SIGINT);
#else      
	write_dbx("\003");	/* (PW)18FEB91 : seems to work better */
#endif	/* GDB */
}


/*  Sends an EOF signal to dbx. (ctrl-D) */
/* ARGSUSED */
static void SigEof(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    write_dbx("\04");
}


/*  Sends a QUIT signal, SIGQUIT, to dbx. 
 *  Simulates the action of the QUIT character (ctrl-\) 
 */
/* ARGSUSED */
static void SigQuit(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    FalseSignal = TRUE;
#ifdef SVR4		/* (MJH) */		/* (PW) is this true for all SYSV ? */
    kill(-(dbxpid), SIGQUIT);
#else
    killpg(dbxpid, SIGQUIT);
#endif /* SVR4 */
}


/* 
 *  Dialog window has its own set of translations for editing.
 *  Special action procedures for keys Delete/Backspace, Carriage Return,
 *  Ctrl-U, Ctrl-C, Ctrl-D, Ctrl-\, and word selection.
 */
void CreateDialogWindow(parent)
Widget parent;
{
    Arg 	args[MAXARGS];
    Cardinal 	n;

    static XtActionsRec dialog_actions[] = {
	{"SigInt", 	(XtActionProc) SigInt},
	{"SigEof", 	(XtActionProc) SigEof},
	{"SigQuit", 	(XtActionProc) SigQuit},
	{"InsertSpace", (XtActionProc) InsertSpace},
	{"Dispatch", 	(XtActionProc) Dispatch},
        {NULL, NULL}
    };

    static String translations = "#override\n\
 	Ctrl<Key>C:	SigInt()\n\
 	Ctrl<Key>D:	SigEof()\n\
 	Ctrl<Key>|:	SigQuit()\n\
 	Ctrl<Key>W:	DeleteWord()\n\
 	Ctrl<Key>U:	DeleteLine()\n\
 	Ctrl<Key>H:	InsertSpace() delete-previous-character()\n\
 	<Key>Delete:	InsertSpace() delete-previous-character()\n\
 	<Key>BackSpace:	InsertSpace() delete-previous-character()\n\
 	<Key>Return:	newline() Dispatch()\n\
    ";

    n = 0;
    XtSetArg(args[n], XtNuseStringInPlace, True);                       n++;
    XtSetArg(args[n], XtNstring, (XtArgVal) DialogText);		n++;
    XtSetArg(args[n], XtNlength, (XtArgVal) DIALOGSIZE);		n++;
    XtSetArg(args[n], XtNeditType, (XtArgVal) XawtextAppend);		n++;
    XtSetArg(args[n], XtNscrollVertical, XawtextScrollAlways);		n++;
    XtSetArg(args[n], XtNwrap, XawtextWrapWord);			n++;
    dialogWindow = XtCreateManagedWidget("dialogWindow", asciiTextWidgetClass,
					 parent, args, n );
    XtOverrideTranslations(dialogWindow, XtParseTranslationTable(translations));
    XtAppAddActions(app_context, dialog_actions, XtNumber(dialog_actions));
}

static void TextSetLastPos(w, lastPos)
Widget w;
XawTextPosition lastPos;
{
    TextWidget ctx = (TextWidget) w;
    ctx->text.lastPos = lastPos;
}
 
void AppendDialogText(s)
    char   *s;
{
    XawTextPosition     i, lastPos;
    XawTextBlock        textblock, nullblock;
    Arg 		args[MAXARGS];
    Cardinal 		n;

    if (!s || !strcmp(s, "")) return;

    textblock.firstPos = 0;
    textblock.length   = strlen(s);
    textblock.ptr      = s;

    lastPos = TextGetLastPos(dialogWindow);
    if (textblock.length > DIALOGSIZE) {
	bell(0);
#ifdef GDB
	fprintf(stderr, "xxgdb error: cannot display string in dialogue window\n\
            string has %d bytes; dialogue window size limit is %d bytes\n",
	    textblock.length, DIALOGSIZE);
#else
	fprintf(stderr, "xdbx error: cannot display string in dialogue window\n\
            string has %d bytes; dialogue window size limit is %d bytes\n",
	    textblock.length, DIALOGSIZE);
#endif
        return;
    }
    if (lastPos + textblock.length > DIALOGSIZE) {
	nullblock.firstPos = 0;
	nullblock.length = 0;
	nullblock.ptr = "";

	i = textblock.length - (DIALOGSIZE - lastPos);
	if (i < 0.9*DIALOGSIZE)
	    i += 0.1*DIALOGSIZE;
        while (DialogText[i] != '\n') i++;
        XawTextReplace(dialogWindow, 0, i+1, &nullblock);
    	lastPos = TextGetLastPos(dialogWindow);
    }
    XawTextReplace(dialogWindow, lastPos, lastPos, &textblock);
    StartPos = TextGetLastPos(dialogWindow);
    XawTextSetInsertionPoint(dialogWindow, StartPos);
}
