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

/*  command.c
 *
 *    Create the command window, the command buttons and their callbacks.
 *
 *    CreateCommandPanel() : 	Create a window with command buttons
 *    CreateButtons() :		Create command buttons in panel
 *    AddButton() :		Add a command button into the command window
 *    ButtonSet() :		Action proc for command button translation
 *
 *    Command callbacks for the command buttons:
 *
 *    forwardSearch() :		forward string search
 *    reverseSearch() :		reverse string search
 *    Search() :		call either forwardSearch() or reverseSearch()
 *    PopupSearch() :		command callback for search button
 *    DoneSearch() :		command callback for DONE button in search panel
 *    CreateSearchPopup() :	create search panel
 *
 *    Command queue manipulation routines:
 *	send_command():		send a command to dbx and record in the queue
 *	get_command():		read command off head of queue
 *	insert_command():	insert command at the head of queue
 *	delete_command():	delete command from head of queue
 */
 
#include <signal.h>
#include <ctype.h>
#include <sys/wait.h>
#include "global.h"

#define	 REVERSE	0
#define	 FORWARD	1

Widget		commandWindow;			/* command panel with buttons */
Boolean		PopupMode = False;
static int	Button;
static Widget	searchPopupShell, searchPopup;
static Widget	AddButton();
static Widget	button[30];
static char	SearchString[BUFSIZ] = "";	/* search string buffer */
static char	command[LINESIZ];
static CommandRec *commandQueue = NULL;
#ifndef GDB
#ifdef BSD
static char	savedCommand[LINESIZ] = ""; 
#endif
#endif	/* not GDB */

/* ARGSUSED */
static void ButtonSet(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    Button = atoi(params[0]);
}

/* ARGSUSED */
/*  Execute the dbx command specifed in client_data
 */
static void DoIt (w, command, call_data)
    Widget w;
    XtPointer command;
    XtPointer call_data;
{
    /* run, cont, next, step, where, up, down, status */
    send_command(command);
    AppendDialogText(command);
}

#ifndef GDB	/* >>>>>>>>>> NOT USED FOR GDB <<<<<<<<<<<<<<< */
/* ARGSUSED */
static void Return (w, client_data, call_data)
    Widget w;
    XtPointer client_data;
    XtPointer call_data;
{
    char *funcname;
    int  nbytes;

    funcname = XFetchBytes(display, &nbytes);	/* from CUT_BUFFER0 */
    if (nbytes == 0)
    	strcpy(command, "return\n");
    else
    	sprintf(command, "return %s\n", funcname);
    send_command(command);
    AppendDialogText(command);
}
#endif /* NOT GDB */

#ifdef GDB	/* >>>>>>>>>>>>>> GDB ONLY <<<<<<<<<<<<<<<< */
/*
	here client_data is "break" or "tbreak"

*/
static void Break(w, client_data, call_data)
    Widget w;
    XtPointer client_data;
    XtPointer call_data;
{
    XawTextPosition pos;
    int line;
    char *funcname;
    int  nbytes;
    char *s;

    funcname = XFetchBytes(display, &nbytes);	/* from CUT_BUFFER0 */
	if (nbytes) 
		{
		s = funcname;
		while (*s == ' ') s++;	/* skip leading spaces (if any) */
		if ((*s >= '0') && (*s <= '9'))
			sprintf(command, "%s *%s\n",client_data,funcname);
		else
			sprintf(command, "%s %s\n",client_data,funcname);
		}
	else
		{
		if (displayedFile != NULL)
			{
			pos = XawTextGetInsertionPoint(sourceWindow);
			line = TextPositionToLine(pos);
			sprintf(command, "%s %d\n",client_data,line);
			}
		else
			{
			UpdateMessageWindow(BREAK_HELP, NULL);
			bell(0);
			return;
			}
		}
		
    send_command(command);
    AppendDialogText(command);
}

#else	/* >>>>>>>>>> NOT USED FOR GDB <<<<<<<<<<<<<<< */

/* ARGSUSED */
static void Stop_at(w, client_data, call_data)
    Widget w;
    XtPointer client_data;
    XtPointer call_data;
{
    XawTextPosition pos;
    int line;

    if (displayedFile == NULL) {
	UpdateMessageWindow(STOP_AT_HELP, NULL);
	bell(0);
	return;
    }
    pos = XawTextGetInsertionPoint(sourceWindow);
    line = TextPositionToLine(pos);
    sprintf(command, "stop at %d\n", line);
    send_command(command);
    AppendDialogText(command);
}

/* ARGSUSED */
static void Stop_in(w, client_data, call_data)
    Widget w;
    XtPointer client_data;
    XtPointer call_data;
{
    char *funcname;
    int  nbytes;

    funcname = XFetchBytes(display, &nbytes);	/* from CUT_BUFFER0 */
    if (nbytes == 0) {
	UpdateMessageWindow(STOP_IN_HELP, NULL);
	bell(0);
	return;
    }
    sprintf(command, "stop in %s\n", funcname);
    send_command(command);
    AppendDialogText(command);
}

#endif /* NOT GDB */

/*  Delete removes the stop_no associated with a given line number.
 *  RemoveStop() is called to undisplay the stop sign only when there
 *  are no more stop_no's associated with that line number.
 */
/* ARGSUSED */
static void Delete(w, client_data, call_data)
    Widget w;
    XtPointer client_data;
    XtPointer call_data;
{
    XawTextPosition pos;
    char	    *string;
    int		    stop_no, line, nbytes;

    string = XFetchBytes(display, &nbytes);
    if (nbytes > 0 && (stop_no = atoi(string)) > 0) {
    	sprintf(command, "delete %d\n", stop_no);
	send_command(command);
	AppendDialogText(command);
	return;
    }
    else if (displayedFile) {
	pos = XawTextGetInsertionPoint(sourceWindow);
	line = TextPositionToLine(pos);
	if (stop_no = LineToStop_no(line)) {
	    sprintf(command, "delete %d\n", stop_no);
	    send_command(command);
	    AppendDialogText(command);
	    return;
	}
    }
    UpdateMessageWindow(DELETE_HELP, NULL);
    bell(0);
}

/* ARGSUSED */
static void Print(w, client_data, call_data)
    Widget w;
    XtPointer client_data;
    XtPointer call_data;
{
    char *string;
    int nbytes;

    if (Button == 3) PopupMode = True;

    string = XFetchBytes(display, &nbytes);
    if (nbytes == 0) {
	UpdateMessageWindow(PRINT_HELP, NULL);
	bell(0);
	return;
    }
    if (client_data == (XtPointer)0)
	sprintf(command, "print %s\n", string);
    else if (client_data == (XtPointer)1)
	sprintf(command, "print *%s\n", string);
    send_command(command);
#ifdef GDB
	if (!PopupMode)		/* for GDB don't display print if everything goes in a window */
#endif
    AppendDialogText(command);
}

#ifndef GDB /* >>>>>>>>>> NOT USED FOR GDB <<<<<<<<<<<<<<< */
/* ARGSUSED */
static void Func(w, client_data, call_data)
    Widget w;
    XtPointer client_data;
    XtPointer call_data;
{
    char *funcname;
    int nbytes;

    funcname = XFetchBytes(display, &nbytes);
    if (nbytes == 0)
    	strcpy(command, "func\n");
    else
    	sprintf(command, "func %s\n", funcname);
    send_command(command);
    AppendDialogText(command);
}
#endif /* NOT GDB */

/* ARGSUSED */
static void Quit(w, client_data, call_data)
    Widget w;
    XtPointer client_data;
    XtPointer call_data;
{
#ifdef SYSV 
    int status;
#else
    union wait status;
#endif /* SYSV */

    write_dbx("quit\n");
    XtDestroyApplicationContext(app_context);
    kill(dbxpid, SIGKILL);
#ifdef SYSV
#ifdef SVR4 
	status = waitpid(dbxpid, (int *)0, WNOHANG);	/* (MJH) */
#else
	waitpid(&status, NULL, WNOHANG);
#endif	/* SVR4 */
#else	/* not SYSV */
    wait3(&status, WNOHANG, NULL);
#endif /* SYSV */
    exit(0);
}


/* ARGSUSED */
static void Display_(w, client_data, call_data)
    Widget w;
    XtPointer client_data;
    XtPointer call_data;
{
    char *string;
    int nbytes;

    string = XFetchBytes(display, &nbytes);
    sprintf(command, "display %s\n", string);
    send_command(command);
    AppendDialogText(command);
}

#ifdef GDB	/* >>>>>>>>>>>>>> GDB ONLY <<<<<<<<<<<<<<<< */
/* ARGSUSED */
static void Undisplay(w, client_data, call_data)
    Widget w;
    XtPointer client_data;
    XtPointer call_data;
{
    char *string;
    int	 stop_no, nbytes;

    string = XFetchBytes(display, &nbytes);
    if (nbytes != 0)
    	{
		if ((stop_no = atoi(string)) > 0)
			sprintf(command, "undisplay %d\n", stop_no);
   		else
   			{
			UpdateMessageWindow(UNDISPLAY_HELP, NULL);
			bell(0);
			return;
   			}
   		}
   	else
		sprintf(command, "undisplay\n");
   		
	send_command(command);
	AppendDialogText(command);
}

#else	/* >>>>>>>>>> NOT USED FOR GDB <<<<<<<<<<<<<<< */

/* ARGSUSED */
static void Undisplay(w, client_data, call_data)
    Widget w;
    XtPointer client_data;
    XtPointer call_data;
{
    char *string;
    int	 stop_no, nbytes;

    string = XFetchBytes(display, &nbytes);
    if (nbytes == 0) {
	UpdateMessageWindow(UNDISPLAY_HELP, NULL);
	bell(0);
	return;
    }
    if ((stop_no = atoi(string)) > 0)
	sprintf(command, "undisplay %d\n", stop_no);
    else
    	sprintf(command, "undisplay %s\n", string);
    send_command(command);
    AppendDialogText(command);
}
#endif /* NOT GDB */

#ifndef GDB	/* >>>>>>>>>> NOT USED FOR GDB <<<<<<<<<<<<<<< */
/* ARGSUSED */
static void Dump(w, client_data, call_data)
    Widget w;
    XtPointer client_data;
    XtPointer call_data;
{
    char *funcname;
    int nbytes;

    funcname = XFetchBytes(display, &nbytes);
    if (nbytes == 0)
    	strcpy(command, "dump\n");
    else
    	sprintf(command, "dump %s\n", funcname);
    send_command(command);
    AppendDialogText(command);
}
#endif /* NOT GDB */


/*  Beginning from startpos, this routine searches text forward for 
 *  searchstring, and returns 1 if searchstring is found, also returning 
 *  the left and right positions of the matched string in left and right; 
 *  else 0 is returned.
 *  It also does wrap-around search.
 */
static forwardSearch(text, startpos, searchstring, left, right)
    char *text;
    int  startpos;
    char *searchstring;
    XawTextPosition  *left, *right;
{
    int  searchlength, searchsize, i, n=0;
    char *s1, *s2;

    searchlength = strlen(searchstring);
    searchsize = strlen(text) - searchlength;
    for (i=startpos+1; i < searchsize; i++) {
	n = searchlength;
	s1 = &text[i];
	s2 = searchstring;
	while (--n >= 0 && *s1++ == *s2++);
	if (n < 0) break;
    }
    if (n < 0) {
    	*left = i;
    	*right = i+searchlength;
    	return 1;
    }
    else {
	for (i=0; i <= startpos; i++) {
	    n = searchlength;
	    s1 = &text[i];
	    s2 = searchstring;
	    while (--n >= 0 && *s1++ == *s2++);
	    if (n < 0) break;
	}
	if (n < 0) {
	    *left = i;
	    *right = i+searchlength;
	    return 1;
	}
	return 0;
    }
}
	

/*  Similar to forwardSearch(), except that it does a reverse search
 */
static reverseSearch(text, startpos, searchstring, left, right)
    char 	    *text;
    XawTextPosition  startpos;
    char 	    *searchstring;
    XawTextPosition  *left, *right;
{
    int  searchlength, i, n=0;
    char *s1, *s2;

    searchlength = strlen(searchstring);
    for (i=startpos; i >= searchlength; i--) {
	n = searchlength;
	s1 = &text[i];
	s2 = &searchstring[searchlength-1];
	while (--n >= 0 && *--s1 == *s2--);
	if (n < 0) break;
    }
    if (n < 0) {
    	*right = i;
    	*left = *right-searchlength;
    	return 1;
    }
    else {
	for (i=strlen(text); i > startpos; i--) {
	    n = searchlength;
	    s1 = &text[i];
	    s2 = &searchstring[searchlength-1];
	    while (--n >= 0 && *--s1 == *s2--);
	    if (n < 0) break;
	}
	if (n < 0) {
            *right = i;
            *left = *right-searchlength;
	    return 1;
	}
	return 0;
    }
}

/* ARGSUSED */
static void PopupSearch(w, client_data, call_data)
    Widget w;
    XtPointer client_data;
    XtPointer call_data;
{
    Arg 	args[MAXARGS];
    Cardinal 	n;
    Dimension	popup_width, dialog_width;
    Position	x, y;

    if (!displayedFile) {
	UpdateMessageWindow(SEARCH_HELP, NULL);
	bell(0);
    }
    else {
	XtRealizeWidget(searchPopupShell);
	n = 0;
	XtSetArg(args[n], XtNwidth, &popup_width);			n++;
	XtGetValues(searchPopupShell, args, n);
	n = 0;
	XtSetArg(args[n], XtNwidth, &dialog_width);			n++;
	XtGetValues(dialogWindow, args, n);
	XtTranslateCoords(dialogWindow, 
			  (Position)(dialog_width - popup_width)/2, 10, &x, &y);
	n = 0;
	XtSetArg(args[n], XtNx, x);					n++;
	XtSetArg(args[n], XtNy, y);					n++;
	XtSetValues(searchPopupShell, args, n);
	XtPopup(searchPopupShell, XtGrabNone);
    }
}


/* ARGSUSED */
/*  This routine handles both forward and reverse text search.
 *  If no text has been entered, the contents of the cut buffer are used
 *  for searching.
 */ 
static void Search(w, direction, call_data)
    Widget w;
    XtPointer direction;
    XtPointer call_data;
{
    XawTextBlock    	textblock;
    XawTextPosition	pos, left, right;
    char		*searchString;

    searchString = XawDialogGetValueString(searchPopup);
    if (strlen(searchString) == 0) {
	textblock.ptr = XFetchBytes(display, &textblock.length);
	if (!textblock.ptr) {
	    UpdateMessageWindow("No search string selected", NULL);
	    bell(0);
	    return;
	}
	searchString = textblock.ptr;
    }
    pos = XawTextGetInsertionPoint(sourceWindow);
    if ((direction == (XtPointer)FORWARD && 
	forwardSearch(displayedFile->buf, pos, searchString, &left, &right)) ||
        (direction == (XtPointer)REVERSE && 
	reverseSearch(displayedFile->buf, pos, searchString, &left, &right))) {
        AdjustText(TextPositionToLine(left));
        XawTextSetSelection(sourceWindow, left, right);
        XawTextSetInsertionPoint(sourceWindow, left);
    }
    else {
        if (direction == (XtPointer)FORWARD)
            UpdateMessageWindow("String not found", NULL);
        else if (direction == (XtPointer)REVERSE)
            UpdateMessageWindow("String not found", NULL);
        else
#ifdef GDB
            UpdateMessageWindow("xxgdb error: illegal search direction", NULL);
#else
            UpdateMessageWindow("xdbx error: illegal search direction", NULL);
#endif
        bell(0);
    }
}

/* ARGSUSED */
static void DoneSearch(w, client_data, call_data)
    Widget w;
    XtPointer client_data;
    XtPointer call_data;
{
    XtPopdown(client_data);
}

/* ARGSUSED */
static void Activate(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    Search(w, (XtPointer)FORWARD, NULL);
    DoneSearch(w, (XtPointer)searchPopupShell, NULL);
}

static void CreateSearchPopup()
{
    Widget	dialogValue;
    Arg 	args[MAXARGS];
    Cardinal 	n;

    static XtActionsRec search_actions[] = {
        {"Activate", Activate},
        {NULL, NULL}
    };

    static String translations = "#override\n\
        <Key>Return:         Activate() \n\
    ";

    n = 0;
    XtSetArg(args[n], XtNinput, True);					n++;
    XtSetArg(args[n], XtNallowShellResize, True);			n++;
    searchPopupShell = XtCreatePopupShell("Search", transientShellWidgetClass, 
	toplevel, args, n);

    n = 0;
    XtSetArg(args[n], XtNlabel, "Enter search string :");		n++;
    XtSetArg(args[n], XtNvalue, SearchString);				n++;
    searchPopup = XtCreateManagedWidget("searchPopup", dialogWidgetClass, 
	searchPopupShell, args, n);
    
    AddButton(searchPopup, "<<", Search, (XtPointer) REVERSE);
    AddButton(searchPopup, ">>", Search, (XtPointer) FORWARD);
    AddButton(searchPopup, "DONE", DoneSearch, (XtPointer)searchPopupShell);

    dialogValue = XtNameToWidget(searchPopup, "value");
    XtOverrideTranslations(dialogValue, XtParseTranslationTable(translations));
    XtAppAddActions(app_context, search_actions, XtNumber(search_actions));
}



static Widget AddButton(parent, name, function, client_data)
Widget parent;
char *name;
void (*function) ();
XtPointer client_data;		/* callback registered data */
{
    Widget 	button;
    Arg 	args[MAXARGS];
    Cardinal 	n;

    static XtActionsRec command_actions[] = {
	{"ButtonSet", (XtActionProc) ButtonSet},
        {NULL, NULL}
    };

    static String translations = "\
	<EnterWindow>:	highlight() \n\
	<LeaveWindow>:	reset() \n\
	<Btn1Down>:	set()\n\
	<Btn1Up>:	ButtonSet(1) notify() unset() \n\
	<Btn3Down>:	set()\n\
	<Btn3Up>:	ButtonSet(3) notify() unset()\n\
    ";

    n = 0;
    XtSetArg(args[n], XtNresize, (XtArgVal) False);			n++;
    if (strcmp(name, "print") == NULL || strcmp(name, "print *") == NULL) {
	XtSetArg(args[n], XtNtranslations, 
	    XtParseTranslationTable(translations)); 			n++;
    }
    button = XtCreateManagedWidget(name, commandWidgetClass, parent, args, n);
    XtAddCallback(button, XtNcallback, function, client_data);
    XtAppAddActions(app_context, command_actions, XtNumber(command_actions));
    return (button);
}


static void CreateButtons (parent)
Widget parent;
{
    int i=0;

#ifdef GDB	/* >>>>>>>>>>>>>> GDB ONLY <<<<<<<<<<<<<<<< */
    button[i++] = AddButton (parent, "run", DoIt, "run\n");
    button[i++] = AddButton (parent, "cont", DoIt, "cont\n");
    button[i++] = AddButton (parent, "next", DoIt, "next\n");
    button[i++] = AddButton (parent, "step", DoIt, "step\n");
    button[i++] = AddButton (parent, "finish", DoIt, "finish\n");
    button[i++] = AddButton (parent, "break", Break, "break");
    button[i++] = AddButton (parent, "tbreak", Break, "tbreak");
    button[i++] = AddButton (parent, "delete", Delete, NULL);
    button[i++] = AddButton (parent, "up", DoIt, "up\n");
    button[i++] = AddButton (parent, "down", DoIt, "down\n");
    button[i++] = AddButton (parent, "print", Print, (XtPointer)0);
    button[i++] = AddButton (parent, "print *", Print, (XtPointer)1);
    button[i++] = AddButton (parent, "display", Display_, NULL);
    button[i++] = AddButton (parent, "undisplay", Undisplay, NULL);
    button[i++] = AddButton (parent, "args", DoIt, "info args\n");
    button[i++] = AddButton (parent, "locals", DoIt, "info locals\n");
    button[i++] = AddButton (parent, "stack", DoIt, "info stack\n");
    button[i++] = AddButton (parent, "search", PopupSearch, NULL);
    button[i++] = AddButton (parent, "file", File, NULL);
    button[i++] = AddButton (parent, "quit", Quit, NULL);
#else	/* >>>>>>>>>> IF NOT GDB <<<<<<<<<<<<<<< */


    button[i++] = AddButton (parent, "run", DoIt, "run\n");
    button[i++] = AddButton (parent, "cont", DoIt, "cont\n");
    button[i++] = AddButton (parent, "next", DoIt, "next\n");
    button[i++] = AddButton (parent, "step", DoIt, "step\n");
#ifdef BSD
    button[i++] = AddButton (parent, "return", Return, "return\n");
#endif
    button[i++] = AddButton (parent, "stop at", Stop_at, NULL);
    button[i++] = AddButton (parent, "stop in", Stop_in, NULL);
    button[i++] = AddButton (parent, "delete", Delete, NULL);
    button[i++] = AddButton (parent, "where", DoIt, "where\n");
    button[i++] = AddButton (parent, "up", DoIt, "up\n");
    button[i++] = AddButton (parent, "down", DoIt, "down\n");
    button[i++] = AddButton (parent, "print", Print, (XtPointer)0);
    button[i++] = AddButton (parent, "print *", Print, (XtPointer)1);
    button[i++] = AddButton (parent, "func", Func, NULL);
    button[i++] = AddButton (parent, "file", File, NULL);
    button[i++] = AddButton (parent, "status", DoIt, "status\n");
#ifndef BSD
    button[i++] = AddButton (parent, "display", Display_, NULL);
    button[i++] = AddButton (parent, "undisplay", Undisplay, NULL);
#endif
    button[i++] = AddButton (parent, "dump", Dump, NULL);
    button[i++] = AddButton (parent, "search", PopupSearch, NULL);
    button[i++] = AddButton (parent, "quit", Quit, NULL);
#endif /* NOT GDB */

    button[i++] = NULL;
    CreateSearchPopup();
}


/*  Create a command widget, and the buttons.  */

void CreateCommandPanel(parent)
Widget parent;
{
    Arg args[10];
    Cardinal n;

    n = 0;
    commandWindow = XtCreateManagedWidget("commandWindow", boxWidgetClass, 
					  parent, args, n);
    CreateButtons(commandWindow);
#ifndef SYSV 
    getwd(cwd);
#endif
}

/**************************************************************************
 *
 *  Command queue functions
 *
 **************************************************************************/

/*  Append command to end of the command queue and send the command to dbx */

void send_command(command)
char *command;
{
    CommandRec *p, *q, *r;

#ifndef GDB 
#ifdef BSD 
    /* Save the command if it is not a blank command; else use the 
       last saved command instead */
    if (strcspn(command, " \n"))
	strcpy(savedCommand, command);
    else
	strcpy(command, savedCommand);
#endif
#endif	/* not GDB */

    p = (CommandRec *)XtNew(CommandRec);
    p->command = XtNewString(command);
    p->next = NULL;
    if (!commandQueue)
	commandQueue = p;
    else {
	q = commandQueue;
	while (r = q->next)
	    q = r;
	q->next = p;
    }

#ifdef GDB
	write_dbx_available();
#else
    write_dbx(command);
#endif /* GDB */
}

/*  Read command at the head of the command queue */

char *get_command()
{
    if (commandQueue) {
	return (commandQueue->command);
    }
    else
	return NULL;
}

/*  Delete command from the head of the command queue */

void delete_command()
{
    CommandRec *p;

    if (p = commandQueue) {
	commandQueue = p->next;
	XtFree(p->command);
	XtFree(p);
    }
}

/*  Insert command into head of queue */

void insert_command(command)
char *command;
{
    CommandRec *p;

    p = (CommandRec *)XtNew(CommandRec);
    p->command = XtNewString(command);
    p->next = NULL;
    if (!commandQueue)
	commandQueue = p;
    else {
	p->next = commandQueue;
	commandQueue = p;
    }
}
