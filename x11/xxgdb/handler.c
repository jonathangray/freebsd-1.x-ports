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

/*  handler.c
 *
 *    Contain action handlers for the parser to invoke upon a dbx command.
 *
 *    TextSetTopPosition():	Set the top character position of text displayed
 *    AdjustText():		Adjust the portion of text displayed.
 *    exec_handler():		Update file, line label, arrow position.
 *    done_handler():		Progrm execution completed, clear breakpoints
 *    stop_at_handler():	Place stop sign on line specified.
 *    stop_in_handler():	Place stop sign on function specified.
 *    updown_handler():		Update file, line label, updown arrow position.
 *    delete_handler():		Remove stop sign.
 *    func_handler():		Display function, if specified.
 *    file_handler():		Display file, if specified.
 *    debug_handler():		Check directory use list, display source file.
 *    cd_handler():		Record current working directory.
 *    use_handler():		Record directory paths.
 *    search_handler():		Adjust source file to display matched line.
 *    list_handler();		Adjust source file to display result.
 *    display_handler():	Display results in display window.
 */

#include <ctype.h>
#include "global.h"
#ifdef BSD
#define	BRACKET	"[%d]"
#else
#define	BRACKET	"(%d)"
#endif

#ifndef GDB
Boolean		Echo = True;		/* display dbx output if true */
static Boolean	Skip_func_handler = False;
#endif

/*  Display text starting from the top position specified by pos */

void TextSetTopPosition(w, pos)
    Widget w;
    XawTextPosition pos;
{
    Arg args[MAXARGS];
    Cardinal n;

    n = 0;
    XtSetArg(args[n], XtNdisplayPosition, (XtArgVal) pos);               n++;
    XtSetValues(w, args, n);
}

/*
 *  Adjust text so that 'line' will fall into the viewable part of the
 *  source window.
 *  Arrows, stop signs, and line label are updated accordingly.
 */
void AdjustText(line)
    int	   	line;
{
    FileRec 		*file;
    int	    		nlines = 0;
    int			i;
    XawTextPosition 	pos;

    if ((file = displayedFile) == NULL || line <= 0) return;
    file->currentline = line;

    if (line < file->topline || line > file->bottomline ) {
	/* Position line about 30% from the top */
	nlines = file->lines*0.3;
	if (line < nlines)			   /* near top */
	    file->topline = 1;
	else if (line > file->lastline - nlines)  /* near bottom */
	    file->topline = MAX(file->lastline - file->lines + 1, 1);
	else
	    file->topline = line - nlines;
	file->bottomline = MIN(file->topline + file->lines - 1, file->lastline);
	TextSetTopPosition(sourceWindow, file->linepos[file->topline]);
	file->topPosition = file->linepos[file->topline];
    }
    XawTextSetInsertionPoint(sourceWindow, file->linepos[line]);

    /* Text window might have scrolled, check topline & bottomline */
    pos = XawTextTopPosition(sourceWindow);
    for (i=1; pos >= file->linepos[i]; i++);
    if (file->topline != i-1) {
	file->topline = i-1;
	file->bottomline = MIN (file->topline + file->lines - 1,
				file->lastline);
    }
    UpdateLineLabel(line);
    UpdateStops(file);
    UpdateArrow(file);
    UpdateUpdown(file);
    UpdateBomb(file);
}
    
#ifdef GDB

#include "gdb_handler.c"

#else /*>>>>>>>>>> ALL THE FOLLOWING IS NOT COMPILED FOR GDB <<<<<<<<<<<<<<<<<<<*/

/*  Handle dbx output of run, cont, next, step, return commands.
 *  Result of output parsing is returned in a set of tokens.
 */
void exec_handler()
{
    int	 line, status;
    char *func, *mesg;
    char *segv = "signal SEGV";
    char *segfault = "Segmentation fault";

    /* Print "stopped in ..." line in message window 
     * Adjust text displayed
     */
    if (Token.func == NULL || Token.line == 0) 
	return; 
    UpdateMessageWindow(Token.mesg,NULL);
    line = Token.line;
    func = XtNewString(Token.func);
    mesg = XtNewString(Token.mesg);
#ifdef MIPS
    status = LoadCurrentFile();
#else
    if (Token.file)
	status = LoadFile(Token.file);
#endif
    arrow.line = line;			/* update arrow sign position */
    strcpy(arrow.func, func);
    updown.line = 0;			/* remove updown, if any */
    if (displayedFile) {
    	strcpy(arrow.file, displayedFile->pathname);
    }
    /* Display bomb sign if segmentation fault occurs in source code */
    if (status != -1 && (strncmp(mesg, segv, strlen(segv)) == NULL ||
	strncmp(mesg, segfault, strlen(segfault)) == NULL)) {
	arrow.line = 0;
	bomb.line = line;
	strcpy(bomb.func, func);
    	if (displayedFile) strcpy(bomb.file, displayedFile->pathname);
    }
    else
	bomb.line = 0;

    AdjustText(line);
#ifndef BSD
    display_handler();
#endif
    XtFree(func);
    XtFree(mesg);
}

/*  Remove all the arrow and updown signs, print message, then 
 *  change the file variable to the file name displayed.
 */
void done_handler()
{
    char command[LINESIZ];

    arrow.line = 0;
    updown.line = 0;
    UpdateArrow(displayedFile);
    UpdateUpdown(displayedFile);
    UpdateMessageWindow("Ready for execution",NULL);
    if (displayedFile == NULL) return;
#ifdef MIPS
    sprintf(command, "file %s\n", displayedFile->filename);
#else
    sprintf(command, "file %s\n", displayedFile->pathname);
#endif
    Parse = False;
    query_dbx(command);
}

/*  Place a stop sign next to the line specified on the source file window 
 *  if it is to be viewable.
 */
void stop_at_handler()
{
    if (Token.stop == 0 || Token.line == 0 || displayedFile == NULL)
	return;
    if (Token.file == NULL)
	stops[Token.stop].file = displayedFile->pathname;
    else
	stops[Token.stop].file = GetPathname(Token.file);
    DisplayStop(displayedFile, Token.line);
    stops[Token.stop].line = Token.line;
    stops[Token.stop].tag = 0;
    nstops = Token.stop;
}


/*
 *  Place a stop sign next to the function routine, getting the line number 
 *  by "list <func>", (or "func <func>" on a MIPS), and resetting the file 
 *  variable properly.
 */
void stop_in_handler()
{
    char command[LINESIZ], *file;
    int  stop;
    int	 line;

    if (Token.stop == 0 || Token.func == NULL || displayedFile == NULL)
	return;
    stop = Token.stop;
#ifdef MIPS
    /* For mips dbx, need to use func command to locate the function */
    Skip_func_handler = True;
    sprintf(command, "func %s\n", Token.func);
    query_dbx(command);
#else
#ifdef BSD
    sprintf(command, "list %s\n", Token.func);
    query_dbx(command);
#else
    sprintf(command, "list %s\n", Token.func);
    query_dbx(command);
    if (Token.line <= 0) 
	return;
    else 
	Token.line += 5;
#endif
#endif

    stops[stop].line = Token.line;
    nstops = stop;
    line = Token.line;

    /* Check the name of the file containing Token.func */
    query_dbx("file\n");
    if ((file = GetPathname(CurrentFile)) && 
        strcmp(file, displayedFile->pathname)) {   /* new file, record stop */
	stops[nstops].file = file;
#ifdef MIPS
	sprintf(command, "file %s\n", displayedFile->filename);
#else
	sprintf(command, "file %s\n", displayedFile->pathname);
#endif
	Parse = False;
	query_dbx(command);
    }
    else { 					   /* same file, display stop */
	stops[nstops].file = displayedFile->pathname;
	DisplayStop(displayedFile, line);
    }
}

/*  
 *  Display an outlined arrow to locate the calling routine in a stack
 *  frame.  BSD and SUN dbx have slightly different output semantics here.
 *  The appropriate file with the calling routine is displayed and the
 *  file variable is set accordingly.
 */
void updown_handler()
{
    char command[LINESIZ], *func, *file;
    int	 line;

    line = Token.line;
    func = XtNewString(Token.func);
#ifdef MIPS
    LoadCurrentFile();
#endif
#ifdef BSD
    file = GetPathname(Token.file);
#else
    if (line <= 0) line = 1;
    LoadCurrentFile();
    if (displayedFile)
	file = displayedFile->pathname;
#endif

    if (line <= 0 || func == NULL || file == NULL) 
	return;
    if (displayedFile && strcmp(file, displayedFile->pathname)) {
	LoadFile(file);
	
	/* set dbx file variable to file */
#ifdef MIPS
	sprintf(command, "file %s\n", displayedFile->filename);
#else
	sprintf(command, "file %s\n", displayedFile->pathname);
#endif
	Parse = False;
	query_dbx(command);
    }
    updown.line = line;
    strcpy(updown.func, func);
    if (displayedFile)
    	strcpy(updown.file, displayedFile->pathname);
    AdjustText(line);
    XtFree(func);
}

/*
 *  Delete handler remove the stop specified and undisplayed the stopsign
 *  if it's visible.
 *  It calls the dbx status command to find out what stops are left, and
 *  then update the array of stops accordingly.
 */
/* ARGSUSED */
void delete_handler()
{
    char s[LINESIZ];
    int  i; 
    int	 line;

    write_dbx("status\n");
    while (fgets(s, LINESIZ, dbxfp) == NULL);
    do {
	if (strcmp(s, dbxprompt) || strcmp(s, "")) {
	    sscanf(s, BRACKET, &i);
	    if (i > 0 && i <= nstops && stops[i].line > 0) 
	    	stops[i].tag = 1;
	}
    } while (fgets(s, LINESIZ, dbxfp));

    for (i=1; i<=nstops; i++)
	if (stops[i].line > 0) {
	    if (stops[i].tag)
		stops[i].tag = 0;
	    else {
		line = stops[i].line;
		stops[i].line = 0;
		stops[i].file = NULL;
		if (LineToStop_no(line) == 0)
		    RemoveStop(line);
	    }
	}
}

/*
 *  This handler displays the function routine on the source window.
 *  It locates the function by sending the dbx command "list <func>",
 *  and loads the appropriate file accordingly.
 */
void func_handler()
{
    int	 line;
    char command[LINESIZ];

    if (Token.func && !Skip_func_handler) {
#ifdef MIPS
	line = Token.line;
#else
	sprintf(command, "list %s\n", Token.func);
	query_dbx(command);
	line = Token.line + 5;
#endif
	LoadCurrentFile();
	AdjustText(line);
    }
    Skip_func_handler = False;
}


/*  File handler first queries the current file set by the user command,
 *  and then loads the file.
 */
/* ARGSUSED */
void file_handler() 	/* Command was 'file' */
{
    if (Token.file)
	strcpy(CurrentFile, Token.file);
    else
	strcpy(CurrentFile, "");
}

/* ARGSUSED */
void debug_handler()
{
    query_dbx("use\n");
    displayedFile = NULL;		/* force reloading of source file */
    if (LoadCurrentFile() == 0) {
	arrow.line = 0;			/* clear arrow sign */
	updown.line = 0;		/* clear updown sign */
	bomb.line = 0;			/* clear bomb sign */
	UpdateArrow(displayedFile);
	UpdateUpdown(displayedFile);
	UpdateBomb(displayedFile);
	ClearStops();
	UpdateStops(displayedFile);
        UpdateMessageWindow("Ready for execution",NULL);
	query_dbx("func main\n");
#ifndef BSD
	query_dbx("display\n");		/* clear display window */
#endif
    }
}

/* ARGSUSED */
void cd_handler()
{
    query_dbx("pwd\n");
}

/* ARGSUSED */
void pwd_handler(s)
char *s;
{
    strcpy(cwd, (char *)strtok(s, "\n"));
}

/* ARGSUSED */
void use_handler(output)
char *output;
{
    if (strcmp(output, "") == NULL)
	query_dbx("use\n");
    else
    	MakeDirList(output);
}

/* ARGSUSED */
void search_handler()
{
    AdjustText(Token.line);
}

/* ARGSUSED */
void list_handler()
{
    int	 line;

    if (Echo) {
	line = Token.line;
	LoadCurrentFile();
    	AdjustText(line);
    }
}

/* ARGSUSED */
/*  Show output on the display window.
 *  If output is null but the display window is managed, replace contents of
 *  the display window with the null string.
 */
void display_handler()
{
    Arg		args[MAXARGS];
    Cardinal	n;

    if (!Token.display || strcmp(Token.display, "") == NULL) {
	if (!XtIsManaged(displayWindow))
	    return;
	else {
	    XtFree(Token.display);
	    Token.display = XtNewString("");
	}
    }
    if (!XtIsManaged(displayWindow)) {
	XtManageChild(separator);
	XtManageChild(displayWindow);
    }
    n = 0;
    XtSetArg(args[n], XtNstring, (XtArgVal) Token.display);		n++;
    XtSetValues(displayWindow, args, n);
    XtFree(Token.display);
}

#endif /* NOT GDB */
