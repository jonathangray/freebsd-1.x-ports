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

/*  gdb_handler.c
 *
 *	WARNING : gdb_handler.c is included by handler.c for GDB.
 *
 *    Contain action handlers for the parser to invoke upon a dbx command.
 *
 *    updown_handler():		Update file, line label, updown arrow position.
 *    debug_handler():		Check directory use list, display main source file.
 *    pwd_handler():		Update current working directory.
 *    search_handler():		Adjust source file to display matched line.
 *    display_info_handler(): Update display window.
 *    break_handler():		Place stop sign on line or function or address specified.
 *    info_dir_handler():	Update search directory list. 
 *    directory_handler():	Update search directory list. 
 *    list_handler():		Adjust source file to display result. 
 *    info_line_handler():	Update current file. 
 *    delete_handler():		Remove stop sign.
 *    display_handler():	Update display window.
 *    info_break_handler():	Update stop signs.
 *    cd_handler():			Record current working directory.
 *    frame_curr_handler():	Update current function name.
 *    exec_handler():		Update file, line label, arrow position.
 *    done_handler():		Progrm execution completed, clear breakpoints
 *    source_handler():		Exec commands of source file specified.
 *    query_gdb():			Send command to gdb.
 */

#ifdef SYSV 
#   include <signal.h>
#endif


/*  
 *  Display an outlined arrow to locate the calling routine in a stack
 *  frame.
 */
void updown_handler()
{
    char *func, *file;
    int	 line;

    line = Token.line;
    func = XtNewString(Token.func);

    if (line <= 0) line = 1;
    LoadCurrentFile();
    if (displayedFile)
	file = displayedFile->pathname;

    if (line <= 0 || func == NULL || file == NULL)
    	{
    	XtFree(func);
		return;
		}
		
    if (displayedFile && strcmp(file, displayedFile->pathname)) {
	LoadFile(file);
    }
    updown.line = line;
    strcpy(updown.func, func);
    if (displayedFile)
    	strcpy(updown.file, displayedFile->pathname);
    AdjustText(line);
    XtFree(func);
}

/* ARGSUSED */
void debug_handler()
{
	/* debug_handler is executed at start-up and with 'symbol-file' command and
	with 'file' command (gdb 4.0) */
	
	info_source_handler(NULL);	/* say no current compilation directory */
	
    query_gdb_directories();

	if (new_gdb4()) /* (PW)28AUG91: test for gdb 4.0 */
		{
		/* (SH) for gdb 4.0 */
	    query_gdb("set height 0\n",		PARSE_OFF | ECHO_OFF | FILTER_OFF);	
	    query_gdb("set width 0\n", 		PARSE_OFF | ECHO_OFF | FILTER_OFF);
    	query_gdb("set p pretty on\n",	PARSE_OFF | ECHO_OFF | FILTER_OFF);
    	query_gdb("set confirm off\n",	PARSE_OFF | ECHO_OFF | FILTER_OFF);
		}
	else
		{
	    query_gdb("set screensize 0\n",		PARSE_OFF | ECHO_OFF | FILTER_OFF);
    	query_gdb("set prettyprint on\n",	PARSE_OFF | ECHO_OFF | FILTER_OFF);
    	}
    
    displayedFile = NULL;		/* force reloading of source file */
 
 	/* here we use FILTER_ON so that any error message will be displayed ! */
	/* tell gdb to use main file and get line number of main(). (,main will end at main) */
	
 	query_gdb("list ,main\n", PARSE_ON | ECHO_OFF | FILTER_ON);

	if (displayedFile && LoadCurrentFile() == 0)
		/* Only if there is a current displayedFile (pavel 4-Dec-1991) */
    	{
		arrow.line = 0;			/* clear arrow sign */
		updown.line = 0;		/* clear updown sign */
		bomb.line = 0;			/* clear bomb sign */
		UpdateArrow(displayedFile);
		UpdateUpdown(displayedFile);
		UpdateBomb(displayedFile);
		ClearStops();
		UpdateStops(displayedFile);
		}
		
	UpdateMessageWindow("Ready for execution",NULL);
	
	/* clear display window */
	
	query_gdb("display\n", PARSE_ON | ECHO_OFF | FILTER_OFF);
}

/* ARGSUSED */
void pwd_handler(s)
char *s;
{
    strcpy(cwd, (char *)strtok(s, "\n"));
}

/* ARGSUSED */
void search_handler()
{
    AdjustText(Token.line);
}

/* ARGSUSED */
/*  Show output on the display window.
 *  If output is null but the display window is managed, replace contents of
 *  the display window with the null string.
 */
void display_info_handler()
{
    Arg		args[MAXARGS];
    Cardinal	n;
    
#ifdef UNDISPWIN
	/* this code removes the display window when there is nothing to display (GWC) */
	
	if (!Token.display || strcmp(Token.display, "") == NULL) {
		XtUnmanageChild(separator);
		XtUnmanageChild(displayWindow);
		return;
	}
#endif /* UNDISPWIN */

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
    Token.display = 0;		/*(PW)14JAN91 */
}

/*  Place a stop sign next to the line specified on the source file window 
 *  if it is to be viewable.
 */
void break_handler()
{
char * file;
int line;
int stop;

    if (Token.stop == 0 || Token.line == 0 || Token.file == 0)
	return;
	
	line = Token.line;
	stop = Token.stop;
	
	if (Token.stop >= 256)	/* see MAXSTOPS in signs.c */
		{
		fprintf(stderr,"Too many breakpoints\n");
		return;
		}
	
	/* load & display file if none is displayed */
	
	file = GetPathname(Token.file);
	
	if (file == NULL)
		return;		/* (PW)11JAN91 */
		
	if (displayedFile == NULL)
		{
		LoadFile(file);
		AdjustText(line);
		}
		
	stops[stop].file = file;
    stops[stop].line = line;
    stops[stop].tag = 0;
    nstops = stop;

	/* display breakpoint sign if file is displayed */
	
	if (displayedFile)
		{
		if (!strcmp(file, displayedFile->pathname))	
			DisplayStop(displayedFile, line);
		}
}

/*  info directories 
 */
void info_dir_handler()
{
    if (Token.file)
		MakeDirList(Token.file);
}

/* ARGSUSED */
void directory_handler()
{
	/* Note : for GDB, the 'directory' command with no
	parameter will reset search directories to current 
	directory only. GDB requires confirmation */
	
    query_gdb_directories();
}

void list_handler()
{
    int	 line;
    
	line = Token.line;

    if (line)
		{
		/* update compilation directory (if gdb 4.0) before loading source file */
		if (new_gdb4())
			query_gdb("info source\n", PARSE_ON | ECHO_OFF | FILTER_OFF);
		
		/* We will display the last line listed. 
		Since we used 'list ,main' we will effectively display main in that case. */

		LoadCurrentFile();
		AdjustText(line);
		}
	else
		{
		AppendDialogText("Error list command\n");
		bell(0);
		}
}

/* ARGSUSED */
void info_line_handler() 	/* Command was 'info line' */
{
    if (Token.file)
	strcpy(CurrentFile, Token.file);
    else
	strcpy(CurrentFile, "");
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
	/* update breakpoints */
    query_gdb("info break\n", PARSE_ON | ECHO_OFF | FILTER_OFF);
}

void display_handler()	/* display or undisplay */
{
	/* update display */
    query_gdb("display\n", PARSE_ON | ECHO_OFF | FILTER_ON);
}

/* 
(gdb) info break
Breakpoints:
Num Enb   Address    Where
#1   y  0x000022f4  in main (pw.c line 34)
#2   y  0x000022a0  in foo (pw.c line 5)
(gdb) info break
No breakpoints.
*/

void info_break_handler(output_string)
char *output_string;
{
int  i; 
int	 line;
char c;

	if (!output_string)
		return;
		
	while(*output_string)
		{
		if (*(output_string++) == '#')
			{
			if (sscanf(output_string, "%d %c", &i,&c) == 2)
				if (i > 0 && i <= nstops && stops[i].line > 0 && c == 'y') 
					stops[i].tag = 1;
			}
		}
		
    for (i=1; i<=nstops; i++)
	if (stops[i].line > 0)
		{
	    if (stops[i].tag)
			stops[i].tag = 0;
	    else 
	    	{
			line = stops[i].line;
			stops[i].line = 0;
			stops[i].file = NULL;
			if (LineToStop_no(line) == 0)
				RemoveStop(line);
			}
		}
}

/* ARGSUSED */
void cd_handler(s)
char *s;
{
    strcpy(cwd,s);
}

/* this handler justs update the function name.
Because the function name is not always displayed
after next,step ... */

static char* funcname = 0;

void frame_curr_handler()
{
	if (Token.func == NULL)
		return;
		
	if (funcname)
		{
		XtFree(funcname);
		funcname = 0;
		}
		
	funcname = XtNewString(Token.func);
}

/*  Handle dbx output of run, cont, next, step, return commands.
 *  Result of output parsing is returned in a set of tokens.
 *
 *	If message is not 0, this is an important message and should
 *	be displayed instead of Token.mesg.
 *	This message will hold the Bus error and segmentation violation errors.
 *	signal is the signal number received (if any).
 */
void exec_handler(message,signal)
char *message;
int signal;
{
    int	 line, status;
    char *func;

    /* Print "stopped in ..." line in message window 
     * Adjust text displayed
     */
    if (Token.line == 0) 
		return; 
		
	if (message)
		UpdateMessageWindow(message,NULL);
	else
		UpdateMessageWindow(Token.mesg,NULL);
		
    line = Token.line;
    func = (Token.func) ? XtNewString(Token.func) : 0;
    
    if (Token.file)
	status = LoadFile(Token.file);
	
    display_info_handler();		/* uses Token.display ! */

	/* because of tbreak, we have to call info break here */
	
     query_gdb("info break\n", PARSE_ON | ECHO_OFF | FILTER_OFF); /* update breakpoints */

	if (func == NULL)
		{
		/* because silly gdb 4.0 displays nothing with frame command when
		confirm is on (possibly a gdb bug) , I just reset confirm to on
		just for this command !. */
		
		if (new_gdb4())
			query_gdb("set confirm on\n",	PARSE_OFF | ECHO_OFF | FILTER_OFF);
			
		/* this will just update funcname (see frame_curr_handler()) */
		query_gdb("frame\n", PARSE_ON | ECHO_OFF | FILTER_OFF);
		
		if (new_gdb4()) /* reset confirm to off */
			query_gdb("set confirm off\n",	PARSE_OFF | ECHO_OFF | FILTER_OFF);
			
		func = funcname;
		if (func == NULL)
			return;
		funcname = 0;	/* tell frame_curr_handler WE are going to XtFree it */
		}
		
    arrow.line = line;			/* update arrow sign position */
	strcpy(arrow.func, func);
	
    updown.line = 0;			/* remove updown, if any */
    if (displayedFile) {
    	strcpy(arrow.file, displayedFile->pathname);
    }
    
    /* Display bomb sign if segmentation fault occurs in source code */
    
    if (status != -1 && message && signal == SIGSEGV) {
    arrow.line = 0;
	bomb.line = line;
	if (func)
	strcpy(bomb.func, func);
	if (displayedFile) strcpy(bomb.file, displayedFile->pathname);
    }
    else
	bomb.line = 0;

    AdjustText(line);
	XtFree(func);
}

/*  Remove all the arrow and updown signs, print message, then 
 *  change the file variable to the file name displayed.
 */
/* ARGSUSED */
void done_handler(message,signal)
char *message;
int signal;
{
    arrow.line = 0;
    updown.line = 0;
    UpdateArrow(displayedFile);
    UpdateUpdown(displayedFile);
    UpdateMessageWindow("Ready for execution",NULL);
}

/*--------------------------------------------------------------------------+
|																			|
|	Function to read the .gdbinit file or any source file.					|
|																			|
|	input : file name.														|
|																			|
|	output : none.															|
|																			|
+--------------------------------------------------------------------------*/
void read_source_file(file)
char *file;
{
char s[LINESIZ];
FILE *fp;

    if (fp = fopen(file, "r"))
    	{
		while (fgets(s, LINESIZ, fp))
			{
			/* Check for comment line,
				DO NOT SEND '\n',
				Take care of source command,
				Take care of define or document commands.
			*/
			
			if ((*s != '#') && strcmp(s,"\n"))	
				{
				if (!(gdb_source_command(s,TRUE) || gdb_define_command(s,fp)))
					query_gdb(s, PARSE_ON | ECHO_ON | FILTER_ON);
				}
			}
			
		fclose(fp);
		}
}

/* WARNING : source_handler() is NOT called by the parser.
It is called by gdb_source_command() in gdb_parser.c.
This is because 'source' command is NEVER sent to gdb,
instead xxgdb sends the commands in the specified file
one by one. */

void source_handler()
{
char *file;

    if (!Token.file || strcmp(Token.file, "") == NULL)
    	{
    	XtFree(Token.file);
		Token.file = XtNewString(".gdbinit");		/* default is .gdbinit */
		}
		
	file = GetPathname(Token.file);
	
	if (file == NULL)
		return;		/* (PW)11JAN91 */

	read_source_file(file);
}

/*  core-file  
 *  gdb does not display current line, file, func ..., so we send
 *  'frame 0' command to have them.
 */
void core_file_handler()
{
	char *message;
	int line;
	int signal;
	
	message = XtNewString(Token.mesg);
	signal = Token.stop;					/* signal number received */
	
	/* because silly gdb 4.0 displays nothing with frame command when
	confirm is on (possibly a gdb bug) , I just reset confirm to on
	just for this command !. */

	if (new_gdb4())
		query_gdb("set confirm on\n",	PARSE_OFF | ECHO_OFF | FILTER_OFF);

	/* this will update updown.line, updown.func and updown.file.
	(see updown_handler) */
	query_gdb("frame 0\n", PARSE_ON | ECHO_OFF | FILTER_OFF);
	
	if (new_gdb4()) /* reset confirm to off */
		query_gdb("set confirm off\n",	PARSE_OFF | ECHO_OFF | FILTER_OFF);

	line = updown.line;
	updown.line = 0;
	
    /* Display bomb sign if segmentation fault occurred in source code */
    
    if (line != 0 && signal == SIGSEGV) {
    arrow.line = 0;
	bomb.line = line;
	strcpy(bomb.func, updown.func);
	strcpy(bomb.file, updown.file);
    } else {
	bomb.line = 0;
	arrow.line = line;
	strcpy(arrow.func, updown.func);
	strcpy(arrow.file, updown.file);
	}
	
	UpdateMessageWindow(message,NULL);
	XtFree(message);
    AdjustText(line); 
}

/*--------------------------------------------------------------------------+
|																			|
|	Function to get the current source path directories.					|
|																			|
|	 WARNING : this function is called at startup of xxgdb					|
|	 to test for gdb 4.0 or earlier versions.								|
|	 The 1st time that query_gdb_directories() is called,					|
|	 new_gdb is true, and 'show directories' command is						|
|	 issued to gdb. Then parse() in gdb_parser.c will test for				|
|	 'show Unknown command' answer from gdb, and in that case				|
|	 will set new_gdb to false. See show_is_undefined() below.				|
|																			|
+--------------------------------------------------------------------------*/
void query_gdb_directories()
{
	if (new_gdb4()) /* (PW)28AUG91: test for gdb 4.0 */
		query_gdb("show directories\n", PARSE_ON | ECHO_OFF | FILTER_OFF);
	else
		query_gdb("info directories\n", PARSE_ON | ECHO_OFF | FILTER_OFF);
}

/*--------------------------------------------------------------------------+
|																			|
|	Function to know if we are running the NEW gdb							|
|																			|
|	return TRUE if we are running gdb 4.0 (or 3.9x or higher)				|
|																			|
|	WARNING : it is important that the default is gdb 4.0.					|
|			  (see comments on query_gdb_directories() above).				|
|																			|
+--------------------------------------------------------------------------*/
static int new_gdb = True; /* default : we are running gdb 4.0 */

int new_gdb4()
{
	return new_gdb;
}

/*--------------------------------------------------------------------------+
|																			|
|	Function to say that 'show' command is undefined and thus				|
|	that we are running an old version of gdb.								|
|																			|
+--------------------------------------------------------------------------*/
void show_is_undefined()
{
	new_gdb = False;
}

/*--------------------------------------------------------------------------+
|																			|
|	Update compilation directory											|
|																			|
|	This function is used for gdb 4.0 which uses the compilation			|
|	directory in the source path.											|
|																			|
|	input : current compilation directory									|
|		(returned from 'info source' gdb command).							|
|																			|
|	output : none.															|
|																			|
+--------------------------------------------------------------------------*/
char cdir[MAXPATHLEN];       /* The compilation directory */

void info_source_handler(compile_dir)
char * compile_dir;
{
	if (compile_dir == NULL) {
		cdir[0] = 0;
		return;
	}

	if (strlen (compile_dir) < MAXPATHLEN) 		/* check length */
		strcpy(cdir,compile_dir);
		
	if (LASTCH(cdir) == '/')          		    /* remove last '/' */
		LASTCH(cdir) = '\0';
}



