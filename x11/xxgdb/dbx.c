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

/*
 *  dbx.c
 *
 *    Handle dbx command initialization file (.dbxinit) and communication 
 *    between dbx and xdbx.
 *
 *    dbx_init():	Handle .dbxinit
 *    debug_init():	
 *    read_dbx():	Read dbx output, parse and filter it before displaying
 *			onto the dialog window.
 *    write_dbx():	Send a command to dbx.
 *    query_dbx():	Send a command to dbx and process it.
 */

#include "global.h"

Boolean	Prompt;			/* True when dbx prompt arrives */
char 	*concat();
char	*dbxprompt; 
char	*xdbxprompt;

/*  Given a dbx command initialization file, this routine executes each dbx 
 *  command in the file.  It sends the command to dbx, and calls read_dbx() 
 *  directly to process output returned from dbx.
 */
 
static void dbx_init(xdbxinit)
char *xdbxinit;
{
#ifndef GDB
    FILE *fp;
    char s[LINESIZ];
#endif /* not GDB */

    if (strcmp(xdbxinit, "") == NULL)
	return;
	
#ifdef GDB

	read_source_file(xdbxinit);
	
#else /* not GDB */
    if (fp = fopen(xdbxinit, "r")) {
		while (fgets(s, LINESIZ, fp)) {
			send_command(s);
			AppendDialogText(s);
			Prompt = False;
			while (!Prompt)
				read_dbx();
		}
	close(fp);
    }
#endif /* not GDB */
}

/*
 *  This routine is called after getting the first dbx prompt.  
 *  > check the use list to create a list of directories for searching
 *    source files.
 *  > ask dbx for the source file and display it if it exists.
 *  > open the command initialization file and executed the commands;
 *    if Tstartup is true, remove the initialization file.
 */
void debug_init()
{
    static visited = False;

    if (!visited) {
	visited = True;
	dbx_init(xdbxinit);
	if (Tstartup)
	    unlink(xdbxinit);
	strcpy(xdbxinit, "");
    }
}

#ifndef GDB
/*
 *  This is a callback procedure invoked everytime when input is pending
 *  on the file descriptor to dbx.
 *  o reads all the data available on the file descriptor line by line
 *    into local variable 'string' and global variable 'output'.
 *    'output' records the entire dbx output whereas 'string' records
 *    only the data read in this invocation of read_dbx().
 *  o in Echo mode, the contents in 'string' is edited by filter()
 *    before it gets displayed on the dialog window.
 *  o once the dbx prompt is read, calls parse() to analyse the dbx output
 *    and take appropriate action.
 */
/* ARGSUSED */
void read_dbx(master, source, id)
XtPointer master;
int 	  *source;
XtInputId *id;
{
    static char *output = NULL; 	/* buffer for dbx output */
    static char *next_string = NULL;
    static char *command;
    char 	*string = NULL;
    char 	s[LINESIZ];
    Boolean 	more;
	
    more = True;
    while (more) {
	Prompt = False;
	/* keep reading until no more or until prompt arrives */
	while (more = fgets(s, LINESIZ, dbxfp) && !Prompt) {
	    if (debug)
		fprintf(stderr, "=>%s", s);
	    /* receive prompt? */
	    if (strncmp(s, dbxprompt, strlen(dbxprompt)) == NULL) {
		Prompt = True;
		/* more stuff behind prompt? */
		if (s[strlen(dbxprompt)])
		    /* remember it */
		    next_string = XtNewString(s+strlen(dbxprompt));
		/* destroy contents */
		strcpy(s, "");
	    }
	    string = concat(string, s);
	    strcpy(s, "");
	}
	output = concat(output, string);
	command = get_command();
		
	if (Echo) {
	    filter(string, output, command);
	    if (Prompt) AppendDialogText(xdbxprompt);
	}
	if (string) {
	    XtFree(string);
	    string = NULL;
	}
	if (next_string) {
	    string = concat(string, next_string);
	    XtFree(next_string);
	    next_string = NULL;
	}
	if (Prompt) {
	    parse(output, command);
	    delete_command();
	    XtFree(output);
	    output = NULL;
	}
    }
}
#endif /* not GDB */

/*  Write string s to dbx, and flush the output.  */

void write_dbx(s)
char *s;
{
	if (debug)
		fprintf(stderr, ">>%s", s);		/* (PW) see what is sent to GDB */
		
    fputs(s, dbxfp);
	fflush(dbxfp);
}

#ifndef GDB
/*  Sends a command to dbx and read the corresponding output, directly
 *  invoking the Xt input procedure, read_dbx().
 */
void query_dbx(command)
char *command;
{
    write_dbx(command);
    insert_command(command);

    Echo = False;
    Prompt = False;
    while (!Prompt)
        read_dbx();

    Parse = True;	/* Always reset Parse and Echo to True */
    Echo = True;
}
#endif
