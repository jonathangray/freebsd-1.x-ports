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

/*  parser.c:
 *
 *    Parse output messages from dbx using regular expression pattern matching,
 *    and take appropriate action.
 *
 *    compile():	Compile the regular expressions in a table.
 *    match():		Try to best match a given string with the regular
 *			expressions found in the table and return an index.
 *    parser_init():	Initialization.
 *    parse():		Parse the dbx output and invoke the appropriate action
 *			handler.
 *    filter():		Modify the dbx output before it gets displayed on the
 *			dialog window.
 *    query_dbx():	Send a query command to dbx and process it.
 */

#include	"global.h"
#include	"regex.h"
#ifdef GDB
#include	"gdb_regex.h"
#else
#ifdef BSD
#ifdef MIPS
#include	"mips_regex.h"
#else
#include	"bsd_regex.h"
#endif
#else
#include	"sun_regex.h"
#endif
#endif /* GDB */

#define BYTEWIDTH       8
#define RE_BUFFER       100

Tokens 	Token;			/* gloabl token structure */
#ifndef GDB
Boolean	Parse = True;		/* Parse flag for parse routine */
#endif

/*
 *  Compile all the regular expression patterns in a pattern table.
 *  A pattern table is an array of pattern records.
 *  Each pattern record consists of a regular
 *  expression, a buffer for the to-be-compiled regular expression,
 *  and an array to associate a given token with a matched register number.
 */
static void compile(patternTable)
PatternRec *patternTable;
{
    PatternRec	*p;
    char 	fastmap[(1 << BYTEWIDTH)];
    int		i;

    for (i=0; patternTable[i].pat; i++) {
	p = &patternTable[i];
	p->buf = (struct re_pattern_buffer *) 
	  XtMalloc (sizeof (struct re_pattern_buffer));
	p->buf->allocated = RE_BUFFER;
	p->buf->buffer = (char *) XtMalloc (p->buf->allocated);
	p->buf->fastmap = fastmap;
	p->buf->translate = NULL;
	re_compile_pattern(p->pat, strlen(p->pat), p->buf);
	re_compile_fastmap(p->buf);
    }
} 

/*
 *  This routine tries to match a given string with each regular 
 *  expression in a given pattern table.  The best match is found, and
 *  the function returns an index to the pattern table.
 */
#ifndef GDB	/* for GDB, match is called from gdb_parser.c */
static
#endif
int match(patternTable, string, type)
    PatternRec 	*patternTable;
    char 	*string;
    int		type;
{
    struct re_registers	regs;
    int			m, bestmatch = -1, index = -1, i, j, r, start, n;
    char 		*s;

    if (strcmp(string, "") == NULL) return -1;
    for (i=0; patternTable[i].pat; i++) {
	if (type != C_ANY && type != i)
	    continue;
	m = re_match(patternTable[i].buf, string, strlen(string), 0, &regs);
	if (m == -2 ) {		/* match error - failure stack overflow */
#ifdef GDB
	    fprintf(stderr, "xxgdb error: regular expression matching failed \
(failure stack overflow)\n");
#else
	    fprintf(stderr, "xdbx error: regular expression matching failed \
(failure stack overflow)\n");
#endif
	    return (-1);
	}
	if (m > bestmatch) {
	    bestmatch = m;
	    index = i;
#ifdef GDB
		/* for GDB, free memory (if not done earlier) */
		XtFree(Token.mesg);
		XtFree(Token.file);
		XtFree(Token.func);
		XtFree(Token.display);
#endif /* GDB */
	    Token.mesg = Token.file = Token.func = Token.display = NULL;
	    Token.line = Token.stop = 0;
	    for (j=0; j<NTOKENS; j++) {
		if ((r = patternTable[i].reg_token[j]) >= 0) {
		    start = regs.start[r];
		    if ((n = regs.end[r] - start) > 0) {
#ifdef GDB
		/* The following error could happen if the pattern table is not correct,
		better test it here.. */
		
			if ( n > strlen(string))	/* Something is wrong here ! */
				{
				fprintf(stderr,"Error match() : n = %d is too big\n",n);
				n = 0;
				}
#endif /* GDB */
			s = (char *) XtMalloc ((n+1) * sizeof(char));
			strncpy(s, string+start, n);
			s[n] = '\0';
		    	switch (j) {
			  case TK_MESG: Token.mesg = s; break;
			  case TK_STOP: Token.stop = atoi(s); XtFree(s); break;
			  case TK_FUNC: Token.func = s; break;
			  case TK_LINE: Token.line = atoi(s); XtFree(s); break;
			  case TK_FILE: Token.file = s; break;
			  case TK_DISP: Token.display = s; break;
		    	}
		    }
		}
	    }
	}
    }
    return index;
}

/*  Compile the regular expressions in the output and command pattern tables. */

void parser_init()
{
    compile(output_pattern);
    compile(command_pattern);
    compile(dataPattern);
}


#ifdef GDB

#include "gdb_parser.c"

#else /*>>>>>>>>>> ALL THE FOLLOWING IS NOT COMPILED FOR GDB <<<<<<<<<<<<<<<<<<<*/

/*  This routine first parses the command string.  
 *  If the command is one of run, cont, next, step, stop at, stop in, 
 *  where, up, or down, it parses the dbx output to decide what action 
 *  to take and dispatch it to one of the handlers.
 *  For other commands, the appropriate handler is called.
 *
 *  !!! This routine has to be re-entrant.
 */
void parse(output, command)
char *output;
char *command;
{
    int  command_type;
    char *output_string;

    if (debug) {
	fprintf(stderr, "parse(output = %s, command = %s)\n", output, command);
    }

    /* Make a local copy of `output' and use that instead */
    output_string = XtNewString(output);
    if (output) strcpy(output, "");

    if (!command) {
	if (match(output_pattern, output_string, O_DEBUG) != -1)
	    debug_handler(); 
	debug_init();
	return;
    }
    if (!Parse)
	return;
	
    command_type = match(command_pattern, command, C_ANY);
    switch (command_type) {
      	case C_EXEC: 
	    if (match(output_pattern, output_string, O_EXEC) != -1)
		exec_handler();
	    else if (match(output_pattern, output_string, O_DONE) != -1)
		done_handler();
	    else
		bell(0);
	    break;
	case C_STOPAT: 
	    if (match(output_pattern, output_string, O_STOPAT) != -1)
		stop_at_handler();
	    else
		bell(0);
	    break;
	case C_STOPIN: 
	    if (match(output_pattern, output_string, O_STOPIN) != -1)
		stop_in_handler();
	    else
		bell(0);
	    break;
	case C_UPDOWN:
	    if (match(output_pattern, output_string, O_UPDOWN) != -1)
		updown_handler();
	    else
		bell(0);
	    break;
	case C_SEARCH:
	    if (match(output_pattern, output_string, O_SEARCH) != -1)
		search_handler();
	    else
		bell(0);
	    break;
      	case C_DELETE:
	    delete_handler(); 
	    break;
	case C_FILE:
	    if (match(output_pattern, output_string, O_FILE) != -1)
	    	file_handler();		/* command was 'file' */
	    else
	    	LoadCurrentFile();	/* command was 'file ...' */
	    break;
	case C_LIST:
	    if (match(output_pattern, output_string, O_LIST) != -1)
	        list_handler();
	    else
		bell(0);
	    break;
	case C_FUNC:
#ifdef MIPS
	    if (match(output_pattern, output_string, O_FUNC) != -1)
#else
	    if (strcmp(output_string, "") == NULL)
#endif
	    	func_handler(); 
	    else
		bell(0);
	    break;
	case C_USE:
	    use_handler(output_string); 
	    break;
	    
#ifndef BSD
	case C_PRINT:
	    if (match(output_pattern, output_string, O_PRINT) != -1)
	    	print_handler(output_string);
	    else
		bell(0);
	    break;
	case C_DEBUG:
	    if (match(output_pattern, output_string, O_DEBUG) != -1)
		debug_handler(); 
	    else
		bell(0);
	    break;
	case C_CD:
	    if (strcmp(output_string, "") == NULL)
	    	cd_handler(); 
	    else
		bell(0);
	    break;
	case C_PWD:
	    pwd_handler(output_string);
	    break;
	case C_DISPLAY:
	    if (strcmp(output_string, "") == NULL ||
	    	match(output_pattern, output_string, O_PRINT) != -1)
	    	display_handler();
	    else
		bell(0);
	    break;
#endif
#ifdef BSD
	case C_STATUS:
	    break;
#endif

	default:
	    break;
    }
    XtFree(output_string);
}

/*  This function edits the dbx output so that unnecessary information is 
 *  not displayed on the dialog window.
 *  It filters away the some output returned by the execution commands; 
 *  output from the search commands, and the display command.
 *  On Sun dbx, it also filters away part of the output returned by the 
 *  up and down commands.
 */
void filter(string, output, command)
char *string, *output, *command;
{
    struct re_registers regs;
    char 		*p;
    int			r;
    static Boolean	deleteRest = False;
    int			command_type = -1;

    if (output == NULL || strcmp(output, "") == NULL) 
	return;


#ifdef BSD
    if (!command) {
	AppendDialogText(string);
	return;
    }
#endif

    if (command)
    	command_type = match(command_pattern, command, C_ANY);
    if (command_type == C_EXEC) {
	if (re_match(output_pattern[O_EXEC].buf, string, strlen(string), 0, 
	&regs) > 0) {
	    r = output_pattern[O_EXEC].reg_token[TK_MESG];
            for (p=string+regs.start[r]; p!=string && *(p-1) != '\n'; p--);
	    strcpy(p, "");
	    if (!Prompt)
		deleteRest = True;
	}
	else if (deleteRest) {
	    strcpy(string, "");
	    if (Prompt)
		deleteRest = False;
	}
	AppendDialogText(string);
	return;
    }

    if (Prompt) {
	char *s;

	s = XtNewString(output);
	switch (command_type) {
#ifndef BSD
	case C_UPDOWN:
	    if (match(output_pattern, s, O_UPDOWN) != -1)
		strcpy(s, Token.mesg);    
	    break;
	case C_DISPLAY:
	    if (match(output_pattern, s, O_PRINT) != -1)
		strcpy(s, "");
	    break;
#endif
#ifdef MIPS
	case C_UPDOWN:
	    if (match(output_pattern, s, O_UPDOWN) != -1)
		strcpy(s, Token.mesg);    
		strcat(s, "\n");
	    break;
	case C_FUNC:
	    if (match(output_pattern, s, O_FUNC) != -1)
		strcpy(s, "");
	    break;
#endif
	case C_SEARCH:
	    if (match(output_pattern, s, O_SEARCH) != -1)
		strcpy(s, "");
	    break;
	case C_LIST:
	    if (match(output_pattern, s, O_LIST) != -1)
		strcpy(s, "");
	    break;
	default:
	    s = XtNewString(string);		/* append 'string' only */
	    break;
	}
	AppendDialogText(s);
	XtFree(s);
    }
    else {
	switch (command_type) {
#ifndef BSD
	case C_UPDOWN:
	case C_DISPLAY:
#endif
#ifdef MIPS
	case C_UPDOWN:
	case C_FUNC:
#endif
	case C_SEARCH:
	case C_LIST:
	    break;
	default:
	    AppendDialogText(string);
	    break;
	}
    }
}
#endif /* NOT GDB */
