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
 *  gdb_regex.h:
 *
 *  Regular expression pattern matching for GNU gdb.
 *  
 *  The reg_token array indicates the register no. for each token type.
 *      reg_token[0] : message
 *      reg_token[1] : stop number
 *      reg_token[2] : function name
 *      reg_token[3] : line number
 *      reg_token[4] : file name
 *      reg_token[5] : display command output
 */

#define	TK_MESG 	0
#define TK_STOP		1
#define TK_FUNC 	2
#define TK_LINE 	3
#define TK_FILE 	4
#define TK_DISP 	5

#define	O_EXEC_MESS_AFTER	0
#define	O_EXEC_MESS_BEFORE	1
#define	O_EXEC				2
#define O_DONE				3
#define O_BREAK 			4			/* Instead of O_STOPAT & O_STOPIN */
#define O_INFO_DIR  		5			/* NEW for GDB */
#define O_FRAME_CURR		6			/* O_FRAME_CURR = O_UPDOWN */
#define O_UPDOWN 			6
#define O_BELL				7
#define O_SEARCH			8
#define O_INFO_LINE			9			/* Instead of O_FILE */
#define O_PRINT				10
#define O_DEBUG				11
#define O_DIRECTORY			12			
#define O_LIST				13			/* for dbx, O_LIST = O_SEARCH */
#define O_DISPLAY			14
#define O_DISPLAY_INFO		15
#define O_PWD				16
#define O_CD				16			/* O_PWD = O_CD */
#define O_RECEIVED_SIGNAL	17			/* test program received signal */
#define O_EXEC_DISPLAY		18			/* special for filter_display_info  */
#define O_READING_SYMBOLS	19			/* special for filter_reading_symbols */
#define O_CORE_FILE			20
#define O_UNDEF_SHOW		21			/* test for undefined show command (for gdb 4.0) */
#define O_INFO_SOURCE		22			/* to get compilation directory (for gdb 4.0) */


#define	C_ANY			-1
#define C_EXEC			0
#define C_BREAK			1			/* Instead of C_STOPAT & C_STOPIN */
#define C_INFO_DIR		2			/* NEW for GDB */
#define C_FRAME_CURR	3
#define C_UPDOWN		4
#define C_DELETE		5
#define C_FINISH		6			/* Instead of C_FUNC */
#define C_INFO_LINE		7			/* Instead of C_FILE */
#define C_SYMBOL_FILE	8
#define C_CD			9
#define C_DIRECTORY		10			/* Instead of C_USE */
#define C_PWD			11
#define C_LIST			12
#define C_SEARCH		13
#define C_DISPLAY_INFO	14			/* must be BEFORE C_DISPLAY ! */
#define C_DISPLAY		15
#define C_UNDISPLAY		16
#define C_PRINT			17
#define C_INFO_BREAK	18
#define C_SOURCE		19
#define C_EXEC_FILE		20
#define C_CORE_FILE		21
#define C_DEFINE		22
#define C_DOCUMENT		23
#define C_END			24
#define C_INFO_SOURCE	25			/* new for gdb 4.0 to get compilation directory */
#define C_FILE			26			/* new for gdb 4.0 : file command */

/*--------------------------------------------------------------------------+
|																			|
|								DBX											|
|																			|
| Reading symbolic information...											|
| Read 46 symbols															|
| (dbx)																		|
| (dbx)																		|
| stopped in main at line 5 in file "pw.c"									|
|	 5			 for (i=0; i<100; i++)										|
| (dbx) use																	|
| /usr1/gnu_sun4/xdbx/														|
| (dbx) file																|
| pw.c																		|
| (dbx) file																|
| No current source file													|
|																			|
+--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------+
|																			|
|								GDB											|
|																			|
| Reading symbol data from /usr1/gnu_sun4/xdbx/pw...done.					|
| Type "help" for a list of commands.										|
| (gdb)																		|
| (gdb)																		|
| Bpt 1, main () (pw.c line 5)												|
| 5				  for (i=0; i<100; i++)										|
| (gdb) info directories													|
| Source directories searched: /usr1/gnu_sun4/xdbx:/usr1/toto				|
| (gdb) info line															|
| Line 10 of "pw.c" starts at pc 0x22dc and ends at 0x22e4.					|
| (gdb) info line															|
| No source file specified.													|
|																			|
+--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------+
|																			|
|								GDB -fullname								|
|																			|
|																			|
|	We use option -fullname (see dbxoptions() in xdbx.c).					|
|																			|
| GDB prints (see source.c of gdb):											|
|																			|
|		"\032\032%s:%d:%d:%s:0x%x\n", s->fullname,							|
|		line, s->line_charpos[line - 1],									|
|		mid_statement ? "middle" : "beg",									|
|		get_frame_pc (get_current_frame()));								|
|																			|
| (gdb) break main															|
| Reading in symbols for pw.c...done.										|
| Breakpoint 1 at 0x229c: file pw.c, line 5.								|
| (gdb) run																	|
| Starting program: /usr1/gnu_sun4/xdbx/pw									|
|																			|
| Bpt 1, main () (pw.c line 5)												|
| /usr1/gnu_sun4/xdbx/pw.c:5:17:beg:0x229c									|
| (gdb) step																|
| /usr1/gnu_sun4/xdbx/pw.c:6:40:beg:0x22b0									|
| (gdb)																		|
|																			|
| (gdb) info directories													|
| Source directories searched: /usr1/gnu_sun4/xdbx							|
| (gdb) info line															|
| Line 10 of "pw.c" starts at pc 0x22dc and ends at 0x22e4.					|
| (gdb) info line															|
| No source file specified.													|
| (gdb) info line															|
| Line number 34 is out of range for "bug1.c".								|
|																			|
| (gdb) display i															|
| 3: i = 0																	|
| (gdb) display																|
| 4: i + 1 = 1																|
| 3: i = 0																	|
| (gdb) undisplay 2															|
| (gdb) display																|
| (gdb) 																	|
|																			|
| (gdb) up																	|
| #2  0x2314 in main () (pw.c line 35)										|
| /usr1/gnu_sun4/xdbx/pw.c:35:158:beg:0x2360								|
| (gdb) up																	|
| Initial frame selected; you cannot go up.									|
| (gdb)																		|
|																			|
| (gdb) down																|
| #0  glop (number=1) (pw2.c line 5)										|
| /usr1/gnu_sun4/xdbx/pw2.c:5:33:beg:0x2360									|
| (gdb) down																|
| Bottom (i.e., innermost) frame selected; you cannot go down.				|
| (gdb)																		|
|																			|
| (gdb) pwd																	|
| Working directory /usr1/gnu_sun4/xdbx.									|
| (gdb) cd ..																|
| Working directory /usr1/gnu_sun4.											|
| (gdb) cd xdbx																|
| Working directory /usr1/gnu_sun4/xdbx.									|
| (gdb) cd toto																|
| toto: No such file or directory.											|
| (gdb)																		|
|																			|
| Program exited with code 01.												|
| (gdb)																		|
|																			|
+--------------------------------------------------------------------------*/
/*

		.		--> any character but '\n'
		*		--> any character 0 to n times
		+		--> any character 1 to n times
		?		--> any character 0 or 1 time
		^		--> begin of line or NOT following character
		$		--> end of line or '$'
		\\w		--> character '0..9a..zA...Z'
		\\W		--> character NOT '0..9a..zA...Z'
		\\<		--> word begin (word is composed of 0..9a..zA...Z)
		\\>		--> word end (word is composed of 0..9a..zA...Z)
		\\b		--> word bound
		\\B		--> not word bound
		\\|		--> means OR
		\\`		--> begin buffer
		\\\'	--> end buffer
		
		
>>	WARNING : be carefull with \\| (OR) : the re_match() function
>>	will NOT correctly update the string for a pattern which is
>>	optional !
>>	--> The number SHOULD NOT point to a patterm which is optional ! 

*/

    /* (PW)5DEC90: I have a problem if we say \\(Bpt.*\n\\)? in exec pattern.
    	That is why I have two cases : with or without Bpt...
    	Note that 'exec without Message' includes 'exec with Message'.
    	We display 'Bpt...' in the message window. */
    	
    /* (PW)14JAN91 (v1.01) : use 
   		\\(\\([0-9]+:.*\n\\(\\( .*\n\\)*}\n\\)?\\)*\\)
    instead of
    	\\(\\([0-9]+:.*\n\\)*\\)
    for display pattern.
    */
    

static PatternRec output_pattern[] = {

#ifndef NeXT  /* if standard GDB */
    /* exec with Message after */
    {"\\(.*\n\\)*\032\032\\([^ \n]+\\):\\([0-9]+\\):\\([0-9]+\\):\\([^ \n]+\\):0x.+\n\\([ ]*[^0-9\n].*\n\\)\\(\\([0-9]+:.*\n\\(\\( .*\n\\)*[ }]*}\n\\)?\\)*\\)",
     NULL,
     {6, -1, -1, 3, 2, 7}
    },
    /* exec with Message before */
    {"\\(.*\n\\)*\\([ ]*[^ \n].*\n\\)\032\032\\([^ \n]+\\):\\([0-9]+\\):\\([0-9]+\\):\\([^ \n]+\\):0x.+\n\\(\\([0-9]+:.*\n\\(\\( .*\n\\)*[ }]*}\n\\)?\\)*\\)",
     NULL,
     {2, -1, -1, 4, 3, 7}	
    },
    /* exec without Message */
    {"\\(.*\n\\)*\032\032\\(\\([^ \n]+\\):\\([0-9]+\\):\\([0-9]+\\):\\([^ \n]+\\):0x.+\n\\)\\(\\([0-9]+:.*\n\\(\\( .*\n\\)*[ }]*}\n\\)?\\)*\\)",
     NULL,
     {2, -1, -1, 4, 3, 7}
    },
    
#else  /* NeXT is a special case : same as standard except that ":0x.+" are removed. */
    /* exec with Message after */
    {"\\(.*\n\\)*\032\032\\([^ \n]+\\):\\([0-9]+\\):\\([0-9]+\\):\\([^ \n]+\\)\n\\([ ]*[^0-9\n].*\n\\)\\(\\([0-9]+:.*\n\\(\\( .*\n\\)*[ }]*}\n\\)?\\)*\\)",
     NULL,
     {6, -1, -1, 3, 2, 7}
    },
    /* exec with Message before */
    {"\\(.*\n\\)*\\([ ]*[^ \n].*\n\\)\032\032\\([^ \n]+\\):\\([0-9]+\\):\\([0-9]+\\):\\([^ \n]+\\)\n\\(\\([0-9]+:.*\n\\(\\( .*\n\\)*[ }]*}\n\\)?\\)*\\)",
     NULL,
     {2, -1, -1, 4, 3, 7}	
    },
    /* exec without Message */
    {"\\(.*\n\\)*\032\032\\(\\([^ \n]+\\):\\([0-9]+\\):\\([0-9]+\\):\\([^ \n]+\\)\n\\)\\(\\([0-9]+:.*\n\\(\\( .*\n\\)*[ }]*}\n\\)?\\)*\\)",
     NULL,
     {2, -1, -1, 4, 3, 7}
    },
#endif /* NeXT */

    /* done */
    {"\\(.*\n\\)*\n\\(Program terminated with signal \\|Program exited with code 0\\|Program exited normally\\).*\n\\(.*\n\\)*",
     NULL,
     {-1, -1, -1, -1, -1, -1}
    },
    /* break */
    {"\\(.*\n\\)*Breakpoint \\([0-9]+\\) at [^ \n]+: file \\([^ \n]+\\), line \\([0-9]+\\).\n",
     NULL,
     {-1, 2, -1, 4, 3, -1}
    },
    /* info directories */
    {"Source directories searched:[ ]*\\([^ \n]+\\)[ ]*\n",
     NULL,
     { -1, -1, -1, -1, 1, -1}
    },
    
    
#ifndef NeXT  /* if standard GDB */
    /* up, down */
    {"\\(.*\n\\)*\\(#[0-9]+[ ]+\\(0x[^ \n]+[ ]+in[ ]+\\)?\\([^ \n]+\\)[^\032]*\\)\032\032\\([^ \n]+\\):\\([0-9]+\\):\\([0-9]+\\):\\([^ \n]+\\):0x.+\n",
     NULL,
     {2, -1, 4, 6, 5, -1}	
    },
#else  /* NeXT is a special case : same as standard except that ":0x.+" are removed. */
    /* up, down */
    {"\\(.*\n\\)*\\(#[0-9]+[ ]+\\(0x[^ \n]+[ ]+in[ ]+\\)?\\([^ \n]+\\)[^\032]*\\)\032\032\\([^ \n]+\\):\\([0-9]+\\):\\([0-9]+\\):\\([^ \n]+\\)\n",
     NULL,
     {2, -1, 4, 6, 5, -1}	
    },
#endif /* NeXT */


    /* bell */
    {"\\(Undefined command: .*\n\\|Already at the \\(top\\|bottom\\) call level\n\\|\
No active stack frames\n\\|no program to run\n\\|no process to run\n\\|\
program is not active\n\\|can't continue execution\n\\|\
.*\ncan't write to process.*\n\\|\
\\(Top\\|End\\)-of-file; did not find search string:.*\n\\)",
     NULL,
     {-1, -1, -1, -1, -1}
    },
    /* search */
    {"\\([0-9]+\\).*\n",
     NULL,
     {-1, -1, -1, 1, -1, -1}
    },
    /* info line */
    {"\\(\\(Line \\(number \\)?[0-9]+ \\(of\\|is out of range for\\) \"\\([^ \n]+\\)\".*\\)\\|\\(No symbol table is loaded.*\\)\\|\\(No source file specified.*\\)\\)\n",
     NULL,
     {-1, -1, -1, -1, 5, -1}
    },
    /* print */
    {"\\(warning: .*\n\\)?\\(\\(.*\\) = .*\n\\(.*\n\\)*\\)",
     NULL,
     { 3, -1, -1, -1, -1, 2}
    },
    /* start gdb (debug)  */
    {"\\(.*\n\\)*Type \"help\" for a list of commands.\n",
     NULL,
     { -1, -1, -1, -1, -1, -1}
    },
    /* directory */
    {"\\([^ \n]+ is already in the source path.[ ]*\n\\)*Source directories searched:[ ]*\\([^ \n]+\\)[ ]*\n",
     NULL,
     { -1, -1, -1, -1, 1, -1}
    },
    /* list : 22MAY91 put message in Token.mesg (edit again 30MAY91) */
    {"\\(\\([^0-9\n]+.*\n\\)*\\)\\(.*\n\\)*\\(\\([0-9]+\\).*\n\\)",
     NULL,
     { 1, -1, -1, 5, -1, -1}
    },
/*    {"\\(\\(.*\n\\)*\\)\\(\\([0-9]+\\).*\n\\)",
     NULL,
     { 1, -1, -1, 4, -1, -1}
    },
*/
    /* display */
    {"\\([0-9]+:.*\n\\)",
     NULL,
     { -1, -1, -1, -1, -1, 1}
    },
    /* info display */
    {"\\(\\([0-9]+:.*\n\\(\\( .*\n\\)*[ }]*}\n\\)?\\)*\\)\\(\\(.*\n\\)*\\)",
     NULL,
     {  5, -1, -1, -1, -1, 1}
    },
/*    {"\\(\\([0-9]+:.*\n\\(\\( .*\n\\)*}\n\\)?\\)*\\)",
     NULL,
     { -1, -1, -1, -1, -1, 1}
    },
*/
    /* pwd or cd */
    {"Working directory[ ]+\\([^ \n]+\\).[ ]*\n\\([ ]*(canonically[ ]+\\([^ \n]+\\)).\n\\)?",
     NULL,
     { 1, -1, -1, -1, -1, 3}
    },
    /* program received signal */
    {"\\(.*\n\\)*\n\\(Program received signal \\([-]?[0-9]+\\), [^ \n]+.*\n\\)\\(.*\n\\)*",
	NULL,
     {  2, 3, -1, -1, -1, -1}
    },
    /* special for test in filter_display_info() */
    {"\\([^0-9\n].*\n\\)*\\([0-9]+:.*\n\\)\\(.*\n\\)*",
     NULL,
     { -1, -1, -1, -1, -1, 2}
    },
    /* special for test in filter_reading_symbols() */
    {"\\(.*\n\\)*\\(Reading in symbols for .*done.\n\\)\\(.*\n\\)*",
     NULL,
     { 2, -1, -1, -1, -1, -1}
    },
    /* core-file */
    {"\\(.*\n\\)*\\(Program terminated with signal \\([-]?[0-9]+\\), [^ \n]+.*\n\\)\\(.*\n\\)*",
	NULL,
     {  2, 3, -1, -1, -1, -1}
    },
    /* Undefined show command (for test gdb 4.0) */
    {"\\(Undefined command: \"show\".\n\\)",
     NULL,
     {-1, -1, -1, -1, -1}
    },
    /* info source (for gdb 4.0) */
    {"\\(.*\n\\)*Compilation directory is \\([^ ]+\\)\n\\(.*\n\\)*",
     NULL,
     { 2, -1, -1, -1, -1, -1}
    },
    NULL 
};

/*

	To simplify the patterns, I used for example :
	
		"f[rame]*" instead of "frame\\|fram\\|fra||fr".
		
	This will cause 'frmeame' to be accepted. But this is
	no problem because 'Undefined command' output is parsed
	before parsing the command line (see parse() in parser.c).
*/

static PatternRec command_pattern[] = {
	/* run -r - cont - c - next - n - step - s - nexti - ni - stepi - si - return
	jump - until - u */
	/* CRL mod 22 4/5/91 GWC - added attach to this list for gdbvx */
	/* (PW) - added target to this list for gdb 4.0 */
	
    {"[ ]*\\(run\\|r\\|cont\\|c\\|next\\|n\\|step\\|s\\|nexti\\|stepi\\|ni\\|si\\|ret[urn]*\\|j[ump]*\\|unt[il]*\\|u\\|at[tach]*\\|ta[rget]*\\)\\( .*\\)?\n",
											NULL, {-1, -1, -1, -1, -1, -1}},
	/* break - tbreak */
    {"[ ]*\\(t\\)?\\(b\\|br\\|bre\\|brea\\|break\\)\\( .*\\)?\n",
    										NULL, {-1, -1, -1, -1, -1, -1}},
    /* (PW)28AUG91 : add show for gdb 4.0 */
    {"[ ]*\\(info\\|show\\)[ ]+directories[ ]*\n",		NULL, {-1, -1, -1, -1, -1, -1}},
    
    /* 'frame' is special case of 'frame n' since it does not change the
    current frame. Else 'frame n' is like up or down. */
	/* CRL mod 23 4/5/91 GWC - changed fr[ame] to f[rame] */
    {"[ ]*f[rame]*[ ]*\n",					NULL, {-1, -1, -1, -1, -1, -1}},
    {"[ ]*\\(up\\|down\\|dow\\|do\\|f[rame]*\\)\\( .*\\)?\n", 
											NULL, {-1, -1, -1, -1, -1, -1}},
											
	/* delete - d - clear - enable - disable - dis - disa */
	/* gdb commands 'delete display' 'enable display' and 'delete environment'
	are also found here. This is superfluous, but no problem */
	
    {"[ ]*\\(del[ete]*\\|d\\|cl[ear]*\\|en[able]*\\|disab[le]*\\|dis\\|disa\\)\\( .*\\)?\n",
    										NULL, {-1, -1, -1, -1, -1, -1}},
    										
    /* because of gdb 4.0 use fin[ish] instead of fi[nish] */				
	{"[ ]*fin[ish]*[ ]*\n",					NULL, {-1, -1, -1, -1, -1, -1}},
    {"[ ]*info[ ]+line[ ]*\n",				NULL, {-1, -1, -1, -1, -1, -1}},
    
    /* symbol-file */
    {"[ ]*sy.*\n", 							NULL, {-1, -1, -1, -1, -1, -1}},
    {"[ ]*cd[ ]*[^ \n]+[ ]*\n", 			NULL, {-1, -1, -1, -1, -1, -1}},
    
    /* directory */
	{"[ ]*dir.*\n",							NULL, {-1, -1, -1, -1, -1, -1}},
	
	{"[ ]*pwd[ ]*\n",						NULL, {-1, -1, -1, -1, -1, -1}},
	/* list */
	{"[ ]*l.*\n", 							NULL, {-1, -1, -1, -1, -1, -1}},
	
	/* forward-search or reverse-search or search */
	
	{"[ ]*\\(fo[rward-search]*\\|rev[erse-search]*\\|sea[rch]*\\)[ ]*",
											NULL, {-1, -1, -1, -1, -1, -1}},
    
    /* 'display' is a special case of 'display exp' since it does not
    add any expression to be displayed */
	{"[ ]*disp[lay]*[ ]*\n",							NULL, {-1, -1, -1, -1, -1, -1}},
	{"[ ]*disp[lay]\\(/[^ \n]+\\)?*[ ]*[^ \n]+[ ]*.*\n",	NULL, {-1, -1, -1, -1, -1, -1}},
	
	/* undisplay */
	{"[ ]*und.*\n",							NULL, {-1, -1, -1, -1, -1, -1}},
	
	/* Note that Token.mesg (if any) is updated with print command (see 1) */
	
    {"[ ]*print[ ]*\\([^ \n]?\\([ ]+[^ \n]+\\)*\\)[ ]*\n",	NULL, { 1, -1, -1, -1, -1, -1}},
    {"[ ]*info[ ]+break[ ]*\n",				NULL, {-1, -1, -1, -1, -1, -1}},
    
    /* source (note that Token.TK_FILE is updated here) */
	{"[ ]*so[urce]*[ ]*\\(.*\\)\n", 		NULL, {-1, -1, -1, -1,  1, -1}},
	
    /* exec-file (just used internally) */
    {"[ ]*ex[ec-file]*.*\n", 				NULL, {-1, -1, -1, -1, -1, -1}},

    /* core-file */
    {"[ ]*cor[e-file]*.*\n", 				NULL, {-1, -1, -1, -1, -1, -1}},

    /* define */
    {"[ ]*def[ine]*[ ]+[^ \n]+\n", 			NULL, {-1, -1, -1, -1, -1, -1}},
    /* document */
    {"[ ]*doc[ument]*[ ]+[^ \n]+\n", 		NULL, {-1, -1, -1, -1, -1, -1}},
    /* end of define or document */
    {"[ ]*end[ ]*.*", 						NULL, {-1, -1, -1, -1, -1, -1}},

    /* info source */
    {"[ ]*info[ ]+source[ ]*\n",			NULL, {-1, -1, -1, -1, -1, -1}},

    /* file */
    {"[ ]*file .*\n", 						NULL, {-1, -1, -1, -1, -1, -1}},
    NULL
};
