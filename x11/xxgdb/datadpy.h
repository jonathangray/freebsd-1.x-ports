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

/*  datadpy.h:
 *
 *  Regular expression pattern matching for C structures
 *
 *  The reg_token array indicates the register no. for each token type.
 *      reg_token[0] : level of indentation
 *      reg_token[2] : field name
 *      reg_token[4] : pointer string
 */

#define TK_INDENT       0
#define TK_FIELD        2
#define TK_POINTER      4

#define D_POINTER	0
#define D_FIELD		1
#define D_STRUCT	2

#ifdef GDB	/* >>>>>>>>>>>>  GDB ONLY <<<<<<<<<<<<<<<<<< */
/*
	Note : for GDB the 'set prettyprint on' must be ON.
	
	Exaamples "
	
		$3 = (struct toto *) 0x40c0
		
		$2 = {
		  pt = 0x40b4,
		  u = 5,
		  v = 6
		}
*/

PatternRec dataPattern[] = {
    {"0x[0-9a-f]+", 				  
     NULL, {-1, -1, -1, -1, -1, -1}
    },
    {"\\([ ]*\\)\\(.*[^ ]+\\)[ ]* = \\((.*) \\)?\\(0x[0-9a-f]+\\)[,]?[ ]*\n", 
     NULL, { 1, -1,  2, -1,  4, -1}
    },
    {"\\([ ]*\\)\\(.*[^ ]*\\)[ ]* = {\n", 		  
     NULL, { 1, -1,  2, -1, -1, -1}
    },
    NULL
};

#else		/* >>>>>>>>>>>>  IF NOT GDB <<<<<<<<<<<<<<<<<< */

PatternRec dataPattern[] = {
    {"0x[0-9a-f]+", 				  
     NULL, {-1, -1, -1, -1, -1, -1}
    },
    {"\\([ ]*\\)\\(.*[^ ]+\\)[ ]* = \\(0x[0-9a-f]+\\)\n", 
     NULL, { 1, -1,  2, -1,  3, -1}
    },
    {"\\([ ]*\\)\\(.*[^ ]*\\)[ ]* = {\n", 		  
     NULL, { 1, -1,  2, -1, -1, -1}
    },
    NULL
};
#endif /* NOT GDB */
