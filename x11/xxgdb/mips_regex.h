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
 *  Created:   	November 24, 1989
 *
 *****************************************************************************/

/*  mips_regex.h:
 *
 *  Regular expression pattern matching for dbx on DECStation.
 *
 *  The reg_token array indicates the register no. for each token type.
 *      reg_token[0] : message
 *      reg_token[1] : stop number
 *      reg_token[2] : function name
 *      reg_token[3] : line number
 *      reg_token[4] : file name
 *      reg_token[5] : display command output
 */

#define TK_MESG         0
#define TK_STOP         1
#define TK_FUNC         2
#define TK_LINE         3
#define TK_FILE         4
#define TK_DISP		5

#define O_EXEC          0
#define O_DONE          1
#define O_STOPAT        2
#define O_STOPIN        3
#define O_UPDOWN        4
#define O_FUNC          4
#define O_BELL          5
#define O_LIST          6
#define O_FILE          7
#define O_SEARCH        8
#define O_PRINT         9
#define O_DEBUG         10

#define C_ANY		-1
#define C_EXEC          0
#define C_STOPAT        1
#define C_STOPIN        2
#define C_UPDOWN        3
#define C_DELETE        4
#define C_FUNC          5
#define C_FILE          6
#define C_USE          	7
#define C_LIST         	8
#define C_SEARCH        9
#define C_PRINT		10
#define C_STATUS	11


static PatternRec output_pattern[] = {
    /* exec */
    {"\\(.*\n\\)*\\(\\(\\(\\(\[[0-9]+\\] \\)?\\(stopped at \\)?\\)\\|Bus error \
\\|Segmentation fault \\|Interrupt \\)?\\[\\([^ ]+\\):\\([0-9]+\\).*\\]\\).*\n",
     NULL,
     {2, -1, 7, 8, -1, -1}
    },
    /* done */
    {"\\(.*\n\\)*Program terminated normally\n",
     NULL,
     {-1, -1, -1, -1, -1, -1}
    },
    /* stop at */
    {"\\[\\([0-9]+\\)\\] stop at \\(\"\\([^ ]+\\)\":\\)?\\([0-9]+\\)\n",
     NULL,
     {-1, 1, -1, 4, 3, -1}
    },
    /* stop in */
    {"\\(\\[.*\\]\n\\)*\\[\\([0-9]+\\)\\] stop in \\([^ ]+\\)\n",
     NULL,
     {-1, 2, 3, -1, -1, -1}
    },
    /* up, down, func */
    {"\\(.*\n\\)*\\([^ ]+\\):[ ]*\\([0-9]+\\).*\n",
     NULL,
     {2, -1, 2, 3, -1, -1}
    },
    /* bell */
    {"\n\\(not that many levels\\|program not active\\|program is not active\\|\
cannot read register unless program is active\\|cannot dump unless program is \
active\\)\n",
     NULL,
     {-1, -1, -1, -1, -1, -1}
    },
    /* list */
    {"[ ]*\\([0-9]+\\).*\n",
     NULL,
     {-1, -1, -1, 1, -1, -1}
    },
    /* file */
    {"\\([^ ]+\\)\n",
     NULL,
     {-1, -1, -1, -1, 1, -1}
    },
    /* search */
    {"[ ]*\\([0-9]+\\).*\n",
     NULL,
     {-1, -1, -1, 1, -1, -1}
    },
    /* print */
    {"\\(.+\n\\(.*\n\\)*\\)",
     NULL,
     { 1, -1, -1, -1, -1, -1}
    },
    /* dbx init */
    {"\\(.*\n\\)*.*\\(dbx version .*\nType 'help' for help.\nreading symbolic \
information ...\n\\)\\(\nwarning: .*\n\\)?\\([^ ]+\\):[ ]*\\([0-9]+\\).*\n",
     NULL,
     {-1, -1, 4, 5, -1, -1}
    },
    NULL
};

static PatternRec command_pattern[] = {
    {"[ ]*\\(run\\|r\\|cont\\|c\\|next\\|n\\|step\\|s\\|return\\)\\( \\|\n\\)",
						NULL, {-1, -1, -1, -1, -1, -1}},
    {"[ ]*\\(stop\\|st\\)[ ]+at[ ]+[0-9]+",   	NULL, {-1, -1, -1, -1, -1, -1}},
    {"[ ]*\\(stop\\|st\\)[ ]+in[ ]+[^ ]+",      NULL, {-1, -1, -1, -1, -1, -1}},
    {"[ ]*\\(up\\|down\\)",                   	NULL, {-1, -1, -1, -1, -1, -1}},
    {"[ ]*\\(delete\\|d\\)",                  	NULL, {-1, -1, -1,  1, -1, -1}},
    {"[ ]*func[ ]*\\([^ ]+\\)[ ]*",           	NULL, {-1, -1,  1, -1, -1, -1}},
    {"[ ]*file[ ]*",           			NULL, {-1, -1, -1, -1,  1, -1}},
    {"[ ]*use[ ]*",           			NULL, {-1, -1, -1, -1,  1, -1}},
    {"[ ]*list[ ]*",           			NULL, {-1, -1, -1, -1,  1, -1}},
    {"[ ]*\\(/\\|\?\\)",                      	NULL, {-1, -1, -1, -1, -1, -1}},
    {"[ ]*\\(print\\|p\\)[ ]*[^ ]+",            NULL, {-1, -1, -1, -1, -1, -1}},
    {"[ ]*status[ ]*",         			NULL, {-1, -1, -1, -1,  1, -1}},
    NULL
};
