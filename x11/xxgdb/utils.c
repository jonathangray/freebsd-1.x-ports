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
 *****************************************************************************/

/*  utils.c
 *
 *    Contain common routines used by other functions.
 *
 *    TextGetLastPos():		Get the last insertion position of text.
 *    TextPositionToLine(): 	Return text position give a line number.
 *    LineToStopNo():		Return the stop number given a line number.
 *    DisableWindowResize():	Fix the size of a window inside vpane.
 *    bell():			Ring the bell.
 *    concat():			Concatenate two strings together
 */

#include "global.h"

XawTextPosition TextGetLastPos(w)
    Widget w;
{
    TextWidget ctx = (TextWidget) w;
    return (ctx->text.lastPos);
}

/*
 * Get the line number where the caret is.
 */
int TextPositionToLine(pos)
XawTextPosition pos;
{
    int line;

    if (displayedFile) {
	if (pos >= displayedFile->linepos[displayedFile->topline]) {
	    for (line = displayedFile->topline;
		 pos > displayedFile->linepos[line]; line++);
	    return (pos == displayedFile->linepos[line] ? line : line-1);
	}
	else {
	    for (line = 1; pos > displayedFile->linepos[line]; line++);
	    return (pos == displayedFile->linepos[line] ? line : line-1);
	}
    }
    else
        return 0;
}

/*
 *  Return the stop number associated with a given line number.
 *  Return 0 if stop number not found.
 */
int LineToStop_no(line)
int line;
{
    int i;

    for (i=1; i <= nstops; i++)
        if (stops[i].line == line && stops[i].file && displayedFile &&
            strcmp(stops[i].file, displayedFile->pathname) == NULL) {
            return i;
        }
    return 0;
}


void DisableWindowResize(w)
Widget w;
{
    Arg args[MAXARGS];
    Cardinal n;
    Dimension height;

    n = 0;
    XtSetArg(args[n], XtNheight, &height);                       n++;
    XtGetValues(w, args, n);
    XawPanedSetMinMax(w, height, height);
    XawPanedAllowResize(w, False);
}


void bell(volume)
int volume;
{
    XBell(XtDisplay(toplevel), volume);
}

/* append string s2 to end of string s1 and return the result */

char *concat(s1, s2)
char *s1, *s2;
{
    if (s2) {
        if (s1 == NULL) {
            s1 = XtMalloc((strlen(s2)+1)*sizeof(char));
            strcpy(s1, s2);
        }
        else {
            s1 = XtRealloc(s1, strlen(s1)+strlen(s2)+2);
            strcat(s1, s2);
        }
    }
#if 0	/*(PW)4DEC90 : bug ! if s2 is null, there is no reason to set s1 to 0 */
    else
        s1 = NULL;
#endif
    return (s1);
}
