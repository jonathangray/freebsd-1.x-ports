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

/*  dataDpy.c:
 *
 *   Provide graphical display of C pointers and structures.
 *
 *   BuildLinePos():	Construct an array indexing the character position of 
 *			each line.
 *   PositionToLine():	Return the character position of a given line.   
 *   SelectPointer():	Action proc for double click on a pointer value,
 *   CreateDataPopup():	Create a popup to display the object pointed to by a
 *			pointer.
 *   UpdateDataPopup(): Update an unused popupshell to display data.
 *   AppendList():	Append a popup to the list.
 *   DeleteList():	Delete a popup from the list.
 *   pop_down():	pop down the popup and free storage.
 *   DestroyDataPopup():event handler for destroying a popup, call DeleteList()
 *			and pop_down() (CRL mod 25)
 *   MovePopup():	Position the popup.
 *   print_handler():	Action handler for displaying pointers and structures.
 */

#include 	"global.h"
#include 	"regex.h"
#include 	"datadpy.h"

#define		MAXLEVELS	20	/* max level of indentation */
#ifdef GDB
#define		INDENT		2	/* # of spaces for each indentation */
#else
#define		INDENT		8	/* # of spaces for each indentation */
#endif /* GDB */
#define 	EMPTY           0
#define 	UNUSED          1
#define 	USED            2
#define 	LEFT_MARGIN	10
#define 	SCROLLBAR_WIDTH	15


static DataDpyRec	**dataDpyTable;
static int		dataDpyTableSize = 0;
static DataDpyRec	*Parent = NULL;
static DataDpyList	*TopParentList = NULL;
static int		font_height, font_width;

#ifdef OBSOLETE
static void		DestroyDataPopup();
#else
/* CRL mod 25 4/12/91 GWC - changed label widget to command widget in popups */
static void		DestroyDataCallback();
#endif

/* 
 *  Build an array which gives the starting text position of each line.
 *  Very similar to the routine in source.c.
 */
static void BuildLinePos(dataDpy)
DataDpyRec *dataDpy;
{
    char *p;
    int	 line, nlines;
    int	 max=0;

    nlines = MAX(1, dataDpy->buflen/CHARS_PER_LINE);
    dataDpy->linepos = (XawTextPosition *)
                    XtMalloc ((nlines+2) * sizeof(XawTextPosition));
    p = dataDpy->buf;
    line = 0;
    dataDpy->linepos[line++] = 0;
    dataDpy->linepos[line++] = 0;
    while (*p) {
        if (*p++ == '\n') {
            if (line == nlines) {       /* buffer full, need more memory */
                dataDpy->linepos = (XawTextPosition *)XtRealloc(dataDpy->linepos,
                          (nlines + ADD_LINES) * sizeof(XawTextPosition));
                nlines += ADD_LINES;
            }
            dataDpy->linepos[line] = p - dataDpy->buf;
	    AssignMax(max, dataDpy->linepos[line] - dataDpy->linepos[line-1]);
	    line++;
        }
    }
    dataDpy->numlines = line - 2;
    dataDpy->maxLineLength = max;
    /* shrink to min size */
    dataDpy->linepos = (XawTextPosition *) XtRealloc     
                        (dataDpy->linepos, line * sizeof(XawTextPosition));
}

/*
 *  Return the line number for the specified text position.
 */
static int PositionToLine(dataDpy, pos)
DataDpyRec *dataDpy;
XawTextPosition pos;
{
    int	 line;

    if (dataDpy && pos >= 0) {
    	for (line = 1; pos >= dataDpy->linepos[line]; line++);
    	return (line-1);
    }
    else
	return (0);
}

/* ARGSUSED */
/*  
 *  Called by double click of pointer button.
 *  If the selected text is a valid pointer, this routine parses the data 
 *  output to obtain the full qualified name of the pointer, and asks
 *  dbx to print the value of the object the pointer is pointing to.
 */
static void SelectPointer(w, event, params, num_params)
    Widget w;
    XEvent *event;
    String *params;
    Cardinal *num_params;
{
    struct re_registers regs;
    XawTextPosition 	left, right;
    char		*selection, *p, *field[MAXLEVELS];
    DataDpyRec		*dataDpy;
    int			fromLine, line;
    int			i, n, nbytes, r, level, newlevel;
    char		name[LINESIZ], command[LINESIZ];

    /* Find out which data display output does the selection belong to */
    dataDpy = NULL;
    for (i=0; dataDpyTable[i]; i++)
	if ((Widget) w == (Widget) dataDpyTable[i]->dataDpyWindow) {
	    dataDpy = dataDpyTable[i];
	    Parent = dataDpy;
	    break;
	}
    if (!dataDpy) return;

    /* Get the selection and check if it's a pointer value, 0x???? */
    selection = XFetchBytes(display, &nbytes);
    if (re_match(dataPattern[D_POINTER].buf, selection, strlen(selection), 0, 0)
	< 0) {
	Parent = NULL;
	return;
    }

    /* Parse the output to get the fully qualified name of the pointer */
    XawTextGetSelectionPos(w, &left, &right);
    fromLine = PositionToLine(dataDpy, left);
    p = dataDpy->buf + dataDpy->linepos[fromLine];

#ifdef GDB	/* (PW)  we now use a new parser which should work better,
	in particular for arrays */
	{
	char *parse_gdb_print();
	char *newcommand;
	
    newcommand = parse_gdb_print(command, dataDpy->buf, dataDpy->buf + left );
    if (*newcommand)
    	{
		PopupMode = True;
    	query_gdb(newcommand, PARSE_ON | ECHO_OFF | FILTER_OFF);
    	}
    }
#else	/* not GDB */

    if (re_match(dataPattern[D_FIELD].buf, p, strlen(p), 0, &regs) >= 0) {
	r = dataPattern[D_FIELD].reg_token[TK_POINTER];
	if (strncmp(selection, p+regs.start[r], regs.end[r]-regs.start[r]))
	    return;
	r = dataPattern[D_FIELD].reg_token[TK_INDENT];
	level = regs.end[r]/INDENT;
	field[level+1] = NULL;

	r = dataPattern[D_FIELD].reg_token[TK_FIELD];
	n = regs.end[r] - regs.start[r];
	field[level] = (char *) XtMalloc ((n+1) * sizeof(char));
	strncpy(field[level], p+regs.start[r], n);
	field[level][n] = '\0';

	for (line = fromLine-1; line > 0; line--) {
	    p = dataDpy->buf + dataDpy->linepos[line];
	    if (re_match(dataPattern[D_STRUCT].buf, p, strlen(p), 0, &regs)>=0){
		r = dataPattern[D_STRUCT].reg_token[TK_INDENT];
		newlevel = regs.end[r]/INDENT;
		if (newlevel == level-1) {
		    level--;
		    r = dataPattern[D_STRUCT].reg_token[TK_FIELD];
		    n = regs.end[r] - regs.start[r];
		    field[level] = (char *) XtMalloc ((n+1) * sizeof(char));
		    strncpy(field[level], p+regs.start[r], n);
		    field[level][n] = '\0';
		}
	    }
	}
	if (*field[0] == '*' && field[1])
	    sprintf(name, "(%s)", field[0]+1);
	else
	    strcpy(name, field[0]);
	    
	for (i=1; field[i]; i++) {
	    strcat(name, ".");
	    strcat(name, field[i]);
	}
	sprintf(command, "print *(%s)\n", name);
	PopupMode = True;
#ifdef GDB
    query_gdb(command, PARSE_ON | ECHO_OFF | FILTER_OFF);
#else
	query_dbx(command);
#endif	/* GDB */
    }
#endif /* GDB */
}

    
/*
 *  Create a data display with a label.
 *  The popupshell has a form widget which consists of a label and a text
 *  widget.
 */
static void CreateDataPopup(dataDpy, label)
DataDpyRec *dataDpy;
char	   *label;
{
    Arg         args[MAXARGS];
    Cardinal    n;
    Dimension	dataDpyHeight, dataDpyWidth;
    XFontStruct	*text_font;

    static XtActionsRec datadpy_actions[] = {
        {"SelectPointer", (XtActionProc) SelectPointer},
        {NULL, NULL}
    };

    static String translations = "#override \n\
        <Btn1Down>:     SelectStart() SelectWord() SelectPointer() \n\
        <Btn1Up>:       SelectEnd() \n\
    ";

    n = 0;
    dataDpy->popupshell = XtCreatePopupShell("Data Popup", 
	transientShellWidgetClass, toplevel, args, n);

    n = 0;
    XtSetArg(args[n], XtNdefaultDistance, 0);                           n++;
    dataDpy->popup = XtCreateManagedWidget("popup", formWidgetClass,
	dataDpy->popupshell, args, n);

    /* Create the label */
    n = 0;
    XtSetArg(args[n], XtNtop, (XtArgVal) XawChainTop);                  n++;
    XtSetArg(args[n], XtNbottom, (XtArgVal) XawChainTop);               n++;
    XtSetArg(args[n], XtNright, (XtArgVal) XawChainRight);              n++;
    XtSetArg(args[n], XtNleft, (XtArgVal) XawChainLeft);               	n++;
    XtSetArg(args[n], XtNlabel, (XtArgVal) label);                     	n++;
    XtSetArg(args[n], XtNresize, (XtArgVal) False);                    	n++;
    XtSetArg(args[n], XtNjustify, (XtArgVal) XtJustifyCenter);         	n++;

#ifdef OBSOLETE
    dataDpy->label = XtCreateManagedWidget("label", labelWidgetClass, 
	dataDpy->popup, args, n);
/*	GWC says it is better to use ButtonReleaseMask instead of ButtonPressMask.*/
	XtAddEventHandler(dataDpy->label, (EventMask) ButtonPressMask, False, 
	DestroyDataPopup, dataDpy);
#else
/* CRL mod 25 4/12/91 GWC - changed label widget to command widget in
popups */
	dataDpy->label = XtCreateManagedWidget("command", commandWidgetClass, 
  		dataDpy->popup, args, n);
    XtAddCallback(dataDpy->label, XtNcallback, DestroyDataCallback, dataDpy);
#endif

    /* Create the text window */
    n = 0;
    XtSetArg(args[n], XtNfromVert, (XtArgVal) dataDpy->label);          n++;
    XtSetArg(args[n], XtNtop, (XtArgVal) XawChainTop);                  n++;
    XtSetArg(args[n], XtNbottom, (XtArgVal) XawChainBottom);            n++;
    XtSetArg(args[n], XtNright, (XtArgVal) XawChainRight);              n++;
    XtSetArg(args[n], XtNleft, (XtArgVal) XawChainLeft);               	n++;

    XtSetArg(args[n], XtNleftMargin, (XtArgVal) LEFT_MARGIN); 		n++;
    XtSetArg(args[n], XtNuseStringInPlace, (XtArgVal) True); 		n++;
    XtSetArg(args[n], XtNstring, (XtArgVal) dataDpy->buf);            	n++;
    XtSetArg(args[n], XtNlength, (XtArgVal) dataDpy->buflen);  		n++;
    XtSetArg(args[n], XtNeditType, (XtArgVal) XawtextRead);             n++;
    XtSetArg(args[n], XtNscrollHorizontal, XawtextScrollWhenNeeded);	n++;
    XtSetArg(args[n], XtNscrollVertical, XawtextScrollWhenNeeded);	n++;
    XtSetArg(args[n], XtNtranslations, XtParseTranslationTable(translations));
                                                                        n++;
    dataDpy->dataDpyWindow = XtCreateManagedWidget("dataDpyWindow", 
	asciiTextWidgetClass, dataDpy->popup, args, n);
    XtAppAddActions(app_context, datadpy_actions, XtNumber(datadpy_actions));

    /* Get the text font */
    n = 0;
    XtSetArg(args[n], XtNfont, &text_font);                  		n++;
    XtGetValues(dataDpy->dataDpyWindow, args, n);

    /* Estimate the size of the text widget, dataDpyWindow, with the number
       of lines and the maximum length of a line.  Assume fixed font width.
    */
    font_height = text_font->ascent + text_font->descent;
    font_width = text_font->max_bounds.width;
    dataDpyHeight = dataDpy->numlines * font_height + 5;
    dataDpyWidth = dataDpy->maxLineLength * font_width + LEFT_MARGIN;
    if (dataDpyHeight > app_resources.dataDpyMaxHeight)
	dataDpyWidth += SCROLLBAR_WIDTH;
	
#if 1	/*(PW)17DEC90 : bug ! */
#define 	SCROLLBAR_HEIGHT	15
    if (dataDpyWidth > app_resources.dataDpyMaxWidth)
	dataDpyHeight += SCROLLBAR_HEIGHT;
#endif

    AssignMin(dataDpyHeight, app_resources.dataDpyMaxHeight);
    AssignMin(dataDpyWidth, app_resources.dataDpyMaxWidth);
		
    n = 0;
    XtSetArg(args[n], XtNheight, (XtArgVal) dataDpyHeight); 		n++;
    XtSetArg(args[n], XtNwidth, (XtArgVal) dataDpyWidth); 		n++;
    XtSetValues(dataDpy->dataDpyWindow, args, n);

    n = 0;
    XtSetArg(args[n], XtNwidth, (XtArgVal) dataDpyWidth); 		n++;
    XtSetValues(dataDpy->label, args, n);
}

/*
 *  Instead of creating a new popupshell, this routine uses an already
 *  existing popupshell for data display.
 *  It changes the label, calculates the size of the popupshell,
 *  and sets the source of the text window to that of the new data.
 */
static void UpdateDataPopup(dataDpy, label)
DataDpyRec *dataDpy;
char	   *label;
{
    Arg args[MAXARGS];
    Cardinal n;
    Dimension	popupHeight, popupWidth, dataDpyHeight, dataDpyWidth,
		labelHeight, labelBorderWidth, dataDpyBorderWidth;

    /* Update the label */
    n = 0;
    XtSetArg(args[n], XtNlabel, (XtArgVal) label);                     	n++;
    XtSetValues(dataDpy->label, args, n);

    /* Calculate the size of popupshell */
    dataDpyHeight = dataDpy->numlines * font_height + 5;
    dataDpyWidth = dataDpy->maxLineLength * font_width + 2*10;
    
#if 1	/*(PW)18DEC90 : bug ! */
    if (dataDpyHeight > app_resources.dataDpyMaxHeight)
	dataDpyWidth += SCROLLBAR_WIDTH;
	
    if (dataDpyWidth > app_resources.dataDpyMaxWidth)
	dataDpyHeight += SCROLLBAR_HEIGHT;
#endif

    AssignMin(dataDpyHeight, app_resources.dataDpyMaxHeight);
    AssignMin(dataDpyWidth, app_resources.dataDpyMaxWidth);

    n = 0;
    XtSetArg(args[n], XtNheight, (XtArgVal) &labelHeight); 		n++;
    XtSetArg(args[n], XtNborderWidth, (XtArgVal) &labelBorderWidth); 	n++;
    XtGetValues(dataDpy->label, args, n);
    n = 0;
    XtSetArg(args[n], XtNborderWidth, (XtArgVal) &dataDpyBorderWidth); 	n++;
    XtGetValues(dataDpy->dataDpyWindow, args, n);

    popupHeight = dataDpyHeight + labelHeight + 2*labelBorderWidth +
		  2*dataDpyBorderWidth;
    popupWidth = dataDpyWidth;
    
    n = 0;
    XtSetArg(args[n], XtNheight, (XtArgVal) popupHeight); 		n++;
    XtSetArg(args[n], XtNwidth, (XtArgVal) popupWidth);			n++;
    XtSetValues(dataDpy->popupshell, args, n);

    /* Set the text source */
    n = 0;
    XtSetArg(args[n], XtNstring, (XtArgVal) dataDpy->buf);		n++;
    XtSetArg(args[n], XtNlength, (XtArgVal) dataDpy->buflen);           n++;
    XawTextSetSource(dataDpy->dataDpyWindow, 
		     XtCreateWidget("textsrc", asciiSrcObjectClass, 
				    dataDpy->dataDpyWindow, args, n),
		     0);
}

/*
 *  Append dataDpy to a DataDpyList pointed to by head.
 */
static void AppendList(head, dataDpy)
DataDpyList **head;
DataDpyRec  *dataDpy;
{
    DataDpyList	*p, *q, *r;

    p = (DataDpyList *) XtNew (DataDpyList);
    p->dataDpy = dataDpy;
    p->next = NULL;
    q = *head;
    if (!q)
	*head = p;
    else {
	while (r = q->next)
	    q = r;
	q->next = p;
    }
}

/*
 *  Removes a dataDpy from its parent's list of children.
 */
static void DeleteList(head, dataDpy)
DataDpyList **head;
DataDpyRec  *dataDpy;
{
    DataDpyList *p, *q;

    if (p = *head) {
	if (p->dataDpy == dataDpy)
	    *head = p->next;
	else {
	    for (q = p->next; q && q->dataDpy != dataDpy;) {
		p = q; 
		q = p->next;
	    }
	    if (q) p->next = q->next;
	}
    }
}
	    
/*
 *  Pop down a dataDpy and all its descendants, freeing storage and
 *  reinitializing fields.
 */
static void pop_down(dataDpy)
DataDpyRec *dataDpy;
{
    DataDpyList *p, *q;

    XtPopdown(dataDpy->popupshell);
    XtFree(dataDpy->linepos);
    XtFree(dataDpy->buf);
    dataDpy->buf = NULL;
    dataDpy->buflen = 0;
    dataDpy->linepos = NULL;
    dataDpy->state = UNUSED; 
    dataDpy->parent = NULL; 
    for (p = dataDpy->childlist; p;) {
	pop_down(p->dataDpy);
	q = p;
	p = p->next;
	XtFree(q);
    }
    dataDpy->childlist = NULL;
}

/*
 *  Invoked by a ButtonPress event on the label of a data display to
 *  pop down itself and its descendants.
 */
/* ARGSUSED */

#ifdef OBSOLETE
static void DestroyDataPopup(w, dataDpy, event)
    Widget w;
    DataDpyRec *dataDpy;
    XEvent *event;
#else
/* CRL mod 25 4/12/91 GWC - changed label widget to command widget */
static void DestroyDataCallback(w, dataDpy, call_data)
    Widget w;
    DataDpyRec *dataDpy;
    caddr_t call_data;
#endif
{
    if (!dataDpy->parent)
    	DeleteList(&TopParentList, dataDpy);
    else
    	DeleteList(&dataDpy->parent->childlist, dataDpy);
    pop_down(dataDpy);
}

/*
 *  Position the data display on the screen to reflect the parent-child
 *  relationship.
 */
static void MovePopup(dataDpy)
DataDpyRec *dataDpy;
{
    Arg         args[MAXARGS];
    Cardinal    n;
    Screen	*screen;
    int		popupHeight, popupWidth, screenHeight, screenWidth;
    Position	x, y;
    Dimension	dataDpyWidth, dataDpyHeight, dataDpyBorderWidth, 
		labelHeight, labelBorderWidth, width, height, borderWidth;
    DataDpyList	*p, *q;

    Parent = NULL;
    if (!dataDpy->parent)
	p = TopParentList;
    else
	p = dataDpy->parent->childlist;

    /*  Look for its previous sibling  */
    for (q = p->next; q && q->dataDpy != dataDpy;) {
	p = q;
	q = q->next;
    }
    /*  If a sibling exists, place the new popup right next to it  */
    if (q) {
	n = 0;
	XtSetArg(args[n], XtNwidth, (XtArgVal) &width);             n++;
	XtSetArg(args[n], XtNborderWidth, (XtArgVal) &borderWidth);	n++;
	XtGetValues(p->dataDpy->popupshell, args, n);
	XtTranslateCoords(p->dataDpy->popupshell, 0, 0, &x, &y);
	x += width;
	y -= borderWidth;
    }
    else {	/* no siblings */
	/*  this is the very first popup  */
	if (!dataDpy->parent) {
	    x = 0;
	    y = 0;
	}
	/*  place it under its parent  */
	else {
	    n = 0;
	    XtSetArg(args[n], XtNheight, (XtArgVal) &height);		n++;
	    XtGetValues(dataDpy->parent->popupshell, args, n);
	    XtTranslateCoords(dataDpy->parent->popupshell, 30, (Position)height,
		 &x, &y);
	}
    }

    /* Make sure the popup does not go outside of the screen */
    n = 0;
    XtSetArg(args[n], XtNwidth, (XtArgVal) &dataDpyWidth);             	n++;
    XtSetArg(args[n], XtNheight, (XtArgVal) &dataDpyHeight);           	n++;
    XtSetArg(args[n], XtNborderWidth, (XtArgVal) &dataDpyBorderWidth); 	n++;
    XtGetValues(dataDpy->dataDpyWindow, args, n);

    n = 0;
    XtSetArg(args[n], XtNheight, (XtArgVal) &labelHeight); 		n++;
    XtSetArg(args[n], XtNborderWidth, (XtArgVal) &labelBorderWidth); 	n++;
    XtGetValues(dataDpy->label, args, n);

    popupHeight = dataDpyHeight + labelHeight + 2*labelBorderWidth +
		  2*dataDpyBorderWidth;
    popupWidth = dataDpyWidth;

    screen = XtScreen(toplevel);
    screenHeight = XHeightOfScreen(screen);
    screenWidth = XWidthOfScreen(screen);

    if (x + popupWidth > screenWidth && y + popupHeight > screenHeight) {
	x = screenWidth - popupWidth;
	y = screenHeight - popupHeight;
    }
    else if (x + popupWidth > screenWidth)
	x = screenWidth - popupWidth;
    else if (y + popupHeight > screenHeight)
	y = screenHeight - popupHeight;

    n = 0;
    XtSetArg(args[n], XtNx, x);						n++;
    XtSetArg(args[n], XtNy, y);						n++;
    XtSetValues(dataDpy->popupshell, args, n);
}

/*
 *  Handler procedure called by parse().
 *  The main function to popup a data display.
 */
void print_handler(output)
char *output;
{
    DataDpyRec 	*dataDpy;
    int		i, j;

    if (!output) return;
    if (!PopupMode) return;
    PopupMode = False;
    XDefineCursor(display, XtWindow(toplevel), watch);
    if (Parent)
    	XDefineCursor(display, XtWindow(Parent->dataDpyWindow), watch);
    UpdateMessageWindow("Click the label to pop down the data popup",NULL);

    /* Searches the table for an unused or empty slot */
    /* (PW)17OCT91 : test i < dataDpyTableSize first (else segment violation)*/
    for (i=0; dataDpyTable && i < dataDpyTableSize && dataDpyTable[i]
    	&& dataDpyTable[i]->state == USED ; i++);
    if (i == dataDpyTableSize) {			/* Table full */
	dataDpyTableSize += ADD_SIZE;
	dataDpyTable = (DataDpyRec **) XtRealloc (dataDpyTable, 
			   dataDpyTableSize * sizeof(DataDpyRec *));
	for (j=i; j<dataDpyTableSize; j++)
	    dataDpyTable[j] = NULL;
    }

    /*  Empty slot found, allocate a data structure and initializes some
	of the fields.  */
    if (dataDpyTable[i] == NULL) {
	dataDpyTable[i] = (DataDpyRec *) XtMalloc (sizeof(DataDpyRec));
	dataDpyTable[i]->state = EMPTY;
	dataDpyTable[i]->parent = NULL;
	dataDpyTable[i]->childlist = NULL;
    }

    dataDpy = dataDpyTable[i];
    dataDpy->id = i;					/* not needed */
    dataDpy->buf = XtNewString(output);
    dataDpy->buflen = strlen(output);
    BuildLinePos(dataDpy);

    if (dataDpy->state == EMPTY)
	CreateDataPopup(dataDpy, Token.mesg);
    else if (dataDpy->state == UNUSED)
	UpdateDataPopup(dataDpy, Token.mesg);

    dataDpy->state = USED;				/* mark it used */
    if (dataDpy->parent = Parent)
    	AppendList(&Parent->childlist, dataDpy);
    else
    	AppendList(&TopParentList, dataDpy);

    MovePopup(dataDpy);
    XtPopup(dataDpy->popupshell, XtGrabNone);
    if (dataDpy->parent)
    	XUndefineCursor(display, XtWindow(dataDpy->parent->dataDpyWindow));
    XUndefineCursor(display, XtWindow(toplevel));
}


#ifdef GDB
#include <string.h>

#define GOODCHARNAME(c)							\
			(	(((c) >='a') &&  ((c) <= 'z'))	\
			||	(((c) >='A') &&  ((c) <= 'Z'))	\
			||	(((c) >='0') &&  ((c) <= '9'))	\
			||	((c) == '_')					\
			||	((c) == '$')					\
			)

static char *result;		/* start of result buffer */
static int result_index;	/* current index in result buffer */
static char *start_txt;		/* pointer 1st char of output to parse */
static char *curr_txt;		/* current pointer in output to parse */

/*--------------------------------------------------------------------------+
|																			|
|	Store a character into the buffer.										|
|																			|
|	Note that characters are added to the buffer RIGHT TO LEFT !			|
|	This is because we parse the output from right to left.					|
|																			|
|	If the result buffer is full, we set result to "".						|
|																			|
+--------------------------------------------------------------------------*/
static void add_char(c)
char c;
{
int i;

	if (result_index == 0)	/* buffer full */
		{
		*result = 0;
		return;
		}
	
	result_index--;
	*(result+result_index) = c;
}

/*--------------------------------------------------------------------------+
|																			|
|	Store a string into the buffer.											|
|																			|
+--------------------------------------------------------------------------*/
static void add_string(s)
char *s;
{
int nbchar;

	nbchar = strlen(s);
	
	/* copy number from last digit */
	
	while (nbchar > 0)
		add_char(*(s + (--nbchar)));
}

/*--------------------------------------------------------------------------+
|																			|
|	Store a number into the buffer.											|
|																			|
+--------------------------------------------------------------------------*/
static void add_num(number)
int number;
{
char tmpnum[128];

	sprintf(tmpnum,"%d",number);
	add_string(tmpnum);
}

/*--------------------------------------------------------------------------+
|																			|
|	Init buffer.															|
|																			|
|	Store a NULL character (as end of string).								|
|																			|
+--------------------------------------------------------------------------*/
static void init_result(buffer,buflen)
char *buffer;
int buflen;
{
	result = buffer;
	result_index = buflen;
	add_char(0);			/* end result by null char */
}

/*--------------------------------------------------------------------------+
|																			|
|	Store the current variable or struct name.								|
|																			|
|	input :		curr_txt points to '=' character,							|
|				start_txt points to beginning of the parse string.			|
|																			|
|	output :	curr_txt points to character before 1st character of		|
|					name.													|
|																			|
|	Note : we have to test for the beginning of the parse string,			|
|	because add_name() is called also for adding the "$n" name				|
|	of the gdb output.														|
|																			|
+--------------------------------------------------------------------------*/
static void add_name ()
{
	curr_txt--;								/* point before '=' */
	while (*curr_txt == ' ') curr_txt--;	/* skip spaces */
	
	/* loop over name */
	while ((curr_txt >= start_txt) && GOODCHARNAME(*curr_txt))
		add_char(*curr_txt--);
}

/*--------------------------------------------------------------------------+
|																			|
|	Skip all previous characters until corresponding " or ' character.		|
|																			|
|	input : curr_txt points before ' or " character							|
|																			|
|	output : curr_txt points before corresponding ' or " character.			|
|																			|
+--------------------------------------------------------------------------*/
void search_char(c)
char c;
{
	while(1)
		{
		while(c != *(curr_txt--));
		
		/* make sure there is not a '\' just before */
		
		if (*curr_txt != '\\')
			return;
		}
}

/*--------------------------------------------------------------------------+
|																			|
|	Skip all previous characters until previous corresponding '{'.			|
|	All "{...}" sequences are skip.											|
|	Return the array item number (if applicable)							|
|																			|
|	input :		curr_txt points to string.									|
|																			|
|	output :	curr_txt points to character before '{'						|
|				return number of commas										|
|																			|
+--------------------------------------------------------------------------*/
static int skip_level()
{
int nbcommas;
char c;

	nbcommas = 0;
	
	while(1)
		{
		switch (c = *(curr_txt--))
			{
			case '{' :
				return nbcommas;
			
			case ',' :
				nbcommas++;
				break;
				
			case '}' :
				skip_level();
				break;
			
			case '"' :
			case '\'' :
				search_char(c);
				break;
				
			default:
				break;
			}
		}
}

/*--------------------------------------------------------------------------+
|																			|
|	Function to parse an output of a gdb print from							|
|	a pointer (0x...) and return a command line to							|
|	print *(0x...)															|
|																			|
|	input : command line pointer (LINESIZ size),							|
|			pointer print output,											|
|			pointer 0x...													|
|																			|
|	output : command line (stored RIGHT justified in commandline)			|
|																			|
|	example																	|
|																			|
|		start = "$1 = { (struct foo *) 0x1224}"								|
|		current points to 0x1224 in start,									|
|																			|
|		commandline = "print *($1)"											|
|																			|
+--------------------------------------------------------------------------*/
char *parse_gdb_print (commandline, start, current)
char *commandline;
char *start;
char *current;
{
char *begin;

	start_txt = start;		/* in static variables */
	curr_txt = current;

	begin = strchr(start,'=');	/* find '=' in "$n =" */
	
	if (!begin)
		return;
		
	init_result(commandline,LINESIZ);

	add_string(")\n");
	
	while (begin <= curr_txt)
		{
		switch (*curr_txt)
			{
			case '=':
				add_name();
				
				/* stop now if we just parsed the '=' in "$n =" */
				if (curr_txt >= start_txt)
					{
					add_char('.');
					skip_level();
					}
				break;
			
			case ',':
			case '{':
				add_char(']');
				add_num(skip_level());
				add_char('[');
				break;
		
			default:
				curr_txt--;
			}
		}
		
	add_string("print *(");
	
	if (debug)
		fprintf(stderr,"datadpy=%s\n",result+result_index);
	
	return result+result_index;
}

#endif /* GDB */
