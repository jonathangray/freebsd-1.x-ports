/*		Character grid hypertext object
**		===============================
*/

#include "LYCurses.h" /* lynx defined curses */

#include <assert.h>
#include <ctype.h>
#include "HTUtils.h"
#include "HTString.h"
#include "GridText.h"
#include "HTFont.h"
#include "HTAccess.h"
#include "tcp.h"
#include "HTParse.h"
#include "HTTP.h"

/* lynx specific defines */
#include "LYUtils.h"
#include "LYStrings.h"
#include "LYStructs.h"
#include "LYGlobalDefs.h"
#include "LYGetFile.h"

struct _HTStream {                      /* only know it as object */
    CONST HTStreamClass *       isa;
    /* ... */
};

#define TITLE_LINES  1


/*	From default style sheet:
*/
extern HTStyleSheet * styleSheet;	/* Default or overridden */

extern int display_lines; /* number of lines in display */

/*	Exports
*/ 
PUBLIC HText * HTMainText = 0;		/* Equivalent of main window */
PUBLIC HTParentAnchor * HTMainAnchor = 0;	/* Anchor for HTMainText */

PUBLIC char * HTAppName = "Lynx";      /* Application name */
PUBLIC char * HTAppVersion = LYNX_VERSION;        /* Application version */

PUBLIC int HTFormNumber = 0;
PUBLIC char * HTCurSelectGroup=NULL;  /* form select group name */
PUBLIC int HTCurSelectGroupType = F_RADIO_TYPE; /* group type */

PUBLIC char * checked_box = "(*)";
PUBLIC char * unchecked_box = "( )";

PUBLIC BOOLEAN underline_on = OFF;
PUBLIC BOOLEAN bold_on      = OFF;

typedef struct _line {
	struct _line	*next;
	struct _line	*prev;
	short unsigned	offset;		/* Implicit initial spaces */
	short unsigned	size;		/* Number of characters */
	BOOL	split_after;		/* Can we split after? */
	BOOL	bullet;			/* Do we bullet? */
	char	data[1];		/* Space for terminator at least! */
} HTLine;

#define LINE_SIZE(l) (sizeof(HTLine)+(l))	/* allow for terminator */

typedef struct _TextAnchor {
	struct _TextAnchor *	next;
	int			number;		/* For user interface */
	int			start;		/* Characters */
        int			line_pos;       /* position in text */
	int			extent;		/* Characters */
	int			line_num;       /* place in document */
	char *		        hightext;       /* the link text */
	char *		        hightext2;       /* a second line*/
        int 		    	hightext2offset; /* offset from left */
	int			link_type;	/* normal or form ? */
	FormInfo *		input_field;	/* info for form links */
	BOOL			show_anchor;    /* show the anchor? */
	HTChildAnchor *		anchor;
} TextAnchor;


/*	Notes on struct _Htext:
**	next_line is valid iff state is false.
**	top_of_screen line means the line at the top of the screen
**			or just under the title if there is one.
*/
struct _HText {
	HTParentAnchor *	node_anchor;
	HTLine * 		last_line;
	int			lines;		/* Number of them */
	int			chars;		/* Number of them */
	TextAnchor *		first_anchor;	/* Singly linked list */
	TextAnchor *		last_anchor;
	int			last_anchor_number;	/* user number */
	BOOL			source;		/* is the text source? */
/* For Internal use: */	
	HTStyle *		style;			/* Current style */
	int			display_on_the_fly;	/* Lines left */
	int			top_of_screen;		/* Line number */
	HTLine *		top_of_screen_line;	/* Top */
	HTLine *		next_line;		/* Bottom + 1 */
	int			permissible_split;	/* in last line */
	BOOL			in_line_1;		/* of paragraph */
	BOOL			stale;			/* Must refresh */

        HTStream*               target;                 /* Output stream */
        HTStreamClass           targetClass;            /* Output routines */
};

/*	Boring static variable used for moving cursor across
*/

#define UNDERSCORES(n) (&underscore_string[(MAX_LINE-1) - (n)])
PRIVATE char * underscore_string;
PUBLIC char * star_string;

PRIVATE int ctrl_chars_on_this_line=0;  /* num of ctrl chars in current line */

PRIVATE HTStyle default_style =
	{ 0,  "(Unstyled)", "",
	(HTFont)0, 1.0, HT_BLACK,		0, 0,
	0, 0, 0, HT_LEFT,		1, 0,	0, 
	NO, NO, 0, 0,			0 };	



PRIVATE HTList * loaded_texts;	/* A list of all those in memory */

/*			Creation Method
**			---------------
*/
PUBLIC HText *	HText_new ARGS1(HTParentAnchor *,anchor)
{
    HTLine * line;
    HText * self = (HText *) calloc(sizeof(*self),1);
    if (!self) return self;
    
    if (!loaded_texts) loaded_texts = HTList_new();
    HTList_addObject(loaded_texts, self);
    if (HTList_count(loaded_texts) > HTCacheSize) {
        if (TRACE) fprintf(stderr, "GridText: Freeing off cached doc.\n"); 
        HText_free((HText *)HTList_removeFirstObject(loaded_texts));
    }
    
    line = self->last_line = (HTLine *)calloc(sizeof(char),LINE_SIZE(MAX_LINE));
    if (line == NULL) outofmem(__FILE__, "HText_New");
    line->next = line->prev = line;
    line->offset = line->size = 0;
    self->lines = self->chars = 0;
    self->first_anchor = self->last_anchor = 0;
    self->style = &default_style;
    self->top_of_screen = 0;
    self->node_anchor = anchor;
    self->last_anchor_number = 0;	/* Numbering of them for references */
    self->stale = YES;

    if(HTOutputFormat == WWW_SOURCE)
        self->source = YES;
    else
        self->source = NO;
    
    HTAnchor_setDocument(anchor, (HyperDoc *)self);

    HTFormNumber = 0;  /* no forms started yet */
    HTMainText = self;
    HTMainAnchor = anchor;
    self->display_on_the_fly = 0;
    
    if (!underscore_string) { /* Make a line */
        char *p;
        underscore_string = (char *)calloc(MAX_LINE+1,1);
        if (underscore_string == NULL) outofmem(__FILE__, "HText_New");
        for (p=underscore_string; p<underscore_string+(MAX_LINE-1); p++)
            *p = '.';           /* Used for printfs later */
        underscore_string[(MAX_LINE-1)] = '\0';
    }

    if (!star_string) { /* Make a line */
        char *p;
        star_string = (char *)calloc(LINESIZE+1,1);
        if (star_string == NULL) outofmem(__FILE__, "HText_New");
        for (p=star_string; p<star_string+(LINESIZE-1); p++)
            *p = '_';           /* Used for printfs later */
        star_string[(LINESIZE-1)] = '\0';
    }

    underline_on = FALSE; /* reset */
    bold_on = FALSE;
    
    return self;
}

/*                      Creation Method 2
**                      ---------------
**
**      Stream is assumed open and left open.
*/
PUBLIC HText *  HText_new2 ARGS2(
                HTParentAnchor *,       anchor,
                HTStream*,              stream)

{
    HText * this = HText_new(anchor);

    if (stream) {
        this->target = stream;
        this->targetClass = *stream->isa;       /* copy action procedures */
    }
    return this;
}

/*	Free Entire Text
**	----------------
*/
PUBLIC void 	HText_free ARGS1(HText *,self)
{
    HTAnchor_setDocument(self->node_anchor, (HyperDoc *)0);
    
    while(YES) {		/* Free off line array */
        HTLine * l = self->last_line;
	l->next->prev = l->prev;
	l->prev->next = l->next;	/* Unlink l */
	self->last_line = l->prev;
	free(l);
	if (l == self->last_line) break;	/* empty */
    };
    
    while(self->first_anchor) {		/* Free off anchor array */
        TextAnchor * l = self->first_anchor;
	self->first_anchor = l->next;

	    /* free form fields */
	if(l->link_type == INPUT_ANCHOR) {

		/* free off option lists */
	    if(l->input_field->type == F_OPTION_LIST_TYPE) {
		OptionType *optptr=l->input_field->select_list;
		OptionType *tmp;
		while(optptr) {
		    tmp = optptr;
		    optptr = optptr->next;
		    free(tmp);
		}
	    }
	    free(l->input_field);
	}

	if(l->hightext)
	   free(l->hightext);
	if(l->hightext2)
	   free(l->hightext2);
	   
	free(l);
    }
    free(self);
}

/*		Display Methods
**		---------------
*/


/*	Output a line
**	-------------
*/
PRIVATE int display_line ARGS1(HTLine *,line)
{
      register int i,j;
      char buffer[2];

      buffer[1] = '\0';

      clrtoeol();
      /* make sure that we don't go over the COLS limit on the display! */

	/* add offset */
      for(i=0;i < line->offset && i < (LYcols-1); i++)
	 addch(' ');

	/* add data */
      for(j=0;i < (LYcols-1) && line->data[j] != '\0'; i++, j++)
	 if(line->data[j] == LY_UNDERLINE_START_CHAR) {
	     start_underline();
	     i--;
	 } else if(line->data[j] == LY_UNDERLINE_END_CHAR) {
	     stop_underline();
	     i--;
	 } else if(line->data[j] == LY_BOLD_START_CHAR) {
	     start_bold();
	     i--;
	 } else if(line->data[j] == LY_BOLD_END_CHAR) {
	     stop_bold();
	     i--;
	 } else {
	     buffer[0] = line->data[j];
	     addstr(buffer);
	 } 

     /* add the return */
     addch('\n');

     stop_underline();
     stop_bold();
     return(0);

}

/*	Output the title line
**	---------------------
*/
PRIVATE void display_title ARGS1(HText *,text)
{
    CONST char * title = HTAnchor_title(text->node_anchor);
    char percent[20], format[20];
    char *cp;

    /* there shouldn't be any \n in the title, but if there is
     * lets kill it now!
     */
    if(title && (cp = strchr(title,'\n')) != NULL)
	*cp = '\0';

    if (text->lines > (display_lines)) {
	sprintf(percent, " (p%d of %d)",
	    (text->top_of_screen/(display_lines)) + 1,
	    (text->lines-1)/(display_lines) + 1);
    } else {
	percent[0] = 0;	/* Null string */
    }

    sprintf(format, "%%%d.%ds%%s\n",	/* Generate format string */
		    (LYcols-1)-strlen(percent),
		    (LYcols-1)-strlen(percent));

    move(0,0);
    printw(format, title ? title : "" , percent);
}



/*	Output a page
**	-------------
*/
PRIVATE void display_page ARGS3(HText *,text, int,line_number, char *, target)
{
    HTLine * line;
    int i;
    char *cp;
    int last_screen = text->lines - (display_lines-2);
    TextAnchor *Anchor_ptr;
    FormInfo *FormInfo_ptr;
    BOOL display_flag=FALSE;
    HTAnchor *link_dest;

    lynx_mode = NORMAL_LYNX_MODE;
 
    if(text == NULL) {
	/* hack to fix reverse clear screen problem */
	addch('*');
	refresh();
	clear();
	addstr("\n\nError accessing document\nNo data available\n");
	refresh();
	nlinks = 0;  /* set number of links to 0 */
	return;
    }

    line = text->last_line->prev;

/*	Constrain the line number to be within the document
*/
    if (text->lines < (display_lines)) line_number = 0;
    else if (line_number>text->lines) line_number = last_screen;
    else if (line_number < 0) line_number = 0;
    
    for(i=0,  line = text->last_line->next;		/* Find line */
    	i<line_number && (line!=text->last_line);
      i++, line=line->next) /* Loop */ assert(line->next != NULL);

    text->top_of_screen = line_number;
    display_title(text);  /* will move cursor to top of screen */
    display_flag=TRUE;
    
 /*	print it
 */
    if (line) {
      for(i=0; i < (display_lines); i++)  {

        assert(line != NULL);
        display_line(line);

        /* if the target is on this line, underline it */
        if(strlen(target) > 0 &&
	    (case_sensitive ?  
	    (cp = LYno_attr_char_strstr(line->data, target)) != NULL : 
	    (cp = LYno_attr_char_case_strstr(line->data, target)) != NULL) &&
            ((cp-line->data) + line->offset + strlen(target)) < LYcols) {

	    int itmp=0;
	    int written=0;
	    int x_pos=line->offset + (cp - line->data);
	    int len = strlen(target);

	    start_underline();
		/* underline string */
	    for(; written < len && line->data[itmp] != '\0'; itmp++)  {
		if(IsSpecialAttrChar(line->data[itmp])) {
		   /* ignore special characters */
		   x_pos--;

		} else if(cp == &line->data[itmp]) {
		  /* first character of target */
            	    move(i+1, x_pos);
		    addch(line->data[itmp]);
		    written++;

		} else if(&line->data[itmp] > cp) { 
			/* print all the other target chars */
		    addch(line->data[itmp]);
		    written++;
		}
	    }

	    stop_underline();
	    move(i+2, 0);
	}

	/* stop if at the last line */
	if(line == text->last_line)  {
	    /* clr remaining lines of display */
	    for(i++; i < (display_lines); i++) {
		move(i+1,0);
		clrtoeol();
	    }
	    break;
	}

	display_flag=TRUE;
	line = line->next;
      }
    }

    text->next_line = line;	/* Line after screen */
    text->stale = NO;		/* Display is up-to-date */

    /* add the anchors to lynx structures */
    nlinks = 0;
    for(Anchor_ptr=text->first_anchor;  Anchor_ptr != NULL &&
		Anchor_ptr->line_num <= line_number+(display_lines);
					    Anchor_ptr = Anchor_ptr->next) {

	if(Anchor_ptr->line_num >= line_number &&
		Anchor_ptr->line_num < line_number+(display_lines)) {

		/* load normal hypertext anchors */
	    if(Anchor_ptr->show_anchor && Anchor_ptr->hightext && 
			strlen(Anchor_ptr->hightext)>0 && 
			Anchor_ptr->link_type == HYPERTEXT_ANCHOR) {

                links[nlinks].hightext  = Anchor_ptr->hightext;
                links[nlinks].hightext2 = Anchor_ptr->hightext2;
                links[nlinks].hightext2_offset = Anchor_ptr->hightext2offset;

                links[nlinks].anchor_number = Anchor_ptr->number;

		link_dest = HTAnchor_followMainLink(
					     (HTAnchor *)Anchor_ptr->anchor);
                if(HTAnchor_address(link_dest))
                    links[nlinks].lname = HTAnchor_address(link_dest);
                else                                               
                    links[nlinks].lname = empty_string;                              

      	        links[nlinks].lx= Anchor_ptr->line_pos;
      	        links[nlinks].ly= (Anchor_ptr->line_num+1)-line_number;
	        links[nlinks].type = WWW_LINK_TYPE;
		links[nlinks].target = empty_string;

	        nlinks++;
		display_flag = TRUE;

	    } else if(Anchor_ptr->link_type == INPUT_ANCHOR
			&& Anchor_ptr->input_field->type != F_HIDDEN_TYPE) {

		lynx_mode = FORMS_LYNX_MODE;

		FormInfo_ptr = Anchor_ptr->input_field;

                links[nlinks].anchor_number = Anchor_ptr->number;

	   	links[nlinks].form = FormInfo_ptr;
		links[nlinks].lx = Anchor_ptr->line_pos;
		links[nlinks].ly= (Anchor_ptr->line_num+1)-line_number;
		links[nlinks].type= WWW_FORM_LINK_TYPE;
		links[nlinks].target= empty_string;
		links[nlinks].lname= empty_string;

		if(FormInfo_ptr->type==F_RADIO_TYPE ||
			FormInfo_ptr->type==F_CHECKBOX_TYPE) {
		    if(FormInfo_ptr->num_value)
			links[nlinks].hightext = checked_box;
		    else
			links[nlinks].hightext = unchecked_box;

		} else if(FormInfo_ptr->type==F_PASSWORD_TYPE) {
		    links[nlinks].hightext = STARS(strlen(FormInfo_ptr->value));

		} else {  /* TEXT type */
		    links[nlinks].hightext = FormInfo_ptr->value;
		}

  		/* never a second line on form types */
		links[nlinks].hightext2 = 0;

		nlinks++;
	         /* bold the link after incrementing nlinks */
		highlight(OFF,nlinks-1);
	
		display_flag = TRUE;

	    } else { /* not showing anchor */
		if(TRACE) 
		    fprintf(stderr,"GridText: Not showing link, hightext=%s\n",
						Anchor_ptr->hightext);
	    }
	} 

	if(Anchor_ptr == text->last_anchor)
	    break;
    }


    if(!display_flag) /* nothing on the page */
	addstr("\n     Document is empty");

    refresh();

}


/*			Object Building methods
**			-----------------------
**
**	These are used by a parser to build the text in an object
*/
PUBLIC void HText_beginAppend ARGS1(HText *,text)
{
    text->permissible_split = 0;
    text->in_line_1 = YES;

}


/*	Add a new line of text
**	----------------------
**
** On entry,
**
**	split	is zero for newline function, else number of characters
**		before split.
**	text->display_on_the_fly
**		may be set to indicate direct output of the finished line.
** On exit,
**		A new line has been made, justified according to the
**		current style. Text after the split (if split nonzero)
**		is taken over onto the next line.
**
**		If display_on_the_fly is set, then it is decremented and
**		the finished line is displayed.
*/
#define new_line(text) split_line(text, 0)

PRIVATE void split_line ARGS2(HText *,text, int,split)
{
    HTStyle * style = text->style;
    int spare;
    int indent = text->in_line_1 ? text->style->indent1st
    				 : text->style->leftIndent;

/*	Make new line
*/
    HTLine * previous = text->last_line;
    HTLine * line = (HTLine *)malloc((int)LINE_SIZE(MAX_LINE));

    ctrl_chars_on_this_line = 0; /*reset since we are going to a new line*/


    if(TRACE)
	fprintf(stderr,"GridText: split_line called\n");
    
    if (line == NULL) outofmem(__FILE__, "split_line");
    text->lines++;
    
    previous->next->prev = line;
    line->prev = previous;
    line->next = previous->next;
    previous->next = line;
    text->last_line = line;
    line->size = 0;
    line->offset = 0;
    text->permissible_split = 0;  /* 12/13/93 */
    line->data[0] = '\0';

    /* add underline char if needed */
    if(underline_on) {
	line->data[0] = LY_UNDERLINE_START_CHAR;
	line->data[1] = '\0';
	line->size = 1;
    }
    /* add bold char if needed */
    if(bold_on) {
	line->data[line->size++] = LY_BOLD_START_CHAR;
	line->data[line->size] = '\0';
    }

/*	Split at required point
*/    
    if (split) {	/* Delete space at "split" splitting line */
	char * p;
	previous->data[previous->size] = 0;
	for (p = &previous->data[split]; *p; p++)
	    if (*p != ' ') break;
	strcat(line->data, p);
	line->size = strlen(line->data);
	previous->size = split;
    }
    
/*	Economise on space.
**	Not on the RS6000 due to a chaotic bug in realloc argument passing.
**	Same problem with Ultrix (4.2) : realloc() is not declared properly.
*/
#ifndef AIX
#ifndef ultrix
    while ((previous->size > 0) &&
    	(previous->data[previous->size-1] == ' '))	/* Strip trailers */
        previous->size--;
	
    previous = (HTLine *) realloc (previous, LINE_SIZE(previous->size));
    if (previous == NULL) outofmem(__FILE__, "split_line");
#endif /* ultrix */
#endif /* AIX */

    previous->prev->next = previous;	/* Link in new line */
    previous->next->prev = previous;	/* Could be same node of course */

/*	Terminate finished line for printing
*/
    previous->data[previous->size] = 0;
     
    
/*	Align left, right or center
*/

    spare =  (LYcols-1) -
    		(int)style->rightIndent + (int)style->leftIndent -
    		previous->size;	/* @@ first line indent */

    switch (style->alignment) {
	case HT_CENTER :
	    previous->offset = previous->offset + indent + spare/2;
	    break;
	case HT_RIGHT :
	    previous->offset = previous->offset + indent + spare;
	    break;
	case HT_LEFT :
	case HT_JUSTIFY :		/* Not implemented */
	default:
	    previous->offset = previous->offset + indent;
	    break;
    } /* switch */

    text->chars = text->chars + previous->size + 1;	/* 1 for the line */
    text->in_line_1 = NO;		/* unless caller sets it otherwise */
    
} /* split_line */


/*	Allow vertical blank space
**	--------------------------
*/
PRIVATE void blank_lines ARGS2(HText *,text, int,newlines)
{
    if (text->last_line->size == 0) {	/* No text on current line */
	HTLine * line = text->last_line->prev;
	while ((line!=text->last_line) && (line->size == 0)) {
	    if (newlines==0) break;
	    newlines--;		/* Don't bother: already blank */
	    line = line->prev;
	}
    } else {
	newlines++;			/* Need also to finish this line */
    }

    for(;newlines;newlines--) {
	new_line(text);
    }
    text->in_line_1 = YES;
}


/*	New paragraph in current style
**	------------------------------
** See also: setStyle.
*/

PUBLIC void HText_appendParagraph ARGS1(HText *,text)
{
    int after = text->style->spaceAfter;
    int before = text->style->spaceBefore;
    blank_lines(text, after>before ? after : before);
}


/*	Set Style
**	---------
**
**	Does not filter unnecessary style changes.
*/
PUBLIC void HText_setStyle ARGS2(HText *,text, HTStyle *,style)
{
    int after, before;

    if (!style) return;				/* Safety */
    after = text->style->spaceAfter;
    before = style->spaceBefore;
    if (TRACE) fprintf(stderr, "GridText: Change to style %s\n", style->name);

    blank_lines (text, after>before ? after : before);

    text->style = style;
}


/*	Append a character to the text object
**	-------------------------------------
*/
PUBLIC void HText_appendCharacter ARGS2(HText *,text, char,ch)
{
    HTLine * line;
    HTStyle * style;
    int indent;

    if(!text || text==NULL)
	return;

    line = text->last_line;
    style = text->style;

    indent = text->in_line_1 ? style->indent1st : style->leftIndent;
    

    if(IsSpecialAttrChar(ch)) 
        if (ch == LY_UNDERLINE_START_CHAR) { 
            line->data[line->size++] = LY_UNDERLINE_START_CHAR;
	    underline_on = ON;
	    ctrl_chars_on_this_line++;
	    return;
        } else if (ch == LY_UNDERLINE_END_CHAR) {
            line->data[line->size++] = LY_UNDERLINE_END_CHAR;
	    underline_on = OFF;
	    ctrl_chars_on_this_line++;
	    return;
        } else if (ch == LY_BOLD_START_CHAR) {
            line->data[line->size++] = LY_BOLD_START_CHAR;
            bold_on = ON;
	    ctrl_chars_on_this_line++;
            return;
        } else if (ch == LY_BOLD_END_CHAR) {
            line->data[line->size++] = LY_BOLD_END_CHAR;
            bold_on = OFF;
	    ctrl_chars_on_this_line++;
            return;
        }

/*		New Line
*/
    if (ch == '\n') {
	    new_line(text);
	    text->in_line_1 = YES;	/* First line of new paragraph */
	    return;
    }

    /* convert EM_SPACE to a space here so that it doesn't get
     * collapsed
     */
    if (ch == HT_EM_SPACE)
	ch = ' ';

    /* I'm going to cheat here in a BIG way.  Since I know that all
     * \r's will be trapped by HTML_put_character I'm going to use
     * \r to mean go down a line but don't start a new paragraph.  
     * i.e. use the second line indenting.
     */
    if (ch == '\r') {
	new_line(text);
	text->in_line_1 = NO;
	return;
    }


/* 		Tabs
*/

    if (ch == '\t') {
        HTTabStop * tab;
	int target;	/* Where to tab to */
	int here = line->size + line->offset +indent;
        if (style->tabs) {	/* Use tab table */
	    for (tab = style->tabs;
	    	tab->position <= here;
		tab++)
		if (!tab->position) {
		    new_line(text);
		    return;
		}
	    target = tab->position;
	} else if (text->in_line_1) {	/* Use 2nd indent */
	    if (here >= style->leftIndent) {
	        new_line(text); /* wrap */
		return;
	    } else {
	        target = style->leftIndent;
	    }
	} else {		/* Default tabs align with left indent mod 8 */
#ifdef DEFAULT_TABS_8
	    target = ((line->offset + line->size + 8) & (-8))
	    		+ style->leftIndent;
#else
	    new_line(text);
	    return;
#endif
	}

	if (target > (LYcols-1) - (int)style->rightIndent) {
	    new_line(text);
	    return;
	} else {
            text->permissible_split = line->size;	/* Can split here */
	    if (line->size == 0) line->offset = line->offset + target - here;
	    else for(; here<target; here++) {
                line->data[line->size++] = ' ';	/* Put character into line */
	    }
	    return;
	}
	/*NOTREACHED*/
    } /* if tab */ 

    
    if (ch==' ') {
        text->permissible_split = line->size;	/* Can split here */
    }

/*	Check for end of line
*/    
    if (((indent + line->offset + line->size) + 
	(int) style->rightIndent - ctrl_chars_on_this_line) >= (LYcols-1)) {

        if (style->wordWrap && HTOutputFormat!=WWW_SOURCE) {
	    split_line(text, text->permissible_split);
	    if (ch==' ') return;	/* Ignore space causing split */

	}  else if(HTOutputFormat==WWW_SOURCE) {
		 /* for source output we 
		  * dont want to wrap this stuff unless absolutely
		  * neccessary LJM 
		  * !
		  * If we don't wrap here we might get a segmentation fault.
		  * but let's see what happens
		  */
		if(line->size >= MAX_LINE-1)
		   new_line(text);  /* try not to linewrap */
	} else {
		/* for normal stuff like pre let's go ahead and
		 * wrap so the user can see all of the text
		 */
		new_line(text);  
	}
    }

/*	Insert normal characters
*/
    if (ch == HT_NON_BREAK_SPACE) {
        ch = ' ';
    }

    {
        HTLine * line = text->last_line;	/* May have changed */
        HTFont font = style->font;
        line->data[line->size++] =	/* Put character into line */
           font & HT_CAPITALS ? TOUPPER(ch) : ch;
        if (font & HT_DOUBLE)		/* Do again if doubled */
            HText_appendCharacter(text, HT_NON_BREAK_SPACE);
	    /* NOT a permissible split */ 
    }
}

/*		Anchor handling
**		---------------
*/
/*	Start an anchor field
*/
PUBLIC void HText_beginAnchor ARGS2(HText *,text, HTChildAnchor *,anc)
{
    char marker[16];

    TextAnchor * a = (TextAnchor *) calloc(sizeof(*a),1);
    
    if (a == NULL) outofmem(__FILE__, "HText_beginAnchor");
    a->hightext  = 0;
    a->hightext2 = 0;
    a->start = text->chars + text->last_line->size;

    a->line_pos = text->last_line->size;
    if (text->last_anchor) {
        text->last_anchor->next = a;
    } else {
        text->first_anchor = a;
    }
    a->next = 0;
    a->anchor = anc;
    a->extent = 0;
    a->link_type = HYPERTEXT_ANCHOR;
    text->last_anchor = a;
    
    if (HTAnchor_followMainLink((HTAnchor*)anc)) {
        a->number = ++(text->last_anchor_number);
    } else {
        a->number = 0;
    }

    /* if we are doing link_numbering add the link number */
    if(keypad_mode == LINKS_ARE_NUMBERED && a->number > 0) {
	sprintf(marker,"[%d]", a->number);
        HText_appendText(text, marker);
	a->start += strlen(marker);
    }
}


PUBLIC void HText_endAnchor ARGS1(HText *,text)
{
    TextAnchor * a = text->last_anchor;
    if (a->number) {	 /* If it goes somewhere */
            a->extent += text->chars + text->last_line->size - a->start;
	    a->show_anchor = YES;
    } else {
	    a->show_anchor = NO;
	    a->extent = 0;
    }
 
}


PUBLIC void HText_appendText ARGS2(HText *,text, CONST char *,str)
{
    CONST char * p;

    if(str==NULL)
	return;
    for(p=str; *p; p++) {
        HText_appendCharacter(text, *p);
    }
}


PRIVATE void remove_special_attr_chars ARGS1(char *,buf)
{
    register char *cp;

    for (cp=buf; *cp != '\0' ; cp++) {
         /* don't print underline chars */
        if(!IsSpecialAttrChar(*cp)) {
           *buf = *cp, 
           buf++;
	}
    }
    *buf = '\0';
}


PUBLIC void HText_endAppend ARGS1(HText *,text)
{
    int cur_line,cur_char;
    TextAnchor *anchor_ptr;
    HTLine *line_ptr;

    if(!text)
	return;

    new_line(text);
    
    /* get the hightext from the text by finding the char
     * position, then
     * bring the anchors in line with the text by adding the
     * text offset to each of the anchors
     */
     /* get first line */
     line_ptr = text->last_line->next;
     cur_char = line_ptr->size;;
     cur_line = 0;

        /* remove the blank lines at the end of document */
     while(text->last_line->data[0]=='\0' && text->lines > 2) {
        HTLine *next_to_the_last_line;

        if(TRACE)
            fprintf(stderr,"GridText: Removing bottom blank line: %s\n",
                                                        text->last_line->data);
    
        next_to_the_last_line = text->last_line->prev;

        /* line_ptr points to the first line */
        next_to_the_last_line->next = line_ptr;
        line_ptr->prev = next_to_the_last_line;
        free(text->last_line);
        text->last_line = next_to_the_last_line;
        text->lines--;

        if(TRACE)
            fprintf(stderr,"GridText: New bottom line: %s\n",
                                                   text->last_line->data);

     }


     if(TRACE)
	fprintf(stderr,"Gridtext: Entering HText_endAppend\n");

     for(anchor_ptr=text->first_anchor; anchor_ptr; 
				             anchor_ptr=anchor_ptr->next) {

re_parse:
	  /* find the right line */
	 for(;anchor_ptr->start >= cur_char; line_ptr = line_ptr->next, 
				    cur_char += line_ptr->size+1, cur_line++) 
		; /* null body */

	 if(anchor_ptr->start == cur_char)
	    anchor_ptr->line_pos = line_ptr->size;
	 else
	    anchor_ptr->line_pos = anchor_ptr->start-(cur_char-line_ptr->size);

	 if(anchor_ptr->line_pos < 0)
	     anchor_ptr->line_pos = 0;


	 if(TRACE)
	     fprintf(stderr,"Gridtext: Anchor found on line:%d col:%d\n",
					cur_line,anchor_ptr->line_pos);

	 /* strip off a spaces at the beginning if they exist
	  * but only on HYPERTEXT_ANCHORS
	  */
	 if(anchor_ptr->link_type == HYPERTEXT_ANCHOR)
             while(isspace(line_ptr->data[anchor_ptr->line_pos]) ||
		IsSpecialAttrChar(line_ptr->data[anchor_ptr->line_pos])) {
		    anchor_ptr->line_pos++;
		    anchor_ptr->extent--;
	     }

	 if(anchor_ptr->extent < 0)
	    anchor_ptr->extent = 0;

	if(TRACE)
	    fprintf(stderr,"anchor text: '%s'   pos: %d",line_ptr->data, 
							anchor_ptr->line_pos);

	  /* if the link begins with a end of line then start the
	   * highlighting on the next line
	   */
	 if(anchor_ptr->line_pos >= strlen(line_ptr->data))
	  {
	     anchor_ptr->start++;

	     if(TRACE)
		fprintf(stderr,"found anchor at end of line\n");
	     goto re_parse;
	  }

	if(TRACE)
	    fprintf(stderr,"anchor text: '%s'   pos: %d",line_ptr->data, 
							anchor_ptr->line_pos);

	 /* copy the link name into the data structure */
	 if(line_ptr->data 
		&& anchor_ptr->extent > 0 
		    && anchor_ptr->line_pos >= 0) {

	     StrnAllocCopy(anchor_ptr->hightext, 
		&line_ptr->data[anchor_ptr->line_pos], anchor_ptr->extent);

	 } else {
	     StrAllocCopy(anchor_ptr->hightext,""); 
	 }

	/*  If true the anchor extends over two lines */
	if(anchor_ptr->extent > strlen(anchor_ptr->hightext)) {
            HTLine *line_ptr2 = line_ptr->next;
		/* double check! */
	    if(line_ptr) {
		StrnAllocCopy(anchor_ptr->hightext2, line_ptr2->data, 
			 (anchor_ptr->extent - strlen(anchor_ptr->hightext))-1);
	        anchor_ptr->hightext2offset = line_ptr2->offset;
	 	remove_special_attr_chars(anchor_ptr->hightext2);
	    }
	 }   

	 remove_special_attr_chars(anchor_ptr->hightext);

        /* subtract any formatting characters from the x position
         * of the link
         */
        if(anchor_ptr->line_pos > 0) {
            register int offset=0, i=0;
            for(; i < anchor_ptr->line_pos; i++)
                if(IsSpecialAttrChar(line_ptr->data[i]))
                    offset++;
            anchor_ptr->line_pos -= offset;
        }

        anchor_ptr->line_pos += line_ptr->offset;  /* add the offset */
        anchor_ptr->line_num  = cur_line;


	if(TRACE)
	   fprintf(stderr,"GridText: adding link on line %d in HText_endAppend\n", cur_line);

 	 if(anchor_ptr == text->last_anchor)
	     break;
     }

}


/* 	Dump diagnostics to stderr
*/
PUBLIC void HText_dump ARGS1(HText *,text)
{
    fprintf(stderr, "HText: Dump called\n");
}
	

/*	Return the anchor associated with this node
*/
PUBLIC HTParentAnchor * HText_nodeAnchor ARGS1(HText *,text)
{
    return text->node_anchor;
}

/*				GridText specials
**				=================
*/
/*	Return the anchor with index N
**
**	The index corresponds to the number we print in the anchor.
*/
PUBLIC HTChildAnchor * HText_childNumber ARGS1(int,number)
{
    TextAnchor * a;
    for (a = HTMainText->first_anchor; a; a = a->next) {
        if (a->number == number) return(a->anchor);
    }
    return (HTChildAnchor *)0;	/* Fail */
}

/* HTGetLinkInfo returns some link info based on the number
 */
PUBLIC int HTGetLinkInfo ARGS3(int, number, char **, hightext, char **, lname)
{
    TextAnchor * a;
    HTAnchor *link_dest;

    for (a = HTMainText->first_anchor; a; a = a->next) {
        if (a->number == number) {
	    *hightext= a->hightext;
            link_dest = HTAnchor_followMainLink(
                                               (HTAnchor *)a->anchor);
            StrAllocCopy(*lname, HTAnchor_address(link_dest));
	    return(YES);
	}
    }
    return(NO);
}

/* HText_getNumOfLines returns the number of lines in the
 * current document
 */
PUBLIC int HText_getNumOfLines()
{
     return(HTMainText->lines);
}

/* HText_getTitle returns the title of the
 * current document
 */
PUBLIC char * HText_getTitle()
{
   return((char *) HTAnchor_title(HTMainText->node_anchor));
}

/* HText_pageDisplay displays a screen of text
 * starting from the line 'line_num'-1
 * this is the primary call for lynx
 */
extern char is_www_index;

PUBLIC void HText_pageDisplay ARGS2(int,line_num, char *, target)
{
    display_page(HTMainText, line_num-1, target);

    is_www_index = HTAnchor_isIndex(HTMainAnchor);
} 

PUBLIC void HText_setStale ARGS1(HText *,text)
{
    text->stale = YES;
}

PUBLIC void HText_refresh ARGS1(HText *,text)
{
    if (text->stale) display_page(text, text->top_of_screen, "");
}

PUBLIC int HText_sourceAnchors ARGS1(HText *,text)
{
    return text->last_anchor_number;
}

PUBLIC BOOL HText_canScrollUp ARGS1(HText *,text)
{
    return (text->top_of_screen != 0);
}

PUBLIC BOOL HText_canScrollDown ()
{
    HText * text = HTMainText;

    return ((text->top_of_screen + display_lines) < text->lines+1);
}

/*		Scroll actions
*/
PUBLIC void HText_scrollTop ARGS1(HText *,text)
{
    display_page(text, 0, "");
}

PUBLIC void HText_scrollDown ARGS1(HText *,text)
{
    display_page(text, text->top_of_screen + display_lines, "");
}

PUBLIC void HText_scrollUp ARGS1(HText *,text)
{
    display_page(text, text->top_of_screen - display_lines, "");
}

PUBLIC void HText_scrollBottom ARGS1(HText *,text)
{
    display_page(text, text->lines - display_lines, "");
}


/*		Browsing functions
**		==================
*/

/* Bring to front and highlight it
*/

PRIVATE int line_for_char ARGS2(HText *,text, int,char_num)
{
    int line_number =0;
    int characters = 0;
    HTLine * line = text->last_line->next;
    for(;;) {
	if (line == text->last_line) return 0;	/* Invalid */
        characters = characters + line->size + 1;
	if (characters > char_num) return line_number;
	line_number ++;
	line = line->next;
    }
}

PUBLIC BOOL HText_select ARGS1(HText *,text)
{
    if (text != HTMainText) {
        HTMainText = text;
	HTMainAnchor = text->node_anchor;
	  /* let lynx do it */
	/* display_page(text, text->top_of_screen, ""); */
    }
    return YES;
}

PUBLIC BOOL HTFindPoundSelector ARGS1(char *,selector)
{
    TextAnchor * a;

    for(a=HTMainText->first_anchor; a; a=a->next) {

        if(a->anchor && a->anchor->tag)
            if(!strcmp(a->anchor->tag, selector)) {

                 www_search_result = a->line_num+1;
                 if (TRACE) 
		    fprintf(stderr, 
		"HText: Selecting anchor [%d] at character %d, line %d\n",
                                     a->number, a->start, www_search_result);

                 return(YES);
            }
    }

    return(NO);

}

PUBLIC BOOL HText_selectAnchor ARGS2(HText *,text, HTChildAnchor *,anchor)
{
    TextAnchor * a;

/* This is done later, hence HText_select is unused in GridText.c
   Should it be the contrary ? @@@
    if (text != HTMainText) {
        HText_select(text);
    }
*/

    for(a=text->first_anchor; a; a=a->next) {
        if (a->anchor == anchor) break;
    }
    if (!a) {
        if (TRACE) fprintf(stderr, "HText: No such anchor in this text!\n");
        return NO;
    }

    if (text != HTMainText) {		/* Comment out by ??? */
        HTMainText = text;		/* Put back in by tbl 921208 */
	HTMainAnchor = text->node_anchor;
    }

    {
	 int l = line_for_char(text, a->start);
	if (TRACE) fprintf(stderr,
	    "HText: Selecting anchor [%d] at character %d, line %d\n",
	    a->number, a->start, l);

	if ( !text->stale &&
	     (l >= text->top_of_screen) &&
	     ( l < text->top_of_screen + display_lines+1))
	         return YES;

	www_search_result = l - (display_lines/3); /* put in global variable */
    }
    
    return YES;
}
 

/*		Editing functions		- NOT IMPLEMENTED
**		=================
**
**	These are called from the application. There are many more functions
**	not included here from the orginal text object.
*/

/*	Style handling:
*/
/*	Apply this style to the selection
*/
PUBLIC void HText_applyStyle ARGS2(HText *, me, HTStyle *,style)
{
    
}


/*	Update all text with changed style.
*/
PUBLIC void HText_updateStyle ARGS2(HText *, me, HTStyle *,style)
{
    
}


/*	Return style of  selection
*/
PUBLIC HTStyle * HText_selectionStyle ARGS2(
	HText *,me,
	HTStyleSheet *,sheet)
{
    return 0;
}


/*	Paste in styled text
*/
PUBLIC void HText_replaceSel ARGS3(
	HText *,me,
	CONST char *,aString, 
	HTStyle *,aStyle)
{
}


/*	Apply this style to the selection and all similarly formatted text
**	(style recovery only)
*/
PUBLIC void HTextApplyToSimilar ARGS2(HText *,me, HTStyle *,style)
{
    
}

 
/*	Select the first unstyled run.
**	(style recovery only)
*/
PUBLIC void HTextSelectUnstyled ARGS2(HText *,me, HTStyleSheet *,sheet)
{
    
}


/*	Anchor handling:
*/
PUBLIC void		HText_unlinkSelection ARGS1(HText *,me)
{
    
}

PUBLIC HTAnchor *	HText_referenceSelected ARGS1(HText *,me)
{
     return 0;   
}


PUBLIC int HText_getTopOfScreen ()
{
      HText * text = HTMainText;
      return text->top_of_screen;
}

PUBLIC int HText_getLines ARGS1(HText *,text)
{
      return text->lines;
}

PUBLIC HTAnchor *	HText_linkSelTo ARGS2(HText *,me, HTAnchor *,anchor)
{
    return 0;
}


PUBLIC int do_www_search ARGS1(document *,doc)
{
       char searchstring[256];

       *searchstring='\0';
       statusline("Enter a database search string: ");
       if(LYgetstr(searchstring, VISIBLE) < 0 || *searchstring == '\0')
	  return(NULLFILE);

       user_message(WWW_WAIT_MESSAGE, doc->address);

       if (HTSearch(searchstring, HTMainAnchor)) {
           StrAllocCopy(doc->address, HTAnchor_address(HTMainAnchor));

	   if(TRACE)
	       fprintf(stderr,"\ndo_www_search: newfile: %s\n",doc->address);

	   return(NORMAL);
	}


       return(NOT_FOUND);
}

/* print the contents of the file in HTMainText to
 * the file descripter fp.
 * if is_reply is true add ">" to the beginning of each
 * line to specify the file is a replied to message
 */
PUBLIC void print_wwwfile_to_fd ARGS2(FILE *,fp, int,is_reply)
{
      register int i;
      HTLine * line = HTMainText->last_line->next;
#ifdef VMS
      extern BOOLEAN HadVMSInterrupt;
#endif /* VMS */

      for(;; line = line->next) {

	  if(is_reply)
             fputc('>',fp);

            /* add offset */
          for(i=0;i < line->offset; i++)
             fputc(' ',fp);

            /* add data */
          for(i=0;line->data[i] != '\0'; i++)
             if(!IsSpecialAttrChar(line->data[i]))
                 fputc(line->data[i],fp);

         /* add the return */
         fputc('\n',fp);

	 if(line == HTMainText->last_line)
	    break;

#ifdef VMS
	if (HadVMSInterrupt)
	    break;
#endif /* VMS */
    }

}

PUBLIC void www_user_search ARGS2(int,start_line, char *,target)
{
    register HTLine * line = HTMainText->last_line->next;
    register int count;
    extern BOOLEAN case_sensitive;

	/* advance to the start line */
    for(count=1; count <= start_line; line=line->next, count++)
	; /* null */

    for(;;) {
	if(case_sensitive && LYno_attr_char_strstr(line->data, target)) {
	    www_search_result=count;
	    return;
	} else if(!case_sensitive && LYno_attr_char_case_strstr(line->data, target)) {
	    www_search_result=count;
	    return;
	} else if(line == HTMainText->last_line) {  /* next line */
	    break;
	} else {			/* end */
	    line = line->next;
	    count++;
	}
    }

	/* search from the beginning */
    line = HTMainText->last_line->next; /* set to first line */
    count = 1;

    for(;;) {
	    if(case_sensitive && LYno_attr_char_strstr(line->data, target)) {
	        www_search_result=count;
		return;
	    } else if(!case_sensitive && LYno_attr_char_case_strstr(line->data, target)) {
	        www_search_result=count;
		return;
	    } else if(count > start_line) {  /* next line */
    		user_message("\"%s\" could not be found in this document",target);
    		sleep(2);
	        return;			/* end */
	    } else {
	        line = line->next;
		count++;
	    }
    }


}

PUBLIC  void  user_message ARGS2(char *,message, char *,argument) 
{
    char temp[256];
    char temp_arg[120];

    if(message==NULL)
	return;

   /* restrict the argument to 120 chars so we don't overflow the temp var */
    strncpy(temp_arg, (temp_arg==NULL ? "" : argument),110);
    temp_arg[110] = '\0';
    sprintf(temp, message, temp_arg);

    statusline(temp);
   
}

/* HText_getOwner returns the owner of the
 * current document
 */
PUBLIC char * HText_getOwner()
{
   return((char *)HTAnchor_owner(HTMainText->node_anchor));
}

PUBLIC void HTuncache_current_document()
{
    /* should remove current document from memory */
    HTList_removeObject(loaded_texts, HTMainText);
    HText_free(HTMainText);
    HTMainText=0;
}

PUBLIC int HTisDocumentSource()
{
   return(HTMainText->source);
}

PUBLIC char * HTLoadedDocumentURL()
{
   if(!HTMainText)
	return ("");

   if(HTMainText->node_anchor && HTMainText->node_anchor->address) 
       	return(HTMainText->node_anchor->address);
   else
	return ("");
}

/*  Form methods
 *    These routines are used to build forms consisting
 *    of input fields 
 */

PRIVATE int HTFormMethod;
PRIVATE char * HTFormAction;

PUBLIC void HText_beginForm ARGS2(char *,action, char *,method)
{
    HTFormMethod = URL_GET_METHOD;
    HTFormNumber++;

    if(action!=NULL)
        StrAllocCopy(HTFormAction, action);
    else
	StrAllocCopy(HTFormAction, HTLoadedDocumentURL());
    
    if(method!=NULL)
	if(!strcasecomp(method,"post"))
	   HTFormMethod = URL_POST_METHOD;

    if(TRACE)
	fprintf(stderr,"BeginForm: action:%s 	Method:%d\n",HTFormAction,
								HTFormMethod);
}

PUBLIC void HText_endForm()
{
    free(HTFormAction);
    HTFormAction = 0;
}

PUBLIC void HText_beginSelect ARGS2(char *,name, BOOLEAN,multiple)
{
   /* save the group name */
   HTCurSelectGroup = name;

   /* if multiple then all options are actually checkboxes */
   if(multiple)
      HTCurSelectGroupType = F_CHECKBOX_TYPE;
	/* if not multiple then all options are radio buttons */
   else
      HTCurSelectGroupType = F_RADIO_TYPE;

   if(TRACE)
       fprintf(stderr,"HText_beginSelect: name=%s type=%d\n",
		(HTCurSelectGroup==NULL ? "<NULL>" : HTCurSelectGroup),
		 HTCurSelectGroupType);
} 

/* we couln't set the value field for the previous option
 * tag so we have to do it now.  Assume that the last anchor
 * was the previous options tag
 */
PUBLIC char * HText_setLastOptionValue ARGS4(HText *,text, char *,value, 
						  int, order, BOOLEAN, checked)
{
   char *cp;
   static char * selectedOptionValue=0;
   int number=0;

   if(text->last_anchor->link_type != INPUT_ANCHOR)
	return NULL;

   if(TRACE)
	fprintf(stderr,"Entering HText_setLastOptionValue: value:%s, checked:%s\n", value, checked ? "on" : "off");

   /* strip return */
   if((cp=strchr(value,'\n'))!=NULL)
	*cp = '\0';

   /* strip end spaces */
   cp = &value[strlen(value)-1];
   while(isspace(*cp)) cp--;
   *(cp+1) = '\0';

   /* find first non space */
   cp = value;
   while(isspace(*cp)) cp++;

   if(HTCurSelectGroupType == F_CHECKBOX_TYPE) {
       StrAllocCopy(text->last_anchor->input_field->value, cp);

       /* put the text on the screen as well */
       HText_appendText(text, cp);

   } else {
	/* create a linked list of option values */

	OptionType * op_ptr = text->last_anchor->input_field->select_list;
	OptionType * new_ptr=0;
	BOOLEAN first_option = FALSE;

	if(!op_ptr) {  /* no option items yet */
	    new_ptr = text->last_anchor->input_field->select_list = 
				(OptionType *) malloc(sizeof(OptionType));

	    first_option = TRUE;
	} else {
	    while(op_ptr->next) {
		number++;
		op_ptr=op_ptr->next;
	    }
	    number++;  /* add one more */

	    op_ptr->next = new_ptr = (OptionType *) malloc(sizeof(OptionType));
	}

	new_ptr->name = 0;
	new_ptr->next = 0;
	StrAllocCopy(new_ptr->name, cp);

	if(first_option) {
	    StrAllocCopy(selectedOptionValue, cp);
	    text->last_anchor->input_field->num_value = 0;
	    text->last_anchor->input_field->value = 
			text->last_anchor->input_field->select_list->name;
	    text->last_anchor->input_field->orig_value = 
			text->last_anchor->input_field->select_list->name;
	} else {
	    int newlen = strlen(cp);
	    int curlen = strlen(selectedOptionValue);
		/* make the selected Option Value as long as the longest
		 * option
		 */
	    if(newlen > curlen)
		StrAllocCat(selectedOptionValue, UNDERSCORES(newlen-curlen));
	}

	if(checked) {
	    int curlen = strlen(new_ptr->name);
	    int newlen = strlen(selectedOptionValue);
		/* set the default option as this one */
	    text->last_anchor->input_field->num_value = number;
	    text->last_anchor->input_field->value = new_ptr->name;
	    text->last_anchor->input_field->orig_value = new_ptr->name;
	    StrAllocCopy(selectedOptionValue, new_ptr->name);
	    if(newlen > curlen)
		StrAllocCat(selectedOptionValue, UNDERSCORES(newlen-curlen));
	}
	     

	/* return the selected Option value to be sent to the screen */
	if(order == LAST_ORDER) {
		/* change the value */
	    text->last_anchor->input_field->size = strlen(selectedOptionValue); 
	    return(selectedOptionValue);
	} else 
	   return(NULL);
   }
	
   if(TRACE)
	fprintf(stderr,"HText_setLastOptionValue: value=%s\n", value);

   return(NULL);
}

/*
 *  Assign a form input anchor
 *  returns the number of charactors to leave blank
 *  so that the input field can fit
 */
PUBLIC int HText_beginInput ARGS2(HText *,text, InputFieldData *,I)
{
	
    TextAnchor * a = (TextAnchor *) calloc(sizeof(*a),1);
    FormInfo * f = (FormInfo *) calloc(sizeof(*f),1);   

    if(TRACE)
	fprintf(stderr,"Entering HText_beginInput\n");

    if (a == NULL || f == NULL) outofmem(__FILE__, "HText_beginInput");

    a->start = text->chars + text->last_line->size;
    a->line_pos = text->last_line->size;

    if (text->last_anchor) {
        text->last_anchor->next = a;
    } else {
        text->first_anchor = a;
    }
    a->next = 0;
    a->anchor = NULL;
    a->link_type = INPUT_ANCHOR;
    a->show_anchor = YES;

    a->hightext = NULL;
    a->extent = 2;

    a->input_field = f;

    f->select_list = 0;
    f->number = HTFormNumber;

    /* special case of option */
	/* set the values and let the parsing below do the work */
    if(I->type!=NULL && !strcmp(I->type,"OPTION")) {

 	if(HTCurSelectGroupType==F_RADIO_TYPE)
	    I->type = "OPTION_LIST";
	else
	    I->type = "CHECKBOX";
	I->name = HTCurSelectGroup;
    }

	/* set SIZE */
    if(I->size != NULL) {
	f->size = atoi(I->size);
	if(f->size == 0)
	   f->size = 20;  /* default */
    } else {
	f->size = 20;  /* default */
    }

	/* set MAXLENGTH */
    if(I->maxlength != NULL) {
	f->maxlength = atoi(I->maxlength);

    } else {
	f->maxlength = 0;  /* 0 means infinite */
    }

	/* set CHECKED */
    /* num_value is only relevent to check and radio types */
    if(I->checked == TRUE)
 	f->num_value = 1; 
    else
 	f->num_value = 0;

	/* set TYPE */
    if(I->type != NULL) {
	if(!strcasecomp(I->type,"password")) {
	    f->type = F_PASSWORD_TYPE;
	} else if(!strcasecomp(I->type,"checkbox")) {
	    f->type = F_CHECKBOX_TYPE;
	} else if(!strcasecomp(I->type,"radio")) {
	    f->type = F_RADIO_TYPE;
	} else if(!strcasecomp(I->type,"submit")) {
	    f->type = F_SUBMIT_TYPE;
	} else if(!strcasecomp(I->type,"image")) {
	    f->type = F_SUBMIT_TYPE;
	} else if(!strcasecomp(I->type,"reset")) {
	    f->type = F_RESET_TYPE;
	} else if(!strcasecomp(I->type,"OPTION_LIST")) {
	    f->type = F_OPTION_LIST_TYPE;
	} else if(!strcasecomp(I->type,"hidden")) {
	   f->type = F_HIDDEN_TYPE;
	   f->size=0;
	} else if(!strcasecomp(I->type,"textarea")) {
	   f->type = F_TEXTAREA_TYPE;
	} else {
	    f->type = F_TEXT_TYPE; /* default */
	}
    } else {
	f->type = F_TEXT_TYPE;
    }


	/* set NAME */
    if(I->name!=NULL) {
        StrAllocCopy(f->name,I->name);

    } else {
	if(f->type==F_RESET_TYPE || f->type==F_SUBMIT_TYPE) {
		/* set name to empty string */
	    StrAllocCopy(f->name,"");
	} else {
		/* error! name must be present */
	    if(TRACE)
		fprintf(stderr,"GridText: No name present in input field; not displaying\n");
	    free(a);
	    free(f);
	    return(0);
	}
    }

	/* set VALUE */
    /* set the value if it exists */
    if(I->value != NULL)
	StrAllocCopy(f->value, I->value);
    else
	StrAllocCopy(f->value, "");

	/* run checks and fill in neccessary values */
    if(f->type==F_RESET_TYPE) {
	if(I->value!=NULL) {
	    f->size = strlen(I->value);
	} else {
	    StrAllocCopy(f->value, "Reset");
	    f->size = 5;
	}
    } else if(f->type==F_SUBMIT_TYPE) {
	if(I->value!=NULL) {
	    f->size = strlen(I->value);
	} else {
	    StrAllocCopy(f->value, "Submit");
	    f->size = 6;
	}
	f->submit_action = NULL;
	StrAllocCopy(f->submit_action, HTFormAction);
	f->submit_method = HTFormMethod;

    } else if(f->type==F_RADIO_TYPE || f->type==F_CHECKBOX_TYPE ) {
	f->size=3;
	if(I->value == NULL)
	   StrAllocCopy(f->value,"on");

    } 

    
    /* set original values */
    if(f->type==F_RADIO_TYPE || f->type==F_CHECKBOX_TYPE ) {
	if(f->num_value)
            StrAllocCopy(f->orig_value, "1");
	else
            StrAllocCopy(f->orig_value, "0");
    } else if(f->type==F_OPTION_LIST_TYPE) {
	f->orig_value=0;
    } else {
        StrAllocCopy(f->orig_value, f->value);
    }

    /* restrict size to maximum allowable size */
    if(f->size > LYcols-10)
	f->size = LYcols-10;  /* maximum */

    /* add this anchor to the anchor list */
    text->last_anchor = a;

    if(TRACE)
	fprintf(stderr,"Input link: name=%s\nvalue=%s\nsize=%d\n",
		 	f->name,(f->value!=NULL ? f->value : ""),f->size);
	
	/* return the size of the input field */
    return(f->size);
}


PUBLIC void HText_SubmitForm ARGS2(FormInfo *,submit_item, document *,doc)
{
   TextAnchor * anchor_ptr = HTMainText->first_anchor;
   int form_number = submit_item->number;
   FormInfo * form_ptr;
   int len;
   char *query=0;
   char *escaped1=NULL, *escaped2=NULL;
   int first_one=1;
   char * last_textarea_name=0;

   if(submit_item->submit_action)
        len = strlen(submit_item->submit_action)+64; /* plus breathing room */
   else
	return;

   /* go through list of anchors and get size first */
   while(1) {
        if(anchor_ptr->link_type == INPUT_ANCHOR) {
   	    if(anchor_ptr->input_field->number == form_number) {

	        form_ptr = anchor_ptr->input_field;
	
	        len += strlen(form_ptr->name)+10;
	        len += strlen(form_ptr->value)+10;
	        len += 32; /* plus and ampersand + safty net */

	    } else if(anchor_ptr->input_field->number > form_number) {
	        break;
	    }
	}

	if(anchor_ptr == HTMainText->last_anchor)
	    break;

	anchor_ptr = anchor_ptr->next;
   }

   /* get query ready */
   query = (char *)calloc (sizeof(char), len);

   if(submit_item->submit_method == URL_GET_METHOD) {
       	strcpy (query, submit_item->submit_action);
       	/* Clip out anchor. */
       	strtok (query, "#");
       	/* Clip out old query. */
       	strtok (query, "?");  
	strcat(query,"?");  /* add the question mark */
   } else {
	StrAllocCopy(doc->post_content_type, 
					"application/x-www-form-urlencoded");
        query[0] = '\0';
   }

   /* reset anchor->ptr */
   anchor_ptr = HTMainText->first_anchor;
   /* go through list of anchors and assemble URL query */
   while(1) {
        if(anchor_ptr->link_type == INPUT_ANCHOR) {
	    if(anchor_ptr->input_field->number == form_number) {

                form_ptr = anchor_ptr->input_field;

                switch(form_ptr->type) {

	        case F_RESET_TYPE:
	        case F_SUBMIT_TYPE:
		    break;

	        case F_RADIO_TYPE:
                case F_CHECKBOX_TYPE:
		    /* only add if selected */
		    if(form_ptr->num_value) {
	                if(first_one)
		            first_one=FALSE;
	                else
		            strcat(query,"&");

		        escaped1 = HTEscape(form_ptr->name,URL_XPALPHAS);
		        escaped2 = HTEscape(form_ptr->value,URL_XPALPHAS);
                        sprintf(&query[strlen(query)], "%s=%s",
					        escaped1, escaped2);
		        free(escaped1);
		        free(escaped2);
		    }
		    break;
		
		case F_TEXTAREA_TYPE:

                    escaped2 = HTEscape(form_ptr->value,URL_XPALPHAS);

		    if(!last_textarea_name || 
			  strcmp(last_textarea_name, form_ptr->name))
		      {
			/* names are different so this is the first
			 * textarea or a different one from any before
			 * it.
			 */
		        if(first_one)
                            first_one=FALSE;
                        else
                            strcat(query,"&");
                        escaped1 = HTEscape(form_ptr->name,URL_XPALPHAS);
                        sprintf(&query[strlen(query)], "%s=%s",
                                            escaped1, escaped2);
                        free(escaped1);
			last_textarea_name = form_ptr->name;
		      }
		    else
		      {
			/* this is a continuation of a previous textarea
			 * add %0a (\n) and the escaped string
			 */
			if(escaped2[0] != '\0')
			    sprintf(&query[strlen(query)], "%%0a%s", escaped2);
		      }
                    free(escaped2);
                    break;

                case F_PASSWORD_TYPE:
	        case F_TEXT_TYPE:
		case F_OPTION_LIST_TYPE:
		case F_HIDDEN_TYPE:
	            if(first_one)
		        first_one=FALSE;
	            else
		        strcat(query,"&");
    
		    escaped1 = HTEscape(form_ptr->name,URL_XPALPHAS);
		    escaped2 = HTEscape(form_ptr->value,URL_XPALPHAS);
                    sprintf(&query[strlen(query)], "%s=%s",
					    escaped1, escaped2);
		    free(escaped1);
		    free(escaped2);
		    break;
	        }
	    } else if(anchor_ptr->input_field->number > form_number) {
	        break;
	    }
        }

        if(anchor_ptr == HTMainText->last_anchor)
            break;

	anchor_ptr = anchor_ptr->next;
   }

   statusline("submitting form");
   
   if(submit_item->submit_method == URL_POST_METHOD) {
       doc->post_data = query;
       if(TRACE)
	    fprintf(stderr,"GridText - post_data: %s\n",query);
       StrAllocCopy(doc->address, submit_item->submit_action);
       return;
   } else { /* GET_METHOD */ 
       StrAllocCopy(doc->address, query);
       free_and_clear(&doc->post_data);
       free_and_clear(&doc->post_content_type);
       return;
   }

}

PUBLIC void HText_ResetForm ARGS1(FormInfo *,form)
{
    TextAnchor * anchor_ptr = HTMainText->first_anchor;

    statusline("resetting form...");

   /* go through list of anchors and reset values */
   while(1) {
        if(anchor_ptr->link_type == INPUT_ANCHOR) {
            if(anchor_ptr->input_field->number == form->number) {

                 if(anchor_ptr->input_field->type == F_RADIO_TYPE ||
                      anchor_ptr->input_field->type == F_CHECKBOX_TYPE) {

		    if(anchor_ptr->input_field->orig_value[0]=='0')
		        anchor_ptr->input_field->num_value = 0;
		    else
		        anchor_ptr->input_field->num_value = 1;
		
		 } else if(anchor_ptr->input_field->type==F_OPTION_LIST_TYPE) {
		    anchor_ptr->input_field->value =
				anchor_ptr->input_field->orig_value;

	         } else {
		    StrAllocCopy(anchor_ptr->input_field->value,
					anchor_ptr->input_field->orig_value);
		 }
	     } else if(anchor_ptr->input_field->number > form->number) {
                 break;
	     }

        }

        if(anchor_ptr == HTMainText->last_anchor)
            break;


        anchor_ptr = anchor_ptr->next;
   }


}

PUBLIC void HText_activateRadioButton ARGS1(FormInfo *,form)
{
    TextAnchor * anchor_ptr = HTMainText->first_anchor;
    int form_number = form->number;

    while(1) {
        if(anchor_ptr->link_type == INPUT_ANCHOR &&
                anchor_ptr->input_field->type == F_RADIO_TYPE) {
                    
	    if(anchor_ptr->input_field->number == form_number) {

		    /* if it has the same name and its on */
	         if(!strcmp(anchor_ptr->input_field->name, form->name) &&
		    			anchor_ptr->input_field->num_value) {
		    anchor_ptr->input_field->num_value = 0;
		    break;
	         }
	    } else if(anchor_ptr->input_field->number > form_number) {
	            break;
	    }

        }

        if(anchor_ptr == HTMainText->last_anchor)
            break;

        anchor_ptr = anchor_ptr->next;
   }

   form->num_value = 1;
}

