/*		Structured stream to Rich hypertext converter
**		============================================
**
**	This generates of a hypertext object.  It converts from the
**	structured stream interface fro HTMl events into the style-
**	oriented iunterface of the HText.h interface.  This module is
**	only used in clients and shouldnot be linked into servers.
**
**	Override this module if making a new GUI browser.
**
**   Being Overidden
**
*/
#include "HTML.h"

/* #define CAREFUL		 Check nesting here notreally necessary */

#include <ctype.h>
#include <stdio.h>

#include "HTAtom.h"
#include "HTChunk.h"
#include "HText.h"
#include "HTStyle.h"

#include "HTAlert.h"
#include "HTMLGen.h"
#include "HTParse.h"

#include "HTNestedLists.h"
#include "HTForms.h"

#include "GridText.h"

#ifdef VMS
#include <curses.h>
#endif
/* from Curses.h */
extern int LYcols;

extern HTStyleSheet * styleSheet;	/* Application-wide */

/*	Module-wide style cache
*/
PRIVATE int 		got_styles = 0;
PRIVATE HTStyle *styles[HTML_ELEMENTS+25];  /* adding 24 nested list styles */
PRIVATE HTStyle *default_style;
PRIVATE char HTML_Last_Char='\0'; /* the last character put on the screen */
PRIVATE char *textarea_name=0;

/* used for nested lists */
PRIVATE int List_Nesting_Level= -1;  /* counter for list nesting level */

/*		HTML Object
**		-----------
*/
#define MAX_NESTING 30		/* Should be checked by parser */

typedef struct _stack_element {
        HTStyle *	style;
	int		tag_number;
} stack_element;

struct _HTStructured {
    CONST HTStructuredClass * 	isa;
    HTParentAnchor * 		node_anchor;
    HText * 			text;

    HTStream*			target;			/* Output stream */
    HTStreamClass		targetClass;		/* Output routines */

    HTChunk 			title;		/* Grow by 128 */
    HTChunk			option;		/* Grow by 128 */
    HTChunk			textarea;	/* Grow by 128 */
    
    char *			comment_start;	/* for literate programming */
    char *			comment_end;

    HTTag *			current_tag;
    BOOL			style_change;
    HTStyle *			new_style;
    HTStyle *			old_style;
    BOOL			in_word;  /* Have just had a non-white char */
    stack_element 	stack[MAX_NESTING];
    stack_element 	*sp;		/* Style stack pointer */
};

struct _HTStream {
    CONST HTStreamClass *	isa;
    /* .... */
};

/*		Forward declarations of routines
*/
PRIVATE void get_styles NOPARAMS;


PRIVATE void actually_set_style PARAMS((HTStructured * me));
PRIVATE void change_paragraph_style PARAMS((HTStructured * me, HTStyle * style));

/*	Style buffering avoids dummy paragraph begin/ends.
*/
#define UPDATE_STYLE if (me->style_change) { actually_set_style(me); }

/* include the character sets 
 * There may be an unlimited number of sets
 * The sets will be referenced using the
 * LYCharSets array
 */
#include "LYCharSets.c"

/*		Set character set
**		----------------
*/

PRIVATE char** p_entity_values = ISO_Latin1;	/* Pointer to translation */

PUBLIC void HTMLUseCharacterSet ARGS1(int,i)
{
     p_entity_values = LYCharSets[i];
}


/*		Flattening the style structure
**		------------------------------
**
On the NeXT, and on any read-only browser, it is simpler for the text to have
a sequence of styles, rather than a nested tree of styles. In this
case we have to flatten the structure as it arrives from SGML tags into
a sequence of styles.
*/

/*		If style really needs to be set, call this
*/
PRIVATE void actually_set_style ARGS1(HTStructured *, me)
{
    if (!me->text) {			/* First time through */
	    me->text = HText_new2(me->node_anchor, me->target);
	    HText_beginAppend(me->text);
	    HText_setStyle(me->text, me->new_style);
	    me->in_word = NO;
    } else {
	    HText_setStyle(me->text, me->new_style);
    }

    me->old_style = me->new_style;
    me->style_change = NO;

}

/*      If you THINK you need to change style, call this
*/

PRIVATE void change_paragraph_style ARGS2(HTStructured *, me, HTStyle *,style)
{
    if (me->new_style!=style) {
    	me->style_change = YES;
	me->new_style = style;
    }
    me->in_word = NO;
}

/*_________________________________________________________________________
**
**			A C T I O N 	R O U T I N E S
*/

/*	Character handling
**	------------------
*/
PRIVATE void HTML_put_character ARGS2(HTStructured *, me, char, c)
{

    switch (me->sp[0].tag_number) {
    case HTML_COMMENT:
    	break;					/* Do Nothing */
	
    case HTML_TITLE:	
	if(c != '\n' && c != '\t')
    	    HTChunkPutc(&me->title, c);
	else
    	    HTChunkPutc(&me->title, ' ');
	break;

    case HTML_SELECT:	
    	HTChunkPutc(&me->option, c);
	break;

    case HTML_TEXTAREA:	
    	HTChunkPutc(&me->textarea, c);
	break;
	
    case HTML_LISTING:				/* Litteral text */
    case HTML_XMP:
    case HTML_PLAINTEXT:
    case HTML_PRE:
/*	We guarrantee that the style is up-to-date in begin_litteral
	But we still want to strip \r's
*/
	if(c != '\r') 
	    HText_appendCharacter(me->text, c);
	break;
	
    default:					/* Free format text */
	if(!strcmp(me->sp->style->name,"Preformatted") ||
		!strcmp(me->sp->style->name,"Listing") ||
		!strcmp(me->sp->style->name,"Example")) {

	    if(c != '\r') 
	        HText_appendCharacter(me->text, c);
	
	} else {

	    if (me->style_change) {
	        if ((c=='\n') || (c==' ')) return;	/* Ignore it */
	        UPDATE_STYLE;
	    }
	    if (c=='\n') {
	        if (me->in_word) {
	            if(HTML_Last_Char != ' ')
		        HText_appendCharacter(me->text, ' ');
		    me->in_word = NO;
	        }

	    } else if (c==' ' || c=='\t') {
	        if(HTML_Last_Char != ' ') 
	            HText_appendCharacter(me->text, ' ');

	    } else if (c=='\r') {
	       /* ignore */
	    } else {
	        HText_appendCharacter(me->text, c);
	        me->in_word = YES;
	    }
	}
    } /* end switch */

    if(c=='\n' || c=='\t') {
     	HTML_Last_Char=' '; /* set it to a generic seperater */

	/* \r's are ignored.  In order to keep collapsing spaces
	 * correctly we must default back to the previous
	 * seperater if there was one
	 */
    } else if(c=='\r' && HTML_Last_Char == ' ') {
     	    HTML_Last_Char=' '; /* set it to a generic seperater */
    } else {
     	HTML_Last_Char = c;
    }
}



/*	String handling
**	---------------
**
**	This is written separately from put_character becuase the loop can
**	in some cases be promoted to a higher function call level for speed.
*/
PRIVATE void HTML_put_string ARGS2(HTStructured *, me, CONST char*, s)
{
   if(s==NULL)
      return;

    switch (me->sp[0].tag_number) {
    case HTML_COMMENT:
    	break;					/* Do Nothing */
	
    case HTML_TITLE:	
    	HTChunkPuts(&me->title, s);
	break;

    case HTML_SELECT:	
    	HTChunkPuts(&me->option, s);
	break;
	
    case HTML_LISTING:				/* Litteral text */
    case HTML_XMP:
    case HTML_PLAINTEXT:
    case HTML_PRE:

/*	We guarrantee that the style is up-to-date in begin_litteral
*/
    	HText_appendText(me->text, s);
	break;
	
    default:					/* Free format text */
        {
	    CONST char *p = s;
	    if (me->style_change) {
		/* Ignore leaders */
		for (; *p && ((*p=='\n') || (*p==' ') || (*p=='\t')); p++);  
		if (!*p) return;
		UPDATE_STYLE;
	    }
	    for(; *p; p++) {
		if (me->style_change) {
		    if ((*p=='\n') || (*p==' ') || (*p=='\t')) 
			continue;  /* Ignore it */
		    UPDATE_STYLE;
		}
		if (*p=='\n') {
		    if (me->in_word) {
		        if(HTML_Last_Char!=' ')
			    HText_appendCharacter(me->text, ' ');
			me->in_word = NO;
		    }

		} else if (*p==' ' || *p=='\t') {
		   if(HTML_Last_Char!=' ')
			HText_appendCharacter(me->text, ' ');
			
		} else if (*p=='\r') {
			/* ignore */
		} else {
		    HText_appendCharacter(me->text, *p);
		    me->in_word = YES;
		}

		/* set the Last Character */
    		if(*p=='\n' || *p=='\t') {
        	    HTML_Last_Char=' '; /* set it to a generic seperater */

        		/* \r's are ignored.  In order to keep 
			 * collapsing spaces
         		 * correctly we must default back to the previous
         		 * seperater if there was one
         		 */
    		} else if(*p=='\r' && HTML_Last_Char == ' ') {
       		    HTML_Last_Char=' '; /* set it to a generic seperater */
    		} else {
       		    HTML_Last_Char = *p;
    		}

	    } /* for */
	}
    } /* end switch */
}


/*	Buffer write
**	------------
*/
PRIVATE void HTML_write ARGS3(HTStructured *, me, CONST char*, s, int, l)
{
    CONST char* p;
    CONST char* e = s+l;
    for (p=s; s<e; p++) HTML_put_character(me, *p);
}


/*	Start Element
**	-------------
*/

extern BOOLEAN HT_Is_Gopher_URL;

PRIVATE void HTML_start_element ARGS4(
	HTStructured *, 	me,
	int,		element_number,
	CONST BOOL*,	 	present,
	CONST char **,	value)
{
    static int OL_Counter[7];  /* counter for ordered lists */
    static BOOLEAN first_option=TRUE;  /* is this the first option tag? */

    switch (element_number) {
    case HTML_A:
	{
	    HTChildAnchor * source;
	    char * href = NULL;
	    if (present[HTML_A_HREF]) {
	    	StrAllocCopy(href, value[HTML_A_HREF]);
		if(!HT_Is_Gopher_URL)  /* dont simplify gopher url's */
		    HTSimplify(href);
		else
		    HT_Is_Gopher_URL= FALSE;
	    }
	    source = HTAnchor_findChildAndLink(
		me->node_anchor,				/* parent */
		present[HTML_A_NAME] ? value[HTML_A_NAME] : 0,	/* Tag */
		present[HTML_A_HREF] ? href : 0,		/* Addresss */
		present[HTML_A_TYPE] && value[HTML_A_TYPE] ? 
			(HTLinkType*)HTAtom_for(value[HTML_A_TYPE])
			    			: 0);
	    
	    if (present[HTML_A_TITLE] && value[HTML_A_TITLE]) {
	        HTParentAnchor * dest = 
		    HTAnchor_parent(
			HTAnchor_followMainLink((HTAnchor*)source)
				    );
		if (!HTAnchor_title(dest))
			HTAnchor_setTitle(dest, value[HTML_A_TITLE]);
	    }
	    UPDATE_STYLE;
	    HText_beginAnchor(me->text, source);
	}
    	break;

    case HTML_FORM:
	{
	    char * action = NULL;
	    char * method = NULL;
	    HTChildAnchor * source;
	    HTAnchor *link_dest;

	    if (present[HTML_FORM_ACTION])  {
	    	StrAllocCopy(action, value[HTML_FORM_ACTION]);
		HTSimplify(action);
	        source = HTAnchor_findChildAndLink(me->node_anchor, 
								0, action, 0);
		link_dest = HTAnchor_followMainLink(source);
                if(HTAnchor_address(link_dest))
                    StrAllocCopy(action, HTAnchor_address(link_dest));
                else
                    StrAllocCopy(action, "");
	    }
	    if (present[HTML_FORM_METHOD]) 
	    	StrAllocCopy(method, value[HTML_FORM_METHOD]);
	    
	    HText_beginForm(action, method);

	    free(action);
	    free(method);
	}
	break;

    case HTML_INPUT:
	{
	    InputFieldData I;
	    int chars;

	    /* init */
	    I.name=NULL; I.type=NULL; I.value=NULL;
	    I.checked=NO; I.size=NULL; I.maxlength=NULL;

	    /* before any input field add a space if necessary */
	    UPDATE_STYLE;
	    HTML_put_character(me,' ');

	    if (present[HTML_INPUT_NAME])  
		StrAllocCopy(I.name, value[HTML_INPUT_NAME]);
	    if (present[HTML_INPUT_TYPE]) 
		StrAllocCopy(I.type, value[HTML_INPUT_TYPE]);
	    if (present[HTML_INPUT_VALUE]) 
		StrAllocCopy(I.value, value[HTML_INPUT_VALUE]);
	    if (present[HTML_INPUT_CHECKED]) 
		I.checked = YES;
	    if (present[HTML_INPUT_SIZE]) 
		StrAllocCopy(I.size, value[HTML_INPUT_SIZE]);
	    if (present[HTML_INPUT_MAXLENGTH]) 
		StrAllocCopy(I.maxlength, value[HTML_INPUT_MAXLENGTH]);

	    chars = HText_beginInput(me->text, &I);
	    for(; chars>0; chars--)
	    	HTML_put_character(me, '_');
	}
	break;
 
    case HTML_TEXTAREA:
	    /* Get ready for the value */
        HTChunkClear(&me->textarea);
	if (present[HTML_TEXTAREA_NAME])  
	    StrAllocCopy(textarea_name, value[HTML_TEXTAREA_NAME]);
	else
	    textarea_name = 0;
	break;


    case HTML_SELECT:
	{
	    char * name = NULL;
	    BOOLEAN multiple=NO;

	    if (present[HTML_SELECT_NAME])  
		StrAllocCopy(name, value[HTML_SELECT_NAME]);
	    if (present[HTML_SELECT_MULTIPLE])  
		multiple=YES;

	    HText_beginSelect(name, multiple);

	    first_option = TRUE;
	}
	break;

    case HTML_OPTION:
	{
		/* an option is a special case of an input field */
	    InputFieldData I;
	    int i;

	    if(!first_option) {
	        /* finish the data off */
       	        HTChunkTerminate(&me->option);
		/* finish the previous option @@@@@ */
	        HText_setLastOptionValue(me->text, me->option.data);
	    }

	    /* start a newline before each option */
            HText_appendCharacter(me->text,'\r');

            /* init */
            I.name=NULL; I.type=NULL; I.value=NULL;
            I.checked=NO; I.size=NULL; I.maxlength=NULL;

	    StrAllocCopy(I.type,"OPTION");

	    if(present[HTML_OPTION_SELECTED])
		I.checked=YES;

	    HText_beginInput(me->text, &I);
	    /* Get ready for the value */
            HTChunkClear(&me->option);

	    first_option = FALSE;

	    /* put 3 underscores and one space before each option */
            for(i=0; i<3; i++)
	    	HText_appendCharacter(me->text, '_');
	    HText_appendCharacter(me->text, ' ');

	}
	break;

    case HTML_LINK:
	if (present[HTML_LINK_HREF]) {
	    char * href = NULL;


	    StrAllocCopy(href, value[HTML_A_HREF]);
	    HTSimplify(href);

	    if(present[HTML_LINK_REV]) {
	        if(!strcasecomp("made", value[HTML_LINK_REV]) ||
	           !strcasecomp("owner", value[HTML_LINK_REV]))
    	            HTAnchor_setOwner(me->node_anchor, href);

	        if(TRACE)
		   fprintf(stderr,"HTML.c: DOC OWNER found\n");
	    }
	    free(href);
	}
	break;
	
    case HTML_TITLE:
        HTChunkClear(&me->title);
	List_Nesting_Level = -1;
	break;
	
    case HTML_NEXTID:
    	/* if (present[NEXTID_N] && value[NEXTID_N])
		HText_setNextId(me->text, atoi(value[NEXTID_N])); */
    	break;
	
    case HTML_ISINDEX:
   	HTAnchor_setIndex(me->node_anchor);
	break;
	
    case HTML_P:
	UPDATE_STYLE;
	/* HText_appendParagraph(me->text); */
	/* since everyone seems to use the paragraph tag to mean
	 * insert two linebreaks, I am changing its meaning now
	 * to mean two line breaks :(  It's so sad....
	 */
	HText_appendCharacter(me->text, '\r');
	HText_appendCharacter(me->text, '\r');
	me->in_word = NO;
	break;

    case HTML_DL:
	List_Nesting_Level++;  /* increment the List nesting level */
	if(List_Nesting_Level <= 0) {
            change_paragraph_style(me, present && present[DL_COMPACT]
    			              ? styles[HTML_DLC] : styles[HTML_DL]);

	} else if(List_Nesting_Level >= 6) {
            change_paragraph_style(me, present && present[DL_COMPACT]
    			              ? styles[HTML_DLC6] : styles[HTML_DL6]);

	} else {
            change_paragraph_style(me, present && present[DL_COMPACT]
    		 ? styles[(HTML_DLC1 - 1) + List_Nesting_Level] 
		 : styles[(HTML_DL1 - 1) + List_Nesting_Level]);
	}
	break;
	
    case HTML_DLC:
        List_Nesting_Level++;  /* increment the List nesting level */
        if(List_Nesting_Level <= 0) {
            change_paragraph_style(me, styles[HTML_DLC]);

        } else if(List_Nesting_Level >= 6) {
            change_paragraph_style(me, styles[HTML_DLC6]);

        } else {
            change_paragraph_style(me, 
                               styles[(HTML_DLC1 - 1) + List_Nesting_Level]);
        }
        break;

    case HTML_DT:
        if (!me->style_change) {
	    HText_appendParagraph(me->text);
	    me->in_word = NO;
	}
	break;
	
    case HTML_DD:
        UPDATE_STYLE;
	HText_appendCharacter(me->text, '\t');	/* Just tab out one stop */
	me->in_word = NO;
	break;

    case HTML_BR:
	/* put in a dummy ' ' character so that any separaters after
	 * the '/n' will be absorbed inside HTML_put_character
	 */
        UPDATE_STYLE;
	HTML_put_character(me, ' ');
	HText_appendCharacter(me->text, '\r');
	break;

    case HTML_HR:
        UPDATE_STYLE;
	{
	    register int i;
	    HText_appendCharacter(me->text, ' '); /* dummy white space */
	    HText_appendCharacter(me->text, '\r');
	    HText_appendCharacter(me->text, ' ');
	    HText_appendCharacter(me->text, ' ');

	    for(i=me->new_style->leftIndent+2; 
			i < LYcols-(me->new_style->rightIndent+4);  i++)
	        HTML_put_character(me, '_');

	    HTML_put_character(me, ' '); /* dummy white space */

	    HText_appendCharacter(me->text, '\r');
	    HText_appendCharacter(me->text, '\r');
	}
	break;
	 

    case HTML_OL:
	OL_Counter[(List_Nesting_Level < 5 ? List_Nesting_Level+1 : 6)] = 1; 
    case HTML_UL:
    case HTML_MENU:
    case HTML_DIR:
	List_Nesting_Level++;

	if(List_Nesting_Level <= 0) {
       	    change_paragraph_style(me, styles[element_number]);

	} else if(List_Nesting_Level >= 6) {
	    if(element_number==HTML_UL || element_number==HTML_OL) 
       	        change_paragraph_style(me, styles[HTML_OL6]);
	    else
       	        change_paragraph_style(me, styles[HTML_MENU6]);

	} else {
	    if(element_number==HTML_UL || element_number==HTML_OL) 
                change_paragraph_style(me, 
		          styles[HTML_OL1 + List_Nesting_Level - 1]);
	    else 
                change_paragraph_style(me, 
		          styles[HTML_MENU1 + List_Nesting_Level - 1]);
	}
	break;
	
    case HTML_LI:
        UPDATE_STYLE;  /* update to the new style */
	HText_appendParagraph(me->text);
	if (me->sp[0].tag_number == HTML_OL) {
	    char number_string[20];
	    register int count=0;
	    sprintf(number_string, "%2d.",
		OL_Counter[(List_Nesting_Level<6 ? List_Nesting_Level : 6)]++);
	    /* hack, because there is no append string! */
	    for(;number_string[count]!='\0';count++)
		if(number_string[count] == ' ')
	            HText_appendCharacter(me->text, number_string[count]);
		else
	    	    HTML_put_character(me, number_string[count]);

	    /* use HTML_put_character so that any other spaces
	     * comming through will be collapsed
	     */
	    HTML_put_character(me, ' ');  /* the spacing charactor */	

	} else if(me->sp[0].tag_number == HTML_UL ||
			   me->sp[0].tag_number == HTML_MENU ) {
	    /* hack, because there is no append string! */
	    HText_appendCharacter(me->text, ' ');
	    HText_appendCharacter(me->text, ' ');
	    /* use HTML_put_character so that any other spaces
	     * comeing through will be collapsed
	     */
	    switch(List_Nesting_Level % 7) {
		case 0:
	    	    HTML_put_character(me, '*');
		    break;
		case 1:
	    	    HTML_put_character(me, '+');
		    break;
		case 2:
	    	    HTML_put_character(me, 'o');
		    break;
		case 3:
	    	    HTML_put_character(me, '#');
		    break;
		case 4:
	    	    HTML_put_character(me, '@');
		    break;
		case 5:
	    	    HTML_put_character(me, '-');
		    break;
		case 6:
	    	    HTML_put_character(me, '=');
		    break;
		    
	    }
	    HTML_put_character(me, ' ');	
	}
	me->in_word = NO;
	break;
	
    case HTML_LISTING:				/* Litteral text */
    case HTML_XMP:
    case HTML_PLAINTEXT:
    case HTML_PRE:
	change_paragraph_style(me, styles[element_number]);
	UPDATE_STYLE;
    	if (me->comment_end)
    	    HText_appendText(me->text, me->comment_end);
	break;

    case HTML_IMG:			/* Images -- put ALT text */
	if (present[HTML_IMG_ALT]) {
	    char *alt_string=NULL;

	    HTML_put_character(me, ' ');  /* space char may be ignored */
	    StrAllocCopy(alt_string, value[HTML_IMG_ALT]);
	    HTML_put_string(me, alt_string);
	    HTML_put_character(me, ' ');  /* space char may be ignored */

	    free(alt_string);

	} else {   /* put [IMAGE] string */
	    HTML_put_string(me," [IMAGE] ");
	}
	break;
    
#ifdef BUGS_IN
    case HTML_HTML:			/* Ignore these altogether */
    case HTML_HEAD:
    case HTML_BODY:
	List_Nesting_Level = -1;
	break;
#endif /* BUGS_IN */
    
    case HTML_B:			 /* Physical character highlighting */
    case HTML_I:
    case HTML_U:
    
    case HTML_EM:			/* Logical character highlighting */
    case HTML_STRONG:
	HText_appendCharacter(me->text,LY_UNDERLINE_START_CHAR);
	if(TRACE)
	   fprintf(stderr,"Beginning underline\n");
    	break;
	
    case HTML_TT:			/* Physical character highlighting */
    case HTML_CODE:
    case HTML_SAMP:
    case HTML_KBD:
    case HTML_VAR:
    case HTML_DFN:
    case HTML_CITE:
	break; /* ignore */

    case HTML_H1:			/* paragraph styles */
    case HTML_H2:
    case HTML_H3:
    case HTML_H4:
    case HTML_H5:
    case HTML_H6:
    case HTML_H7:
    case HTML_ADDRESS:
    case HTML_BLOCKQUOTE:
    	change_paragraph_style(me, styles[element_number]);	/* May be postponed */
	/* List_Nesting_Level = -1;  Go ahead and let people shoot themselves
	 * in the foot
	 */
	break;

    } /* end switch */

    if (HTML_dtd.tags[element_number].contents!= SGML_EMPTY) {
	if (me->sp == me->stack) {
            fprintf(stderr, "HTML: ****** Maximum nesting of %d exceded!\n",
            MAX_NESTING);
            return;
        }

    	--(me->sp);
	me->sp[0].style = me->new_style;	/* Stack new style */
	me->sp[0].tag_number = element_number;

	if(TRACE)
	    fprintf(stderr,"HTML:begin_element: adding style to stack - %s\n",
							me->new_style->name);
    }	
}


/*		End Element
**		-----------
**
*/
/*	When we end an element, the style must be returned to that
**	in effect before that element.  Note that anchors (etc?)
**	don't have an associated style, so that we must scan down the
**	stack for an element with a defined style. (In fact, the styles
**	should be linked to the whole stack not just the top one.)
**	TBL 921119
**
**	We don't turn on "CAREFUL" check because the parser produces
**	(internal code errors apart) good nesting. The parser checks
**	incoming code errors, not this module.
*/
PRIVATE void HTML_end_element ARGS2(HTStructured *, me, int , element_number)
{
#ifdef CAREFUL			/* parser assumed to produce good nesting */
    if (element_number != me->sp[0].tag_number) {
        fprintf(stderr, "HTMLText: end of element %s when expecting end of %s\n",
		HTML_dtd.tags[element_number].name,
		HTML_dtd.tags[me->sp->tag_number].name);
		/* panic */
    }
#endif
    
    me->sp++;				/* Pop state off stack */

    if(TRACE)
	fprintf(stderr,"HTML:end_element: Popped style off stack - %s\n",me->sp->style->name);
    
    switch(element_number) {

    case HTML_A:
	UPDATE_STYLE;
	HText_endAnchor(me->text);
	break;

    case HTML_FORM:
	UPDATE_STYLE;
	HText_endForm();
	HText_appendCharacter(me->text,'\r'); 
	break;

    case HTML_SELECT:
	/* finish the data off */
       	HTChunkTerminate(&me->option);
	/* finish the previous option @@@@@ */
	HText_setLastOptionValue(me->text, me->option.data);
	break;

    case HTML_TEXTAREA:
        {
            InputFieldData I;
            int chars;

            /* init */
            I.name=NULL; I.type=NULL; I.value=NULL;
            I.checked=NO; I.size=NULL; I.maxlength=NULL;

            /* before any input field add a space if necessary */
            UPDATE_STYLE;
            HTML_put_character(me,' ');
	    HText_appendCharacter(me->text,'\r');

	    /* finish the data off */
            HTChunkTerminate(&me->textarea);

	    /* type defaults to text */
	    I.name= textarea_name;
	    StrAllocCopy(I.value, me->textarea.data);
	    StrAllocCopy(I.size,"60");

            chars = HText_beginInput(me->text, &I);
	    for(; chars>0; chars--)
	    	HTML_put_character(me, '_');
	    HText_appendCharacter(me->text,'\r');

	    break;
	}

    case HTML_TITLE:
        HTChunkTerminate(&me->title);
    	HTAnchor_setTitle(me->node_anchor, me->title.data);
	break;
	
    case HTML_LISTING:				/* Litteral text */
    case HTML_XMP:
    case HTML_PLAINTEXT:
    case HTML_PRE:
    	if (me->comment_start)
    	    HText_appendText(me->text, me->comment_start);
	change_paragraph_style(me, me->sp->style);  /* Often won't really change */
	break;

    case HTML_OL:
    case HTML_DL:
    case HTML_UL:
    case HTML_MENU:
    case HTML_DIR:
	List_Nesting_Level--;
	if(TRACE)
	    fprintf(stderr,"Reducing List Nesting Level to %d\n",
						    List_Nesting_Level);
	change_paragraph_style(me, me->sp->style);  

        break;

#ifdef BUGS_IN
    case HTML_HTML:			/* Ignore these altogether */
    case HTML_HEAD:
    case HTML_BODY:
	break;
#endif /* BUGS_IN */

    case HTML_B:
    case HTML_I:
    case HTML_U:
    
    case HTML_EM:			/* Logical character highlighting */
    case HTML_STRONG:
	HText_appendCharacter(me->text,LY_UNDERLINE_END_CHAR);
	if(TRACE)
	   fprintf(stderr,"Ending underline\n");
    	break;
	
    case HTML_TT:			/* Physical character highlighting */
    case HTML_CODE:
    case HTML_SAMP:
    case HTML_KBD:
    case HTML_VAR:
    case HTML_DFN:
    case HTML_CITE:
	break;

    default:
	change_paragraph_style(me, me->sp->style);  /* Often won't really change */
	break;
	
    } /* switch */
}


/*		Expanding entities
**		------------------
*/
/*	(In fact, they all shrink!)
*/

PRIVATE void HTML_put_entity ARGS2(HTStructured *, me, int, entity_number)
{
    HTML_put_string(me, p_entity_values[entity_number]);
}



/*	Free an HTML object
**	-------------------
**
** If the document is empty, the text object will not yet exist.
   So we could in fact abandon creating the document and return
   an error code.  In fact an empty document is an important type
   of document, so we don't.
**
**	If non-interactive, everything is freed off.   No: crashes -listrefs
**	Otherwise, the interactive object is left.	
*/
PUBLIC void HTML_free ARGS1(HTStructured *, me)
{
    UPDATE_STYLE;		/* Creates empty document here! */
    if (me->comment_end)
		HTML_put_string(me,me->comment_end);
    HText_endAppend(me->text);

    if (me->target) {
        (*me->targetClass.free)(me->target);
    }
    free(me);
}


PRIVATE void HTML_abort ARGS2(HTStructured *, me, HTError, e)

{			
    List_Nesting_Level = -1;

    if(me->text)
	HText_endAppend(me->text);

    if (me->target) {
        (*me->targetClass.abort)(me->target, e);
    }
    free(me);
}


/*	Get Styles from style sheet
**	---------------------------
*/
PRIVATE void get_styles NOARGS
{
    got_styles = YES;

    default_style =		HTStyleNamed(styleSheet, "Normal");

    styles[HTML_H1] =		HTStyleNamed(styleSheet, "Heading1");
    styles[HTML_H2] =		HTStyleNamed(styleSheet, "Heading2");
    styles[HTML_H3] =		HTStyleNamed(styleSheet, "Heading3");
    styles[HTML_H4] =		HTStyleNamed(styleSheet, "Heading4");
    styles[HTML_H5] =		HTStyleNamed(styleSheet, "Heading5");
    styles[HTML_H6] =		HTStyleNamed(styleSheet, "Heading6");
    styles[HTML_H7] =		HTStyleNamed(styleSheet, "Heading7");

    styles[HTML_DL] =		HTStyleNamed(styleSheet, "Glossary");
	/* nested list styles */
    styles[HTML_DL1] =		HTStyleNamed(styleSheet, "Glossary1");
    styles[HTML_DL2] =		HTStyleNamed(styleSheet, "Glossary2");
    styles[HTML_DL3] =		HTStyleNamed(styleSheet, "Glossary3");
    styles[HTML_DL4] =		HTStyleNamed(styleSheet, "Glossary4");
    styles[HTML_DL5] =		HTStyleNamed(styleSheet, "Glossary5");
    styles[HTML_DL6] =		HTStyleNamed(styleSheet, "Glossary6");

    styles[HTML_UL] =
    styles[HTML_OL] =		HTStyleNamed(styleSheet, "List");
	/* nested list styles */
    styles[HTML_OL1] =		HTStyleNamed(styleSheet, "List1");
    styles[HTML_OL2] =		HTStyleNamed(styleSheet, "List2");
    styles[HTML_OL3] =		HTStyleNamed(styleSheet, "List3");
    styles[HTML_OL4] =		HTStyleNamed(styleSheet, "List4");
    styles[HTML_OL5] =		HTStyleNamed(styleSheet, "List5");
    styles[HTML_OL6] =		HTStyleNamed(styleSheet, "List6");

    styles[HTML_MENU] =
    styles[HTML_DIR] =		HTStyleNamed(styleSheet, "Menu");    
	/* nested list styles */
    styles[HTML_MENU1] =	HTStyleNamed(styleSheet, "Menu1");    
    styles[HTML_MENU2] =	HTStyleNamed(styleSheet, "Menu2");    
    styles[HTML_MENU3] =	HTStyleNamed(styleSheet, "Menu3");    
    styles[HTML_MENU4] =	HTStyleNamed(styleSheet, "Menu4");    
    styles[HTML_MENU5] =	HTStyleNamed(styleSheet, "Menu5");    
    styles[HTML_MENU6] =	HTStyleNamed(styleSheet, "Menu6");    

    styles[HTML_DLC] =		HTStyleNamed(styleSheet, "GlossaryCompact");
	/* nested list styles */
    styles[HTML_DLC1] =		HTStyleNamed(styleSheet, "GlossaryCompact1");
    styles[HTML_DLC2] =		HTStyleNamed(styleSheet, "GlossaryCompact2");
    styles[HTML_DLC3] =		HTStyleNamed(styleSheet, "GlossaryCompact3");
    styles[HTML_DLC4] =		HTStyleNamed(styleSheet, "GlossaryCompact4");
    styles[HTML_DLC5] =		HTStyleNamed(styleSheet, "GlossaryCompact5");
    styles[HTML_DLC6] =		HTStyleNamed(styleSheet, "GlossaryCompact6");

    styles[HTML_ADDRESS] =	HTStyleNamed(styleSheet, "Address");
    styles[HTML_BLOCKQUOTE] =	HTStyleNamed(styleSheet, "Blockquote");
    styles[HTML_PLAINTEXT] =
    styles[HTML_XMP] =		HTStyleNamed(styleSheet, "Example");
    styles[HTML_PRE] =		HTStyleNamed(styleSheet, "Preformatted");
    styles[HTML_LISTING] =	HTStyleNamed(styleSheet, "Listing");
}
/*				P U B L I C
*/

/*	Structured Object Class
**	-----------------------
*/
PUBLIC CONST HTStructuredClass HTMLPresentation = /* As opposed to print etc */
{		
	"text/html",
	HTML_free,
	HTML_abort,
	HTML_put_character, 	HTML_put_string,  HTML_write,
	HTML_start_element, 	HTML_end_element,
	HTML_put_entity
}; 


/*		New Structured Text object
**		--------------------------
**
**	The strutcured stream can generate either presentation,
**	or plain text, or HTML.
*/
PUBLIC HTStructured* HTML_new ARGS3(
	HTParentAnchor *, 	anchor,
	HTFormat,		format_out,
	HTStream*,		stream)
{

    HTStructured * me;
    
    if (format_out != WWW_PLAINTEXT && format_out != WWW_PRESENT) {
        HTStream * intermediate = HTStreamStack(WWW_HTML, format_out,
		stream, anchor);
	if (intermediate) return HTMLGenerator(intermediate);
        fprintf(stderr, "** Internal error: can't parse HTML to %s\n",
       		HTAtom_name(format_out));
	exit (-99);
    }

    me = (HTStructured*) calloc(sizeof(*me),1);
    if (me == NULL) outofmem(__FILE__, "HTML_new");

    if (!got_styles) get_styles();

    me->isa = &HTMLPresentation;
    me->node_anchor =  anchor;
    me->title.size = 0;
    me->title.growby = 128;
    me->title.allocated = 0;
    me->title.data = 0;
    me->option.size = 0;
    me->option.growby = 128;
    me->option.allocated = 0;
    me->option.data = 0;
    me->textarea.size = 0;
    me->textarea.growby = 128;
    me->textarea.allocated = 0;
    me->textarea.data = 0;
    me->text = 0;
    me->style_change = YES; /* Force check leading to text creation */
    me->new_style = default_style;
    me->old_style = 0;
    me->sp = me->stack + MAX_NESTING - 1;
    me->sp->tag_number = -1;				/* INVALID */
    me->sp->style = default_style;			/* INVALID */
    
    me->comment_start = NULL;
    me->comment_end = NULL;
    me->target = stream;
    if (stream) me->targetClass = *stream->isa;	/* Copy pointers */
    
    return (HTStructured*) me;
}


/*	HTConverter for HTML to plain text
**	----------------------------------
**
**	This will convert from HTML to presentation or plain text.
*/
PUBLIC HTStream* HTMLToPlain ARGS3(
	HTPresentation *,	pres,
	HTParentAnchor *,	anchor,	
	HTStream *,		sink)
{
    return SGML_new(&HTML_dtd, HTML_new(anchor, pres->rep_out, sink));
}


/*	HTConverter for HTML to C code
**	------------------------------
**
**	C copde is like plain text but all non-preformatted code
**	is commented out.
**	This will convert from HTML to presentation or plain text.
*/
PUBLIC HTStream* HTMLToC ARGS3(
	HTPresentation *,	pres,
	HTParentAnchor *,	anchor,	
	HTStream *,		sink)
{
    
    HTStructured * html;
    
    (*sink->isa->put_string)(sink, "/* ");	/* Before even title */
    html = HTML_new(anchor, WWW_PLAINTEXT, sink);
    html->comment_start = "/* ";
    html->comment_end = " */\n";	/* Must start in col 1 for cpp */
/*    HTML_put_string(html,html->comment_start); */
    return SGML_new(&HTML_dtd, html);
}


/*	Presenter for HTML
**	------------------
**
**	This will convert from HTML to presentation or plain text.
**
**	Override this if you have a windows version
*/
#ifndef GUI
PUBLIC HTStream* HTMLPresent ARGS3(
	HTPresentation *,	pres,
	HTParentAnchor *,	anchor,	
	HTStream *,		sink)
{
    return SGML_new(&HTML_dtd, HTML_new(anchor, WWW_PRESENT, NULL));
}
#endif


/*	Record error message as a hypertext object
**	------------------------------------------
**
**	The error message should be marked as an error so that
**	it can be reloaded later.
**	This implementation just throws up an error message
**	and leaves the document unloaded.
**	A smarter implementation would load an error document,
**	marking at such so that it is retried on reload.
**
** On entry,
**	sink 	is a stream to the output device if any
**	number	is the HTTP error number
**	message	is the human readable message.
**
** On exit,
**	returns	a negative number to indicate lack of success in the load.
*/

PUBLIC int HTLoadError ARGS3(
	HTStream *, 	sink,
	int,		number,
	CONST char *,	message)
{
    HTAlert(message);		/* @@@@@@@@@@@@@@@@@@@ */
    return -number;
} 

