#include "LYCurses.h"
#include "HTUtils.h"
#include "GridText.h"
#include "LYUtils.h"
#include "LYStructs.h"  /* includes HTForms.h */
#include "LYStrings.h"
#include "LYGlobalDefs.h"

PRIVATE int form_getstr PARAMS((struct link * form_link));

PUBLIC int change_form_link ARGS4(struct link *, form_link, int, mode, 
				document *,newdoc, BOOLEAN *,refresh_screen)
{

    FormInfo *form = form_link->form;
    int c=DO_NOTHING;

    /* move to the link position */
    move(form_link->ly, form_link->lx);

    switch(form->type) {
	case F_CHECKBOX_TYPE:
	    if(form->num_value) {
		form_link->hightext = unchecked_box;
		form->num_value = 0;
	    } else {
		form_link->hightext = checked_box;
		form->num_value = 1;
	    }
	    break;

	case F_RADIO_TYPE:
		/* radio buttons must have one and
		 * only one down at a time! 
		 */
	    if(form->num_value) {
		statusline("One radio button must be checked at all times!");
		sleep(1);

	    } else {	
		int i;
		/* run though list of the links on the screen and
		 * unselect any that are selected. :) 
		 */
		start_bold();
		for(i=0; i < nlinks; i++)
		   if(links[i].type == WWW_FORM_LINK_TYPE &&
		       links[i].form->type == F_RADIO_TYPE &&
		        links[i].form->number == form->number &&
   		         /* if it has the same name and its on */
                          !strcmp(links[i].form->name, form->name) &&
                            links[i].form->num_value ) 
		     {
			move(links[i].ly, links[i].lx);
			addstr(unchecked_box);
			links[i].hightext = unchecked_box;
		     }
		stop_bold();
		/* will unselect other button and select this one */
		HText_activateRadioButton(form);
		/* now highlight this one */
		form_link->hightext = checked_box;
	    }
	    break;

	case F_TEXT_TYPE:
	case F_PASSWORD_TYPE:
	    c = form_getstr(form_link);
	    if(form->type == F_PASSWORD_TYPE) 
        	form_link->hightext = STARS(strlen(form->value));
	    else
	    	form_link->hightext = form->value;
	    break;

	case F_RESET_TYPE:
	    HText_ResetForm(form);
            *refresh_screen = TRUE;
	    break;
	
	case F_SUBMIT_TYPE:
		/* returns new document URL */
	    newdoc->address = (char *)HText_SubmitForm(form);
	    newdoc->link = 0;
	    break;

    }

    return(c);

} 

#ifdef getyx
#define GetYX(y,x)   getyx(stdscr,y,x)
#else
#define GetYX(y,x)   y = stdscr->_cury, x = stdscr->_curx
#endif

#define LEFT_MARGIN 8
#define RIGHT_MARGIN 2

PRIVATE int form_getstr ARGS1(struct link *, form_link)
{
     FormInfo *form = form_link->form;
     int pointer = 0, tmp_pointer=0;
     int ch, i;
     int last_char, len;
     int max_length = (form->maxlength ? form->maxlength : 1024);
     char inputline[1024];
     int startcol, startline;
     int cur_col, far_col;
     BOOLEAN has_there_ever_been_data;  /* True if data was ever in string */
     BOOLEAN extend=TRUE; /* TRUE to enable line extention */
     BOOLEAN line_extended=FALSE;  /* TRUE if the line was moved to accomadate
				    * more text entry
				    */
#ifdef VMS
    extern BOOLEAN HadVMSInterrupt;/* Flag from cleanup_sig() AST       */
#endif

     /* get the initial position of the cursor */
     GetYX(startline, startcol);

     /* clear the old string */
     for(i=0; i < form->size; i++) 
	addch('_');

     /* go back to the initial position */
     move(startline, startcol);

     if(startcol + form->size > LYcols-1)
	far_col = LYcols-1;
     else
	far_col = startcol + form->size;

	/* if there is enough room to fit the whole
	 * string then disable the moving text feature
	 */
     if(form->maxlength && (far_col-startcol) >= form->maxlength)
	extend=FALSE;

     strcpy(inputline, form->value);

     /* find that last char in the string that is non-space */
     last_char = strlen(inputline)-1;
     while(isspace(inputline[last_char])) last_char--; 
     inputline[last_char+1] = '\0';

     if(last_char==-1)
	has_there_ever_been_data=FALSE;
     else
	has_there_ever_been_data=TRUE;

top:
     /*** Check to see if there's something in the inputline already ***/
     len = strlen(inputline);
     if(extend && len+startcol+LEFT_MARGIN > far_col) {
	pointer = (len - (far_col - startcol)) + 5;
	line_extended = TRUE;
     } else {
	line_extended = FALSE;
     }
     if(pointer > len || pointer < 0)
	pointer = 0;
     
     cur_col = startcol;
     while (inputline[pointer] != '\0') {
	  if(form->type == F_PASSWORD_TYPE)
	     addch('*');
	  else
	     addch(inputline[pointer]);
	  pointer++;
	  cur_col++;
     }
     refresh();

     for (;;) {
	  ch = LYgetch();

	  switch (ch) {
#ifdef VMS
	  if (HadVMSInterrupt) {
	      HadVMSInterrupt = FALSE;
	      ch = 7;
	  }
#endif /* VMS */

#ifdef AIX
          case '\227':
#endif
	  case '\n':
	  case '\r':
	  case '\t':
	  case DNARROW:
	  case UPARROW:
	       inputline[pointer] = '\0';
    	       StrAllocCopy(form->value, inputline);
		/* fill out the rest of the space with underscores */
	       for(;cur_col<far_col;cur_col++)
		  addch('_');
	       return(ch);
	       break;

          /* Control-G aborts */
          case 7:
               return(-1);

	 	/* break */  /* implicit break */


	  /* erase all the junk on the line and start fresh */
	  case 21 :
		move(startline, startcol);
     		/* clear the old string */
     		for(i=0; i <= cur_col-startcol; i++) 
		    addch('_');
		move(startline, startcol);  /* move back */
		pointer = 0;  /* clear inputline */
		cur_col = startcol;
		line_extended = FALSE;
		refresh();
		break;

	  /**  Backspace and delete **/

	  case '\010':
	  case '\177':
	  case LTARROW:
	       if (pointer > 0) {
		    addch('\010');
		    addch('_');
		    addch('\010');
	       
		    pointer--;
		    cur_col--;

		    if(line_extended && cur_col+RIGHT_MARGIN+LEFT_MARGIN < far_col) {
     			if(pointer + startcol+LEFT_MARGIN > far_col) {
			    tmp_pointer = (pointer - (far_col - startcol)) 
							      + RIGHT_MARGIN;
			    if(tmp_pointer < 0) tmp_pointer=0;
			} else {
			    tmp_pointer = 0;
			    line_extended = FALSE;
			}

			move(startline, startcol);
     			/* clear the old string */
     			for(i=0; i <= cur_col-startcol; i++) 
		    	    addch('_');
			move(startline, startcol);
		
			cur_col = startcol;
     
     			while (tmp_pointer != pointer) {
				if(form->type == F_PASSWORD_TYPE)
                                    addch('*');
                                else
                                     addch(inputline[tmp_pointer]);
	  			tmp_pointer ++;
	  			cur_col++;
     			}
			
		    }
		    refresh();

	       } else if(ch == LTARROW) {
		   char c='n';
		   if(has_there_ever_been_data) {
			statusline("Do you want to go back to the previous document? [n]");
			c=LYgetch();
		   } else 
		        c='Y';

		   if(toupper(c) == 'Y') {
		        inputline[0] = '\0';
    		        StrAllocCopy(form->value, inputline);
		        return(ch);
		   } else {
		        statusline("Enter text. Use arrows or tab to move off of field.");
		       /* move back to the string */
			move(startline, startcol);
			refresh();
		   }
	       }
	       break;

	  case RTARROW:  /* print error */
		{ 
		    int newx, newy;
     		    GetYX(newy, newx);
		    statusline("Link already selected!");
		    sleep(1);
		    statusline("Enter text. Use arrows or tab to move off of field.");
                       /* move back to the string */
                    move(newy, newx);
                    refresh();
		}
	        break;	    
	       
	  default:
	       if (printable(ch) && pointer < max_length) {
		    has_there_ever_been_data=TRUE; /* must be now */
		    inputline[pointer++]= ch;
		    cur_col++;
		    if(form->type == F_PASSWORD_TYPE)
		 	addch('*');
		    else
		    	addch(ch);

		    if(extend && cur_col+RIGHT_MARGIN > far_col) {
			tmp_pointer = (pointer - (far_col - startcol)) + 10;
			if(tmp_pointer > pointer)
			    tmp_pointer = 1;
			line_extended = TRUE;

			move(startline, startcol);
     			/* clear the old string */
     			for(i=0; i <= cur_col-startcol; i++) 
		    	    addch('_');
			move(startline, startcol);
     
			cur_col = startcol;

     			while (tmp_pointer != pointer) {
				if(form->type == F_PASSWORD_TYPE)
                                    addch('*');
                                else
                                     addch(inputline[tmp_pointer]);
	  			tmp_pointer ++;
	  			cur_col++;
     			}
			
		    } else if(pointer >= max_length) {
			statusline("Maximum length reached!");
			inputline[pointer] = '\0';
			move(startline,startcol);
		  	pointer=0;
			/* go back to to top and print it out again */
			goto top;
		    }
		    refresh();
	       } else if(!printable(ch)) {
		  /* terminate the string and return the char */
                   inputline[pointer] = '\0';
                   StrAllocCopy(form->value, inputline);
                    /* fill out the rest of the space with underscores */
                   for(;cur_col<far_col;cur_col++)
                      addch('_');
                   return(ch);
	       }
	  }
     }

}
