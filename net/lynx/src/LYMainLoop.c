#include "LYCurses.h"
#include "HTUtils.h"
#include "HTAccess.h"
#include "GridText.h"
#include "LYGlobalDefs.h"
#include "LYUtils.h"
#include "GridText.h"
#include "LYStrings.h"
#include "LYOptions.h"
#include "LYSignal.h"
#include "LYGetFile.h"
#include "HTForms.h"
#include "LYSearch.h"
#include "LYClean.h"
#include "LYHistory.h"
#include "LYPrint.h"
#include "LYMail.h"
#include "LYEdit.h"
#include "LYShowInfo.h"
#include "LYBookmark.h"
#include "LYSystem.h"
#include "LYKeymap.h"

#ifdef DIRED_SUPPORT
#include "LYLocal.h"
#endif

PRIVATE int are_different PARAMS((document *doc1, document *doc2));

/*
 * here's where we do all the work
 * mainloop is basically just a big switch dependent on the users input
 * I have tried to offload most of the work done here to procedures to
 * make it more modular, but this procedure still does alot of variable
 * manipulation.  This need some work to make it neater.
 */

void mainloop NOARGS
{
    int  c, cmd, arrowup=FALSE, show_help=FALSE;
    int lines_in_file= -1;
    int newline;
    char prev_target[512];
    char user_input_buffer[1024];
    char *owner_address;  /* holds the responsible owner's address */
    document newdoc, curdoc;
    BOOLEAN first_file=TRUE;
    BOOLEAN refresh_screen=FALSE;
    BOOLEAN force_load = FALSE;
#ifdef VMS
    extern BOOLEAN HadVMSInterrupt;   /* Flag from cleanup_sig */
#endif VMS

#ifdef DIRED_SUPPORT
   char *cp,*tp;
   char tmpbuf[1024];
   struct stat dir_info;
   taglink *t1,*t2;
   BOOLEAN retain_cache=FALSE; /* used in lynx_edit_mode to retain tags */
#endif

/*
 *  curdoc.address contains the name of the file that is currently open
 *  newdoc.address contains the name of the file that will soon be 
 *		   opened if it exits
 *  prev_target contains the last search string the user searched for
 *  newdoc.title contains the link name that the user last chose to get into
 *		   the current link (file)
 */
      /* initalize some variables*/
    nhist = 0;
    newdoc.address=0;
    curdoc.address=0;
    StrAllocCopy(newdoc.address, startfile);
    newdoc.title=0;
    curdoc.title=0;
    StrAllocCopy(newdoc.title, "Entry into main screen");
    newdoc.post_data=0;
    curdoc.post_data=0;
    newdoc.post_content_type=0;
    curdoc.post_content_type=0;
    newdoc.line=1;
    newdoc.link=0;
    *prev_target='\0';
    *user_input_buffer='\0';

    if(TRACE)
	fprintf(stderr,"Entering mainloop, startfile=%s\n",startfile);

    if(user_mode==NOVICE_MODE)
        display_lines = LYlines-4;
    else
        display_lines = LYlines-2;

    while (TRUE) {
	/* if newdoc.address is different then curdoc.address then we need 
	 * to go out and find and load newdoc.address
	 */
	if (LYforce_no_cache ||force_load ||are_different(&curdoc, &newdoc)) {

		force_load = FALSE;  /* done */
try_again:
	
		/* push the old file onto the history stack
	 	 */
		if(curdoc.address && newdoc.address) {
		    push(&curdoc);

		} else if(!newdoc.address) {
		    /* if newdoc.address is empty then pop a file 
		     * and load it 
		     */
                    pop(&newdoc);
		}

#ifdef DIRED_SUPPORT
		if (lynx_edit_mode && retain_cache == FALSE) {
		  LYforce_no_cache = 1;
		  while((t1=tagged) != NULL) { 
		     tagged = tagged->next;
		     free(t1);
		  }
	        }
		retain_cache = FALSE;
#endif

		switch(getfile(&newdoc)) {


		case NOT_FOUND: 

        	           /* OK! can't find the file */
                           /* so it must not be around now.  Print an error */
                   /* statusline(LINK_NOT_FOUND); don't print error */
		   /* sleep(1);  dont pause for awhile */

		   if(error_logging && !first_file && owner_address) 
			/* send an error message */
                      mailmsg(curdoc.link, owner_address, 
			history[nhist-1].address, history[nhist-1].title);

			/* do the NULL stuff also and reload the old file */

		   /* the first file wasn't found or has gone missing */
		   if(!nhist) { 
			/* if nhist = 0 then it must be the first file */
			if (!dump_output_immediately)
			    cleanup();
		        printf("\nlynx: Can't access start file %s\n",
								  startfile);
			exit(1);
		    }

		case NULLFILE:  /* not supposed to return any file */

		   /* the first file wasn't found or has gone missing */
		   if(!nhist) { 
			/* if nhist = 0 then it must be the first file */
			cleanup();
			printf("\nlynx: Start file could not be found or is not displayable.  Exiting...\n");
			exit(1);
		    }

		    newdoc.address = 0; /* to pop last doc */
		    goto try_again;
                    break;

		case NORMAL: 
#ifdef TRAVERSAL
		   /* during traversal build up a list of all links
		    * traversed.  Traversal mode is a special 
		    * feature to traverse every link in the web
		    */	
		   add_to_traverse_list(curdoc.address, curdoc.title);
#endif TRAVERSAL
		   curdoc.line = -1; 

		  /*
            	   * set newline to the saved line
            	   * savline contains the line the
            	   * user was on if he has been in
            	   * the file before or it is 1
            	   * if this is a new file
		   */
                   newline = newdoc.line;
			/* if we are going to a target line override
			 * the www_search line result
			 */
		   if(newline > 1)	
			www_search_result = -1;

	  	   break;	
		}  /* end switch */

	   if(TRACE)
	      sleep(2); /* allow me to look at the results */

	   /* set the files the same */
	   StrAllocCopy(curdoc.address, newdoc.address);
	   StrAllocCopy(curdoc.post_data, newdoc.post_data);
	   StrAllocCopy(curdoc.post_content_type, newdoc.post_content_type);

	   /* reset WWW present mode so that if we were getting
	    * the source, we get rendered HTML from now on
	    */
	   HTOutputFormat = WWW_PRESENT;
	   LYUserSpecifiedURL = FALSE;  /* only set for goto's */

  	} /* end if(STREQ(newdoc.address,curdoc.address) */

        if(dump_output_immediately) {
            print_wwwfile_to_fd(stdout,0);
	    return;
	}

	/* if the resent_sizechange variable is set to true
	   then the window size changed recently. 
	*/
	if(recent_sizechange) {
		stop_curses();
		start_curses(); 
		clear();
		refresh_screen = TRUE; /*to force a redraw */
		recent_sizechange=FALSE;
		display_lines = LYlines-2;
	}

        if(www_search_result != -1) {
             /* This was a WWW search, set the line
              * to the result of the search
              */
             newline = www_search_result;
             www_search_result = -1;  /* reset */
	     more = HText_canScrollDown();
        }


	/* if the curdoc.line is different than newline then there must
	 * have been a change since last update. Run showpage.
	 * showpage will put a fresh screen of text out.
	 * If this is a WWW document then use the 
	 * WWW routine HText_pageDisplay to put the page on
	 * the screen
         */
	if (curdoc.line != newline) {
	
   	    refresh_screen = FALSE;

	    HText_pageDisplay(newline, prev_target);

#ifdef DIRED_SUPPORT
	    if(lynx_edit_mode && nlinks && tagged != NULL)
	      showtags(tagged);
#endif
	    /* if more equals true then there is more
	     * info below this page 
	     */
	    more = HText_canScrollDown();
	    curdoc.line = newline = HText_getTopOfScreen()+1;
            lines_in_file = HText_getNumOfLines();

            if(HText_getTitle()) {
	        StrAllocCopy(curdoc.title, HText_getTitle());
	    } else {
	        StrAllocCopy(curdoc.title, newdoc.title);
	    }
	   owner_address = HText_getOwner();

	   if (arrowup) { 
		/* arrow up is set if we just came up from
		 * a page below 
		 */
	        curdoc.link = nlinks - 1;
	        arrowup = FALSE;
	   } else {
	        curdoc.link = newdoc.link;
		if(curdoc.link >= nlinks)
	            curdoc.link = nlinks - 1;
	   }

	   show_help = FALSE; /* reset */
	   newdoc.link = 0;
	   newdoc.line = 1;
	   curdoc.line = newline; /* set */
	}

	/* refesh the screen if neccessary */
	if(refresh_screen) {
	    clear();
	    HText_pageDisplay(newline, prev_target);

#ifdef DIRED_SUPPORT
	    if(lynx_edit_mode && nlinks && tagged != NULL)
	      showtags(tagged);
#endif
	    if(user_mode == NOVICE_MODE)
		noviceline(more);  /* print help message */
	    refresh_screen=FALSE;

	}

	if(first_file == TRUE) { /* we can never again have the first file */
	    first_file = FALSE; 
#ifdef NOTDEFINED /* fall through to show what it is on start as well */ 
	    show_help=TRUE;
		/* put initial help on the screen */
	    if(user_mode==NOVICE_MODE) 
	        noviceline(more);

	    if(more)  /* show some help for the first file */
                statusline(MOREHELP);
            else
                statusline(HELP);
#endif /* NOTDEFINED */
	}

	/* if help is not on the screen 
	 * then put a message on the screen
	 * to tell the user other misc info
	 */
	if (!show_help) {
	    /* make sure form novice lines are replaced */
	    if(user_mode == NOVICE_MODE) {
		noviceline(more);
	    }

	    /* if we are in forms mode then explicitly
	     * tell the user what each kind of link is
	     */
	    if(HTisDocumentSource()) {
		/* currently displaying HTML source */
		statusline(SOURCE_HELP);

	    } else if(lynx_mode==FORMS_LYNX_MODE) {
                if(links[curdoc.link].type == WWW_FORM_LINK_TYPE)
		    switch(links[curdoc.link].form->type) {
                    case F_PASSWORD_TYPE:
                        statusline(FORM_LINK_PASSWORD_MESSAGE);
		        break;
		    case F_OPTION_LIST_TYPE:
                        statusline(FORM_LINK_OPTION_LIST_MESSAGE);
			break;
                    case F_CHECKBOX_TYPE:
                    case F_RADIO_TYPE:
                        statusline(FORM_LINK_CHECKBOX_MESSAGE);
		        break;
                    case F_SUBMIT_TYPE:
                        statusline(FORM_LINK_SUBMIT_MESSAGE);
		        break;
                    case F_RESET_TYPE:
                        statusline(FORM_LINK_RESET_MESSAGE);
		        break;
                    case F_TEXT_TYPE:
		    case F_TEXTAREA_TYPE:
                        statusline(FORM_LINK_TEXT_MESSAGE);
		        break;
                    }
		else
	            statusline(NORMAL_LINK_MESSAGE);

		/* let them know if it's an index -- very rare*/
		if(is_www_index) {
		    move(LYlines-1,LYcols-8);
		    start_reverse();
		    addstr("-index-");
		    stop_reverse();
		}
			
	    } else if(user_mode == ADVANCED_MODE && nlinks>0) {
		/* show the URL */
		if(more)
		    if(is_www_index)
		        user_message("-more- -index- %s",
						 links[curdoc.link].lname);
		    else
		        user_message("-more- %s",links[curdoc.link].lname);
		else
		    if(is_www_index)
		        user_message("-index- %s",links[curdoc.link].lname);
		    else
		        statusline(links[curdoc.link].lname);
	    } else if(is_www_index && more) {
		statusline(WWW_INDEX_MORE_MESSAGE);
	    } else if(is_www_index) {
		statusline(WWW_INDEX_MESSAGE);
	    } else if (more) {
		if(user_mode == NOVICE_MODE)
			statusline(MORE);
		else
			statusline(MOREHELP);
	    } else {
	       statusline(HELP);
   	    }	     
	} else {
	   show_help = FALSE;
	}

	highlight(ON, curdoc.link);	/* highlight current link */

#ifdef TRAVERSAL
	/* this is a special feature to traverse every link in
	 * the database, and look for errors
	 */
	if( lookup(links[cur].lname)) {
		if(more || (cur > -1 && cur < nlinks-1))
		   c=DNARROW;
		else {
		   if(STREQ(curdoc.title,"Entry into main screen"))
		       exit(0);
		   c=LTARROW;
		}
	} else {
		add_to_table(links[cur].lname,);
		c = RTARROW;
	}
#else

	if(nlinks > 0 && links[curdoc.link].type == WWW_FORM_LINK_TYPE &&
			(links[curdoc.link].form->type == F_TEXT_TYPE ||
			 links[curdoc.link].form->type == F_PASSWORD_TYPE ||
			 links[curdoc.link].form->type == F_TEXTAREA_TYPE))
	  {
	    /* replace novice lines if in NOVICE_MODE */
    	    if(user_mode==NOVICE_MODE)
	     {
		move(LYlines-2,0); clrtoeol();
		addstr(FORM_NOVICELINE_ONE);
		move(LYlines-1,0); clrtoeol();
		addstr(FORM_NOVICELINE_TWO);
	     }
	    c=change_form_link(&links[curdoc.link],
				    FORM_UP, &newdoc, &refresh_screen);

	    if(c == '\n')  /* make return act like tab */
		c = '\t';
	  }
	else
	    /* Get a keystroke from the user
	     */ 
	    c=LYgetch();   /* get user input */


#ifdef VMS
          if (HadVMSInterrupt) {
              HadVMSInterrupt = FALSE;
              c = DO_NOTHING;
          }
#else
	if(recent_sizechange) {
	    if(c <= 0)
	       c = DO_NOTHING;
	}
#endif /* VMS */

#endif TRAVERSAL


new_keyboard_input:  /* a goto point for new input without going
           * back through the getch() loop
           */

	cmd=keymap[c+1];  /* add 1 to map EOF to 0 */

#if defined(DIRED_SUPPORT) && defined(OK_OVERRIDE)
	if (lynx_edit_mode && override[c+1] && !no_dired_support)
	  cmd = override[c+1];
#endif

new_cmd:  /* a goto point for new input without going
           * back through the getch() loop
           */

	switch(cmd) {
	case 0: /* unmapped character */
	default:
	    if (more)
                statusline(MOREHELP);
            else
                statusline(HELP);
            show_help = TRUE;

            if(TRACE)
                printw("%d", c);  /* show the user input */
            break;

	case LYK_1:
	case LYK_2:
	case LYK_3:
	case LYK_4:
	case LYK_5:
	case LYK_6:
	case LYK_7:
	case LYK_8:
	case LYK_9:
	    /* get a number from the user and follow that link number */
	    switch(follow_link_number(c, &curdoc.link)) {
	    case DO_LINK_STUFF:
                    /* follow a normal link */
                StrAllocCopy(newdoc.address, links[curdoc.link].lname);
		free_and_clear(&newdoc.post_data);
		break;

	    case PRINT_ERROR:
		statusline(BAD_LINK_NUM_ENTERED);
		sleep(1);
		break;
	    }
	    break;

	case LYK_SOURCE:  /* toggle view source mode */
	     if(HTisDocumentSource()) 
	         HTOutputFormat = WWW_PRESENT;
	     else
	         HTOutputFormat = WWW_SOURCE;
	     LYforce_no_cache = TRUE;
	     free_and_clear(&curdoc.address); /* so it doesn't get pushed */
	     break;

	case LYK_RELOAD:  /* control-R to reload and refresh */
	     LYforce_no_cache = TRUE;
	     newdoc.line=curdoc.line;
	     newdoc.link=curdoc.link;
	     free_and_clear(&curdoc.address); /* so it doesn't get pushed */
#ifdef VMS
	     clearok(curscr, TRUE);
	     refresh();
#endif /* VMS */
	     break;

#ifdef NOT_DONE_YET
	case LYK_PIPE:  
	    /* ignore for now */
	    break
#endif NOT_DONE_YET

	case LYK_QUIT:	/* quit */
	    statusline("Are you sure you want to quit? [Y] ");
	    if(toupper(LYgetch()) != 'N')
	        return;
	    else {
	        statusline("Excellent!");
		sleep(1);
	    }
	    break;
	
	case LYK_ABORT:
	    return;  /* dont ask the user about quitting */
	    break;

	case LYK_NEXT_PAGE:	/* next page */
	    if(more)
	           newline += display_lines;
	    else if(curdoc.link < nlinks-1) 
		{
		   highlight(OFF,curdoc.link);
		   curdoc.link = nlinks-1;  /* put on last link */
	        } 
	    else
		{
		   statusline("You are already at the end of this document.");
		   sleep(2);
		}
	    break;

	case LYK_PREV_PAGE:  /* page up */
	   if(newline > 1) 
	           newline -= display_lines;
	   else if(curdoc.link > 0) 
		{
		   highlight(OFF,curdoc.link);
		   curdoc.link = 0;  /* put on last link */
		}
	    else 
		{
		   statusline("You are already at the beginning of this document.");
		   sleep(2);
		}
	   break;

	case  LYK_UP_TWO:
	    if(newline > 1) 
	        newline -= 2;
	    else 
	        {
		   statusline("You are already at the beginning of this document.");
		   sleep(2);
	        }
	    break;

	case  LYK_DOWN_TWO:
	    if(more)
	           newline += 2;
	    else
		{
		   statusline("You are already at the end of this document.");
		   sleep(2);
		}
	    break;

	case LYK_REFRESH:
	   refresh_screen=TRUE;
#ifdef VMS
	   clearok(curscr, TRUE);
	   refresh();
#endif /* VMS */
	   break;

	case LYK_HOME:
	    if(curdoc.line > 1)
	        newline = 1;
	    else {
		cmd = LYK_PREV_PAGE;
		goto new_cmd;
	    }
	    break;

	case LYK_END:
	    if(more) {
	       newline = MAXINT; /* go to end of file */
	       arrowup = TRUE;  /* position on last link */
	    } else {
		cmd = LYK_NEXT_PAGE;
		goto new_cmd;
	    }
	    break;

	case LYK_PREV_LINK:
	    if (curdoc.link>0) {		/* previous link */
		highlight(OFF, curdoc.link);   /* unhighlight the current link */
		curdoc.link--;

	    } else if(!more && curdoc.link==0 && newline==1) { /* at the top of list */ 
		/* if there is only one page of data and the user
		 * goes off the top, then just move the cursor to
		 * last link on the page
		 */
		highlight(OFF,curdoc.link); /* unhighlight the current link */
		curdoc.link = nlinks-1;  /* the last link */

	    } else if (curdoc.line > 1) {	/* previous page */
		/* go back to the previous page */
		newline -= (display_lines);
		arrowup = TRUE;

	    } else {
		statusline("You're already at the top of this document");
		sleep(1);
	    }
	    break;

	case LYK_NEXT_LINK:
	    if (curdoc.link<nlinks-1) {		/* next link */
		highlight(OFF, curdoc.link);
		curdoc.link++;
	    /* at the bottom of list and there is only one page 
	     * move to the top link on the page
	     */
	    } else if(!more && newline==1 && curdoc.link==nlinks-1) {
		highlight(OFF,curdoc.link); 
		curdoc.link = 0;

            } else if (more) {  /* next page */
                 newline += (display_lines);

	    } else {
		statusline("You're already at the end of this document");
		sleep(1);
	    }
	    break;

        case LYK_UP_LINK:
            if (curdoc.link>0) {         /* more links? */
                int i, newlink= -1;
                for(i=curdoc.link; i>=0; i--)
                   if(links[i].ly < links[curdoc.link].ly) {
                        newlink=i;
                        break;
                   }

                if(newlink > -1) {
                    highlight(OFF, curdoc.link);
                    curdoc.link=newlink;
                } else if(!more && newline==1 && curdoc.link==0) {
                    highlight(OFF,curdoc.link);
                    curdoc.link = nlinks-1;
                } else if (more) {  /* next page */
                        newline += (display_lines);
                }

            /* at the bottom of list and there is only one page
             * move to the top link on the page
             */
            } else if(!more && newline==1 && curdoc.link==nlinks-1) {
                highlight(OFF,curdoc.link);
                curdoc.link = 0;

            } else if (curdoc.line > 1) {  /* next page */
                    newline -= (display_lines);

	    } else {
		statusline("You're already at the top of this document");
		sleep(1);
            }
            break;

	case LYK_DOWN_LINK:
	    if (curdoc.link<nlinks-1) {         /* more links? */
		int i, newlink= -1;
		for(i=curdoc.link; i<nlinks; i++)
		   if(links[i].ly > links[curdoc.link].ly) {
			newlink=i;
			break;
		   }
			
		if(newlink > -1) {
                    highlight(OFF, curdoc.link);
                    curdoc.link=newlink;
		} else if(!more && newline==1 && curdoc.link==nlinks-1) {
                    highlight(OFF,curdoc.link);
                    curdoc.link = 0;
                } else if (more) {  /* next page */
                        newline += (display_lines);
		}

            /* at the bottom of list and there is only one page
             * move to the top link on the page
             */
            } else if(!more && newline==1 && curdoc.link==nlinks-1) {
                highlight(OFF,curdoc.link);
                curdoc.link = 0;

            } else if (more) {  /* next page */
                    newline += (display_lines);

	    } else {
		statusline("You're already at the bottom of this document");
		sleep(1);
            }
            break;

	case LYK_RIGHT_LINK:
	    if (curdoc.link<nlinks-1 &&
			links[curdoc.link].ly == links[curdoc.link+1].ly) {
                highlight(OFF,curdoc.link);
		curdoc.link++;
	    }
	    break;

	case LYK_LEFT_LINK:
	    if (curdoc.link>0 &&
			links[curdoc.link].ly == links[curdoc.link-1].ly) {
                highlight(OFF,curdoc.link);
		curdoc.link--;
	    }
	    break;

	case LYK_HISTORY: 	/* show the history page */
	  if(strcmp(curdoc.title,"Lynx History Page")) {	
		/* don't do if already viewing history page */	

		/* push current file so that the history
		 * list contains the curent file for printing
		 * purposes.
		 * pop the file afterwards to prevent multiple copies 
		 */
                push(&curdoc);

		/* print history options to file */
	    	showhistory(&newdoc.address);  
 		free_and_clear(&curdoc.address);  /* so it doesn't get pushed */
		free_and_clear(&newdoc.post_data);

                pop(&curdoc);

		refresh_screen=TRUE;

	        break; 
	  } /* end if strncmp */

	/* dont put break here so that if the backspace key is pressed in
	 * the history page, then it acts like a left arrow 
	 */

	case LYK_PREV_DOC:			/* back up a level */
	        if (nhist>0) {  /* if there is anything to go back to */

		    /* set newdoc.address to empty to pop a file */
		    free_and_clear(&newdoc.address);
#ifdef DIRED_SUPPORT
		    if (lynx_edit_mode)
		      HTuncache_current_document();
#endif
	        } else if(child_lynx==TRUE) {
	   	   return; /* exit on left arrow in main screen */ 

		} else {
		    statusline("Already at the first document");
		    sleep(1);
		}
	     break;

	case LYK_ACTIVATE:			/* follow a link */
	    if (nlinks > 0) {
	        if(links[curdoc.link].type == WWW_FORM_LINK_TYPE) {
		        c = change_form_link(&links[curdoc.link],
				    FORM_UP, &newdoc, &refresh_screen);
			goto new_keyboard_input;

	        } else {   /* Not a forms link */
		    /* follow a normal link */
		    StrAllocCopy(newdoc.address, links[curdoc.link].lname);
		    StrAllocCopy(newdoc.title, links[curdoc.link].hightext);
		    free_and_clear(&newdoc.post_data);
		    newdoc.link = 0;
		    force_load = TRUE;  /* force MainLoop to reload */
#ifdef DIRED_SUPPORT
		    if (lynx_edit_mode) {
		       if (is_a_file(newdoc.address))
			  retain_cache = TRUE;
		       else /* if (dir_list_style == MIXED_STYLE) */ {
			  HTuncache_current_document();
			  HTUnEscape(newdoc.address);
			  strip_trailing_slash(newdoc.address);
		       }
		    }
#endif
		}
	    }
	    break;

	case LYK_GOTO:   /* 'g' to goto a random URL  */
	 
	    if(no_goto) {
	    	statusline("Goto a random URL is disallowed!");
		sleep(1);
		break;
	    }
	 
	    statusline("URL to open: "); 
	    LYgetstr(user_input_buffer, VISIBLE); /* ask the user */

	    if(no_file_url && !strncmp(user_input_buffer,"file:",5)) {
                 statusline("You are not allowed to goto \"file:\" URL's");
                 sleep(2);

            } else if(*user_input_buffer != '\0') {
	    
		/* make a name for this new URL */
	        StrAllocCopy(newdoc.title, "A URL specified by the user");
		/* get rid of leading spaces (and any other spaces) */
		collapse_spaces(user_input_buffer);
	        StrAllocCopy(newdoc.address, user_input_buffer);
		free_and_clear(&newdoc.post_data);
		force_load = TRUE;
#ifdef DIRED_SUPPORT
		if (lynx_edit_mode) 
		  HTuncache_current_document();
#endif
	    } 
	    break;

	case LYK_HELP:			/* show help file */
	    if(!STREQ(curdoc.address, helpfile)) {
	        StrAllocCopy(newdoc.address, helpfile);  /* set the filename */
		/* make a name for this help file */
	        StrAllocCopy(newdoc.title, "Help Screen");
		free_and_clear(&newdoc.post_data);

	    }
	    break;

	case LYK_INDEX:  /* index file */
		/* make sure we are not in the index already */
	    if(!STREQ(curdoc.address, indexfile)) {

	        if(indexfile[0]=='\0') { /* no defined index */
		    statusline("No index is currently available");
		    sleep(1);

	        } else {
	            StrAllocCopy(newdoc.address, indexfile);
	            StrAllocCopy(newdoc.title, "System Index"); /* name it */
		    free_and_clear(&newdoc.post_data);
	        } /* end else */
	    }  /* end if */
	    break;

	case LYK_FORM_UP:  /* change form */
	    if(lynx_mode == FORMS_LYNX_MODE) {
		if(links[curdoc.link].type == WWW_FORM_LINK_TYPE) {
		    c = change_form_link(&links[curdoc.link],
				    FORM_UP, &newdoc, &refresh_screen);
		    goto new_keyboard_input;
		} else {
		    statusline("'X' can only toggle a form link");
		}
	    } else {
		statusline("'X' only toggles in forms mode");
	    }
	    break;	

	case LYK_FORM_DOWN:  /* change form */
	    if(lynx_mode==FORMS_LYNX_MODE) {
		if(links[curdoc.link].type == WWW_FORM_LINK_TYPE) {
		    c = change_form_link(&links[curdoc.link],
				    FORM_DOWN,&newdoc,&refresh_screen);
		    goto new_keyboard_input;
		} else {
		    statusline("'Z' can only toggle a form link");
		}
	    } else {
		statusline("'Z' only toggles in forms mode");
	    }
	    break;	

	case LYK_MAIN_MENU:	/* return to main screen */
		/* if its already the startfile then don't reload it */
	    if(!STREQ(curdoc.address,startfile)) {
		
		statusline("Do you really want to go to the Main screen? (y/n) [n] ");
		if(toupper(LYgetch())=='Y') {
	            StrAllocCopy(newdoc.address, startfile);
                    StrAllocCopy(newdoc.title, "Entry into main screen");
		    free_and_clear(&newdoc.post_data);
	            highlight(OFF,curdoc.link); 
#ifdef DIRED_SUPPORT
		    if (lynx_edit_mode)
		      HTuncache_current_document();
#endif
		}
#ifdef VMS
		if (HadVMSInterrupt)
		    HadVMSInterrupt = FALSE;
#endif /* VMS */
	    } else {
		statusline("Already at main screen!");
		sleep(1);
	    }
	    break;

	case LYK_OPTIONS:     /* options screen */

#ifdef DIRED_SUPPORT
	    c = dir_list_style;
#endif
	    options(); /* do the options stuff */

#ifdef DIRED_SUPPORT
	    if (c != dir_list_style) {
	       HTuncache_current_document();
	       newdoc.address = curdoc.address;
	       curdoc.address = NULL;	
	    }
#endif
	    refresh_screen = TRUE; /* to repaint screen */
	    break;

	case LYK_INDEX_SEARCH: /* search for a user string */
	     if(is_www_index) {
               /* perform a database search */

		/* do_www_search will try to go out and get the document
		 * if it returns yes a new document was returned and is
		 * named in the newdoc.address
		 */
		if(do_www_search(&newdoc) == NORMAL) {
                    push(&curdoc);
		
		   /* make the curdoc.address the newdoc.address so that
		    * getfile doesn't try to get the newdoc.address.
		    * Since we have already gotton it.
		    */ 
		   StrAllocCopy(curdoc.address, newdoc.address);
		   StrAllocCopy(newdoc.post_data, curdoc.post_data);
		   curdoc.line = -1;
		   refresh_screen = TRUE; /* redisplay it */
		} else {
		   /* restore the old file */
		   StrAllocCopy(newdoc.address, curdoc.address);  
		   StrAllocCopy(newdoc.post_data, curdoc.post_data);
		}
	     } else {
		statusline("Not a searchable indexed document -- press '/' to search for a text string");
		sleep(1);
	     }
	     break;

	case LYK_WHEREIS: /* search within the document */
  	/* search for the next occurance of the user string */
	case LYK_NEXT:
            /* user search */
	    if(cmd != LYK_NEXT)
	        *prev_target = '\0';  /* reset old target */
	    textsearch(&curdoc, prev_target); 
	    break;

	case LYK_COMMENT:  /* reply by mail */
	   if(!owner_address) {
		statusline("No owner is defined for this file so you cannot send a comment");
		sleep(1);
	   } else {
		statusline("Do you wish to send a comment? [N]");
	       if(toupper(LYgetch()) == 'Y') {

	          if(is_url(owner_address) != MAILTO_URL_TYPE) { 
			/* the address is a url */
			/* just follow the link */
			
		       StrAllocCopy(newdoc.address, owner_address);

	          } else {
		    /* the owner_address is a mailto: url type */
		       if(strchr(owner_address,':')!=NULL)
			    /* send a reply. The address is after the colon */
	      	         reply_by_mail(strchr(owner_address,':')+1,curdoc.address); 
		       else
	      	         reply_by_mail(owner_address,curdoc.address); 

	               refresh_screen=TRUE;  /* to force a showpage */
	          }
	       }
	   }
	   break;

#ifdef DIRED_SUPPORT

	case LYK_TAG_LINK:  /* tag or untag the current link */
	   if(lynx_edit_mode && nlinks && !no_dired_support) {
	      if (dir_list_style == MIXED_STYLE) {
		 if (!strcmp(links[curdoc.link].hightext,"../")) 
		   break;
	      } else if (!strncmp(links[curdoc.link].hightext,"Up to ",6)) 
		 break;
	      t1 = tagged;
	      while(t1 != NULL) {
		 if(!strcmp(links[curdoc.link].lname,t1->name)) {
		    if(t1==tagged) 
		      tagged = t1->next;
		    else 
		      t2->next = t1->next;
		    free(t1);
		    tagflag(OFF,curdoc.link);
		    break;
		 }
		 t2 = t1;
		 t1 = t1->next;
	      }
	      if(t1 == NULL) {
		 t1 = (taglink *) malloc(sizeof(taglink));
		 if (tagged == NULL) 
		   tagged = t1;
		 else 
		   t2->next = t1;
		 t1->next = NULL;
		 t1->name = links[curdoc.link].lname;
		 tagflag(ON,curdoc.link);
	      }
	      if(curdoc.link < nlinks-1) {
		highlight(OFF, curdoc.link);
		curdoc.link++;
	      } else if(!more && newline==1 && curdoc.link==nlinks-1) {
		highlight(OFF,curdoc.link); 
		curdoc.link = 0;
	      } else if (more) {  /* next page */
                newline += (display_lines);
	      }
	   }
	   break;

	case LYK_MODIFY:  /* rename a file or directory */
           if(lynx_edit_mode && nlinks && !no_dired_support) {
	      if (local_modify(&curdoc)) {
		 HTuncache_current_document();
		 newdoc.address = curdoc.address;
		 curdoc.address = NULL;	
		 newdoc.line = curdoc.line;
		 newdoc.link = curdoc.link;
		 clear();
	      }
	   }
           break;

	case LYK_CREATE:  /* create a new file or directory */
           if(lynx_edit_mode && !no_dired_support) {
	      if (local_create(&curdoc)) {
		 HTuncache_current_document();
		 newdoc.address = curdoc.address;
		 curdoc.address = NULL;	
		 newdoc.line = curdoc.line;
		 newdoc.link = curdoc.link > -1 ? curdoc.link : 0;
		 clear();
	      }
	   }
           break;
#endif

	case LYK_EDIT:  /* edit */

#ifdef DIRED_SUPPORT

/* Allow the user to edit the link rather than curdoc in edit mode */

	   if(lynx_edit_mode && *editor != '\0' && !no_editor && !no_dired_support) {
	      if (nlinks > 0) {
		 cp = links[curdoc.link].lname;
		 if(is_url(cp) == FILE_URL_TYPE) {
		    tp = cp;
		    if(!strncmp(tp,"file://localhost",16))
		       tp += 16;
		    else if(!strncmp(tp,"file:",5))
		       tp += 5;
		    strcpy(tmpbuf,tp);
		    HTUnEscape(tmpbuf);
		    if(stat(tmpbuf,&dir_info) == -1) {
		       statusline("System error - failure to get status. ");
		       sleep(3);
		    } else {
		       if (((dir_info.st_mode) & S_IFMT) == S_IFREG) {
			  strcpy(tmpbuf,cp);
			  HTUnEscape(tmpbuf);
			  if(edit_current_file(tmpbuf,curdoc.link,newline))
			    HTuncache_current_document();
			  newdoc.address = curdoc.address;
			  curdoc.address = NULL;	
			  newdoc.line = curdoc.line;
			  newdoc.link = curdoc.link;
			  clear();  /* clear the screen */
			 }
		    }
		 }
	      }
	   } else
#endif
	   if(editor && *editor != '\0' && !no_editor) {
		if(edit_current_file(newdoc.address,curdoc.link,newline))
	            HTuncache_current_document();
		LYforce_no_cache = TRUE;  /*force the document to be reloaded*/
	        free_and_clear(&curdoc.address); /* so it doesn't get pushed */
		newdoc.line = curdoc.line;
		newdoc.link = curdoc.link;
		clear();  /* clear the screen */

	   } else if(!editor || *editor == '\0') {
		statusline("No editor is defined!");
		sleep(2);
	   }
	   break;

#ifdef DIRED_SUPPORT
        case LYK_REMOVE:  /* remove files and directories */
	   if(lynx_edit_mode && nlinks && !no_dired_support) {
	      if (local_remove(&curdoc)) {
		 HTuncache_current_document();
		 newdoc.address = curdoc.address;
		 curdoc.address = NULL;	
		 newdoc.line = curdoc.line;
		 newdoc.link = curdoc.link;
		 clear();
	      }
	   }
	   break;
#endif
	case LYK_INFO:  /* show document info */
	   /* show some info */
		/* don't do if already viewing info page */	
	   if(strcmp(curdoc.title, SHOWINFO_TITLE)) {
	        showinfo(&curdoc, lines_in_file, &newdoc, owner_address);
		LYforce_no_cache = TRUE;
           } else {
		/* if already in info page; get out */
		cmd = LYK_PREV_DOC;
		goto new_cmd;
	   }
           break;

	case LYK_PRINT:  /* print the file */
		/* don't do if already viewing print options page */	
	   if(strcmp(curdoc.title,"Lynx Printing Options")) {	

                print_options(&newdoc.address, lines_in_file);
	        refresh_screen=TRUE;  /* redisplay */
	   }
	   break;

#ifdef DIRED_SUPPORT
       case LYK_DIRED_MENU:  /* provide full file management menu */
	   if(lynx_edit_mode && !no_dired_support) {
	      dired_options(&curdoc,&newdoc.address);
	      refresh_screen=TRUE;  /* redisplay */
	      retain_cache = TRUE;  /* retain tags */
	   }
	   break;
#endif
	case LYK_ADD_BOOKMARK:  /* a to add link to bookmark file */
	   if(strcmp(curdoc.title,HISTORY_PAGE_TITLE) && 
				strcmp(curdoc.title,PRINT_OPTIONS_TITLE)) {

		if(nlinks) {
		    statusline("Save D)ocument or L)ink to bookmark file or C)ancel? (d,l,c): ");
		    c = LYgetch();
		    if(toupper(c) == 'D')
		        save_bookmark_link(curdoc.address, curdoc.title);
		    else if(toupper(c) == 'L')
			if(links[curdoc.link].type != WWW_FORM_LINK_TYPE)
                            save_bookmark_link(links[curdoc.link].lname, 
						links[curdoc.link].hightext);
			else
			  {
			    statusline("Cannot save form fields/links");
			    sleep(2);
			  }
		} else {
		    statusline("Save D)ocument to bookmark file or C)ancel? (d,c): ");
		    c = LYgetch();
		    if(toupper(c) == 'D')
		        save_bookmark_link(curdoc.address, curdoc.title);
		}
	   } else {
		statusline("History and Print files cannot be saved in the bookmark page");
		sleep(1);
	   }
		
	   break;

	case LYK_DEL_BOOKMARK:  /* delete home page link */
	    if(bookmark_page && (strstr(curdoc.address, bookmark_page) ||
			!strcmp(curdoc.title, MOSAIC_BOOKMARK_TITLE))) {
		statusline("Do you really want to delete this link from your bookmark file? (y/n)");
             	c = LYgetch();
                if(toupper(c) != 'Y')
                    break;
                remove_bookmark_link(links[curdoc.link].anchor_number-1);
		LYforce_no_cache = TRUE;  /*force the document to be reloaded*/
                newdoc.line=curdoc.line;
		if(curdoc.link == nlinks-1)
		    /* if we deleted the last link on the page */
                    newdoc.link=curdoc.link-1; 
		else
                    newdoc.link=curdoc.link;
	    } else {
		statusline("Remove only works on bookmark page links");
		sleep(2);
	        break;
	    }
	    /* fall through on deletion */

	case LYK_VIEW_BOOKMARK:   /* v to view home page */
	    /* see if a home page exists
	     * if it does replace newdoc.address with it's name 
	     */
	    if(get_bookmark_filename(&newdoc.address) != NULL) {
		LYforce_HTML_mode = TRUE;  /* force HTML */
		LYforce_no_cache = TRUE;  /*force the document to be reloaded*/
		StrAllocCopy(newdoc.title, "Bookmark File");
		free_and_clear(&newdoc.post_data);
	    } else {
		statusline("Unable to open Home page, use 'a' to save a link first");
       		sleep(2);
	    }

	    break;

	case LYK_SHELL:  /* shell escape */
	    if(!no_shell) {
	        stop_curses();
#ifdef VMS
		printf("Spawning DCL subprocess.  Logout to return to Lynx.");
		fflush(stdout);
		system("");
#else
                signal(SIGINT, SIG_IGN);
		printf(
		 "Spawning your default shell.  Use 'exit' to return to Lynx.\n");
	        fflush(stdout);
	        system(getenv("SHELL"));
                signal (SIGINT, cleanup_sig);
#endif /* VMS */
	        start_curses();
	        refresh_screen=TRUE;  /* for a showpage */
	    } else {
		statusline("The (!) command is currently disabled");
		sleep(2);
	    }
	    break;

	case LYK_DOWNLOAD:
	    if (nlinks > 0) {
                if(links[curdoc.link].type == WWW_FORM_LINK_TYPE) {
                        statusline("You cannot download a input field");
			sleep(2);

                } else {   /* Not a forms link */
                    /* follow a normal link */
                    StrAllocCopy(newdoc.address, links[curdoc.link].lname);
                    StrAllocCopy(newdoc.title, links[curdoc.link].hightext);
		    free_and_clear(&newdoc.post_data);
                    newdoc.link = 0;
	            HTOutputFormat = HTAtom_for("www/download");
		    /*force the document to be reloaded*/
		    LYforce_no_cache = TRUE;  
		    force_load = TRUE;  /* force MainLoop to reload */
                }
            } else {
		statusline("Nothing to download");
		sleep(1);
	    }
	    break;

#ifdef DIRED_SUPPORT
	  case LYK_UPLOAD:
	    if (lynx_edit_mode && !no_dired_support) {
                LYUpload_options(&newdoc.address,curdoc.address);
	     }
	    break;
#endif
	case LYK_TRACE_TOGGLE:
	    if(user_mode == ADVANCED_MODE) {
	        if(WWW_TraceFlag)
		    WWW_TraceFlag = FALSE;	
	        else 
		    WWW_TraceFlag = TRUE;	

		statusline(WWW_TraceFlag ? "Trace ON!" : "Trace OFF!");
		sleep(1);
	    }
	        break;

	case LYK_DO_NOTHING:
	    /* pretty self explanitory */
	    break;

	} /* end of BIG switch */
    }
}

PRIVATE int are_different ARGS2(document *,doc1, document *,doc2)
{

    if(!doc1->address)
	return (TRUE);
    if(!doc2->address)
	return (TRUE);

    if(strcmp(doc1->address, doc2->address))
	return(TRUE);

    if(doc1->post_data)
      {
	if(doc2->post_data)
	  {
	    if(strcmp(doc1->post_data, doc2->post_data))
		return(TRUE);
	  }
	else
	    return(TRUE);
      }
    else
        if(doc2->post_data)
	    return(TRUE);

    return(FALSE);
}
