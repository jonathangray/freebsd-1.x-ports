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

/*
 * here's where we do all the work
 * mainloop is basically just a big switch dependent on the users input
 * I have tried to offload most of the work done here to procedures to
 * make it more modular, but this procedure still does alot of variable
 * manipulation.  This need some work to make it neater.
 */

void mainloop ()
{
    int  c, cmd, arrowup=FALSE, show_help=FALSE;
    int lines_in_file= -1;
    int newline;
    char prev_target[MAXHIGHLIGHT];
    char line_buffer[1024];
    char address_buffer[1024];
    char *owner_address;  /* holds the responsible owner's address */
    document newdoc, curdoc;
    BOOLEAN first_file=TRUE;
    BOOLEAN refresh_screen=FALSE;
#ifdef VMS
    extern BOOLEAN HadVMSInterrupt;   /* Flag from cleanup_sig */
#endif VMS

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
    newdoc.address = startfile;
    newdoc.title = line_buffer;
    strcpy(newdoc.title, "Entry into main screen");
    newdoc.line = 1;
    newdoc.link = 0;
    curdoc.address 	= NULL;
    *prev_target	= '\0';
    *address_buffer = '\0';

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
	if (curdoc.address==NULL || newdoc.address==NULL ||
			!STREQ(curdoc.address, newdoc.address)) {
try_again:
		/* push the old file onto the history stack
	 	 */
		if(curdoc.address!=NULL && newdoc.address!=NULL) {
		    push(&curdoc);

		} else if(newdoc.address==NULL) {
		    /* if newdoc.address is empty then pop a file 
		     * and load it 
		     */
                    pop(&newdoc);
		}

		switch(getfile(&newdoc)) {


		case NOT_FOUND: 

        	           /* OK! can't find the file */
                           /* so it must not be around now.  Print an error */
                   /* statusline(LINK_NOT_FOUND); don't print error */
		   /* sleep(1);  dont pause for awhile */

		   if(error_logging && !first_file && owner_address) 
			/* send an error message */
                      mailmsg(curdoc.link, owner_address, 
			history[nhist-1].hfname, history[nhist-1].hightext);

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

		    pop(&newdoc);
		    newdoc.line = 0; /* reset */
		    newdoc.link = 0; /* reset */
		    refresh_screen = TRUE; /*to force a redraw */

		    /* make sure the correct document is in memory! */
		    if(!HTLoadAbsolute(newdoc.address)) {
			newdoc.address=NULL;
			goto try_again;  /* shouldn't ever happen */
		    } 
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

	  	   break;	
		}  /* end switch */

           owner_address = HText_getOwner();

	   if(TRACE)
	      sleep(2); /* allow me to look at the results */

	   /* set the files the same */
	   curdoc.address = newdoc.address;

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
	    /* if more equals true then there is more
	     * info below this page 
	     */
	    more = HText_canScrollDown();
	    curdoc.line = newline = HText_getTopOfScreen()+1;
            lines_in_file = HText_getNumOfLines();

            if(HText_getTitle()) {
	        curdoc.title = HText_getTitle();
	    } else {
	        curdoc.title = newdoc.title;
		if(curdoc.title == NULL)
		    curdoc.title = empty_string;
	    }

	   if (arrowup) { 
		/* arrow up is set if we just came up from
		 * a page below 
		 */
	        curdoc.link = nlinks - 1;
	        arrowup = FALSE;
	   } else {
	        curdoc.link = newdoc.link;
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
	    if(user_mode == NOVICE_MODE)
		noviceline(more);  /* print help message */
	    refresh_screen=FALSE;

	}

	if(first_file == TRUE) { /* we can never again have the first file */
	    first_file = FALSE;  
	    show_help=TRUE;
		/* put initial help on the screen */
	    if(user_mode==NOVICE_MODE) 
	        noviceline(more);

	    if(more)  /* show some help for the first file */
                statusline(MOREHELP);
            else
                statusline(HELP);
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
		statusline(MORE);
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
			  links[curdoc.link].form->type == F_PASSWORD_TYPE))
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
                newdoc.address = links[curdoc.link].lname;
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
	     HTuncache_current_document();
	     newdoc.address = curdoc.address;
	     curdoc.address = NULL;	
	     break;

	case LYK_RELOAD:  /* control-R to reload and refresh */
	     HTuncache_current_document();
	     newdoc.address = curdoc.address;
	     curdoc.address = NULL;	
	     newdoc.line=curdoc.line;
	     newdoc.link=curdoc.link;
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
	   newdoc.link=curdoc.link;
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
		    newdoc.address = NULL;

	        } else if(child_lynx==TRUE) {
	   	   return; /* exit on left arrow in main screen */ 
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
		    newdoc.address = links[curdoc.link].lname;
		    newdoc.title = links[curdoc.link].hightext;
		    newdoc.link = 0;
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
	    LYgetstr(address_buffer, VISIBLE); /* ask the user */

	    if(no_file_url && !strncmp(address_buffer,"file:",5)) {
                 statusline("You are not allowed to goto \"file:\" URL's");
                 sleep(2);

            } else if(*address_buffer != '\0') {
	    
		/* make a name for this new URL */
		newdoc.title = line_buffer;
	        strcpy(newdoc.title, "A URL specified by the user");
	        newdoc.address = address_buffer;
	    } 
	    break;

	case LYK_HELP:			/* show help file */
	    if(!STREQ(curdoc.address, helpfile)) {
	        newdoc.address = helpfile;  /* set the filename */

		/* make a name for this help file */
		newdoc.title = line_buffer;
	        strcpy(newdoc.title, "Help Screen");
	    }
	    break;

	case LYK_INDEX:  /* index file */
		/* make sure we are not in the index already */
	    if(!STREQ(curdoc.address, indexfile)) {

	        if(indexfile[0]=='\0') { /* no defined index */
		    statusline("No index is currently available");
		    sleep(1);

	        } else {
	            newdoc.address = indexfile;
		    newdoc.title = line_buffer;
	            strcpy(newdoc.title, "System Index"); /* name it */
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
	            newdoc.address = startfile;
		    newdoc.title = line_buffer;
                    strcpy(newdoc.title, "Entry into main screen");
	            highlight(OFF,curdoc.link); 
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
	    options(); /* do the options stuff */
	    refresh_screen = TRUE; /* to repaint screen */
	    break;

	case LYK_INDEX_SEARCH: /* search for a user string */
	     if(is_www_index) {
               /* perform a database search */

		/* do_www_search will try to go out and get the document
		 * if it returns yes a new document was returned and is
		 * named in the newdoc.address
		 */
		if(do_www_search(&newdoc.address) == NORMAL) {
                    push(&curdoc);
		
		   /* make the curdoc.address the newdoc.address so that
		    * getfile doesn't try to get the newdoc.address.
		    * Since we have already gotton it.
		    */ 
		   curdoc.address = newdoc.address;
		   curdoc.line = -1;
		   refresh_screen = TRUE; /* redisplay it */
		} else {
		   /* restore the old file */
		   newdoc.address = curdoc.address;  
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
			
		       newdoc.address = owner_address;

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

	case LYK_EDIT:  /* edit */
	   if(*editor != '\0' && !no_editor) {
		if(edit_current_file(newdoc.address,curdoc.link,newline))
	            HTuncache_current_document();
		newdoc.address = curdoc.address;
	        curdoc.address = NULL;	
		newdoc.line = curdoc.line;
		newdoc.link = curdoc.link;
		clear();  /* clear the screen */

	   } else if(*editor == '\0') {
		statusline("No editor is defined!");
		sleep(2);
	   }
	   break;

	case LYK_INFO:  /* show document info */
	   /* show some info */
	   showinfo(&curdoc, lines_in_file, owner_address);
	   refresh_screen=TRUE;
	   break;

	case LYK_PRINT:  /* print the file */
		/* don't do if already viewing print options page */	
	   if(strcmp(curdoc.title,"Lynx Printing Options")) {	

		/* save the old file */
                print_options(&newdoc.address, lines_in_file);
	        refresh_screen=TRUE;  /* redisplay */
	   }
	   break;

	case LYK_ADD_BOOKMARK:  /* a to add link to bookmark file */
	   if(strcmp(curdoc.title,HISTORY_PAGE_TITLE) && 
				strcmp(curdoc.title,PRINT_OPTIONS_TITLE)) {

		statusline("Do you wish to save this document in your current bookmark file? (y/n)");
		c = LYgetch();
		if(toupper(c) == 'Y')
		    save_bookmark_link(&curdoc);
	   } else {
		statusline("History and Print files cannot be saved in the bookmark page");
		sleep(1);
	   }
		
	   break;

#ifdef NOT_WORKING_YET
	case LYK_DEL_BOOKMARK:  /* delete home page link */
	    if(strstr(newdoc.address,bookmark_page)) {
		rewind(fp);
		remove_bookmark_link(fp, newdoc.address, curdoc.link, newline);
	        refresh_screen=TRUE;  /* reparse and display file */
		*curdoc.address = '\0';  /* reopen file */
	    } else {
		statusline("D)elete only works on bookmark page links");
		sleep(1);
	    }
	    break;
#endif NOT_WORKING_YET

	case LYK_VIEW_BOOKMARK:   /* v to view home page */
	    /* see if a home page exists
	     * if it does replace newdoc.address with it's name 
	     */
	    if(get_bookmark_filename(&newdoc.address)) {
		LYforce_HTML_mode = TRUE;  /* force HTML */
		LYforce_no_cache = TRUE;  /*force the document to be reloaded*/
		newdoc.title = line_buffer;
	        strcpy(newdoc.title, "Bookmark Page");
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
                    newdoc.address = links[curdoc.link].lname;
                    newdoc.title = links[curdoc.link].hightext;
                    newdoc.link = 0;
	            HTOutputFormat = HTAtom_for("www/download");
		    /*force the document to be reloaded*/
		    LYforce_no_cache = TRUE;  
                }
            } else {
		statusline("Nothing to download");
		sleep(1);
	    }
	    break;

	case LYK_DO_NOTHING:
	    /* pretty self explanitory */
	    break;

	} /* end of BIG switch */
    }
}

