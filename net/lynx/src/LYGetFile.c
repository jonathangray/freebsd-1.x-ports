#include "LYCurses.h"
#include "HTUtils.h"
#include "GridText.h"
#include "HTAnchor.h"       /* Anchor class */
#include "HTAccess.h"
#include "LYGlobalDefs.h"
#include "LYUtils.h"
#include "LYSignal.h"
#include "LYGetFile.h"
#include "LYPrint.h"
#include "LYHistory.h"
#include "LYStrings.h"
#include "LYClean.h"
#include "LYDownload.h"
#include "LYNews.h"
#include "LYMail.h"
#include "LYSystem.h"

#ifndef VMS
#include <sys/types.h>
#include <sys/file.h>
#if SVR4
#include <sys/fcntl.h>
#endif
#endif

extern char * HTLoadedDocumentURL();
extern char * WWW_Download_File;

PUBLIC BOOLEAN getfile ARGS1(document *,doc)
{
        int url_type;
	char *tmptr;

	/* reset WWW_Download_File just in case */
	if(WWW_Download_File) {
	   free(WWW_Download_File);
	   WWW_Download_File = 0;
	}

	if(TRACE) {
	    fprintf(stderr,"getfile: getting %s\n",doc->address);
	}

	/* check to see if this is a universal document ID
	 * that lib WWW wants to handle
 	 *
	 * some special URL's we handle ourselves :)
	 */
	 if((url_type = is_url(doc->address)) != 0) {
#ifdef TRAVERSAL
		/* only done for traversals IGNORE! */
		if(url_type == HTTP_URL_TYPE) {
		    if(!HTLoadAbsolute(doc->address))
		        return(NOT_FOUND);
		} else {
		    return(NULLFILE);
		}
#else
		if(url_type == LYNXPRINT_URL_TYPE) {
		    return(printfile(doc));

		} else if(url_type == NEWSPOST_URL_TYPE) {

		    if(no_newspost) {
			statusline("News posting is disabled!");
			sleep(2);
			return(NULLFILE);
		    } else
		        return(LYNewsPost(doc));

		} else if(url_type == LYNXDOWNLOAD_URL_TYPE) {
		    LYDownload(doc->address);
		    return(NORMAL);

		} else if(url_type == LYNXHIST_URL_TYPE) {
			/* doc will change to the new file */
		    historytarget(doc);
		    if(!HTLoadAbsolute(doc->address))
		        return(NOT_FOUND);
		    return(NORMAL);

		} else if(url_type == LYNXEXEC_URL_TYPE) {
#ifdef EXEC_LINKS
        	    if (no_exec) {
            	        statusline("Execution has been disabled by system administrator.");
            		sleep(2);
		    } else if(local_exec || (local_exec_on_local_files &&
                     !strncmp(HTLoadedDocumentURL(), "file://localhost",16))) {
			/* Bug puts slash on end if none is in the string */
			char *last_slash = strrchr(doc->address,'/');
			if(last_slash-doc->address==strlen(doc->address)-1)
			    doc->address[strlen(doc->address)-1] = '\0';

			/* Show URL before executing it */
			statusline(doc->address);
			sleep(1);
			stop_curses();
			/* run the command */
			system(doc->address+9);
			/* Make sure user gets to see screen output */
			printf("\nPress <return> to return to Lynx");
			fflush(stdout);
			LYgetch();
#ifdef VMS
			{
			  extern BOOLEAN HadVMSInterrupt;
			  HadVMSInterrupt = FALSE;
			}
#endif /* VMS */
			start_curses();
			
           	     } else {
                	statusline("Execution is not enabled for this file.  See the O)ptions menu. (Press 'o')");
                	sleep(2);
		     }
#else /* no exec_links */
		     statusline("Execution capibilities are not compiled into this version");
		     sleep(2);
#endif /* EXEC_LINKS */
                     return(NULLFILE);

		  /* disable www telnet access if not telnet_ok */
		} else if(url_type == TELNET_URL_TYPE || 
			      url_type == TN3270_URL_TYPE ||
			           url_type == TELNET_GOPHER_URL_TYPE) {

		    if(!telnet_ok) {
		    	statusline("Telnet access is disabled!");
		    	sleep(1);
		    } else {
			stop_curses();
                        HTLoadAbsolute(doc->address);
			start_curses();
                        fflush(stdout);

		    }

		    return(NULLFILE);

		} else if(url_type == RLOGIN_URL_TYPE) {

		    stop_curses();
                    HTLoadAbsolute(doc->address);
                    fflush(stdout);
		    start_curses();
		    return(NULLFILE);

		/* if its a gopher index type and there isn't a search
		 * term already attached then do this.  Otherwise
   		 * just load it!
		 */
		} else if(url_type == INDEX_GOPHER_URL_TYPE &&
					strchr(doc->address,'?') == NULL) {
		    int status;
			/* load it because the do_www_search routine
			 * uses the base url of the currently loaded
			 * document :(
			 */
		    if(!HTLoadAbsolute(doc->address))
			return(NOT_FOUND);
		    status = do_www_search(&doc->address);
		    if(status == NULLFILE) {
			pop(doc);
		        status = HTLoadAbsolute(doc->address);
		    }
		    return(status); 
		} else if(url_type == MAILTO_URL_TYPE) {
		    tmptr = (char *)strchr(doc->address,':')+1;
		    reply_by_mail(tmptr,"");
		    return(NULLFILE);
		
		} else {
		    user_message(WWW_WAIT_MESSAGE,doc->address);

		    if(!HTLoadAbsolute(doc->address)) {
		        return(NOT_FOUND);
		    }

		    lynx_mode = NORMAL_LYNX_MODE;

		    /* return the interupts to the way they were */

		    /* some URL's don't actually return a document
		     * compare doc->address with the document that is 
		     * actually loaded and return NULL if not
		     * loaded.  If www_search_result is not -1
		     * then this is a reference to a named anchor
		     * within the same document.  Do NOT return
		     * NULL
		     */
                    {
                        char *pound;
                        /* check for #selector */
                        pound = (char *)strchr(doc->address, '#');

			/* check to see if there is a temp
			 * file waiting for us to download
			 */
			if(WWW_Download_File) {
				LYdownload_options(&doc->address,
							WWW_Download_File);

				HTOutputFormat = WWW_PRESENT;
				if(!HTLoadAbsolute(doc->address)) 
                        	    return(NOT_FOUND);
				else 
				    return(NORMAL);

			} else if(pound == NULL &&
                                strcmp(doc->address, HTLoadedDocumentURL()) ) {
		
				/* nothing needed to be shown */
				return(NULLFILE);

                        } else {
                        /* may set www_search_result */
                            if(pound != NULL)
                                HTFindPoundSelector(pound+1);
                            return(NORMAL);
                        }
                    }
		}
#endif TRAVERSAL
	  } else {
	      statusline("Badly formed address");
	      sleep(1);
              return(NULLFILE);
	  }
}

/* the user wants to select a link by number
 * if follow_link_number returns DO_LINK_STUFF do_link will be
 * run immeditely following its execution.
 * if follow_link_number returns PRINT_ERROR an error message will
 * be given to the user, if follow_link_number returns DO_FORMS_STUFF
 * some forms stuff will be done, and if follow_link_number returns
 * DO_NOTHING nothing special will run after it.
 */

PUBLIC int follow_link_number ARGS2(int,c, int *,cur)
{
    char temp[120];
    int link_number;

    temp[0] = c;
    temp[1] = '\0';
    statusline("follow link number: ");
    /* get the number from the user */
    if (LYgetstr(temp, VISIBLE) <0 || strlen(temp) == 0) {
        statusline("Cancelled!!!");
        sleep(1);
        return(DO_NOTHING);
    }

    link_number = atoi(temp);

    if (link_number > 0)  {
              /* get the lname, and hightext,
               * direct from
               * www structures and add it to the cur link
               * so that we can pass it transparently on to
               * get_file()
               * this is done so that you may select a link
               * anywhere in the document, whether it is displayed
               * on the screen or not!
               */
	       if(HTGetLinkInfo(link_number, &links[*cur].hightext, 
					 &links[*cur].lname)) {
                   links[*cur].type = WWW_LINK_TYPE;

		   return(DO_LINK_STUFF);
       		} else {
		   return(PRINT_ERROR);
		}
    } else {
            return(PRINT_ERROR);
    }

}
