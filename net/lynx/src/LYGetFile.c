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

#ifdef DIRED_SUPPORT
#include "LYLocal.h"

PRIVATE char * LYSanctify ARGS1(char *, href) 
{
    int i;
    char *p,*cp,*tp;
    char address_buffer[1024];

    i = strlen(href) - 1;
    while (i && href[i] == '/') href[i--] = '\0';

    if ((cp = (char *) strchr(href,'~')) != NULL) {
       if (!strncmp(href,"file://localhost/",17))
	 tp = href + 17;
       else 
	 tp = href + 5;
       if ((cp-tp) && *(cp-1) != '/')
	 return href;
       LYstrncpy(address_buffer,href,cp-href);
       if (address_buffer[strlen(address_buffer)-1] == '/')
	 address_buffer[strlen(address_buffer)-1] = '\0';
       p = getenv("HOME");
       strcat(address_buffer,p);
       if (strlen(++cp))
	 strcat(address_buffer,cp);
       if (strcmp(href,address_buffer)) {
	   free(href);
	   StrAllocCopy(href,address_buffer);
       }
    }
    return href;
}

#endif


PRIVATE int fix_http_urls PARAMS((document *doc));
extern char * HTLoadedDocumentURL();
extern char * WWW_Download_File;

PUBLIC BOOLEAN getfile ARGS1(document *,doc)
{
        int url_type;
	char *tmptr;
	DocAddress WWWDoc;  /* a WWW absolute doc address struct */

	/* load the WWWDoc struct in case we need to use it */
	WWWDoc.address = doc->address;
        WWWDoc.post_data = doc->post_data;
        WWWDoc.post_content_type = doc->post_content_type;

	/* reset WWW_Download_File just in case */
	free_and_clear(&WWW_Download_File);

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
		    if(!HTLoadAbsolute(&WWWDoc))
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
#ifdef DIRED_SUPPORT
		} else if(url_type == LYNXDIRED_URL_TYPE) {
		    if (no_dired_support) {
		       statusline("File management support is disabled!");
		       sleep(2);
		       return(NULLFILE);
		    } else
		       return(local_dired(doc));
#endif
		} else if(url_type == LYNXHIST_URL_TYPE) {
			/* doc will change to the new file */
		    historytarget(doc);

			/* we changed it so reload */
		    WWWDoc.address = doc->address;
        	    WWWDoc.post_data = doc->post_data;
        	    WWWDoc.post_content_type = doc->post_content_type;

		    if(!HTLoadAbsolute(&WWWDoc))
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
			if (strstr(doc->address,"//") == doc->address+9)
			    system(doc->address+11);
			else
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
		    	sleep(2);
		    } else {
			stop_curses();
                        HTLoadAbsolute(&WWWDoc);
			start_curses();
                        fflush(stdout);

		    }

		    return(NULLFILE);

  		/* disable www news access if not news_ok */
                } else if(url_type == NEWS_URL_TYPE && !news_ok) {

                    statusline("USENET news access is disabled!");
                    sleep(2);
                    return(NULLFILE);

		} else if(url_type == RLOGIN_URL_TYPE) {

		    stop_curses();
                    HTLoadAbsolute(&WWWDoc);
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
		    if(!HTLoadAbsolute(&WWWDoc))
			return(NOT_FOUND);
		    status = do_www_search(doc);
		    if(status == NULLFILE) {
			pop(doc);
		        status = HTLoadAbsolute(&WWWDoc);
		    }
		    return(status); 
		} else if(url_type == MAILTO_URL_TYPE) {
		    tmptr = (char *)strchr(doc->address,':')+1;
		    reply_by_mail(tmptr,"");
		    return(NULLFILE);
		
		} else {

		    if(url_type == HTTP_URL_TYPE || url_type == FTP_URL_TYPE)
			fix_http_urls(doc);
		    WWWDoc.address = doc->address;  /* possible reload */

#ifdef DIRED_SUPPORT
		    lynx_edit_mode = FALSE;
		    if(url_type == FILE_URL_TYPE) {
		        doc->address = LYSanctify(doc->address);
		        WWWDoc.address = doc->address;
		    }
#endif
		    user_message(WWW_WAIT_MESSAGE,doc->address);

		    if(!HTLoadAbsolute(&WWWDoc)) {
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


			/* check for redirection
			 */
			if(use_this_url_instead) {
			    free(doc->address);
			    doc->address = use_this_url_instead;
			    free_and_clear(&doc->post_data);
			    use_this_url_instead = 0;
			}

			/* check to see if there is a temp
			 * file waiting for us to download
			 */
			if(WWW_Download_File) {
				LYdownload_options(&doc->address,
							WWW_Download_File);

		    		WWWDoc.address = doc->address;
		    		free_and_clear(&WWWDoc.post_data);
		    		free_and_clear(&WWWDoc.post_content_type);
				HTOutputFormat = WWW_PRESENT;
				if(!HTLoadAbsolute(&WWWDoc)) 
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
	      user_message("Badly formed address %s",doc->address);
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

PRIVATE int fix_http_urls ARGS1(document *,doc)
{
   char *slash;

   if(TRACE)
      fprintf(stderr,"LYGetFile: URL %s changed to ",doc->address);

   /* if there isn't a slash besides the two at the beginning, append one */
   if((slash = strrchr(doc->address, '/')) != NULL) 
	if(*(slash-1) != '/' || *(slash-2) != ':')
	     return(0);

   StrAllocCat(doc->address, "/");

   if(TRACE)
      fprintf(stderr,"%s",doc->address);

   return(1);
}
