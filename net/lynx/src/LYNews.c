#include "LYCurses.h"
#include "HTUtils.h"
#include "HTAccess.h"
#include "LYSignal.h"
#include "LYStructs.h"
#include "LYUtils.h"
#include "LYClean.h"
#include "LYStrings.h"
#include "LYGetFile.h"
#include "LYHistory.h"
#include "LYSystem.h"

#include "LYGlobalDefs.h"

/* global variable for async i/o */
BOOLEAN term_message;
PRIVATE void terminate_message  PARAMS((int sig));

PUBLIC int LYNewsPost ARGS1(document *,newdoc)
{
	char *newsgroups = strchr(newdoc->address,':')+1;
	char user_input[1024];
	FILE *fd;
	char *tmptr;
	char c;  /* user input */
	char tmpfile[100];
	char cmd[130];
        DocAddress WWWDoc;

	term_message=FALSE;

	/* pop previous document off of stack and load into main memory */
	pop(newdoc);
	WWWDoc.address = newdoc->address;
	WWWDoc.post_data = newdoc->post_data;
	WWWDoc.post_content_type = newdoc->post_content_type;
        if(!HTLoadAbsolute(&WWWDoc))
            return(NOT_FOUND);

	clear();
	move(2,0);

	tempname(tmpfile,NEW_FILE);
	if((fd = fopen(tmpfile,"w")) == NULL) {
	    statusline("Unable to open temp file");
	    sleep(2);
	    return(NORMAL);
	}

	addstr("You will be posting to:");
	addstr("\n	");
	addstr(newsgroups);
	addch('\n');

	/* Use ^C to cancel mailing of comment */
	/* and don't let sigints exit lynx     */
        signal(SIGINT, terminate_message);

	term_message=FALSE;

	addstr("\n\n Please enter your mail address\n");
	strcpy(user_input,"From: ");
	/* add the mail address if there is one */
	if(personal_mail_address)
	    strcat(user_input,personal_mail_address);

	if (LYgetstr(user_input, VISIBLE) < 0 || term_message) {
            statusline("News Post Cancelled!!!");
	    sleep(1);
	    fclose(fd);  /* close the temp file */
	    goto cleanup;
	}
	fprintf(fd,"%s\n",user_input);

        addstr("\n\n Please enter a subject line\n");
        strcpy(user_input,"Subject: ");

	/* add the default subject */
	tmptr = newdoc->title;
	while(isspace(*tmptr)) tmptr++;
	if(strncasecomp(tmptr, "Re:",3))
            strcat(user_input, "Re: ");
        strcat(user_input, newdoc->title);

        if (LYgetstr(user_input, VISIBLE) < 0 || term_message) {
            statusline("News Post Cancelled!!!");
            sleep(1);
            fclose(fd);  /* close the temp file */
            goto cleanup;
        }

	fprintf(fd,"%s\n",user_input);

	/* add Newsgroups: summary: and Keywords: */
	fprintf(fd,"Newsgroups: %s\nSummary: \nKeywords: \n\n",newsgroups);

	if(!no_editor && editor && *editor != '\0') {
	    /* ask if the user wants to include the original message */
	    statusline("Do you wish to inlude the original message? (y/n) ");
	    c=0;
	    while((c = toupper(LYgetch())) != 'Y' && c != 'N')
	        ; /* null body */
	    if(toupper(c) == 'Y')
	        /* the 1 will add the reply ">" in front of every line */
	        print_wwwfile_to_fd(fd,1);

	    fclose(fd);

	    /* spawn the users editor on the news file */
	    sprintf(user_input,"%s %s",editor,tmpfile);
	    statusline("Spawning your selected editor to edit mail message");
	    stop_curses();
	    if(system(user_input)) {
		statusline("Error spawning editor, check your editor definition in the options menu");
	  	sleep(1);
	    }
	    start_curses();

	} else {
	
	    addstr("\n\n Please enter your message below.");
	    addstr("\n When you are done, press enter and put a single period (.)");
	    addstr("\n on a line and press enter again.");
	    addstr("\n\n");
	    scrollok(stdscr,TRUE);
	    refresh();
    	    *user_input = '\0';
	    if (LYgetstr(user_input, VISIBLE) < 0 || term_message) {
	        statusline("News Post Cancelled!!!");
	        sleep(1);
	        fclose(fd);  /* close the temp file */
	        goto cleanup;
	    }


	    while(!STREQ(user_input,".") && !term_message) { 
	       addch('\n');
	       fprintf(fd,"%s\n",user_input);
	       *user_input = '\0';
	       if (LYgetstr(user_input, VISIBLE) < 0) {
	          statusline("News Post Cancelled!!!");
	          sleep(1);
	          fclose(fd);  /* close the temp file */
	          goto cleanup;
	       }
	    }

	    fprintf(fd,"\n");

	    fclose(fd);  /* close the temp file */
	    scrollok(stdscr,FALSE);  /* stop scrolling */
	}

	statusline("Post this message? (y/n) ");
	while((c = toupper(LYgetch())) != 'Y' && c != 'N')
	    ; /* null body */

	clear();  /* clear the screen */

	if(c == 'N') {
	   goto cleanup;
	}

	sprintf(cmd,"%s %s",INEWS,tmpfile);

        stop_curses();
	printf("Posting your message:\n\n%s\n\nPlease wait...", cmd);
#ifndef VMS
	signal(SIGINT, SIG_IGN);
#endif /* not VMS */
	system(cmd);
	start_curses();
#ifdef VMS
	goto cleandown;
#endif /* VMS */

	/* come here to cleanup and exit */
cleanup:
	signal(SIGINT, cleanup_sig);
cleandown:
	term_message = FALSE;

	scrollok(stdscr,FALSE);  /* stop scrolling */
	remove(tmpfile);

	return(NORMAL);
}

PRIVATE void terminate_message ARGS1(int,sig)
{
	term_message=TRUE;
#ifdef VMS
	/* Reassert the AST */
	signal(SIGINT, terminate_message);
        /* Refresh the screen to get rid of the "interrupt" message */
	clearok(curscr, TRUE);
	refresh();
#endif /* VMS */
}

