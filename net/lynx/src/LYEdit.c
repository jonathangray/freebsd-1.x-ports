#include "LYCurses.h"
#include "HTUtils.h"
#include "LYSignal.h"
#include "LYUtils.h"
#include "LYClean.h"
#include "LYGlobalDefs.h"
#include "LYEdit.h"
#include "LYStrings.h"
#include "LYSystem.h"

/*
 *  in edit mode invoke either emacs or vi or the default editor
 *  to display and edit the current file
 *  Both vi and emacs will open the file to the same line that the
 *  screen cursor is on when editing is invoked
 *  returns FALSE if file uneditable
 */

PUBLIC int edit_current_file ARGS3(char *,newfile, int,cur, int,lineno)
{

	char command[90];
        char *filename = NULL;
	char *colon, *localhost_ptr;
	FILE *fp;
	int url_type = is_url(newfile);

	/* if its a remote file then we can't edit it */
	if(url_type != FILE_URL_TYPE) {
	    statusline("Lynx cannot currently (E)dit remote WWW files");
	    sleep(1);
	    return FALSE;
	}

	 /* try and open it as a completely referenced file */
	colon = strchr(newfile,':');
	if((fp = fopen(colon+1,"r")) == NULL) {

	    localhost_ptr = strstr(newfile,"localhost");
	    if(localhost_ptr==NULL || 
#ifdef VMS
	       /* Allow Unix-like or VMS pathspecs */
	       ((fp = fopen(localhost_ptr+10,"r"))==NULL &&
	        (fp = fopen(filename=
			    HTVMS_name("",localhost_ptr+9),"r"))==NULL)) {
#else
	       (fp = fopen(localhost_ptr+9,"r"))==NULL) {
		    
#endif /* VMS */
	        statusline("Lynx cannot currently edit remote WWW files");
	        sleep(1);
	        return FALSE;
	    } else {
#ifdef VMS
		if (filename == NULL)
		    filename = localhost_ptr+10;
#else
		filename = localhost_ptr+9;
#endif /* VMS */
	        fclose(fp);
	    }
	} else {
	    filename = colon+1;
	    fclose(fp);
	}
		

#ifndef VMS
	if(strstr(editor,"emacs") || strstr(editor,"vi")) 
	    sprintf(command,"%s +%d \"%s\"",editor, lineno+links[cur].ly, 
                                                                filename);
	else
	       sprintf(command,"%s \"%s\"",editor, filename);

#else /* VMS */
        /* Don't allow editing if user lacks write and delete access */
	if (access(filename,2)) {
		statusline("You are not authorized to edit this file.");
		sleep(1);
		return FALSE;
	}
        sprintf(command,"%s %s",editor, filename);

#endif /* VMS */

	stop_curses();
#ifndef VMS
	signal(SIGINT, SIG_IGN);
#endif /* not VMS */
	system(command);
#ifndef VMS
	signal(SIGINT, cleanup_sig);
#endif /* not VMS */
	start_curses();
	return TRUE;
}
