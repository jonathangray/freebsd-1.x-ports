#include "HTUtils.h"
#include "LYUtils.h"
#include "LYStrings.h"
#include "LYStructs.h"
#include "LYGlobalDefs.h"
#include "LYCharSets.h"
#include "LYKeymap.h"

PRIVATE int is_true ARGS1(char *,string)
{
   if(!strncasecomp(string,"TRUE",4))
	return(TRUE);
   else
	return(FALSE);
}

PUBLIC void read_cfg ARGS1(char *,cfg_filename)
{
    FILE *fp;
    char buffer[501];
    char *line_feed;

    if(cfg_filename == NULL || (fp = fopen(cfg_filename,"r")) == NULL) {
	if(TRACE && cfg_filename != NULL)
	    fprintf(stderr,"lynx.cfg file not found as %s\n",cfg_filename);
	return;
    }

    while(fgets(buffer, 500, fp) != NULL) {

	/* strip of \n at the end */
	if((line_feed = (char *)strchr(buffer,'\n')) != NULL)
	    *line_feed = '\0';
	
	if(buffer[0] == '#') {
	    /* nothing */

	} else if(buffer[0] == '\0') {
	    /* nothing */

        } else if(!strncasecomp(buffer,"KEYMAP:",7)) {
            char *key;
            char *func;
  
            key = buffer + 7;
            if ((func = strchr(key, ':')) != NULL)
                *func++ = '\0';
            if (!remap(key, func))
                fprintf(stderr, "key remapping of %s to %s failed\n",key,func);

	} else if(!strncasecomp(buffer,"CHARACTER_SET:",14)) {
	    int i=0;
	    for(; LYchar_set_names[i]; i++)
		if(!strcmp(buffer+14,LYchar_set_names[i])) {
		    current_char_set=i;
		    break;
		}

	} else if(!strncasecomp(buffer,"STARTFILE:",10)) {
	    int length = strlen(buffer)-9;

	    startfile = (char *) calloc(length, sizeof(char));
	    if(startfile==NULL)
		perror("Out of memory in read_cfg");
	    else
	        strcpy(startfile, buffer+10);

	} else if(!strncasecomp(buffer,"HELPFILE:",9)) {
	    int length = strlen(buffer)-8;

	    helpfile = (char *) calloc(length, sizeof(char));
	    if(helpfile==NULL)
		perror("Out of memory in read_cfg");
	    else
	        strcpy(helpfile, buffer+9);

	} else if(!strncasecomp(buffer,"DEFAULT_INDEX_FILE:",19)) {
	    strcpy(indexfile, buffer+19);

#if defined(EXEC_LINKS) || defined(EXEC_SCRIPTS)
	} else if(!strncasecomp(buffer,
				    "LOCAL_EXECUTION_LINKS_ALWAYS_ON:",32)) {
	    local_exec = is_true(buffer+32);

	} else if(!strncasecomp(buffer,
			    "LOCAL_EXECUTION_LINKS_ON_BUT_NOT_REMOTE:",40)) {
	    local_exec_on_local_files = is_true(buffer+40);
#endif /* defined(EXEC_LINKS) || defined(EXEC_SCRIPTS) */

	} else if(!strncasecomp(buffer,"MAIL_SYSTEM_ERROR_LOGGING:",26)) {
	    error_logging = is_true(buffer+26);

	} else if(!strncasecomp(buffer,"VI_KEYS_ALWAYS_ON:",18)) {
	    vi_keys = is_true(buffer+18);

	} else if(!strncasecomp(buffer,"EMACS_KEYS_ALWAYS_ON:",21)) {
	    emacs_keys = is_true(buffer+21);

	} else if(!strncasecomp(buffer,
			"DEFAULT_KEYPAD_MODE_IS_NUMBERS_AS_ARROWS:",41)) {
	    if(is_true(buffer+41))
		keypad_mode = NUMBERS_AS_ARROWS;
	    else
		keypad_mode = LINKS_ARE_NUMBERED;

	} else if(!strncasecomp(buffer,"CASE_SENSITIVE_ALWAYS_ON:",25)) {
	     case_sensitive = is_true(buffer+25);

	} else if(!strncasecomp(buffer,"DEFAULT_USER_MODE:",18)) {
		if(!strncasecomp(buffer+18,"NOVICE",5))
		   user_mode = NOVICE_MODE;
		else if(!strncasecomp(buffer+18,"INTER",5))
		   user_mode = INTERMEDIATE_MODE;
		else if(!strncasecomp(buffer+18,"ADVANCE",7))
		   user_mode = ADVANCED_MODE;

	} else if(!strncasecomp(buffer,"DEFAULT_BOOKMARK_FILE:",22)) {
		LYstrncpy(bookmark_page,buffer+22,250);

	} else if(!strncasecomp(buffer,"DEFAULT_EDITOR:",15)) {
		LYstrncpy(editor,buffer+15,250);

	} else if(!strncasecomp(buffer,"PRINTER:",8)) {
	        char *colon, *next_colon;
	        printer_type *cur_printer, *prev_printer;

	        /* make linked list of printers */
	        if(printers == NULL) {  /* first printer */
		
	            cur_printer =(printer_type *)calloc(sizeof(printer_type),1);
		
	            if(cur_printer == NULL)
		        perror("Out of memory in read_cfg");

		    printers = cur_printer;
		
	        } else {

		   /* find the last printer */
		   for(prev_printer=printers; prev_printer->next != NULL;
				  	     prev_printer = prev_printer->next)
			   ;  /* null body */

	            cur_printer = (printer_type *)calloc(sizeof(printer_type),1);
		
	            if(cur_printer == NULL)
		        perror("Out of memory in read_cfg");
		    else
		        prev_printer->next = cur_printer;
	        }
		
		cur_printer->next = NULL;
		cur_printer->name = NULL;
		cur_printer->command = NULL;
		cur_printer->always_enabled = FALSE;

		/* find first colon */
		colon = (char *)strchr(buffer+8,':');
		/* make sure it isn't escaped by a backslash */
		while(colon!=NULL && *(colon-1)=='\\')
			/* if it was escaped, try again */
		    colon = (char *)strchr(colon+1,':');
	
		if(colon!=NULL) {
		
		    cur_printer->name = calloc((colon-(buffer+7)),sizeof(char));
		    if(cur_printer->name == NULL)
		        perror("Out of memory in read_cfg");

		    LYstrncpy(cur_printer->name, buffer+8, colon-(buffer+8));	

		    remove_backslashes(cur_printer->name);

		    next_colon = (char *)strchr(colon+1,':');
		    /* make sure it isn't escaped by a backslash */
		    while(next_colon!=NULL && *(next_colon-1)=='\\')
			/* if it was escaped, try again */
		        next_colon = (char *)strchr(next_colon+1,':');

		    if(next_colon!=NULL) {
			
			cur_printer->command = calloc(next_colon-colon, 
								sizeof(char));

			if(cur_printer->command == NULL)
                            perror("Out of memory in read_cfg");

			LYstrncpy(cur_printer->command, colon+1, 
							next_colon-(colon+1));

		        remove_backslashes(cur_printer->command);
			
		        cur_printer->always_enabled = is_true(next_colon+1);
		    }
		}

	} else if(!strncasecomp(buffer,"DOWNLOADER:",11)) {
	        char *colon, *next_colon;
	        download_type *cur_download, *prev_download;

	        /* make linked list of printers */
	        if(downloaders == NULL) {  /* first downloader */
		
	            cur_download =
		  	     (download_type *)calloc(sizeof(download_type),1);
		
	            if(cur_download == NULL)
		        perror("Out of memory in read_cfg");

		    downloaders = cur_download;
		
	        } else {

		   /* find the last download */
		   for(prev_download=downloaders; prev_download->next != NULL;
			  	           prev_download = prev_download->next)
			   ;  /* null body */

	            cur_download = (download_type *)calloc(sizeof(download_type),1);
		
	            if(cur_download == NULL)
		        perror("Out of memory in read_cfg");
		    else
		        prev_download->next = cur_download;
	        }
		
		cur_download->next = NULL;
		cur_download->name = NULL;
		cur_download->command = NULL;
		cur_download->always_enabled = FALSE;

		/* find first colon */
		colon = (char *)strchr(buffer+11,':');
		/* make sure it isn't escaped by a backslash */
		while(colon!=NULL && *(colon-1)=='\\')
			/* if it was escaped, try again */
		    colon = (char *)strchr(colon+1,':');
	
		if(colon!=NULL) {
		
		    cur_download->name = 
				    calloc((colon-(buffer+10)),sizeof(char));
		    if(cur_download->name == NULL)
		        perror("Out of memory in read_cfg");

		    LYstrncpy(cur_download->name,buffer+11,colon-(buffer+11));	

		    remove_backslashes(cur_download->name);

		    next_colon = (char *)strchr(colon+1,':');
		    /* make sure it isn't escaped by a backslash */
		    while(next_colon!=NULL && *(next_colon-1)=='\\')
			/* if it was escaped, try again */
		        next_colon = (char *)strchr(next_colon+1,':');

		    if(next_colon!=NULL) {
			
			cur_download->command = calloc(next_colon-colon, 
								sizeof(char));

			if(cur_download->command == NULL)
                            perror("Out of memory in read_cfg");

			LYstrncpy(cur_download->command, colon+1, 
							next_colon-(colon+1));

		        remove_backslashes(cur_download->command);
			
		        cur_download->always_enabled = is_true(next_colon+1);
		    }
		}
        }  /* end of Huge if */
    } /* end if while */
}
