#include "HTUtils.h"
#include "LYUtils.h"
#include "LYStrings.h"
#include "LYStructs.h"
#include "LYGlobalDefs.h"
#include "LYCharSets.h"
#include "LYKeymap.h"

PUBLIC BOOLEAN have_read_cfg=FALSE;

#ifdef VMS
#define DISPLAY "DECW$DISPLAY"
#else
#define DISPLAY "DISPLAY"
#endif /* VMS */

PRIVATE int is_true ARGS1(char *,string)
{
   if(!strncasecomp(string,"TRUE",4))
	return(TRUE);
   else
	return(FALSE);
}

PRIVATE void add_item_to_list ARGS2(char *,buffer, lynx_html_item_type **,list_ptr)
{
   char *colon, *next_colon;
   lynx_html_item_type *cur_item, *prev_item;

   /* make a linked list */

   if(*list_ptr == NULL) {  /* first item */
      
      cur_item =(lynx_html_item_type *)calloc(sizeof(lynx_html_item_type),1);
      
      if(cur_item == NULL)
	perror("Out of memory in read_cfg");
      
      *list_ptr = cur_item;
      
   } else {
      
      /* find the last item */
      for(prev_item = *list_ptr; prev_item->next != NULL;
	  prev_item = prev_item->next)
	;  /* null body */
      
      cur_item = (lynx_html_item_type *)calloc(sizeof(lynx_html_item_type),1);
      
      if(cur_item == NULL)
	perror("Out of memory in read_cfg");
      else
	prev_item->next = cur_item;
   }
   
   cur_item->next = NULL;
   cur_item->name = NULL;
   cur_item->command = NULL;
   cur_item->always_enabled = FALSE;
   
   /* find first colon */
   colon = (char *)strchr(buffer,':');
   /* make sure it isn't escaped by a backslash */
   while(colon!=NULL && *(colon-1)=='\\')
     /* if it was escaped, try again */
     colon = (char *)strchr(colon+1,':');
   
   if(colon!=NULL) {

      cur_item->name = calloc((colon-buffer+1),sizeof(char));
      if(cur_item->name == NULL)
	perror("Out of memory in read_cfg");
      
      LYstrncpy(cur_item->name, buffer, colon-buffer);	
      
      remove_backslashes(cur_item->name);
      
      next_colon = (char *)strchr(colon+1,':');
      /* make sure it isn't escaped by a backslash */
      while(next_colon!=NULL && *(next_colon-1)=='\\')
	/* if it was escaped, try again */
	next_colon = (char *)strchr(next_colon+1,':');
      
      if(next_colon!=NULL) {
	 
	 cur_item->command = calloc(next_colon-colon, 
				       sizeof(char));
	 
	 if(cur_item->command == NULL)
	   perror("Out of memory in read_cfg");
	 
	 LYstrncpy(cur_item->command, colon+1, 
		   next_colon-(colon+1));
	 
	 remove_backslashes(cur_item->command);
	 
	 cur_item->always_enabled = is_true(next_colon+1);
      }
   }
}

PUBLIC void read_cfg ARGS1(char *,cfg_filename)
{
    FILE *fp;
    char buffer[501];
    char *line_feed;

    if (!cfg_filename || strlen(cfg_filename) == 0) {
	if(TRACE)
	    fprintf(stderr,"No filename following -cfg switch!\n");
	return;
    }

    if((fp = fopen(cfg_filename,"r")) == NULL) {
	if(TRACE)
	    fprintf(stderr,"lynx.cfg file not found as %s\n",cfg_filename);
	return;
    }

    have_read_cfg=TRUE;

    while(fgets(buffer, 500, fp) != NULL) {

	/* strip of \n at the end */
	if((line_feed = (char *)strchr(buffer,'\n')) != NULL)
	    *line_feed = '\0';
	
	if(buffer[0] == '#') {
	    /* nothing */

	} else if(buffer[0] == '\0') {
	    /* nothing */

        } else if(!strncasecomp(buffer,"SUFFIX:",7)) {
	    char *extention;
	    char *mime_type;

	    if(strlen(buffer) > 9) {
	        extention = buffer + 7;
	        if((mime_type = strchr(extention, ':')) != NULL) 
		    *mime_type++ = '\0';
		    HTSetSuffix(extention, mime_type, "binary", 1.0);
	    }

        } else if(!strncasecomp(buffer,"VIEWER:",7)) {
	    char *mime_type;
	    char *viewer;
	    char *environment;

	    if(strlen(buffer) > 9) {
	        mime_type = buffer + 7;
	        if((viewer = strchr(mime_type, ':')) != NULL) 
		    *viewer++ = '\0';
		    if((environment = strchr(viewer, ':')) != NULL) {
			*environment++ = '\0';
			/* if environment equals xwindows then only
			 * assign the presentation if there is a display
			 * variable
			 */
			if(!strcasecomp(environment,"XWINDOWS")) {
			    if(getenv(DISPLAY)) 
		      		HTSetPresentation(mime_type, viewer, 1.0, 	
								      3.0, 0.0);
			} else if(!strcasecomp(environment,"NON_XWINDOWS")) {
			    if(!getenv(DISPLAY)) 
		      		HTSetPresentation(mime_type, viewer, 1.0, 	
								      3.0, 0.0);
			} else {
		            HTSetPresentation(mime_type, viewer, 1.0, 3.0, 0.0);
			}
		    } else {
		        HTSetPresentation(mime_type, viewer,  1.0, 3.0, 0.0);
		    }
	    }

        } else if(!strncasecomp(buffer,"KEYMAP:",7)) {
            char *key;
            char *func;
  
            key = buffer + 7;
            if ((func = strchr(key, ':')) != NULL)
                *func++ = '\0';
            if (!remap(key, func))
                fprintf(stderr, "key remapping of %s to %s failed\n",key,func);

	} else if(!strncasecomp(buffer,"GLOBAL_MAILCAP:",15)) {

	    StrAllocCopy(global_type_map, buffer+15);

	} else if(!strncasecomp(buffer,"GLOBAL_EXTENSION_MAP:",21)) {

	    StrAllocCopy(global_extension_map, buffer+21);

	} else if(!strncasecomp(buffer,"PERSONAL_MAILCAP:",17)) {

            StrAllocCopy(personal_type_map, buffer+17);

        } else if(!strncasecomp(buffer,"PERSONAL_EXTENSION_MAP:",23)) {

            StrAllocCopy(personal_extension_map, buffer+23);

	} else if(!strncasecomp(buffer,"CHARACTER_SET:",14)) {
	    int i=0;
	    for(; LYchar_set_names[i]; i++)
		if(!strcmp(buffer+14,LYchar_set_names[i])) {
		    current_char_set=i;
		    break;
		}

	} else if(!strncasecomp(buffer,"STARTFILE:",10)) {

	    StrAllocCopy(startfile, buffer+10);

	} else if(!strncasecomp(buffer,"HELPFILE:",9)) {

	    StrAllocCopy(helpfile, buffer+9);

	} else if(!strncasecomp(buffer,"DEFAULT_INDEX_FILE:",19)) {
	    StrAllocCopy(indexfile, buffer+19);

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
		StrAllocCopy(bookmark_page,buffer+22);

	} else if(!strncasecomp(buffer,"DEFAULT_EDITOR:",15)) {
		StrAllocCopy(editor,buffer+15);

	} else if(!strncasecomp(buffer,"PRINTER:",8)) {
	        add_item_to_list (&buffer[8],&printers);

	} else if(!strncasecomp(buffer,"DOWNLOADER:",11)) {
	        add_item_to_list(&buffer[11],&downloaders);

#ifdef DIRED_SUPPORT
	} else if(!strncasecomp(buffer,"UPLOADER:",9)) {
	        add_item_to_list(&buffer[9],&uploaders);
#endif
        }  /* end of Huge if */
    } /* end if while */
}
