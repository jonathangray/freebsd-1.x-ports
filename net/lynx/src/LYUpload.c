/* Routines to upload files to the local filesystem */
/* Created by: Rick Mallett, Carleton University */
/* Report problems to rmallett@ccs.carleton.ca */

#include "LYCurses.h"
#include "HTUtils.h"
#include "LYUtils.h"
#include "LYGlobalDefs.h"
#include "LYSignal.h"
#include "LYStrings.h"
#include "LYClean.h"
#include "LYGetFile.h"
#include "LYUpload.h"
#include "LYSystem.h"
#include "LYLocal.h"

/*
 *  LYUpload uploads a file to a given location using a 
 *  specified upload method.
 */

/* it parses an incoming link that looks like
 *
 *  LYNXDIRED://UPLOAD=<#>/TO=<STRING>
 */

PUBLIC void LYUpload ARGS1(char *,line) 
{
    char *method, *directory;
    int method_number;
    int count;
    char tmpbuf[256];
    char buffer[256];
    lynx_html_item_type *upload_command=0;
    char c;
    char *cp;
    FILE *fp;
#ifdef VMS
    extern BOOLEAN HadVMSInterrupt;
#endif /* VMS */

    /* parse out the Method and the Location */

    if((directory = (char *)strstr(line, "TO=")) == NULL)
	goto failed;
    *(directory-1) = '\0';
    /* go past "Directory=" */
    directory+=3;

    if((method = (char *)strstr(line, "UPLOAD=")) == NULL)
	goto failed;
    /* go past "Method=" */
    method+=6;
    method_number = atoi(method);

    statusline("Enter a filename: ");
retry:
    *tmpbuf = '\0';
    if(LYgetstr(tmpbuf,0)==-1)
       goto cancelled;

    if(*tmpbuf=='\0')
       goto cancelled;

    if (strstr(tmpbuf,"../") != NULL) {
       statusline("Illegal redirection \"../\" found! Request ignored. ");
       sleep(3);
       goto cancelled;
    } else if(strchr(tmpbuf,'/') != NULL) {
       statusline("Illegal character \"/\" found! Request ignored. ");
       sleep(3);
       goto cancelled;
    } else if (tmpbuf[0] == '~') {
       statusline("Illegal redirection using \"~\" found! Request ignored. ");
       sleep(3);
       goto cancelled;
    }
    sprintf(buffer,"%s/%s",directory,tmpbuf);

	/* see if it already exists */
    if((fp = fopen(buffer,"r")) != NULL) {
        fclose(fp);

#ifdef VMS
	statusline("File exists. Create higher version? (y/n)");
#else
	statusline("File exists. Overwrite? (y/n)");
#endif /* VMS */
	c = 0;
	while(toupper(c)!='Y' && toupper(c)!='N') 
	  c = LYgetch();
#ifdef VMS
	if(HadVMSInterrupt) {
	   HadVMSInterrupt = FALSE;
	   return;
	}
#endif /* VMS */

	if(toupper(c) == 'N') {
	   statusline("Enter a filename: ");
	   goto retry;
	}
     }

    sprintf(buffer,"%s/%s",directory,tmpbuf);

     /* see if we can write to it */

    if((fp = fopen(buffer,"w")) != NULL) {
       fclose(fp);
       remove(buffer);
    } else {
       statusline("Cannot write to file. Enter a new filename: ");
       goto retry;
    }

    /* use configured upload commands */
    
    for(count=0, upload_command=uploaders; count < method_number;
	count++, upload_command = upload_command->next)
      ; /* null body */

	/* commands have the form "command %s [etc]"
	 * where %s is the filename
	 */
        if(upload_command->command != NULL) {
	   cp = quote_pathname(buffer); /* to prevent spoofing of the shell */
	   sprintf(tmpbuf,upload_command->command,cp);
	   free(cp);
        } else {
            statusline("ERROR! - upload command is misconfigured");
	    sleep(2);
	    goto failed;
        }

        stop_curses();
#ifndef VMS
        signal(SIGINT, SIG_IGN);
#endif /* not VMS */
        if(TRACE)
            fprintf(stderr,"command: %s\n",tmpbuf);
        system(tmpbuf);
        fflush(stdout);
#ifndef VMS
        signal(SIGINT, cleanup_sig);
#endif /* not VMS */
        start_curses();
        /* don't remove(file); */

    return;

failed:

    statusline("Unable to upload file");
    sleep(2);
    return;

cancelled:

    statusline("Cancelling");
    return;

}	

/*
 * LYUpload_options writes out the current upload choices to a file
 * so that the user can select printers in the same way that
 * they select all other links 
 * upload links look like
 *  LYNXDIRED//UPLOAD=<#>/TO=<STRING>
 */

PUBLIC int LYUpload_options ARGS2 (char **,newfile, char *,directory)
{
    static char upload_filename[256];
    static char *tempfile=NULL;
    FILE *fp0;
    lynx_html_item_type *cur_upload;
    char *last_slash;
    int count;
    static char curloc[256];
    char *cp;

    if(tempfile == NULL) {
	tempfile = (char *) malloc(127);
        tempname(tempfile,NEW_FILE);
#ifndef VMS
    } 
#else
    } else {
        remove(tempfile);   /* put VMS code to remove duplicates here */
    }
#endif /* VMS */

    cp = directory;
    if(!strncmp(cp,"file://localhost",16))
        cp += 16;
    else if(!strncmp(cp,"file:",5))
        cp += 5;
    strcpy(curloc,cp);
    HTUnEscape(curloc);
    if (curloc[strlen(curloc)-1] == '/')
        curloc[strlen(curloc)-1] = '\0';

    if((fp0 = fopen(tempfile,"w")) == NULL) {
        perror("Trying to open upload options file\n");
        exit(1);
    }

    /* make the file a URL now */
#ifdef VMS
    sprintf(upload_filename,"file://localhost/%s",tempfile);
#else
    sprintf(upload_filename,"file://localhost%s",tempfile);
#endif /* VMS */
    *newfile = upload_filename;

    fprintf(fp0,"<head><title>%s</title></head><body>",UPLOAD_OPTIONS_TITLE);

    fprintf(fp0,"\n<h1>Upload Options</h1>\n");


    fputs("You have the following upload choices<br>\n",fp0);
    fputs("please select one:\n<dl>",fp0);

    if(uploaders != NULL) {

       for(count=0, cur_upload=uploaders; cur_upload != NULL; 
	   cur_upload = cur_upload->next, count++) {

	  fprintf(fp0,"<dt><a href=\"LYNXDIRED://UPLOAD=%d/TO=%s\">",count,curloc);

	  fprintf(fp0, (cur_upload->name ? 
			cur_upload->name : "No Name Given"));
	  fprintf(fp0,"</a>\n\n");
	}
    } else {
	fprintf(fp0,"\
<br><dd>No other upload methods have been defined yet.  You may\n\
define an unlimited number of upload methods using the lynx.cfg\n\
file.");

    }
    fclose(fp0);

    return(0);
}



