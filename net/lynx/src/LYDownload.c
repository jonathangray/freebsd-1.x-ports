#include "LYCurses.h"
#include "HTUtils.h"
#include "LYUtils.h"
#include "LYGlobalDefs.h"
#include "LYSignal.h"
#include "LYStrings.h"
#include "LYClean.h"
#include "LYGetFile.h"
#include "LYDownload.h"
#include "LYSystem.h"

/*
 *  LYDownload takes a URL and downloads it using a user selected
 *  download program
 */

/* it parses an incoming link that looks like
 *
 *  LYNXDOWNLOAD://Method=<#>/File=<STRING>[/SugFile=<STRING>]
 */

#ifdef VMS
#define COPY_COMMAND "copy/nolog/noconf %s %s"
#else
#define COPY_COMMAND "cp %s %s"
#endif

PUBLIC void LYDownload ARGS1(char *,line) 
{
    char *method, *file, *sug_file=0;
    int method_number;
    int count;
    char buffer[256];
    char command[256];
    download_type *download_command=0;
    char c;
    FILE *fp;
#ifdef VMS
    extern BOOLEAN HadVMSInterrupt;
#endif /* VMS */

    /* parse out the sug_file, Method and the File */
    if((sug_file = (char *)strstr(line, "SugFile=")) != NULL) {
	*(sug_file-1) = '\0';
        /* go past "SugFile=" */
        sug_file+=8;
    }
	

    if((file = (char *)strstr(line, "File=")) == NULL)
	goto failed;
    *(file-1) = '\0';
    /* go past "File=" */
    file+=5;


    if((method = (char *)strstr(line, "Method=")) == NULL)
	goto failed;
    /* go past "Method=" */
    method+=7;
    method_number = atoi(method);
 

    if(method_number < 0) {
	/* write to local file */	
	statusline("Enter a filename: ");
retry:
	if(sug_file)
	   strcpy(buffer,sug_file);
	else
	   *buffer = '\0';
	if(LYgetstr(buffer,0)==-1)
	   goto cancelled;

	if(*buffer=='\0')
	   goto cancelled;

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

	/* see if we can write to it */
	if((fp = fopen(buffer,"w")) != NULL) {
	    fclose(fp);
	    remove(buffer);
	} else {
	    statusline("Cannot write to file. Enter a new filename: ");
	    goto retry;
	}

	statusline("Saving.....");
	sprintf(command,COPY_COMMAND,file,buffer);
	if(TRACE)
	    fprintf(stderr,"command: %s\n",command);
	system(command);

    } else {

	/* use configured download commands */

        for(count=0, download_command=downloaders; count < method_number;
       			    count++, download_command = download_command->next)
	    ; /* null body */

	/* commands have the form "command %s [etc]"
	 * where %s is the filename
	 */
        if(download_command->command != NULL) {
   	    sprintf(buffer,download_command->command,file);
        } else {
            statusline("ERROR! - download command is misconfigured");
	    sleep(2);
	    goto failed;
        }

        stop_curses();
#ifndef VMS
        signal(SIGINT, SIG_IGN);
#endif /* not VMS */
        if(TRACE)
            fprintf(stderr,"command: %s\n",buffer);
        system(buffer);
        fflush(stdout);
#ifndef VMS
        signal(SIGINT, cleanup_sig);
#endif /* not VMS */
        start_curses();
        /* don't remove(file); */
    }

    return;

failed:

    statusline("Unable to download file");
    sleep(2);
    return;

cancelled:

    statusline("Cancelling");
    return;

}	

/*
 * LYdownload_options writes out the current download choices to a file
 * so that the user can select printers in the same way that
 * they select all other links 
 * download links look like
 *  LYNXDOWNLOAD://Method=<#>/File=<STRING>[/SugFile=<STRING>]
 */

PUBLIC int LYdownload_options ARGS2(char **,newfile, char *,data_file)
{
    static char download_filename[256];
    char *sug_filename=0;
    char buffer[LINESIZE];
    FILE *fp0;
    download_type *cur_download;
    char *last_slash;
    int count;

    /* get a suggested filename */
    StrAllocCopy(sug_filename,*newfile);
    change_sug_filename(sug_filename);

    tempname(buffer,NEW_FILE);

    if((fp0 = fopen(buffer,"wb")) == NULL) {
        perror("Trying to open download options file\n");
        exit(1);
    }

    /* make the file a URL now */
#ifdef VMS
    sprintf(download_filename,"file://localhost/%s",buffer);
#else
    sprintf(download_filename,"file://localhost%s",buffer);
#endif /* VMS */
    *newfile = download_filename;

    fprintf(fp0,"<head><title>%s</title></head><body>",DOWNLOAD_OPTIONS_TITLE);

    fprintf(fp0,"\n<pre>\n                  Download Options\n\n");


    fputs("        You have the following download choices\n",fp0);
    fputs("                     please select one\n\n",fp0);

    if(!no_disk_save)
         fprintf(fp0,"\
     <a href=\"LYNXDOWNLOAD://Method=-1/File=%s/SugFile=%s\">Save to disk</a>\n\n",
						data_file, sug_filename);
    else
	fprintf(fp0,"     Save to disk disabled.\n\n");

    if(downloaders != NULL) {

        for(count=0, cur_download=downloaders; cur_download != NULL; 
			    cur_download = cur_download->next, count++) {

	    if(!no_download || cur_download->always_enabled) {
	        fprintf(fp0,"\
     <a href=\"LYNXDOWNLOAD://Method=%d/File=%s\">", count,data_file);

		fprintf(fp0, (cur_download->name ? 
				cur_download->name : "No Name Given"));
		fprintf(fp0,"</a>\n\n");
	    }
	}
    } else {
	fprintf(fp0,"\
     No other download methods have been defined yet.  You may\n\
     define an unlimited number of download methods using the lynx.cfg\n\
     file.");

    }
    fclose(fp0);

    /* free off temp copy */
    free(sug_filename);

    return(0);
}
