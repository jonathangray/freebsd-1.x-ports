#include "HTUtils.h"
#include "LYUtils.h"
#include "LYStrings.h"
#include "LYBookmark.h"
#include "LYGlobalDefs.h"

#if defined(VMS) || defined(SCO)
#include <time.h>
#else /* UNIX */
#include <sys/time.h>
#endif /* VMS */


PRIVATE BOOLEAN is_mosaic_hotlist=FALSE;
PRIVATE char * convert_mosaic_bookmark_file PARAMS((char *filename_buffer));

/* tries to open the bookmark file for reading.
 * if successful the file is closed and the filename
 * is returned and the URL is given in name
 */
PUBLIC char * get_bookmark_filename ARGS1(char **,URL)
{
    char URL_buffer[256];
    static char filename_buffer[256];
    char string_buffer[256];
    FILE *fp;

    if(!bookmark_page) {
	statusline("Bookmark file is not defined. Use 'O' to see options");
	sleep(1);
	return(NULL);
    }

    /* see if it is in the home path */
#ifdef VMS
    sprintf(filename_buffer,"sys$login:%s", bookmark_page);
#else
    sprintf(filename_buffer,"%s/%s",getenv("HOME"), bookmark_page);
#endif /* VMS */
    if((fp = fopen(filename_buffer,"r")) != NULL) {
	goto success;
    }

    /* see if we can open it raw */
    if((fp = fopen(bookmark_page,"r")) != NULL) {
	strcpy(filename_buffer, bookmark_page);
	goto success;
    } 

    /* failure */
    return(NULL);

success:
    /* we now have the file open.  Check if it is a mosaic
     * hotlist
     */
    if(fgets(string_buffer, 255, fp) &&
		!strncmp(string_buffer, "ncsa-xmosaic-hotlist-format-1", 29)) {
	char * newname;
	/* it is a mosaic hotlist file */
	is_mosaic_hotlist=TRUE;
	fclose(fp);
	newname = convert_mosaic_bookmark_file(filename_buffer);
#ifdef VMS
    sprintf(URL_buffer,"file://localhost/%s", newname);
#else
    sprintf(URL_buffer,"file://localhost%s", newname);
#endif /* VMS */

    } else {
	fclose(fp);
	is_mosaic_hotlist=FALSE;
#ifdef VMS
    sprintf(URL_buffer,"file://localhost/%s", filename_buffer);
#else
    sprintf(URL_buffer,"file://localhost%s", filename_buffer);
#endif /* VMS */
    }

    StrAllocCopy(*URL, URL_buffer);
    return(filename_buffer);  /* bookmark file exists */

} /* big end */

PRIVATE char * convert_mosaic_bookmark_file ARGS1(char *,filename_buffer)
{
    static char *newfile=NULL;
    FILE *fp, *nfp;
    char buf[BUFSIZ];
    int line= -2;
    char *endline;

    if(newfile == NULL) {
	newfile = (char *) malloc(128);
        tempname(newfile, NEW_FILE);
#ifndef VMS
    } /* otherwise reuse the existing tempfile */
#else
    } else {
        remove(newfile);   /* put VMS code to remove duplicates here */
    }
#endif /* VMS */


    if((nfp = fopen(newfile, "w")) == NULL) {
	statusline("Unable to open tempfile for X Mosaic hotlist conversion");
	sleep(2);
	return ("");
    }

    if((fp = fopen(filename_buffer, "r")) == NULL)
	return ("");  /* should always open */

    fprintf(nfp,"<title>%s</title>\n",MOSAIC_BOOKMARK_TITLE);
    fprintf(nfp,"\
     This file is an HTML representation of the X Mosaic hotlist file.\n\
     Outdated or invalid links may be removed by using the\n\
     remove bookmark command, it is usually the 'R' key but may have\n\
     been remapped by you or your system administrator.\n\n<p>\n<ol>\n");

    while (fgets(buf, sizeof(buf), fp) != NULL) {
	if(line >= 0) {
	    endline = &buf[strlen(buf)-1];
	    if(*endline == '\n')
		*endline = '\0';
	    if((line % 2) == 0) { /* even lines */
		if(*buf != '\0') {
		    strtok(buf," "); /* kill everything after the space */
	            fprintf(nfp,"<LI><a href=\"%s\">",buf); /* the URL */
		}
	    } else { /* odd lines */
	        fprintf(nfp,"%s</a>\n",buf);  /* the title */
	    }
	} 
	/* else - ignore the line (this gets rid of first two lines) */
	line++;
    }
    fclose(nfp);
    fclose(fp);
    return(newfile);
}

PUBLIC void save_bookmark_link ARGS2(char *,address, char *,title)
{
	FILE *fp;
	BOOLEAN first_time=FALSE;
	char * filename;
	char * bookmark_URL=0;
	char filename_buffer[256];

	filename = get_bookmark_filename(&bookmark_URL);
	if(bookmark_URL)
	    free(bookmark_URL); /* don't need it */

	if(filename == NULL) {
	    first_time= TRUE;
	    /* try in the home directory first */
#ifdef VMS
    	    sprintf(filename_buffer,"sys$login:%s", bookmark_page);
#else
    	    sprintf(filename_buffer,"%s/%s",getenv("HOME"), bookmark_page);
#endif /* VMS */
    	    if((fp = fopen(filename_buffer,"w")) == NULL) {
		/* try it raw */
    		if((fp = fopen(bookmark_page,"r")) == NULL) {
	            statusline("ERROR - unable to open bookmark file");
	            sleep(1);
	            return;
		}
	    }

	} else {
	    if((fp = fopen(filename,"a+")) == NULL) {
	       statusline("ERROR - unable to open bookmark file");
	       sleep(1);
	       return;
	    }
	}

	if(first_time) {
	    fprintf(fp,"<title>%s</title>\n",BOOKMARK_TITLE);
	    fprintf(fp,"\
     You can delete links using the new remove bookmark command.\n\
     it is usually the 'R' key but may have been remapped by you or\n\
     your system administrator.<br>\n\
     This file may also be edited with a standard text editor.\n\
     Outdated or invalid links may be removed by simply deleting\n\
     the line the link appears on in this file.\n\
     Please refer to the Lynx documentation or help files\n\
     for the HTML link syntax.\n\n<p>\n<ol>\n");
	}

	if(is_mosaic_hotlist) {
	    time_t NowTime = time (NULL);
	    char *TimeString = (char *)ctime (&NowTime);
		/* TimeString has a \n at the end */
	    fprintf(fp,"%s %s%s\n", address, TimeString, title);
	} else {
	    fprintf(fp,"<LI><a href=\"%s\">%s</a>\n",address, title);
	}

	fclose(fp);

	statusline("Done!");
	sleep(1);
}
	
PUBLIC void remove_bookmark_link ARGS1(int,cur)
    {
        FILE *fp, *nfp;
        char buf[BUFSIZ];
        int n;
	char newfile[128];
        char *filename;
        char *URL=0;

        if(TRACE)
	    fprintf(stderr,"remove_bookmark_link: deleting link number: %d\n",
									  cur);

	filename = get_bookmark_filename(&URL);
	if(URL)
	   free(URL); /* don't need it */

	fp = fopen(filename, "r");
        tempname(newfile, NEW_FILE);
        nfp = fopen(newfile, "w");
        if (fp == NULL || nfp == NULL) {
             statusline("Unable to open bookmark file for deletion of link.");
             sleep(2);
             return;
        }
   
	if(is_mosaic_hotlist) {
	    int del_line = cur*2;  /* two lines per entry */
	    n = -3;  /* skip past cookie and name lines */
            while (fgets(buf, sizeof(buf), fp) != NULL) {
		n++;
		if(n == del_line || n == del_line+1) 
		    continue;  /* remove two lines */
                if (fputs(buf, nfp) == EOF)
                        goto failure;
	    }

	} else {
            n = -1;
            while (fgets(buf, sizeof(buf), fp) != NULL) {
                if (n < cur && LYstrstr(buf, "<a href=")) {
                        if (++n == cur)
                                continue;
                }
                if (fputs(buf, nfp) == EOF)
                        goto failure;
            }
            fclose(fp);
	}

     	if(TRACE)
	    fprintf(stderr,"remove_bookmark_link: files: %s %s\n",
							newfile, filename);
	
        if (fclose(nfp) != EOF
#ifdef ODD_RENAME /* dont use with VMS! */
				) {
		char buffer[512];
		sprintf(buffer,"mv %s %s",newfile, filename);
		system(buffer);
#else
        			&& rename(newfile, filename) != -1) {
#endif /* ODD_RENAME */

#ifdef VMS
		char VMSfilename[256];
		/* purge lower version of file */
		sprintf(VMSfilename, "%s;-1", filename);
        	while (delete(VMSfilename) == 0) ;
#endif
                return;
	} else {
            statusline("Error closing and renaming files. ");
	    if(TRACE)
		perror("renaming the file");
	    sleep(1);
	}
	   
      failure:
        statusline("Bookmark deletion failed.");
        sleep(2);
        fclose(fp);
        remove(newfile);
    }

