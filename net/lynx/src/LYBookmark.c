#include "HTUtils.h"
#include "LYUtils.h"
#include "LYStrings.h"
#include "LYGlobalDefs.h"

PUBLIC void save_bookmark_link ARGS1(document *,doc)
{
	FILE *fp;
	BOOLEAN first_time=FALSE;
	char name_buffer[256];

	*name_buffer = '\0';

	if(*bookmark_page == '\0') {
	    statusline("ERROR - No bookmark page defined");
	    sleep(1);
	    return;
	}

#ifdef VMS
	if(NULL != strchr(bookmark_page,':')) {
#else
	if(*bookmark_page == '/') {
#endif /* VMS */
	    if((fp = fopen(bookmark_page,"r")) == NULL) {
		/* first time home page */
	        first_time= TRUE;
	    } else
	        fclose(fp);
	} else {
#ifdef VMS
	    sprintf(name_buffer,"sys$login:%s",bookmark_page);
#else
	    sprintf(name_buffer,"%s/%s",getenv("HOME"),bookmark_page);
#endif /* VMS */
	    if((fp = fopen(name_buffer,"r")) == NULL) {
		/* first time home page */
	        first_time= TRUE;
	    } else
	        fclose(fp);
	}

	if(*name_buffer == '\0') {
	    if((fp = fopen(bookmark_page,"a+")) == NULL) {
	       statusline("ERROR - unable to open bookmark page");
	       sleep(1);
	       return;
	    }
	} else {
	    if((fp = fopen(name_buffer,"a+")) == NULL) {
	       statusline("ERROR - unable to open bookmark page");
	       sleep(1);
	       return;
	    }
	}

	if(first_time) {
	    fprintf(fp,"<title>Bookmark Page</title>\n");
	    fprintf(fp,"\
     This file may be edited with a standard text editor.\n\
     Outdated or invalid links may be removed by simply deleting\n\
     the line the link appears on in this file.\n\
     Please refer to the Lynx documentation or help files\n\
     for the HTML link syntax.\n\n<p>");
	}

	fprintf(fp,"\n<li><a href=\"%s\">%s</a>",doc->address, doc->title);

	fclose(fp);

	statusline("Done!");
	sleep(1);
}
	
PUBLIC int get_bookmark_filename ARGS1(char **,name)
{
    static char name_buffer[256];
    char curdir[256];
    FILE *fp;

#ifdef NEXT
    getwd (curdir);
#else
    getcwd (curdir, DIRNAMESIZE);
#endif /* NEXT */

    if(*bookmark_page == '\0') {
	statusline("Bookmark page is not defined. Use 'O' to see options");
	sleep(1);
	return(FALSE);
    }

	/* see if it is in the home path */
#ifdef VMS
        sprintf(name_buffer,"sys$login:%s", bookmark_page);
#else
        sprintf(name_buffer,"%s/%s",getenv("HOME"), bookmark_page);
#endif /* VMS */
	if((fp = fopen(name_buffer,"r")) != NULL) {
	   fclose(fp);
#ifdef VMS
	   sprintf(name_buffer,"file://localhost/sys$login:%s", bookmark_page);
#else
	   sprintf(name_buffer,"file://localhost%s/%s",getenv("HOME"),
								bookmark_page);
#endif /* VMS */
	   *name = name_buffer;
	   if(TRACE)
		fprintf(stderr,"get_bookmark_page: %s\n",*name);
	   return(TRUE);  /* bookmark file exists */
	}

    /* see if we can open it raw */
    if((fp = fopen(bookmark_page,"r")) != NULL) {
	fclose(fp);
#ifdef VMS
	sprintf(name_buffer,"file://localhost/%s",bookmark_page);
#else
	sprintf(name_buffer,"file://localhost%s",bookmark_page);
#endif /* VMS */
	*name = name_buffer;
   	return(TRUE);  /* bookmark file exists */
    } 

       statusline("Unable to open Home page, use 'a' to save a link");
       sleep(1);
       return(FALSE);

} /* big end */

#ifdef NOT
PUBLIC void remove_bookmark_link ARGS4(FILE *,in_file, char *,filename, 
				       int,cur, int,cur_offset)
{
    int line_to_delete;
    long source, target;
    char deadbuff[128];
    int byteread;

    line_to_delete = cur_offset + links[cur].ly;

	/* Code below from Garrett Blythe */
	/*
	 * 	Open file in binary mode for exact seeks
	 */
	fclose(in_file);
	in_file = fopen(filename, "rb+");

	/*
	 * 	Find line to be deleted
	 */
	while(--line_to_delete)
		fgets(deadbuff, 128, in_file);

	/*
	 * 	Remember target position
	 *	Read in line to remove
	 *	Remember source position
	 */
	target = ftell(in_file);
	fgets(deadbuff, 128, in_file);
	source = ftell(in_file);

	while(!feof(in_file))
	{
		/*
		 * 	Find source
		 *	Read character
		 *	Remember source
		 */
		fseek(in_file, source, SEEK_SET);
		if(EOF == (byteread = fgetc(in_file)))
			break;
		source = ftell(in_file);


		/*
		 * 	Find target
		 *	Write character
		 *	Remember target
		 */
		fseek(in_file, target, SEEK_SET);
		fputc(byteread, in_file);
		target = ftell(in_file);
	}


	/*
	 *	Write over rest of file
	 *	Open file in standard read mode
	 */
	fseek(in_file, 0, SEEK_END);
	source = ftell(in_file);
	while(source >= target)
	{
		fputc(' ', in_file);
		source--;
		fseek(in_file, source, SEEK_SET);
	}
	fclose(in_file);
	in_file = fopen(filename, "r");
}
#endif /* NOT */
