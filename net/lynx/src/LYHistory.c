#include "HTUtils.h"
#include "LYUtils.h"
#include "LYHistory.h"
#include "LYPrint.h"
#include "LYDownload.h"
#include "LYGlobalDefs.h"

#ifdef DIRED_SUPPORT
#include "LYUpload.h"
#include "LYLocal.h"
#endif
 
/*
 * push the current filename, link and line number onto the history list
 */

PUBLIC void push ARGS1(document *,doc)
{

    if( *doc->address == '\0')  /* dont push null file names */
	return;

    /* don't push the history, download, or printer lists */
    if(!strcmp(doc->title, HISTORY_PAGE_TITLE) ||
		!strcmp(doc->title, PRINT_OPTIONS_TITLE) ||
		!strcmp(doc->title, DOWNLOAD_OPTIONS_TITLE) )
	return;

#ifdef DIRED_SUPPORT
    if(!strcmp(doc->title, DIRED_MENU_TITLE) ||
		!strcmp(doc->title, UPLOAD_OPTIONS_TITLE) )
	return;
#endif

    if(nhist>1 && STREQ(history[nhist-1].address, doc->address)) 
        return;  /* file is identical to one before it don't push it */

    if(nhist>2 && STREQ(history[nhist-2].address, doc->address)) {
	  nhist--; /* pop one off the stack */
          return;  /* file is identical to one two before it don't push it */
    }

    if (nhist<MAXHIST)  {
	history[nhist].link = doc->link;
	history[nhist].page = doc->line;
	history[nhist].title = 0;
	StrAllocCopy(history[nhist].title, doc->title);
	history[nhist].address = 0;
	StrAllocCopy(history[nhist].address, doc->address);
	history[nhist].post_data = 0;
	StrAllocCopy(history[nhist].post_data, doc->post_data);
	history[nhist].post_content_type = 0;
	StrAllocCopy(history[nhist].post_content_type, doc->post_content_type);
	nhist++;

        if(TRACE)
    	    fprintf(stderr,"\npush: address:%s\n      title:%s\n",
						doc->address,doc->title);
    }
}


/*
 * pop the previous filename, link and line number from the history list
 */
PUBLIC void pop ARGS1(document *,doc)
{
 
    if (nhist>0) {
	nhist--;
	doc->link = history[nhist].link;
	doc->line = history[nhist].page;
	free_and_clear(&doc->title);
	doc->title = history[nhist].title;    /* will be freed later */
	free_and_clear(&doc->address);
	doc->address = history[nhist].address;  /* will be freed later */
	free_and_clear(&doc->post_data);
	doc->post_data = history[nhist].post_data;
	free_and_clear(&doc->post_content_type);
	doc->post_content_type = history[nhist].post_content_type;

        if(TRACE)
	    fprintf(stderr,"pop: address:%s\n     title:%s\n",
						doc->address,doc->title);

    }
}

/*
 * pop the specified hist entry, link and line number from the history list
 * but don't actually remove the entry, just return it.
 * this procedure is badly named :)
 */
PUBLIC void pop_num ARGS2(int,number, document *,doc)
{
    if (nhist >= number) {
	doc->link = history[number].link;
	doc->line = history[number].page;
	StrAllocCopy(doc->title, history[number].title);
	StrAllocCopy(doc->address, history[number].address);
	StrAllocCopy(doc->post_data, history[nhist].post_data);
	StrAllocCopy(doc->post_content_type, history[nhist].post_content_type);
    }
}

/*
 * This procedure outputs the history buffer into a temporary file
 *  
 */

PUBLIC void showhistory ARGS1(char **,newfile)
{
	int x=0;
        static char *tmpfile=NULL;
	static char hist_filename[256];
	FILE *fp0;

	if(tmpfile == NULL) {
	    tmpfile = (char *) malloc(128);
	    tempname(tmpfile,NEW_FILE);
#ifndef VMS
	} 
#else
	} else {
	    remove(tmpfile);  /* put VMS code to remove duplicates here */
	}
#endif /* VMS */

	if((fp0 = fopen(tmpfile,"w")) == NULL) {
		perror("Trying to open history file\n");
		exit(1);
	}

	/* make the file a URL now */
#ifdef VMS
	sprintf(hist_filename,"file://localhost/%s",tmpfile);
#else
	sprintf(hist_filename,"file://localhost%s",tmpfile);
#endif /* VMS */
	StrAllocCopy(*newfile, hist_filename);
	LYforce_HTML_mode=TRUE; /* force this file to be HTML */
	LYforce_no_cache=TRUE; /* force this file to be new */

	fprintf(fp0,"<head><title>%s</title></head><body>\n",
							HISTORY_PAGE_TITLE);

	fprintf(fp0,"<h1>YOU HAVE REACHED THE HISTORY PAGE</h1>\n<pre>");
        for(x=nhist-1; x >= 0; x--) {	

		/* the number of the document in the hist stack
		 * and its name in a link
		 */
	   fprintf(fp0,
		"\n\n  %d.  -- You selected:  <a href=\"LYNXHIST:%d\">%s</a>", 
		x, x,
	       (history[x].title!=NULL ? history[x].title : "no title"));
	}

	fprintf(fp0,"</pre>");

	fclose(fp0);

}

/* 
 * this is a kludge to make the history page seem like any other type of file
 * since more info is needed than can be provided by the normal link structure
 * I saved out the history number to a special URL
 * 
 * the info looks like:
 *  LYNXHIST:#
 */


PUBLIC void historytarget ARGS1(document *,newdoc)
{
    int number = atoi(newdoc->address+9);

    pop_num(number, newdoc);    

    if(number != 0)
	StrAllocCat(newdoc->title," (From History)");
}
