#include "HTUtils.h"
#include "LYUtils.h"
#include "LYHistory.h"
#include "LYPrint.h"
#include "LYDownload.h"
#include "LYGlobalDefs.h"
 
/*
 * push the current filename, link and line number onto the history list
 */

PUBLIC void push ARGS1(document *,doc)
{
    char *temp_hightext, *temp_hfname;

    if( *doc->address == '\0')  /* dont push null file names */
	return;

    /* don't push the history, download, or printer lists */
    if(!strcmp(doc->title, HISTORY_PAGE_TITLE) ||
		!strcmp(doc->title, PRINT_OPTIONS_TITLE) ||
		!strcmp(doc->title, DOWNLOAD_OPTIONS_TITLE) )
	return;

    if(nhist>1 && STREQ(history[nhist-1].hfname, doc->address) &&
        history[nhist-1].hpageno == doc->line )
        return;  /* file is identical to one before it don't push it */

    if(nhist>2 && STREQ(history[nhist-2].hfname, doc->address) &&
        history[nhist-2].hpageno == doc->line ) {
	  nhist--; /* pop one off the stack */
          return;  /* file is identical to one two before it don't push it */
    }

    if (nhist<MAXHIST)  {
        /* copy hightext and hfname to tempory holdings
         * the doc-> address and title are sometimes the same
         * variable since they may have come from the history
         * list.  make hightext and hfname NULL so that they
         * don't get free'd by StrAllocCopy and free them
         * manually after the copy!
         */
        temp_hightext = history[nhist].hightext;
        temp_hfname = history[nhist].hfname;
        history[nhist].hightext = NULL;
        history[nhist].hfname = NULL;

	history[nhist].hlinkno = doc->link;
	history[nhist].hpageno = doc->line;
	StrAllocCopy(history[nhist].hightext, doc->title);
	StrAllocCopy(history[nhist].hfname, doc->address);
	nhist++;

	if(temp_hightext!=NULL)
	    free(temp_hightext);
	if(temp_hfname!=NULL)
	    free(temp_hfname);

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
	doc->link = history[nhist].hlinkno;
	doc->line = history[nhist].hpageno;
	doc->title = history[nhist].hightext;
	doc->address = history[nhist].hfname;

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
    if (nhist>= number) {
	doc->link = history[number].hlinkno;
	doc->line = history[number].hpageno;
	doc->title = history[number].hightext;
	doc->address = history[number].hfname;
    }
}

/*
 * This procedure outputs the history buffer into a temporary file
 *  
 */

PUBLIC void showhistory ARGS1(char **,newfile)
{
	int x=0;
        char tmpfile[256];
	static char hist_filename[256];
	FILE *fp0;

	*newfile = 0;  /* don't free current value */

	tempname(tmpfile,NEW_FILE);

	if((fp0 = fopen(tmpfile,"wb")) == NULL) {
		perror("Trying to open history file\n");
		exit(1);
	}

	/* make the file a URL now */
#ifdef VMS
	sprintf(hist_filename,"file://localhost/%s",tmpfile);
#else
	sprintf(hist_filename,"file://localhost%s",tmpfile);
#endif /* VMS */
	*newfile = hist_filename;
	LYforce_HTML_mode=TRUE; /* force this file to be HTML */

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
	       (history[x].hightext!=NULL ? history[x].hightext : "no title"));
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
