/*		FILE WRITER				HTFWrite.h
**		===========
**
**	This version of the stream object just writes to a C file.
**	The file is assumed open and left open.
**
**	Bugs:
**		strings written must be less than buffer size.
*/

#include "LYCurses.h"
#include "HTFWriter.h"

#include "HTFormat.h"
#include "HTAlert.h"
#include "HTFile.h"
#include "HTPlain.h"

#include "LYStrings.h"
#include "LYUtils.h"
#include "LYGlobalDefs.h"
#include "LYSignal.h"
#include "LYSystem.h"

PUBLIC char * WWW_Download_File=0; /* contains the name of the temp file
				    * which is being downloaded into
				    */
PUBLIC char LYCancelDownload=FALSE;  /* exported to HTFormat.c in libWWW */

extern char dump_output_immediately; /* if true dump to stdout and quit */
#ifdef VMS
extern BOOLEAN HadVMSInterrupt;      /* flag from cleanup_sig()		*/
#endif /* VMS */

/*		Stream Object
**		------------
*/

struct _HTStream {
	CONST HTStreamClass *	isa;
	
	FILE *			fp;
	char * 			end_command;
	char * 			remove_command;
};


/*_________________________________________________________________________
**
**			A C T I O N 	R O U T I N E S
**  Bug:
**	All errors are ignored.
*/

/*	Character handling
**	------------------
*/

PRIVATE void HTFWriter_put_character ARGS2(HTStream *, me, char, c)
{
    putc(c, me->fp);
}



/*	String handling
**	---------------
**
**	Strings must be smaller than this buffer size.
*/
PRIVATE void HTFWriter_put_string ARGS2(HTStream *, me, CONST char*, s)
{
    fputs(s, me->fp);
}


/*	Buffer write.  Buffers can (and should!) be big.
**	------------
*/
PRIVATE void HTFWriter_write ARGS3(HTStream *, me, CONST char*, s, int, l)
{
    fwrite(s, 1, l, me->fp); 
}




/*	Free an HTML object
**	-------------------
**
**	Note that the SGML parsing context is freed, but the created
**	object is not,
**	as it takes on an existence of its own unless explicitly freed.
*/

PRIVATE void HTFWriter_free ARGS1(HTStream *, me)
{

    fflush(me->fp);
    if (me->end_command) {		/* Temp file */
    	fclose(me->fp);
	if (strcmp(me->end_command, "SaveToFile")) {
	    if(!dump_output_immediately) {
                HTProgress(me->end_command);  /* Tell user what's happening */
	        stop_curses();
	    }
	    system(me->end_command);

            if (me->remove_command) {
	        /* NEVER REMOVE THE FILE unless during an abort!!!*/
	        /* system(me->remove_command); */
		free(me->remove_command);
	    }
	    if(!dump_output_immediately)
	        start_curses();
	}
	free (me->end_command);
    }

    free(me);

    if(dump_output_immediately)
       exit(0);
}

/*	Abort writing
*/

PRIVATE void HTFWriter_abort ARGS2(HTStream *, me, HTError, e)
{
    if(TRACE)
       fprintf(stderr,"HTFWriter_abort called\n");


    fclose(me->fp);
    if (me->end_command) {              /* Temp file */
        if (TRACE) fprintf(stderr,
                "HTFWriter: Aborting: file not executed.\n");
        free (me->end_command);
        if (me->remove_command) {
            system(me->remove_command);
            free(me->remove_command);
        }
    }

    if(WWW_Download_File) { /* get rid of it */
        free(WWW_Download_File);
        WWW_Download_File=0;
    }

    free(me);
}


/*	Structured Object Class
**	-----------------------
*/
PRIVATE CONST HTStreamClass HTFWriter = /* As opposed to print etc */
{		
	"FileWriter",
	HTFWriter_free,
	HTFWriter_abort,
	HTFWriter_put_character, 	HTFWriter_put_string,
	HTFWriter_write
}; 


/*	Subclass-specific Methods
**	-------------------------
*/

PUBLIC HTStream* HTFWriter_new ARGS1(FILE *, fp)
{
    HTStream* me;
    
    if (!fp) return NULL;

    me = (HTStream*)calloc(sizeof(*me),1);
    if (me == NULL) outofmem(__FILE__, "HTML_new");
    me->isa = &HTFWriter;       

    me->fp = fp;
    me->end_command = NULL;
    me->remove_command = NULL;

    return me;
}

/*	Make system command from template
**	---------------------------------
**
**	See mailcap spec for description of template.
*/
/* @@ to be written.  sprintfs will do for now.  */


#ifndef VMS
#define REMOVE_COMMAND "/bin/rm -f %s"
#else
#define REMOVE_COMMAND "delete/noconfirm/nolog %s;"
#endif

/*	Take action using a system command
**	----------------------------------
**
**	originally from Ghostview handling by Marc Andreseen.
**	Creates temporary file, writes to it, executes system command
**	on end-document.  The suffix of the temp file can be given
**	in case the application is fussy, or so that a generic opener can
**	be used.
*/
PUBLIC HTStream* HTSaveAndExecute ARGS3(
	HTPresentation *,	pres,
	HTParentAnchor *,	anchor,	/* Not used */
	HTStream *,		sink)	/* Not used */
{
    char *fnam;
    CONST char * suffix;
    HTStream* me;

#if defined(EXEC_LINKS) || defined(EXEC_SCRIPTS)
    if(pres->quality == 999.0) { /* exec link */
        if (no_exec) {
            statusline("Execution is disabled.");
            sleep(2);
            return HTPlainPresent(pres, anchor, sink);
        }
	if(!local_exec) 
	   if(!strncmp(anchor->address,"file://localhost",16) &&
						local_exec_on_local_files) {
		/* allow it to continue */
	   } else {
		statusline("Execution is not enabled for this file.  See the O)ptions menu. (Press 'o')");
		sleep(2);
		return HTPlainPresent(pres, anchor, sink);
	   }
    }
#endif /* defined(EXEC_LINKS) || defined(EXEC_SCRIPTS) */
    
    me = (HTStream*)calloc(sizeof(*me),1);
    if (me == NULL) outofmem(__FILE__, "Save and execute");
    me->isa = &HTFWriter;  
    
#ifdef NOT
    /* Save the file under a suitably suffixed name */
    
    suffix = HTFileSuffix(pres->rep);
#endif /* NOT */

    fnam = (char *)malloc(64);
    tempname (fnam, 0);  /* lynx routine to create a filename */
#ifdef NOT
#ifdef VMS
    if (suffix) {
        char *cp;
	while ((cp=strchr(fnam, '.')) != NULL)
	    *cp = '_';
    }
#endif /* VMS */
    if (suffix) strcat(fnam, suffix);
#endif /* NOT */
    
    me->fp = fopen (fnam, "w");
    if (!me->fp) {
	HTAlert("Can't open temporary file!");
        free(fnam);
	free(me);
	return NULL;
    }

/*	Make command to process file
*/
    me->end_command = (char *)calloc (
    			(strlen (pres->command) + 10+ 3*strlen(fnam))
    			 * sizeof (char),1);
    if (me == NULL) outofmem(__FILE__, "SaveAndExecute");
    
    sprintf (me->end_command, pres->command, fnam);

/*	Make command to delete file
*/ 
    me->remove_command = (char *)calloc (
    			(strlen (REMOVE_COMMAND) + 10+ strlen(fnam))
    			 * sizeof (char),1);
    if (me == NULL) outofmem(__FILE__, "SaveAndExecute");
    
    sprintf (me->remove_command, REMOVE_COMMAND, fnam);

    free (fnam);
    return me;
}


/*	Format Converter using system command
**	-------------------------------------
*/

/* @@@@@@@@@@@@@@@@@@@@@@ */

/*      Save to a local file   LJM!!!
**      --------------------
**
**      usually a binary file that can't be displayed
**
**      originally from Ghostview handling by Marc Andreseen.
**      Asks the user if he wants to continue, creates a temporary
**      file, and writes to it.  In HTSaveToFile_Free
**      the user will see a list of choices for download
*/
PUBLIC HTStream* HTSaveToFile ARGS3(
        HTPresentation *,       pres,
        HTParentAnchor *,       anchor, /* Not used */
        HTStream *,             sink)   /* Not used */
{
    HTStream * ret_obj;
    char fnam[64];
    char c=0;

    ret_obj = (HTStream*)calloc(sizeof(* ret_obj),1);
    if (ret_obj == NULL) outofmem(__FILE__, "Save To File");
    ret_obj->isa = &HTFWriter;
    ret_obj->remove_command = NULL;
    ret_obj->end_command = NULL;

    if(dump_output_immediately) {
        ret_obj->fp = stdout; /* stdout*/
        return ret_obj;
    }

    LYCancelDownload = FALSE;
    if(HTOutputFormat != HTAtom_for("www/download")) {
        statusline(
      "This file cannot be displayed on this terminal:  D)ownload, or C)ancel");

        while(toupper(c)!='C' && toupper(c)!='D' && c!=7) {
	    c=LYgetch();
#ifdef VMS
	    /** 'C'ancel on Control-C or Control-Y and a 'N'o to the exit query
 	     **/
	    if (HadVMSInterrupt) {
	        HadVMSInterrupt = FALSE;
	        c = 'C';
	    }
#endif /* VMS */
        }

        /** Cancel on 'C', 'c' or Control-G **/
        if(toupper(c)=='C' || c==7) {
            statusline("Cancelling file.");
	    LYCancelDownload = TRUE;
            free(ret_obj);
            return(NULL);
        }
    }

/*	Set up a 'D'ownload
*/
    tempname(fnam,NEW_FILE);

    ret_obj->fp = fopen (fnam, "w");
    if (!ret_obj->fp) {
        HTAlert("Can't open output file! Cancelling");
        free(ret_obj);
        return NULL;
    }

    StrAllocCopy(WWW_Download_File,fnam);

/*	Make command to delete file
*/ 
    ret_obj->remove_command = (char *)calloc (
    			(strlen (REMOVE_COMMAND) + 10+ strlen(fnam))
    			 * sizeof (char),1);
    if (ret_obj == NULL) outofmem(__FILE__, "SaveToFile");
    
    sprintf (ret_obj->remove_command, REMOVE_COMMAND, fnam);

    ret_obj->end_command = (char *)malloc (sizeof(char)*12);
    if (ret_obj == NULL) outofmem(__FILE__, "SaveToFile");
    sprintf(ret_obj->end_command, "SaveToFile");

    statusline("Retrieving file.  - PLEASE WAIT -");

    return ret_obj;
}

/*      Dump output to stdout LJM!!!
**      ---------------------
**
*/
PUBLIC HTStream* HTDumpToStdout ARGS3(
        HTPresentation *,       pres,
        HTParentAnchor *,       anchor, /* Not used */
        HTStream *,             sink)   /* Not used */
{
    HTStream * ret_obj;
    ret_obj = (HTStream*)calloc(sizeof(* ret_obj),1);
    if (ret_obj == NULL) outofmem(__FILE__, "Save To File");
    ret_obj->isa = &HTFWriter;
    ret_obj->remove_command = NULL;
    ret_obj->end_command = NULL;

    ret_obj->fp = stdout; /* stdout*/
    return ret_obj;
}
