#include "LYCurses.h"
#include "HTUtils.h"
#include "HTML.h"
#include "HTAccess.h"
#include "LYUtils.h"
#include "LYGlobalDefs.h"
#include "LYSignal.h"
#include "LYGetFile.h"
#include "LYStrings.h"
#include "LYClean.h"
#include "LYCharSets.h"
#include "LYReadCFG.h"
#include "LYrcFile.h"
#include "LYKeymap.h"

/* ahhhhhhhhhh!! Global variables :-< */
int HTCacheSize = DEFAULT_CACHE_SIZE;  /* number of docs cached in memory */
BOOLEAN LYShowCursor = SHOW_CURSOR;  /* to show or not to show */
char *empty_string = "\0";
int display_lines;  /* number of lines in display */
int www_search_result= -1;
printer_type *printers = NULL;    /* linked list of printers */
download_type *downloaders = NULL;    /* linked list of download options*/
char *log_file_name = NULL;  /* for WAIS log file name in libWWW */
int port_syntax = 1;
char *startfile = STARTFILE; /* the first file */
char *helpfile = HELPFILE; /* the help file */
char indexfile[256];  /* an index file if there is one */
char personal_mail_address[120];  /* the users mail address */
char *display=NULL;
BOOLEAN LYforce_HTML_mode=FALSE;
BOOLEAN LYforce_no_cache=FALSE;
BOOLEAN LYUserSpecifiedURL=TRUE;  /* always true the first time */
BOOLEAN recent_sizechange=FALSE;  /* the window size changed recently? */
BOOLEAN user_mode=NOVICE_MODE;
BOOLEAN dump_output_immediately=FALSE;
BOOLEAN is_www_index=FALSE;
BOOLEAN lynx_mode=NORMAL_LYNX_MODE;
BOOLEAN child_lynx = FALSE;
#if defined(EXEC_LINKS) || defined(EXEC_SCRIPTS)
#ifndef NEVER_ALLOW_REMOTE_EXEC
BOOLEAN local_exec = LOCAL_EXECUTION_LINKS_ALWAYS_ON;
#else
BOOLEAN local_exec = FALSE;
#endif /* NEVER_ALLOW_REMOTE_EXEC */
BOOLEAN local_exec_on_local_files = LOCAL_EXECUTION_LINKS_ON_BUT_NOT_REMOTE;
#endif /* defined(EXEC_LINKS) || defined(EXEC_SCRIPTS) */
BOOLEAN error_logging = MAIL_SYSTEM_ERROR_LOGGING;
BOOLEAN vi_keys = VI_KEYS_ALWAYS_ON;
BOOLEAN emacs_keys = EMACS_KEYS_ALWAYS_ON;
BOOLEAN keypad_mode = DEFAULT_KEYPAD_MODE;
BOOLEAN case_sensitive = CASE_SENSITIVE_ALWAYS_ON;
BOOLEAN telnet_ok = TRUE;
BOOLEAN no_inside_telnet = FALSE;
BOOLEAN no_outside_telnet = FALSE;
BOOLEAN no_suspend = FALSE;
BOOLEAN no_editor = FALSE;
BOOLEAN no_shell = FALSE;
BOOLEAN no_bookmark = FALSE;
BOOLEAN no_option_save = FALSE;
BOOLEAN no_print = FALSE;
BOOLEAN no_download = FALSE;
BOOLEAN no_disk_save = FALSE;
BOOLEAN no_exec = FALSE;
BOOLEAN exec_frozen = FALSE;
BOOLEAN no_goto = FALSE;
BOOLEAN no_file_url = FALSE;
BOOLEAN no_newspost = FALSE;
char editor[256];  /* the name of the current editor */
char bookmark_page[256];   /* the name of the current bookmark page */
int LYlines = 24;
int LYcols = 80;

linkstruct links[MAXLINKS] = {NULL};

histstruct history[MAXHIST] = {NULL};

int nlinks = 0;  /* number of links in memory */
int nhist = 0;   /* number of history entries */
int more = FALSE; /* is there more text to display? */

PRIVATE void FatalProblem PARAMS((int sig));
extern void mainloop();

PUBLIC int main ARGS2(int,argc, char **,argv)
{
    int  i;  /* indexing variable */
    char *terminal;
    char *cp;
    BOOLEAN restrictions_set=FALSE;
    BOOLEAN stack_dump=FALSE;


    /* initialize some variables */
	/* use the home file specified by the environment if it exits */
    terminal=NULL;
    *editor='\0';   /* set it empty */
    *bookmark_page='\0';
    strcpy(indexfile, DEFAULT_INDEX_FILE);
    *personal_mail_address = '\0';

    /* read the lynx.cfg file */
    read_cfg(LYNX_CFG_FILE); 

    /* get WWW_HOME environment variable if it exists) */
    if((cp = getenv("WWW_HOME")) != NULL)
	startfile = cp;

    /*
     * Process arguments - with none, look for the database in STARTDIR,
     * starting with STARTFILE.
     *
     * If a pathname is given, use it as the starting point.  Split it
     * into directory and file components, 'cd' to the directory, and
     * view the file.
     */
    for (i=1; i<argc; i++) {
	if (strncmp(argv[i], "-anonymous", 10) == 0) {
	    if(!restrictions_set)
	        parse_restrictions("default");

        } else if(strncmp(argv[i], "-restrictions", 13) == 0) {
            if((cp=strchr(argv[i],'=')) != NULL)
                parse_restrictions(cp+1);
	    else 
	    {
		/* print help */
		printf("\n\
   USAGE: lynx -restrictions=[option][,option][,option]\n\
   list of options:\n\
   all             restricts all options.\n\
   default         same as commandline option -anonymous.  Disables\n\
                   default services for anonymous users.  Currently set to,\n\
                   all restricted except for: inside_telnet, outside_telnet,\n\
                   and goto.  Defaults settable within userdefs.h\n\
   inside_telnet   disallow telnets for people coming from inside your\n\
                   domain.\n\
   outside_telnet  disallow telnets for people coming from outside your\n\
                   domain.\n\
   shell           disallow shell escapes\n");
	        printf("\
   editor          disallow editing\n\
   bookmark        disallow changing the location of the bookmark file.\n\
   options_save    disallow saving options in .lynxrc\n\
   print           disallow most print options\n\
   goto            disable the 'g' (goto) command.\n\
   file_url        disallow using G)oto to go to file: URL's\n\
   download        disallow saving binary files to disk in the download menu.\n\
   exec            disable execution scripts\n\
   exec_frozen     disallow the user from changing the execution link\n\
   news_post       disallow USENET News posting\n\
                   setting in the O)ptions menu.\n");
		exit(1);
	    }


	} else if(strncmp(argv[i], "-editor", 7) == 0) {
	    if((cp=strchr(argv[i],'=')) != NULL)
	    	strcpy(editor,cp+1);
	    else {
	    	strcpy(editor,argv[i+1]);
		i++;
	    }
	} else if(strncmp(argv[i], "-display", 8) == 0) {

	    char putenv_command[90];

	    if((cp=strchr(argv[i],'=')) != NULL)
	    	display = cp+1;
	    else {
	    	display = argv[i+1];
		i++;
	    }
	    
#ifdef UNIX
	    sprintf(putenv_command,"DISPLAY=%s",display);
	    putenv(putenv_command);
#endif UNIX

	} else if(strncmp(argv[i], "-index", 6) == 0) {
	    if((cp=strchr(argv[i],'=')) != NULL)
	    	strcpy(indexfile, cp+1);
	    else {
	    	strcpy(indexfile, argv[i+1]);
		i++;
	    }

	} else if(strncmp(argv[i], "-cfg", 4) == 0) {
		/* reading the cfg file may overide 
		 * previous settings 
		 */
	    if((cp=strchr(argv[i],'=')) != NULL)
                read_cfg(cp+1);
            else {
                read_cfg(argv[i+1]);
                i++;
            }


	} else if(strncmp(argv[i], "-stack_dump", 11) == 0) {
	    stack_dump = TRUE;

	} else if(strncmp(argv[i], "-cache", 6) == 0) {
	    if((cp=strchr(argv[i],'=')) != NULL)
	    	HTCacheSize = atoi(cp+1);
	    else {
	    	HTCacheSize= atoi(argv[i+1]);
		i++;
	    }

	    /* limit size */
	    if(HTCacheSize < 2) HTCacheSize = 2;

	} else if(strncmp(argv[i], "-vikeys", 7) == 0) {
	    vi_keys = TRUE;

	} else if(strncmp(argv[i], "-emacskeys", 7) == 0) {
	    emacs_keys = TRUE;

	} else if(strncmp(argv[i], "-version", 8) == 0) {
	    printf("\nLynx Version %s\n(c)1994 University of Kansas\nLou Montulli@ukanaix.cc.ukans.edu\n\n",LYNX_VERSION);
	    exit(0);

	} else if(strncmp(argv[i], "-case", 5) == 0) {
	    case_sensitive = TRUE;

	} else if(strncmp(argv[i], "-dump", 5) == 0) {
	    dump_output_immediately = TRUE;
	    LYcols=80;

	} else if(strncmp(argv[i], "-source", 7) == 0) {
	    dump_output_immediately = TRUE;
	    HTOutputFormat = HTAtom_for("www/dump");
	    LYcols=999;

	} else if(strncmp(argv[i], "-force_html", 11) == 0) {
	    LYforce_HTML_mode = TRUE;

	} else if (strncmp(argv[i], "-trace", 6) == 0) {
	    WWW_TraceFlag = TRUE;

	} else if (strncmp(argv[i], "-linknums", 6) == 0) {
	    keypad_mode = LINKS_ARE_NUMBERED;

 	} else if (strncmp(argv[i], "-noprint", 8) == 0) {
	    no_print=TRUE;

 	} else if (strncmp(argv[i], "-print", 6) == 0) {
	    no_print=FALSE;

#if defined(EXEC_LINKS) || defined(EXEC_SCRIPTS)
 	} else if (strncmp(argv[i], "-exec", 5) == 0) {
#ifndef NEVER_ALLOW_REMOTE_EXEC
	    local_exec=TRUE;
#else
	    local_exec_on_local_files=TRUE;
#endif /* NEVER_ALLOW_REMOTE_EXEC */

 	} else if (strncmp(argv[i], "-locexec", 8) == 0) {
	    local_exec_on_local_files=TRUE;

 	} else if (strncmp(argv[i], "-noexec", 7) == 0) {
	    local_exec=FALSE;
#endif /* defined(EXEC_LINKS) || defined(EXEC_SCRIPTS) */

 	} else if (strncmp(argv[i], "-child", 6) == 0) {
	    child_lynx=TRUE;

 	} else if (strncmp(argv[i], "-nolog", 6) == 0) {
	    error_logging=TRUE;

	} else if(strncmp(argv[i], "-show_cursor", 12) == 0) {
	    LYShowCursor = TRUE;

	} else if (strncmp(argv[i], "-term", 5) == 0) {
	    if((cp=strchr(argv[i],'=')) != NULL)
		terminal = cp+1;
	    else {
	    	terminal = argv[i+1];
		i++;
	    }

 	} else if (strncmp(argv[i], "-telnet", 7) == 0) {
	    telnet_ok=FALSE;

	} else if (strncmp(argv[i], "-", 1) == 0) {
	    printf("Usage: %s [options] [file]\n",argv[0]);
	    printf("Options are:\n");
	    printf("    -anonymous       used to specify the anonymous account\n");
	    printf("    -case            enable case sensitive user searching\n");
	    printf("    -cache=NUMBER    NUMBER of documents cached in memory. (default is %d\n",DEFAULT_CACHE_SIZE);
	    printf("    -cfg=FILENAME    specifies a lynx.cfg file other than the default.\n");
	    printf("    -display=DISPLAY set the display variable for X execed programs\n");
	    printf("    -dump            dump the first file to stdout and exit\n");
	    printf("    -editor=EDITOR   enable edit mode with specified editor\n");
	    printf("    -emacskeys       enable emacs-like key movement\n");
#if defined(EXEC_LINKS) || defined(EXEC_SCRIPTS)
#ifndef NEVER_ALLOW_REMOTE_EXEC
	    printf("    -exec            enable local program execution\n");
#endif /* NEVER_ALLOW_REMOTE_EXEC */
	    printf("    -locexec         enable local program execution from local files only\n");
	    printf("    -noexec          disable local program execution (DEFAULT)\n");
#endif /* defined(EXEC_LINKS) || defined(EXEC_SCRIPTS) */
	    printf("    -force_html      forces the first document to be interpreted as HTML\n");
	    printf("    -help            print this usage message\n");
	    printf("    -index=URL       set the default index file to URL\n");
	    printf("    -noprint         disable print functions\n");
	    printf("    -print           enable print functions (DEFAULT)\n");
	    printf("    -restrictions=[options]  use -restrictions to see list\n");
	    printf("    -show_cursor     don't hide the curser in the lower right corner\n");
	    printf("    -source          dump the source of the first file to stdout and exit\n");
	    printf("    -telnet          disable telnets\n");
	    printf("    -term=TERM       set terminal type to TERM\n");
	    printf("    -trace           turns on WWW trace mode\n");
	    printf("    -vikeys          enable vi-like key movement\n");
	    printf("    -version         prints version information\n");
	    exit(0);
	} else {	/* alternate database path */

		startfile = argv[i];
	}
    }

    /* read the rc file */
    read_rc();
#if defined(EXEC_LINKS) || defined(EXEC_SCRIPTS)
#ifdef NEVER_ALLOW_REMOTE_EXEC
    if (local_exec) {
        local_exec = FALSE;
	local_exec_on_local_files=TRUE;
    }
#endif /* NEVER_ALLOW_REMOTE_EXEC */
#endif /* defined(EXEC_LINKS) || defined(EXEC_SCRIPTS) */

    if (emacs_keys)
        set_emacs_keys();
 
    if (vi_keys)
        set_vi_keys();
 
    if (keypad_mode == NUMBERS_AS_ARROWS)
        set_numbers_as_arrows();

    /* disable news posting if no posting command */
    if (INEWS == "" || !strcasecomp(INEWS,"none"))
        no_newspost = TRUE;

#ifdef VMS
    set_vms_keys();
#endif /* VMS */


    /* trap interrupts */    
    if(!dump_output_immediately)
        (void) signal (SIGHUP, cleanup_sig);
    (void) signal (SIGTERM, cleanup_sig);
#ifdef SIGWINCH
    (void) signal (SIGWINCH, size_change);
#endif
#ifndef VMS
    if(!TRACE && !dump_output_immediately && !stack_dump) {
        (void) signal (SIGINT, cleanup_sig);
        (void) signal (SIGBUS, FatalProblem);
        (void) signal (SIGSEGV, FatalProblem);
        (void) signal (SIGILL, FatalProblem);
        /* Since we're doing lots of TCP, just ignore SIGPIPE altogether. */
        (void) signal (SIGPIPE, SIG_IGN);
    }
#endif

    /* set up the proper character set */
    HTMLUseCharacterSet(current_char_set);

	/* if its not a URL then make it one */
    if(!is_url(startfile)) {
	    /* rewrite the file as a URL */
	   char *old_startfile=startfile; 

	   startfile = NULL;  /* so StrAllocCopy doesn't free it */
#ifdef VMS
	   StrAllocCopy(startfile,"file://localhost/");
	   if(strchr(old_startfile,':') == NULL &&
	      strchr(old_startfile,'[') == NULL) {
#else
	   StrAllocCopy(startfile,"file://localhost");
	   if(*old_startfile != '/') {
#endif /* VMS */
               	char curdir[256];
#ifdef NEXT
      		getwd (curdir);
#else
    		getcwd (curdir, DIRNAMESIZE);
#endif /* NEXT */
		StrAllocCat(startfile,curdir);

#ifndef VMS
		StrAllocCat(startfile,"/");
#endif
	   }
	   StrAllocCat(startfile,old_startfile);
    }

    /*
     * anonymous may already be set above by command line options.
     * so this just sets the correct options.
     */
#ifndef VMS
#ifdef NO_CUSERID
    if (STREQ(((char *)getlogin()==NULL ? " " : getlogin()) , ANONYMOUS_USER)) {
#else
    if (STREQ((char *)cuserid((char *) NULL), ANONYMOUS_USER)) {
#endif
	if(!restrictions_set)
	    parse_restrictions("default");
    }
#endif /* VMS */

    if(!inlocaldomain())
        telnet_ok = !no_outside_telnet;
    else
        telnet_ok = !no_inside_telnet;


#ifdef SIGTSTP
    if(no_suspend)
	signal(SIGTSTP,SIG_IGN);
#endif SIGTSTP

    /*
     * here's where we do all the work
     */
    if(dump_output_immediately) {
	mainloop();
        (void) signal (SIGHUP, SIG_IGN);
        (void) signal (SIGTERM, SIG_IGN);
        (void) signal (SIGINT, SIG_IGN);
    } else {
 	if (setup(terminal)) {
	    mainloop();
	    cleanup();
	}
    }
 
    exit(0);
    /* NOTREACHED */
}

#ifndef VMS

static void FatalProblem ARGS1(int,sig)
{
fprintf (stderr, "\r\n\
Congratulations, you have found a bug in Lynx Ver. %s\r\n\
If a core file was generated in your directory,\r\n\
please run 'dbx lynx' (or 'dbx /path/lynx' if the\r\n\
lynx executable is not in your current directory)\r\n\
and then type:\r\n",LYNX_VERSION);
fprintf (stderr, "\
  dbx> where\r\n\
and mail the results, and a description of\r\n\
what you were doing at the time of the error\r\n\
including the URL of the document that caused the crash,\r\n\
to montulli@ukanaix.cc.ukans.edu.\r\n\
Thank you for your support.\r\n\n\
...exiting Lynx now with signal:%d\r\n\n",sig);

    /* ignore further interrupts */     /*  mhc: 11/2/91 */
    (void) signal (SIGHUP, SIG_IGN);
    (void) signal (SIGTERM, SIG_IGN);
#ifndef VMS  /* use ttclose() from cleanup() for VMS */
    (void) signal (SIGINT, SIG_IGN);
#endif
     (void) signal (SIGBUS, SIG_IGN);
     (void) signal (SIGSEGV, SIG_IGN);
     (void) signal (SIGILL, SIG_IGN);

  cleanup_sig(sig);
  signal (SIGBUS, 0);
  signal (SIGSEGV, 0);
  signal (SIGILL, 0);
  abort();  /* exit and dump core */
}
#endif /* VMS */
