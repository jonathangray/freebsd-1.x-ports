/* exp_main_sub.c - miscellaneous subroutines for Expect or Tk main() */

#include "expect_cf.h"
#include <stdio.h>
#include <errno.h>
#include "tcl.h"
#include "tclInt.h"
#include "exp_rename.h"
#include "exp_prog.h"
#include "exp_command.h"
#include "exp_tty_in.h"
#include "exp_log.h"
#include "exp_event.h"
#ifdef TCL_DEBUGGER
#include "Dbg.h"
#endif

#ifdef __CENTERLINE__
#undef	VERSION
#define	VERSION		"5.0.3"		/* I give up! */
					/* It is not necessary that number */
					/* be accurate.  It is just here to */
					/* pacify Centerline which doesn't */
					/* seem to be able to get it from */
					/* the Makefile. */
#undef	SCRIPTDIR
#define SCRIPTDIR	"example/"
#undef	EXECSCRIPTDIR
#define EXECSCRIPTDIR	"example/"
#endif
static char exp_version[] = VERSION;
#define NEED_TCL_MAJOR		7
#define NEED_TCL_MINOR		0

char *exp_argv0 = "this program";	/* default program name */
void (*exp_app_exit)() = 0;
void (*exp_event_exit)() = 0;
FILE *exp_cmdfile = 0;
char *exp_cmdfilename = 0;
int exp_cmdlinecmds = FALSE;
int exp_interactive =  FALSE;
int exp_buffer_command_input = FALSE;
int exp_fgets();

Tcl_Interp *exp_interp;	/* for use by signal handlers who can't figure out */
			/* the interpreter directly */
int exp_tcl_debugger_available = FALSE;

int exp_getpid;

static void
usage(interp)
Tcl_Interp *interp;
{
	errorlog("usage: expect [-di] [-c cmds] [[-f] cmdfile] [args]\r\n");
	exp_exit(interp,1);
}

void
exp_exit(interp,status)
Tcl_Interp *interp;
int status;
{
	/* use following checks to prevent recursion in exit handlers */
	static int did_event_exit = FALSE;

	if (!interp) {
		/* if no interp handy (i.e., called from interrupt handler) */
		/* use last one created - it's a hack but we're exiting */
		/* ungracefully to begin with */
		interp = exp_interp;
	}

	exp_exit_handlers(interp);

	if (exp_event_exit) {
		if (!did_event_exit) {
			did_event_exit = TRUE;
			(*exp_event_exit)(interp);
		} else {
			debuglog("event exit handler called recursively - forcing exit\r\n");
		}
	}

	Tcl_DeleteInterp(interp);

	exit(status);
}

void
exp_exit_handlers(interp)
Tcl_Interp *interp;
{
	extern int exp_forked;

	/* use following checks to prevent recursion in exit handlers */
	static int did_app_exit = FALSE;
	static int did_expect_exit = FALSE;

	if (!interp) {
		/* if no interp handy (i.e., called from interrupt handler) */
		/* use last one created - it's a hack but we're exiting */
		/* ungracefully to begin with */
		interp = exp_interp;
	}

	if (!did_expect_exit) {
		did_expect_exit = TRUE;
		/* called user-defined exit routine if one exists */
		if (exp_onexit_action) {
			int result = Tcl_GlobalEval(interp,exp_onexit_action);
			if (result != TCL_OK) exp_background_error(interp);
		}
	} else {
		debuglog("onexit handler called recursively - forcing exit\r\n");
	}

	if (exp_app_exit) {
		if (!did_app_exit) {
			did_app_exit = TRUE;
			(*exp_app_exit)(interp);
		} else {
			debuglog("application exit handler called recursively - forcing exit\r\n");
		}
	}

	if (!exp_disconnected
	    && !exp_forked
	    && (exp_dev_tty != -1)
	    && isatty(exp_dev_tty)
	    && exp_ioctled_devtty) {
		exp_tty_set(interp,&exp_tty_original,exp_dev_tty,0);
	}
	/* all other files either don't need to be flushed or will be
	   implicitly closed at exit.  Spawned processes are free to continue
	   running, however most will shutdown after seeing EOF on stdin.
	   Some systems also deliver SIGHUP and other sigs to idle processes
	   which will blow them away if not prepared.
	*/

#ifdef CRAY
	ttyp_reset();
#endif
}


/* this stupidity because Tcl needs commands in writable space */
static char prompt1[] = "prompt1";
static char prompt2[] = "prompt2";

static char *prompt2_default = "+> ";
static char prompt1_default[] = "expect%d.%d> ";

/*ARGSUSED*/
int
Exp_Prompt1Cmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	Interp *iPtr = (Interp *)interp;

	sprintf(interp->result,prompt1_default,
		iPtr->numLevels,iPtr->curEventNum+1);			
	return(TCL_OK);
}

/*ARGSUSED*/
int
Exp_Prompt2Cmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	strcpy(interp->result,prompt2_default);
	return(TCL_OK);
}

/*ARGSUSED*/
static int
ignore_procs(interp,s)
Tcl_Interp *interp;
char *s;		/* function name */
{
	return ((s[0] == 'p') &&
		(s[1] == 'r') &&
		(s[2] == 'o') &&
		(s[3] == 'm') &&
		(s[4] == 'p') &&
		(s[5] == 't') &&
		((s[6] == '1') ||
		 (s[6] == '2')) &&
		(s[7] == '\0')
	       );
}

/* handle an error from Tcl_Eval or Tcl_EvalFile */
static void
handle_eval_error(interp,check_for_nostack)
Tcl_Interp *interp;
int check_for_nostack;
{
	char *msg;

	/* if errorInfo has something, print it */
	/* else use what's in interp->result */

	msg = Tcl_GetVar(interp,"errorInfo",TCL_GLOBAL_ONLY);
	if (!msg) msg = interp->result;
	else if (check_for_nostack) {
		/* suppress errorInfo if generated via */
		/* error ... -nostack */
		if (0 == strncmp("-nostack",msg,8)) return;
	}

	/* no \n at end, since ccmd will already have one. */
	/* Actually, this is not true if command is last in */
	/* file and has no newline after it, oh well */
	errorlog("%s\r\n",exp_cook(msg,(int *)0));
}

/* user has pressed escape char from interact or somehow requested expect.
If a user-supplied command returns:

TCL_ERROR,	assume user is experimenting and reprompt
TCL_OK,		ditto
TCL_RETURN,	return TCL_OK (assume user just wants to escape() to return)
EXP_TCL_RETURN,	return TCL_RETURN
anything else	return it
*/
int
exp_interpreter(interp)
Tcl_Interp *interp;
{
	int rc;
	char *ccmd;		/* pointer to complete command */
	char line[BUFSIZ+1];	/* space for partial command */
	int newcmd = TRUE;
	Tcl_DString dstring;
	Interp *iPtr = (Interp *)interp;
	int tty_changed = FALSE;

	exp_tty tty_old;
	int was_raw, was_echo;

	int dummy;
	int fd = fileno(stdin);
	
	expect_key++;

	Tcl_DStringInit(&dstring);

	newcmd = TRUE;
	while (TRUE) {
		/* force terminal state */
		tty_changed = exp_tty_cooked_echo(interp,&tty_old,&was_raw,&was_echo);

		if (newcmd) {
			rc = Tcl_Eval(interp,prompt1);
			if (rc == TCL_OK) exp_log(1,"%s",interp->result);
			else exp_log(1,prompt1_default,iPtr->numLevels,
						   iPtr->curEventNum+1);
		} else {
			rc = Tcl_Eval(interp,prompt2);
			if (rc == TCL_OK) exp_log(1,"%s",interp->result);
			else exp_log(1,prompt2_default,1);
		}

#ifdef SHARE_CMD_BUFFER
		if (exp_fgets(interp,line,BUFSIZ) == EXP_EOF) {
			if (!newcmd) line[0] = 0;
			else exp_exit(interp,0);
		}
#else
		exp_fs[fd].force_read = 1;
		rc = exp_get_next_event(interp,&fd,1,&dummy,EXP_TIME_INFINITY,
			exp_fs[fd].key);
		/*  check for rc == EXP_TCLERROR? */

		if (rc != EXP_EOF) {
			rc = read(0,line,BUFSIZ);
#ifdef SIMPLE_EVENT
			if (rc == -1 && errno == EINTR) {
				if (tcl_AsyncReady) {
					(void) Tcl_AsyncInvoke(interp,TCL_OK);
				}
				continue;
			}
#endif
			if (rc <= 0) {
				if (!newcmd) line[0] = 0;
				else rc = EXP_EOF;
			} else line[rc] = '\0';
		}

#ifdef BEFORE_ASYNC
		if (rc != EXP_EOF) {
			if (0 >= (rc = read(0,line,BUFSIZ))) {
				if (!newcmd) line[0] = 0;
				else rc = EXP_EOF;
			} else line[rc] = '\0';
		}
#endif

		if (rc == EXP_EOF) exp_exit(interp,0);

		if (debugfile) fwrite(line,1,strlen(line),debugfile);
		/* intentionally always write to logfile */
		if (logfile) fwrite(line,1,strlen(line),logfile);
		/* no need to write to stdout, since they will see */
		/* it just from it having been echoed as they are */
		/* typing it */
#endif /*SHARE_CMD_BUFFER*/

		ccmd = Tcl_DStringAppend(&dstring,line,rc);
		if (!Tcl_CommandComplete(ccmd)) {
			newcmd = FALSE;
			continue;	/* continue collecting command */
		}
		newcmd = TRUE;

		if (tty_changed) exp_tty_set(interp,&tty_old,was_raw,was_echo);
		rc = Tcl_RecordAndEval(interp,ccmd,0);
		Tcl_DStringFree(&dstring);
		switch (rc) {
		case TCL_OK:
			if (*interp->result != 0)
				exp_log(1,"%s\r\n",exp_cook(interp->result,(int *)0));
			continue;
		case TCL_ERROR:
			handle_eval_error(interp,1);
			/* since user is typing by hand, we expect lots */
			/* of errors, and want to give another chance */
			continue;
#define finish(x)	{rc = x; goto done;}
		case TCL_BREAK:
		case TCL_CONTINUE:
			finish(rc);
		case EXP_TCL_RETURN:
			finish(TCL_RETURN);
		case TCL_RETURN:
			finish(TCL_OK);
		default:
			/* note that ccmd has trailing newline */
			errorlog("error %d: %s\r\n",rc,ccmd);
			continue;
		}
	}
	/* cannot fall thru here, must jump to label */
 done:
	if (tty_changed) exp_tty_set(interp,&tty_old,was_raw,was_echo);

	Tcl_DStringFree(&dstring);

	return(rc);
}

/*ARGSUSED*/
int
Exp_ExpVersionCmd(clientData, interp, argc, argv)
ClientData clientData;
Tcl_Interp *interp;
int argc;
char **argv;
{
	int emajor, umajor;
	char *user_version;	/* user-supplied version string */

	if (argc == 1) {
		Tcl_SetResult(interp,exp_version,TCL_STATIC);
		return(TCL_OK);
	}
	if (argc > 3) {
		exp_error(interp,"usage: expect_version [[-exit] version]");
		return(TCL_ERROR);
	}

	user_version = argv[argc==2?1:2];
	emajor = atoi(exp_version);
	umajor = atoi(user_version);

	/* first check major numbers */
	if (emajor == umajor) {
		int u, e;

		/* now check minor numbers */
		char *dot = strchr(user_version,'.');
		if (!dot) {
			exp_error(interp,"version number must include a minor version number");
			return TCL_ERROR;
		}

		u = atoi(dot+1);
		dot = strchr(exp_version,'.');
		e = atoi(dot+1);
		if (e >= u) return(TCL_OK);
	}

	if (argc == 2) {
		exp_error(interp,"%s requires Expect version %s (but using %s)",
			exp_argv0,user_version,exp_version);
		return(TCL_ERROR);
	}
	errorlog("%s: requires Expect version %s (but using %s)\r\n",
		exp_argv0,user_version,exp_version);
	exp_exit(interp,1);
	/*NOTREACHED*/
}

static char init_auto_path[] = "lappend auto_path $exp_library $exp_exec_library";

int
Exp_Init(interp)
Tcl_Interp *interp;
{
	static int first_time = TRUE;

	if (first_time) {
		int tcl_major = atoi(TCL_VERSION);
		char *dot = strchr(TCL_VERSION,'.');
		int tcl_minor = atoi(dot+1);

		if (tcl_major < NEED_TCL_MAJOR || 
		    (tcl_major == NEED_TCL_MAJOR && tcl_minor < NEED_TCL_MINOR)) {
			sprintf(interp->result,
			   "%s compiled with Tcl %d.%d but needs at least Tcl %d.%d\n",
				exp_argv0,tcl_major,tcl_minor,
				NEED_TCL_MAJOR,NEED_TCL_MINOR);
			return TCL_ERROR;
		}

		exp_getpid = getpid();
		exp_init_pty();
		exp_init_tty(); /* do this only now that we have looked at */
				/* original tty state */
		exp_init_sig();
		exp_init_event();
		exp_init_trap();
		exp_init_unit_random();
		exp_init_spawn_ids();

		first_time = FALSE;
	}

	/* save last known interp for emergencies */
	exp_interp = interp;

	/* initialize commands */
	exp_init_most_cmds(interp);	/* add misc     cmds to interpreter */
	exp_init_expect_cmds(interp);	/* add expect   cmds to interpreter */
	exp_init_main_cmds(interp);	/* add main     cmds to interpreter */
	exp_init_trap_cmds(interp);	/* add trap     cmds to interpreter */
	exp_init_tty_cmds(interp);	/* add tty      cmds to interpreter */
	exp_init_interact_cmds(interp);	/* add interact cmds to interpreter */

	exp_init_spawn_id_vars(interp);

	Tcl_SetVar(interp,"expect_library",SCRIPTDIR,0);/* deprecated */
	Tcl_SetVar(interp,"exp_library",SCRIPTDIR,0);
	Tcl_SetVar(interp,"exp_exec_library",EXECSCRIPTDIR,0);
	Tcl_Eval(interp,init_auto_path);

#ifdef TCL_DEBUGGER
	Dbg_IgnoreFuncs(interp,ignore_procs);
/*	Dbg_Interactor(interp,exp_interpreter);*/
#endif

	return TCL_OK;
}

static char sigexit_init_default[] = "trap exit {SIGINT SIGTERM}";
static char debug_init_default[] = "trap {exp_debug 1} SIGINT";

void
exp_parse_argv(interp,argc,argv)
Tcl_Interp *interp;
int argc;
char **argv;
{
	char argc_rep[10]; /* enough space for storing literal rep of argc */

	int sys_rc = TRUE;	/* read system rc file */
	int my_rc = TRUE;	/* read personal rc file */

	int exp_buffer_command_input = FALSE;/* read in entire cmdfile at once */

	int c;
	int rc;

	extern int optind;
	extern char *optarg;
	char *args;		/* ptr to string-rep of all args */
	char *debug_init;

	exp_argv0 = argv[0];

#ifdef TCL_DEBUGGER
	Dbg_ArgcArgv(argc,argv,1);
#endif

	/* initially, we must assume we are not interactive */
	/* this prevents interactive weirdness courtesy of unknown via -c */
	/* after handling args, we can change our mind */
	Tcl_SetVar(interp, "tcl_interactive", "0", TCL_GLOBAL_ONLY);

	Tcl_Eval(interp,sigexit_init_default);

	while ((c = getopt(argc, argv, "b:c:dD:f:inN")) != EOF) {
		switch(c) {
		case 'c': /* command */
			exp_cmdlinecmds = TRUE;
			rc = Tcl_Eval(interp,optarg);
			if (rc != TCL_OK) {
			    errorlog("%s\r\n",exp_cook(Tcl_GetVar(interp,"errorInfo",TCL_GLOBAL_ONLY),(int *)0));
			}
			break;
		case 'd': exp_is_debugging = TRUE;
			debuglog("expect version %s\r\n",exp_version);
			break;
#ifdef TCL_DEBUGGER
		case 'D':
			exp_tcl_debugger_available = TRUE;
			if (Tcl_GetInt(interp,optarg,&rc) != TCL_OK) {
				errorlog("%s: -D argument must be 0 or 1\r\n",
					exp_argv0);
				exp_exit(interp,1);
			}

			/* set up trap handler before Dbg_On so user does */
			/* not have to see it at first debugger prompt */
			if (0 == (debug_init = getenv("EXPECT_DEBUG_INIT"))) {
				debug_init = debug_init_default;
			}
			Tcl_Eval(interp,debug_init);
			if (rc == 1) Dbg_On(interp,0);
			break;
#endif
		case 'f': /* name of cmd file */
			exp_cmdfilename = optarg;
			break;
		case 'b': /* read cmdfile one part at a time */
			exp_cmdfilename = optarg;
			exp_buffer_command_input = TRUE;
			break;
		case 'i': /* interactive */
			exp_interactive = TRUE;
			break;
		case 'n': /* don't read personal rc file */
			my_rc = FALSE;
			break;
		case 'N': /* don't read system-wide rc file */
			sys_rc = FALSE;
			break;
		default: usage(interp);
		}
	}

	for (c = 0;c<argc;c++) {
		debuglog("argv[%d] = %s  ",c,argv[c]);
	}
	debuglog("\r\n");

	/* if user hasn't explicitly requested we be interactive */
	/* look for a file or some other source of commands */
	if (!exp_interactive) {
		/* get cmd file name, if we haven't got it already */
		if (!exp_cmdfilename && (optind < argc)) {
			exp_cmdfilename = argv[optind];
			optind++;
		}

		if (exp_cmdfilename) {
			if (streq(exp_cmdfilename,"-")) {
				exp_cmdfile = stdin;
				exp_cmdfilename = 0;
			} else if (exp_buffer_command_input) {
				exp_cmdfile = fopen(exp_cmdfilename,"r");
				if (exp_cmdfile) {
					exp_cmdfilename = 0;
					exp_close_on_exec(fileno(exp_cmdfile));
				} else {
					errorlog("%s: %s\r\n",exp_cmdfilename,strerror(errno));
					exp_exit(interp,1);
				}
			}
		} else if (!exp_cmdlinecmds) {
			if (isatty(0)) {
				/* no other source of commands, force interactive */
				exp_interactive = TRUE;
			} else {
				/* read cmds from redirected stdin */
				exp_cmdfile = stdin;
			}
		}
	}

	if (exp_interactive) {
		Tcl_SetVar(interp, "tcl_interactive","1",TCL_GLOBAL_ONLY);
	}

	/* collect remaining args and make into argc, argv0, and argv */
	sprintf(argc_rep,"%d",argc-optind);
	Tcl_SetVar(interp,"argc",argc_rep,0);
	debuglog("set argc %s\r\n",argc_rep);

	if (exp_cmdfilename) {
		Tcl_SetVar(interp,"argv0",exp_cmdfilename,0);
		debuglog("set argv0 \"%s\"\r\n",exp_cmdfilename);
	} else {
		Tcl_SetVar(interp,"argv0",exp_argv0,0);
		debuglog("set argv0 \"%s\"\r\n",exp_argv0);
	}

	args = Tcl_Merge(argc-optind,argv+optind);
	debuglog("set argv \"%s\"\r\n",args);
	Tcl_SetVar(interp,"argv",args,0);
	ckfree(args);

	exp_interpret_rcfiles(interp,my_rc,sys_rc);
}

/* read rc files */
void
exp_interpret_rcfiles(interp,my_rc,sys_rc)
Tcl_Interp *interp;
int my_rc;
int sys_rc;
{
	int rc;

	if (sys_rc) {
	    char file[200];
	    int fd;

	    sprintf(file,"%s/expect.rc",SCRIPTDIR);
	    if (-1 != (fd = open(file,0))) {
		if (TCL_ERROR == (rc = Tcl_EvalFile(interp,file))) {
		    errorlog("error executing system initialization file: %s\r\n",file);
		    if (rc != TCL_ERROR)
				errorlog("Tcl_Eval = %d\r\n",rc);
		    if (*interp->result != 0)
				errorlog("%s\r\n",interp->result);
		    exp_exit(interp,1);
		}
		close(fd);
	    }
	}
	if (my_rc) {
	    char file[200];
	    char *home;
	    int fd;
	    char *getenv();

	    if (NULL != (home = getenv("HOME"))) {
		sprintf(file,"%s/.expect.rc",home);
		if (-1 != (fd = open(file,0))) {
		    if (TCL_ERROR == (rc = Tcl_EvalFile(interp,file))) {
			errorlog("error executing file: %s\r\n",file);
			if (rc != TCL_ERROR)
				errorlog("Tcl_Eval = %d\r\n",rc);
			if (*interp->result != 0)
				errorlog("%s\r\n",interp->result);
			exp_exit(interp,1);
		    }
		    close(fd);
	        }
	    }
	}
}

void
exp_interpret_cmdfilename(interp,filename)
Tcl_Interp *interp;
char *filename;
{
	debuglog("executing commands from command file\r\n");

	if (TCL_OK != Tcl_EvalFile(interp,filename)) {
		handle_eval_error(interp,0);
	}
}

void
exp_interpret_cmdfile(interp,fp)
Tcl_Interp *interp;
FILE *fp;
{
	int rc;
	int newcmd;

	Tcl_DString dstring;
	Tcl_DStringInit(&dstring);

	debuglog("executing commands from command file\r\n");

	newcmd = TRUE;
	while (1) {
		char line[BUFSIZ];/* buffer for partial Tcl command */
		char *ccmd;	/* pointer to complete Tcl command */

		if (fgets(line,BUFSIZ,fp) == NULL) {
			if (newcmd) break;
			else line[0] = 0;
		}
		ccmd = Tcl_DStringAppend(&dstring,line,-1);
		if (!Tcl_CommandComplete(ccmd)) {
			newcmd = FALSE;
			continue;	/* continue collecting command */
		}
		newcmd = TRUE;

		rc = Tcl_Eval(interp,ccmd);
		Tcl_DStringFree(&dstring);
		if (rc != TCL_OK) {
			handle_eval_error(interp,0);
		}
	}
	Tcl_DStringFree(&dstring);
}

#ifdef SHARE_CMD_BUFFER
/* fgets that shared input buffer with expect_user */
int
exp_fgets(interp,buf,max)
Tcl_Interp *interp;
char *buf;
int max;
{
	char *nl;	/* position of newline which signifies end of line */
	int write_count;/* length of first line of incoming data */

	int m = fileno(stdin);
	struct exp_f *f;
	int cc;

	int dummy;

	/* avoid returning no data, just because someone else read it in by */
	/* passing most recent key */
	cc = exp_get_next_event(interp,&m,1,&dummy,EXP_TIME_INFINITY,exp_fs[m].key);

	if (cc == EXP_DATA_NEW) {
		/* try to read it */

		cc = exp_i_read(m,EXP_TIME_INFINITY);

		/* the meaning of 0 from i_read means eof.  Muck with it a */
		/* little, so that from now on it means "no new data arrived */
		/* but it should be looked at again anyway". */
		if (cc == 0) {
			cc = EXP_EOF;
		} else if (cc > 0) {
			f = exp_fs + m;
			f->buffer[f->size += cc] = '\0';
		}
	} else if (cc == EXP_DATA_OLD) {
		f = exp_fs + m;
		cc = 0;
	}

	/* EOF and TIMEOUT return here */
	/* In such cases, there is no need to update screen since, if there */
	/* was prior data read, it would have been sent to the screen when */
	/* it was read. */
	if (cc < 0) return (cc);

	/* copy up to end of first line */

	/* calculate end of first line */
	nl = strchr(f->buffer,'\n');
	if (nl) write_count = 1+nl-f->buffer;
	else write_count = f->size;

	/* make sure line fits in buffer area */
	if (write_count > max) write_count = max;

	/* copy it */
	memcpy(buf,f->buffer,write_count);
	buf[write_count] = '\0';

	/* update display and f */

	f->printed = 0;
	/* for simplicity force f->printed = 0.  This way, the user gets */
	/* to see the commands that are about to be executed.  Not seeing */
	/* commands you are supposedly typing sounds very uncomfortable! */

	if (logfile_all || (loguser && logfile)) {
		fwrite(f->buffer,1,write_count,logfile);
	}
	if (debugfile) fwrite(f->buffer,1,write_count,debugfile);

	f->size -= write_count;
	memcpy(f->buffer,f->buffer+write_count,1+f->size);
	/* copy to lowercase buffer */
	exp_lowmemcpy(f->lower,f->buffer,1+f->size);

	return(write_count);
}
#endif /*SHARE_CMD_BUFFER*/

static struct exp_cmd_data cmd_data[]  = {
{"expect_version",Exp_ExpVersionCmd,	0,	0},	/* deprecated */
{"exp_version",	Exp_ExpVersionCmd,	0,	0},
{"prompt1",	Exp_Prompt1Cmd,		0,	EXP_NOPREFIX},
{"prompt2",	Exp_Prompt2Cmd,		0,	EXP_NOPREFIX},
{0}};

void
exp_init_main_cmds(interp)
Tcl_Interp *interp;
{
	exp_create_commands(interp,cmd_data);
}
