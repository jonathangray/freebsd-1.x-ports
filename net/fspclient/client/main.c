/******************************************************************************
* This file is Copyright 1992 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
******************************************************************************/

#include "client.h"
#include "main.h"
#include "macro.h"
#include "parse.h"
#include "redirect.h"
#include <ctype.h>

/*****************************************************************************/

#ifndef ANSI_PROTOTYPES
#define CMD(com,fun,glob,help) extern int fun();
#else /* ANSI_PROTOTYPES */
#define CMD(com,fun,glob,help) extern int fun(int argc, char **argv, char **envp);
#endif /* ANSI_PROTOTYPES */

#include "remote/table.h"
#include "local/table.h"

#undef CMD

#ifndef ANSI_PROTOTYPES
static int fsp_help();
#else /* ANSI_PROTOTYPES */
static int fsp_help(int argc, char **argv, char **envp);
#endif /* ANSI_PROTOTYPES */

/*****************************************************************************/

#ifndef __STDC__
#define CMD(com,fun,glob,help) { "com", NEEDCONN, glob, fun, help },
#else /* __STDC__ */
#define CMD(com,fun,glob,help) { #com, NEEDCONN, glob, fun, help },
#endif /* __STDC__ */

static DT dispatch_table[] = {
#define NEEDCONN 1
#include "remote/table.h"
#undef NEEDCONN

#define NEEDCONN 0
#include "local/table.h"
CMD(help,fsp_help,NOGLOB,"give help on commands or macros")
CMD(?,fsp_help,NOGLOB,"identical to `help'")
{ (char *)0, 0, NOGLOB, 0, (char *)0 }
};
#undef NEEDCONN

/*****************************************************************************/

#include "patchlevel.h"
static char packageid[] = PACKAGEID;
static char copyright[] = COPYRIGHT;

extern char **environ;
#define COMMSIZE 512

int notconnected, notquit, last_retcode, dbug_flag;
char *myprompt = "fsp> ";
char *pager_command = (char*)0;

static int myport;

static int
#ifndef ANSI_PROTOTYPES
execute_builtin(argc, argv, envp)
    int argc;
    char *argv[];
    char *envp[];
#else /* ANSI_PROTOTYPES */
execute_builtin(int argc, char **argv, char **envp)
#endif /* ANSI_PROTOTYPES */
{
    int  i;

    i = 0;
    while (dispatch_table[i].com && strcmp(dispatch_table[i].com, argv[0]))
	i++;

    if (dispatch_table[i].com == (char*)0)
	return 1;

    if (dispatch_table[i].needconn && notconnected)
    {
	if (env_host == (char*)0 || env_port == (char*)0)
	{
	    ffprintf(STDERR, "?not connected\n");
	    return 0;
	}

	notconnected = (init_client(env_host, atoi(env_port), myport) < 0);

	if (notconnected)
	{
	    ffprintf(STDERR, "?failed to connect -- aborting `%s' command\n",
		     argv[0]);
	    return 0;
	}
    }

    switch (dispatch_table[i].glob)
    {
      case NOGLOB:
	break;
      case LOCALGLOB:
	local_glob_routines();
	break;
      case REMOTEGLOB:
	remote_glob_routines();
	break;
    }

    last_retcode = (dispatch_table[i].fun)(argc, argv, envp);

    if (last_retcode && client_intr_state < 2 && notquit && onerrorargv)
    	execute_command(onerrorargc, onerrorargv);

    return 0;
}

static void
#ifndef ANSI_PROTOTYPES
execute_macro(argc, argv)
    int argc;
    char *argv[];
#else /* ANSI_PROTOTYPES */
execute_macro(int argc, char **argv)
#endif /* ANSI_PROTOTYPES */
{
    char *macro_comm, **myargv;
    int mymaxargc = 10;

    myargv = (char**)calloc(mymaxargc, sizeof(char*));

    while ((macro_comm = get_macroline()) != (char*)0)
    {
	char *comm;
	int myargc;

	comm = strdup(macro_comm);

	if ((myargc = parsemyargs(comm, &myargv, &mymaxargc, argc, argv)) > 0)
	{
	    execute_command(myargc, myargv);
	    freemyargs(myargc,myargv);
	}

	(void)free(comm);
    }

    (void)free((char*)myargv);
}

static int broken_pipe;
static int save_client_intr_state;

static RETSIGTYPE
#ifndef ANSI_PROTOTYPES
pipe_handler(sig)
    int sig;
#else /* ANSI_PROTOTYPES */
pipe_handler(int sig)
#endif /* ANSI_PROTOTYPES */
{
    broken_pipe = 1;
    client_intr_state = 2;
    save_client_intr_state = client_intr_state;
}

void
#ifndef ANSI_PROTOTYPES
execute_command(argc, argv)
    int argc;
    char *argv[];
#else /* ANSI_PROTOTYPES */
execute_command(int argc, char **argv)
#endif /* ANSI_PROTOTYPES */
{
    int builtin;
    FILE *pipefs = 0;
    iobuffers storedfs;
#ifndef ANSI_PROTOTYPES
    RETSIGTYPE (*old_pipe_handler)();
#else /* ANSI_PROTOTYPES */
    RETSIGTYPE (*old_pipe_handler)(int);
#endif /* ANSI_PROTOTYPES */

    builtin = (strcmp(argv[0], BUILTIN) == 0);
    broken_pipe = 0;

    if (argc > 1 && argv[argc-1][0] == '|')
    {
        old_pipe_handler = signal(SIGPIPE, pipe_handler);

	pipefs = popen(&argv[argc-1][1], "w");
	if (pipefs == 0)
	{
	    ffprintf(STDERR, "?cannot open pipe to command `%s'\n",
		     &argv[argc-1][1]);
	    return;
	}

	storedfs = global_iobuffers;
	STDOUT   = pipefs;

	argv[--argc] = 0;
    }

    if (builtin || initialise_macro(argc, argv))
    {	/* either specific builtin or no such macro */
	if (builtin)
	{
	    argc--;
	    argv++;
	}

	if (execute_builtin(argc, argv, environ))
	    /* no such builtin */
	    ffprintf(STDERR, "?unknown command `%s'\n", argv[0]);
    }
    else
	execute_macro(argc, argv);
        
    if (pipefs)
    {
	global_iobuffers = storedfs;
	pclose(pipefs);

        (void)signal(SIGPIPE, old_pipe_handler);
    }

    if (broken_pipe)
	client_intr_state = save_client_intr_state;
}

int
#ifndef ANSI_PROTOTYPES
execute_stdin(argc, argv)
    int argc;
    char *argv[];
#else /* ANSI_PROTOTYPES */
execute_stdin(int argc, char **argv)
#endif /* ANSI_PROTOTYPES */
{
    int myargc;
    int mymaxargc = 10;
    char **myargv;

    myargv = (char**)calloc(mymaxargc, sizeof(char*));

    while (notquit && !feof(STDIN))
    {
	char comm[COMMSIZE];	/* pick a number, any number ... correct! */
	char *thislabel = 0;
        int myargv0len, islabel;

	/* get command; currently we don't allow continuation lines */
	ffprintf(STDPROMPT, "%s", myprompt);
	if (my_fgets(comm, COMMSIZE, STDIN) == (char*)0)
	    break;

	client_intr_state = 1;	/* interrupts cause abort of operations */
	client_intr_cnt   = 0;	/* reset the number of interrupts received */

	if ((myargc = parsemyargs(comm, &myargv, &mymaxargc, argc, argv)) < 1)
	    continue;

	myargv0len = strlen(myargv[0]);

	if (myargv0len > 0 && myargv[0][myargv0len - 1] == ':')
	    thislabel = myargv[0];

	islabel = (thislabel != 0);

	if (skiptolabel && thislabel
	   && strncmp(skiptolabel, thislabel, myargv0len - 1) == 0)
	{
	    (void)free(skiptolabel);
	    skiptolabel = 0;
	}

	if (skiptolabel)
	    continue;

	if (myargc > islabel)
	    execute_command(myargc - islabel, myargv + islabel);

	freemyargs(myargc, myargv);
    }

    (void)free((char*)myargv);

    return 0;
}

static struct
{
    char *prog;
    char *name;
}
commandline_names[] =
{
    { "fcatcmd",	"cat"	},
    { "fcdcmd",		"cd"	},
    { "fducmd",		"du"	},
    { "fgetcmd",	"get"	},
    { "fgrabcmd",	"grab"	},
    { "flscmd",		"ls"	},
    { "fmkdir",		"mkdir"	},
    { "fprocmd",	"pro"	},
    { "fput",		"put"	},
    { "frmcmd",		"rm"	},
    { "frmdircmd",	"rmdir"	},
    { "ftarcmd",	"tar"	},
    { "ftouch",		"touch"	},
    { "fver",		"ver"	},
    { 0,		0	}	/* both these should be 0 */
};

static char *
#ifndef ANSI_PROTOTYPES
command_name(progname)
    char *progname;
#else /* ANSI_PROTOTYPES */
command_name(char *progname)
#endif /* ANSI_PROTOTYPES */
{
    int i;

    for (i = 0; commandline_names[i].prog; i++)
	if (strcmp(commandline_names[i].prog, progname) == 0)
	    break;

    return commandline_names[i].name;
}

static RETSIGTYPE
#ifndef ANSI_PROTOTYPES
interrupt_handler(sig)
    int sig;
#else /* ANSI_PROTOTYPES */
interrupt_handler(int sig)
#endif /* ANSI_PROTOTYPES */
{
    char *txt = (char*)0;

    switch (client_intr_cnt)
    {
      case 0:
	txt = "Interrupt!";
	break;
      case 1:
	txt = "Ouch!";
	break;
      case 2:
	txt = "Urgh!";
	break;
      case 3:
	txt = "Argh!";
	break;
      case 4:
	txt = "Ok, ok, I've got the idea.  I'll stop when I can!";
	break;
      case 5:
	txt = "Stop it!  Hit something like ^\\ if you want a fatal death.";
	break;
      case 6:
	txt = "Not talking to you anymore [sulk]...";
	break;
      default:
	break;
    }

    if (txt)
	ffprintf(STDPROMPT, "%s\n", txt);

    client_intr_cnt++;

    if (client_intr_state)
	client_intr_state++;

    (void)signal(SIGINT, interrupt_handler);
}

static void
#ifndef ANSI_PROTOTYPES
init_env()
#else /* ANSI_PROTOTYPES */
init_env(void)
#endif /* ANSI_PROTOTYPES */
{
    util_get_env();

    if (!env_myport)
	myport = 0;
    else
	myport = atoi(env_myport);
}

int
#ifndef ANSI_PROTOTYPES
main(argc, argv)
    int argc;
    char *argv[];
#else /* ANSI_PROTOTYPES */
main(int argc, char **argv)
#endif /* ANSI_PROTOTYPES */
{
#ifndef ANSI_PROTOTYPES
    extern int getopt();
#else /* ANSI_PROTOTYPES */
    extern int getopt(int argc, char **argv, char *optstring);
#endif /* ANSI_PROTOTYPES */
    extern char *optarg;
    extern int optind, opterr;
    int thisflag;
    int show_banner, dont_run;
    int flag_errs = 0;
    char *comname;

    comname = strrchr(argv[0], '/');
    if (comname == 0)
	comname = argv[0];
    else
	comname++;
    comname = command_name(comname);

    standalone = (comname != 0);

    opterr = 0;
    optind = 1;

    last_retcode = 0;
    dbug_flag	 = 0;
    show_banner	 = 0;
    dont_run	 = 0;

    if (!standalone)
    {
	while ((thisflag = getopt(argc, argv, "dvV")) != -1)
	    switch (thisflag)
	    {
	      case 'd':
		dbug_flag++;
		break;

	      case 'v':
		show_banner = 1;
		break;

	      case 'V':
		show_banner = 1;
		dont_run = 1;
		break;

	      case '?':
		flag_errs++;
		ffprintf(STDERR,"unrecognised flag (-%c) ignored\n",thisflag);
		break;

	      default:
		break;
	    }

	optind--;
	argv[optind] = argv[0];
	argc -= optind;
	argv += optind;
    }

    initialise_stdio();

    if (flag_errs)
	ffprintf(STDDBG, "total of %d command line errors\n", flag_errs);

    if (show_banner)
	ffprintf(STDINFO, "FSP client version %s (id: %s)\n%s\n",
		 PATCHLEVEL, packageid + 4, copyright + 4);

    if (dont_run)
	return 0;

    notconnected = 1;
    notquit = 1;

    {
	char *startup[3];

	startup[0] = "source";

	if ((startup[1] = (char *)getenv("FSPRC")) == (char*)0)
	    startup[1] = "~/.fsprc";

	startup[2] = (char*)0;

	execute_command(2, startup);
    }

    if (comname != 0)
    {
	init_env();
	argv[0] = comname;
	return execute_builtin(argc, argv, environ);
    }

    if (argc > 1)
    {
	if (initialise_macro(argc - 1, argv + 1) == 0)
	    execute_macro(argc - 1, argv + 1);
	else
	{
	    /* strdup() is used so that the variables can be free()d later */
	    env_host = strdup(argv[1]);

	    if (argc > 2)
		env_port = strdup(argv[2]);
	    else
		env_port = strdup("21");

	    if (argc > 3)
		env_dir  = strdup(argv[3]);
	    else
		env_dir  = strdup("/");
	}
    }

    init_env();

    /* only set up an interrupt handler if fsp is running interactively */
    if (STDPROMPT)
	(void)signal(SIGINT,interrupt_handler);

    /* if a pager wasn't set in the .fsprc, then check for one now */
    if (!pager_command)
    {
	pager_command = (char *)getenv("PAGER");
	/* allow pager_command to be free'd */
	    if (pager_command)
		pager_command = strdup(pager_command);
    }

    (void)execute_stdin(argc, argv);

    finish_client();

    return last_retcode;
}

#define NCO 6
static void
#ifndef ANSI_PROTOTYPES
fsp_builtin_long_help_all()
#else /* ANSI_PROTOTYPES */
fsp_builtin_long_help_all(void)
#endif /* ANSI_PROTOTYPES */
{
    int i;
    int notext = 0;

    ffprintf(STDOUT, "Builtin commands are:\n");

    for (i = 0; dispatch_table[i].com; i++)
        if (dispatch_table[i].help)
	    ffprintf(STDOUT, "      %-10s    %s\n",
		     dispatch_table[i].com, dispatch_table[i].help);
	else
	    notext++;

    if (notext)
    {
	int j;

	ffprintf(STDOUT, "\nUndocumented commands are:\n");

	for (i = 0, j = 0; dispatch_table[i].com; i++)
	    if (!dispatch_table[i].help)
	    {
		ffprintf(STDOUT, "  %-10s%s",
			 dispatch_table[i].com, j == NCO-1? "\n" : "");
		j = (j+1) % NCO;
	    }

	if (j)
	    ffprintf(STDOUT, "\n");
    }
}

static int
#ifndef ANSI_PROTOTYPES
fsp_builtin_long_help(name)
    char *name;
#else /* ANSI_PROTOTYPES */
fsp_builtin_long_help(char *name)
#endif /* ANSI_PROTOTYPES */
{
    int i;

    for (i = 0; dispatch_table[i].com; i++)
	if (strcmp(dispatch_table[i].com, name) == 0)
	    break;

    if (dispatch_table[i].com)
    {
	if (dispatch_table[i].help)
	    ffprintf(STDOUT, "%-10s    %s\n",
		 dispatch_table[i].com, dispatch_table[i].help);
	else
	    ffprintf(STDOUT, "no help available for command `%s'\n", name);
    }
    else
	return 1;

    return 0;
}

static void
#ifndef ANSI_PROTOTYPES
fsp_builtin_short_help()
#else /* ANSI_PROTOTYPES */
fsp_builtin_short_help(void)
#endif /* ANSI_PROTOTYPES */
{
    int i, j;

    ffprintf(STDOUT, "Builtin commands are:\n");

    for (i = 0, j = 0; dispatch_table[i].com; i++)
    {
	ffprintf(STDOUT, "  %-10s%s",
		 dispatch_table[i].com, j == NCO-1? "\n" : "");
	j = (j+1) % NCO;
    }

    if (j)
	ffprintf(STDOUT,"\n");
}

static int
#ifndef ANSI_PROTOTYPES
fsp_help(argc, argv, envp)
    int argc;
    char *argv[];
    char *envp[];
#else /* ANSI_PROTOTYPES */
fsp_help(int argc, char **argv, char **envp)
#endif /* ANSI_PROTOTYPES */
{
    if (argc > 1)
    {
	if (strcmp(argv[1], "all") == 0)
	{
	    fsp_builtin_long_help_all();
	    ffprintf(STDOUT, "\n");
	    fsp_macro_long_help_all();
	}
	else
	{
	    int i;
	    for (i = 1; i < argc; i++)
		if (fsp_macro_long_help(argv[i])
		    && fsp_builtin_long_help(argv[i]))
		    ffprintf(STDERR, "no such command or macro: `%s'\n",
			     argv[i]);
	}
    }
    else
    {
	fsp_builtin_short_help();
	ffprintf(STDOUT, "\n");
	fsp_macro_short_help();
    }

    return 0;
}
