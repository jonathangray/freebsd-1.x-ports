
/*
 * This file is part of the Seyon, Copyright (c) 1992-1993 by Muhammad M.
 * Saggaf. All rights reserved.
 *
 * See the file COPYING (1-COPYING) or the manual page seyon(1) for a full
 * statement of rights and permissions for this program.
 */

#include <signal.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include "seyon.h"
#include "SeDecl.h"

#if HAVE_TERMIOS
#include <termios.h>
struct termios  newmode,
                oldmode;
#else
#if HAVE_TERMIO
#include <termio.h>
struct termio   newmode,
                oldmode;
#else
#if HAVE_SGTTYB
#include <sys/ioctl.h>
struct sgttyb   newmode,
                oldmode;
#endif
#endif
#endif

#define SEYON_EMU_NAME    "seyon-emu"

#include "version.h"
static char     version[] = "$Revision: 1.1 $";

extern Widget   CreateCommandCenter();
extern void     SetIcon(),
                GetResources(),
                InitVariables();
extern void     PopupInitError(),
                PopupFatalError();

void            s_script(),
                s_set(),
                s_exit(),
                die(),
                cleanup_exit();

int             tfd,
                child_pipe[2],
                scriptToMainPipe[2],
                mainToTermPipe[2];
FILE           *tfp;

struct QueryResources qres;
XtAppContext    app_con;
Widget          topLevel;
pid_t           mainPid;

int
main(argc, argv)
     int             argc;
     char           *argv[];
{
  int             OpenModem();
  void            DispatchActions(),
                  GetParameters(),
                  ShowOpenModemErrMsg(),
                  exit();

  char           *arg[REG_BUF], termEmu[REG_BUF];
  int             sepIndex, i, n, retStatus;
#ifdef HAVE_FAS
  char            large_string[1024];
#endif

  XtAppContext    appContext;
  char            modemList[LRG_BUF], 
                 *curModem, 
                 *startupAction = modemList;

  static char    *fallbackResources[] = {
#include "Seyon.ad.h"
    NULL,
  };

  static XrmOptionDescRec optionList[] =  {
    {"-modems", "modems", XrmoptionSepArg, NULL},
    {"-script", "script", XrmoptionSepArg, NULL},
    {"-entries", "defaultPhoneEntries", XrmoptionSepArg, NULL},
    {"-dial", "dialAutoStart", XrmoptionNoArg, "True"},
    {"-nodial", "dialAutoStart", XrmoptionNoArg, "False"},
    {"-emulator", "emulator", XrmoptionSepArg, ""},
    {"-noemulator", "noemulator", XrmoptionNoArg, ""},
    {"-nodefargs", "nodefargs", XrmoptionNoArg, ""},
  };
  
  for (i = 1; i < argc && strcmp(argv[i], "--"); i++);
  sepIndex = i;

  /* Find if the -noemulator switch is given */
  for (i = 1; i < sepIndex && 
	   strncmp(argv[i], "-noemulator", max(4, strlen(argv[i]))); i++);

  /* If no, launch Seyon via the emulator */
  if (i >= sepIndex) {

	for (i = 1; i < sepIndex &&
		 strncmp(argv[i], "-emulator", max(3, strlen(argv[i]))); i++);
	if (i < sepIndex - 1)
	  strcpy(termEmu, argv[i+1]);
	else
	  strcpy(termEmu, SEYON_EMU_NAME);

	/* Find if the -nodefargs switch is given */
	for (i = 1; i < sepIndex && 
		 strncmp(argv[i], "-nodefargs", max(5, strlen(argv[i]))); i++);

	n = 1;

	/* If no, use the default emulator arguments */
	if (i >= sepIndex) {
	  arg[n] = "-name"; n++;
	  arg[n] = "Seyon"; n++;
	  arg[n] = "-T"; n++;
	  arg[n] = "Seyon Terminal Emulator"; n++;
	  arg[n] = "-n"; n++;
	  arg[n] = "Terminal"; n++;
	}

	/* Pass all switches aftetr '--' to the emulator */
	for (i = sepIndex + 1; i < argc; i++, n++)
	  arg[n] = argv[i];
	
	arg[n] = "-e"; n++;

#ifndef HAVE_FAS
	arg[n] = argv[0]; n++;
	arg[n] = "-noemulator"; n++;

	/* Pass all switches before '--' to Seyon */
	for (i = 1; i < sepIndex; i++, n++)
	  arg[n] = argv[i];
#else
	arg[n] = "/bin/sh";	n++;
	arg[n] = "-c"; n++;
	arg[n] = large_string;
	strcpy(arg[n], argv[0]);
	strcat(arg[n], " -noemulator");
	for (i = 1; i < sepIndex; i++) {
	  strcat(arg[n]," ");
	  strcat(arg[n],argv[i]);
	}
	n++;
#endif

	arg[n] = NULL; n++;

	arg[0] = termEmu;
	execvp(arg[0], arg);

	fprintf(stderr, "%s `%s.\n%s\n", ">> Warning: Could not execute", 
			termEmu, ">> Notice: Falling to `xterm'.");

	arg[0] = "xterm";
	execvp(arg[0], arg);
	
	fprintf(stderr, "%s\n%s\n", ">> Error: Could not execute `xterm'.",
			">> Notice: Giving up.");
	exit(1);
  }  /* if (i >= sepIndex)... */

  /* ---------------------------------------------------------------------- */
  /* ---------------------------------------------------------------------- */

  /* The real program begins here */

  topLevel = XtAppInitialize(&appContext, "Seyon", optionList,
							 XtNumber(optionList), &argc, argv,
							 fallbackResources, NULL, 0);
  app_con = appContext;
  GetResources(topLevel);

  mainPid = getpid();
  setup_signal_handlers();
  SetIcon(topLevel);

  printf("\r\n%s %s\r\n", "Seyon Copyright (c) 1992-1993 Muhammad M. Saggaf.",
		 "All rights reserved.");
  printf("\rVersion %s rev. %s %s-%s %s@%s %s %s.\r\n\n", VERSION, REVISION, 
		 COMPILE_HOSTTYPE, COMPILE_OSNAME, COMPILE_BY, COMPILE_HOST,
		 COMPILE_DATE, COMPILE_TIME);

  if ((tfp = fopen("/dev/tty", "r+")) == NULL) {
    PopupInitError("errTtyAccess", exit);
    goto MainLoop;
  }

  tfd = fileno(tfp);

  io_get_attr(tfd, &oldmode);  /* get current console tty mode	*/
  newmode = oldmode;	       /* copy (structure) to newmode  */

#if HAVE_TERMIOS || HAVE_TERMIO
  newmode.c_oflag &= ~OPOST;
  newmode.c_iflag |= (IGNBRK | IGNPAR);
  newmode.c_iflag &= ~(IXON | IXOFF | ISTRIP | BRKINT);
  newmode.c_lflag &= ~(ICANON | ISIG | ECHO);
  newmode.c_cflag |= CREAD;
  newmode.c_cc[VMIN] = 1;
  newmode.c_cc[VTIME] = 1;
#else
#if HAVE_SGTTYB
  newmode.sg_flags = CBREAK;
#endif
#endif

  set_tty_mode();

  InitVariables(topLevel);

  if (argc > 1 && strcmp(argv[1], "--")) {
	SeErrorF("Unknown or incomplete command-line switch: `%s'", argv[1], 
			 "", "");
    PopupInitError("errSwitches", do_exit);
    goto MainLoop;
  }

  /* ---------------------------------------------------------------------- */

  /* Open modem port and configure it */

  strcpy(modemList, qres.modems);
  curModem = GetFirstWord(modemList);

  show("Locating Modems...");

  do {
	if ((retStatus = OpenModem(curModem)) >= 0) break;

	ShowOpenModemErrMsg(curModem, retStatus);
	if (retStatus != ERR_MDM_NOMODEM)
	  show(FmtString("Modem ``%s'' is Unavailable.\n", curModem, "", ""));
	curModem = GetNextWord();
  } while (curModem[0] != '\0');

  if (retStatus < 0) {
	SeError("No Modems Available");
	PopupInitError("errModemInit", do_exit); 
	goto MainLoop;
  }

  show(FmtString("Modem ``%s'' is Available.\n", curModem, "", ""));

  /* ---------------------------------------------------------------------- */

  CreateCommandCenter();

  /* ---------------------------------------------------------------------- */

  pipe(child_pipe);
  pipe(scriptToMainPipe);
  pipe(mainToTermPipe);
  XtAppAddInput(appContext, child_pipe[0], (XtPointer)XtInputReadMask, 
				ExecProcRequest, NULL);
  XtAppAddInput(appContext, scriptToMainPipe[0], (XtPointer)XtInputReadMask, 
				GetParameters, NULL);

  IdleGuard();
  if (qres.showFunMessages) XtAppAddTimeOut(appContext, 
						    qres.funMessagesInterval*1000, FunMessage, NULL);

  /* ---------------------------------------------------------------------- */

  strcpy(startupAction, qres.startupAction);
  if (qres.script) sprintf(startupAction + strlen(startupAction), 
						   " RunScript(%s);", qres.script);
  if (qres.dialAutoStart) strcat(startupAction, " DialEntries(Default);");

  /* ---------------------------------------------------------------------- */

  linkflag = 2;
  ProcRequest(DISPATCH_ACTION, "", startupAction);

  /* ---------------------------------------------------------------------- */

  XtSetMappedWhenManaged(topLevel, True);
  XtMapWidget(topLevel);		/* I don't know why I need this, but I do */

MainLoop:
  XtAppMainLoop(app_con);
  return 0;
}

void
setup_signal_handlers()
{
  signal(SIGINT, SIG_IGN);
  signal(SIGQUIT, SIG_IGN);

#ifdef SIGBUS
  signal(SIGBUS, die);
#endif
  signal(SIGFPE, die);
  signal(SIGILL, die);
  signal(SIGIOT, die);
  signal(SIGSEGV, die);
  signal(SIGTERM, die);
  signal(SIGTRAP, die);
}

void
die(sig)
     int             sig;
{
  void 	          KillChildProc();
  signal(sig, SIG_IGN);

  SeErrorF("Killed by signal %d", sig, "", "");
  SeNoticeF("Debugging info: pid=%d.", getpid(), "", "");

  if (getpid() == mainPid) {
    KillTerminal();
	KillChildProc();
	
	SeNotice("Press any key to exit");
	getchar();
	cleanup_exit(1);
  }
  else {
    write_child_info(child_pipe, KILL_TERM, "Terminal Proc Exited");
	exit(1);
  }
}

void
do_exit(rc)
     int             rc;
{
  void 	          KillChildProc();

  XtUnmapWidget(topLevel);
  KillTerminal();
  KillChildProc();

  unlock_tty();

  fflush(tfp);
  restore_orig_mode();
  fclose(tfp);

  CloseModem();

  XtDestroyApplicationContext(XtWidgetToApplicationContext(topLevel));
  exit(rc);
}

void
cleanup_exit(status)
     int             status;
{
  SeNotice("cleaning up..");
  do_exit(status);
}

void
s_exit()
{
  show("I'm rated PG-34!!");
  do_exit(0);
}
