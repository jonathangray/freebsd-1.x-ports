/*
 * This file is part of the Seyon, Copyright (c) 1992-1993 by Muhammad M.
 * Saggaf. All rights reserved.
 *
 * See the file COPYING (1-COPYING) or the manual page seyon(1) for a full
 * statement of rights and permissions for this program.
 */

/*
 * This file contains routines for Seyon's terminal. The main routine is
 * terminal(), which reads characters from the terminal and sends them to the
 * port. That routine also forks a child process that reads characters from
 * the port and writes them to the temrinal. Once the parent receives SIGTERM
 * (which should be sent by the grand parent), it kills the child and exits.
 */

#include <stdio.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>

#include <X11/Intrinsic.h>

#include "seyon.h"
#include "SeDecl.h"

extern char     TtyReadChar();
extern int      MdmReadStr();

extern FILE    *tfp,            /* terminal pointer */
               *cfp;            /* capture file pointer */
extern Boolean  capture;        /* are we capturing or not ? */
extern int      tfd,            /* terminal descriptor */
                mainToTermPipe[];
extern pid_t    mainPid;

void            send_tbyte(),
                toggle(),
                cleanup();

pid_t           readProcPid = 0; /* pid of child process */

/*---------------------------------------------------------------------------+
| DoNothingHandler - handler for doing nothing.
+---------------------------------------------------------------------------*/

void
DoNothingHandler(sigNum)
     int       sigNum;
{
#ifdef DEBUG
  showf(">>> in DoNothingHandler, pid = %d, sig = %d", getpid(), sigNum);
#endif

  /* Reinstall the signal handler */
  signal(sigNum, DoNothingHandler);
}

/*---------------------------------------------------------------------------+
| TerminalKillHandler - handler for the terminal kill (SIGTERM).
+---------------------------------------------------------------------------*/

void
TerminalKillHandler(dummy)
     int             dummy;
{
  signal(SIGTERM, SIG_IGN);

  /* Kill the child process and wait for it to die, sounds vicious, 
     doesn't it? The child process is the read process from the port */

  if (readProcPid && kill(readProcPid, SIGTERM) == 0) 
    while(wait((int*)0) < 0);
  
  fflush(tfp);
  exit(0);
}

/*---------------------------------------------------------------------------+
| TerminalSuspendHandler - handler for the terminal suspension (SIGUSR1).
+---------------------------------------------------------------------------*/

void
TerminalSuspendHandler(sigNum)
     int             sigNum;
{
  /* Don't deliver or stack (as pending) any more instances of the signal
     until we've woken up (below) */
  signal(sigNum, SIG_IGN);

  if (readProcPid) kill(readProcPid, sigNum);

  pause();

#ifdef DEBUG
  showf(">>> Terminal %d woke up - good", getpid());
#endif

  /* After waking up */

  if (readProcPid) kill(readProcPid, SIGCONT);

  /* Reinstall the signal handler */
  signal(sigNum, TerminalSuspendHandler);
}

/*---------------------------------------------------------------------------+
| TerminalRefreshParametersHandler - handler for the terminal to get the 
|                                    current session parameters (SIGUSR2).
+---------------------------------------------------------------------------*/

void
TerminalRefreshParametersHandler(sigNum)
     int       sigNum;
{
  int        PutParameters();
  void       GetParameters();

  /* Don't deliver or stack (as pending) any more instances of the signal
     until we're finished with the work at hand */
  signal(sigNum, SIG_IGN);

  GetParameters(NULL, mainToTermPipe);

  if (readProcPid && (PutParameters(mainToTermPipe) < 0 || 
                      kill(readProcPid, sigNum) != 0))
    SePError("Could not pipe parameters");

  /* Reinstall the signal handler */
  signal(sigNum, TerminalRefreshParametersHandler);
}

/*
 * Terminal: main routine. Has two processes, one to read from terminal 
 *           and send to the port and the other to do the oppsite.
 */

void
Terminal()
{
  char            c;

  /* Tell the program where to go when signals are received */
  signal(SIGTERM, TerminalKillHandler);
  signal(SIGUSR1, TerminalSuspendHandler);
  signal(SIGUSR2, TerminalRefreshParametersHandler);
  signal(SIGCONT, DoNothingHandler);
  signal(SIGCHLD, SIG_DFL);

  /* Split into read and write processes */

  /* Child, read proc: read from port and write to tty */
  if ((readProcPid = SeFork()) == 0)
    PortToTty();

  /* Parent, write proc: read from tty and write to port */

  while (1)
    if (TtyReadChar(tfd, &c) >= 0) send_tbyte(c);
    else {
      SeError("TTY read error. Terminal process exiting");
      if (readProcPid && kill(readProcPid, SIGTERM) == 0) 
        while(wait((int*)0) < 0);
      ProcRequest(POPUP_ERROR, "TTY Read Error", "errReadError");
      exit(1);
    }
  /*NOT REACHED*/
}

/*
 * Read from the port and write to the tty
 */

void
PortToTty()
{
  static char           zmSig[] = "**\030B00";
  static char          *zmSigPtr = zmSig;
  char                  buf[BUFSIZ], c;
  int                   n, i;

  signal(SIGTERM, TerminalKillHandler);

  /* Let the main process know we're all set */
  kill(mainPid, SIGCONT);

  while (1) {

    /* Read incoming charaters and exit the process if a read error
       is encountered */

    if ((n = MdmReadStr(buf)) < 0) {
      SeError("Modem read error. Terminal process exiting");
      ProcRequest(POPUP_ERROR, "Modem Read Error", "errReadError");
      ProcRequest(KILL_TERM, "", "");
      exit(1);
    }

    /* Write incoming characters to the tty */
    fwrite(buf, sizeof(char), n, tfp); 
    fflush(tfp);

    for(i = 0; i < n; i++) {
      c = buf[i];
      
      /* Write to capture file if capture is enabled */
      if (capture) fputc(c, cfp);

      /* Look for Zmodem signature */
      if (c != *zmSigPtr)
        zmSigPtr = zmSig;
      else if (*++zmSigPtr == '\0' && qres.autoZmodem) 
        ProcRequest(DISPATCH_ACTION, "Zmodem Auto-Download", 
					qres.autoZmodemAction);

    } /* for... */
  } /* while(1)... */
  /*NOT REACHED*/
}

/*
 * send a translated character to the modem
 */

void
send_tbyte(c)
     int             c;
{
  switch (c) {

    /*Translate new line to carriage return if newline translation mode is
      in effect*/
  case '\n':
    switch (newlineTrMode) {
    case 2:
      c = '\r';
      break;
    case 3:
      sendbyte('\r');
    default:
      break;
    }
    break;

    /*Translate backspace to delete if del translation mode is in effect*/
  case 0x08:
    if (qres.backspaceTranslation)
      c = 0x7f;
    break;

  default:
    break;
  }

  /*Send ESC before the character if meta key is pressed with the  character
    and the meta key translation mode is on*/
  if (qres.metaKeyTranslation && (c & 0x80)) {
    sendbyte('\033');
    sendbyte(c);
  }

  /*Send the character to the port*/
  else
    sendbyte(c);
}

/*---------------------------------------------------------------------------+
| Routines to manipulate the terminal process. 
+---------------------------------------------------------------------------*/

pid_t           termProcPid = 0; /* pid of the terminal process */

/*---------------------------------------------------------------------------+
| StartTerminal - starts the terminal process.
+---------------------------------------------------------------------------*/

void
StartTerminal()
{
  /* Child Process */
  if ((termProcPid = SeFork()) == 0) 
	{Terminal(); exit(1);}
}

/*---------------------------------------------------------------------------+
| KillTerminal - kills the terminal process.
+---------------------------------------------------------------------------*/

void
KillTerminal()
{
  void     (*oldSigHandler)();

  if (termProcPid == 0) return;
  /* Make sure it's not suspended so that it can react to SIGTERM */
/*  if (SuspContTerminal(TERM_CONTINUE) == 0) return;*/

  oldSigHandler = signal(SIGCHLD, SIG_DFL);

  /* Kill the child and wait for it to die */
  if (termProcPid && kill(termProcPid, SIGTERM) == 0) 
    {while(wait((int*)0) < 0); termProcPid = 0;}

  signal(SIGCHLD, oldSigHandler);
}

/*---------------------------------------------------------------------------+
| SuspContTerminal - suspends or resumes the terminal process.
+---------------------------------------------------------------------------*/

int
SuspContTerminal(state)
     int state;
{
  /* This variable keeps track of whether 
     the terminal is active or suspended */
  static int termSuspended = 0;
  
  if (termProcPid == 0) return 0;

  if (state == TERM_CONTINUE) {
    if (termProcPid && kill(termProcPid, SIGCONT) == 0) 
      termSuspended = 0;
    return 1;
  }

  if (termSuspended) return 0;
  if (termProcPid && kill(termProcPid, SIGUSR1) == 0) 
    {termSuspended = 1; return 1;}
  else
    return 0;
}

int
TerminalRefreshParameters()
{
  int    PutParameters();

  if (termProcPid && (PutParameters(mainToTermPipe) < 0 || 
                      kill(termProcPid, SIGUSR2) != 0))
    return -1;
  else return 0;
}

/*
 * Restart the terminal process (refresh) by killing it and starting a new
 * one
 */

void
RestartTerminal()
{
  KillTerminal();
  StartTerminal();
}
