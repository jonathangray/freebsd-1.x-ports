
/*
 * This file is part of the Seyon, Copyright (c) 1992-1993 by Muhammad M.
 * Saggaf. All rights reserved.
 *
 * See the file COPYING (1-COPYING) or the manual page seyon(1) for a full
 * statement of rights and permissions for this program.
 */

#include "config.h"

#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/time.h>

#include "seyon.h"
#include "SeDecl.h"
#include "SeSig.h"

extern FILE    *tfp;
extern int      tfd;

/*
 * miscellaneous routines
 */

/*
 * a simple toggle
 */

void
toggle_flag(flag)
     Boolean        *flag;
{
  *flag = !*flag;
}

/*
 * print a message to the tty
 */

void
show(msg)
     char           *msg;
{
  fprintf(tfp, "%s\r\n", msg);
}

/*
 * print a formatted message to the tty
 */

void
showf(fmt, a, b, c)
     char           *fmt,
                    *a,
                    *b,
                    *c;
{
  fprintf(tfp, fmt, a, b, c);
  fprintf(tfp, "\r\n");
}

void
SeError(msg)
     char           *msg;
{
  char            buf[REG_BUF];

  sprintf(buf, "\r>> Error: %s.", msg);
  show(buf);
}

void
SeErrorF(fmt, a, b, c)
     char           *fmt,
                    *a,
                    *b,
                    *c;
{
  char            buf[REG_BUF];

  sprintf(buf, fmt, a, b, c);
  SeError(buf);
}

void
se_warning(msg)
     char           *msg;
{
  char            buf[REG_BUF];

  sprintf(buf, "\r>> Warning: %s.", msg);
  show(buf);
}

void
se_warningf(fmt, a, b, c)
     char           *fmt,
                    *a,
                    *b,
                    *c;
{
  char            buf[REG_BUF];

  sprintf(buf, fmt, a, b, c);
  se_warning(buf);
}

void
SeNotice(msg)
     char           *msg;
{
  char            buf[REG_BUF];

  sprintf(buf, "\r>> Notice: %s.", msg);
  show(buf);
}

void
SeNoticeF(fmt, a, b, c)
     char           *fmt,
                    *a,
                    *b,
                    *c;
{
  char            buf[REG_BUF];

  sprintf(buf, fmt, a, b, c);
  SeNotice(buf);
}

void
SePError(msg)
     char           *msg;
{
  char            buf[REG_BUF];

  sprintf(buf, "%s: %s", msg, strerror(errno));
  SeError(buf);
}

void
SePErrorF(fmt, a, b, c)
     char           *fmt,
                    *a,
                    *b,
                    *c;
{
  char            buf[REG_BUF];

  sprintf(buf, fmt, a, b, c);
  SePError(buf);
}

/* ------------------------------------------------------------
 * Routines that have to do with forking or executing external commands
 */

/*
 * SeFork: fork a process and print an error message in case of failure.
 */

int
SeFork()
{
  pid_t           pid;

  if ((pid = fork()) < 0)
    SePError("Faild to fork process");

  return pid;
}

/*
 * execute an external command though a shell
 */

void
ShellCommandHandler(sig, fio_p)
     int             sig;
     XtPointer       fio_p;
{
  void            PostExecPrep();
                
  if (wait((int*)0) < 0) SePError("ShellCommand wait failed");
  XoAppIgnoreSignal(app_con, SIGCHLD);

  set_tty_mode();
  set_modem_fio(*(int *)fio_p);

  SeyonMessage("Shell Command Completed");
  PostExecPrep();
  inhibit_child = False;
}

void
ShellCommand(command)
     char           *command;
{
  ExecShellCommand(command, 1);
}

void
ExecShellCommand(command, top)
     char           *command;
	 int             top;
{
  void            PreExecPrep();
  static char    *shell = NULL;
  char            cmd[REG_BUF],
                 *scmd;
  static int      fio;
  pid_t           forkRes;

  if (command == NULL) return;

  if (shell == NULL) {
    shell = (char*)getenv("SHELL");
    if (!shell) shell = "/bin/sh";
  }

  if (top) PreExecPrep();

  io_set_attr(tfd, &oldmode);
  fio = get_modem_fio();

  if (top)
	XoAppAddSignal(app_con, SIGCHLD, ShellCommandHandler, (XtPointer)&fio);
  else
	signal(SIGCHLD, SIG_IGN);

  forkRes = SeFork();
  if (forkRes == 0) {
    scmd = str_stripspc_copy(cmd, command);

    show("");

    if (*scmd == '$') {
      SeNotice("Redirecting stdin/stdout");
      mattach();				/* Attach modem to stdin/stdout */
      scmd++;
    }

    if (setuid(getuid()) < 0)
      SePError("Failed to set effective uid");

    if (*scmd == '\0') {
      SeNotice(FmtString1("Executing the shell ``%s''", shell));
      execl(shell, shell, (char*)NULL);
      SeError(FmtString1("Execution of the shell ``%s'' failed", shell));
      exit(1);
    }
	
    SeNotice(FmtString1("Executing the command ``%s''", scmd));
    execl(shell, shell, "-c", scmd, (char*)NULL);
    SePError(FmtString1("Execution of the command ``%s'' failed", scmd));
    exit(1);
  }
  else if (forkRes > 0) {
	if (top) inhibit_child = True;
	else {
	  wait((int*)0);			/* Wait for the child process to terminate */
	  set_tty_mode();
	  set_modem_fio(*(int *)fio);
	}
  }  /* if (forkRes == 0)... */
}

void
PreProcessPrep()
{
  SuspContTerminal(TERM_SUSPEND);
  SetKillButtonSens(True);
}

void
PostProcessPrep()
{
  SuspContTerminal(TERM_CONTINUE);
  SetKillButtonSens(False);
}

void
PreExecPrep()
{
  SuspContTerminal(0);
  w_exit_up(False);
}

void
PostExecPrep()
{
  SuspContTerminal(1);
  w_exit_up(True);
}

/*
 * routines related to file handling
 */

/*
 * expand '~' in a file name
 */

char           *
expand_fname(fname, buffer)
     char           *fname,
                    *buffer;
{
  char           *home,
                 *buf,
                  name[REG_BUF];
  int             i;

  str_stripspc_copy(name, fname);
  buf = buffer;

  for (i = 0; name[i]; i++) {
    if (name[i] == '~') {
      if ((home = (char *) getenv("HOME")) == NULL)
	return NULL;
      strcpy(buf, home);
      buf += strlen(home);
    }
    else {
      *buf = name[i];
      buf++;
    }
  }
  *buf = '\0';

  return buffer;
}

/*
 * open a file for reading by searching the current, then default, then
 * home directories
 */

FILE*
open_file(fname, directory)
     char           *fname,
                    *directory;
{
  return open_file_va(fname, directory, NULL);
}

/*
 * similar to the above, but accepts more than one default directory
 */

FILE*
open_file_va(fname, dir1, dir2)
     char           *fname,
                    *dir1,
                    *dir2;
{
  FILE           *fp;
  char            name[REG_BUF],
                  fullname[REG_BUF],
                  buffer[REG_BUF];

  str_stripspc_copy(name, fname);

  if (dir1) {
    sprintf(fullname, "%s/%s", expand_fname(dir1, buffer), name);

    if ((fp = fopen(fullname, "r")) != NULL) {
      strcpy(fname, fullname);
      return fp;
    }

    if (dir2) {
      sprintf(fullname, "%s/%s", expand_fname(dir2, buffer), name);

      if ((fp = fopen(fullname, "r")) != NULL) {
		strcpy(fname, fullname);
		return fp;
      }
    }
  }	/* if (dir1)... */

  if ((fp = fopen(name, "r")) != NULL) {
    strcpy(fname, name);
    return fp;
  }

  SeErrorF("/OFV/ Could not open the file `%s'", name, "", "");
  if (dir1) {
    SeNoticeF("Tried the default directory `%s'", dir1, "", "");
    if (dir2)
      SeNoticeF("Tried the default directory `%s'", dir2, "", "");
  }
  SeNotice("Tried the current directory");

  return NULL;
}

/*
 * another implementation of the above using varargs, currently not used
 */

/*FILE *open_file_va(args)
	 va_list args;
	 va_decl
{
  FILE *fp;
  char *name, *dir, fullname[REG_BUF];
  char buffer[REG_BUF];

  va_start(args);
  name = va_arg(args, char *);

  if (fp = fopen(name, "r"))
	return fp;

  while(dir = va_arg(args, char *))
	{
	  sprintf(fullname, "%s/%s", expand_fname(SSpc(dir), buffer), name);

	  if (fp = fopen(fullname, "r"))
		return fp;
	}

  va_end(args);

  if (dir = (char *) getenv("HOME")) {
	sprintf(fullname, "%s/%s", dir, name);
	
	if (fp = fopen(fullname, "r"))
	  return fp;
  }

  showf("<< Seyon: file '%s' not in current, default, or home directory >>",
		name, "", "");
  return NULL;
}*/

/*
 * read a file into a buffer
 */

void
read_file(fp, line)
     FILE           *fp;
     char           *line[];
{
  char            buffer[REG_BUF + 1];
  int             i;

  for (i = 0; i < MAX_ENT && fgets(buffer, REG_BUF, fp) != NULL; i++)
    line[i] = strcpy((char *)malloc(sizeof(buffer)), SSpc(buffer));
  line[i] = NULL;
}

/*
 * similar to the above, but closes the file after readsing it
 */

void
read_close_file(fp, line)
     FILE           *fp;
     char           *line[];
{
  read_file(fp, line);
  fclose(fp);
}

/*
 * writes data to a pipe
 */

void
write_pipe_data(pd, data, size)
     int            *pd;
     char           *data;
     int             size;
{
  if (write(pd[1], data, size) < 0)
    show("<< Could not write to pipe >>");
}

/*
 * reads data from a pipe
 */

void
read_pipe_data(pd, data, size)
     int            *pd;
     char           *data;
     int             size;
{
  read(pd[0], data, size);
}

/*
 * misc functions
 */

void
IdleGuard()
{
  struct stat     statBuf;
  time_t          idleTime;
  static time_t   totalIdleTime;
  int             timeToNextCall;

  if (qres.idleGuard && !inhibit_child) {
    if (fstat(tfd, &statBuf) < 0) {
      SePError("/IG/ Could not stat the tty");
      return;
    }

    if ((idleTime = time((time_t *) 0) - statBuf.st_mtime) >=
	qres.idleGuardInterval * 0.99) {
      MdmPutString(qres.idleGuardString);
      timeToNextCall = qres.idleGuardInterval;
      totalIdleTime += idleTime;
      SeyonMessagef("Idle for %d minutes", (totalIdleTime + 30) / 60);
    }
    else {
      timeToNextCall = qres.idleGuardInterval - (int)idleTime;
      totalIdleTime = 0;
    }

    XtAppAddTimeOut(app_con, timeToNextCall * 1000,
		    (XtTimerCallbackProc) IdleGuard, (XtPointer) app_con);
  }
}
