
/*
 * This file is part of the Seyon, Copyright (c) 1992-1993 by Muhammad M.
 * Saggaf. All rights reserved.
 *
 * See the file COPYING (1-COPYING) or the manual page seyon(1) for a full
 * statement of rights and permissions for this program.
 */

#include "config.h"

#include <stdio.h>
#ifndef NOSTDHDRS
#include <stdlib.h>
#endif
#include <unistd.h>
#include <signal.h>
#include <setjmp.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/time.h>
#include <ctype.h>
#include <math.h>

#include "SeDecl.h"

void
ReadCommentedFile(fp, line)
     FILE           *fp;
     char           *line[];
{
  char            buffer[REG_BUF + 1],
                 *bufPtr,
                 *ptr;
  int             i;

  for (i = 0; i < MAX_ENT && fgets(buffer, REG_BUF, fp) != NULL;) {

    /*Strip newline character from end of string*/
    buffer[strlen(buffer) - 1] = '\0';

    /*Remove leading and trailing spaces*/
    bufPtr = SSpc(buffer);

    /*Ignore this line if it is empty or starts with a comment*/
    if (bufPtr[0] == '\0' || bufPtr[0] == '#')
      continue;

    /*Ignore trailing comments*/
    if ((ptr = (char *) strrchr(bufPtr, '#')) != NULL)
      *ptr = '\0';

    line[i++] = XtNewString(bufPtr);
  }

  line[i] = NULL;
}

void
FreeList(listArr)
     XtPointer       listArr[];
{
  int             i;

  for (i = 0; listArr[i]; i++)
    XtFree(listArr[i]);

  listArr[0] = NULL;
}

int
ConvertStringToIntArray(str, intArr)
	 char *str;
	 int  intArr[];
{
  char num[TIN_BUF], *numPtr;
  int i;
  
  intArr[0] = 0;

  if (!str) return 0;

  for (i = 0;;) {
	while (isspace(*str)) str++;
	if (!*str) return 0;

	for (numPtr = num; *str && !isspace(*str); *numPtr++ = *str++);
	*numPtr = '\0';
	intArr[i] = atoi(num);
	intArr[++i] = 0;
  }
}

/* ------------------------------------------------------------
 * Routines to read from the tty.
 */

/*
 * TtyReadStr: reads a bunch of characters from the modem.
 */

int
TtyReadStr(fd, buf)
     int             fd;
	 char           *buf;
{
  int      count;

  /* The do-loop is used to restart the system call if it is interrupted by 
	 a signal. This is simpler than missing with SA_INTERRUPT */
  do
	/* BUFSIZ is defined in stdio.h */
	count = read(fd, buf, BUFSIZ);
  while (count < 0 && errno ==  EINTR);

  if (count < 0) {
	SePError("character read"); 
	return -1;
  }

  return count;
}

jmp_buf         timedReadEnv;

void
timedReadAlarmHandler(dummy)
     int             dummy;
{
  longjmp(timedReadEnv, 1);
}

int
TtyTimedReadChar(fd, readChar, expireTime)
     int             fd;
	 char           *readChar;
     int             expireTime;
{
  /* BUFSIZ is defined in stdio.h */
  static char     readBuf[BUFSIZ],
                 *bufPtr;
  static int      count = 0;
  static void     (*oldSigHandler)();
  static unsigned oldAlarmTimeLeft;

  if (count > 0) {
    count--;
	*readChar = *++bufPtr;
    return 0;
  }

  if (expireTime > 0) {
	oldSigHandler = signal(SIGALRM, timedReadAlarmHandler);
	oldAlarmTimeLeft = alarm((unsigned)expireTime);

	if (setjmp(timedReadEnv) != 0) {
	  alarm(max(oldAlarmTimeLeft - expireTime, 0));
	  signal(SIGALRM, oldSigHandler);
	  return -1;
	}
  }

  count = TtyReadStr(fd, (bufPtr = readBuf));

  if (expireTime > 0) {
	alarm(max(oldAlarmTimeLeft - expireTime + alarm(0), 0));
	signal(SIGALRM, oldSigHandler);
  }

  if (count < 0) return count;

  count--;
  *readChar = *bufPtr;

  return 0;
}

/*
 * TtyReadChar: reads one character from the tty.
 */

char
TtyReadChar(fd, readChar)
     int             fd;
	 char           *readChar;
{
  return TtyTimedReadChar(fd, readChar, 0);
}

/*---------------------------------------------------------------------------+
| TtyReadLine - reads one line from the tty.
+---------------------------------------------------------------------------*/

int
TtyReadLine(fd, buf)
     int             fd;
	 char           *buf;
{
  char            c;
  int             i, readCharRet;

  while ((readCharRet = TtyReadChar(fd, &c)) >= 0 && (c == '\r' || c == '\n'));
  if (readCharRet < 0) return readCharRet;

  i = 0;
  do buf[i++] = c;
  while ((readCharRet = TtyReadChar(fd, &c)) >=0 && (c != '\r' && c != '\n'));
  
  buf[i] = '\0';
  return readCharRet;
}

int
TtyTimedWaitFor(fd, expectedString, waitTime)
     int             fd;
	 char           *expectedString;
     int             waitTime;
{
  time_t          expireTime;
  char            c, *ptr = expectedString;

  expireTime = time((time_t*)0) + waitTime;

  while (expireTime != time((time_t*)0)) {
    if ((TtyTimedReadChar(fd, &c, 1)) < 0)
      continue;

    if ((char)c != *ptr) ptr = expectedString;
    else if (*++ptr == '\0') return 0;
  } /* while... */

  return -1;
}

/* ------------------------------------------------------------
 * Miscellaneous routines.
 */

/*
 * usleep: for systems that do not have it.
 */

#if !HAVE_USLEEP
void
usleep(usec)
	 unsigned long usec;
{
#if HAVE_SELECT

  /* Orest Zborowski originally wrote this */

  struct timeval  timeout;

  timeout.tv_sec = usec / 1000000;
  timeout.tv_usec = usec - 1000000 * timeout.tv_sec;
  select(1, NULL, NULL, NULL, &timeout);
#else

/* This busy-waiting, normally a bad idea on a multi-tasking system, is used
   because sleep(1) is way too much of a delay. */

  int             i;

  for (i = 0; i < usec; i++);
#endif /* HAVE_SELECT */
}
#endif /* HAVE_USLEEP */

/* 
 * dup2: for systems that do not have it.
 */

#if !HAVE_DUP2
int
dup2(oldfd, newfd)
     int             oldfd,
                     newfd;
{
  if (fcntl(oldfd, F_GETFL, 0) == -1)	/* Valid file descriptor? */
    return (-1);	       /* No, return an error. */
  close(newfd);		       /* Ensure newfd is closed */
  return (fcntl(oldfd, F_DUPFD, newfd));	/* Dup oldfd into newfd */
}

#endif /* HAVE_DUP2  Thanks to Bill Allie CIS: 76703,2061 */
