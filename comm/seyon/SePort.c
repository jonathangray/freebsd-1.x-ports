
/*
 * This file is part of the Seyon, Copyright (c) 1992-1993 by Muhammad M.
 * Saggaf. All rights reserved.
 *
 * See the file COPYING (1-COPYING) or the manual page seyon(1) for a full
 * statement of rights and permissions for this program.
 */

#include "config.h"

#include <signal.h>
#include <fcntl.h>
#include <errno.h>
#include <limits.h>

#include <X11/Intrinsic.h>

#if HAVE_TERMIOS
#include <termios.h>
#else
#if HAVE_TERMIO
#include <termio.h>
#else
#if HAVE_SGTTYB
#include <sys/ioctl.h>
#endif
#endif
#endif

#if LF_USE_DEV_NUMBERS
#include <sys/stat.h>
#ifdef SVR4
#include <sys/mkdev.h>
#else
#include <sys/sysmacros.h>
#endif /* SVR4 */
#endif /* LF_USE_DEV_NUMBERS */

#if USE_NONSTD_BAUD
#ifdef linux
#include <sys/ioctl.h>
#include <linux/fs.h>
#include <linux/tty.h>
#endif
#endif

#include "seyon.h"
#include "SeDecl.h"

#if !defined(O_NDELAY) && defined(O_NONBLOCK)
#define O_NDELAY O_NONBLOCK
#endif

#if !defined(MAX_INPUT) && defined(_POSIX_MAX_INPUT)
#define MAX_INPUT _POSIX_MAX_INPUT
#endif

extern char TtyReadChar();
extern int  TtyReadStr(),
            TtyTimedReadChar(),
            TtyReadLine(),
            TtyTimedWaitFor(),
            IoGetModemStat();

extern speed_t  io_get_speed();

/*
 * MDELAY is the delay in the output because (on my modem) the command would be
 * ignored if sent at full speed.  Change for other setups. (This setting is
 * for U.S. Robotics Password Modem).
 */

#if HAVE_TERMIOS
static struct termios pmode;	/* modem device control structure */
#else
#if HAVE_TERMIO
static struct termio pmode;		/* modem device control structure */
#else
#if HAVE_SGTTYB
static struct sgttyb pmode;
#endif
#endif
#endif

char            modem_port[REG_BUF]; /* modem port device file string */
static int      mfd = -1;		/* modem port file descriptor */
int             baudrate = B9600; /* baud rate */

void
MdmIFlush()
{
  int TtyIFlush();
  TtyIFlush(mfd);
}

void
MdmOFlush()
{
  int TtyOFlush();
  TtyOFlush(mfd);
}

void
MdmIOFlush()
{
  int TtyIOFlush();
  TtyIOFlush(mfd);
}

void
send_break()
{
  io_send_break(mfd);
}

void
MdmPutString(s)
     char           *s;
{
  char            c;

  usleep(MDELAY);
  for (; (c = *s); s++) {
    if (*s == '^' && *(s + 1))
      if (*(++s) == '^') c = *s;
      else c = *s & 0x1f;

    if (c == '~') sleep(1);
    else send_tbyte(c);
    usleep(MDELAY);
  }
}

void
mprintf(fmt, a, b, c)
     char           *fmt,
                    *a,
                    *b,
                    *c;
{
  char            buf[REG_BUF];

  sprintf(buf, fmt, a, b, c);
  MdmPutString(buf);
}

void
get_modem_attr()
{
  io_get_attr(mfd, &pmode);
  baudrate = io_get_speed(&pmode);
}

void
set_modem_attr()
{
  io_set_speed(&pmode, baudrate);
  io_set_attr(mfd, &pmode);
}

int
get_modem_fio()
{
  return fcntl(mfd, F_GETFL);
}

void
set_modem_fio(fio)
     int             fio;
{
  fcntl(mfd, F_SETFL, fio);
}


/*---------------------------------------------------------------------------+
| MdmSaveRestoreAttr - saves or restores the modem attributes.
+---------------------------------------------------------------------------*/

int
MdmSaveRestoreAttr(action)
	 int action;
{
#if HAVE_TERMIOS
  static struct termios savedAttr;
#else
#if HAVE_TERMIO
  static struct termio savedAttr;
#else
#if HAVE_SGTTYB
  static struct sgttyb savedAttr;
#endif
#endif
#endif

  if (mfd == -1) return -1;

  if (action == ATTR_SAVE)
	return io_get_attr(mfd, &savedAttr);
  else if (action == ATTR_RESTORE) {
	/* Not sure how to do this with sgttyb */
#if HAVE_TERMIOS || HAVE_TERMIO
	savedAttr.c_cflag &= ~HUPCL; /* don't hangup on closing */
#endif
	return io_set_attr(mfd, &savedAttr);
  }

  return -1;
}

int
GetModemStat(newModem)
	 int             newModem;
{
  static Boolean  useModemControl = True;
  int             retStat;

  if (newModem) useModemControl = True;

  if (useModemControl && (retStat = IoGetModemStat(mfd)) < 0) {
	SeErrorF("Could not get control line status for device %s", 
			 modem_port, "", "");
	SeNotice("Disabling status toggles for that device");
	useModemControl = False;
	qres.ignoreModemDCD = True;
    PopupError("errModemControl", NULL);
  }
  
  return (useModemControl ?  retStat : 0);
}

int
Online()
{
  return((GetModemStat(0) & MDM_DCD) ? 1 : 0);
}

void
cancel_dial(verbose)
     int             verbose;
{
  MdmPutString(qres.dialCancelString);
  MdmPurge();

  if (verbose)
    SeyonMessage("Canceled");
}

void
SetInitialModemAttr()
{
#if HAVE_TERMIOS || HAVE_TERMIO
  pmode.c_iflag |= (IGNBRK | IGNPAR);
  pmode.c_iflag &= ~(ISTRIP | BRKINT);
  pmode.c_lflag = 0;

#ifdef XCLUDE
  pmode.c_lflag |= XCLUDE;
#endif

  pmode.c_oflag &= ~OPOST;		/* transparent output */
#if HAVE_TERMIO
  pmode.c_cflag = baudrate | CREAD | CLOCAL;
#else
  pmode.c_cflag = CREAD | CLOCAL;
  pmode.c_ispeed = pmode.c_ospeed = baudrate;
#endif
  /* this many characters satisfy reads */
  pmode.c_cc[VMIN] = min(qres.modemVMin, MAX_INPUT);
  pmode.c_cc[VTIME] = 1;		/* or in this many tenths of a second */
#else
#if HAVE_SGTTYB
  pmode.sg_flags = RAW;
#endif
#endif

  xc_setflow();
  set_rtscts();
}

void
set_rtscts()
{
#ifdef CRTSCTS
  if (qres.rtsctsFlowControl) {
    pmode.c_cflag |= CRTSCTS;

#ifdef CNORTSCTS
    pmode.c_cflag &= ~CNORTSCTS;
#endif

  }
  else {
    pmode.c_cflag &= ~CRTSCTS;

#ifdef CNORTSCTS
    pmode.c_cflag |= CNORTSCTS;
#endif

  }
#endif

  if (mfd != -1)
    io_set_attr(mfd, &pmode);
}

void
xc_setflow()
{
  if (qres.xonxoffFlowControl) {

#if HAVE_TERMIOS
    pmode.c_iflag |= IXON | IXOFF;
#else
#if HAVE_TERMIO
    pmode.c_iflag |= IXON | IXOFF;
    pmode.c_iflag &= ~IXANY;
#else
#if HAVE_SGTOBTYB
    pmode.sg_flags |= TANDEM;
#endif
#endif
#endif

  }
  else
#if HAVE_TERMIOS
    pmode.c_iflag &= ~(IXON | IXOFF);
#else
#if HAVE_TERMIO
    pmode.c_iflag &= ~(IXON | IXOFF);
    pmode.c_iflag &= ~IXANY;
#else
#if HAVE_SGTTYB
    pmode.sg_flags &= ~TANDEM;
#endif
#endif
#endif

  if (mfd != -1)
    io_set_attr(mfd, &pmode);
}

char           *
mport(s)			/* get/set port string */
     char           *s;
{
  if (s != NULL)
    strcpy(modem_port, s);
  return (modem_port);
}

int
MdmSetGetBaud(baudIndex)
     int             baudIndex;
{
  static char    *baud[] = {"0", "300", "1200", "2400", "4800", "9600", 
							  "19200", "38400", "57600", "115200", NULL};
  long            retBaud;

  if (baudIndex != -1)
    retBaud = mbaud(baud[baudIndex]);
  else
    retBaud = mbaud(NULL);

  for (baudIndex = 0; baud[baudIndex]; baudIndex++)
    if (atol(baud[baudIndex]) == retBaud)
      return baudIndex;

  return -1;
}

int
MdmSetGetCSize(bits)
     int             bits;
{
  if (bits != -1) {
    switch (bits) {
#if HAVE_TERMIOS || HAVE_TERMIO
    case 5:
      pmode.c_cflag &= ~CSIZE;
      pmode.c_cflag |= CS5;
	  MdmSetGetIStrip(1);
      break;
    case 6:
      pmode.c_cflag &= ~CSIZE;
      pmode.c_cflag |= CS6;
	  MdmSetGetIStrip(1);
      break;
    case 7:
      pmode.c_cflag &= ~CSIZE;
      pmode.c_cflag |= CS7;
	  MdmSetGetIStrip(1);
      break;
    case 8:
      pmode.c_cflag &= ~CSIZE;
      pmode.c_cflag |= CS8;
	  MdmSetGetIStrip((int)qres.stripHighBit);
      break;
#else
#if HAVE_SGTTYB
    case 5:
    case 6:
    case 7:
      pmode.sg_flags |= CBREAK;
      pmode.sg_flags &= ~RAW;
      MdmSetGetIStrip(0);
      break;
    case 8:
      pmode.sg_flags |= RAW;
      pmode.sg_flags &= ~CBREAK;
      MdmSetGetIStrip(0);
      break;
#endif
#endif
    default:
      SeErrorF("invalid number of bits: %d", bits, "", "");
      return -1;
    }
	/* io_set_attr is called in  MdmSetGetIStrip */
  }

  if (mfd != -1)
    io_get_attr(mfd, &pmode);

#if HAVE_TERMIOS || HAVE_TERMIO
  switch (pmode.c_cflag & CSIZE) {
  case CS5:
    return 5;
  case CS6:
    return 6;
  case CS7:
    return 7;
  case CS8:
    return 8;
  default:
    SeError("Consistency error in number of bits");
    return -1;
#else
#if HAVE_SGTTYB
  switch (pmode.sg_flags & CBREAK) {
  case 0:
    return 8;
  default:
    return 7;
#endif
#endif
  }
}

int
MdmSetGetParity(parity)
     int             parity;
{
  if (parity != -1) {
    switch (parity) {
#if HAVE_TERMIOS || HAVE_TERMIO
    case 0:
      pmode.c_cflag &= ~PARENB;
      break;
    case 1:
      pmode.c_cflag |= (PARENB | PARODD);
      break;
    case 2:
      pmode.c_cflag |= PARENB;
      pmode.c_cflag &= ~PARODD;
      break;
#else
#if HAVE_SGTTYB
    case 0:
      pmode.sg_flags &= ~ANYP;
      break;
    case 1:
      pmode.sg_flags |= ODDP;
      pmode.sg_flags &= ~EVENP;
      break;
    case 2:
      pmode.sg_flags |= EVENP;
      pmode.sg_flags &= ~ODDP;
      break;
#endif
#endif
    default:
      SeErrorF("Invalid parity: %d", parity, "", "");
      return -1;
    }
    if (mfd != -1)
      io_set_attr(mfd, &pmode);
  }

  if (mfd != -1)
    io_get_attr(mfd, &pmode);

#if HAVE_TERMIOS || HAVE_TERMIO
  if (!(pmode.c_cflag & PARENB))
    return 0;
  else if (pmode.c_cflag & PARODD)
    return 1;
  else
    return 2;
#else
#if HAVE_SGTTYB
  switch (pmode.sg_flags & ANYP) {
  case ODDP:
    return 1;
  case EVENP:
    return 2;
  default:
    return 0;
  }
#endif
#endif
}

int
MdmSetGetIStrip(flag)
     int             flag;
{
#if HAVE_TERMIOS || HAVE_TERMIO
  if (flag != -1) {
	if (flag)
	  pmode.c_iflag |= ISTRIP;
	else
	  pmode.c_iflag &= ~ISTRIP;
	
    if (mfd != -1)
      io_set_attr(mfd, &pmode);
  }

  if (mfd != -1)
    io_get_attr(mfd, &pmode);

  if (pmode.c_iflag & ISTRIP)
    return 1;
  else
    return 0;
#else
#if !HAVE_SGTTYB
  if (mfd != -1)
    io_set_attr(mfd, &pmode);

  return 0;
#endif
#endif
}

int
MdmSetGetStopBits(bits)
     int             bits;
{
#if HAVE_TERMIOS || HAVE_TERMIO
  if (bits != -1) {
    switch (bits) {
    case 1:
      pmode.c_cflag &= ~CSTOPB;
      break;
    case 2:
      pmode.c_cflag |= CSTOPB;
      break;
    default:
      SeErrorF("invalid number of stop bits: %d", bits, "", "");
      return -1;
    }
    if (mfd != -1)
      io_set_attr(mfd, &pmode);
  }

  if (mfd != -1)
    io_get_attr(mfd, &pmode);

  if (pmode.c_cflag & CSTOPB)
    return 2;
  else
    return 1;
#else
#if !HAVE_SGTTYB
  if (mfd != -1)
    io_set_attr(mfd, &pmode);

  return 1;
#endif
#endif
}

/*
 * Get/set the baud rate of the modem port. If the port hasn't been opened
 * yet, just store in pmode for mopen() to use when it opens the port.
 */
long
mbaud(s)
     char           *s;
{
#if USE_NONSTD_BAUD
#ifdef linux
  static struct serial_struct ser_io;

  if (ioctl(mfd, TIOCGSERIAL, &ser_io) < 0) {
    SePError("Could not get linux serial info");
    return -1;
  }
#endif
#endif

  if (s != NULL) {
    /* this gives a more limited, realistic */
    switch (atol(s)) {	       /* range than in sgtty.h */
    case 300:
      baudrate = B300;
      break;
    case 1200:
      baudrate = B1200;
      break;
    case 2400:
      baudrate = B2400;
      break;
    case 4800:
      baudrate = B4800;
      break;
    case 9600:
      baudrate = B9600;
      break;
    case 19200:
      baudrate = B19200;
      break;
    case 38400:
      baudrate = B38400;
#ifdef linux
      ser_io.flags &= ~ASYNC_SPD_MASK;
#endif
      break;
    case 57600:
      baudrate = B38400;
#ifdef linux
      ser_io.flags &= ~ASYNC_SPD_MASK;
      ser_io.flags |= ASYNC_SPD_HI;
#endif
      break;
    case 115200:
      baudrate = B38400;
#ifdef linux
      ser_io.flags &= ~ASYNC_SPD_MASK;
      ser_io.flags |= ASYNC_SPD_VHI;
#endif
      break;
    default:
      return (-1);
    }
    io_set_speed(&pmode, baudrate);
    if (mfd != -1) {
      io_set_attr(mfd, &pmode);
#ifdef linux
      if (baudrate == B38400)
	if (ioctl(mfd, TIOCSSERIAL, &ser_io) < 0) {
	  SePError("Could not set linux serial info");
	  return -1;
	}
#endif
    }
  }

  if (mfd != -1)
    io_get_attr(mfd, &pmode);

  switch (io_get_speed(&pmode)) {
  case B300:
    return (300);
  case B1200:
    return (1200);
  case B2400:
    return (2400);
  case B4800:
    return (4800);
  case B9600:
    return (9600);
  case B19200:
    return (19200);
  case B38400:
#ifdef linux
    if (mfd != -1)
      if (ioctl(mfd, TIOCGSERIAL, &ser_io) < 0) {
		SePError("Could not get linux serial info");
		return -1;
      }

    if ((ser_io.flags & ASYNC_SPD_MASK) == ASYNC_SPD_VHI)
      return 115200;
    else if ((ser_io.flags & ASYNC_SPD_MASK) == ASYNC_SPD_HI)
      return 57600;
    else
      return 38400;
#else
      return 38400;
  case B57600:
      return 57600;
  case B115200:
      return 115200;
#endif
  }

  SeError("Consistency error in baud rate");
  return -1;
}

/*
 * The following routine is used to hang up the modem.  This is accomplished
 * by setting the baud rate to 0.  According to my documentation on termio,
 * setting the baud rate to zero will result in DTR not being asserted. This
 * hangs up some (most?) modems.  If not, the second part of the routine
 * sends the Hayes modem "escape" and then a hangup command.
 */

void
MdmHangup()
{
  int terminalWasActive;

  if (mfd == -1) return;

  terminalWasActive = SuspContTerminal(TERM_SUSPEND);
  if (qres.hangupViaDTR) {
    io_set_speed(&pmode, B0);	/* set baud 0 (drop DTR) */
    io_set_attr(mfd, &pmode);

    sleep(1);					/* wait a second */

    io_set_speed(&pmode, baudrate);	/* reset baud rate */
    io_set_attr(mfd, &pmode);
  }
  else {						/* use Hayes command */
    sleep(2);					/* allow for "escape guard time" */
    MdmPutString(qres.modemAttentionString); /* send modem escape command */

    sleep(3);					/* more "escape guard time" */
    MdmPutString(qres.modemHangupString); /* send hangup command */
  }
  MdmPurge();
  if (terminalWasActive) SuspContTerminal(TERM_CONTINUE);
  
  UpdateStatusBox(NULL);
}

  
/*
 * Opens the modem port and configures it. Returns 0 for success or -1 on
 * error.
 */

int
OpenModem(modem)
	 String modem;
{
  int             LockModem(),
                  UnlockModem();
  void            MdmIOFlush();
  int             oldFlags;

  if (modem == NULL || modem[0] == '\0') return ERR_MDM_NOMODEM;
  if (LockModem(modem)) return ERR_MDM_LOCKED;

  /* Need O_NDELAY to get the file open before we have carrier */
  if ((mfd = open(modem, O_RDWR | O_NDELAY)) < 0) 
	{UnlockModem(modem); return ERR_MDM_OPENFAILED;}

  /* Now, we must reset the O_NDELAY mode so that read() works correctly */
  if (((oldFlags = fcntl(mfd, F_GETFL, 0)) == -1) ||
      (fcntl(mfd, F_SETFL, oldFlags & ~O_NDELAY) == -1)) 
	{UnlockModem(modem); return ERR_MDM_RESETFLAGSFAILED;}

#if HAVE_SGTTYB
  if (ioctl(mfd, TIOCEXCL) < 0)
    SePError("exclusive-use");
#endif

  mport(modem);

  MdmIOFlush();
  MdmSaveRestoreAttr(ATTR_SAVE);
  SetInitialModemAttr();
  io_set_attr(mfd, &pmode);

  GetModemStat(1);

  if (mbaud(qres.defaultBPS) == -1)
    se_warningf("invalid default BPS value: %s", qres.defaultBPS, "", "");
  if (MdmSetGetCSize(qres.defaultBits) < 0)
    se_warningf("invalid default number of bits: %s", qres.defaultBits, 
				"", "");
  if (MdmSetGetParity(qres.defaultParity) < 0)
    se_warningf("invalid default parity value: %s", qres.defaultParity, 
				"", "");
  if (MdmSetGetStopBits(qres.defaultStopBits) < 0)
    se_warningf("invalid default number of stop bits: %s",
				qres.defaultStopBits, "", "");

  return 0;
}

void
ShowOpenModemErrMsg(modemName, retStatus)
	 String    modemName;
	 int       retStatus;
{
  switch(retStatus) {
  case ERR_MDM_NOMODEM:
	SeError("No Modem Specified");
	break;
  case ERR_MDM_LOCKED:
	SeError(FmtString("Modem ``%s'' is Locked", modemName, "", ""));
	break;
  case ERR_MDM_OPENFAILED:
	SePError(FmtString("Unable to Open Modem ``%s''", modemName, "", ""));
	break;
  case ERR_MDM_RESETFLAGSFAILED:
	SePError(FmtString("Unable to Reset Flags for Modem ``%s''", 
					   modemName, "", ""));
	break;
  default:
	SeError(FmtString("Unknown Error While Openeong Modem ``%s''", 
					  modemName, "", ""));
	break;
  }
}

int
CloseModem()
{
  if (mfd == -1) return -1;
  MdmSaveRestoreAttr(ATTR_RESTORE);
  close(mfd);
  return 0;
}

/*
 * Attach standard input and output to the modem port. This only gets called
 * after a fork by the child process; which then exec's a program that uses
 * standard i/o for some data transfer protocol. (To put this here is
 * actually a kludge, but I wanted to keep the modem-specific stuff in a
 * black box.)
 */
void
mattach()
{
  extern int dup2();
  /*
   * attach standard i/o to port
   */
  dup2(mfd, 0);					/* close local stdin and connect to port */
  dup2(mfd, 1);					/* close local stdout and connect to port */

  close(mfd);					/* close the old port descriptor */
}

/* ------------------------------------------------------------
 * Routines to read from the modem.
 */

/*
 * MdmReadStr: reads a bunch of characters from the modem.
 */

int
MdmReadStr(buf)
	 char           *buf;
{
  return TtyReadStr(mfd, buf);
}

/*
 * MdmReadChar: reads one character from the modem.
 */

char
MdmReadChar(readChar)
	 char           *readChar;
{
  return TtyReadChar(mfd, readChar);
}

int
MdmTimedReadChar(readChar, expireTime)
	 char           *readChar;
     int             expireTime;
{
  return TtyTimedReadChar(mfd, readChar, expireTime);
}

/*
 * MdmReadLine: reads one line from the modem.
 */

int
MdmReadLine(buf)
	 char           *buf;
{
  return TtyReadLine(mfd, buf);
}

int
MdmTimedWaitFor(expectedString, waitTime)
	 char           *expectedString;
     int             waitTime;
{
  return TtyTimedWaitFor(mfd, expectedString, waitTime);
}

/*
 * MdmPurge: throws away all incoming characters until no more are sent.
 */

void
MdmPurge()
{
  char            c;
  while (MdmTimedReadChar(&c, 1) >= 0);
}

#ifdef retired
int
readbyte(seconds)
     int             seconds;
{
  return trminp(mfd, seconds);
}
#endif

/*
 *    Output a byte to the modem port. All data sent to the modem
 *    is output through this routine.
 */

void
sendbyte(ch)
     int             ch;
{
  char            c = ch & 0xff;

  if (write(mfd, &c, 1) < 0)
    SePError("character write");
}

void
sendf_slowly(format, a, b, c)
     char           *format,
                    *a,
                    *b,
                    *c;
{
  char            buffer[SM_BUF];

  sprintf(buffer, format, a, b, c);
  send_slowly(buffer);
}

void
send_slowly(s)
     char           *s;
{
  while (*s) {
    sendbyte(*s++);
    /*
     * avoid busy waiting by using usleep if available. adjust MDELAY if
     * you're having trouble
     */
    usleep(MDELAY);
  }
}

/*
 * lock_tty() returns non-zero if the lock file exists (prevents Seyon from
 * running).
 *
 * unlock_tty() deletes the lock file.
 *
 * Simple, eh?
 */

char            lckf[SM_BUF];
char            ltmp[SM_BUF];
pid_t           lockPid;

int
LockModem(modem)
	 String modem;
{
  strcpy(modem_port, modem);
  return lock_tty();
}

int
UnlockModem(modem)
	 String modem;
{
  unlock_tty();
  return 0;
}

int
lock_tty()
{
  int             lfd;
  pid_t           pid,
                  lckpid;
  char           *modemname;
#if LF_USE_ASCII_PID
  char            pidstr[20],
                  lckpidstr[20];
  int             nb;
#endif
#if LF_USE_DEV_NUMBERS
  struct stat  mbuf;
#endif

  /* Get our PID, and initialize the filename strings */
  pid = getpid();

#if !LF_USE_DEV_NUMBERS
  modemname = strrchr(modem_port, '/');
  sprintf(lckf, "%s/%s%s", LF_PATH, LF_PREFIX, 
		  (modemname ? (modemname + 1) : modem_port));
#else
  if(stat(modem_port, &mbuf) < 0) {
	SePErrorF("could not stat modem port %s", modem_port, "", "");
	return -1;
  }
  sprintf(lckf,"%s/%s%03u.%03u.%03u", LF_PATH, LF_PREFIX, major(mbuf.st_dev),
		  major(mbuf.st_rdev), minor(mbuf.st_rdev));
#endif /* LF_USE_DEV_NUMBERS */

  sprintf(ltmp, "%s/%s%d", LF_PATH, "LTMP.", pid);
  /* Create the LTMP.<pid> file and scribble our PID in it */
  unlink(ltmp);
  if ((lfd = creat(ltmp, 0644)) == -1) {
    SePErrorF("Could not create temporary lock file %s", ltmp, "", "");
    return -1;
  }

#if LF_USE_ASCII_PID
  sprintf(pidstr, "%10d\n", pid);
  write(lfd, pidstr, 11);
#else
  write(lfd, (char*)&pid, sizeof(pid));
#endif
  close(lfd);

  /*
   * Attempt to link directly - if it works, we're done.
   */
relink:
  if (link(ltmp, lckf) == 0) {
    unlink(ltmp);
    lockPid = pid;
    return 0;
  }

  /*
   * Oh brother, there's a LCK..* file already; we must now expend effort to
   * learn if it's stale or not.
   */
  if ((lfd = open(lckf, O_RDONLY)) != -1) {

#if LF_USE_ASCII_PID
    for (nb = 0; nb < 20 && read(lfd, lckpidstr + nb, sizeof(char)); nb++);
    if (nb) {
      lckpid = atol(lckpidstr);
#else
    if (read(lfd, (char *)&lckpid, sizeof(lckpid)) == sizeof(lckpid)) {
#endif

      lockPid = (pid_t) lckpid;
      if (kill(lckpid, 0) == 0) {
		SeErrorF("Device %s is locked by process %d", modem_port, lckpid, "");
		unlink(ltmp);
		return -1;
      }
    }
  }

  /*
   * The LCK..* file was stale.  Remove and retry.
   */
  if (unlink(lckf)) {
    SePErrorF("Unable to unlink stale lock file \"%s\"", lckf, "", "");
    unlink(ltmp);
    return -1;
  }
  goto relink;
  /* NOTREACHED */
}

void
unlock_tty()
{
  /* Don't remove the lock file unless it's the one we created */
  if (getpid() == lockPid)
    unlink(lckf);
}
