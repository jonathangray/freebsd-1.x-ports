/*****************************************************************************
 *
 *  xdbx - X Window System interface to the dbx debugger
 *
 *  Copyright 1989 The University of Texas at Austin
 *  Copyright 1990 Microelectronics and Computer Technology Corporation
 *
 *  Permission to use, copy, modify, and distribute this software and its
 *  documentation for any purpose and without fee is hereby granted,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name of The University of Texas
 *  and Microelectronics and Computer Technology Corporation (MCC) not be 
 *  used in advertising or publicity pertaining to distribution of
 *  the software without specific, written prior permission.  The
 *  University of Texas and MCC makes no representations about the 
 *  suitability of this software for any purpose.  It is provided "as is" 
 *  without express or implied warranty.
 *
 *  THE UNIVERSITY OF TEXAS AND MCC DISCLAIMS ALL WARRANTIES WITH REGARD TO
 *  THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 *  FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TEXAS OR MCC BE LIABLE FOR
 *  ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
 *  RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
 *  CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 *  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *  Author:  	Po Cheung
 *  Created:   	March 10, 1989
 *
 *****************************************************************************
 * 
 *  xxgdb - X Window System interface to the gdb debugger
 *  
 * 	Copyright 1990 Thomson Consumer Electronics, Inc.
 *  
 *  Permission to use, copy, modify, and distribute this software and its
 *  documentation for any purpose and without fee is hereby granted,
 *  provided that the above copyright notice appear in all copies and that
 *  both that copyright notice and this permission notice appear in
 *  supporting documentation, and that the name of Thomson Consumer
 *  Electronics (TCE) not be used in advertising or publicity pertaining
 *  to distribution of the software without specific, written prior
 *  permission.  TCE makes no representations about the suitability of
 *  this software for any purpose.  It is provided "as is" without express
 *  or implied warranty.
 *
 *  TCE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 *  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT
 *  SHALL TCE BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES
 *  OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 *  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 *  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 *  SOFTWARE.
 *
 *  Adaptation to GDB:  Pierre Willard
 *  XXGDB Created:   	December, 1990
 *
 *****************************************************************************/

/*  calldbx.c
 *
 *    Set up communication between dbx and xdbx using pseudo terminal, and
 *    call dbx.
 *
 *    open_master():	Open the master side of pty.
 *    open_slave(): 	Open the slave side of pty.
 *    calldbx(): 	Invoke dbx.
 */

/*
 * (JBL)10MAY91 : not only OLDSUNOS but generic BSD have sgttyb
 */
#if defined(OLDSUNOS) || defined(BSD)
#include	<sys/ioctl.h>
#else
#include	<termio.h>
#endif

#ifdef I386BSD
#include      <sys/ioctl_compat.h>
#endif

#include	"global.h"

#ifdef SYSV 
#ifdef SVR4				/* (MJH) Need to use STREAMS pseudo-ttys */
#define MASTER_CLONE "/dev/ptmx"
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/stropts.h>
#include <fcntl.h>
#include <stdio.h>
#include <signal.h>
#else
#ifdef sco
#  include	<sys/fcntl.h>
#endif /* sco */
#endif /* SVR4 */
#endif /* SYSV */

extern char *progname;		/* (MJH) */

FILE   	    	*dbxfp = NULL;		/* file pointer to dbx */
int    	    	dbxpid = 0;		/* dbx process id */

#ifdef SYSV
char            dbxfbuf[BUFSIZ];        
#endif

static int	dbxInputId;		/* dbx input id */
#ifndef SVR4				/* (MJH) */
static char 	pty[11] = "/dev/pty??";	/* master side of pseudo-terminal */
static char 	tty[11] = "/dev/tty??";	/* slave side of pseudo-terminal */
#endif /* SVR4 */
extern char	*dbxprompt;

/*
 *  Xdbx talks to dbx through a pseudo terminal which is a pair of master
 *  and slave devices: /dev/pty?? and /dev/tty??, where ?? goes from p0 to
 *  sf (system dependent).  The pty is opened for both read and write.
 */
static int open_master()
{
    int master;
    
#ifdef SVR4				/* (MJH) Use STREAMS */

    if((master = open(MASTER_CLONE, O_RDWR)) < 0)
	perror(MASTER_CLONE);
    else
	return master;
#else
    int  i;
    char c;

#ifndef sco
	for (c='p'; c<'t'; c++) {
	for (i=0; i<16; i++) {
#else
	c = 'p';
	for (i=0; i<8; i++) {
#endif
	    pty[8] = c;
	    pty[9] = "0123456789abcdef"[i];
	    if ((master = open(pty, O_RDWR)) >= 0) 
		return (master); 
	}
#ifndef sco
	}
#endif
#endif /* SVR4 */

#ifdef GDB
    fprintf(stderr, "xxgdb: all ptys in use\n");
#else
    fprintf(stderr, "xdbx: all ptys in use\n");
#endif
    exit(1);
}

/*ARGSUSED*/
static int open_slave(master)
    int master;
{
    int slave;

#ifdef SVR4				/* (MJH) */
    char *slave_name;
    extern char *ptsname(int master);
    void (*handler)();

    if((handler = signal(SIGCHLD, SIG_DFL) != SIG_ERR) &&
       (grantpt(master) == 0) &&
       (signal(SIGCHLD, handler) == SIG_DFL) &&
       (unlockpt(master) == 0) &&
       ((slave_name = ptsname(master)) != NULL) &&
       ((slave = open(slave_name, O_RDWR)) >= 0) &&
       (ioctl(slave, I_PUSH, "ptem") >= 0) &&
       (ioctl(slave, I_PUSH, "ldterm") >= 0))
	return slave;
    perror("Pseudo-tty slave");
    fprintf(stderr, "open: cannot open slave pty %s", slave_name);
    exit(1);
#else
    tty[8] = pty[8];
    tty[9] = pty[9];
    if ((slave = open(tty, O_RDWR)) < 0)
	    {
		perror(tty);
		exit(1);
	    }
    return slave;
#endif /* SVR4 */
}

/* ARGSUSED */
void calldbx(argc, argv)
int argc;
char *argv[];
{
/*
 * (JBL)10MAY91 : use sgttyb if generic BSD
 */
#if !(defined(OLDSUNOS) || defined(BSD))
    struct termio Termio;
#else
    struct sgttyb Termio;
#endif
    int  	  master;		/* file descriptor of master pty */
    int  	  slave; 		/* file descriptor of slave pty */
    int		  fd; 			/* file descriptor of controlling tty */
    int		  pid;			/* process id */
    int		  pgrp;			/* process group id */
    char 	  *debugger; 		/* name of executable debugger */
    char	  errmsg[LINESIZ];

#ifdef GDB	/* for GDB, we use XXGDB_DEBUGGER instead */
    debugger = (char *) getenv("XXGDB_DEBUGGER");	/* first looks up env var */
#else
    debugger = (char *) getenv("DEBUGGER");	/* first looks up env var */
#endif

/* CRL mod 4 3/15/91 GWC if no env var then try app res for db_name */
    if (debugger == NULL &&
	app_resources.db_name &&
	strcmp(app_resources.db_name, "") != NULL)
	debugger =  XtNewString(app_resources.db_name);
      
    if (debugger == NULL)
	debugger  = XtNewString(DEBUGGER);

/* CRL mod 4 3/15/91 GWC -  allow the user to specify a db_prompt */
    if (app_resources.db_prompt &&
	strcmp(app_resources.db_prompt, "") != NULL)
	dbxprompt = XtNewString(app_resources.db_prompt);
  
    /* construct dbx prompt string based on the name of debugger invoked */
    if (dbxprompt == NULL) {
	dbxprompt = XtMalloc((4+strlen(debugger)) * sizeof(char));
	sprintf(dbxprompt, "(%s) ", debugger);
    }
    
	if (debug)
		fprintf(stderr,"debugger=\"%s\"\nprompt=\"%s\"\n",debugger,dbxprompt);
  
    /*
     * Clear controlling tty.  Do this now, so that open_slave and
     * open_master will cause the selected pty to become the
     * controlling tty.
     */

#if defined(SVR4)	/* (MJH) */
    if ((tcgetsid(0) != tcgetpgrp(0)) && /* Check if fore- or back-ground  */
	(fd = open("/dev/tty", O_RDWR|O_NDELAY)) > 0) {
  	ioctl(fd, TIOCNOTTY, 0);
  	close(fd);
  	}
#else	/* not SVR4 */
	if ((fd = open("/dev/tty", O_RDWR)) > 0) {
#ifndef SYSV 
  	ioctl(fd, TIOCNOTTY, 0);
#endif /* SYSV */
  	close(fd);
  	}
#endif	/* SVR4 */

    master = open_master();
    
#if defined(SVR4) || !defined(SYSV)
    slave = open_slave(master);
#endif

    dbxpid = fork();
    if (dbxpid == -1) {
	sprintf(errmsg, "%s error: Cannot fork %s\n", progname, debugger);	/* (MJH) */
	perror(errmsg);
	exit(1);
    }
    else if (dbxpid) { 
	/* 
	 * Parent : close the slave side of pty
	 *	    close stdin and stdout
	 *	    set the dbx file descriptor to nonblocking mode
	 *	    open file pointer with read/write access to dbx
	 *	    set line buffered mode
	 *	    register dbx input with X
	 */

#if defined(SVR4) || !defined(SYSV)		/* (MJH) */
	close(slave);
#endif /* SYSV */

	close(0);
	close(1);
	fcntl(master, F_SETFL, FNDELAY);
	
	if((dbxfp = fdopen(master, "r+")) == NULL)	/* (MJH) */
	{
	    perror("Associating stdio stream with pty master");
	    exit(1);
	}
    
    /* (PW)10APR91 : I use 'setvbuf' instead of 'setlinebuf',
    because I had a problem with 'define' commands in source
    of gdbinit files. Also for SYSV setvbuf is required. */
 /*	setlinebuf(dbxfp); */
 
   /* (JBL)10MAY91 : to get the thing to work on my machine
    * i needed to set unbuffered mode via setbuf command
    * this should work on generic BSD platforms
    */
   
#ifdef SYSV
	setvbuf(dbxfp, dbxfbuf, _IONBF, BUFSIZ);
#else
#ifdef BSD
	setbuf(dbxfp, NULL);
#else
	setvbuf(dbxfp, NULL, _IONBF, 0);
#endif
#endif

	dbxInputId = XtAppAddInput(app_context, master, XtInputReadMask, 
				   read_dbx, NULL);
    }
    else { 
	/* 
	 * Child : close master side of pty
	 * 	   redirect stdin, stdout, stderr of dbx to pty
	 *	   unbuffer output data from dbx
	 *	   exec dbx with arguments
	 */
	char	      *s;
      
#if defined(SYSV) && !defined(SVR4)		/* (MJH) */
        setpgrp();
        slave = open_slave(master);
#endif
	close(master);

	/*
	 * Modify local and output mode of slave pty
	 */
	 
	/*
	 * (JBL)10MAY91 : use sgttyb if OLDSUN or generic BSD
	 */ 
#if !(defined(OLDSUNOS) || defined(BSD))
	ioctl(slave, TCGETA, &Termio);
	Termio.c_lflag &= ~ECHO;	/* No echo */
	Termio.c_oflag &= ~ONLCR;	/* Do not map NL to CR-NL on output */
	ioctl(slave, TCSETA, &Termio);
#else
	ioctl(slave, TIOCGETP, &Termio);
	Termio.sg_flags &= ~ECHO;	/* No echo */
	Termio.sg_flags &= ~CRMOD;	/* Do not map NL to CR-NL on output */
	ioctl(slave, TIOCSETP, &Termio);
#endif

	dup2(slave, 0);
	dup2(slave, 1);
	dup2(slave, 2);
	if (slave > 2)
	    close(slave);
	    
	fcntl(1, F_SETFL, FAPPEND);
	setbuf(stdout, NULL);

	/*
	 * Set our process group to that of the terminal,
	 * so we can change the group of the terminal.
	 */
#ifndef SYSV
	ioctl(0, TIOCGPGRP, &pgrp);
	setpgrp(0, pgrp);

	/*
	 * Now set the process group of the terminal and of us
	 * to our process id.  This clears us from the control
	 * of the other process group.
	 */
	pid = getpid();
	ioctl(0, TIOCSPGRP, &pid);
	setpgrp(0, pid);
#endif /* not SYSV */

#ifdef SVR4						/* (MJH) */
	tcsetpgrp(0, tcgetpgrp(0));
	tcsetpgrp(0, getpid());
#endif /* SVR4 */

	argv[0] = debugger;

	if ((s = getenv("TERMCAP")) != NULL && *s != '/')
		unsetenv("TERMCAP");
	putenv("TERM=dumb");

	execvp(debugger, argv);
	sprintf(errmsg, "%s error: cannot exec %s", progname, debugger);
	perror(errmsg);
	exit(1);
    }
}
