/*
 * Copyright 1993 Network Computing Devices, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this 
 * software without specific, written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)common.c,v 1.5 1994/02/12 03:42:40 greg Exp $
 */

/* ************************************************************ *\
 *								*
 *    Common support routines for sockets			*
 *								*
 *       James L. Peterson	 				*
 *	(c) Copyright MCC, 1987                                 *
 * 				  				*
 * 				  				*
 \* *********************************************************** */

#include "scope.h"
#include <audio/Aos.h>

/* ********************************************** */
/*						  */
/*       Debugging support routines               */
/*						  */
/* ********************************************** */

enterprocedure(s)
    char       *s;
{
    debug(2, (stderr, "-> %s\n", s));
}

warn(s)
    char       *s;
{
    fprintf(stderr, "####### %s\n", s);
}

panic(s)
    char       *s;
{
    fprintf(stderr, "%s\n", s);
    exit(1);
}

/* ************************************************************ */
/*								*/
/*    Signal Handling support					*/
/*								*/
/* ************************************************************ */

#include <signal.h>

SignalUSR1()
{
    debug(1, (stderr, "==> SIGUSR1 received\n"));
    if (silent)
	silent = 0;
    else
	silent = 1;
}

SignalURG()
{
    debug(1, (stderr, "==> SIGURG received\n"));
}

SignalPIPE()
{
    debug(1, (stderr, "==> SIGPIPE received\n"));
}

SignalINT()
{
    debug(1, (stderr, "==> SIGINT received\n"));
    exit(1);
}

SignalQUIT()
{
    debug(1, (stderr, "==> SIGQUIT received\n"));
    exit(1);
}

SignalTERM()
{
    debug(1, (stderr, "==> SIGTERM received\n"));
    exit(1);
}

SignalTSTP()
{
    debug(1, (stderr, "==> SIGTSTP received\n"));
}

SignalCONT()
{
    debug(1, (stderr, "==> SIGCONT received\n"));
}

SetSignalHandling()
{
    enterprocedure("SetSignalHandling");
#if defined(SIGURG)
    (void) signal(SIGURG, SignalURG);
#endif /* defined(SIGURG) */
    (void) signal(SIGPIPE, SignalPIPE);
    (void) signal(SIGINT, SignalINT);
    (void) signal(SIGQUIT, SignalQUIT);
    (void) signal(SIGTERM, SignalTERM);
    (void) signal(SIGTSTP, SignalTSTP);
    (void) signal(SIGCONT, SignalCONT);
    (void) signal(SIGUSR1, SignalUSR1);
}



/* ************************************************************ */
/*								*/
/*   Create a socket for a service to listen for clients        */
/*								*/
/* ************************************************************ */

#include <sys/types.h>		/* needed by sys/socket.h and netinet/in.h */
#include <sys/uio.h>		/* for struct iovec, used by socket.h */
#include <sys/socket.h>		/* for AF_INET, SOCK_STREAM, ... */
#include <sys/ioctl.h>		/* for FIONCLEX, FIONBIO, ... */
#include <netinet/in.h>		/* struct sockaddr_in */
#include <netdb.h>		/* struct servent * and struct hostent *  */

#define	BACKLOG	5

/* for use in the UsingFD call -- defined later */
extern int  NewConnection();


SetUpConnectionSocket(port)
    int         port;
{
    FD          ConnectionSocket;
    struct sockaddr_in sin;

#ifndef	SO_DONTLINGER
    struct linger linger;

#endif	/* SO_DONTLINGER */

    enterprocedure("SetUpConnectionSocket");

    /* create the connection socket and set its parameters of use */
    ConnectionSocket = socket(AF_INET, SOCK_STREAM, 0);
    if (ConnectionSocket < 0) {
	perror("socket");
	exit(-1);
    }
    (void) setsockopt(ConnectionSocket, SOL_SOCKET, SO_REUSEADDR, (char *) NULL, 0);
    (void) setsockopt(ConnectionSocket, SOL_SOCKET, SO_USELOOPBACK, (char *) NULL, 0);

#ifdef	SO_DONTLINGER
    (void) setsockopt(ConnectionSocket, SOL_SOCKET, SO_DONTLINGER, (char *) NULL, 0);
#else	/* SO_DONTLINGER */
    linger.l_onoff = 0;
    linger.l_linger = 0;
    (void) setsockopt(ConnectionSocket, SOL_SOCKET, SO_LINGER, (char *) &linger, sizeof linger);
#endif	/* SO_DONTLINGER */

    /* define the name and port to be used with the connection socket */
    bzero((char *) &sin, sizeof(sin));
    sin.sin_family = AF_INET;

    /*
     * the address of the socket is composed of two parts: the host machine
     * and the port number.  We need the host machine address for the current
     * host
     */
    {
	/* define the host part of the address */
	char        MyHostName[256];
	struct hostent *hp;

	(void) gethostname(MyHostName, sizeof(MyHostName));
	ScopeHost = (char *) malloc((AuInt32) strlen(MyHostName));
	(void) strcpy(ScopeHost, MyHostName);
	hp = gethostbyname(MyHostName);
	if (hp == NULL)
	    panic("No address for our host");
	bcopy((char *) hp->h_addr, (char *) &sin.sin_addr, hp->h_length);
    }
    /*
     * new code -- INADDR_ANY should be better than using the name of the host
     * machine.  The host machine may have several different network
     * addresses.  INADDR_ANY should work with all of them at once.
     */
    sin.sin_addr.s_addr = INADDR_ANY;

    sin.sin_port = htons(port);
    ScopePort = port;

    /* bind the name and port number to the connection socket */
    if (bind(ConnectionSocket, (struct sockaddr *) & sin, sizeof(sin)) < 0) {
	perror("bind");
	exit(-1);
    }
    debug(4, (stderr, "Socket is FD %d for %s,%d\n",
	      ConnectionSocket, ScopeHost, ScopePort));

    /* now activate the named connection socket to get messages */
    if (listen(ConnectionSocket, BACKLOG) < 0) {
	perror("listen");
	exit(-1);
    };

    /* a few more parameter settings */
    set_fd_close_on_exec (ConnectionSocket);
    set_fd_nonblocking (ConnectionSocket);

    debug(4, (stderr, "Listening on FD %d\n", ConnectionSocket));
    UsingFD(ConnectionSocket, NewConnection);
}

#ifdef DNET
#include <netdnet/dn.h>
#include <netdnet/dnetdb.h>

SetUpDnetConnectionSocket(displaynum)
    int         displaynum;
{
    FD          ConnectionSocket;
    struct sockaddr_dn sdn;

    enterprocedure("SetUpDnetConnectionSocket");

    /* create the connection socket and set its parameters of use */
    ConnectionSocket = socket(AF_DECnet, SOCK_STREAM, 0);
    if (ConnectionSocket < 0) {
	perror("socket");
	exit(-1);
    }
    (void) setsockopt(ConnectionSocket, SOL_SOCKET, SO_REUSEADDR, (char *) NULL, 0);
    (void) setsockopt(ConnectionSocket, SOL_SOCKET, SO_USELOOPBACK, (char *) NULL, 0);

    {
	/* define the host part of the address */
	char       *MyHostName;
	struct dn_naddr *myaddr;
	extern char *dnet_htoa();

	myaddr = getnodeadd();
	if (myaddr == NULL)
	    panic("No address for our host");
	MyHostName = dnet_htoa(myaddr);
	ScopeHost = (char *) malloc((AuInt32) strlen(MyHostName));
	(void) strcpy(ScopeHost, MyHostName);
    }

    /* define the name and port to be used with the connection socket */
    bzero((char *) &sdn, sizeof(sdn));
    sdn.sdn_family = AF_DECnet;
    sprintf(sdn.sdn_objname, "X$X%d", displaynum);
    sdn.sdn_objnamel = strlen(sdn.sdn_objname);

    /* bind the name and port number to the connection socket */
    if (bind(ConnectionSocket, (struct sockaddr *) & sdn, sizeof(sdn)) < 0) {
	perror("bind");
	exit(-1);
    }
    debug(4, (stderr, "DNET Socket is FD %d\n", ConnectionSocket));

    /* now activate the named connection socket to get messages */
    if (listen(ConnectionSocket, BACKLOG) < 0) {
	perror("listen");
	exit(-1);
    };

    /* a few more parameter settings */
    set_fd_close_on_exec (ConnectionSocket);
    set_fd_nonblocking (ConnectionSocket);

    debug(4, (stderr, "Listening on FD %d\n", ConnectionSocket));
    UsingFD(ConnectionSocket, NewConnection);
}

#endif	/* DNET */


void set_fd_close_on_exec (fd)
    int fd;
{
    /* a few more parameter settings */
#ifdef FD_CLOEXEC
    (void) fcntl (fd, F_SETFD, FD_CLOEXEC);
#else
    (void) fcntl (fd, F_SETFD, 1);
#endif
}


void set_fd_nonblocking (fd)
    int fd;
{
    /*
     * Set the connection non-blocking since we use select() to block.
     */
    /* ultrix reads hang on Unix sockets, hpux reads fail */
#if defined(O_NONBLOCK) && (!defined(ultrix) && !defined(hpux) && !defined(AIXV3) && !defined(uniosu))
    (void) fcntl (fd, F_SETFL, O_NONBLOCK);
#else
#ifdef FIOSNBIO
    int ON = 1;
    (void) ioctl (fd, FIOSNBIO, &ON);
#else
#if (defined(AIXV3) || defined(uniosu)) && defined(FIONBIO)
    int ON = 1;
    ioctl(fd, FIONBIO, &ON);
#else
    (void) fcntl (fd, F_SETFL, FNDELAY);
#endif
#endif
#endif
}
