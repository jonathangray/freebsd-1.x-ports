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
 * $NCDId: @(#)scope.c,v 1.6 1994/02/12 03:42:13 greg Exp $
 */

/* ******************************************************
 *
 * A spy program o reveal Audio traffic
 * based on
 *						      	*
 *  A spy program to reveal X11  traffic	    	*
 *						      	*
 *	James Peterson, 1988	       			*
 *	(c) Copyright MCC, 1988 			*
 *							*
 ***************************************************** */

#include "scope.h"


/* ********************************************** */
/*                                                */
/* ********************************************** */

#define DefaultPort AU_DEFAULT_TCP_PORT

char        ServerHostName[255];
AuInt32        ServerBasePort = DefaultPort;
AuInt32        ServerInPort = 1;
AuInt32        ServerOutPort = 0;
AuInt32        ServerDisplay = 0;

#ifdef DNET
char        UseDnetInPort = 0,
            UseDnetServer = 0;

#endif				/* DNET */


/* ********************************************** */
/*                                                */
/*                                                */
/* ********************************************** */

short
GetServerport()
{
    short       port;

    enterprocedure("GetServerport");

    port = ServerBasePort + ServerOutPort + ServerDisplay;
    debug(4, (stderr, "Server service is on port %d\n", port));
    return (port);
}

short
GetScopePort()
{
    short       port;

    enterprocedure("GetScopePort");

    port = ServerBasePort + ServerInPort + ServerDisplay;
    debug(4, (stderr, "scope service is on port %d\n", port));
    return (port);
}

/* ********************************************** */
/*                                                */
/* ********************************************** */

Usage()
{
    fprintf(stderr, "Usage: auscope\n");

#ifdef DNET
    fprintf(stderr, "              [-tc <inet | dnet>]  -- client transport\n");
    fprintf(stderr, "              [-ts <inet | dnet>]  -- server transport\n");
#endif				/* DNET */

    fprintf(stderr, "              [-h<server-host>]\n");
    fprintf(stderr, "              [-i<in-port>]\n");
    fprintf(stderr, "              [-o<out-port>]\n");
    fprintf(stderr, "              [-d<display-number>]\n");
    fprintf(stderr, "              [-v<n>]  -- verbose output\n");
    fprintf(stderr, "              [-q]  -- quiet output\n");
    fprintf(stderr, "              [-s]  -- start with quiet output\n");
    fprintf(stderr, "              [-D<debug-level>]\n");
    exit(1);
}


char       *
OfficialName() /* forward type declaration */ ;

ScanArgs(argc, argv)
    int         argc;
    char      **argv;
{
    Verbose = 1 /* default verbose-ness level */ ;
    silent = 0;			/* defaults to off */

    /* Scan argument list */
    while (--argc > 0) {
	++argv;
	if (**argv == '-')
	    switch (*++*argv) {
		/*
		 * debug levels: 2 - trace each procedure entry 4 - I/O,
		 * connections 8 - Scope internals 16 - Message protocol 32 -
		 * 64 - malloc 128 - 256 - really low level
		 */
	    case 'D':
		debuglevel = atoi(++*argv);
		if (debuglevel == 0)
		    debuglevel = 255;
		debuglevel |= 1;
		Verbose = 7;
		debug(1, (stderr, "debuglevel = %d\n", debuglevel));
		break;
	    case 's':		/* start silently */
		silent = 1;
		debug(1, (stderr, "silent = %d\n", silent));
		break;
	    case 'q':		/* quiet mode */
		Verbose = 0;
		debug(1, (stderr, "Verbose = %d\n", Verbose));
		break;

	    case 'v':		/* verbose mode */
		Verbose = atoi(++*argv);
		debug(1, (stderr, "Verbose = %d\n", Verbose));
		break;

	    case 'o':
		ServerOutPort = atoi(++*argv);
		if (ServerOutPort <= 0)
		    ServerOutPort = 0;
		debug(1, (stderr, "ServerOutPort = %d\n", ServerOutPort));
		break;

	    case 'd':
		ServerDisplay = atoi(++*argv);
		if (ServerDisplay <= 0)
		    ServerDisplay = 0;
		debug(1, (stderr, "ServerDisplay=%d\n", ServerDisplay));
		break;

	    case 'i':
		ServerInPort = atoi(++*argv);
		if (ServerInPort <= 0)
		    ServerInPort = 0;
		debug(1, (stderr, "ServerInPort = %d\n", ServerInPort));
		break;

	    case 'h':
		if (++*argv != NULL && **argv != '\0')
		    strcpy(ServerHostName, OfficialName(*argv));
		debug(1, (stderr, "ServerHostName=%s\n", ServerHostName));
		break;

#ifdef DNET
	    case 't':{
		    register char *which = NULL;

		    switch (*++*argv) {
		    case 'c':
			which = &UseDnetInPort;
			break;
		    case 's':
			which = &UseDnetServer;
			break;
		    }
		    if (which == NULL || argc < 1) {
			if (which == NULL)
			    fprintf(stderr, "Unknown option %c\n", **argv);
			else
			    fprintf(stderr, "Missing argument\n");
			Usage();
			break;
		    }
		    --argc, argv++;
		    if (!strcmp(*argv, "dnet"))
			*which = 1;
		    else if (!strcmp(*argv, "inet"))
			*which = 0;
		    debug(1, (stderr, "UseDnetInPort=%d UseDnetServer=%d\n",
			      UseDnetInPort, UseDnetServer));
		}
		break;
#endif				/* DNET */

	    default:
		fprintf(stderr, "Unknown option %c\n", **argv);
		Usage();
		break;

	    }
	else {
	    /* file argument to scope -- error */
	    Usage();
	}
    }

    /* check for different port numbers or different machines */
    if (ServerInPort == ServerOutPort)
	if (ServerHostName[0] == '\0') {
	    fprintf(stderr, "Can't have auscope on same port as server (%d)\n",
		    ServerInPort);
	    Usage();
	}
}


/* ********************************************** */
/*                                                */
/* ********************************************** */

main(argc, argv)
    int         argc;
    char      **argv;
{
    ScanArgs(argc, argv);
    InitializeFD();
    InitializeAudio();
    SetUpStdin();

#ifdef DNET
    if (UseDnetInPort)
	SetUpDnetConnectionSocket(ServerInPort + ServerDisplay);
    else
#endif				/* DNET */

	SetUpConnectionSocket(GetScopePort());
    SetSignalHandling();

    MainLoop();
}

TimerExpired()
{
    debug(16, (stderr, "Timer tick\n"));
}

/* ********************************************** */
/*                                                */
/* ********************************************** */

/*
  here is where we would add code to allow control from
  the keyboard.  We would want to read a command and
  interpret it.  Possibilties:

  (a) verbose level setting
  (b) reset time
  (c) save Audio requests to a file.
  (d) replay Audio requests from a file.
  (e) allow fake events, errors to be generated.
*/

ReadStdin(fd)
    FD          fd;
{
    char        buf[2048];
    AuInt32        n;

    enterprocedure("ReadStdin");
    n = read(fd, buf, 2048);
    debug(4, (stderr, "read %d bytes from stdin\n", n));
}

SetUpStdin()
{
    enterprocedure("SetUpStdin");
    UsingFD(fileno(stdin), ReadStdin);
}

/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

/*
  auscope is really meant to look at one client at a time.  However,
  it can easily handle multiple clients and servers.  To do so,
  we need to have a pair of FDs: one for the client and one for the
  server for that client.  If either goes away, so does the other.
  We need to be able to identify the other FD of a pair, so that if
  we get input from one, we can write it to the other.
*/

struct fdinfo {
    Boolean     Server;
    AuInt32        ClientNumber;
    FD          pair;
};

static AuInt32 ClientNumber = 0;
struct fdinfo FDinfo[StaticMaxFD];

SetUpPair(client, server)
    FD          client;
    FD          server;
{
    if (client >= 0) {
	ClientNumber += 1;
	FDinfo[client].Server = false;
	FDinfo[client].pair = server;
	FDinfo[client].ClientNumber = ClientNumber;
	if (server >= 0) {
	    FDinfo[server].Server = true;
	    FDinfo[server].pair = client;
	    FDinfo[server].ClientNumber = FDinfo[client].ClientNumber;
	}
    } else if (server >= 0) {
	close(server);
	NotUsingFD(server);
    }
}


CloseConnection(fd)
    FD          fd;
{
    debug(4, (stderr, "close %d and %d\n", fd, FDPair(fd)));
    StopClientConnection(ServerHalf(fd));
    StopServerConnection(ClientHalf(fd));

    close(fd);
    NotUsingFD(fd);
    close(FDPair(fd));
    NotUsingFD(FDPair(fd));
}

/* ************************************************************ */

FD
FDPair(fd)
    FD          fd;
{
    return (FDinfo[fd].pair);
}

FD
ClientHalf(fd)
    FD          fd;
{
    if (FDinfo[fd].Server)
	return (FDinfo[fd].pair);
    return (fd);
}

FD
ServerHalf(fd)
    FD          fd;
{
    if (FDinfo[fd].Server)
	return (fd);
    return (FDinfo[fd].pair);
}

char       *
ClientName(fd)
    FD          fd;
{
    static char name[12];

    if (ClientNumber <= 1)
	return ("");
    sprintf(name, " %d", FDinfo[fd].ClientNumber);
    return (name);
}


/* ********************************************** */
/*                                                */
/* ********************************************** */

/* when we get data from a client, we read it in, copy it to the
   server for this client, then dump it to the client. Note, we don't
   have to have a server, if there isn't one. */

DataFromClient(fd)
    FD          fd;
{
    unsigned char buf[2048];
    AuInt32        n;
    FD          ServerFD;

    enterprocedure("DataFromClient");
    n = read(fd, (char *) buf, 2048);
    debug(4, (stderr, "read %d bytes from Client%s\n", n, ClientName(fd)));
    if (n < 0) {
	PrintTime();
	perror("Client --> read error:");
	CloseConnection(fd);
	return;
    }
    if (n == 0) {
	PrintTime();
	fprintf(stdout, "Client%s --> EOF\n", ClientName(fd));
	CloseConnection(fd);
	return;
    }
    ServerFD = FDPair(fd);
    if (ServerFD < 0) {

#ifdef DNET
	if (UseDnetServer)
	    ServerFD = ConnectToDnetServer(false);
	else
#endif				/* DNET */

	    ServerFD = ConnectToServer(false);
	SetUpPair(fd, ServerFD);
    }
    /* write bytes from client to server, allow for server to fail */
    if (ServerFD >= 0) {
	AuInt32        BytesToWrite = n;
	unsigned char *p = buf;

	while (BytesToWrite > 0) {
	    int         n1 = write(ServerFD, (char *) p, (int) BytesToWrite);

	    debug(4, (stderr, "write %d bytes to Server%s\n", n1, ClientName(fd)));
	    if (n1 > 0) {
		BytesToWrite -= n1;
		p += n1;
	    } else {
		perror("Error on write to Server");
		CloseConnection(fd);
		BytesToWrite = 0;
	    }
	}
    }
    /* also report the bytes to standard out */
    ReportFromClient(fd, buf, n);
}

/* ********************************************** */
/*                                                */
/* ********************************************** */

/* similar situation for the server, but note that if there is no client,
   we close the connection down -- don't need a server with no client. */

DataFromServer(fd)
    FD          fd;
{
    unsigned char buf[2048];
    AuInt32        n;
    FD          ClientFD;

    enterprocedure("DataFromServer");
    n = read(fd, (char *) buf, 2048);
    debug(4, (stderr, "read %d bytes from Server%s\n", n, ClientName(fd)));
    if (n < 0) {
	PrintTime();
	perror("read error <- Server");
	CloseConnection(fd);
	return;
    }
    if (n == 0) {
	PrintTime();
	fprintf(stdout, "EOF <-- Server%s\n", ClientName(fd));
	CloseConnection(fd);
	return;
    }
    ClientFD = FDPair(fd);
    if (ClientFD < 0) {
	CloseConnection(fd);
	return;
    }
    /* write bytes from server to client, allow for client to fail */
    {
	AuInt32        BytesToWrite = n;
	unsigned char *p = buf;

	while (BytesToWrite > 0) {
	    int         n1 = write(ClientFD, (char *) p, (int) BytesToWrite);

	    debug(4, (stderr, "write %d bytes to Client%s\n", n1, ClientName(fd)));
	    if (n1 > 0) {
		BytesToWrite -= n1;
		p += n1;
	    } else {
		perror("Error on write to Client");
		CloseConnection(fd);
		BytesToWrite = 0;
	    }
	}
    }

    /* also report the bytes to standard out */
    ReportFromServer(fd, buf, n);
}



/* ************************************************************ */
/*								*/
/*     Create New Connection to a client program and to Server  */
/*								*/
/* ************************************************************ */

#include <sys/types.h>		/* needed by sys/socket.h and netinet/in.h */
#include <sys/uio.h>		/* for struct iovec, used by socket.h */
#include <sys/socket.h>		/* for AF_INET, SOCK_STREAM, ... */
#include <sys/ioctl.h>		/* for FIONCLEX, FIONBIO, ... */
#include <netinet/in.h>		/* struct sockaddr_in */
#include <netdb.h>		/* struct servent * and struct hostent * */
#include <errno.h>		/* for EINTR, EADDRINUSE, ... */

#if defined(SYSV) && defined(SYSV386)
#include <net/errno.h>
#endif /* defined(SYSV) && defined(SYSV386) */

extern int  errno;

NewConnection(fd)
    FD          fd;
{
    FD          ServerFD = -1;
    FD          ClientFD = -1;

    ClientFD = ConnectToClient(fd);

#ifdef DNET
    if (UseDnetServer)
	ServerFD = ConnectToDnetServer(true);
    else
#endif				/* DNET */

	ServerFD = ConnectToServer(true);
    SetUpPair(ClientFD, ServerFD);
}


/* ************************************************************ */

FD
ConnectToClient(ConnectionSocket)
    FD          ConnectionSocket;
{
    FD          ClientFD;
    struct sockaddr_in from;
    int         len = sizeof(from);

    enterprocedure("ConnectToClient");

    ClientFD = accept(ConnectionSocket, (struct sockaddr *) & from, &len);
    debug(4, (stderr, "Connect To Client: FD %d\n", ClientFD));
    if (ClientFD < 0 && errno == EWOULDBLOCK) {
	debug(4, (stderr, "Almost blocked accepting FD %d\n", ClientFD));
	panic("Can't connect to Client");
    }
    if (ClientFD < 0) {
	debug(4, (stderr, "NewConnection: error %d\n", errno));
	panic("Can't connect to Client");
    }
    UsingFD(ClientFD, DataFromClient);
    set_fd_close_on_exec (ClientFD);
    set_fd_nonblocking (ClientFD);
    StartClientConnection(ClientFD);
    return (ClientFD);
}



/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */



FD
ConnectToServer(report)
    Boolean     report;
{
    FD          ServerFD;
    struct sockaddr_in sin;
    struct hostent *hp;

#ifndef	SO_DONTLINGER
    struct linger linger;

#endif				/* SO_DONTLINGER */

    enterprocedure("ConnectToServer");

    /* establish a socket to the name server for this host */
    bzero((char *) &sin, sizeof(sin));
    ServerFD = socket(AF_INET, SOCK_STREAM, 0);
    if (ServerFD < 0) {
	perror("socket() to Server failed");
	debug(1, (stderr, "socket failed\n"));
	panic("Can't open connection to Server");
    }
    (void) setsockopt(ServerFD, SOL_SOCKET, SO_REUSEADDR, (char *) NULL, 0);
    (void) setsockopt(ServerFD, SOL_SOCKET, SO_USELOOPBACK, (char *) NULL, 0);

#ifdef	SO_DONTLINGER
    (void) setsockopt(ServerFD, SOL_SOCKET, SO_DONTLINGER, (char *) NULL, 0);
#else				/* SO_DONTLINGER */
    linger.l_onoff = 0;
    linger.l_linger = 0;
    (void) setsockopt(ServerFD, SOL_SOCKET, SO_LINGER, (char *) &linger, sizeof linger);
#endif				/* SO_DONTLINGER */

    /* determine the host machine for this process */
    if (ServerHostName[0] == '\0')
	(void) gethostname(ServerHostName, sizeof(ServerHostName));
    debug(4, (stderr, "try to connect on %s\n", ServerHostName));

    hp = gethostbyname(ServerHostName);
    if (hp == 0) {
	perror("gethostbyname failed");
	debug(1, (stderr, "gethostbyname failed for %s\n", ServerHostName));
	panic("Can't open connection to Server");
    }
    sin.sin_family = AF_INET;
    bcopy((char *) hp->h_addr, (char *) &sin.sin_addr, hp->h_length);
    sin.sin_port = htons(GetServerport());

    if ((sin.sin_port == ScopePort)
	    && strcmp(ServerHostName, ScopeHost) == 0) {
	char        error_message[100];

	sprintf(error_message, "Trying to attach to myself: %s,%d\n",
		ServerHostName, sin.sin_port);
	panic(error_message);
    }
    /* ******************************************************** */
    /* try to connect to Server */

    if (connect(ServerFD, (struct sockaddr *) & sin, sizeof(sin)) < 0) {
	debug(4, (stderr, "connect returns errno of %d\n", errno));
	if (errno != 0)
	    if (report)
		perror("connect");
	switch (errno) {
	case ECONNREFUSED:
	    /*
	     * experience says this is because there is no Server to connect
	     * to
	     */
	    close(ServerFD);
	    debug(1, (stderr, "No Server\n"));
	    if (report)
		warn("Can't open connection to Server");
	    return (-1);

	default:
	    close(ServerFD);
	    panic("Can't open connection to Server");
	}
    }
    debug(4, (stderr, "Connect To Server: FD %d\n", ServerFD));
    if (ServerFD >= 0) {
	UsingFD(ServerFD, DataFromServer);
	StartServerConnection(ServerFD);
    }
    return (ServerFD);
}

#ifdef DNET
#include <netdnet/dn.h>
#include <netdnet/dnetdb.h>

FD
ConnectToDnetServer(report)
    Boolean     report;
{
    FD          ServerFD;
    struct sockaddr_dn sdn;
    struct dn_naddr *node_addr;
    struct nodeent *np;

    enterprocedure("ConnectToDnetServer");

    /* establish a socket to the name server for this host */
    bzero((char *) &sdn, sizeof(sdn));
    ServerFD = socket(AF_DECnet, SOCK_STREAM, 0);
    if (ServerFD < 0) {
	perror("socket() to Server failed");
	debug(1, (stderr, "socket failed\n"));
	panic("Can't open connection to Server");
    }
    (void) setsockopt(ServerFD, SOL_SOCKET, SO_REUSEADDR, (char *) NULL, 0);
    (void) setsockopt(ServerFD, SOL_SOCKET, SO_USELOOPBACK, (char *) NULL, 0);

    /* determine the host machine for this process */
    debug(4, (stderr, "try to connect on %s\n", ServerHostName));

    if (ServerHostName[0] == '\0')
	node_addr = getnodeadd();
    else
	node_addr = dnet_addr(ServerHostName);

    if (node_addr == NULL) {
	if ((np = getnodebyname(ServerHostName)) == NULL) {
	    perror("getnodebyname failed");
	    debug(1, (stderr, "getnodebyname failed for %s\n", ServerHostName));
	    panic("Can't open connection to Server");
	} else {
	    bcopy(np->n_addr, sdn.sdn_nodeaddr, np->n_length);
	    sdn.sdn_nodeaddrl = np->n_length;
	}
    } else {
	sdn.sdn_add = *node_addr;
    }
    sdn.sdn_family = AF_DECnet;
/* XXX */
    sprintf(sdn.sdn_objname, "X$X%d", ServerOutPort + ServerDisplay);
    sdn.sdn_objnamel = strlen(sdn.sdn_objname);

    if (ServerOutPort == ServerInPort
	    && strcmp(ServerHostName, ScopeHost) == 0) {
	char        error_message[100];

	sprintf(error_message, "Trying to attach to myself: %s,%d\n",
		ServerHostName, ServerOutPort + ServerDisplay);
	panic(error_message);
    }
    /* ******************************************************** */
    /* try to connect to Server */

    if (connect(ServerFD, (struct sockaddr *) & sdn, sizeof(sdn)) < 0) {
	debug(4, (stderr, "connect returns errno of %d\n", errno));
	if (errno != 0)
	    if (report)
		perror("connect");
	switch (errno) {
	case ECONNREFUSED:
	    /*
	     * experience says this is because there is no Server to connect
	     * to
	     */
	    close(ServerFD);
	    debug(1, (stderr, "No Server\n"));
	    if (report)
		warn("Can't open connection to Server");
	    return (-1);

	default:
	    close(ServerFD);
	    panic("Can't open connection to Server");
	}
    }
    debug(4, (stderr, "Connect To Server: FD %d\n", ServerFD));
    if (ServerFD >= 0) {
	UsingFD(ServerFD, DataFromServer);
	StartServerConnection(ServerFD);
    }
    return (ServerFD);
}

#endif				/* DNET */

/* ********************************************** */
/*                                                */
/* ********************************************** */

char       *
OfficialName(name)
    char       *name;
{
    struct hostent *HostEntry;

#ifdef DNET
    if (UseDnetServer)
	return (name);
#endif				/* DNET */

    HostEntry = gethostbyname(name);
    if (HostEntry == NULL) {
	perror("gethostbyname");
	exit(-1);
    }
    debug(4, (stderr, "Official name of %s is %s\n", name, HostEntry->h_name));
    return (HostEntry->h_name);
}
