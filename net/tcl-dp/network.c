/*
 * network.c --
 *
 *	This file implements most of the network connection management
 *	functions of Tcl-DP.  The following comments are inherited from
 *	the progenitors of Tcl-DP.
 *
 * 	This file contains a simple Tcl "connect" command
 *	that returns an standard Tcl File descriptor (as would
 *	be returned by Tcl_OpenCmd).  This part of the file was written by
 *	Pekka Nikander <pnr@innopoli.ajk.tele.fi>
 *
 *	Tim MacKenzie <tym@dibbler.cs.monash.edu.au> extended it to
 *	create servers, accept connections, shutdown parts of full
 *	duplex connections and handle UNIX domain sockets.
 *
 *	Brian Smith <bsmith@cs.berkeley.edu> further modified it to
 *	add support for various send/receive primitives, and connectionless
 *	sockets.
 *
 * Copyright 1992 Telecom Finland
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that this copyright
 * notice appears in all copies.  Telecom Finland
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 *
 * Copyright 1992 Regents of the University of California.
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 *
 */

#include "tclInt.h"
#include "tclUnix.h"

#include <assert.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <sys/un.h>

#include "default.h"
#include "tclInt.h"
#include "tkInt.h"
#include "tk.h"

#include "network.h"

#define max(a,b)	((a)>(b)?(a):(b))
#define min(a,b)	((a)<(b)?(a):(b))
#define abs(a)		((a)>0?(a):-(a))

/*
 * This is a "magic number" prepended to the beginning of the packet
 * It's used to help resync the packet machanism in the event of errors.
 */
#define PACKET_MAGIC	0x6feeddcc

static int  inet_connect	_ANSI_ARGS_((char *host, int port,
					     int server, 
					     int udp));
static int  unix_connect	_ANSI_ARGS_((char *path, 
					     int server,
                                             int udp));
static void HandleEvent	_ANSI_ARGS_((ClientData clientData, 
					     int mask));

/*
 * For every file descriptor handler created, a structure of 
 * the following type is maintained.
 */
typedef struct FileHandle 
{
    Tcl_Interp *interp;
    OpenFile *filePtr;		/* Open file descriptor (file or socket) */
    int mask;			/* Mask of file descriptor conditions */
    char *rCmd;			/* Command to call on readable condition */
    char *wCmd;			/* Command to call on writable condition */
    char *eCmd;			/* Command to call on exception condition */
    char *fileId;		/* Represents filePtr */

} FileHandle;

static FileHandle *handlers[256];	/* Indexed by fd. */

/*
 *------------------------------------------------------------------
 *
 * Tcp_MakeOpenFile --
 *
 *      Set up an OpenFile structure in the interpreter.
 *
 * Results:
 *	None
 *
 * Side effects:
 *	Adds an OpenFile to the list.
 *------------------------------------------------------------------
 */

 /* ARGSUSED */
static void
Tcp_MakeOpenFile (interp, fd)
    Tcl_Interp *interp;
    int fd;
{
    Interp   *interpPtr = (Interp *) interp;
    OpenFile *filePtr;

    filePtr = (OpenFile *) ckalloc (sizeof (OpenFile));

    filePtr->f = NULL;
    filePtr->f2 = NULL;

    /*
     * Open the file with the correct type
     */
    filePtr->f = fdopen (fd, "r+");

    /*
     * Don't do buffered communication if full-duplex... it breaks!
     */
    setbuf (filePtr->f, (char *) NULL);

    filePtr->readable = 1;
    filePtr->writable = 1;
    filePtr->numPids = 0;
    filePtr->pidPtr = NULL;
    filePtr->errorId = -1;

    /*
     * Enter this new OpenFile structure in the table for the interpreter.
     * May have to expand the table to do this.
     */

    TclMakeFileTable (interpPtr, fd);
    if (interpPtr->filePtrArray[fd] != NULL) {
	panic("Tcl_OpenCmd found file already open");
    }
    interpPtr->filePtrArray[fd] = filePtr;
}

/*
 *------------------------------------------------------------------
 *
 * Tcp_ConnectCmd --
 *
 *	Open a socket and connect it.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	An open socket connection.
 *
 *------------------------------------------------------------------
 */

 /* ARGSUSED */
int
Tcp_ConnectCmd(notUsed, interp, argc, argv)
    ClientData notUsed;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    int fd;			/* Open file descriptor */
    int port;			/* User specified port number */
    char *pathname, *host;	/* Pathname (unix) or host (inet) */
    int unixSocket;		/* Unix domain socket? */
    int udp;			/* UDP protocol? */
    int server;			/* Set up listening socket? */

    udp = 0;
    server = 0;
    unixSocket = 0;
    switch (argc) {
    case 4:			/* Must be "connect -server host port" 
				 */
	if (strcmp(argv[1], "-server") != 0) {
	    sprintf(interp->result,
		    "%s: should be \"connect -server host port\"",
		    argv[0]);
	    return TCL_ERROR;
	}
	server = 1;
	host = argv[2];
	if (Tcl_GetInt(interp, argv[3], &port) != 0) {
	    return TCL_ERROR;
	}
	break;

    case 3:			/* Either "connect host port", 
				 * "connect -udp port", or 
				 * "connect -server path" 
				 */
	if (strcmp(argv[1], "-server") == 0) {
	    pathname = argv[2];
	    unixSocket = 1;
	    server = 1;
	} else if (strcmp(argv[1], "-udp") == 0) {
	    host = "";
	    if (Tcl_GetInt(interp, argv[2], &port) != 0) {
		return TCL_ERROR;
	    }
	    udp = 1;
	} else {
	    host = argv[1];
	    if (Tcl_GetInt(interp, argv[2], &port) != 0) {
		return TCL_ERROR;
	    }
	}
	break;

    case 2:			/* Must be "connect path" 
				 */
	pathname = argv[1];
	unixSocket = 1;
	break;

    default:
	sprintf(interp->result, "%s: should be one of the forms:\n",
		argv[0]);
	Tcl_AppendResult(interp, " \"connect -server host port\"\n",
			 (char *) NULL);
	Tcl_AppendResult(interp, " \"connect host port\"\n",
			 (char *) NULL);
	Tcl_AppendResult(interp, " \"connect -udp port\"\n",
			 (char *) NULL);
	Tcl_AppendResult(interp, " \"connect -server path\"\n",
			 (char *) NULL);
	Tcl_AppendResult(interp, " or \"connect path\"\n",
			 (char *) NULL);
	return TCL_ERROR;
    }

    /*
     * Create the connection
     */
    if (unixSocket) {
	fd = unix_connect(pathname, server, udp);
    } else {
	fd = inet_connect(host, port, server, udp);
    }

    if (fd < 0) {
	/* Tell them why it fell apart */
	if (unixSocket) {
	    if (server) {
		sprintf(interp->result,
			"Couldn't setup listening socket with path \"%s\": ",
			pathname, Tcl_UnixError(interp));
	    } else {
		sprintf(interp->result,
			"Couldn't connect to \"%s\": ", pathname,
			Tcl_UnixError(interp));
	    }
	} else if (server) {
	    if (port == 0) {
		sprintf(interp->result,
			"Couldn't setup listening socket on any port: %s",
			Tcl_UnixError(interp));
	    } else {
		sprintf(interp->result,
			"Couldn't setup listening socket on port %d: %s",
			port, Tcl_UnixError(interp));
	    }
	} else {
	    sprintf(interp->result,
		    "Couldn't open connection to %s:%d : %s",
		    host, port, Tcl_UnixError(interp));
	}
	return TCL_ERROR;
    }

    if (!unixSocket) {
      struct sockaddr_in sockaddr;
      int res, len;
	  
      /* Find the local port we're using for the connection. */
	  
      len = sizeof (sockaddr);
      res = getsockname (fd, (struct sockaddr *) &sockaddr, &len);
	  
      if (res < 0) {
	sprintf(interp->result, "file%d %d", fd, errno);
      } else 
	sprintf(interp->result, "file%d %d", fd,
		(int) ntohs(sockaddr.sin_port));
    } else {
      sprintf(interp->result, "file%d", fd);
    }

    Tcp_MakeOpenFile(interp, fd);

    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * FindFileHandler --
 *
 *	Find the filehandler associated with the
 *	descriptor passed in.
 *
 * Results:
 *	A pointer to the handler, or NULL if there is none.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

static FileHandle *
FindFileHandler (filePtr)
    OpenFile *filePtr;
{
  int fd;

  if (!filePtr)
    return ((FileHandle *) NULL);

  fd = fileno (filePtr->f);

  if ((fd < 0) || (fd > 255))
    return ((FileHandle *) NULL);

  return (handlers[fd]);
}

/*
 *------------------------------------------------------------------
 *
 * Tcp_ShutdownCmd --
 *
 *    Shutdown a socket for reading writing or both using 
 *    the shutdown (2) system call.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	Modifies the OpenFile structure appropriately.
 *	Delete any created filehandlers.
 *
 *------------------------------------------------------------------
 */

 /* ARGSUSED */
int
Tcp_ShutdownCmd(notUsed, interp, argc, argv)
    ClientData notUsed;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    OpenFile *filePtr;
    int fd;
    FileHandle *handler;

    /*
     * Check args, find file
     */
    if (argc != 3) 
      {
      wrong_args:
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " fileid <option>\"", (char *) NULL);
	return TCL_ERROR;
      }

    if (TclGetOpenFile (interp, argv[1], &filePtr) != TCL_OK)
	return TCL_ERROR;

    fd      = fileno (filePtr->f);
    handler = FindFileHandler (filePtr);

    /*
     * Call shutdown with correct args, update file handler
     */
    if (!strcmp(argv[2], "0") || !strcmp(argv[2], "receives") ||
	!strcmp(argv[2], "read")) {
	if (!filePtr->readable) {
	    Tcl_AppendResult(interp, "File is not readable", (char *) NULL);
	    return TCL_ERROR;
	}
	if (shutdown(fd, 0)) {
	    Tcl_AppendResult(interp, "shutdown: ", Tcl_UnixError(interp),
			     (char *) NULL);
	    return TCL_ERROR;
	}
	filePtr->readable = 0;
    } else if (!strcmp(argv[2], "1") || !strcmp(argv[2], "sends") ||
	       !strcmp(argv[2], "write")) {
	if (!filePtr->writable) {
	    Tcl_AppendResult(interp, "File is not writable", (char *) NULL);
	    return TCL_ERROR;
	}
	if (shutdown(fd, 1)) {
	    Tcl_AppendResult(interp, "shutdown: ", Tcl_UnixError(interp),
			     (char *) NULL);
	    return TCL_ERROR;
	}
	filePtr->writable = 0;
    } else if (!strcmp(argv[2], "2") || !strcmp(argv[2], "all") ||
	       !strcmp(argv[2], "both")) {
	if (shutdown(fd, 2)) {
	    Tcl_AppendResult(interp, "shutdown: ", Tcl_UnixError(interp),
			     (char *) NULL);
	    return TCL_ERROR;
	}
	filePtr->writable = 0;
	filePtr->readable = 0;
    } else
	goto wrong_args;

    /*
     * Update the handler, freeing it if it's dead.
     */
    if (handler) 
      {
	if ((filePtr->readable == 0) &&
	    (handler->rCmd != NULL)) {
	    Tk_EventuallyFree((ClientData) handler->rCmd, free);
	    handler->rCmd = NULL;
	}
	if ((filePtr->writable == 0) &&
	    (handler->wCmd != NULL)) {
	    Tk_EventuallyFree((ClientData) handler->wCmd, free);
	    handler->wCmd = NULL;
	}
	if ((filePtr->writable == 0) && (filePtr->readable == 0)) {
	  if (handler->eCmd != NULL) {
	    Tk_EventuallyFree((ClientData) handler->eCmd, free);
	    handler->eCmd = NULL;
	  }

	  /*
	   * Delete handler.
	   */
	  Tk_DeleteFileHandler (fd);

	  handlers[fd] = (FileHandle *) NULL;
	  Tk_EventuallyFree((ClientData) handler, free);
	}
      }
    return TCL_OK;
}

/*
 *------------------------------------------------------------------
 *
 * Tcp_AcceptCmd --
 *
 *    Accept a connection on a listening socket
 *
 * Results:
 *	a standard tcl result
 *
 * Side effects:
 *	Opens a new file.
 *
 *------------------------------------------------------------------
 */

 /* ARGSUSED */
int
Tcp_AcceptCmd(notUsed, interp, argc, argv)
    ClientData notUsed;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    struct sockaddr_in sockaddr;
    int len = sizeof sockaddr;
    OpenFile *filePtr;
    int fd;

    if (argc != 2) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " listening_socket\"", (char *) NULL);
	return TCL_ERROR;
    }

    if (TclGetOpenFile (interp, argv[1], &filePtr) != TCL_OK) {
	return TCL_ERROR;
    }

    if (!filePtr->readable) 
      {
	Tcl_AppendResult (interp, "\"", argv[1],
			  "\" wasn't opened for reading", (char *) NULL);
	return TCL_ERROR;
      }

    fd = accept (fileno (filePtr->f), (struct sockaddr *) &sockaddr, &len);

    if (fd < 0) 
      {
	Tcl_AppendResult (interp, strerror ("accept"), 
			  (char *) NULL);
	Tk_BackgroundError (interp);

	return TCL_ERROR;
      }

    /*
     * Create the fileId structure.
     */
    Tcp_MakeOpenFile (interp, fd);

    sprintf (interp->result, "file%d", fd);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------
 *
 * unix_connect --
 *
 * 	Create a (unix_domain) fd connection using given rendevous
 *
 * Results:
 *	An open fd or -1.
 *
 * Side effects:
 * 	None.
 *----------------------------------------------------------------
 */

static int
unix_connect(path, server, udp)
    char *path;			/* Path name to create or use */
    int server;			/* 1->make server, 0->connect to server */
    int udp;			/* Make it a udp protocol socket */
{
    struct sockaddr_un sockaddr;
    int sock, status;
    extern int errno;

    if (udp) {
	sock = socket(PF_UNIX, SOCK_DGRAM, 0);
    } else {
	sock = socket(PF_UNIX, SOCK_STREAM, 0);
    }
    if (sock < 0) {
	return -1;
    }
    bzero((char *) &sockaddr, sizeof(sockaddr));
    sockaddr.sun_family = AF_UNIX;
    strncpy(sockaddr.sun_path, path, sizeof(sockaddr.sun_path) - 1);
    /* Just in case addr is too long... */
    sockaddr.sun_path[sizeof(sockaddr.sun_path) - 1] = 0;

    if (server)
	status = bind(sock, (struct sockaddr *) & sockaddr, sizeof(sockaddr));
    else if (!udp) {
	status = connect(sock, (struct sockaddr *) & sockaddr, sizeof(sockaddr));
    }
    if (status < 0) {
	close(sock);
	return -1;
    }
    if (server && !udp) {
	listen(sock, 5);
    }
    return sock;
}

/*
 *----------------------------------------------------------------
 *
 * inet_connect --
 *
 * 	Create a (inet domain) fd connection to given host and port.
 *
 * Results:
 *	An open fd or -1.
 *
 * Side effects:
 * 	None.
 *----------------------------------------------------------------
 */

static int
inet_connect(host, port, server, udp)
    char *host;			/* Host to connect, name or IP address */
    int port;			/* Port number to use */
    int server;			/* 1->make server, 0->connect to server */
    int udp;			/* Make it a udp protocol socket */
{
    struct hostent *hostent, _hostent;
    struct sockaddr_in sockaddr;
    int sock, status;
    int hostaddr, hostaddrPtr[2];
    extern int errno;

    hostent = gethostbyname(host);
    if (hostent == NULL) {
	hostaddr = inet_addr(host);
	if (hostaddr == -1) {
	    if (server && !strlen(host))
		hostaddr = INADDR_ANY;
	    else {
		errno = EINVAL;
		return -1;
	    }
	}
	_hostent.h_addr_list = (char **) hostaddrPtr;
	_hostent.h_addr_list[0] = (char *) &hostaddr;
	_hostent.h_addr_list[1] = NULL;
	_hostent.h_length = sizeof(hostaddr);
	_hostent.h_addrtype = AF_INET;
	hostent = &_hostent;
    }
    if (udp) {
	sock = socket(PF_INET, SOCK_DGRAM, 0);
    } else {
	sock = socket(PF_INET, SOCK_STREAM, 0);
    }
    if (sock < 0) {
	return -1;
    }
    bzero((char *) &sockaddr, sizeof(sockaddr));
    sockaddr.sin_family = AF_INET;
    memcpy((char *) &(sockaddr.sin_addr.s_addr),
	   (char *) hostent->h_addr_list[0],
	   (size_t) hostent->h_length);
    sockaddr.sin_port = htons(port);

    if (server | udp) {
	status = bind(sock, (struct sockaddr *) &sockaddr, 
		      sizeof(sockaddr));
    } else {
	status = connect(sock, (struct sockaddr *) &sockaddr, 
			 sizeof(sockaddr));
    }

    if (status < 0) {
	close(sock);
	return -1;
    }
    if (server && !udp) {
	listen(sock, 5);
    }
    return sock;
}

/*
 *----------------------------------------------------------------
 *
 * Tcp_FileHandlerCmd --
 *
 * 	Register a file handler with an open fileId.  If there is
 *	already and existing handler, it will be no longer called.
 *	If no mask and command are given, any existing handler
 *	will be deleted.
 *
 * Results:
 *	A standard Tcl result. (Always OK).
 *
 * Side effects:
 *	A new file handler is associated with a give TCL open file.
 *	Whenever the file is readable, writeable and/or there is
 *	an expection condition on the file, a user supplied TCL
 *	command is called.
 *
 *----------------------------------------------------------------
 */

 /* ARGSUSED */
int
Tcp_FileHandlerCmd(notUsed, interp, argc, argv)
    ClientData notUsed;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    OpenFile *filePtr;
    int fd, mask;
    FileHandle *handler;

    /*
     * Checks args.
     */
    if (argc != 2 && argc != 4) 
      {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " fileId ?mode command?\"", (char *) NULL);
	return TCL_ERROR;
      }

    if (TclGetOpenFile (interp, argv[1], &filePtr) != TCL_OK) 
      {
	return TCL_ERROR;
      }

    fd = fileno (filePtr->f);

    if (handler = FindFileHandler (filePtr))
      {
	if (handler->rCmd != NULL) {
	  Tk_EventuallyFree((ClientData) handler->rCmd, free);
	}
	if (handler->wCmd != NULL) {
	  Tk_EventuallyFree((ClientData) handler->wCmd, free);
	}
	if (handler->eCmd != NULL) {
	  Tk_EventuallyFree((ClientData) handler->eCmd, free);
	}

	handlers[fd] = (FileHandle *) NULL;

	if (handler->fileId)
	  ckfree ((char *) handler->fileId);
	ckfree ((char *) handler);

	handler = NULL;
      }

    Tk_DeleteFileHandler (fd);
  
    if (argc == 2) 
      return TCL_OK;

    /*
     * Find out on what situations the user is interested in. 
     * This is not the most elegant or efficient way to do this, 
     * but who cares?
     */
    mask = 0;
    if (strchr(argv[2], 'r')) {
	mask |= TK_READABLE;
    }
    if (strchr(argv[2], 'w')) {
	mask |= TK_WRITABLE;
    }
    if (strchr(argv[2], 'e')) {
	mask |= TK_EXCEPTION;
    }
    if (mask == 0 || (strlen(argv[2]) != strspn(argv[2], "rwe"))) {
	Tcl_AppendResult(interp, "bad mask argument \"", argv[2],
		  "\": should be any combination of \"r\", \"w\" and \"e\"",
			 (char *) NULL);
	return TCL_ERROR;
    }

    /*
     * Create a new handler.
     */

    handler = (FileHandle *) ckalloc (sizeof (FileHandle));
    handler->interp  = interp;
    handler->filePtr = filePtr;
    handler->fileId  = ckalloc (strlen (argv[1]) + 1);
    handler->rCmd = NULL;
    handler->wCmd = NULL;
    handler->eCmd = NULL;
    handler->mask = 0;
    strcpy (handler->fileId, argv[1]);

    handlers[fd] = handler;

    if (mask & TK_READABLE) {
	if (handler->rCmd) {
	    Tk_EventuallyFree((ClientData) handler->rCmd, free);
	}
	handler->rCmd = ckalloc(strlen(argv[3]) + 1);
	strcpy(handler->rCmd, argv[3]);
    }
    if (mask & TK_WRITABLE) {
	if (handler->wCmd) {
	    Tk_EventuallyFree((ClientData) handler->wCmd, free);
	}
	handler->wCmd = ckalloc(strlen(argv[3]) + 1);
	strcpy(handler->wCmd, argv[3]);
    }
    if (mask & TK_EXCEPTION) {
	if (handler->eCmd) {
	    Tk_EventuallyFree((ClientData) handler->eCmd, free);
	}
	handler->eCmd = ckalloc(strlen(argv[3]) + 1);
	strcpy(handler->eCmd, argv[3]);
    }

    handler->mask = mask;

    /*
     * Finally, get Tk to call HandleEvent whenever there is a
     * file descriptor condition.
     */

#ifdef TK_EXTENDED
    Tk_CreateFileHandler (fd, (FILE *) NULL, mask, 
			  HandleEvent, (ClientData) handler);
#else
    Tk_CreateFileHandler (fd, mask, HandleEvent, (ClientData) handler);
#endif

    return TCL_OK;
}

/*
 *----------------------------------------------------------------
 *
 * HandleEvent --
 *
 * 	This procedure is called from Tk_DoOneEvent whenever there is
 *	a file descriptor condition on a given file descriptor.  It is
 *	installed by the "filehandler" command.  A Tcl command
 *	given by the user is executed to handle the condition.  If
 *	and EOF or ERROR condition is noticed, the file descriptor
 *	is closed.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The user supplied command can do anything.
 *
 *----------------------------------------------------------------
 */

static void
HandleEvent(clientData, mask)
    ClientData clientData;
    int mask;
{
    int result;
    FileHandle *handler = (FileHandle *) clientData;
    Tcl_Interp *interp;
    OpenFile   *filePtr;
    OpenFile *dummy;
    int fd;
    int delete;

    if (!handler)
      return;

    if (!(interp = handler->interp) ||
	!(filePtr = handler->filePtr))
      return;

    Tk_Preserve ((ClientData) handler);

    delete = 0;
    if (TclGetOpenFile (interp, handler->fileId, &dummy) != TCL_OK) 
      {
	/*  File descriptor is closed. 
	 */
	Tcl_ResetResult (interp);
	delete = 1;
      }
    else 
      {
	assert (dummy == handler->filePtr);

	if (mask & TK_EXCEPTION) {
	  if (handler->eCmd != NULL) 
	    {
	      Tk_Preserve((ClientData) handler->eCmd);
	      result = Tcl_VarEval(interp, handler->eCmd, " e ",
				   handler->fileId, (char *) NULL);
	      Tk_Release((ClientData) handler->eCmd);
	      if (result != TCL_OK)
		goto close;
	    } 
	  else 
	    goto close;
	  
	} else if ((mask & TK_READABLE) && (handler->rCmd != NULL)) {
	    Tk_Preserve((ClientData) handler->rCmd);
	    result = Tcl_VarEval(interp, handler->rCmd, " r ",
				 handler->fileId, (char *) NULL);
	    Tk_Release((ClientData) handler->rCmd);
	    if (result != TCL_OK) {
		Tk_BackgroundError(interp);
	    }
	} else if ((mask & TK_WRITABLE) && (handler->wCmd != NULL)) {
	    Tk_Preserve((ClientData) handler->wCmd);
	    result = Tcl_VarEval(interp, handler->wCmd, " w ",
				 handler->fileId, (char *) NULL);
	    Tk_Release((ClientData) handler->wCmd);
	    if (result != TCL_OK) {
		Tk_BackgroundError(interp);
	    }
	} else if (feof(filePtr->f) || ferror(filePtr->f)) 
	  {
	  close:
	    if (Tcl_VarEval (interp, "close ", handler->fileId,
			     (char *) NULL) != TCL_OK)
	      Tk_BackgroundError (interp);

	    delete = 1;
	  }
      }

    Tk_Release ((ClientData) handler);

    if (delete) 
      {
	int i;

	i = 0;
	while (i < 256)
	  {
	    if (handlers[i] == handler)
	      fd = i;
	    i++;
	  }

	handlers[fd] = (FileHandle *) NULL;
	Tk_DeleteFileHandler (fd);
	Tk_EventuallyFree ((ClientData) handler, free);
    }
}

/*
 *--------------------------------------------------------------
 *
 * Tcp_ReceiveCmd --
 *
 *      This procedure is invoked to process the "receive" Tcl/Tk
 *      command.  See the user documentation for details on what
 *      it does.
 *
 * Results:
 *	A standard tcl result.
 *
 * Side effects:
 *	The file descriptor passed in is read.
 *
 *--------------------------------------------------------------
 */

#define READ_BUF_SIZE 4095

 /* ARGSUSED */
int
Tcp_ReceiveCmd(clientData, interp, argc, argv)
    ClientData *clientData;	/* Ignored */
    Tcl_Interp *interp;		/* Tcl interpreter */
    int argc;			/* Number of arguments */
    char *argv[];		/* Arg list */
{
    OpenFile *filePtr;
    int count;
    char buffer[READ_BUF_SIZE + 1];
    int flags, i, len;
    char c;

    if ((argc < 2) || (argc > 4)) {
	goto syntaxError;
    }

    /*  Parse flag parameters;
     */

    flags = 0;
    for (i = 2; i < argc; i++) {
	len = strlen(argv[i]);
	if (argv[i][0] != '-') {
	    goto syntaxError;
	}
	c = argv[i][1];
	if ((c == 'p') && (strncmp(argv[i], "-peek", len) == 0)) 
	  flags |= MSG_PEEK;
	else if ((c == 'o') && 
		 (strncmp(argv[i], "-out-of-band", len) == 0)) 
	  flags |= MSG_OOB;
	else
	  goto syntaxError;
    }

    if (TclGetOpenFile(interp, argv[1], &filePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (!filePtr->readable) {
	Tcl_AppendResult(interp, "\"", argv[1],
			 "\" wasn't opened for reading", (char *) NULL);
	return TCL_ERROR;
    }

    /*
     *  Read the message in one or more chunks.
     */
    flags = 0;
    buffer[READ_BUF_SIZE] = 0;
    while (1) 
      {
	/*
	 * Read the message and put on trailing 0 at end of string in case we
	 * received a partial message.
	 */
	count = recv(fileno(filePtr->f), buffer, READ_BUF_SIZE, flags);
	if ((count == -1) || ((count != strlen(buffer) + 1))) {
	    Tcl_ResetResult(interp);
	    Tcl_AppendResult(interp, "error reading \"", argv[1],
			     "\": ", Tcl_UnixError(interp), (char *) NULL);
	    Tk_BackgroundError(interp);
	    return TCL_ERROR;
	}
	Tcl_AppendResult(interp, buffer, (char *) NULL);
	if (count < READ_BUF_SIZE) {
	    break;
	}
      }

    return TCL_OK;

  syntaxError:

    Tcl_AppendResult (interp, 
		      "wrong # args: should be \"", argv[0],
		      " fileId ?-peek? ?-out-of-band?\"", 
		      (char *) NULL);
    return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * Tcp_SendCmd --
 *
 *      This procedure is invoked to process the "send" Tcl/Tk
 *      command.  See the user documentation for details on what
 *      it does.
 *
 * Results:
 *	A standard tcl result.
 *
 * Side effects:
 *	The specified string is written to the file descriptor passed
 *	in.
 *
 *--------------------------------------------------------------
 */

 /* ARGSUSED */
int
Tcp_SendCmd(clientData, interp, argc, argv)
    ClientData *clientData;	/* Ignored */
    Tcl_Interp *interp;		/* Tcl interpreter */
    int argc;			/* Number of arguments */
    char *argv[];		/* Arg list */
{
    OpenFile *filePtr;
    FILE *f;
    int len, flags;

    flags = 0;
    if (argc == 4) {
	if (strncmp(argv[3], "-out-of-band", strlen(argv[3])) != 0) {
	    Tcl_AppendResult(interp, "bad argument \"", argv[3],
			   "\": should be \"-out-of-band\"", (char *) NULL);
	    return TCL_ERROR;
	} else {
	    flags |= MSG_OOB;
	}
    } else if (argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " fileId string ?-out-of-band?\"", (char *) NULL);
	return TCL_ERROR;
    }
    if (TclGetOpenFile(interp, argv[1], &filePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (!filePtr->writable) {
	Tcl_AppendResult(interp, "\"", argv[1],
			 "\" wasn't opened for writing", (char *) NULL);
	return TCL_ERROR;
    }
    f = filePtr->f;
    len = strlen(argv[2]) + 1;
    if (len != send(fileno(f), argv[2], len, flags)) {
	Tcl_AppendResult(interp, "error writing \"", argv[1],
			 "\": ", Tcl_UnixError(interp), (char *) NULL);
	Tk_BackgroundError(interp);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * IsReadable --
 *
 *      This function determines if a file descriptor is readable.
 *
 * Results:
 *	1 if fd is readable, 0 otherwise.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */

int
IsReadable(fd)
    int fd;
{
    fd_set fds;
    fd_set *fdset;
    struct timeval tv;

    fdset = &fds;

    FD_ZERO (fdset);
    FD_SET (fd, fdset);

    tv.tv_sec = 0;
    tv.tv_usec = 0;

    return (select (fd + 1, fdset, 
		    (fd_set *) NULL, 
		    (fd_set *) NULL, &tv) == 1);
}

/*
 *--------------------------------------------------------------
 *
 * Tcp_PacketReceive --
 *
 *      This procedure is the C interface to the "packetReceive"
 *      extension. See the user documentation for a description.
 *
 * Results:
 *	A standard tcl result.
 *
 * Side effects:
 *	The file descriptor passed in is read.
 *
 *--------------------------------------------------------------
 */

int
Tcp_PacketReceive(interp, filePtr, block)
    Tcl_Interp *interp;		/* Tcl interpreter */
    OpenFile *filePtr;
    int block;
{
    /*
     * The next two variables are used to keep track of partial
     * messages that come in (indexed by fd).  It sometimes happens 
     * that only part of a message will be available at the time we 
     * get called here.  In this case, we save the partial message in
     * partialBuffer[fd] until we can process the whole thing.
     */
    static char *partialBuffer[256];	/* Buffer for data (NULL -> no data) */
    static int   partialSize[256];	/* Size of the buffer */

    int magic;
    int currSize, maxSize;
    char *buffer;
    char *offset;
    int numRead, numLeft;
    int packetLen;
    int fd, tmp;
    int argc;
    char *argv[256];

    if (!filePtr || (!filePtr->readable)) 
      {
	Tcl_AppendResult(interp, "socket wasn't opened for reading",
			 (char *) NULL);
	return TCL_ERROR;
      }

    fd = fileno(filePtr->f);

    /*
     * If we're going to block, then go ahead with the
     * read, otherwise check if it's readable first.
     */
    if (block || IsReadable(fd))
      {
	/*
	 * Add in what was in partialBuffer.
	 */
	if (partialSize[fd] > 0) 
	  {
	    currSize = partialSize[fd];
	    maxSize = 8192 + partialSize[fd];
	    buffer = realloc (partialBuffer[fd], (unsigned) maxSize);

	    /* XXX: Should check for alloc error */

	    partialBuffer[fd] = NULL;
	    partialSize[fd] = 0;
	  } 
	else 
	  {
	    currSize = 0;
	    maxSize = 8192;
	    buffer = (char *) malloc ((unsigned) maxSize);

	    /* XXX: Should check for alloc error */
	  }

	/*
	 * Read in everything we can from the pipe.
	 */
	while ((numRead = read (fd, buffer+currSize, 4096)) == 4096) 
	  {
	    currSize += 4096;
	    if (currSize >= maxSize) 
	      {
		maxSize += 8192;
		buffer = realloc (buffer, (unsigned) maxSize);

		/* XXX: Should check for alloc error */
	      }
	  }
	currSize += numRead;

	/* Test for read error.
	 */
	if (numRead <= 0) 
	  {
	    /*
	     * The file descriptor was readable, but there was no data.
	     * The connection's probably closed, and we let our 
	     * caller close the connection.
	     */
	    char str[10];
	    sprintf (str, "file%d ", fd);

	    Tcl_AppendResult (interp, "Tcp_PacketReceive -- error reading ",
			      str, Tcl_UnixError (interp),
			      (char *) NULL);

	    Tk_BackgroundError (interp);

	    free (buffer);
	    return TCL_ERROR;
	  }
      }
    else 
      {
	/*
	 * The file descriptor is not readable, and caller doesn't 
	 * want us to block, so return NULL string.
	 */
	Tcl_ResetResult (interp);
	return TCL_OK;
      }

    /*
     * Ok, we've got data.  Create an argc and argv list, one for each
     * packet we read, and merge 'em to create the result.
     *
     * Format of each packet:
     *
     *		First 4 bytes are PACKET_MAGIC.
     *		Next 4 bytes are packetLen.
     *		Next packetLen-sizeof(int) is zero terminated string
     */

    offset = buffer;
    Tcl_ResetResult(interp);

    argc = 0;
    numLeft = currSize;
    while (numLeft > 0) {
	/*
	 * Every packet is at least 8 bytes (of header).  Check that
	 * we've got enough of a packet to parse, otherwise set things
	 * up so we fall through to the code to save partial packets.
	 */
	if (numLeft >= 8) {

	    bcopy (offset, 
		   (char *) &tmp, sizeof (int));
	    magic = ntohl (tmp);

	    bcopy (offset+ sizeof (int), 
		   (char *) &tmp, sizeof(int));
	    packetLen = ntohl (tmp);

	} else {
	    magic = PACKET_MAGIC;
	    packetLen = 8;
	}

	/*
	 * Check the magic number on the packet.
	 */
	if (magic != PACKET_MAGIC)
	  {
	    /*
	     * Badly formatted packet.  Drop the rest on the floor.
	     */
	    char str[10];
	    sprintf (str, "file%d", fd);

	    Tcl_AppendResult (interp, "Tcp_PacketReceive -- error on ", 
			      str, " (badly formatted packet) ",
			      Tcl_UnixError (interp), 
			      (char *) NULL);

	    Tk_BackgroundError (interp);

	    return TCL_ERROR;
	  }

	/*
	 * If we only read part of the packet, save the rest 
	 * for when more data comes in.
	 */
	if (numLeft < packetLen) 
	  {
	    partialBuffer[fd] = malloc ((unsigned) numLeft);
	    partialSize[fd]   = numLeft;
	    bcopy (offset, partialBuffer[fd], numLeft);
	    numLeft = 0;
	  } 
	else 
	  {
	    argv[argc++] = offset + 2 * sizeof (int);
	    offset  += packetLen;
	    numLeft -= packetLen;
	  }
    }

    Tcl_SetResult (interp, Tcl_Merge (argc, argv), TCL_DYNAMIC);
    free (buffer);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * Tcp_PacketReceiveCmd --
 *
 *      This procedure is invoked to process the "packetReceive" Tcl/Tk
 *      command.  See the user documentation for details on what
 *      it does.
 *
 * Results:
 *	A standard tcl result.
 *
 * Side effects:
 *	The specified string is written to the file descriptor passed
 *	in.
 *
 *--------------------------------------------------------------
 */

 /* ARGSUSED */
int
Tcp_PacketReceiveCmd(clientData, interp, argc, argv)
    ClientData *clientData;	/* Ignored */
    Tcl_Interp *interp;		/* Tcl interpreter */
    int argc;			/* Number of arguments */
    char *argv[];		/* Arg list */
{
    OpenFile *filePtr;
    int block;

    if (((argc == 3) && (strcmp(argv[2], "-noblock") != 0)) ||
	(argc != 2)) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " fileId ?-noblock?\"", (char *) NULL);
	return TCL_ERROR;
    }
    block = (argc == 2);
    if (TclGetOpenFile(interp, argv[1], &filePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    return (Tcp_PacketReceive(interp, filePtr, block));
}

/*
 *--------------------------------------------------------------
 *
 * Tcp_PacketSend --
 *
 *      This procedure is the C interface to the "packetSend" extension.
 *
 * Results:
 *	A standard tcl result.
 *
 * Side effects:
 *	The specified string is written to the file descriptor passed
 *	in.
 *
 *--------------------------------------------------------------
 */

int
Tcp_PacketSend(interp, filePtr, message)
    Tcl_Interp *interp;		/* Tcl interpreter */
    OpenFile *filePtr;
    char *message;
{
    int fd;
    int strLen;
    int packetLen, tmp;
    static char buffer[4096];

    if (!filePtr || (!filePtr->writable)) 
      {
	Tcl_AppendResult (interp, "socket wasn't opened for writing",
			  (char *) NULL);
	return TCL_ERROR;
      }

    fd = fileno (filePtr->f);

    /*
     * Format up the packet:
     *	  First 4 bytes are PACKET_MAGIC.
     *	  Next 4 bytes are packetLen.
     *	  Next packetLen-(sizeof(int)) bytes are zero terminated message.
     */
    strLen = strlen (message) + 1;

    tmp = htonl (PACKET_MAGIC);
    bcopy ((char *) &tmp, buffer, sizeof(int));

    packetLen = strLen + 2 * sizeof (int);
    tmp = htonl (packetLen);
    bcopy ((char *) &tmp, buffer + sizeof (int), sizeof (int));

    bcopy (message, buffer + 2 * sizeof (int), strLen);
    if (write (fd, buffer, packetLen) != packetLen) 
      {
	char str[10];
	sprintf (str, "file%d ", fd);

	Tcl_AppendResult (interp, "Tcp_PacketSend -- error writing ", str,
			  Tcl_UnixError (interp), (char *) NULL);

	Tk_BackgroundError (interp);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * Tcp_PacketSendCmd --
 *
 *      This procedure is invoked to process the "packetSend" Tcl/Tk
 *      command.  See the user documentation for details on what
 *      it does.
 *
 * Results:
 *	A standard tcl result.
 *
 * Side effects:
 *	The specified string is written to the file descriptor passed
 *	in.
 *
 *--------------------------------------------------------------
 */

 /* ARGSUSED */
int
Tcp_PacketSendCmd(clientData, interp, argc, argv)
    ClientData *clientData;	/* Ignored */
    Tcl_Interp *interp;		/* Tcl interpreter */
    int argc;			/* Number of arguments */
    char *argv[];		/* Arg list */
{
    OpenFile *filePtr;

    if (argc != 3) 
      {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " fileId string\"", (char *) NULL);
	return TCL_ERROR;
      }

    if (TclGetOpenFile (interp, argv[1], &filePtr) != TCL_OK)
      return TCL_ERROR;

    return (Tcp_PacketSend (interp, filePtr, argv[2]));
}

/*
 *--------------------------------------------------------------
 *
 * Tcp_ReceiveFromCmd --
 *
 *      This procedure is invoked to process the "receiveFrom" Tcl/Tk
 *      command.  See the user documentation for details on what
 *      it does.
 *
 * Results:
 *	A standard tcl result.
 *
 * Side effects:
 *	The file descriptor passed in is read.
 *
 *--------------------------------------------------------------
 */
 /* ARGSUSED */
int
Tcp_ReceiveFromCmd(notUsed, interp, argc, argv)
    ClientData *notUsed;
    Tcl_Interp *interp;		/* Tcl interpreter */
    int argc;			/* Number of arguments */
    char *argv[];		/* Arg list */
{
    OpenFile *filePtr;
    int fd, first;
    char buffer[READ_BUF_SIZE + 1];

    if (argc != 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " fileId\"", (char *) NULL);
	return TCL_ERROR;
    }
    if (TclGetOpenFile(interp, argv[1], &filePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (!filePtr->readable) {
	Tcl_AppendResult(interp, "\"", argv[1],
			 "\" wasn't opened for reading", (char *) NULL);
	return TCL_ERROR;
    }
    buffer[READ_BUF_SIZE] = 0;
    first = 1;
    fd = fileno(filePtr->f);
    while (1) {
	int count, addrLen;
	struct sockaddr_in addr;

	/*
	 * Read the message and put on trailing 0 at end of string in case we
	 * received a partial message.
	 */
	addrLen = sizeof(addr);
	bzero((char *) &addr, addrLen);
	count = recvfrom(fd, buffer, READ_BUF_SIZE, 0, &addr, &addrLen);
	if ((count == -1) || ((count != strlen(buffer) + 1))) {
	    Tcl_ResetResult(interp);
	    Tcl_AppendResult(interp, "error reading \"", argv[1],
			     "\": ", Tcl_UnixError(interp), (char *) NULL);
	    Tk_BackgroundError(interp);
	    return TCL_ERROR;
	}
	if (first) {
	    char *addrName;

	    addrName = Tcm_CreateAddress(addr.sin_addr.s_addr,
					 (int) ntohs(addr.sin_port));
	    Tcl_AppendResult(interp, addrName, " {", (char) NULL);
	    first = 0;
	}
	Tcl_AppendResult(interp, buffer, (char *) NULL);
	if (count < READ_BUF_SIZE) {
	    break;
	}
    }
    Tcl_AppendResult(interp, "}", (char *) NULL);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * Tcp_SendToCmd --
 *
 *      This procedure is invoked to process the "sendTo" Tcl/Tk
 *      command.  See the user documentation for details on what
 *      it does.
 *
 * Results:
 *	A standard tcl result.
 *
 * Side effects:
 *	The specified string is written to the file descriptor passed
 *	in.
 *
 *--------------------------------------------------------------
 */

 /* ARGSUSED */
int
Tcp_SendToCmd(clientData, interp, argc, argv)
    ClientData *clientData;	/* Ignored */
    Tcl_Interp *interp;		/* Tcl interpreter */
    int argc;			/* Number of arguments */
    char *argv[];		/* Arg list */
{
    OpenFile *filePtr;
    int fd, len, status;
    struct sockaddr_in *addrPtr;

    if (argc != 4) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " fileId string address\"", (char *) NULL);
	return TCL_ERROR;
    }
    if (TclGetOpenFile(interp, argv[1], &filePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (!filePtr->writable) {
	Tcl_AppendResult(interp, "\"", argv[1],
			 "\" wasn't opened for writing", (char *) NULL);
	return TCL_ERROR;
    }
    fd = fileno(filePtr->f);
    errno = 0;
    len = strlen(argv[2]) + 1;
    addrPtr = Tcm_FindAddr(argv[3]);
    if (addrPtr == NULL) {
	Tcl_AppendResult(interp, argv[0], ": invalid address \"", argv[3],
			 "\"", (char *) NULL);
	return TCL_ERROR;
    }
    status = sendto(fd, argv[2], len, 0, addrPtr, 
		    sizeof(struct sockaddr_in));
    if (len != status) {
	Tcl_AppendResult(interp, "error writing \"", argv[1],
			 "\": ", Tcl_UnixError(interp), (char *) NULL);
	Tk_BackgroundError(interp);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * Tcp_Init -
 *
 *	Initialize the connection management level functions of 
 *	Tcl-DP and register them with the given interpreter.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Serveral new commands are added to the interpreter.
 *
 *--------------------------------------------------------------
 */

void
Tcp_Init(interp)
    Tcl_Interp *interp;		/* Tcl interpreter */
{
    Tcl_CreateCommand(interp, "connect", Tcp_ConnectCmd,
		      (ClientData) NULL, (void (*) ()) NULL);
    Tcl_CreateCommand(interp, "shutdown", Tcp_ShutdownCmd,
		      (ClientData) NULL, (void (*) ()) NULL);
    Tcl_CreateCommand(interp, "accept", Tcp_AcceptCmd,
		      (ClientData) NULL, (void (*) ()) NULL);
    Tcl_CreateCommand(interp, "filehandler", Tcp_FileHandlerCmd,
		      (ClientData) NULL, (void (*) ()) NULL);
    Tcl_CreateCommand(interp, "send", Tcp_SendCmd,
		      (ClientData) NULL, (void (*) ()) NULL);
    Tcl_CreateCommand(interp, "receive", Tcp_ReceiveCmd,
		      (ClientData) NULL, (void (*) ()) NULL);
    Tcl_CreateCommand(interp, "packetSend", Tcp_PacketSendCmd,
		      (ClientData) NULL, (void (*) ()) NULL);
    Tcl_CreateCommand(interp, "packetReceive", Tcp_PacketReceiveCmd,
		      (ClientData) NULL, (void (*) ()) NULL);
    Tcl_CreateCommand(interp, "sendTo", Tcp_SendToCmd,
		      (ClientData) NULL, (void (*) ()) NULL);
    Tcl_CreateCommand(interp, "receiveFrom", Tcp_ReceiveFromCmd,
		      (ClientData) NULL, (void (*) ()) NULL);
}
