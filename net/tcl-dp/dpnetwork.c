/*
 * dpnetwork.c --
 *
 *	This file implements most of the network connection management
 *	functions of Tcl-DP.  The following comments are inherited from
 *	the progenitors of Tcl-DP.
 *
 * 	This file contains a simple Tcl "dp_connect" command
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
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */
#include <stdio.h>
#include <assert.h>
#include "tk.h"
#include "dpInt.h"
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#include <sys/un.h>
#include <sys/param.h>
#include <sys/ioctl.h>
#include <sys/uio.h>

#ifdef NO_ERRNO_H
int errno;
#define EINVAL 22
#else
#include <errno.h>
#endif

#define max(a,b)	((a)>(b)?(a):(b))
#define min(a,b)	((a)<(b)?(a):(b))
#ifndef	abs
# define abs(a)		((a)>0?(a):-(a))
#endif

/*
 * This is a "magic number" prepended to the beginning of the packet
 * It's used to help resync the packet machanism in the event of errors.
 */
#define PACKET_MAGIC	0x6feeddcc

static int  Tdp_inet_connect	_ANSI_ARGS_((char *host, int port,
					     int server, 
					     int udp));
static int  Tdp_unix_connect	_ANSI_ARGS_((char *path, 
					     int server,
                                             int udp));
static void Tdp_HandleEvent	_ANSI_ARGS_((ClientData clientData, 
					     int mask));

static void Tdp_FreeHandler	_ANSI_ARGS_((ClientData clientData));

/*
 * For every file descriptor handler created, a structure of 
 * the following type is maintained.
 */
typedef struct DP_FileHandle {
    Tcl_Interp *interp;
    FILE *filePtr;		/* Open file descriptor (file or socket) */
    int mask;			/* Mask of file descriptor conditions */
    char *rCmd;			/* Command to call on readable condition */
    char *wCmd;			/* Command to call on writable condition */
    char *eCmd;			/* Command to call on exception condition */
    char *fileId;		/* Represents filePtr */

} DP_FileHandle;

static DP_FileHandle *handlers[MAX_OPEN_FILES];	/* Indexed by fd. */

/*
 * We keep around a single, large buffer which we can receive data into.
 * The size of this buffer is the maximum size of any of the receive buffers
 * on any open sockets, stored in bufferSize.
 */
static char *buffer;				/* Buffer for receiving data */
static int bufferSize;				/* Size of buffer */

/*
 * For TCP, it's possible to get a line in pieces.  In case everything we
 * want isn't there (e.g., in dp_packetReceive), we need a place to store
 * partial results when we're in non-blocking mode or peeking at the data.
 * The partial buffers below are created dynamically to store incomplete
 * data in these cases.
 */
typedef struct PartialRead {
    char *buffer;		/* Buffer of characters */
    int bufSize;		/* Size of buffer */
    int offset;			/* Offset of current character within the buffer */
    struct PartialRead *next;	/* Next buffer in chain */
} PartialRead;

static PartialRead *partial[MAX_OPEN_FILES];

/*
 * The next array stores state about each socket.  The optFlags is an or'd
 * combination of the following state:
 *	FD_BLOCKING	-- Blocking I/O mode is on
 *	FD_GOTPARTIAL	-- Have received a partial message (only applicable for TCP)
 *	FD_TCP		-- Is a TCP/IP line (otherwise udp)
 *	FD_UNIX		-- Is a unix domain sokcet (otherwise internet)
 *	FD_SERVER	-- Was created with -server
 *	FD_AUTO_CLOSE	-- Socket should auto close on error.
 */
static unsigned char optFlags[MAX_OPEN_FILES];
#define	FD_BLOCKING	1
#define	FD_GOTPARTIAL	2
#define	FD_TCP		4
#define	FD_UNIX		8
#define	FD_SERVER	16
#define	FD_AUTO_CLOSE	32

/*
 *--------------------------------------------------------------
 *
 * Tdp_SetBlocking --
 *
 *	Make the socket blocking (or non-blocking) as specified,
 *	and be efficient about it (i.e., cache the current state).
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The socket whose file descriptor is passed in will be either
 *	blocking or not, as specified, after this call.
 *
 *--------------------------------------------------------------
 */
static void
Tdp_SetBlocking (fd, block)
    int fd;		/* File descriptor of socket */
    int block;		/* 1 if we should block from now on, 0 if not */
{
    int optval = !block;

    if (block) {
	if ((optFlags[fd] & FD_BLOCKING) == 0) {
	    ioctl (fd, FIONBIO, &optval);
	    optFlags[fd] |= FD_BLOCKING;
	}
    } else {
	if (optFlags[fd] & FD_BLOCKING) {
	    ioctl (fd, FIONBIO, &optval);
	    optFlags[fd] &= ~FD_BLOCKING;
	}
    }
}


/*
 *--------------------------------------------------------------
 *
 * Tdp_GetBufferSize --
 *
 *	Get the size of the receive buffer on a socket.
 *
 * Results:
 *	The size of the receive buffer of the specified socket, in bytes,
 *	or -1 on error.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */
static int
Tdp_GetBufferSize(fd)
    int fd;
{
    int optlen, optval, result;

    optlen = sizeof(int);
    result = getsockopt(fd, SOL_SOCKET, SO_RCVBUF, (char *)&optval, &optlen);
    if (result == -1) {
	return -1;
    } else {
	return optval;
    }
}

/*
 *------------------------------------------------------------------
 *
 * Tdp_MakeOpenFile --
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
Tdp_MakeOpenFile (interp, fd)
    Tcl_Interp *interp;
    int fd;
{
    FILE *fp;

    /*
     * Open the file with the correct type
     */
    fp = fdopen (fd, "r+");

    /*
     * Turn off buffering.  Otherwise, we run into nasty interaction
     * problems with gets/puts/read and our transmission commands below.
     */
    setbuf (fp, (char *) NULL);

    Tcl_EnterFile(interp, fp, TCL_FILE_READABLE|TCL_FILE_WRITABLE);
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_AllocateBuffer --
 *
 *	This command is called to allocate (or reallocate) the global
 *	receive buffer when the file descriptor passed in is created or
 *	modified.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The global variable "buffer" is (re)allocated
 *
 *--------------------------------------------------------------
 */
static void
Tdp_AllocateBuffer (fd)
    int fd;		/* File descriptor of socket created/modified */
{
    /*
     * Get the size of the send/receive buffer, and make sure the buffer
     * we have is big enough to receive the largest possible message.
     */
    if (buffer == NULL) {
	bufferSize = Tdp_GetBufferSize(fd) + 32;
	buffer = ckalloc(bufferSize);
    } else if (Tdp_GetBufferSize(fd) > bufferSize) {
	bufferSize = Tdp_GetBufferSize(fd) + 32;
	buffer = ckrealloc(buffer, bufferSize);
    }
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_FDIsReady --
 *
 *      This function determines if a file descriptor is readable
 *	or writeable.
 *
 * Results:
 *	An or'd combination of TCL_FILE_READABLE and TCL_FILE_WRITABLE
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */
int
Tdp_FDIsReady(fd)
    int fd;
{
    fd_set readFdset;
    fd_set writeFdset;
    struct timeval tv;
    int rv;

    FD_ZERO (&readFdset);
    FD_SET (fd, &readFdset);
    FD_ZERO (&writeFdset);
    FD_SET (fd, &writeFdset);

    tv.tv_sec = 0;
    tv.tv_usec = 0;

    select (fd + 1, &readFdset, &writeFdset, (SELECT_MASK *) NULL, &tv);
    if (FD_ISSET(fd, &readFdset)) {
	rv = TCL_FILE_READABLE;
    } else {
	rv = 0;
    }
    if (FD_ISSET(fd, &writeFdset)) {
	rv |= TCL_FILE_WRITABLE;
    }
    return rv;
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_FreeReadBuffer --
 *
 *	This function is called to free up all the memory associated
 *	with a file once the file is closed.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Any data buffered locally will be lost.
 *
 *--------------------------------------------------------------
 */
static void
Tdp_FreeReadBuffer(fd)
    int fd;
{
    PartialRead *readList;

    while (partial[fd] != NULL) {
	readList = partial[fd];
	partial[fd] = readList->next;
	ckfree (readList->buffer);
	ckfree (readList);
    }
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_Unread --
 *
 *	This function puts data back into the read chain on a
 *	file descriptor.  It's basically an extended "ungetc".
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Subsequent calls to Tdp_Read on the fd will get this data.
 *
 *--------------------------------------------------------------
 */
static void
Tdp_Unread (fd, buffer, numBytes, copy)
    int fd;                     /* File descriptor */
    char *buffer;               /* Data to unget */
    int numBytes;               /* Number of bytes to unget */
    int copy;			/* Should we copy the data, or use this buffer? */
{
    PartialRead *new;

    new = (PartialRead *)ckalloc (sizeof(PartialRead));
    if (copy) {
	new->buffer = ckalloc (numBytes);
	memcpy (new->buffer, buffer, numBytes);
    } else {
	new->buffer = buffer;
    }
    new->bufSize = numBytes;
    new->offset = 0;
    new->next = partial[fd];
    partial[fd] = new;
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_Read --
 *
 *	This function impplements a "recv"-like command, but
 *	buffers partial reads.  The semantics are the same as
 *	with recv.
 *
 * Results:
 *	Number of bytes read, or -1 on error (with errno set).
 *
 * Side effects:
 *	All available data is read from the file descriptor.
 *
 *--------------------------------------------------------------
 */
static int
Tdp_Read (fd, buffer, numReq, flags)
    int fd;			/* File descriptor to read from */
    char *buffer;		/* Place to put the data */
    int numReq;			/* Number of bytes to get */
    int flags;			/* Flags for receive */
{
    int peek;
    PartialRead *readList;
    PartialRead *tmp;
    int numRead;
    int numToCopy;

    readList = partial[fd];

    /*
     * If there's no data left over from a previous read, then just do a recv
     * This is the common case.
     */
    if (readList == NULL) {
	numRead = recv(fd, buffer, numReq, flags);
	return numRead;
    }

    /*
     * There's data left over from a previous read.  Yank it in and
     * only call recv() if we didn't get enough data (this keeps the fd
     * readable if they only request as much data as is in the buffers).
     */
    peek = flags & MSG_PEEK;
    numRead = 0;
    while ((readList != NULL) && (numRead < numReq)) {
	numToCopy = readList->bufSize - readList->offset;
	if (numToCopy + numRead > numReq) {
	    numToCopy = numReq - numRead;
	}
	memcpy (buffer+numRead, readList->buffer+readList->offset, numToCopy);

	/*
	 * Consume the data if we're not peeking at it
	 */
	tmp = readList;
	readList = readList->next;
	if (!peek) {
	    tmp->offset += numToCopy;
	    if (tmp->offset == tmp->bufSize) {
		ckfree (tmp->buffer);
		ckfree (tmp);
		partial[fd] = readList;
	    }
	}
	numRead += numToCopy;
    }

    /*
     * Only call recv if we reached the end of previously read data and they
     * didn't get enough and the fd has data to be consumed.
     */
    if ((numRead < numReq) && (Tdp_FDIsReady(fd) & TCL_FILE_READABLE)) {
	numToCopy = numReq - numRead;
	numRead += recv(fd, buffer+numRead, numToCopy, flags);
    }

    return numRead;
}

/*
 *------------------------------------------------------------------
 *
 * Tdp_ConnectCmd --
 *
 *      This procedure is the C interface to the "dp_connect"
 *      command. See the user documentation for a description.
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
Tdp_ConnectCmd(notUsed, interp, argc, argv)
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
    char tmp[256];

    pathname = NULL;
    host = NULL;
    udp = 0;
    server = 0;
    unixSocket = 0;

    if (argc < 2) {
error:
	Tcl_SetResult (interp, argv[0], TCL_VOLATILE);
	Tcl_AppendResult(interp, ": should be one of the forms:\n",
			 (char *) NULL);
	Tcl_AppendResult(interp, " \"dp_connect -server port\"\n",
			 (char *) NULL);
	Tcl_AppendResult(interp, " \"dp_connect host port\"\n",
			 (char *) NULL);
	Tcl_AppendResult(interp, " \"dp_connect -udp port\"\n",
			 (char *) NULL);
	Tcl_AppendResult(interp, " \"dp_connect -server path\"\n",
			 (char *) NULL);
	Tcl_AppendResult(interp, " or \"dp_connect path\"\n",
			 (char *) NULL);
	return TCL_ERROR;
    }

    /*
     * Break into one of three catergories:
     *	udp sockets,
     *	server setup
     *	client setup
     */
    if (strcmp (argv[1], "-udp") == 0) {
	udp = 1;
    } else if (strcmp (argv[1], "-server") == 0) {
	server = 1;
    }

    if (udp) {
	/*
	 * Must be "dp_connect -udp port"
	 */
	if (argc != 3) {
	    goto error;
	}
	host = "";
	if (Tcl_GetInt(interp, argv[2], &port) != TCL_OK) {
	    return TCL_ERROR;
	}
    } else if (server) {
	/*
	 * Must be either "dp_connect -server port"
	 * or "dp_connect -server path"
	 */
	if (argc != 3) {
	    goto error;
	}
	host = "";
	if (Tcl_GetInt(interp, argv[2], &port) != TCL_OK) {
	    pathname = argv[2];
	    unixSocket = 1;
	} 

    } else {
	/*
	 * Client setup. Must be one of:
	 *	"dp_connect host port" or
	 *	"dp_connect path"
	 */
	if (argc == 3) {
	    host = argv[1];
	    if (Tcl_GetInt(interp, argv[2], &port) != TCL_OK) {
		return TCL_ERROR;
	    }
	} else if (argc == 2) {
	    pathname = argv[1];
	    unixSocket = 1;
	} else {
	    goto error;
	}

    }

    /*
     * Create the connection
     */
    if (unixSocket) {
	fd = Tdp_unix_connect(pathname, server, udp);
    } else {
	fd = Tdp_inet_connect(host, port, server, udp);
    }

    if (fd < 0) {
	/* Tell them why it fell apart */
	if (unixSocket) {
	    if (server) {
		Tcl_AppendResult (interp, 
			"Couldn't setup listening socket with path \"",
			pathname, "\": ", Tcl_PosixError(interp),
			(char *)NULL);
	    } else {
		Tcl_AppendResult (interp, 
			"Couldn't connect to \"", pathname, "\": ",
			Tcl_PosixError(interp), (char *)NULL);
	    }
	} else if (server) {
	    if (port == 0) {
		Tcl_AppendResult (interp,
			"Couldn't setup listening socket on any port: ",
			Tcl_PosixError(interp), (char *)NULL);
	    } else {
		sprintf (tmp, "%d", port);
		Tcl_AppendResult (interp,
			"Couldn't setup listening socket on port ", tmp,
			": ", Tcl_PosixError(interp), (char *)NULL);
	    }
	} else {
	    sprintf (tmp, "%d", port);
	    Tcl_AppendResult (interp,
		    "Couldn't open connection to ", host, ":", tmp, " : ",
		    Tcl_PosixError(interp), (char *)NULL);
	}
	return TCL_ERROR;
    }

    Tdp_MakeOpenFile(interp, fd);

    /*
     * Clear up any leftover data that might not have been cleaned
     * up, just in case.
     */
    Tdp_FreeReadBuffer (fd);

    if (!unixSocket) {
        struct sockaddr_in sockaddr;
        int res, len;
	  
        /* Find the local port we're using for the connection. */
	  
        len = sizeof (sockaddr);
        res = getsockname (fd, (struct sockaddr *) &sockaddr, &len);
	  
        if (res < 0) {
	    sprintf (tmp, "file%d %d", fd, errno);
        } else  {
	    sprintf(tmp, "file%d %d", fd, ntohs(sockaddr.sin_port));
	}
    } else {
	sprintf(tmp, "file%d", fd);
    }
    Tcl_SetResult (interp, tmp, TCL_VOLATILE);

    Tdp_AllocateBuffer (fd);
    if (udp) {
	optFlags[fd] = FD_BLOCKING | FD_AUTO_CLOSE;
    } else {
	optFlags[fd] = FD_TCP | FD_BLOCKING | FD_AUTO_CLOSE;
    }
    if (unixSocket) {
	optFlags[fd] |= FD_UNIX;
    }
    if (server) {
	optFlags[fd] |= FD_SERVER;
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_SocketOptionCmd --
 *
 *	This function implements the tcl "dp_socketOption" command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard tcl result.
 *
 * Side effects:
 *	The system level properties of the socket may be changed
 *	by this call.
 *
 *--------------------------------------------------------------
 */
            /* ARGSUSED */
int
Tdp_SocketOptionCmd (clientData, interp, argc, argv)
    ClientData *clientData;         /* Often ignored */
    Tcl_Interp *interp;             /* tcl interpreter */
    int argc;                       /* Number of arguments */
    char *argv[];                   /* Arg list */
{
    char c;
    int len;
    FILE *filePtr;
    int fd;
    int optname;
    int optval;
    int result;
    int optlen;
    char tmp[256];

    optlen = sizeof(int);
    if ((argc != 3) && (argc != 4)) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			" socket option ?value?\"", (char *) NULL);
	return TCL_ERROR;
    }

    if (Tcl_GetOpenFile (interp, argv[1], 0, 0, &filePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    fd = fileno (filePtr);

    c = tolower(argv[2][0]);
    len = strlen(argv[2]);

    /* ------------------------ SEND BUFFER ---------------------------- */
    if ((c == 's') && (strncasecmp(argv[2], "sendBuffer", len) == 0)) {
	optname = SO_SNDBUF;
	if (argc == 4) {
	    if (Tcl_GetInt(interp, argv[3], &optval) != TCL_OK) {
		return TCL_ERROR;
	    }
	}

    /* ------------------------ RECV BUFFER ---------------------------- */
    } else if ((c == 'r') && (strncasecmp(argv[2], "recvBuffer", len) == 0)) {
	optname = SO_RCVBUF;
	if (argc == 4) {
	    if (Tcl_GetInt(interp, argv[3], &optval) != TCL_OK) {
		return TCL_ERROR;
	    }
	}

    /* ------------------------ NON BLOCKING --------------------------- */
    } else if ((c == 'n') && (strncasecmp(argv[2], "noblock", len) == 0)) {
	if (argc == 3) {
	    sprintf (tmp, "%s", ((optFlags[fd]&FD_BLOCKING)?"no":"yes"));
	    Tcl_SetResult(interp, tmp, TCL_VOLATILE);
	} else {
	    if (strcmp (argv[3], "yes") == 0) {
		Tdp_SetBlocking (fd, 0);
	    } else if (strcmp (argv[3], "no") == 0) {
		Tdp_SetBlocking (fd, 1);
	    } else {
		Tcl_AppendResult (interp, argv[0], ": Bad value \"",
				  argv[3], "\"", NULL);
		return TCL_ERROR;
	    }
	}
	return TCL_OK;

    /* ------------------------ KEEP ALIVE ----------------------------- */
    } else if ((c == 'k') && (strncasecmp(argv[2], "keepAlive", len) == 0)) {
	optname = SO_KEEPALIVE;
	if (argc == 4) {
	    if (strcmp (argv[3], "yes") == 0) {
		optval = 1;
	    } else if (strcmp (argv[3], "no") == 0) {
		optval = 0;
	    } else {
		Tcl_AppendResult (interp, argv[0], ": Bad value \"",
				  argv[3], "\"", NULL);
		return TCL_ERROR;
	    }
	}

    /* ------------------------ AUTO CLOSE ----------------------------- */
    } else if ((c == 'a') && (strncasecmp(argv[2], "autoClose", len) == 0)) {
	if (argc == 3) {
	    sprintf (tmp, "%s", ((optFlags[fd]&FD_AUTO_CLOSE)?"yes":"no"));
	    Tcl_SetResult(interp, tmp, TCL_VOLATILE);
	} else {
	    if (strcmp (argv[3], "yes") == 0) {
		optFlags[fd] |= FD_AUTO_CLOSE;
	    } else if (strcmp (argv[3], "no") == 0) {
		optFlags[fd] &= ~FD_AUTO_CLOSE;
	    } else {
		Tcl_AppendResult (interp, argv[0], ": Bad value \"",
				  argv[3], "\"", NULL);
		return TCL_ERROR;
	    }
	}
	return TCL_OK;

    /* ------------------------ ERROR ---------------------------------- */
    } else {
	Tcl_AppendResult(interp, argv[0], "unknown option \"",
			 argv[2], "\"", (char *) NULL);
	return TCL_ERROR;
    }

    if (argc == 4) {
	result = setsockopt(fd, SOL_SOCKET, optname, (char *)&optval, optlen);
    } else {
	result = getsockopt(fd, SOL_SOCKET, optname, (char *)&optval, &optlen);
    }

    if (result == -1) {
	Tcl_AppendResult(interp, argv[0], ": ", Tcl_PosixError(interp),
			 (char *) NULL);
	return TCL_ERROR;
    }

    if (optname == SO_KEEPALIVE) {
	sprintf (tmp, "%s", optval?"yes":"no");
    } else {
	sprintf (tmp, "%d", optval);
    }
    Tcl_SetResult(interp, tmp, TCL_VOLATILE);

    if ((optname == SO_RCVBUF) && (optval > bufferSize)) {
	Tdp_AllocateBuffer (fd);
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_FindFileHandler --
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

#define	Tdp_FindFileHandler(fd)			\
	(((fd) < 0 || (fd) >= MAX_OPEN_FILES)	\
	 ? ((DP_FileHandle *)NULL)		\
	 : handlers[fd])

/*
 *------------------------------------------------------------------
 *
 * Tdp_ShutdownCmd --
 *
 *      This procedure is the C interface to the "dp_shutdown"
 *      command. See the user documentation for a description.
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
Tdp_ShutdownCmd(notUsed, interp, argc, argv)
    ClientData notUsed;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    FILE *filePtr;
    int fd;
    DP_FileHandle *handler;
    int permissions;

    /*
     * Check args, find file
     */
    if (argc != 3) {
wrong_args:
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " fileid <option>\"", (char *) NULL);
	return TCL_ERROR;
    }

    if (Tcl_GetOpenFile(interp, argv[1], 0, 0, &filePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    fd = fileno (filePtr);
    permissions = Tcl_FilePermissions(filePtr);
    if (permissions == -1) {
	Tcl_AppendResult(interp, "unable to determine access for socket \"",
			 argv[1], "\"", (char *)NULL);
	return TCL_ERROR;
    }
    handler = Tdp_FindFileHandler (fd);

    /*
     * Call shutdown with correct args, update file handler
     */
    if (!strcmp(argv[2], "0") ||
	!strcmp(argv[2], "receives") ||
	!strcmp(argv[2], "read")) {
	if ((permissions & TCL_FILE_READABLE) == 0) {
	    Tcl_AppendResult(interp, "File is not readable", (char *) NULL);
	    return TCL_ERROR;
	}
	if (shutdown(fd, 0)) {
	    Tcl_AppendResult(interp, "shutdown: ", Tcl_PosixError(interp),
			     (char *) NULL);
	    return TCL_ERROR;
	}
	permissions &= ~TCL_FILE_READABLE;
    } else if (!strcmp(argv[2], "1") ||
	       !strcmp(argv[2], "sends") ||
	       !strcmp(argv[2], "write")) {
	if ((permissions & TCL_FILE_WRITABLE) == 0) {
	    Tcl_AppendResult(interp, "File is not writable", (char *) NULL);
	    return TCL_ERROR;
	}
	if (shutdown(fd, 1)) {
	    Tcl_AppendResult(interp, "shutdown: ", Tcl_PosixError(interp),
			     (char *) NULL);
	    return TCL_ERROR;
	}
	permissions &= ~TCL_FILE_WRITABLE;
    } else if (!strcmp(argv[2], "2") ||
	       !strcmp(argv[2], "all") ||
	       !strcmp(argv[2], "both")) {
	if (shutdown(fd, 2)) {
	    Tcl_AppendResult(interp, "shutdown: ", Tcl_PosixError(interp),
			     (char *) NULL);
	    return TCL_ERROR;
	}
	permissions = 0;
    } else {
	goto wrong_args;
    }
    Tcl_EnterFile(interp, filePtr, permissions);

    /*
     * Update the handler, freeing it if it's dead.
     */
    if (handler) {
	if (((permissions & TCL_FILE_READABLE) == 0) &&
	    (handler->rCmd != NULL)) {
	    ckfree(handler->rCmd);
	    handler->rCmd = NULL;
	}
	if (((permissions & TCL_FILE_WRITABLE) == 0) &&
	    (handler->wCmd != NULL)) {
	    ckfree(handler->wCmd);
	    handler->wCmd = NULL;
	}
	if ((permissions&(TCL_FILE_READABLE|TCL_FILE_WRITABLE)) == 0) {
	    if (handler->eCmd != NULL) {
		ckfree(handler->eCmd);
		handler->eCmd = NULL;
	    }

	    /*
	     * Delete handler.
	     */
	    Tk_DeleteFileHandler (fd);

	    handlers[fd] = (DP_FileHandle *) NULL;
	    Tk_EventuallyFree((ClientData) handler,
			      (Tk_FreeProc *)Tdp_FreeHandler);
	}
    }
    return TCL_OK;
}

/*
 *------------------------------------------------------------------
 *
 * Tdp_AcceptCmd --
 *
 *      This procedure is the C interface to the "dp_accept"
 *      command. See the user documentation for a description.
 *	It accepts a connection on a listening socket.
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
Tdp_AcceptCmd(notUsed, interp, argc, argv)
    ClientData notUsed;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    struct sockaddr_in sockaddr;
    int len = sizeof sockaddr;
    FILE *filePtr;
    int fd, fd1;
    int addr, f1, f2, f3, f4;
    char tmp[128];

    if (argc != 2) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " listening_socket\"", (char *) NULL);
	return TCL_ERROR;
    }

    if (Tcl_GetOpenFile (interp, argv[1], 0, 1, &filePtr) != TCL_OK) {
	return TCL_ERROR;
    }

    fd1 = fileno (filePtr);
    if (optFlags[fd1] & FD_SERVER) {
	fd = accept (fd1, (struct sockaddr *) &sockaddr, &len);
    } else {
	Tcl_AppendResult (interp, argv[0], ": must be a server socket", NULL);
	return TCL_ERROR;
    }

    if (fd < 0) {
	Tcl_AppendResult (interp, "accept: ", strerror (errno), (char *) NULL);
	return TCL_ERROR;
    }

    /*
     * Create the fileId structure.
     */
    Tdp_MakeOpenFile (interp, fd);
    if (sockaddr.sin_family == AF_INET) {
      addr = htonl(sockaddr.sin_addr.s_addr);
      f1 = (addr >> 24) & 0xff;
      f2 = (addr >> 16) & 0xff;
      f3 = (addr >> 8) & 0xff;
      f4 = addr & 0xff;
    } else {
      f1 = f2 = f3 = f4 = 0;
    }

    sprintf (tmp, "file%d %d.%d.%d.%d", fd, f1, f2, f3, f4);
    Tcl_SetResult(interp, tmp, TCL_VOLATILE);

    Tdp_AllocateBuffer(fd);
    optFlags[fd] = FD_AUTO_CLOSE | FD_TCP |
		   FD_BLOCKING | (optFlags[fd1] & FD_UNIX);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------
 *
 * Tdp_unix_connect --
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
Tdp_unix_connect(path, server, udp)
    char *path;			/* Path name to create or use */
    int server;			/* 1->make server, 0->connect to server */
    int udp;			/* Make it a udp protocol socket */
{
    struct sockaddr_un sockaddr;
    int sock, status;

    if (udp) {
	sock = socket(PF_UNIX, SOCK_DGRAM, 0);
    } else {
	sock = socket(PF_UNIX, SOCK_STREAM, 0);
    }
    if (sock < 0) {
	return -1;
    }
    memset((char *) &sockaddr, 0, sizeof(sockaddr));
    sockaddr.sun_family = AF_UNIX;
    strncpy(sockaddr.sun_path, path, sizeof(sockaddr.sun_path) - 1);

    /* Just in case addr is too long... */
    sockaddr.sun_path[sizeof(sockaddr.sun_path) - 1] = 0;

    if (server | udp) {
	status = bind(sock, (struct sockaddr *)&sockaddr, sizeof(sockaddr));
    } else {
	status = connect(sock, (struct sockaddr *)&sockaddr, sizeof(sockaddr));
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
 * Tdp_inet_connect --
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
Tdp_inet_connect(host, port, server, udp)
    char *host;			/* Host to connect, name or IP address */
    int port;			/* Port number to use */
    int server;			/* 1->make server, 0->connect to server */
    int udp;			/* Make it a udp protocol socket */
{
    struct hostent *hostent, _hostent;
    struct sockaddr_in sockaddr;
    int sock, status;
    int hostaddr, hostaddrPtr[2];
    char localhost[MAXHOSTNAMELEN];

    if (host == NULL) {
        gethostname(localhost,MAXHOSTNAMELEN);
        host = localhost;
    }

    hostent = gethostbyname(host);
    if (hostent == NULL) {
	hostaddr = inet_addr(host);
	if (hostaddr == -1) {
	    if (server && !strlen(host)) {
		hostaddr = INADDR_ANY;
	    } else {
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
    memset((char *)&sockaddr, 0,sizeof(sockaddr));
    sockaddr.sin_family = AF_INET;
    memcpy((char *)&(sockaddr.sin_addr.s_addr),
	   (char *)hostent->h_addr_list[0],
	   (size_t)hostent->h_length);
    sockaddr.sin_port = htons(port);

    if (server | udp) {
	status = bind(sock, (struct sockaddr *)&sockaddr, sizeof(sockaddr));
    } else {
	status = connect(sock, (struct sockaddr *)&sockaddr, sizeof(sockaddr));
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
 *--------------------------------------------------------------
 *
 * Tdp_IsReadyCmd --
 *
 *	This procedure implements the "dp_isreadable" function, which
 *	returns whether a file has input pending.
 *
 * Results:
 *	A standard tcl result.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */
 /* ARGSUSED */
int
Tdp_IsReadyCmd(clientData, interp, argc, argv)
    ClientData clientData;   /* Ignored */
    Tcl_Interp *interp;               /* Tcl interpreter */
    int argc;                 /* Number of arguments */
    char *argv[];             /* Arg list */
{
    FILE *filePtr;
    int state, readable, writeable;
    char tmp[32];

    if (argc != 2)
      goto syntaxError;

    if (Tcl_GetOpenFile(interp, argv[1], 0, 0, &filePtr) != TCL_OK) {
        return TCL_ERROR;
    }

    state = Tdp_FDIsReady(fileno(filePtr));
    readable = (state & TCL_FILE_READABLE) != 0;
    writeable = (state & TCL_FILE_WRITABLE) != 0;
    sprintf (tmp,"%d %d", readable, writeable);
    Tcl_SetResult(interp, tmp, TCL_VOLATILE);
    return TCL_OK;

  syntaxError:
    Tcl_AppendResult (interp, "wrong # args: should be \"",
			       argv[0], " fileId\"", (char *) NULL);
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------
 *
 * Tdp_FileHandlerCmd --
 *
 *      This procedure is the C interface to the "dp_filehandler"
 *      command. See the user documentation for a description.
 * 	Register a file handler with an open fileId.  If there is
 *	already an existing handler, it will be no longer called.
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
Tdp_FileHandlerCmd(notUsed, interp, argc, argv)
    ClientData notUsed;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    FILE *filePtr;
    int fd, mask;
    DP_FileHandle *handler;

    /*
     * Checks args.
     */
    if (argc != 2 && argc != 4) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " fileId ?mode command?\"", (char *) NULL);
	return TCL_ERROR;
    }

    if (Tcl_GetOpenFile (interp, argv[1], 0, 0, &filePtr) != TCL_OK) {
	return TCL_ERROR;
    }

    fd = fileno (filePtr);
    assert(fd < MAX_OPEN_FILES);

    if ((handler = Tdp_FindFileHandler (fd)) != NULL) {
	handlers[fd] = (DP_FileHandle *) NULL;
	Tk_EventuallyFree((ClientData)handler,
			  (Tk_FreeProc *)Tdp_FreeHandler);
	handler = NULL;
    }

    Tk_DeleteFileHandler (fd);
    if (argc == 2)  {
        return TCL_OK;
    }

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
    handler = (DP_FileHandle *) ckalloc (sizeof (DP_FileHandle));
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
	handler->rCmd = ckalloc(strlen(argv[3]) + 1);
	strcpy(handler->rCmd, argv[3]);
    }
    if (mask & TK_WRITABLE) {
	handler->wCmd = ckalloc(strlen(argv[3]) + 1);
	strcpy(handler->wCmd, argv[3]);
    }
    if (mask & TK_EXCEPTION) {
	handler->eCmd = ckalloc(strlen(argv[3]) + 1);
	strcpy(handler->eCmd, argv[3]);
    }

    handler->mask = mask;

    /*
     * Finally, get Tk to call Tdp_HandleEvent whenever there is a
     * file descriptor condition.
     */
#ifdef TK_EXTENDED
    Tk_CreateFileHandler (fd, (FILE *) NULL, mask, 
			  (Tk_FileProc *)Tdp_HandleEvent, (ClientData) handler);
#else
    Tk_CreateFileHandler (fd, mask, (Tk_FileProc *)Tdp_HandleEvent,
			  (ClientData) handler);
#endif

    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_FreeHandler --
 *
 *	Free up a file handler and all it's parts.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */
static void
Tdp_FreeHandler(clientData)
    ClientData clientData;
{
    DP_FileHandle *handler = (DP_FileHandle *)clientData;

    if (handler->rCmd != NULL) {
	ckfree(handler->rCmd);
    }
    if (handler->wCmd != NULL) {
	ckfree(handler->wCmd);
    }
    if (handler->eCmd != NULL) {
	ckfree(handler->eCmd);
    }
    if (handler->fileId != NULL) {
	ckfree((char *)handler->fileId);
    }
    ckfree ((char *)handler);
}

/*
 *----------------------------------------------------------------
 *
 * Tdp_HandleEvent --
 *
 * 	This procedure is called from Tk_DoOneEvent whenever there is
 *	a file descriptor condition on a given file descriptor.  It is
 *	installed by the "dp_filehandler" command.  A Tcl command
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
Tdp_HandleEvent(clientData, mask)
    ClientData clientData;
    int mask;
{
    int result;
    DP_FileHandle *handler = (DP_FileHandle *) clientData;
    Tcl_Interp *interp;
    FILE *filePtr;
    FILE *dummy;
    int fd;
    int delete;

    if (!handler)
      return;

    interp = handler->interp;
    filePtr = handler->filePtr;
    if ((interp == NULL) || (filePtr == NULL)) {
        return;
    }

    Tk_Preserve ((ClientData) handler);

    delete = 0;
    if (Tcl_GetOpenFile (interp, handler->fileId, 0, 0, &dummy) != TCL_OK) {
	/*  File descriptor is closed. */
	Tcl_ResetResult (interp);
	delete = 1;
    } else {
        Tcl_DString	cmd;
	assert (dummy == handler->filePtr);
	Tcl_DStringInit(&cmd);

	if (mask & TK_EXCEPTION) {
	    if (handler->eCmd != NULL) {
		Tcl_DStringAppend(&cmd, handler->eCmd, -1);
		Tcl_DStringAppend(&cmd, " e ", 3);
		Tcl_DStringAppend(&cmd, handler->fileId, -1);
		result = Tcl_GlobalEval(interp, Tcl_DStringValue(&cmd));
		Tcl_DStringFree(&cmd);
		if (result != TCL_OK) {
		    goto close;
		}
	    } else {
		goto close;
	    }
	} else if ((mask & TK_READABLE) && (handler->rCmd != NULL)) {
	    Tcl_DStringAppend(&cmd, handler->rCmd, -1);
	    Tcl_DStringAppend(&cmd, " r ", 3);
	    Tcl_DStringAppend(&cmd, handler->fileId, -1);
	    result = Tcl_GlobalEval(interp, Tcl_DStringValue(&cmd));
	    Tcl_DStringFree(&cmd);
	    if (result != TCL_OK) {
		Tk_BackgroundError(interp);
	    }
	} else if ((mask & TK_WRITABLE) && (handler->wCmd != NULL)) {
	    Tcl_DStringAppend(&cmd, handler->wCmd, -1);
	    Tcl_DStringAppend(&cmd, " w ", 3);
	    Tcl_DStringAppend(&cmd, handler->fileId, -1);
	    result = Tcl_GlobalEval(interp, Tcl_DStringValue(&cmd));
	    Tcl_DStringFree(&cmd);
	    if (result != TCL_OK) {
		Tk_BackgroundError(interp);
	    }
	} else if (feof(filePtr) || ferror(filePtr)) {
	    close:
	    if (Tcl_VarEval (interp, "close ", handler->fileId,
			     (char *) NULL) != TCL_OK)
		Tcl_AppendResult (interp, "Unexpected EOF on ",
				 handler->fileId, (char *) NULL);
	        Tk_BackgroundError (interp);

	    delete = 1;
	}
    }

    Tk_Release ((ClientData) handler);

    if (delete) {
        fd = fileno (filePtr);
        assert(fd < MAX_OPEN_FILES);

        if ((handler = Tdp_FindFileHandler (fd)) != NULL) {
	    handlers[fd] = (DP_FileHandle *) NULL;
	    Tk_EventuallyFree((ClientData)handler,
			      (Tk_FreeProc *)Tdp_FreeHandler);
	}
        Tk_DeleteFileHandler (fd);
    }
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_ReceiveCmd --
 *
 *      This procedure is invoked to process the "dp_receive" Tcl/Tk
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
Tdp_ReceiveCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Ignored */
    Tcl_Interp *interp;		/* Tcl interpreter */
    int argc;			/* Number of arguments */
    char *argv[];		/* Arg list */
{
    FILE *filePtr;
    int count;
    int fd, flags;
    int i, len;
    char c;

    if ((argc < 2) || (argc > 5)) {
	goto syntaxError;
    }

    if (Tcl_GetOpenFile(interp, argv[1], 0, 1, &filePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    fd = fileno(filePtr);

    /* 
     * Make sure this is a non-server TCP socket
     */
    if ((optFlags[fd] & FD_TCP) == 0) {
	Tcl_AppendResult(interp, "can't use ", argv[0], " on non TCP socket",
			 (char *) NULL);
	return TCL_ERROR;
    }

    if ((optFlags[fd] & FD_SERVER) != 0) {
	Tcl_AppendResult(interp, "can't use ", argv[0], " on server socket",
			 (char *) NULL);
	return TCL_ERROR;
    }

    /*
     * Get the extra parameters, if specified
     */
    count = bufferSize;
    flags = 0;
    for (i=2; i<argc; i++) {
	len = strlen (argv[i]);
	c = argv[i][1];
	if ((c == 'p') && (strncmp(argv[i], "-peek", len) != 0)) {
	    flags |= MSG_PEEK;
	} else if (Tcl_GetInt(interp, argv[i], &count) != TCL_OK) {
	    goto syntaxError;
	}
    }

    /*
     * Read the message into the global buffer and put on trailing
     * 0 at end of string in case we received a partial message.
     */
    count = Tdp_Read(fd, buffer, count, flags);
    Tcl_ResetResult(interp);
    if (count == -1) {
	/*
	 * If the file is in non-blocking mode, return null string
	 */
	if (errno == EWOULDBLOCK || errno == EAGAIN) {
	    return TCL_OK;
	} else {
	    Tcl_AppendResult(interp, "error reading ", argv[1],  ": ",
		    Tcl_PosixError(interp), (char *) NULL);
	    return TCL_ERROR;
	}
    }

    /*
     * If we get an eof, the connection is closed and we
     * should do some cleanup.
     */
    if (count == 0) {
	if (optFlags[fd] & FD_AUTO_CLOSE) {
	    Tdp_CleanupFile(interp, argv[1], fd);
	    Tcl_ResetResult(interp);
	    return TCL_OK;
	} else {
	    Tcl_AppendResult(interp,
			"error reading socket (connection closed) ",
			argv[1], (char *) NULL);
	    return TCL_ERROR;
	}
    }

    /*
     * Ok, we got what we got.  Return it.
     */
    buffer[count] = 0;
    Tcl_SetResult(interp, buffer, TCL_STATIC);
    return TCL_OK;

  syntaxError:
    Tcl_AppendResult (interp,
                      "syntax error: should be \"", argv[0],
                      " fileId ?numBytes? ?-peek?\"",
                      (char *) NULL);
    return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_CleanupFile --
 *
 *	Clean up a socket on error.
 *
 * Results:
 *	None
 *
 * Side effects:
 *	Will close the file and remove the handler if auto close
 *	is on.  This is the default action.
 *
 *--------------------------------------------------------------
 */
void
Tdp_CleanupFile(interp, file, fd)
    Tcl_Interp *interp;
    char *file;
    int fd;
{
    if (optFlags[fd] & FD_AUTO_CLOSE) {
	Tcl_VarEval(interp, "dp_filehandler ", file, (char *) NULL);
	Tcl_VarEval(interp, "close ", file, (char *) NULL);
    }
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_SendCmd --
 *
 *      This procedure is invoked to process the "dp_send" Tcl/Tk
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
Tdp_SendCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Ignored */
    Tcl_Interp *interp;		/* Tcl interpreter */
    int argc;			/* Number of arguments */
    char *argv[];		/* Arg list */
{
    FILE *filePtr;
    int count;
    int newline;
    int fd;
    char tmp[256];

    if ((argc < 3) || (argc > 4)) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " fileId string ?nonewline?\"", (char *) NULL);
	return TCL_ERROR;
    }

    if (Tcl_GetOpenFile(interp, argv[1], 1, 1, &filePtr) != TCL_OK) {
	return TCL_ERROR;
    }
    fd = fileno(filePtr);

    /* 
     * Make sure this is a non-server TCP socket
     */
    if ((optFlags[fd] & FD_TCP) == 0) {
	Tcl_AppendResult(interp, "can't use ", argv[0], " on non TCP socket",
			 (char *) NULL);
	return TCL_ERROR;
    }

    if ((optFlags[fd] & FD_SERVER) != 0) {
	Tcl_AppendResult(interp, "can't use ", argv[0], " on server socket",
			 (char *) NULL);
	return TCL_ERROR;
    }

    newline = 1;
    if (argc == 4) {
	if (strncmp(argv[3], "nonewline", strlen(argv[3])) != 0) {
	    Tcl_AppendResult(interp, "bad argument \"", argv[3],
			     "\": should be \"nonewline\"",  (char *) NULL);
	    return TCL_ERROR;
	}
	newline = 0;
    }

    {
      struct iovec	iov[2];
      register int	iovcnt = 1;
      iov[0].iov_len = strlen(argv[2]);
      iov[0].iov_base = argv[2];
      if (newline) {
	++iovcnt;
	iov[1].iov_len = 1;
	iov[1].iov_base = "\n";
      }
      /* Use writev to reduce number of kernel calls */
      count = writev(fd, iov, iovcnt);
    }
    if (count == -1) {
	if (errno == EPIPE) {
	    /*
	     * Got a broken pipe signal.  Close the file, delete the file
	     * handler, and return 0 bytes written.
	     */
	    Tdp_CleanupFile(interp, argv[1], fd);
	    Tcl_SetResult(interp, "0", TCL_STATIC);
	    return TCL_OK;
	}

	if (errno == EWOULDBLOCK || errno == EAGAIN) {
	    Tcl_SetResult(interp, "0", TCL_STATIC);
	    return TCL_OK;
	}

	Tcl_AppendResult(interp, "error writing ", argv[1],  ": ",
			 Tcl_PosixError(interp), (char *) NULL);
	return TCL_ERROR;
    }
    sprintf (tmp, "%d", count);
    Tcl_SetResult(interp, tmp, TCL_VOLATILE);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_PacketReceive --
 *
 *      This procedure is the C interface to the "dp_packetReceive"
 *      command. See the user documentation for a description.
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
Tdp_PacketReceive(interp, fileHandle, peek)
    Tcl_Interp *interp;		/* Tcl interpreter */
    char *fileHandle;
    int peek;
{
    FILE *filePtr;
    int numRead;
    int packetLen;
    int fd;
    int headerSize;
    int header[2];
    char *errMsg;
    int flags;

    if (Tcl_GetOpenFile(interp, fileHandle, 0, 1, &filePtr) != TCL_OK) {
      return TCL_ERROR;
    }

    fd = fileno(filePtr);
    assert(fd < MAX_OPEN_FILES);

    /* 
     * Make sure this is a non-server TCP socket
     */
    if ((optFlags[fd] & FD_TCP) == 0) {
	Tcl_AppendResult(interp, "can't use \"dp_packetReceive\" on non TCP socket",
			 (char *) NULL);
	return TCL_ERROR;
    }

    if ((optFlags[fd] & FD_SERVER) != 0) {
	Tcl_AppendResult(interp, "can't use \"dp_packetReceive\" on server socket",
			 (char *) NULL);
	return TCL_ERROR;
    }


    if (peek) {
	flags = MSG_PEEK;
    } else {
	flags = 0;
    }

    /*
     * Read in the header (2*sizeof(int))
     */
    headerSize = 2*sizeof(int);
    numRead = Tdp_Read (fd, (char *)header, headerSize, flags);

    if (numRead <= 0) {
	goto readError;
    }

    /*
     * Check for incomplete read.  If so, put it back (only if we consumed it!)
     * and return.
     */
    if (numRead < headerSize) {
	if (!peek) {
	    Tdp_Unread (fd, (char *)header, headerSize, 1);
	}
	Tcl_ResetResult(interp);
	return TCL_OK;
    }

    header[0] = ntohl(header[0]);
    header[1] = ntohl(header[1]);

    /*
     * Format of each packet:
     *
     *		First 4 bytes are PACKET_MAGIC.
     *		Next 4 bytes are packetLen.
     *		Next packetLen-headerSize is zero terminated string
     */
    if (header[0] != PACKET_MAGIC) {
        Tcl_AppendResult(interp, "Error reading ", fileHandle,
			 ": badly formatted packet", (char *) NULL);
	return TCL_ERROR;
    }
    packetLen = header[1] - headerSize;

    /*
     * Expand the size of the global buffer, as needed.
     */
    if (header[1] > bufferSize) {
	ckfree(buffer);
	bufferSize = header[1]+32;
	buffer = ckalloc(bufferSize);
    }

    /*
     * Read in the packet.  If it's only partially there, unread it and
     * return.  If we're peeking, we need to be careful since the header
     * is still in the queue.
     */
    if (peek) {
	numRead = Tdp_Read (fd, buffer, header[1], flags);
	if (numRead <= 0) {
	    goto readError;
	}

	/*
	 * Only partially there.  Return a null string.
	 */
	if (numRead != header[1]) {
	    Tcl_ResetResult(interp);
	    return TCL_OK;
	}

	buffer[numRead] = 0;
	Tcl_SetResult (interp, buffer+headerSize, TCL_STATIC);
	return TCL_OK;
    }

    /*
     * We're not peeking, so we've consumed the header (this is normal mode).
     * Read in the packet, and if it's not all there, put it back.
     */
    numRead = Tdp_Read (fd, buffer, packetLen, flags);
    if (numRead <= 0) {
	goto readError;
    }

    if (numRead != packetLen) {
	Tdp_Unread (fd, buffer, numRead, 1);
	Tdp_Unread (fd, (char *)header, headerSize, 1);
	Tcl_ResetResult(interp);
	return TCL_OK;
    }

    buffer[numRead] = 0;
    Tcl_SetResult(interp, buffer, TCL_STATIC);
    return TCL_OK;

readError:
    /*
     *
     * If we're in non-blocking mode, and this would block, return.
     * If the connection is closed (numRead == 0), don't return an
     * error message.  Otherwise, return one.
     *
     * In either case, we close the file, delete the file handler, and
     * return a null string.
     */

    if (errno == EWOULDBLOCK || errno == EAGAIN) {
	Tcl_ResetResult(interp);
	return TCL_OK;
    }

    /* Record the error before closing the file */
    if (numRead != 0) {
	errMsg = Tcl_PosixError (interp);
    } else {
	errMsg = NULL;	/* Suppresses spurious compiler warning */
    }

    /* 
     * Remove the file handler and close the file.  We want to go through
     * tcl in case the user has overridden the close procedure
     */
    Tdp_CleanupFile(interp, fileHandle, fd);
    Tdp_FreeReadBuffer(fd);

    Tcl_ResetResult(interp);
    if (numRead == 0) {
	return TCL_OK;
    } else {
	Tcl_AppendResult (interp, "Tdp_PacketReceive -- error reading ",
		  fileHandle, ": ", errMsg, (char *) NULL);
	return TCL_ERROR;
    }
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_PacketReceiveCmd --
 *
 *      This procedure is invoked to process the "dp_packetReceive" Tcl/Tk
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
Tdp_PacketReceiveCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Ignored */
    Tcl_Interp *interp;		/* Tcl interpreter */
    int argc;			/* Number of arguments */
    char *argv[];		/* Arg list */
{
    char *fileHandle;
    int len, peek;

    if ((argc < 2) || (argc > 3)) {
      syntaxError:
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " fileId ?-peek?", (char *) NULL);
	return TCL_ERROR;
    }

    fileHandle = argv[1];

    if (argc == 3) {
	len = strlen(argv[2]);
	if (strncmp(argv[2], "-peek", len) == 0) {
	    peek = 1;
	} else {
	    goto syntaxError;
	}
    } else {
	peek = 0;
    }
    return (Tdp_PacketReceive(interp, fileHandle, peek));
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_PacketSend --
 *
 *      This procedure is the C interface to the "dp_packetSend" command.
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
Tdp_PacketSend(interp, fileHandle, message)
    Tcl_Interp *interp;		/* Tcl interpreter */
    char *fileHandle;
    char *message;
{
    FILE *filePtr;
    int fd;
    int strLen;
    int packetLen;
    int numSent;
    int header[2];
    struct iovec iov[2];
    char tmp[256];

    if (Tcl_GetOpenFile(interp, fileHandle, 1, 1, &filePtr) != TCL_OK) {
	return TCL_ERROR;
    }

    fd = fileno (filePtr);

    /* 
     * Make sure this is a non-server TCP socket
     */
    if ((optFlags[fd] & FD_TCP) == 0) {
	Tcl_AppendResult(interp, "can't use \"dp_packetSend\" on non TCP socket",
			 (char *) NULL);
	return TCL_ERROR;
    }

    if ((optFlags[fd] & FD_SERVER) != 0) {
	Tcl_AppendResult(interp, "can't use \"dp_packetSend\" on server socket",
			 (char *) NULL);
	return TCL_ERROR;
    }


    /*
     * Format up the packet:
     *	  First 4 bytes are PACKET_MAGIC.
     *	  Next 4 bytes are packetLen.
     *	  Next packetLen-(sizeof(int)) bytes are zero terminated message.
     */
    strLen = strlen (message);
    packetLen = strLen + 2 * sizeof (int);

    header[0] = htonl (PACKET_MAGIC);
    header[1] = htonl (packetLen);

    /* Set up scatter/gather vector */
    iov[0].iov_len = 2*sizeof(int);
    iov[0].iov_base = (char *)header;
    iov[1].iov_len = strLen;
    iov[1].iov_base = message;

    /* Send it off, with error checking */
    numSent = writev (fd, iov, 2);
    if (numSent != packetLen) {

	if ((errno == 0) || (errno == EWOULDBLOCK || errno == EAGAIN)) {
	    /*
	     * Non-blocking I/O: return number of bytes actually sent.
	     */
	    Tcl_ResetResult(interp);
	    sprintf (tmp, "%d", numSent - 2*sizeof(int));
	    Tcl_SetResult(interp, tmp, TCL_VOLATILE);
	    return TCL_OK;
	} else if (errno == EPIPE) {
	    /*
	     * Got a broken pipe signal, which means the far end closed the
	     * connection.  Close the file, delete the file handler, and
	     * return 0 bytes sent.
	     */
	    Tdp_CleanupFile(interp, fileHandle, fd);
	    sprintf (tmp, "0");
	    Tcl_SetResult(interp, tmp, TCL_VOLATILE);
	    return TCL_OK;
	} else {
	    Tcl_AppendResult (interp, "Tdp_PacketSend -- error writing ",
			      fileHandle, ": ",
			      Tcl_PosixError (interp), (char *) NULL);
	}

	return TCL_ERROR;
    }

    /*
     * Return the number of bytes sent (minus the header).
     */
    sprintf (tmp, "%d", numSent - 2*sizeof(int));
    Tcl_SetResult(interp, tmp, TCL_VOLATILE);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_PacketSendCmd --
 *
 *      This procedure is invoked to process the "dp_packetSend" Tcl/Tk
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
Tdp_PacketSendCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Ignored */
    Tcl_Interp *interp;		/* Tcl interpreter */
    int argc;			/* Number of arguments */
    char *argv[];		/* Arg list */
{
    char *fileHandle;

    if (argc != 3) {
	Tcl_AppendResult (interp, "wrong # args: should be \"", argv[0],
			  " fileId string\"", (char *) NULL);
	return TCL_ERROR;
    }

    fileHandle = argv[1];

    return (Tdp_PacketSend (interp, fileHandle, argv[2]));
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_ReceiveFromCmd --
 *
 *      This procedure is invoked to process the "dp_receiveFrom" Tcl/Tk
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
Tdp_ReceiveFromCmd(notUsed, interp, argc, argv)
    ClientData notUsed;
    Tcl_Interp *interp;		/* Tcl interpreter */
    int argc;			/* Number of arguments */
    char *argv[];		/* Arg list */
{
    FILE *filePtr;
    int fd;
    int flags, numBytes;
    int len, i;
    char c;
    char *addrName;
    int count, addrLen;
    struct sockaddr_in addr;
    int noaddr;

    if ((argc < 2) || (argc > 4)) {
	goto syntaxError;
    }

    if (Tcl_GetOpenFile(interp, argv[1], 0, 1, &filePtr) != TCL_OK) {
	return TCL_ERROR;
    }

    /*  Parse flag parameters; */
    flags = 0;
    numBytes = bufferSize;
    noaddr = 0;
    for (i = 2; i < argc; i++) {
	len = strlen(argv[i]);
	c = argv[i][1];
	if ((c == 'p') && (strncmp(argv[i], "-peek", len) == 0)) {
	    flags |= MSG_PEEK;
	} else if ((c == 'n') && (strncmp(argv[i], "-noaddr", len) != 0)) {
	    noaddr = 1;
	} else if (Tcl_GetInt(interp, argv[i], &numBytes) != TCL_OK) {
	    goto syntaxError;
	}
    }

    addrLen = sizeof(addr);
    memset((char *) &addr, 0, addrLen);
    fd = fileno(filePtr);

    /* 
     * Make sure this is a UDP socket
     */
    if (optFlags[fd] & FD_TCP) {
	Tcl_AppendResult(interp, "can't use ", argv[0], " on a TCP socket",
			 (char *) NULL);
	return TCL_ERROR;
    }

    /*
     * Read the message and put on trailing 0 at end of string in case we
     * received a partial message.
     */
    count = recvfrom(fd, buffer, numBytes, flags,
		     (struct sockaddr *)&addr, &addrLen);
    if (count == -1) {
	Tcl_ResetResult(interp);
	if (errno == EWOULDBLOCK || errno == EAGAIN) {
	    return TCL_OK;
	}
	Tcl_AppendResult (interp, "error reading ", argv[1], ": ",
			  Tcl_PosixError (interp), (char *) NULL);

	return TCL_ERROR;
    }
    if (!noaddr) {
	addr.sin_port = ntohs(addr.sin_port);
	addrName = Tdp_CreateAddress(addr.sin_addr.s_addr, addr.sin_port);
	Tcl_SetResult(interp, addrName, TCL_STATIC);
	Tcl_AppendElement(interp, buffer);
    } else {
	Tcl_SetResult(interp, buffer, TCL_STATIC);
    }
    return TCL_OK;

  syntaxError:
    Tcl_AppendResult (interp,
                      "wrong # args: should be \"", argv[0],
                      " fileId ?numBytes? ?-peek?\"",
                      (char *) NULL);
    return TCL_ERROR;
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_SendToCmd --
 *
 *      This procedure is invoked to process the "dp_sendTo" Tcl/Tk
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
Tdp_SendToCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Ignored */
    Tcl_Interp *interp;		/* Tcl interpreter */
    int argc;			/* Number of arguments */
    char *argv[];		/* Arg list */
{
    FILE *filePtr;
    int fd, len, status;
    struct sockaddr_in *addrPtr;

    if (argc != 4) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			 " fileId string address\"", (char *) NULL);
	return TCL_ERROR;
    }
    if (Tcl_GetOpenFile(interp, argv[1], 1, 1, &filePtr) != TCL_OK) {
	return TCL_ERROR;
    }

    fd = fileno(filePtr);
    len = strlen(argv[2]) + 1;

    /* 
     * Make sure this is a UDP socket
     */
    if (optFlags[fd] & FD_TCP) {
	Tcl_AppendResult(interp, "can't use ", argv[0], " on a TCP socket",
			 (char *) NULL);
	return TCL_ERROR;
    }

    addrPtr = (struct sockaddr_in *)Tdp_FindAddr(argv[3]);
    if (addrPtr == NULL) {
	Tcl_AppendResult(interp, argv[0], ": invalid address \"", argv[3],
			 "\"", (char *) NULL);
	return TCL_ERROR;
    }

    status = sendto(fd, argv[2], len, 0, (struct sockaddr *)addrPtr, 
		    sizeof(struct sockaddr_in));
    if (status != len) {
	Tcl_AppendResult(interp, "error writing ", argv[1], ": ",
			 Tcl_PosixError(interp), (char *) NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_Tcp_Init -
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
Tdp_Tcp_Init(interp)
    Tcl_Interp *interp;		/* Tcl interpreter */
{
    Tcl_CreateCommand(interp, "dp_isready",
	(Tcl_CmdProc *)Tdp_IsReadyCmd,
	(ClientData) NULL, (void (*) _ANSI_ARGS_((ClientData))) NULL);
    Tcl_CreateCommand(interp, "dp_socketOption",
	(Tcl_CmdProc *)Tdp_SocketOptionCmd,
	(ClientData) NULL, (void (*) _ANSI_ARGS_((ClientData))) NULL);
    Tcl_CreateCommand(interp, "dp_connect",
	(Tcl_CmdProc *)Tdp_ConnectCmd,
	(ClientData) NULL, (void (*) _ANSI_ARGS_((ClientData))) NULL);
    Tcl_CreateCommand(interp, "dp_shutdown", 
	(Tcl_CmdProc *)Tdp_ShutdownCmd,
	(ClientData) NULL, (void (*) _ANSI_ARGS_((ClientData))) NULL);
    Tcl_CreateCommand(interp, "dp_accept", 
	(Tcl_CmdProc *)Tdp_AcceptCmd,
	(ClientData) NULL, (void (*) _ANSI_ARGS_((ClientData))) NULL);
    Tcl_CreateCommand(interp, "dp_filehandler", 
	(Tcl_CmdProc *)Tdp_FileHandlerCmd,
	(ClientData) NULL, (void (*) _ANSI_ARGS_((ClientData))) NULL);
    Tcl_CreateCommand(interp, "dp_send", 
	(Tcl_CmdProc *)Tdp_SendCmd,
	(ClientData) NULL, (void (*) _ANSI_ARGS_((ClientData))) NULL);
    Tcl_CreateCommand(interp, "dp_receive", 
	(Tcl_CmdProc *)Tdp_ReceiveCmd,
	(ClientData) NULL, (void (*) _ANSI_ARGS_((ClientData))) NULL);
    Tcl_CreateCommand(interp, "dp_packetSend", 
	(Tcl_CmdProc *)Tdp_PacketSendCmd,
	(ClientData) NULL, (void (*) _ANSI_ARGS_((ClientData))) NULL);
    Tcl_CreateCommand(interp, "dp_packetReceive", 
	(Tcl_CmdProc *)Tdp_PacketReceiveCmd,
	(ClientData) NULL, (void (*) _ANSI_ARGS_((ClientData))) NULL);
    Tcl_CreateCommand(interp, "dp_sendTo", 
	(Tcl_CmdProc *)Tdp_SendToCmd,
	(ClientData) NULL, (void (*) _ANSI_ARGS_((ClientData))) NULL);
    Tcl_CreateCommand(interp, "dp_receiveFrom", 
	(Tcl_CmdProc *)Tdp_ReceiveFromCmd,
	(ClientData) NULL, (void (*) _ANSI_ARGS_((ClientData))) NULL);
}
