/*
 * dprpc.c
 *
 *	This file partially implements the RPC commands of Tcl-DP.
 *	Some RPC commands of Tcl-DP are implemented in Tcl code.
 *	See "rpc.tcl" for more information.
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
#include <math.h>
#include <limits.h>

#include "tkInt.h"
#include "dpInt.h"

/* ---------------------------------------------------- */
/*
 *  The following tokens appear as the first characters
 *  in the RPC messages which are transmitted between processes.
 *
 */

#define TOK_RPC		'e'
#define TOK_RDO		'd'
#define TOK_RET		'r'
#define TOK_ERR		'x'

/* ---------------------------------------------------- */

/* The following elements are indexed by fd */
static int rpcWaiting[MAX_OPEN_FILES];	/* Is there an RPC out on this line? */
static char *rpcCheck[MAX_OPEN_FILES];	/* Command to verify command is legal */
static int rpcResult[MAX_OPEN_FILES];	/* Result of RPC (TCL_OK or TCL_ERROR)
				 	 * when it is received */
static char *rpcValue[MAX_OPEN_FILES];	/* interp->result for RPC */

static int rpcFlush[MAX_OPEN_FILES];	/* Number of messages to flush on RPC
				 	 * (because RPC was cancelled or
					 * timed out) */
static char *rpcTimeoutReturn[MAX_OPEN_FILES];	/* indexed by fd;	 */
static Tk_TimerToken timerToken[MAX_OPEN_FILES]; /* Timer token for timeout on this RPC */
static Tcl_Interp *timerInterp[MAX_OPEN_FILES]; /* Interp associated w/token */

/* ---------------------------------------------------- */


static int Tdp_ReceiveRPC_helper _ANSI_ARGS_((Tcl_Interp *, char *, char *,
					      int, int));
static int send_tagged_packet _ANSI_ARGS_((Tcl_Interp *, char *,
					   int, char *));

/* ---------------------------------------------------- */
/*
 * The following data structure is state maintained by the
 * RPC system for this interpreter.
 *
 */

typedef struct {
    char *rpcFile;		/* Current RPC connection */
    char *checkCmd;		/* Current check command */
} RPCInterpData;

/* ---------------------------------------------------- */
/*
 * The following data structure is state used to modify the
 * behaviour of commands.
 *
 */

typedef struct {
    Tcl_CmdInfo		cmdInfo;
    int			resCode;
    Tcl_DString		result;
    RPCInterpData *	rd;
} RPCCmdWrap;

static void Tdp_FreeRPCInterpData _ANSI_ARGS_((ClientData *clientData,
					      Tcl_Interp *interp));

static char * Tdp_RPCVarTrace _ANSI_ARGS_((ClientData clientData,
					  Tcl_Interp *interp,
					  char *name1, char *name2,
					  int flags));

static void Tdp_CommandTrace _ANSI_ARGS_((ClientData clientData,
					 Tcl_Interp *interp,
					 int level,
					 char *cmd, Tcl_CmdProc *cmdProc,
					 ClientData cmdClientData,
					 int argc, char *argv[]));

static int Tdp_CmdWrapProc _ANSI_ARGS_((ClientData clientData,
				       Tcl_Interp *interp,
				       int argc, char *argv[]));


/*
 *--------------------------------------------------------------
 *
 * Tdp_FreeRPCInterpData --
 *
 *	Internal routine to free up RPCInterpData associated with
 *	an interpreter; this is called when the interpreter is
 *	deleted.
 *
 * Results:
 *	None
 *
 * Side effects:
 *	The RPCInterpData passed in is free'd -- it should never
 *	be referenced again.
 *
 *--------------------------------------------------------------
 */
static void
Tdp_FreeRPCInterpData(clientData, interp)
     ClientData *	clientData;
     Tcl_Interp *	interp;
{
    register RPCInterpData *rd;
    
    rd = (RPCInterpData *)clientData;
    ckfree((char *)rd);
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_RPCVarTrace --
 *
 *	Trace on the rpcFile variable.  This ensures that the
 *	variable is readonly.  It also allows us to grab the
 *	RPC interpreter data pointer in routines that need it,
 *	where the Tcl-DP API only passes the interpreter.
 *
 * Results:
 *	Description of return values.
 *
 * Side effects:
 *	Global variables touched.
 *	I/O operations performed.
 *	Delayed effects.
 *
 *--------------------------------------------------------------
 */
static char *
Tdp_RPCVarTrace(clientData, interp, name1, name2, flags)
     ClientData		clientData;
     Tcl_Interp *	interp;
     char *		name1;
     char *		name2;
     int		flags;
{
    register RPCInterpData *	rd = (RPCInterpData *)clientData;

    if (!(flags & TCL_INTERP_DESTROYED)) {
	if (rd->rpcFile != (char *)NULL) {
	    (void) Tcl_SetVar(interp, "rpcFile", rd->rpcFile, TCL_GLOBAL_ONLY);
	} else {
	    (void) Tcl_UnsetVar(interp, "rpcFile", TCL_GLOBAL_ONLY);
	    flags |= TCL_TRACE_DESTROYED;
	}
	if (flags & TCL_TRACE_DESTROYED) {
	    Tcl_TraceVar(interp, "rpcFile",
		       TCL_GLOBAL_ONLY |TCL_TRACE_READS|
		       TCL_TRACE_WRITES|TCL_TRACE_UNSETS,
		       Tdp_RPCVarTrace, (ClientData)rd);
	}
	if (flags & (TCL_TRACE_WRITES|TCL_TRACE_UNSETS)) {
	    return "rpcFile is read-only";
	}
    }

    return (char *)NULL;
}


/*
 *--------------------------------------------------------------
 *
 * Tdp_CommandTrace:
 *	This procedure traces commands.  If there is currently an
 *	active "check" command, evaluate it with the current command
 *	appended to it.  (Of course, we don't recursively check!)
 *	If the check command throws TCL_RETURN, arrange for the
 *	command to return the value the check command returned;
 *	if the check command returns TCL_OK, run the command
 *	normally; otherwise, make sure the command returns an
 *	error.
 *
 *	This is complicated by the fact that the trace cannot
 *	actually indicate to the Tcl evaluator whether or not
 *	evaluation should procede; it can only arrange to change
 *	the command so that it behaves differently.
 *
 * Results:
 *	None
 *
 * Side Effects:
 *	Potentially any, since it can evaluate a Tcl command.
 *	Of particular interest is that it may "wrap" the command
 *	being traced to modify its execution behaviour.
 *
 *--------------------------------------------------------------
 */
static void
Tdp_CommandTrace(clientData, interp, level, cmd, cmdProc,
		 cmdClientData, argc, argv)
     ClientData		clientData;
     Tcl_Interp *	interp;
     int		level;
     char *		cmd;
     Tcl_CmdProc *	cmdProc;
     ClientData		cmdClientData;
     int		argc;
     char *		argv[];
{
    register RPCInterpData *rd;
    register char *checkCmd;
    Tcl_DString cc;

    rd = (RPCInterpData *)clientData;
    checkCmd = rd->checkCmd;
    if ((checkCmd != (char *)NULL) && (cmdProc != Tdp_CmdWrapProc)) {
	register int resCode;
	register RPCCmdWrap *wrap;
	Tcl_CmdInfo cmdInfo;
	char *mcmd;

	/*
	 * Run the check command.
	 */
	rd->checkCmd = (char *)NULL;
	Tcl_DStringInit(&cc);
	Tcl_DStringAppend(&cc, checkCmd, -1);
	Tcl_DStringAppend(&cc, " ", 1);
	mcmd = Tcl_Merge(argc, argv);
	Tcl_DStringAppend(&cc, mcmd, -1);
	ckfree((char *)mcmd);
	resCode = Tcl_GlobalEval(interp, Tcl_DStringValue(&cc));
	Tcl_DStringFree(&cc);
	rd->checkCmd = checkCmd;

	/*
	 * Build the wrapper.
	 */
	wrap = (RPCCmdWrap *)ckalloc(sizeof(RPCCmdWrap));

	/* %%% Really need to handle out-of-memory here. */
	Tcl_GetCommandInfo(interp, argv[0], &wrap->cmdInfo);
	Tcl_DStringInit(&wrap->result);
	wrap->rd = rd;
	wrap->resCode = resCode;
	if (resCode == TCL_RETURN || resCode == TCL_ERROR) {
	    Tcl_DStringAppend(&wrap->result, interp->result, -1);
	} else if (resCode != TCL_OK && resCode != TCL_CONTINUE) {
	    Tcl_DStringAppend(&wrap->result, "authorization to execute \"", -1);
	    Tcl_DStringAppend(&wrap->result, argv[0], -1);
	    Tcl_DStringAppend(&wrap->result, "\" is denied", -1);
	}
	cmdInfo.proc = Tdp_CmdWrapProc;
	cmdInfo.clientData = (ClientData)wrap;
	cmdInfo.deleteProc = NULL;
	cmdInfo.deleteData = NULL;
	Tcl_SetCommandInfo(interp, argv[0], &cmdInfo);
    }
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_CmdWrapProc --
 *
 *	
 *	This procedure is set up by the command trace to modify the
 *	behaviour of a command.  It will return either a predetermined
 *	value or an access error, while also removing itself so that
 *	future invocations of the command will work normally.
 *	functions are related to this one.
 *
 * Results:
 *	TCL_ERROR, if the trace determined an error should be signalled.
 *	TCL_OK, if the trace computed a result this should return to the
 *		interpreter.
 *
 * Side effects:
 *	Modifies the interpreter with a result string.
 *
 *--------------------------------------------------------------
 */
static int
Tdp_CmdWrapProc(clientData, interp, argc, argv)
    ClientData		clientData;
    Tcl_Interp *	interp;
    int			argc;
    char *		argv[];
{
    register RPCCmdWrap *	wrap = (RPCCmdWrap *)clientData;
    int			res;
    char *checkCmd;

    /*
     * Restore the command.
     */
    Tcl_SetCommandInfo(interp, argv[0], &wrap->cmdInfo);

    /*
     * Run the command, or return a result.
     */
    switch (wrap->resCode) {
    case TCL_OK:
	checkCmd = wrap->rd->checkCmd;
	wrap->rd->checkCmd = (char *)NULL;
	res = (*wrap->cmdInfo.proc)(wrap->cmdInfo.clientData,
				    interp, argc, argv);
	wrap->rd->checkCmd = checkCmd;
	break;

    case TCL_CONTINUE:
	res = (*wrap->cmdInfo.proc)(wrap->cmdInfo.clientData,
				    interp, argc, argv);
	break;

    case TCL_RETURN:
	res = TCL_OK;
	Tcl_DStringResult(interp, &wrap->result);
	break;

    default:
	res = TCL_ERROR;
	Tcl_DStringResult(interp, &wrap->result);
	break;

    }

    /*
     * Free up storage and return.
     */
    ckfree((char *)wrap);
    return res;
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_CancelRPC --
 *
 *	This procedure is invoked to force current RPC's that are
 *	waiting for results of remote evaluation to stop waiting
 *	and simply return an error.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	All waiting RPC's are cancelled.
 *
 *--------------------------------------------------------------
 */
static void
Tdp_CancelRPC (fd, errmsg)
    int fd;
    int errmsg;
{
    assert(fd < MAX_OPEN_FILES);

    rpcWaiting[fd] = 0;
    rpcResult[fd] = TCL_ERROR;
    rpcFlush[fd]++;
    if (timerToken[fd]) {
        Tk_DeleteTimerHandler(timerToken[fd]);
        timerToken[fd] = 0;
    }
    
    if (rpcValue[fd]) {
        ckfree((char *) rpcValue[fd]);
	rpcValue[fd] = NULL;
    }
    if (errmsg) {
	rpcValue[fd] = (char *)ckalloc(50);
	sprintf (rpcValue[fd], "RPC Cancelled on file%d", fd);
    }
    
    if (rpcTimeoutReturn[fd]) {
        ckfree((char *) rpcTimeoutReturn[fd]);
        rpcTimeoutReturn[fd] = NULL;
    }
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_CleanupRPC --
 *
 *	This function is invoked when an error occurs on a file.  It
 *	closes the file, deletes any filehandler associated with the
 *	file, and forces the RPC to return an error.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The file is closed.
 *
 *--------------------------------------------------------------
 */
static void
Tdp_CleanupRPC (interp, fd, close)
    Tcl_Interp *interp;
    int fd;
    int close;
{
    char temp[20];

    Tdp_CancelRPC (fd, 0);
    rpcFlush[fd] = 0;

    if (close) {
	sprintf(temp, "file%d", fd);
	Tdp_CleanupFile(interp, temp, fd);
    }
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_TimeoutHandler
 *
 *	This procedure is the callback of the Tk TimerHandler
 *	to process timeouts on RPC's.  On RPC's that have timed out,
 *	this callback will set their return value to TCL_ERROR.
 *
 * Results:
 *	Unused.
 *
 * Side effects:
 *	Processes timeouts on RPC's.  Will setup another Tk timer
 *	to handle RPC timeouts not yet occurred.  Affects
 *	the timerToken variable.
 *
 *--------------------------------------------------------------
 */
static void
Tdp_TimeoutHandler(clientData)
    ClientData clientData;		/* File descriptor. */
{
    int fd = (int)clientData;
    Tcl_Interp *interp = timerInterp[fd];

    /*
     * Record error, reset rpcWaiting flag, and mark
     * the return for flushing for this RPC.  This will
     * work because, if the far end never returns a value,
     * it is truly dead, and we'll never get anything ever
     * again on this socket.
     */
    timerToken[fd] = (Tk_TimerToken) 0;
    timerInterp[fd] = NULL;
    rpcResult[fd] = TCL_ERROR;
    rpcWaiting[fd] = 0;
    rpcFlush[fd]++;

    if (rpcTimeoutReturn[fd]) {
	/*
	 * Eval the callback for timeouts, if specified,
	 * and use that callback return value as the
	 * return value for this RPC;
	 */

	char *cmd;

	cmd = (char *) ckalloc(strlen(rpcTimeoutReturn[fd]) + 20);
	sprintf(cmd, "%s file%d", rpcTimeoutReturn[fd], fd);

	rpcResult[fd] = Tcl_GlobalEval(interp, cmd);
	ckfree(cmd);
	ckfree((char *) rpcTimeoutReturn[fd]);
	rpcTimeoutReturn[fd] = NULL;
    } else {
	char str[10];
	sprintf(str, "file%d", fd);
	Tcl_ResetResult (interp);
	Tcl_AppendResult (interp, "dp_RPC timed out on ", str, (char *)NULL);
    }

    if (rpcValue[fd]) {
	ckfree((char *) rpcValue[fd]);
    }
    rpcValue[fd] = (char *) ckalloc(strlen(interp->result) + 1);
    strcpy(rpcValue[fd], interp->result);
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_ReceiveRPC_helper --
 *
 *	This procedure is an internal routine to process RPC commands.
 *	It assumes the caller is providing some additional needed
 *	information, so we don't repeat work already done.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */

static int
Tdp_ReceiveRPC_helper(interp, fileHandle, command, respond, fd)
    Tcl_Interp *interp;
    char *fileHandle;		/* Socket connected to the process
			         * that originated the RPC. */
    char *command;		/* Tcl command requested by remote process
			         * to be evaluated. */
    int respond;		/* 1 if should send return value back. */
    int fd;			/* File descriptor of socket */
{
    char *rpcFileOld;
    int result;
    RPCInterpData *rd = (RPCInterpData *)NULL;
    ClientData cd;
    char *checkCmd;

    assert(fd < MAX_OPEN_FILES);

    /*
     * Find the RPCInterpData associated with this interpreter.  It is the
     * client data associated with our trace on rpcFile.
     */
    cd = Tcl_VarTraceInfo(interp, "rpcFile", TCL_GLOBAL_ONLY,
			  Tdp_RPCVarTrace, (ClientData)0);
    /* %%% Need to handle error here */
    rd = (RPCInterpData *)cd;

    /*
     * Set the global variable "rpcFile" to hold the fileId of
     * the socket connected to the RPC originating process.
     * Thus, the Tcl command can communicate with the
     * the RPC originating process.
     */

    rpcFileOld = rd->rpcFile;
    rd->rpcFile = fileHandle;

    /*
     * Need to trigger traces to update Tcl view, if creating variable
     */
    if (rpcFileOld == (char *)NULL) {
        (void) Tcl_GetVar(interp, "rpcFile", TCL_GLOBAL_ONLY);
    }

    /*
     * Check that the command is legal to evaluate.  If so,
     * evaluate the command and send back the result,
     * if necessary.  Otherwise, send back the error message from
     * the checker. Eliminate the result from this interpreter.
     */
    checkCmd = rd->checkCmd;
    rd->checkCmd = rpcCheck[fd];
    result = Tcl_GlobalEval(interp, command);
    rd->checkCmd = checkCmd;

    if (respond) {
	(void) send_tagged_packet(interp, fileHandle,
		 (result != TCL_OK) ? TOK_ERR : TOK_RET, interp->result);
    }
    Tcl_ResetResult(interp);

    /*
     * Reset the original value of the global variable "rpcFile";
     */

    rd->rpcFile = rpcFileOld;
    if (rd->rpcFile == (char *)NULL) {
        Tcl_UnsetVar(interp, "rpcFile", TCL_GLOBAL_ONLY);
    }

    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_ReceiveRPC --
 *
 *	This procedure is the C interface to the "dp_ReceiveRPC"
 *	command.  It's called to handle an incoming RPC request
 *	that has been received.  It evalutes the requested command
 *	and sends a return value back to the originating process,
 *	if specified.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */

int
Tdp_ReceiveRPC(interp, fileHandle, command, respond)
    Tcl_Interp *interp;
    char *fileHandle;		/* Socket connected to the process
			         * that originated the RPC. */
    char *command;		/* Tcl command requested by remote process
			         * to be evaluated. */
    int respond;		/* 1 if should send return value back. */
{
    FILE *filePtr;
    int fd;

    if (Tcl_GetOpenFile(interp, fileHandle, 0, 1, &filePtr) != TCL_OK) {
      return TCL_ERROR;
    }
    fd = fileno(filePtr);

    return Tdp_ReceiveRPC_helper(interp, fileHandle, command, respond, fd);
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_ReceiveRPCCmd --
 *
 *	This procedure processes the "dp_ReceiveRPC" Tcl command.
 *      Parses args and calls Tdp_ReceiveRPC to do the work.
 *	See Tdp_ReceiveRPC for more info.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */

 /* ARGSUSED */
int
Tdp_ReceiveRPCCmd(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    char *fileHandle;

    if (argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], " peer command\"", (char *) NULL);
	return TCL_ERROR;
    }
    fileHandle = argv[1];

    return (Tdp_ReceiveRPC(interp, fileHandle, argv[2], 1));
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_ProcessRPCMessages --
 *
 *	This procedure is the C interface to the "dp_ProcessRPCMessages"
 *	command.  This command reads one or more messages off a given
 *	socket, and processes these messages as RPC messages.
 *	An RPC message is headed by one of the following:
 *
 *	TOK_RET	: message is the return value
 *		  of an RPC which this process requested.
 *	TOK_ERR	: message is the error return value
 *		  of an RPC which this process requested.
 *	TOK_RDO	: message is an incoming request for this
 *		  process to evaluate an RDO.
 *	TOK_RPC	: message is an incoming request for this
 *		  process to evaluate an RPC.
 *
 *	This procedure will block as it reads the socket.
 *	If wait is specified (non-zero), then this procedure will
 *	not return until it either a TOK_RET or TOK_ERR message
 *	is received.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */
int
Tdp_ProcessRPCMessages(interp, fileHandle, wait)
    Tcl_Interp *interp;
    char *fileHandle;		/* Socket connected to RPC peer. */
    int wait;			/* 1 if wait for an incoming TOK_RET
			         * or TK_ERR message. */
{
    char *buffer;
    char token;
    int fd, result;

    /*
     * Determine the file descriptor we actually need to use.
     */
    {
      FILE *filePtr;
      if (Tcl_GetOpenFile(interp, fileHandle, 0, 1, &filePtr) != TCL_OK) {
	return TCL_ERROR;
      }
      fd = fileno(filePtr);
      assert(fd < MAX_OPEN_FILES);
    }

    while (1) {
	result = Tdp_PacketReceive(interp, fileHandle, 0);
	if (result != TCL_OK) {
	    /*
	     * Got a badly formed packet on the pipe or the connection
	     * is closed.  Tdp_PacketReceive already closed the file
	     * and cleaned up the file handler for us.  We just have to
	     * clean up the rpc data structures and get out.
	     * Ignore any errors generated during cleanup, and return our own.
	     */

	    Tdp_CleanupRPC (interp, fd, 0);

	    Tcl_ResetResult (interp);
	    Tcl_AppendResult (interp, "Error receiving RPC on ", fileHandle,
			      " -- Closing connection", (char *)NULL);
	    return result;
	}

	/*
	 * Only part of the data is available right now.  If waiting,
	 * do another read until it's all there.  Otherwise, return.
	 */
	if (interp->result[0] == 0) {
	    if (!wait) {
		return TCL_OK;
	    } else {
		continue;
	    }
	}


	buffer = interp->result;
	/* XXX */
	token = *buffer++;
	if (*buffer++ != ' ') {
	    return TCL_ERROR;
	}

	/* Handle received message depending on token; */
	switch (token) {
	case TOK_RET:
	case TOK_ERR:
	    /*
	     * Received message is either the return value
	     * or an error value of an RPC which this process
	     * originated.
	     *
	     * If the message is an error return value, we save the
	     * result here, and the original caller will eventually
	     * handle it.  This is tricky, since we don't want the
	     * file handler which called us to perceive it as an
	     * error (which would be interpreted as something wrong
	     * with the connection), we want the RPC routine to get
	     * the error.
	     */

	    /*
	     * If previous RPC was cancelled on this line, ignore the 
	     * return value.
	     */
	    if (rpcFlush[fd]) {
		rpcFlush[fd]--;
		if (!wait) {
		    return TCL_OK;
		} else {
		    continue;
		}
	    }

	    rpcWaiting[fd] = 0;
	    if (timerToken[fd]) {
		Tk_DeleteTimerHandler(timerToken[fd]);
		timerToken[fd] = 0;
	    }
	    if (token == TOK_RET) {
		rpcResult[fd] = TCL_OK;
	    } else {
		rpcResult[fd] = TCL_ERROR;
	    }

	    if (rpcValue[fd]) {
		ckfree((char *) rpcValue[fd]);
		rpcValue[fd] = NULL;
	    }

	    rpcValue[fd] = (char *) ckalloc(strlen(buffer)+1);
	    strcpy(rpcValue[fd], buffer);

	    if (rpcTimeoutReturn[fd]) {
		ckfree((char *) rpcTimeoutReturn[fd]);
		rpcTimeoutReturn[fd] = NULL;
	    }

	    return TCL_OK;

	case TOK_RDO:
	case TOK_RPC:
	    /*
	     * evaluate the received message as an RDO or RPC request.
	     * Note that this could be a return value from a previous
	     * RDO call.
	     */
	    {
		char *cmd;

		cmd = ckalloc(strlen(buffer) + 1);
		strcpy(cmd, buffer);

		Tdp_ReceiveRPC(interp, fileHandle, cmd, (token == TOK_RPC));

		ckfree((char *) cmd);
	    }
	    break;

	default:
	    break;
	}

	if (!wait)
	    return TCL_OK;
    }
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_ProcessRPCMessagesCmd --
 *
 *	This procedure is invoked to process the "dp_ProcessRPCMessages"
 *	Tcl command.  Parses args and calls Tdp_ProcessRPCMessages
 *	to do the work.  See Tdp_ProcessRPCMessages for more info.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */

 /* ARGSUSED */
int
Tdp_ProcessRPCMessagesCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Unused. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    char *fileHandle;
    int wait;

    if (argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], " fileId wait\"", (char *) NULL);
	return TCL_ERROR;
    }
    fileHandle = argv[1];

    if (Tcl_GetInt(interp, argv[2], &wait) != TCL_OK)
	return TCL_ERROR;

    return (Tdp_ProcessRPCMessages(interp, fileHandle, wait));
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_RPC --
 *
 *	This procedure is the C interface to the "dp_RPC" command.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */

int
Tdp_RPC(interp, fileHandle, command, events, timeout, timeoutReturn)
    Tcl_Interp *interp;		/* Tcl interpreter. */
    char *fileHandle;
    char *command;
    int events;			/* Flags for Tk_DoOneEvent. */
    int timeout;		/* In milliseconds. */
    char *timeoutReturn;	/* Tcl command evaluated
			         * when timeout occurs. */
{
    int fd;
    int result;

    /*
     * Get the file descriptor we will be using.
     */
    {
      FILE *filePtr;
      if (Tcl_GetOpenFile(interp, fileHandle, 1, 1, &filePtr) != TCL_OK) {
	return TCL_ERROR;
      }
      fd = fileno(filePtr);
      assert(fd < MAX_OPEN_FILES);
    }

    if (rpcWaiting[fd]) {
	Tcl_AppendResult(interp, "dp_RPC error: already in dp_RPC",
			 (char *) NULL);
	return TCL_ERROR;
    }

    if (events >= 0)
	events = events | TK_FILE_EVENTS;

    /*
     * Record that we will doing an RPC;
     */
    rpcWaiting[fd] = 1;
    rpcResult[fd] = TCL_ERROR;

    if (rpcValue[fd]) {
	ckfree((char *) rpcValue[fd]);
	rpcValue[fd] = NULL;
    }

    if (rpcTimeoutReturn[fd]) {
	ckfree((char *) rpcTimeoutReturn[fd]);
	rpcTimeoutReturn[fd] = NULL;
    }

    if (timeout > 0) {
	/* Record timeout parameters, if supplied; */
	timerToken[fd] = Tk_CreateTimerHandler(timeout, Tdp_TimeoutHandler,
					       (ClientData) fd);
	timerInterp[fd] = interp;

	if (events < 0) {
	    events = TK_FILE_EVENTS | TK_TIMER_EVENTS;
	} else {
	    events = events | TK_TIMER_EVENTS;
	}

	if (timeoutReturn) {
	    rpcTimeoutReturn[fd] = (char *) ckalloc(strlen(timeoutReturn) + 1);
	    strcpy(rpcTimeoutReturn[fd], timeoutReturn);
	}
    }

    /*
     * Prepare and send off the RPC command;
     */
    (void) send_tagged_packet(interp, fileHandle, TOK_RPC, command);

    /*
     * Handle incoming RPC messages, waiting for the RPC return value;
     */
    if (events >= 0) {
	while (rpcWaiting[fd])
	    Tk_DoOneEvent(events);
    } else
	Tdp_ProcessRPCMessages(interp, fileHandle, 1);

    /*
     * Report RPC return value;
     */

    result = rpcResult[fd];

    if (result == 0) {
	Tcl_ResetResult (interp);
    }

    if (rpcValue[fd]) {
	Tcl_SetResult(interp, rpcValue[fd], TCL_DYNAMIC);
	rpcValue[fd] = NULL;
    }

    /*
     * Cleanup RPC records;
     */

    rpcWaiting[fd] = 0;
    rpcResult[fd] = TCL_ERROR;

    if (rpcTimeoutReturn[fd]) {
	ckfree((char *) rpcTimeoutReturn[fd]);
	rpcTimeoutReturn[fd] = NULL;
    }

    return (result);
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_RPCCmd --
 *
 *	This procedure processes the "dp_RPC" Tcl command.  Parses
 *	args and calls Tdp_RPC to do the work.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */

 /* ARGSUSED */
int
Tdp_RPCCmd(clientData, interp, argc, argv)
    ClientData clientData;		/* Unused. */
    Tcl_Interp *interp;			/* Current interpreter.	*/
    int argc;				/* Number of arguments.	*/
    char **argv;			/* Argument strings. 	*/
{
    char *fileHandle;
    char *command = NULL;
    int result;

    int events = -1;		/* Default: RPC will block. 	*/
    int timeout = 0;		/* Default: no timeout. 	*/
    char *timeoutReturn = NULL;	/* Default: no timeoutReturn. 	*/

    int rpc_argc;		/* Passed to Tdp_RPC. */
    char **rpc_argv;		/* Passed to Tdp_RPC. */

    if (argc < 3) {
      error_args:
	Tcl_AppendResult(interp,
			 "wrong # args: should be \"", argv[0], " fileId",
			 " ?-events events?",
		     " ?-timeout millisecs ??-timeoutReturn callback???",
			 " command ?args ...?\"",
			 (char *) NULL);

	result = TCL_ERROR;
	goto done;
    }
    fileHandle = argv[1];

    rpc_argc = argc - 2;
    rpc_argv = argv + 2;

    /* Process the parameter flags as long as we see a '-' character;
     */

    while (rpc_argv[0][0] == '-') {

	if (rpc_argc < 3) {
	    goto error_args;
	}
	/* Process a parameter flag by either:
	 *   Constructing events mask from "-events" parameter;
	 *   Retrieving timeout value from "-timeout" parameter;
	 *   Retrieving timeoutReturn value from "-timeoutReturn" parameter;
	 */

	if (strcmp(rpc_argv[0], "-events") == 0) {
	    int i;
	    int eventc;
	    char **eventv;

	    int none = 0;

	    if (Tcl_SplitList(interp, rpc_argv[1], &eventc, &eventv) != TCL_OK) {
		Tcl_AppendResult(interp, "bad parameter \"", rpc_argv[1],
				 "\" for -events flag of ", argv[0],
				 (char *) NULL);
		result = TCL_ERROR;
		goto done;
	    }
	    events = TK_FILE_EVENTS;

	    for (i = 0; i < eventc; i++) {
		if (strcmp(eventv[i], "x") == 0)
		    events = events | TK_X_EVENTS;
		else if (strcmp(eventv[i], "rpc") == 0)
		    events = events | TK_FILE_EVENTS;
		else if (strcmp(eventv[i], "file") == 0)
		    events = events | TK_FILE_EVENTS;
		else if (strcmp(eventv[i], "timer") == 0)
		    events = events | TK_TIMER_EVENTS;
		else if (strcmp(eventv[i], "idle") == 0)
		    events = events | TK_IDLE_EVENTS;
		else if (strcmp(eventv[i], "all") == 0)
		    events = TK_ALL_EVENTS;
		else if (strcmp(eventv[i], "none") == 0)
		    none = 1;
		else {
		    Tcl_AppendResult(interp, "unknown event type \"",
				     eventv[i], "\" : should be ",
			       "x, rpc, file, timer, idle, all, or none",
				     (char *) NULL);
		    free((char *) eventv);

		    result = TCL_ERROR;
		    goto done;
		}
	    }

	    if ((none) || (eventc <= 0))
		events = -1;

	    ckfree((char *) eventv);
	} else if (strcmp(rpc_argv[0], "-timeout") == 0) {
	    if (Tcl_GetInt(interp, rpc_argv[1], &timeout) != TCL_OK) {
		result = TCL_ERROR;
		goto done;
	    }
	} else if (strcmp(rpc_argv[0], "-timeoutReturn") == 0) {
	    if (timeoutReturn) {
		ckfree((char *) timeoutReturn);
		timeoutReturn = NULL;
	    }

	    timeoutReturn = (char *) ckalloc(strlen(rpc_argv[1]) + 1);
	    strcpy(timeoutReturn, rpc_argv[1]);
	} else {
	    Tcl_AppendResult(interp, "unknown parameter flag \"",
			     rpc_argv[0], "\" : should be ",
			     "-events, -timeout, or -timeoutReturn",
			     (char *) NULL);
	    result = TCL_ERROR;
	    goto done;
	}

	rpc_argc = rpc_argc - 2;
	rpc_argv = rpc_argv + 2;
    }

    command = Tcl_Merge(rpc_argc, rpc_argv);
    result = Tdp_RPC(interp, fileHandle, command, events,
		     timeout, timeoutReturn);

  done:
    if (timeoutReturn)
	ckfree((char *) timeoutReturn);
    if (command)
	ckfree((char *) command);

    return (result);
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_ProcessRPCCommandCmd --
 *
 *	This procedure is invoked to process the "dp_ProcessRPCCommand"
 *	Tcl/Tk command.  See the user documentation for details on
 *	what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */

 /* ARGSUSED */
int
Tdp_ProcessRPCCommandCmd(unused, interp, argc, argv)
    ClientData unused;		/* Unused. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    char *fileHandle;

    if (argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], " status fileId\"",
			 (char *) NULL);
	return TCL_ERROR;
    }
    fileHandle = argv[2];

    if (argv[1][0] == 'e') {
        FILE *filePtr;
	if (Tcl_GetOpenFile(interp, fileHandle, 0, 0, &filePtr) != TCL_OK) {
	  return TCL_ERROR;
	}
	Tdp_CleanupRPC (interp, fileno(filePtr), 1);
	Tcl_ResetResult(interp);
	Tcl_AppendResult(interp, "Error in ", argv[0], " e ", argv[2], ": ",
				Tcl_PosixError(interp), (char *)NULL);
	return TCL_ERROR;
    } else
	return (Tdp_ProcessRPCMessages(interp, fileHandle, 0));
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_RDOCmd --
 *
 *	This procedure is invoked to process the "dp_RDO" Tcl
 *	command.  Parses args and sends out the packet.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */

/*
 * sprintf template when both callback and onerror are specified.
 *	Params are cmd, onerror, callback
 */
static char *ceCmdTemplate =
"if [catch {%s} dp_rv] {\
    dp_RDO $rpcFile eval \"%s \\{$dp_rv\\}\"\
} else {\
    dp_RDO $rpcFile eval \"%s $dp_rv\"\
}";

/*
 * sprintf template when just onerror is specified.
 *	Params are cmd, onerror
 */
static char *eCmdTemplate =
"if [catch {%s} dp_rv] {\
    dp_RDO $rpcFile eval \"%s \\{$dp_rv\\}\"\
}";

/*
 * sprintf template when just callback is specified.
 *	Params are cmd, callback
 */
static char *cCmdTemplate = 
"set dp_rv [%s]; dp_RDO $rpcFile eval \"%s \\{$dp_rv\\}\"";


 /* ARGSUSED */
int
Tdp_RDOCmd(unused, interp, argc, argv)
    ClientData unused;		/* Unused. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    char *fileHandle;
    char *command;
    char *cmd;
    int result;
    char *callback;
    char *onerror;
    int i;

    if (argc < 3) {
      error_args:
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], " peer ?-callback callback? ",
			 "?-onerror callback? ",
			 "command ?args ...?\"",
			 (char *) NULL);
	return TCL_ERROR;
    }
    fileHandle = argv[1];

    callback = NULL;
    onerror = NULL;
    for (i=2; i<argc; i++) {
	if (strcmp(argv[i], "-callback") == 0) {
	    i++;
	    callback = argv[i];
	} else if (strcmp(argv[i], "-onerror") == 0) {
	    i++;
	    onerror = argv[i];
	} else {
	    break;
	}
    }
    if (argc == i) {
	goto error_args;
    }

    cmd = Tcl_Merge(argc - i, argv + i);

    if (onerror != NULL) {
	if (callback != NULL) {
	    /*
	     * Both onerror & callback specified.  Form of
	     * command is:
	     *	if [catch $cmd dp_rv] {
	     *	   dp_RDO $rpcFile eval {$onerror $dp_rv}
	     *  } else {
	     *	   dp_RDO $rpcFile eval {$callback $dp_rv}
	     *  }
	     */
	    command = (char *) ckalloc(strlen(cmd) +
				       strlen(onerror) +
				       strlen(callback) +
				       strlen(ceCmdTemplate));
	    sprintf (command, ceCmdTemplate, cmd, onerror, callback);
	} else {
	    /*
	     * Just onerror specified.  Form of
	     * command is:
	     *	if [catch $cmd dp_rv] {
	     *	   dp_RDO $rpcFile eval {$onerror $dp_rv}
	     *  }
	     */
	    command = (char *) ckalloc(strlen(cmd) +
				       strlen(onerror) +
				       strlen(eCmdTemplate));
	    sprintf (command, eCmdTemplate, cmd, onerror);
	}
    } else {
	if (callback != NULL) {
	    /*
	     * Just callback specified.  Form of
	     * command is:
	     *	   dp_RDO $rpcFile $callback [$cmd]
	     */
	    command = (char *) ckalloc(strlen(cmd) +
				       strlen(callback) +
				       strlen(cCmdTemplate));
	    sprintf (command, cCmdTemplate, cmd, callback);
	} else {
	    /*
	     * No callbacks specified.  Form of
	     * command is:
	     *	$cmd
	     */
	    command = (char *) ckalloc(strlen(cmd) + 1);
	    strcpy (command, cmd);
	}
    }
    ckfree((char *) cmd);

    result = send_tagged_packet(interp, fileHandle, TOK_RDO, command);
    ckfree((char *) command);
    return (result);
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_CancelRPCCmd --
 *
 *	This procedure is invoked to force current RPC's that are
 *	waiting for results of remote evaluation to stop waiting
 *	and simply return an error.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */

 /* ARGSUSED */
int
Tdp_CancelRPCCmd(unused, interp, argc, argv)
    ClientData unused;		/* Unused. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    if (argc < 1) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], " ?fileId? ?fileId? ...",
			 (char *) NULL);
	return TCL_ERROR;
    }
    if (argc == 1) {
	int i;
	for (i = 0; i < MAX_OPEN_FILES; i++) {
	    if (rpcWaiting[i]) {
		Tdp_CancelRPC(i, 1);
	    }
	}

	return TCL_OK;
    }
    argc = argc - 1;
    argv = argv + 1;

    while (argc > 0) {
        FILE *filePtr;
	int fd;

	if (Tcl_GetOpenFile(interp, argv[0], 0, 0, &filePtr) != TCL_OK)
	    return TCL_ERROR;

	fd = fileno(filePtr);
	Tdp_CancelRPC(fd, 1);

	argc = argc - 1;
	argv = argv + 1;
    }

    return (TCL_OK);
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_SetCheckCmd --
 *
 *	This function implements the "dp_SetCheckCmd" tcl command.
 *	See the user docs for details.
 *
 * Results:
 *	A standard tcl result.
 *
 * Side effects:
 *	From now on, whenever an RPC is received, a tcl check function
 *	will be called to verify whether it's allowed in this server. 
 *	This means some commands may not be allowed as RPC's.
 *
 *--------------------------------------------------------------
 */
            /* ARGSUSED */
int
Tdp_SetCheckCmd (clientData, interp, argc, argv)
    ClientData clientData;          /* ignored */
    Tcl_Interp *interp;             /* tcl interpreter */
    int argc;                       /* Number of arguments */
    char *argv[];                   /* Arg list */
{
    char *old;
    FILE *filePtr;
    int fd;

    if (argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			" file cmd\"", (char *) NULL);
	return TCL_ERROR;
    }

    if (Tcl_GetOpenFile(interp, argv[1], 0, 0, &filePtr) != TCL_OK)
	return TCL_ERROR;
    fd = fileno(filePtr);
    old = rpcCheck[fd];
    rpcCheck[fd] = NULL;
    if (strcmp (argv[2], "none") != 0) {
	rpcCheck[fd] = ckalloc(strlen(argv[2])+1);
	strcpy (rpcCheck[fd], argv[2]);
    } 
    if (old != NULL) {
	Tcl_SetResult (interp, old, TCL_DYNAMIC);
    } else {
	Tcl_SetResult (interp, "none", TCL_STATIC);
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * Tdp_RPCInit --
 *
 *	This procedure initializes this RPC implementation.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */

int
Tdp_RPCInit(interp)
    Tcl_Interp *interp;		/* Current interpreter. */
{
    RPCInterpData *	rd;

    /*
     * Build interpreter data we will need.  Link in rpcFile.  Set up to
     * automatically free the interpreter data when we finish.  Set up
     * command tracing.
     */
    rd = (RPCInterpData *)ckalloc(sizeof(RPCInterpData));
    if (rd == (RPCInterpData *)NULL) {
	Tcl_AppendResult (interp, "Error allocating RPCInterpData",
			 (char *)NULL);
	return TCL_ERROR;
    }
    rd->rpcFile = (char *)NULL;
    rd->checkCmd = (char *)NULL;
    Tcl_TraceVar(interp, "rpcFile",
			TCL_GLOBAL_ONLY | TCL_TRACE_READS |
			TCL_TRACE_WRITES | TCL_TRACE_UNSETS,
			Tdp_RPCVarTrace, (ClientData)rd);
    Tcl_CallWhenDeleted(interp,
			(Tcl_InterpDeleteProc *)Tdp_FreeRPCInterpData,
			(ClientData)rd);
    Tcl_CreateTrace(interp, INT_MAX, Tdp_CommandTrace, (ClientData)rd);

    /*
     * Create commands.
     */
    Tcl_CreateCommand(interp, "dp_RDO",
		      Tdp_RDOCmd,
		      (ClientData) rd, (void (*)()) NULL);
    Tcl_CreateCommand(interp, "dp_RPC",
		      Tdp_RPCCmd,
		      (ClientData) rd, (void (*)()) NULL);
    Tcl_CreateCommand(interp, "dp_CancelRPC",
		      Tdp_CancelRPCCmd,
		      (ClientData) rd, (void (*)()) NULL);
    Tcl_CreateCommand(interp, "dp_ProcessRPCMessages",
		      Tdp_ProcessRPCMessagesCmd,
		      (ClientData) rd, (void (*)()) NULL);
    Tcl_CreateCommand(interp, "dp_ProcessRPCCommand",
		      Tdp_ProcessRPCCommandCmd,
		      (ClientData) rd, (void (*)()) NULL);
    Tcl_CreateCommand(interp, "dp_ReceiveRPC",
		      Tdp_ReceiveRPCCmd,
		      (ClientData) rd, (void (*)()) NULL);
    Tcl_CreateCommand(interp, "dp_SetCheckCmd",
		      Tdp_SetCheckCmd,
		      (ClientData) rd, (void (*)()) NULL);

    return TCL_OK;
}

static int
send_tagged_packet(interp, fileHandle, type, command)
    Tcl_Interp *interp;
    char *fileHandle;
    int type;
    char *command;
{
    char *buffer;
    int result;

    buffer = (char *) ckalloc(strlen(command) + 3);
    buffer[0] = type;
    buffer[1] = ' ';
    strcpy(buffer + 2, command);
    result = Tdp_PacketSend(interp, fileHandle, buffer);
    ckfree(buffer);
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * ReadSysClock --
 *
 *	Return the value of the system clock as a double.
 *
 * Results:
 *	Value of the system clock.
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */
double ReadSysClock()
{
    struct timeval tv;
    (void) gettimeofday(&tv, NULL);
    return (tv.tv_sec + tv.tv_usec / 1000000.0);
}
