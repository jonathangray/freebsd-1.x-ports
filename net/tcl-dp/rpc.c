/*
 * rpc.c
 *
 *	This file partially implements the RPC commands of Tcl-DP.
 *	Some RPC commands of Tcl-DP are implemented in Tcl code.
 *	See "rpc.tcl" for more information.
 *
 * Copyright 1992 Regents of the University of California.
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

#include "tcl.h"
#include "tclInt.h"

#include "tk.h"

#include "util.h"
#include "network.h"

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

static int rpcWaiting[256];		/* indexed by fd;	 */
static int  rpcResult[256];		/* indexed by fd;	 */
static char *rpcValue[256];		/* indexed by fd;	 */

static double         rpcTime[256];	/* indexed by fd;	 */
static int         rpcTimeout[256];	/* indexed by fd;	 */
static char *rpcTimeoutReturn[256];	/* indexed by fd;	 */

/* ---------------------------------------------------- */

static Tk_TimerToken timerToken;


/*
 *--------------------------------------------------------------
 *
 * TimeoutHandler
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
TimeoutHandler (clientData)
     ClientData clientData;		/* Tcl interpreter. */
{
  Tcl_Interp *interp = (Tcl_Interp *) clientData;
  int timeLeftMin;
  int i;
  double currTime;

  if (!interp)
    return;

  currTime = (double) ReadSysClock ();

  timerToken = (Tk_TimerToken) 0;

  /* Run through all sockets, checking for RPC timeouts;
   */
  for (i = 0, timeLeftMin = 0; i < 256; i++)
    {
      if (rpcWaiting[i] && (rpcTimeout[i] > 0))
	{
	  /* On socket i, we are waiting for an RPC return value, and 
	   * the RPC caller specified a timeout;
	   */

	  if (rpcTime[i] + ((double) rpcTimeout[i] <= currTime))
	    {
	      /*  Timeout occurred;
	       *  Record error and reset rpcWaiting flag for this RPC;
	       */

	      rpcResult[i]  = TCL_ERROR;

	      rpcTimeout[i] = 0;
	      rpcTime[i]    = 0.0;

	      if (rpcValue[i])
		ckfree ((char *) rpcValue[i]);
	      rpcValue[i] = NULL;
		  
	      if (rpcTimeoutReturn[i])
		{
		  /*  Eval the callback for timeouts, if specified,
		   *  and use that callback return value as the 
		   *  return value for this RPC;
		   */
		  
		  char fileId[20];
		  sprintf (fileId, "file%d", i);
		  
		  rpcResult[i] = Tcl_VarEval (interp, 
					      rpcTimeoutReturn[i], " ", 
					      fileId, (char *) NULL);
		  rpcValue[i] = (char *) ckalloc (strlen (interp->result) + 1);
		  strcpy (rpcValue[i], interp->result);
		}

	      if (rpcTimeoutReturn[i])
		ckfree ((char *) rpcTimeoutReturn[i]);
	      rpcTimeoutReturn[i] = NULL;

	      rpcWaiting[i] = 0;
	    }
	  else 
	    {
	      /*  Timeout not expired;
	       *  Compute the timeLeft until the expiration;
	       */

	      int timeLeft;
	      timeLeft = (int) ((rpcTime[i] + ((double) rpcTimeout[i])) -
				currTime);

	      /*  Find minimum timeLeft;
	       */

	      if ((timeLeftMin <= 0) || (timeLeftMin > timeLeft))
		timeLeftMin = timeLeft;
	    }
	}
    }

  /*  If there are RPC's with timeouts that haven't yet occurred,
   *  setup another Tk TimerHandler for the future.
   */

  if (timeLeftMin > 0)
    timerToken = Tk_CreateTimerHandler (timeLeftMin, TimeoutHandler, 
					(ClientData) interp);
}

/*
 *--------------------------------------------------------------
 *
 * Tcm_ReceiveRPC --
 *
 *	This procedure is the C interface to the "ReceiveRPC"
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
Tcm_ReceiveRPC(interp, filePtr, command, respond)
    Tcl_Interp *interp;
    OpenFile *filePtr;	/* Socket connected to the process 
			 * that originated the RPC. */
    char *command;	/* Tcl command requested by remote process
			 * to be evaluated. */
    int respond;	/* 1 if should send return value back. */
{
    char rpcFileOld[10];
    char rpcFileNew[10];

    char *buffer;
    int result;

    /*
     * Set the global variable "rpcFile" to hold the fileId of 
     * the socket connected to the RPC originating process.
     * Thus, the Tcl command can communicate with the 
     * the RPC originating process.
     */

    strcpy (rpcFileOld, 
	    Tcl_GetVar (interp, "rpcFile", TCL_GLOBAL_ONLY));

    sprintf (rpcFileNew, "file%d", fileno(filePtr->f));
    Tcl_SetVar (interp, "rpcFile", rpcFileNew, TCL_GLOBAL_ONLY);

    /*
     * Evaluate the command, and prepare to send back the result,
     * if necessary.
     */

    result = Tcl_GlobalEval (interp, command);

    if (respond) 
      {
	/*
	 * Tag the response with TOK_RET or TOK_ERR, depending
	 * on the result of the command evaluation.
	 */

	buffer = ckalloc (strlen (interp->result) + 10);
	strcpy (buffer + 2, interp->result);
	buffer[1] = ' ';

	if (result != TCL_OK)
	    buffer[0] = TOK_ERR;
	else
	    buffer[0] = TOK_RET;

	Tcp_PacketSend (interp, filePtr, buffer);

	ckfree ((char *) buffer);
      }

    /*
     * Reset the original value of the global variable "rpcFile";
     */

    Tcl_SetVar (interp, "rpcFile", rpcFileOld, TCL_GLOBAL_ONLY);

    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * Tcm_ReceiveRPCCmd --
 *
 *	This procedure processes the "ReceiveRPC" Tcl command.
 *      Parses args and calls Tcm_ReceiveRPC to do the work.
 *	See Tcm_ReceiveRPC for more info.
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
Tcm_ReceiveRPCCmd(unused, interp, argc, argv)
    ClientData unused;
    Tcl_Interp *interp;
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    OpenFile *filePtr;

    if (argc != 3) 
      {
	Tcl_AppendResult (interp, "wrong # args: should be \"",
			  argv[0], " peer command\"", (char *) NULL);
	return TCL_ERROR;
      }

    if (TclGetOpenFile (interp, argv[1], &filePtr) != TCL_OK)
      return TCL_ERROR;

    return (Tcm_ReceiveRPC (interp, filePtr, argv[2], 1));
}

/*
 *--------------------------------------------------------------
 *
 * Tcm_ProcessRPCMessages --
 *
 *	This procedure is the C interface to the "ProcessRPCMessages" 
 *	command.  This command reads one or more messages off of a given
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
Tcm_ProcessRPCMessages(interp, filePtr, wait)
    Tcl_Interp *interp;
    OpenFile *filePtr;	/* Socket connected to RPC peer. */
    int wait;		/* 1 if wait for an incoming TOK_RET
			   or TK_ERR message. */
{
    char *buffer;
    char token;

    int i;
    int fd;

    int result;

    int    argc;
    char **argv;

    char temp[20];

    fd = fileno (filePtr->f);

    while (1) 
      {
	if (Tcp_PacketReceive (interp, filePtr, 1) != TCL_OK) 
	  {
	    rpcWaiting[fd] = 0;
	    rpcResult[fd]  = TCL_ERROR;
	    rpcTimeout[fd] = 0;
	    rpcTime[fd]    = 0.0;
	    
	    if (rpcValue[fd])
	      ckfree ((char *) rpcValue[fd]);
	    rpcValue[fd] = NULL;

	    if (rpcTimeoutReturn[fd])
	      ckfree ((char *) rpcTimeoutReturn[fd]);
	    rpcTimeoutReturn[fd] = NULL;

	    Tcl_AppendResult (interp, 
			      "RPC error : Tcm_ProcessRPCMessages",
			      (char *) NULL);

	    sprintf (temp, "file%d", fd);

	    Tcl_VarEval (interp, "filehandler ", temp,
			 (char *) NULL);
	    Tcl_VarEval (interp, "close ", temp,
			 (char *) NULL);

	    return TCL_ERROR;
	  }

	if (Tcl_SplitList (interp, interp->result, &argc, &argv) != TCL_OK) 
	  return TCL_ERROR;

	for (i = 0; i < argc; i++) 
	  {
	    buffer = argv[i];

	    /* Skip whitespace;
	     */
	    while (buffer && (buffer[0] != '\0') && (buffer[0] == ' '))
	      buffer++;

	    /* Grab incoming message token;
	     */
	    token = buffer[0];	
	    buffer++;

	    /* Skip whitespace;
	     */
	    while (buffer && (buffer[0] != '\0') && (buffer[0] == ' '))
		buffer++;

	    /* Handle received message depending on token;
	     */
	    switch (token) 
	      {
	      case TOK_RET:		/* received message is the return 
					 * value of an RPC which this process
					 * originated. */
		rpcWaiting[fd] = 0;
		rpcResult[fd]  = TCL_OK;
		rpcTimeout[fd] = 0;
		rpcTime[fd]    = 0.0;

		if (rpcValue[fd])
		  ckfree ((char *) rpcValue[fd]);
		rpcValue[fd] = NULL;

		rpcValue[fd] = (char *) ckalloc (strlen (buffer) + 1);
		strcpy (rpcValue[fd], buffer);

		if (rpcTimeoutReturn[fd])
		  ckfree ((char *) rpcTimeoutReturn[fd]);
		rpcTimeoutReturn[fd] = NULL;

		ckfree ((char *) argv);
		return TCL_OK;

	      case TOK_ERR:		/* received message is the error 
					 * return value of an RPC which 
					 * this process originated. */
		rpcWaiting[fd] = 0;
		rpcResult[fd]  = TCL_ERROR;
		rpcTimeout[fd] = 0;
		rpcTime[fd]    = 0.0;

		if (rpcValue[fd])
		  ckfree ((char *) rpcValue[fd]);
		rpcValue[fd] = NULL;

		rpcValue[fd] = (char *) ckalloc (strlen (buffer) + 1);
		strcpy (rpcValue[fd], buffer);

		if (rpcTimeoutReturn[fd])
		  ckfree ((char *) rpcTimeoutReturn[fd]);
		rpcTimeoutReturn[fd] = NULL;

		result = Tcl_VarEval (interp, "error {", buffer, "}",
				      (char *) NULL);

		ckfree ((char *) argv);
		return (result);

	      case TOK_RDO:		/* evaluate the received message 
					 * as an RDO request */
		{
		  char *command;

		  command = ckalloc (strlen (buffer) + 1);
		  strcpy (command, buffer);

		  Tcm_ReceiveRPC (interp, filePtr, command, 0);

		  ckfree ((char *) command);
		}
		break;
		
	      case TOK_RPC:		/* evaluate the received message 
					 * as an RPC request */
		{
		  char *command;

		  command = ckalloc (strlen (buffer) + 1);
		  strcpy (command, buffer);

		  Tcm_ReceiveRPC (interp, filePtr, command, 1);

		  ckfree ((char *) command);
		}
		break;

	      default:
		break;
	      }
	  }

	ckfree ((char *) argv);

	if (!wait)
	  return TCL_OK;
      }
}

/*
 *--------------------------------------------------------------
 *
 * Tcm_ProcessRPCMessagesCmd --
 *
 *	This procedure is invoked to process the "ProcessRPCMessages" 
 *	Tcl command.  Parses args and calls Tcm_ProcessRPCMessages
 *	to do the work.  See Tcm_ProcessRPCMessages for more info.
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
Tcm_ProcessRPCMessagesCmd(unused, interp, argc, argv)
    ClientData unused;		/* Unused. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    OpenFile *filePtr;
    int wait;

    if (argc != 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], " fileId wait\"", (char *) NULL);
	return TCL_ERROR;
    }

    if (TclGetOpenFile(interp, argv[1], &filePtr) != TCL_OK)
	return TCL_ERROR;

    if (Tcl_GetInt(interp, argv[2], &wait) != TCL_OK)
	return TCL_ERROR;

    return (Tcm_ProcessRPCMessages (interp, filePtr, wait));
}

/*
 *--------------------------------------------------------------
 *
 * Tcm_RPC --
 *
 *	This procedure is the C interface to the "RPC" command.
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
Tcm_RPC(interp, filePtr, command, events, timeout, timeoutReturn)
     Tcl_Interp *interp;	/* Tcl interpreter. */
     OpenFile   *filePtr;
     char *command;
     int   events;		/* Flags for Tk_DoOneEvent. */
     int   timeout;		/* In milliseconds. */
     char *timeoutReturn;	/* Tcl command evaluated
				 * when timeout occurs. */
{
    static char buffer[4096];

    int fd;
    int result;

    fd = fileno(filePtr->f);

    if (rpcWaiting[fd]) {
	Tcl_AppendResult(interp, "RPC error: already in RPC",
			 (char *) NULL);
	return TCL_ERROR;
    }
    if (strlen(command) + 10 >= 4096) {
	Tcl_AppendResult(interp, "RPC error: command too long",
			 (char *) NULL);
	return TCL_ERROR;
    }

    /*
     * If events is negative, then block.
     */

    if (events >= 0)
      events = events | TK_FILE_EVENTS;

    /*
     * Record that we will doing an RPC;
     */

    rpcWaiting[fd] = 1;
    rpcResult[fd]  = TCL_ERROR;

    if (rpcValue[fd]) 
      ckfree ((char *) rpcValue[fd]);
    rpcValue[fd] = NULL;

    if (rpcTimeoutReturn[fd])
      ckfree ((char *) rpcTimeoutReturn[fd]);
    rpcTimeoutReturn[fd] = NULL;

    rpcTime[fd]    = 0.0;
    rpcTimeout[fd] = 0;

    if ((events >= 0) &&
	(timeout > 0))
      {
	/* Record timeout parameters, if supplied;
	 */

	rpcTime[fd]    = (double) ReadSysClock ();
	rpcTimeout[fd] = timeout;

	if (!timerToken)
	  timerToken = Tk_CreateTimerHandler (timeout, TimeoutHandler, 
					      (ClientData) interp);

	events = events | TK_TIMER_EVENTS;

	if (timeoutReturn)
	  {
	    rpcTimeoutReturn[fd] = 
	      (char *) ckalloc (strlen (timeoutReturn) + 1);
	    strcpy (rpcTimeoutReturn[fd], timeoutReturn);
	  }
      }

    /*
     * Prepare and send off the RPC command;
     */

    sprintf(buffer, "%c %s", TOK_RPC, command);

    Tcp_PacketSend(interp, filePtr, buffer);

    /*
     * Handle incoming RPC messages, waiting for the RPC return value;
     */

    if (events >= 0)
      {
	while (rpcWaiting[fd])
	  Tk_DoOneEvent (events);
      }
    else
      Tcm_ProcessRPCMessages (interp, filePtr, 1);

    /*
     * Report RPC return value;
     */

    result = rpcResult[fd];
    Tcl_ResetResult(interp);
    if (rpcValue[fd])
      Tcl_AppendResult(interp, rpcValue[fd], (char *) NULL);

    /*
     * Cleanup RPC records;
     */

    rpcWaiting[fd] = 0;
    rpcResult[fd]  = TCL_ERROR;
    rpcTimeout[fd] = 0;
    rpcTime[fd]    = 0.0;

    if (rpcValue[fd])
      ckfree ((char *) rpcValue[fd]);
    rpcValue[fd] = NULL;

    if (rpcTimeoutReturn[fd])
      ckfree ((char *) rpcTimeoutReturn[fd]);
    rpcTimeoutReturn[fd] = NULL;

    return (result);
}

/*
 *--------------------------------------------------------------
 *
 * Tcm_RPCCmd --
 *
 *	This procedure processes the "RPC" Tcl command.  Parses 
 *	args and calls Tcm_RPC to do the work.
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
Tcm_RPCCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Unused. */
    Tcl_Interp *interp;		/* Current interpreter.	*/
    int argc;			/* Number of arguments.	*/
    char **argv;		/* Argument strings. 	*/
{
    OpenFile *filePtr;
    char *command = NULL;
    int   result;

    int   events  = -1;		/* Default: RPC will block. 	*/
    int   timeout =  0;		/* Default: no timeout. 	*/
    char *timeoutReturn = NULL; /* Default: no timeoutReturn. 	*/

    int    rpc_argc;		/* Passed to Tcm_RPC. */
    char **rpc_argv;		/* Passed to Tcm_RPC. */
    
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

    if (TclGetOpenFile (interp, argv[1], &filePtr) != TCL_OK)
      return TCL_ERROR;

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

      if (strcmp (rpc_argv[0], "-events") == 0) 
	{
	  int    i;
	  int    eventc;
	  char **eventv;

	  int none = 0;

	  if (Tcl_SplitList (interp, rpc_argv[1], &eventc, &eventv) != TCL_OK)
	    {
	      Tcl_AppendResult(interp, "bad parameter \"", rpc_argv[1],
			       "\" for -events flag of ", argv[0],
			       (char *) NULL);
	      result = TCL_ERROR;
	      goto done;
	    }

	  events = TK_FILE_EVENTS;

	  for (i = 0; i < eventc; i++) 
	    {
	      if (strcmp (eventv[i], "x") == 0)
		events = events | TK_X_EVENTS;
	      else if (strcmp (eventv[i], "rpc") == 0) 
		events = events | TK_FILE_EVENTS;
	      else if (strcmp (eventv[i], "file") == 0) 
		events = events | TK_FILE_EVENTS;
	      else if (strcmp (eventv[i], "timer") == 0)
		events = events | TK_TIMER_EVENTS;
	      else if (strcmp (eventv[i], "idle") == 0)
		events = events | TK_IDLE_EVENTS;
	      else if (strcmp (eventv[i], "all") == 0)
		events = TK_ALL_EVENTS;
	      else if (strcmp (eventv[i], "none") == 0)
		none = 1;
	      else 
		{
		  Tcl_AppendResult (interp, "unknown event type \"",
				    eventv[i], "\" : should be ",
				    "x, rpc, file, timer, idle, all, or none",
				    (char *) NULL);
		  free ((char *) eventv);

		  result = TCL_ERROR;
		  goto done;
		}
	    }

	  if ((none) || (eventc <= 0))
	    events = -1;

	  free ((char *) eventv);
	} 
      else if (strcmp (rpc_argv[0], "-timeout") == 0) 
	{
	  if (Tcl_GetInt (interp, rpc_argv[1], &timeout) != TCL_OK)
	    {
	      result = TCL_ERROR;
	      goto done;
	    }
	}
      else if (strcmp (rpc_argv[0], "-timeoutReturn") == 0) 
	{
	  if (timeoutReturn)
	    ckfree ((char *) timeoutReturn);

	  timeoutReturn = (char *) ckalloc (strlen (rpc_argv[1]) + 1);
	  strcpy (timeoutReturn, rpc_argv[1]);
	}
      else 
	{
	  Tcl_AppendResult (interp, "unknown parameter flag \"",
			    rpc_argv[0], "\" : should be ",
			    "-events, -timeout, or -timeoutReturn",
			    (char *) NULL);
	  result = TCL_ERROR;
	  goto done;
	}

      rpc_argc = rpc_argc - 2;
      rpc_argv = rpc_argv + 2;
    }

    command = Tcl_Merge (rpc_argc, rpc_argv);
    result  = Tcm_RPC (interp, filePtr, command, events, 
		       timeout, timeoutReturn);

  done:
    if (timeoutReturn)
      ckfree ((char *) timeoutReturn);
    if (command)
      ckfree ((char *) command);

    return (result);
}

/*
 *--------------------------------------------------------------
 *
 * Tcm_ProcessRPCCommandCmd --
 *
 *	This procedure is invoked to process the "ProcessRPCCommand"
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
Tcm_ProcessRPCCommandCmd(unused, interp, argc, argv)
    ClientData unused;		/* Unused. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    OpenFile *filePtr;

    if (argc != 3) {
	Tcl_AppendResult (interp, "wrong # args: should be \"",
			  argv[0], " status fileId\"", 
			  (char *) NULL);
	return TCL_ERROR;
    }

    if (TclGetOpenFile (interp, argv[2], &filePtr) != TCL_OK)
	return TCL_ERROR;

    if (argv[1][0] == 'e') 
      {
	int fd;

	fd = fileno (filePtr->f);

	rpcWaiting[fd] = 0;
	rpcResult[fd]  = TCL_ERROR;
	rpcTimeout[fd] = 0;
	rpcTime[fd]    = 0.0;
	    
	if (rpcValue[fd])
	  ckfree ((char *) rpcValue[fd]);
	rpcValue[fd] = NULL;

	if (rpcTimeoutReturn[fd])
	  ckfree ((char *) rpcTimeoutReturn[fd]);
	rpcTimeoutReturn[fd] = NULL;

	sprintf (interp->result,
		 "Error in %s e %s: %s", argv[0], argv[2],
		 Tcl_UnixError (interp));
	fprintf (stderr, "%s\n", interp->result);

	Tcl_VarEval (interp, "filehandler ", argv[2],
		     (char *) NULL);
	Tcl_VarEval (interp, "close ", argv[2],
		     (char *) NULL);

	return TCL_ERROR;
      }
    else
      return (Tcm_ProcessRPCMessages (interp, filePtr, 0));
}

/*
 *--------------------------------------------------------------
 *
 * Tcm_RDO --
 *
 *	This procedure is the C interface to the "RDO" extension.
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
Tcm_RDO (interp, filePtr, command)
    Tcl_Interp *interp;		/* Current interpreter. */
    OpenFile *filePtr;
    char *command;
{
    static char buffer[4096];

    if (strlen(command) + 10 >= 4096) 
      {
	Tcl_AppendResult(interp, "RDO error: command too long",
			 (char *) NULL);
	return TCL_ERROR;
      }

    /*
     * Prepare and send off the RDO command;
     */

    sprintf (buffer, "%c %s", TOK_RDO, command);
    Tcp_PacketSend (interp, filePtr, buffer);

    return (TCL_OK);
}

/*
 *--------------------------------------------------------------
 *
 * Tcm_RDOCmd --
 *
 *	This procedure is invoked to process the "RDO" Tcl
 *	command.  Parses args and calls Tcm_RDO to do the work.
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
Tcm_RDOCmd(unused, interp, argc, argv)
    ClientData unused;		/* Unused. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    OpenFile *filePtr;
    char *command;
    int result;

    if (argc < 3) {
      error_args:
	Tcl_AppendResult (interp, "wrong # args: should be \"", 
			  argv[0], " peer ?-callback callback? ",
			  "command ?args ...?\"",
			  (char *) NULL);
	return TCL_ERROR;
    }

    if (TclGetOpenFile (interp, argv[1], &filePtr) != TCL_OK)
      return TCL_ERROR;

    if ((argv[2][0] == '-') &&
	(strcmp (argv[2], "-callback") == 0))
      {
	char *cmd;
	char *callback;
	int   callbackLen;

	if (argc < 5)
	  goto error_args;

	/*  Process -callback parameter;
	 */

	callback    = argv[3];
	callbackLen = strlen (callback);

	cmd = Tcl_Merge (argc - 4, argv + 4);

	command = (char *) ckalloc (strlen (cmd) + callbackLen + 100);
	sprintf (command, 
		 "eval \"global rpcFile; RDO $rpcFile %s [%s]\"",
		 callback, cmd);

	ckfree ((char *) cmd);
      }
    else
      command = Tcl_Merge (argc - 2, argv + 2);

    result = Tcm_RDO (interp, filePtr, command);

    ckfree ((char *) command);
    return (result);
}

/*
 *--------------------------------------------------------------
 *
 * Tcm_CancelRPCCmd --
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
Tcm_CancelRPCCmd (unused, interp, argc, argv)
    ClientData unused;		/* Unused. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    OpenFile *filePtr;

    if (argc < 1) {
      error_args:
	Tcl_AppendResult (interp, "wrong # args: should be \"", 
			  argv[0], " ?fileId? ?fileId? ...",
			  (char *) NULL);
	return TCL_ERROR;
    }

    if (argc == 1)
      {
	int i;
	for (i = 0; i < 256; i++)
	  {
	    rpcWaiting[i] = 0;
	    rpcResult[i]  = TCL_ERROR;

	    if (rpcValue[i])
	      ckfree ((char *) rpcValue[i]);
	    rpcValue[i] = NULL;

	    if (rpcTimeoutReturn[i])
	      ckfree ((char *) rpcTimeoutReturn[i]);
	    rpcTimeoutReturn[i] = NULL;

	    rpcTimeout[i] = 0;
	    rpcTime[i]    = 0.0;
	  }

	return TCL_OK;
      }

    argc = argc - 1;
    argv = argv + 1;

    while (argc > 0)
      {
	int fd;

	if (TclGetOpenFile (interp, argv[0], &filePtr) != TCL_OK)
	  return TCL_ERROR;

	fd = fileno (filePtr->f);

	rpcWaiting[fd] = 0;
	rpcResult[fd]  = TCL_ERROR;

	if (rpcValue[fd])
	  ckfree ((char *) rpcValue[fd]);
	rpcValue[fd] = NULL;
	
	if (rpcTimeoutReturn[fd])
	  ckfree ((char *) rpcTimeoutReturn[fd]);
	rpcTimeoutReturn[fd] = NULL;
	
	rpcTimeout[fd] = 0;
	rpcTime[fd]    = 0.0;

	argc = argc - 1;
	argv = argv + 1;
      }

    return (TCL_OK);
}

/*
 *--------------------------------------------------------------
 *
 * Tcm_RPCInit --
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
Tcm_RPCInit(interp)
    Tcl_Interp *interp;		/* Current interpreter. */
{
    Tcl_CreateCommand(interp, "RDO",
		      Tcm_RDOCmd,
		      (ClientData) NULL, (void (*) ()) NULL);
    Tcl_CreateCommand(interp, "RPC",
		      Tcm_RPCCmd,
		      (ClientData) NULL, (void (*) ()) NULL);
    Tcl_CreateCommand(interp, "CancelRPC",
		      Tcm_CancelRPCCmd,
		      (ClientData) NULL, (void (*) ()) NULL);
    Tcl_CreateCommand(interp, "ProcessRPCMessages",
		      Tcm_ProcessRPCMessagesCmd,
		      (ClientData) NULL, (void (*) ()) NULL);
    Tcl_CreateCommand(interp, "ProcessRPCCommand",
		      Tcm_ProcessRPCCommandCmd,
		      (ClientData) NULL, (void (*) ()) NULL);
    Tcl_CreateCommand(interp, "ReceiveRPC",
		      Tcm_ReceiveRPCCmd,
		      (ClientData) NULL, (void (*) ()) NULL);

    Tcl_SetVar(interp, "rpcFile", "", TCL_GLOBAL_ONLY);

    return TCL_OK;
}
