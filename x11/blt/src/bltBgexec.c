
/*
 * bltBgexec.c --
 *
 *	This module implements a background "exec" command for the
 *	Tk toolkit.
 *
 * Copyright 1993-1994 by AT&T Bell Laboratories.
 * Permission to use, copy, modify, and distribute this software
 * and its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice appear in all
 * copies and that both that the copyright notice and warranty
 * disclaimer appear in supporting documentation, and that the
 * names of AT&T Bell Laboratories any of their entities not be used
 * in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 *
 * AT&T disclaims all warranties with regard to this software, including
 * all implied warranties of merchantability and fitness.  In no event
 * shall AT&T be liable for any special, indirect or consequential
 * damages or any damages whatsoever resulting from loss of use, data
 * or profits, whether in an action of contract, negligence or other
 * tortuous action, arising out of or in connection with the use or
 * performance of this software.
 *
 * bgexec command created by George Howlett.
 */

#include "blt.h"

#include <fcntl.h>
#include <signal.h>
#include <sys/param.h>
#include <sys/types.h>
#ifdef HAVE_SYS_WAIT_H
#   include <sys/wait.h>
#endif

/* The wait-related definitions are taken from tclUnix.h */

/*
 * Not all systems declare the errno variable in errno.h. so this
 * file does it explicitly.  The list of system error messages also
 * isn't generally declared in a header file anywhere.
 */

extern int errno;

/*
 * The type of the status returned by wait varies from UNIX system
 * to UNIX system.  The macro below defines it:
 */

#ifdef AIX
#   define WAIT_STATUS_TYPE pid_t
#else
#ifndef NO_UNION_WAIT
#   define WAIT_STATUS_TYPE union wait
#else
#   define WAIT_STATUS_TYPE int
#endif
#endif

/*
 * Supply definitions for macros to query wait status, if not already
 * defined in header files above.
 */

#ifndef WIFEXITED
#   define WIFEXITED(stat)  (((*((int *) &(stat))) & 0xff) == 0)
#endif

#ifndef WEXITSTATUS
#   define WEXITSTATUS(stat) (((*((int *) &(stat))) >> 8) & 0xff)
#endif

#ifndef WIFSIGNALED
#   define WIFSIGNALED(stat) (((*((int *) &(stat)))) && ((*((int *) &(stat))) == ((*((int *) &(stat))) & 0x00ff)))
#endif

#ifndef WTERMSIG
#   define WTERMSIG(stat)    ((*((int *) &(stat))) & 0x7f)
#endif

#ifndef WIFSTOPPED
#   define WIFSTOPPED(stat)  (((*((int *) &(stat))) & 0xff) == 0177)
#endif

#ifndef WSTOPSIG
#   define WSTOPSIG(stat)    (((*((int *) &(stat))) >> 8) & 0xff)
#endif

#ifndef BGEXEC_VERSION
#define BGEXEC_VERSION "1.4"
#endif

#define BUFFER_SIZE	1000	/* Maximum number of bytes per read */
#define MAX_READS       100	/* Maximum number of successful reads
			         * before stopping to let Tk catch up
			         * on events */

typedef struct {
    char *storage;		/* Buffer to store command output
				 * (malloc-ed): Initially points to
				 * static storage */
    unsigned int used;		/* Number of characters read into the
				 * buffer */
    unsigned int size;		/* Size of buffer allocated */
    char staticSpace[BUFFER_SIZE * 2 + 1];	/* Static buffer space */

} Buffer;

typedef struct {
    Tcl_Interp *interp;		/* Interpreter containing variable */

    char *updateName;		/* Name of a Tcl variable (malloc'ed)
				 * to be updated when no more data is
				 * currently available for reading
				 * from the output pipe.  It's
				 * appended with the contents of the
				 * current buffer (data which has
				 * arrived since the last idle
				 * point). If it's NULL, no updates
				 * are made */

    char *outputName;		/* Name of a Tcl variable (malloc'ed)
				 * to be set with the contents of
				 * stdout after the last UNIX
				 * subprocess has completed. Setting
				 * this variable triggers the
				 * termination of all subprocesses,
				 * regardless whether they have
				 * already completed or not */

    char *errorName;		/* Name of a Tcl variable (malloc'ed)
				 * to hold any available data from
				 * standard error */

    char *statusName;		/* Name of a Tcl variable (malloc'ed)
				 * to hold exit status of the last
				 * process.
				 */

    Tk_TimerToken timerToken;	/* Token for timer handler which polls
				 * for the exit status of each
				 * sub-process. If zero, there's no
				 * timer handler queued. */

    int outputId;		/* File descriptor for output pipe.  */
    int errorId;		/* File Descriptor for error file. */

    Buffer buffer;		/* Buffer storing subprocess' stdin/stderr */

    unsigned int numPids;	/* Number of processes created in pipeline */
    int *pidPtr;		/* Array of process Ids. */

    unsigned int sigNum;	/* If non-zero, indicates signal to send
				 * subprocesses when cleaning up.*/
    int keepFlag;		/* Indicates to set Tcl output
				 * variables with trailing newlines
				 * intact */
    unsigned int lastCount;	/* Number of bytes read the last time a
				 * buffer was retrieved */
    int fixMark;		/* Index of fixed newline character in
				 * buffer.  If -1, no fix was made. */

} BackgroundInfo;

/*
 *----------------------------------------------------------------------
 *
 * GetBuffer --
 *
 *	Returns the output currently saved in buffer storage
 *
 *----------------------------------------------------------------------
 */
static char *
GetBuffer(bufferPtr)
    Buffer *bufferPtr;
{
    bufferPtr->storage[bufferPtr->used] = '\0';
    return (bufferPtr->storage);
}

/*
 *----------------------------------------------------------------------
 *
 * InitBuffer --
 *
 *	Initializes the buffer storage, clearing any output that may
 *	have accumulated from previous usage.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Buffer storage is cleared.
 *
 *---------------------------------------------------------------------- */
static void
InitBuffer(bufferPtr)
    Buffer *bufferPtr;
{
    bufferPtr->storage = bufferPtr->staticSpace;
    bufferPtr->size = BUFFER_SIZE * 2;
    bufferPtr->storage[0] = '\0';
    bufferPtr->used = 0;
}

/*
 *----------------------------------------------------------------------
 *
 * ResetBuffer --
 *
 *	Resets the buffer storage, freeing any malloc'ed space.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static void
ResetBuffer(bufferPtr)
    Buffer *bufferPtr;
{
    if (bufferPtr->storage != bufferPtr->staticSpace) {
	free(bufferPtr->storage);
    }
    InitBuffer(bufferPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * GrowBuffer --
 *
 *	Doubles the size of the current buffer.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
static int
GrowBuffer(bufferPtr)
    Buffer *bufferPtr;
{
    char *newPtr;

    /*
     * Allocate a new buffer, double the old size
     */

    bufferPtr->size += bufferPtr->size;
    newPtr = (char *)malloc(sizeof(char) * (bufferPtr->size + 1));
    if (newPtr == NULL) {
	return TCL_ERROR;
    }
    strcpy(newPtr, bufferPtr->storage);
    if (bufferPtr->storage != bufferPtr->staticSpace) {
	free((char *)bufferPtr->storage);
    }
    bufferPtr->storage = newPtr;
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * AppendOutputToBuffer --
 *
 *	Appends any available data from a given file descriptor to the
 *	buffer.
 *
 * Results:
 *	Returns TCL_OK when EOF is found, TCL_RETURN if reading
 *	data would block, and TCL_ERROR if an error occured.
 *
 *----------------------------------------------------------------------
 */
static int
AppendOutputToBuffer(f, bufferPtr)
    int f;
    Buffer *bufferPtr;
{
    int numBytes, bytesLeft;
    register int i, n;
    char *array;

    /*
     * ------------------------------------------------------------------
     *
     * Worry about indefinite postponement.
     *
     * Typically we want to stay in the read loop as long as it takes
     * to collect all the data that's currently available.  But if
     * it's coming in at a constant high rate, we need to arbitrarily
     * break out at some point. This allows for both setting the
     * output variable and the Tk program to handle idle events.
     *
     * ------------------------------------------------------------------
     */

    for (i = 0; i < MAX_READS; i++) {

	/*
	 * Allocate a larger buffer when the number of remaining bytes
	 * is below a threshold (BUFFER_SIZE).
	 */

	bytesLeft = bufferPtr->size - bufferPtr->used;
	if (bytesLeft < BUFFER_SIZE) {
	    GrowBuffer(bufferPtr);
	    bytesLeft = bufferPtr->size - bufferPtr->used;
	}
        array = bufferPtr->storage + bufferPtr->used;
	numBytes = read(f, array, bytesLeft);

	if (numBytes == 0) {	/* EOF: break out of loop. */
	    return TCL_OK;
	}
	if (numBytes < 0) {

	    /*
	     * Either an error has occurred or no more data is
	     * currently available to read.
	     */
#ifdef O_NONBLOCK
	    if (errno == EAGAIN) {
#else
	    if (errno == EWOULDBLOCK) {
#endif /*O_NONBLOCK*/
		break;
	    }
	    bufferPtr->storage[0] = '\0';
	    return TCL_ERROR;
	}
        /* Clean out NUL bytes, make spaces */
        for (n = 0; n < numBytes; n++) {
	    if (array[n] == 0) {
		array[n] = ' ';
	    }
	}
	bufferPtr->used += numBytes;
	bufferPtr->storage[bufferPtr->used] = '\0';
    }
    return TCL_RETURN;
}

/*
 *----------------------------------------------------------------------
 *
 * FixNewline --
 *
 *	Clips off the trailing newline in the buffer (if one exists).
 *	Saves the location in the buffer where the fix was made.
 *
 *---------------------------------------------------------------------- */
static void
FixNewline(infoPtr)
    BackgroundInfo *infoPtr;
{
    Buffer *bufferPtr = &(infoPtr->buffer);

    infoPtr->fixMark = -1;
    if (bufferPtr->used > 0) {
	int mark = bufferPtr->used - 1;

	if (bufferPtr->storage[mark] == '\n') {
	    bufferPtr->storage[mark] = '\0';
	    infoPtr->fixMark = mark;
	}
    }
}

/*
 *----------------------------------------------------------------------
 *
 * UnfixNewline --
 *
 *	Restores the previously clipped newline in the buffer.
 *	The fixMark field indicates whether one was clipped.
 *
 *----------------------------------------------------------------------
 */
static void
UnfixNewline(infoPtr)
    BackgroundInfo *infoPtr;
{
    Buffer *bufferPtr = &(infoPtr->buffer);

    if (infoPtr->fixMark >= 0) {
	bufferPtr->storage[infoPtr->fixMark] = '\n';
	infoPtr->fixMark = -1;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * GetLastAppended --
 *
 *	Returns the output saved from the last time this routine
 *	was called.
 *
 *----------------------------------------------------------------------
 */
static char *
GetLastAppended(infoPtr)
    BackgroundInfo *infoPtr;
{
    Buffer *bufferPtr = &(infoPtr->buffer);
    char *string;

    bufferPtr->storage[bufferPtr->used] = '\0';
    string = bufferPtr->storage + infoPtr->lastCount;
    infoPtr->lastCount = bufferPtr->used;
    return (string);
}

/*
 *----------------------------------------------------------------------
 *
 * DestroyBackgroundInfo --
 *
 * 	This procedure is invoked by Tk_EventuallyFree or Tk_Release
 * 	to clean up the internal structure (BackgroundInfo) at a safe
 * 	time (when no-one is using it anymore).
 *
 *	Right now, our only concern is protecting infoPtr->outputName,
 *	since subsequent calls to trace procedures (via CallTraces)
 *	may still use it (as part1 and possibly part2).
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	The memory allocated to the BackgroundInfo structure released.
 *
 *---------------------------------------------------------------------- */

 /* ARGSUSED */
static void
DestroyBackgroundInfo(clientData)
    ClientData clientData;	/* Background info record. */
{
    BackgroundInfo *infoPtr = (BackgroundInfo *)clientData;

    ResetBuffer(&(infoPtr->buffer));
    if (infoPtr->updateName != NULL) {
	free(infoPtr->updateName);
    }
    if (infoPtr->outputName != NULL) {
	free(infoPtr->outputName);
    }
    if (infoPtr->errorName != NULL) {
	free(infoPtr->errorName);
    }
    if (infoPtr->statusName != NULL) {
	free(infoPtr->statusName);
    }
    if (infoPtr->pidPtr != NULL) {
	if (infoPtr->numPids > 0) {
	    Tcl_DetachPids(infoPtr->numPids, infoPtr->pidPtr);
	}
	Tcl_ReapDetachedProcs();
	free((char *)infoPtr->pidPtr);
    }
    free((char *)infoPtr);
}

/*
 * ----------------------------------------------------------------------
 *
 * CleanupProc --
 *
 *	This procedure cleans up the BackgroundInfo data structure
 *	associated with the detached subprocesses.  It is called when
 *	the variable associated with UNIX subprocesses has been
 *	overwritten.  This usually occurs when the subprocesses have
 *	completed or an error was detected.  However, it may be used
 *	to terminate the detached processes from the Tcl program by
 *	setting the associated variable.
 *
 * Results:
 *	Always returns NULL.  Only called from a variable trace.
 *
 * Side effects:
 *	The output descriptor is closed and the variable trace is
 *	deleted.  In addition, the subprocesses are signaled for
 *	termination.
 *
 * ---------------------------------------------------------------------- 
 */
static char *
CleanupProc(clientData, interp, part1, part2, flags)
    ClientData clientData;	/* File output information. */
    Tcl_Interp *interp;
    char *part1, *part2;
    int flags;
{
    BackgroundInfo *infoPtr = (BackgroundInfo *)clientData;

    if (!(flags & (TCL_TRACE_WRITES | TCL_GLOBAL_ONLY))) {
	return NULL;
    }
    if (infoPtr->outputId != -1) {
	close(infoPtr->outputId);
	Tk_DeleteFileHandler(infoPtr->outputId);
    }
    if (infoPtr->timerToken != (Tk_TimerToken) 0) {
	Tk_DeleteTimerHandler(infoPtr->timerToken);
    }
    if (infoPtr->errorId >= 0) {

	/*
	 * If an error variable needs to be set, reset the error file
	 * descriptor and read the captured stderr from the temporary
	 * file
	 */

	if ((infoPtr->errorName != NULL) &&
	    (lseek(infoPtr->errorId, 0L, 0) >= 0)) {
	    int result;

	    ResetBuffer(&(infoPtr->buffer));
	    do {
		result = AppendOutputToBuffer(infoPtr->errorId,
		    &(infoPtr->buffer));
	    } while (result == TCL_RETURN);

	    if (result == TCL_OK) {
		if (!infoPtr->keepFlag) {
		    FixNewline(infoPtr);
		}
		Tcl_SetVar(infoPtr->interp, infoPtr->errorName,
		    GetBuffer(&(infoPtr->buffer)), TCL_GLOBAL_ONLY);
	    } else if (result == TCL_ERROR) {
		Tcl_AppendResult(infoPtr->interp, "error appending buffer: ",
		    Tcl_PosixError(infoPtr->interp), (char *)NULL);
		Tk_BackgroundError(infoPtr->interp);
	    }
	}
	close(infoPtr->errorId);
    }
    Tcl_UntraceVar2(interp, part1, part2, flags, CleanupProc, clientData);

    if ((infoPtr->pidPtr != NULL) && (infoPtr->sigNum > 0)) {
	register int i;

	for (i = 0; i < infoPtr->numPids; i++) {
	    kill(infoPtr->pidPtr[i], (int)infoPtr->sigNum);
	}
    }
    Tk_EventuallyFree((ClientData)infoPtr, DestroyBackgroundInfo);
    return NULL;
}

/*
 *----------------------------------------------------------------------
 *
 * StatusProc --
 *
 *	This is a timer handler procedure which gets called
 *	periodically to reap any of the sub-processes if they have
 *	terminated.  After the last process has terminated, the
 *	contents of standard output (saved in infoPtr->buffer) are
 *	stored in the output variable, which triggers the cleanup
 *	proc (using a variable trace). If the status variable is
 *	active (infoPtr->statusName != NULL), then set the status the
 *	last process to exit in the status variable.
 *
 * Results:
 *	None.  Called from the Tk event loop.
 *
 * Side effects:
 *	Many. The contents of pidPtr is shifted, leaving only those
 *	sub-processes which have not yet terminated.  If there are
 *	still subprocesses left, this procedure is placed in the timer
 *	queue again. Otherwise the output and possibly the status
 *	variables are updated.  The former triggers the cleanup
 *	routine which will destroy the information and resources
 *	associated with these background processes.
 *
 *---------------------------------------------------------------------- 
 */
static void
StatusProc(clientData)
    ClientData clientData;
{
    BackgroundInfo *infoPtr = (BackgroundInfo *)clientData;
    register int i;
    int result;
    WAIT_STATUS_TYPE waitStatus;
    char *statusMesg, *statusInfo;
    int numLeft;		/* Number of processes still not reaped */

#ifdef notdef
    fprintf(stderr, "in StatusProc(numPids=%d)\n", infoPtr->numPids);
#endif
    numLeft = 0;
    for (i = 0; i < infoPtr->numPids; i++) {
	result = waitpid(infoPtr->pidPtr[i], (int *)&waitStatus, WNOHANG);
	if ((result == 0) || ((result == -1) && (errno != ECHILD))) {
	    if (numLeft < i) {
		infoPtr->pidPtr[numLeft] = infoPtr->pidPtr[i];
	    }
	    numLeft++;
	    continue;
	}
	/*
	 * Collect the status information associated with the subprocess.
	 * We'll use it only if this is the last subprocess to be reaped.
	 */
	if (WIFEXITED(waitStatus)) {
	    statusInfo = "Child completed";
	    statusMesg = "CHILDSTATUS";
	} else if (WIFSIGNALED(waitStatus)) {
	    statusInfo = Tcl_SignalMsg((int)(WTERMSIG(waitStatus)));
	    statusMesg = "CHILDKILLED";
	} else if (WIFSTOPPED(waitStatus)) {
	    statusInfo = Tcl_SignalMsg((int)(WSTOPSIG(waitStatus)));
	    statusMesg = "CHILDSUSP";
	} else {
	    statusMesg = "UNKNOWN";
	    statusInfo = "Child status unknown";
	}
    }

    infoPtr->numPids = numLeft;
    if (numLeft > 0) {
	/* Keep polling for the status of the children that are left */
	infoPtr->timerToken = Tk_CreateTimerHandler(1000, StatusProc,
	    (ClientData)infoPtr);
    } else {
	if (infoPtr->statusName != NULL) {
	    Tcl_DString buffer;
	    char string[20];

	    /*
	     * Set the status variable with the status of the last
	     * process reaped.  The status is a list of an error token,
	     * the exit status, and a message.
	     */

	    Tcl_DStringInit(&buffer);
	    Tcl_DStringAppendElement(&buffer, statusMesg);
	    sprintf(string, "%d", WEXITSTATUS(waitStatus));
	    Tcl_DStringAppendElement(&buffer, string);
	    Tcl_DStringAppendElement(&buffer, statusInfo);

	    Tcl_SetVar(infoPtr->interp, infoPtr->statusName,
		Tcl_DStringValue(&buffer), TCL_GLOBAL_ONLY);
	    Tcl_DStringFree(&buffer);
	}
	/*
	 * Setting the output variable also triggers the cleanup
	 * procedure which frees memory and destroys the variable
	 * traces.
	 */
	Tk_Preserve((ClientData)infoPtr);
	Tcl_SetVar(infoPtr->interp, infoPtr->outputName,
	    GetBuffer(&(infoPtr->buffer)), TCL_GLOBAL_ONLY);
	Tk_Release((ClientData)infoPtr);
    }
}

/*
 *----------------------------------------------------------------------
 *
 * BackgroundProc --
 *
 *	This procedure is called when output from the detached command
 *	is available.  The output is read and saved in a buffer in the
 *	BackgroundInfo structure.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Data is stored in infoPtr->buffer.  This character array may
 *	be increased as more space is required to contain the output
 *	of the command.
 *
 *---------------------------------------------------------------------- */
 /* ARGSUSED */
static void
BackgroundProc(clientData, mask)
    ClientData clientData;	/* File output information. */
    int mask;			/* Not used. */
{
    BackgroundInfo *infoPtr = (BackgroundInfo *)clientData;
    int result;

    result = AppendOutputToBuffer(infoPtr->outputId, &(infoPtr->buffer));
    if (result == TCL_RETURN) {
	if (infoPtr->updateName != NULL) {
	    if (!infoPtr->keepFlag) {
		FixNewline(infoPtr);
	    }
	    Tcl_SetVar(infoPtr->interp, infoPtr->updateName,
		GetLastAppended(infoPtr),(TCL_GLOBAL_ONLY | TCL_APPEND_VALUE));
	    if (!infoPtr->keepFlag) {
		UnfixNewline(infoPtr);
	    }
	}
	return;
    }
    if (result == TCL_ERROR) {
	Tcl_AppendResult(infoPtr->interp, "error appending buffer: ",
	    Tcl_PosixError(infoPtr->interp), (char *)NULL);
	Tk_BackgroundError(infoPtr->interp);
    }
    if (!infoPtr->keepFlag) {
	FixNewline(infoPtr);
    }
    if (infoPtr->updateName != NULL) {
	Tcl_SetVar(infoPtr->interp, infoPtr->updateName,
	    GetLastAppended(infoPtr), (TCL_GLOBAL_ONLY | TCL_APPEND_VALUE));
    }
    /*
     * We're here if we've seen EOF or an error has occurred.  In
     * either case, set up a timer handler to periodically poll for
     * exit status of each process.  Initially check at the next idle
     * interval.
     */

    infoPtr->timerToken = Tk_CreateTimerHandler(0, StatusProc,
	(ClientData)infoPtr);

    /* Delete the file handler and close the file descriptor. */

    close(infoPtr->outputId);
    Tk_DeleteFileHandler(infoPtr->outputId);
    infoPtr->outputId = -1;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_BgExecCmd --
 *
 *	This procedure is invoked to process the "bgexec" Tcl command.
 *	See the user documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *---------------------------------------------------------------------- 
 */
 /* ARGSUSED */
static int
BgExecCmd(clientData, interp, argc, argv)
    ClientData clientData;	/* Not used. */
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    int outputId;		/* File id for output pipe.  -1
				 * means command overrode. */
    int errorId = -1;
    int *errFilePtr;
    int *pidPtr;
    int numPids;
    BackgroundInfo *infoPtr;
    register int i;
    int parseSwitches;

    if (argc < 3) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " ?switches? varName command args", (char *)NULL);
	return TCL_ERROR;
    }
    infoPtr = (BackgroundInfo *)malloc(sizeof(BackgroundInfo));
    if (infoPtr == NULL) {
	interp->result = "can't allocate file info structure";
	return TCL_ERROR;
    }
    /* Initialize the background information structure */

    infoPtr->interp = interp;
    infoPtr->timerToken = (Tk_TimerToken) 0;
    infoPtr->keepFlag = 0;
    infoPtr->sigNum = SIGHUP;
    infoPtr->errorName = NULL;
    infoPtr->updateName = NULL;
    infoPtr->statusName = NULL;
    infoPtr->outputName = NULL;
    InitBuffer(&(infoPtr->buffer));

    parseSwitches = 1;
    errFilePtr = NULL;		/* By default, stderr goes to the tty */

    for (i = 1; i < argc; i++) {
	if ((parseSwitches) && (argv[i][0] == '-')) {
	    register char *swtch;
	    int length;
	    char c;

	    swtch = argv[i];
	    length = strlen(swtch);
	    c = swtch[1];

	    if ((c == '-') && (swtch[2] == '\0')) {
		parseSwitches = 0;
		continue;
	    } else if ((c == 'k') && (length > 1) &&
		(strncmp(swtch, "-keepnewline", length) == 0)) {
		infoPtr->keepFlag = 1;
		continue;
	    }
	    i++;

	    if (i == argc) {
		Tcl_AppendResult(interp, "missing value for \"", swtch, "\"",
		    (char *)NULL);
		goto error;
	    }
	    if ((c == 'u') && (strncmp(swtch, "-updatevar", length) == 0)) {
		infoPtr->updateName = strdup(argv[i]);
	    } else if ((c == 'e') &&
		(strncmp(swtch, "-errorvar", length) == 0)) {
		infoPtr->errorName = strdup(argv[i]);
		errFilePtr = &errorId;
	    } else if ((c == 'o') &&
		(strncmp(swtch, "-outputvar", length) == 0)) {
		infoPtr->outputName = strdup(argv[i]);
	    } else if ((c == 's') &&
		(strncmp(swtch, "-statusvar", length) == 0)) {
		infoPtr->statusName = strdup(argv[i]);
	    } else if ((c == 'k') && (length > 1) &&
		(strncmp(swtch, "-killsignal", length) == 0)) {
		int value;

		if (Tcl_GetInt(interp, argv[i], &value) != TCL_OK) {
		    goto error;
		}
		if ((value < 0) || (value > 31)) {
		    Tcl_AppendResult(interp, "bad kill signal number \"",
			argv[i], "\"", (char *)NULL);
		}
		infoPtr->sigNum = (unsigned int)value;
	    } else {
		Tcl_AppendResult(interp, "bad switch \"", swtch, "\": ",
		    "should be -errorvar, -keepnewline, -killsignal, ",
		    "-outputvar, -updatevar, or --", (char *)NULL);
		goto error;
	    }
	} else {
	    if (infoPtr->outputName == NULL) {
		infoPtr->outputName = strdup(argv[i++]);
	    }
	    break;
	}
    }
    if ((infoPtr->outputName == NULL) || (argc == i)) {
	Tcl_AppendResult(interp, "missing command: should be \"", argv[0],
	    " ?switches? varName command ?args?\"", (char *)NULL);
	goto error;
    }
    numPids = Tcl_CreatePipeline(interp, argc - i, argv + i, &pidPtr,
	(int *)NULL, &outputId, errFilePtr);
    if (numPids < 0) {
	goto error;
    }
    infoPtr->outputId = outputId;
    infoPtr->errorId = errorId;
    infoPtr->numPids = (unsigned int)numPids;
    infoPtr->pidPtr = pidPtr;
    infoPtr->lastCount = 0;
    infoPtr->fixMark = -1;

    /*
     * Put a trace on the output variable.  The will also allow the
     * subprocesses to be terminated prematurely by the user.
     */
    Tcl_TraceVar(interp, infoPtr->outputName,
	(TCL_TRACE_WRITES | TCL_GLOBAL_ONLY), CleanupProc,
	(ClientData)infoPtr);

    if (outputId == -1) {

	/*
	 * If output has been redirected, start polling immediately
	 * for the exit status of each process.  Normally, this is
	 * delayed until after standard output has been closed by the
	 * last process.  I'm guessing about the timer interval.
	 */

	infoPtr->timerToken = Tk_CreateTimerHandler(1000, StatusProc,
	    (ClientData)infoPtr);
    } else {

	/* 
	 * Make the output descriptor non-blocking and associate it
	 * with a file handler routine 
	 */

#ifdef O_NONBLOCK
	fcntl(outputId, F_SETFL, O_NONBLOCK);
#else
	fcntl(outputId, F_SETFL, O_NDELAY);
#endif
	Tk_CreateFileHandler(outputId, TK_READABLE, BackgroundProc,
	    (ClientData)infoPtr);
    }

    return TCL_OK;
  error:
    if (infoPtr != NULL) {
	free((char *)infoPtr);
    }
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_BgExecInit --
 *
 *	This procedure is invoked to initialize the "bgexec" Tcl
 *	command.  See the user documentation for details on what it
 *	does.
 *
 * Results:
 *	Nothing.
 *
 * Side effects:
 *	See the user documentation.
 *
 *---------------------------------------------------------------------- */
int
Blt_BgExecInit(interp)
    Tcl_Interp *interp;
{
    Tk_Window tkwin;

    if (Blt_FindCmd(interp, "blt_bgexec", (ClientData *)NULL) == TCL_OK) {
	Tcl_AppendResult(interp, "\"blt_bgexec\" command already exists",
	    (char *)NULL);
	return TCL_ERROR;
    }
    tkwin = Tk_MainWindow(interp);
    if (tkwin == NULL) {
	Tcl_AppendResult(interp, "\"blt_bgexec\" requires Tk", (char *)NULL);
	return TCL_ERROR;
    }
    Tcl_SetVar2(interp, "blt_versions", "blt_bgexec", BGEXEC_VERSION,
	TCL_GLOBAL_ONLY);
    Tcl_CreateCommand(interp, "blt_bgexec", BgExecCmd, (ClientData)tkwin,
	(Tcl_CmdDeleteProc *)NULL);
    return TCL_OK;
}
