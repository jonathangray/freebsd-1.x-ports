/* 
 * system.c --
 *
 *  Version of "system" library routine for versions of Unix that have a
 * system function that uses wait instead of waitpid.  These versions are
 * broken, file a bug report with your vendor and use this one.  If your
 * Unix does not have waitpid, this uses the simuated one supplied with
 * UCB Tcl.
 *-----------------------------------------------------------------------------
 * Copyright 1991-1993 Karl Lehenbauer and Mark Diekhans.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies.  Karl Lehenbauer and
 * Mark Diekhans make no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without express or
 * implied warranty.
 *-----------------------------------------------------------------------------
 * $Id: system.c,v 1.1 1994/02/09 01:53:38 jkh Exp $
 *-----------------------------------------------------------------------------
 */

#include "tclExtdInt.h"

/*
 *-----------------------------------------------------------------------------
 *
 * system --
 *     Does the equivalent of the Unix "system" library call, but uses waitpid
 * to wait on the correct process, rather than waiting on all processes and
 * throwing the exit statii away for the processes it isn't interested in.
 *-----------------------------------------------------------------------------
 */
int 
system (command)
    CONST char  *command;
{
    pid_t            processID;
    WAIT_STATUS_TYPE processStatus;

    processID = fork ();
    if (processID < 0)
        return -1;

    if (processID == 0) {
        execl ("/bin/sh", "sh", "-c", command, (char *) NULL);
        _exit (256);
    }

    /*
     * Parent process.
     */
    if (waitpid (processID, &processStatus, 0) == -1)
        return -1;

    return (WEXITSTATUS (processStatus));
}
