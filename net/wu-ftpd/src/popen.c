/* Copyright (c) 1988 The Regents of the University of California. All rights
 * reserved.
 *
 * This code is derived from software written by Ken Arnold and published in
 * UNIX Review, Vol. 6, No. 8.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met: 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer. 2.
 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution. 3. All advertising
 * materials mentioning features or use of this software must display the
 * following acknowledgement: This product includes software developed by the
 * University of California, Berkeley and its contributors. 4. Neither the
 * name of the University nor the names of its contributors may be used to
 * endorse or promote products derived from this software without specific
 * prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */

#ifndef lint
static char sccsid[] = "@(#)popen.c 5.9 (Berkeley) 2/25/91";
#endif /* not lint */

#include "config.h"

#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#ifdef SVR4
#include <sys/resource.h>
#endif

/* Special version of popen which avoids call to shell.  This insures noone
 * may create a pipe to a hidden program as a side effect of a list or dir
 * command. */
static int *pids;
static int fds;

FILE *
ftpd_popen(char *program, char *type, int closestderr)
{
    register char *cp;
    FILE *iop;
    int argc,
      gargc,
      pdes[2],
      pid;
    char **pop,
     *argv[100],
     *gargv[1000],
     *vv[2];
    extern char **ftpglob(register char *v),
    **copyblk(register char **v),
     *strspl(register char *cp, register char *dp);

    if (*type != 'r' && *type != 'w' || type[1])
        return (NULL);

    if (!pids) {
#ifndef HAVE_GETDTABLESIZE
        struct rlimit rlp;

		rlp.rlim_cur = rlp.rlim_max = RLIM_INFINITY;
		if (getrlimit( RLIMIT_NOFILE, &rlp ) )
			return(NULL);
		fds = rlp.rlim_cur;
#else
        if ((fds = getdtablesize()) <= 0)
            return (NULL);
#endif

        if ((pids = (int *) malloc((u_int) (fds * sizeof(int)))) == NULL)
              return (NULL);
#ifdef USG
        (void) memset((char *)pids, fds * sizeof(int), 0);
#else
        bzero((char *) pids, fds * sizeof(int));
#endif
    }
    if (pipe(pdes) < 0)
        return (NULL);

    /* break up string into pieces */
    for (argc = 0, cp = program;; cp = NULL)
        if (!(argv[argc++] = strtok(cp, " \t\n")))
            break;

    /* glob each piece */
    gargv[0] = argv[0];
    for (gargc = argc = 1; argv[argc]; argc++) {
        if (!(pop = ftpglob(argv[argc]))) { /* globbing failed */
            vv[0] = strspl(argv[argc], "");
            vv[1] = NULL;
            pop = copyblk(vv);
        }
        argv[argc] = (char *) pop;  /* save to free later */
        while (*pop && gargc < 1000)
            gargv[gargc++] = *pop++;
    }
    gargv[gargc] = NULL;

    iop = NULL;
    switch (pid = vfork()) {
    case -1:                    /* error */
        (void) close(pdes[0]);
        (void) close(pdes[1]);
        goto pfree;
        /* NOTREACHED */
    case 0:                 /* child */
        if (*type == 'r') {
            if (pdes[1] != 1) {
                dup2(pdes[1], 1);
                if (closestderr)
                    (void) close(2);
                else 
                    dup2(pdes[1], 2);  /* stderr, too! */
                (void) close(pdes[1]);
            }
            (void) close(pdes[0]);
        } else {
            if (pdes[0] != 0) {
                dup2(pdes[0], 0);
                (void) close(pdes[0]);
            }
            (void) close(pdes[1]);
        }
        execv(gargv[0], gargv);
        _exit(1);
    }
    /* parent; assume fdopen can't fail...  */
    if (*type == 'r') {
        iop = fdopen(pdes[0], type);
        (void) close(pdes[1]);
    } else {
        iop = fdopen(pdes[1], type);
        (void) close(pdes[0]);
    }
    pids[fileno(iop)] = pid;

  pfree:for (argc = 1; argv[argc] != NULL; argc++) {
        blkfree((char **) argv[argc]);
        free((char *) argv[argc]);
    }
    return (iop);
}

ftpd_pclose(FILE * iop)
{
    register int fdes;
    int pid;
#if defined (SVR4)
    sigset_t sig, omask;
    int stat_loc;
    sigemptyset(&sig);
    sigaddset(&sig, SIGINT); sigaddset(&sig,SIGQUIT); sigaddset(&sig, SIGHUP);
#elif defined (_OSF_SOURCE)
    int omask;
    int status;
#elif defined(M_UNIX)
	int stat_loc;
	sigset_t oldmask, newmask;
#else
    int omask;
    union wait stat_loc;
#endif


    /* pclose returns -1 if stream is not associated with a `popened'
     * command, or, if already `pclosed'. */
    if (pids == 0 || pids[fdes = fileno(iop)] == 0)
        return (-1);
    (void) fclose(iop);
#ifdef SVR4
    sigprocmask( SIG_BLOCK, &sig, &omask);
#elif defined(M_UNIX)
    sigemptyset(&newmask);
    sigaddset(&newmask, SIGINT);
    sigaddset(&newmask, SIGQUIT);
    sigaddset(&newmask, SIGHUP);
    (void) sigprocmask(SIG_BLOCK, &newmask ,&oldmask);
#else
    omask = sigblock(sigmask(SIGINT) | sigmask(SIGQUIT) | sigmask(SIGHUP));
#endif

#ifdef _OSF_SOURCE
    while ((pid = wait(&status)) != pids[fdes] && pid != -1) ;
#else
#ifndef NeXT
    while ((pid = wait((int *) &stat_loc)) != pids[fdes] && pid != -1) ;
#else
    while ((pid = wait(&stat_loc)) != pids[fdes] && pid != -1) ;
#endif
#endif
    pids[fdes] = 0;
#ifdef SVR4
    sigprocmask( SIG_SETMASK, &omask, (sigset_t *)NULL);
    return(pid == -1 ? -1 : WEXITSTATUS(stat_loc));
#elif defined(M_UNIX)
	(void) sigprocmask(SIG_SETMASK, &oldmask, (sigset_t)0);
#else
    (void)sigsetmask(omask);
#ifdef _OSF_SOURCE
    return (pid == -1 ? -1 : status);
#elif defined(M_UNIX) || defined(LINUX)
	return (pid == -1 ? -1 : WEXITSTATUS(stat_loc));
#else
    return (pid == -1 ? -1 : stat_loc.w_status);
#endif
#endif
}
