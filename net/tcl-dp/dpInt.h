/*
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

#include "dp.h"

#include<sys/types.h>

#ifndef NO_SYS_TIME_H
#   include <sys/time.h>
#else
#   include <time.h>
#endif

#if HAVE_STDLIB_H
#    include <stdlib.h>
#else
#    include <compat/stdlib.h>
#endif

#ifdef HAVE_SYS_SELECT_H
#   include <sys/select.h>
#endif

#ifndef NO_FD_SET
#   define SELECT_MASK fd_set
#else
#   ifndef _AIX
	typedef long fd_mask;
#   endif
#   if defined(_IBMR2)
#       define SELECT_MASK void
#   else
#       define SELECT_MASK int
#   endif
#endif

#ifdef NO_STDLIB_H
#   include "compat/stdlib.h"
#else
#   include <stdlib.h>
#endif

#ifdef NO_STRING_H
#include "compat/string.h"
#else
#include <string.h>
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/uio.h>

/*
 * Some unix systems use bzero without giving a prototype.
 * We define it as a macro.
 */
#define bzero(b,len) memset(b, 0, (size_t)(len))

/* system calls made by dpnetwork.c */
int	setsockopt	_ANSI_ARGS_((int socket, int level, int option_name,
				     const void *option_value, int option_len));
int	getsockopt	_ANSI_ARGS_((int socket, int level, int option_name,
				     void *option_value, int *option_len));
int	gethostname 	_ANSI_ARGS_((char *address, int address_len));
int	close		_ANSI_ARGS_((int fd));
int	gettimeofday	_ANSI_ARGS_((struct timeval *tp,
				     struct timezone *tzp));
int     listen		_ANSI_ARGS_((int s, int backlog));
int	read		_ANSI_ARGS_((int fd, char *buf, int nbyte));
int	recv		_ANSI_ARGS_((int s, void *buf, int len, int flags));
#ifndef HAVE_SYS_SELECT_H
#ifndef _HPUX_SOURCE
int	select		_ANSI_ARGS_((int nfds, SELECT_MASK *readfds,
				     SELECT_MASK *writefds,
				     SELECT_MASK *exceptfds,
				     const struct timeval *timeout));
#endif
#endif
int	send		_ANSI_ARGS_((int s, const void *msg, int len, int flags));
int	sendmsg		_ANSI_ARGS_((int s, const struct msghdr msg[],
				     int flags));
void	setbuf		_ANSI_ARGS_((FILE *stream, char *buf));
int     shutdown	_ANSI_ARGS_((int s, int how));
int	socket		_ANSI_ARGS_((int domain, int type, int protocol));
int	write		_ANSI_ARGS_((int fd, char *buf, int nbyte));
#ifdef NO_WRITEV
int	writev		_ANSI_ARGS_((int fd, struct iovec *iov, int nv));
#endif
char	tolower		_ANSI_ARGS_((char c));

#ifdef NO_STRNCASECMP
int	strncasecmp	_ANSI_ARGS_((const char *s1, const char *s2, int n));
#endif


#define DP_VERSION "3.0"

/*
 * Maximum number of allowable open files.  This sets limits on various
 * internal table sizes.
 */
 
#define MAX_OPEN_FILES  1024
