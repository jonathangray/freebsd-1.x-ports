/*
 * Copyright 1989 Jon Bennett, Mike Bolotski, David Gagne, Dan Lovinger
 * Copyright 1986 Chris Gutherie
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of the copyright holders not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.  The copyright holders make no
 * representations about the suitability of this software for any purpose.
 * It is provided "as is" without express or implied warranty.
 *
 * THE COPYRIGHT HOLDERS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

#include <X11/Xlib.h>
#include <X11/Xos.h>

#include <stdio.h>
#include <signal.h>
#include <pwd.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>

#ifndef FD_SET
#define	FD_SET(n, s)	(((s)->fds_bits[0]) |= (1 << n))
#define	FD_CLR(n, s)	(((s)->fds_bits[0]) &= ~(1 << n))
#define	FD_ZERO(s)	bzero((char *)(s), sizeof (*(s)))
#define	FD_ISSET(n, s)	(((s)->fds_bits[0]) & (1 << n))
#endif

#include <sys/ioctl.h>
#include "defs.h"
#include "data.h"

#ifdef PROFILE
#include <setjmp.h>

static jmp_buf foojmp;
static int fooflag=0;
gobyebye()
{
    longjmp(foojmp,1);
}
#endif
struct sockaddr_in xtrekAddress;

int             main(argc, argv)
  int             argc;
  char           *argv[1];

{
  char           *configfile;
#ifdef PROFILE
  if(setjmp(foojmp)) {
      printf("exiting normally\n");
      return 0;
  }
  signal(SIGHUP,gobyebye);
  printf("profiling enabled.\n");
#endif
  if (argc <= 1)
    configfile = DEFAULT_CONFIG;
  else
    configfile = argv[1];

  xtrekAddress.sin_family = AF_INET;
  xtrekAddress.sin_addr.s_addr = INADDR_ANY;
  xtrekAddress.sin_port = htons(XTREK_PORT);
  xtrek_socket = socket(AF_INET, SOCK_STREAM, 0);
  if (xtrek_socket < 0) {
    fprintf(stderr, "Can't open xtrek socket.\n");
    perror("socket");
    return 1;
  }
  if (bind(xtrek_socket, (struct sockaddr *)&xtrekAddress, sizeof xtrekAddress) < 0) {
    fprintf(stderr, "Can't bind to xtrek address.\n");
    perror("bind");
    (void) close(xtrek_socket);
    return 1;
  } {
    static int      on = 1;

    (void) setsockopt(xtrek_socket, SOL_SOCKET, SO_REUSEADDR, (char *) &on, sizeof on);
    (void) setsockopt(xtrek_socket, SOL_SOCKET, SO_KEEPALIVE, (char *) &on, sizeof on);
    ioctl(xtrek_socket, FIONBIO, (char *) &on);
  }
  if (listen(xtrek_socket, MAXPLAYER) < 0) {
    fprintf(stderr, "Can't listen on xtrek socket.\n");
    perror("listen");
    (void) close(xtrek_socket);
    return 1;
  }
  srandom(time(0));
  subdaemon(configfile);

  /* The main loop monster! */
  doinput();
  return 0;
}
