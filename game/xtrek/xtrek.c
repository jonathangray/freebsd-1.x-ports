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
#include <pwd.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <setjmp.h>
#include <netdb.h>
#include <signal.h>

#include "defs.h"

#ifndef FD_SET
#define	FD_SET(n, s)	(((s)->fds_bits[0]) |= (1 << n))
#define	FD_CLR(n, s)	(((s)->fds_bits[0]) &= ~(1 << n))
#define	FD_ZERO(s)	bzero((char *)(s), sizeof (*(s)))
#define	FD_ISSET(n, s)	(((s)->fds_bits[0]) & (1 << n))
#endif

/* apparently hpux doesn't believe Xos.h when it comes to rindex() ... */
#ifdef hpux
char *rindex();
#endif

jmp_buf            sdied;

void done()
{
  fprintf(stderr, "broken pipe\n");
  longjmp(sdied, 0);
}

/* cond_addto_fontpath moved here in recognition of the fact that
 * xtrekd should have no particular knowledge of the machines
 * that are requesting windows - FONTDIR need *not* be the same
 */

void      cond_addto_fontpath(d, element)
Display        *d; 
char           *element;
{
  char          **fontPathList;		/* Return list of directories */
  int           numPaths;		/* Number of returned directories */
  int           i;			/* Index into font path list */

  fontPathList = XGetFontPath(d, &numPaths);

  for (i = 0; i < numPaths && strcmp(fontPathList[i], element); i++);

  if (i >= numPaths) {
    char          **newPathList;

    if (newPathList = (char **) malloc((numPaths + 1) * sizeof (char *))) {
      for (i = 0; i < numPaths; i++)
	newPathList[i] = fontPathList[i];

      newPathList[i] = element;
      XSetFontPath(d, newPathList, numPaths + 1);
      free(newPathList);
    }
  }

  /* Give back storage for returned font paths */
  if (fontPathList)
    XFreeFontPath(fontPathList);
  XFlush(d);
}

char *getAlias(disp,l)
Display *disp;
char *l;
{
    char *t;
    static char buf[256];

    t = XGetDefault(disp, PROGRAM_NAME, "name");

    if(t) 
      strncpy(buf, t, sizeof buf);
    else 
      strncpy(buf, l, sizeof buf);

    return buf;
}

void startup(s, d, l)
  register int    s;
  register char  *d, *l;
{
  char            buf[256];
  int             n = 1;
  Display         *disp;

  if (!(disp = XOpenDisplay(d)))
    {
	fprintf(stderr,"xtrek: cannot connect to %s", d);
	exit(1);
    }

  cond_addto_fontpath(disp, FONTDIR);
 
  sprintf(buf, "Display: %s Login: %s Name: %s", d, l, getAlias(disp, l));
  puts(buf);
  write(s, buf, strlen(buf));
  write(s, "\015\012", 2);

  while ((n = read(s, buf, sizeof buf)) >= 0) {
      if (n == 0) {
	  write(s, "DONE\n", 5);		/* Does this cause SIGPIPE? */

	  /* No, I think it causes read() above to return -1 */
	  sleep(1);
      }
      else
	write(fileno(stdout), buf, n);
  }

  XCloseDisplay(disp);
}

void usage(p)
  register char  *p;
{
  printf("Usage: %s <machine running xtrekd>\n", p);
  exit(1);
}

main(ac, av)
  register int    ac;
  register char **av;
{
  struct sockaddr_in inet_sin;
  struct passwd   *pwent;
  int             s;
  char            hostname[80];		/* The xtrek-server-machine */
  char            display[80], *dpy_env;
  extern struct servent *getservbyname();

  if (ac != 2)
    usage(av[0]);
  
  if ((pwent = getpwuid(getuid())) == (struct passwd *) NULL) {
      printf("Who are you?\n");
      exit(2);
  }

  dpy_env = (char *) getenv("DISPLAY");
  if (dpy_env == (char *) NULL) {
      printf("Your DISPLAY environment variable is not set - are you in X?\n");
      exit(3);
  }

  if ((strncmp(dpy_env, "unix", 4) == 0) || (*dpy_env == ':')) {
      gethostname(display, sizeof(display));
      strcat(display, rindex(dpy_env, ':'));
  } else {
      strcpy(display, dpy_env);
  }

  inet_sin.sin_addr.s_addr = inet_addr(av[1]);

  if (inet_sin.sin_addr.s_addr != -1) {
      inet_sin.sin_family = AF_INET;
      (void) strcpy(hostname, av[1]);
  } else {
      struct hostent *host = (struct hostent *) NULL;

      host = gethostbyname(av[1]);
      if (host) {
	  inet_sin.sin_family = host->h_addrtype;
	  bcopy(host->h_addr, (caddr_t) & inet_sin.sin_addr,
		host->h_length);
	  strncpy(hostname, host->h_name, strlen(host->h_name));
      } else {
	  printf("%s: unknown host\n", av[1]);
	  exit(1);
      }
  }

  inet_sin.sin_port = htons(XTREK_PORT);
  signal(SIGPIPE, done);

  if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
    perror("socket");
    exit(1);
  }

  if (connect(s, (struct sockaddr *) &inet_sin, sizeof (inet_sin)) < 0) {
    perror("connect");
    (void) close(s);
    exit(1);
  }

  if (setjmp(sdied) == 0)
    startup(s, display, pwent->pw_name);

  exit(0);
}



