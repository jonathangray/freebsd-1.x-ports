#ifndef lint
static char	*sccsid = "@(#)$Header: /a/cvs/386BSD/ports/news/nntp/server/main.c,v 1.2 1994/04/25 23:58:22 adam Exp $";
#endif

/*
 *	Network News Transfer Protocol server
 *
 *	Phil Lapsley
 *	University of California, Berkeley
 *	(Internet: phil@berkeley.edu; UUCP: ...!ucbvax!phil)
 *	Stan Barber
 *	Baylor College of Medicine
 *	(Internet: sob@tmc.edu; UUCP: ...!bcm!sob)
 */

#include "common.h"
#include <sys/socket.h>
#include <netinet/in.h>
#ifndef EXCELAN
#include <netdb.h>
#else
struct sockaddr_in current_peer = { AF_INET, IPPORT_NNTP };
#endif
#include <signal.h>
/* XXX Should be #ifdef VARARGS */
#if defined(sun) || defined(hpux)
#include <varargs.h>
#endif

#ifdef SETPROCTITLE
char	**Argv = NULL;		/* pointer to argument vector */
char	*LastArgv = NULL;	/* end of argv */
#endif /* SETPROCTITLE */

main(argc,argv,envp)
char **argv, **envp;
{

#ifdef ALONE	/* If no inetd */

	int			sockt, client, length;
	struct sockaddr_in	from;
	extern int 		reaper();
#ifdef LOAD
	register int load;
#endif /* LOAD */

	disassoc();

	/* fd 0-2 should be open and point to / now. */

	/* Let's close all the other FD's just to be sure */
	for(sockt = 3; sockt < 40; sockt++)
		(void) close(sockt);

#ifdef SYSLOG
#ifdef BSD_42
	openlog("nntpd", LOG_PID);			/* fd 3 */
#else /* !BSD_42 */
	openlog("nntpd", LOG_PID, SYSLOG);		/* fd 3 */
#endif /* BSD_42 */
#endif /* SYSLOG */


#ifdef FASTFORK
	num_groups = read_groups();	/* Read active file now (fd 4) */
					/* and then do it every */
	set_timer();			/* so often later */
#endif /* FASTFORK */

#ifndef EXCELAN
	sockt = get_socket();		/* should be fd 4 or 5 */
#ifdef USG
	(void) signal(SIGCLD, SIG_IGN);
#else /* !USG */
	(void) signal(SIGCHLD, reaper);
#endif /* USG */

#ifdef DEBUG
	(void) signal(SIGUSR1, debugup);
	(void) signal(SIGUSR2, debugdown);
#endif

	if (listen(sockt, SOMAXCONN) < 0) {
#ifdef SYSLOG
		syslog(LOG_ERR, "main: listen: %m");
#endif /* SYSLOG */
		exit(1);
	}
#endif /* EXCELAN */

#ifdef SETPROCTITLE
	/*
	 *  Save start and extent of argv for setproctitle.
	 */

	Argv = argv;
#ifdef SDD
	LastArgv = argv[argc - 1] + strlen(argv[argc - 1]);
#else /*SDD*/
	while (*envp)
		envp++;
	LastArgv = envp[-1] + strlen(envp[-1]);
#endif /*SDD*/
#endif /* SETPROCTITLE */
#if defined(LOAD) && defined(SETPROCTITLE)
	/* If LOAD and SETPROCTITLE, display load before first accept() */
	load = getla();
	setproctitle("%sing connections: loadav %d",
	    load > LOAD ? "reject" : "accept", load);
#endif /* LOAD && SETPROCTITLE */

	for (;;) {
#ifdef LOAD
		char oline[NNTP_STRLEN];
#endif /* LOAD */
#ifdef EXCELAN
		int status;
		sockt = 3;
		sockt = get_socket();
		if (sockt < 0)
			continue;
#ifdef USG
		(void) signal(SIGCLD, SIG_IGN);
		memset((char *)&from,'\0',sizeof(from));
#else /* !USG */
		bzero((char *)&from,sizeof(from));
#endif /* USG */
		client = accept(sockt, &from);
#else /* !EXCELAN */
		length = sizeof (from);
		client = accept(sockt, &from, &length);
#endif /* EXCELAN */
		if (client < 0) {
#ifdef SYSLOG
#ifdef EXCELAN
			if (errno != EINTR && errno != 60 )
#else /* !EXCELAN */
			if (errno != EINTR)
#endif /* EXCELAN */
				syslog(LOG_ERR, "accept: %m\n");
#endif /* SYSLOG */
#ifdef EXCELAN
			close(sockt);
			sleep(1);
#endif /* EXCELAN */
			continue;
		}

#ifdef LOAD
		if (( load = getla()) > LOAD ) {
#ifdef SETPROCTITLE
			setproctitle("rejecting connections: loadav %d", load);
#endif /* SETPROCTITLE */
			sprintf( oline, "%d loadav at %d, try later\r\n",
				ERR_GOODBYE, load );
			write( client, oline, strlen( oline ));
#ifdef SYSLOG
			syslog( LOG_INFO, "loadav at %d, sleeping", load );
#endif /* SYSLOG */
			close( client );
			sleep( 5 );
			continue;
		} else {
#ifdef SETPROCTITLE
			setproctitle("accepting connections: loadav %d", load);
#endif /* SETPROCTITLE */
		}
#endif /* LOAD */

		switch (fork()) {
		case	-1:
#ifdef SYSLOG
				syslog(LOG_ERR, "fork: %m\n");
#endif /* SYSLOG */
#ifdef EXCELAN
				(void) close(sockt);
#endif /* EXCELAN */
				(void) close(client);
				break;

		case	0:
#ifdef EXCELAN
				if (fork())
					exit(0);
#ifdef USG
				(void * )memcpy(&current_peer,&from,
					 sizeof(from));
#else /* !USG */
				bcopy(&from,&current_peer,sizeof(from));
#endif /* USG */
				make_stdio(sockt);
#else /* !EXCELAN */
				(void) close(sockt);
				make_stdio(client);
#endif /* EXCELAN */
#ifdef USG
				(void) signal(SIGCLD,SIG_DFL);
#endif /* USG */
				serve();
				break;

		default:
#ifdef EXCELAN
				(void) close(sockt);
#else /* !EXCELAN */
				(void) close(client);
#endif /* EXCELAN */
				break;
		}
	}

#else /* !ALONE */		/* We have inetd */

#ifdef LOAD
	{
		register int load;

		if (( load = getla()) > LOAD ) {
			printf("%d loadav at %d, try later\r\n", 
			       ERR_GOODBYE, load );
#ifdef SYSLOG
#ifdef BSD_42
			openlog("nntpd", LOG_PID);
#else /* !BSD_42 */
			openlog("nntpd", LOG_PID, SYSLOG);
#endif /* BSD_42 */
			syslog( LOG_INFO, "loadav at %d, exiting", load );
#endif /* SYSLOG */
			(void) fflush(stdout);
			exit(1);
		}
	}
#endif /* LOAD */
#ifdef SETPROCTITLE
	/*
	 *  Save start and extent of argv for setproctitle.
	 */

	Argv = argv;
#ifdef SDD
	LastArgv = argv[argc - 1] + strlen(argv[argc - 1]);
#else /*SDD*/
	while (*envp)
		envp++;
	LastArgv = envp[-1] + strlen(envp[-1]);
#endif /*SDD*/
#endif /* SETPROCTITLE */

#ifdef USG
	(void) signal(SIGCLD,SIG_DFL);
#endif /* USG */

	serve();

#endif /* ALONE */
}

/*
 * clobber argv so ps will show what we're doing.
 * stolen from sendmail
 */
#ifdef SETPROCTITLE
#if defined(sun) || defined(hpux)
/*VARARGS*/
void
setproctitle(va_alist)
    va_dcl
{
    register char *p, *fmt;
    register int i;
    char buf[BUFSIZ];
    va_list ap;

    va_start(ap);
    fmt = va_arg(ap, char *);
    
    (void) vsprintf(buf, fmt, ap);

    /* make ps print "(nntpd)" */
    p = Argv[0];
    *p++ = '-';

    i = strlen(buf);
    if (i > LastArgv - p - 2) {
	i = LastArgv - p - 2;
	buf[i] = '\0';
    }
    strcpy(p, buf);
    p += i;
    while (p < LastArgv)
	*p++ = ' ';

    va_end(ap);
}
#else
/*VARARGS1*/
void
setproctitle(fmt, a, b, c)
char *fmt;
{
	register char *p;
	register int i;
	char buf[BUFSIZ];

	(void) sprintf(buf, fmt, a, b, c);

	/* make ps print "(nntpd)" */
	p = Argv[0];
	*p++ = '-';

	i = strlen(buf);
	if (i > LastArgv - p - 2) {
		i = LastArgv - p - 2;
		buf[i] = '\0';
	}
	(void) strcpy(p, buf);
	p += i;
	while (p < LastArgv)
		*p++ = ' ';
}
#endif /* hpux */
#endif /* SETPROCTITLE */
