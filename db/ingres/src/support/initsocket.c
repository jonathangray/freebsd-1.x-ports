#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>

#include <netinet/in.h>
#include <arpa/inet.h>

#include <stdio.h>
#include <errno.h>
#include <netdb.h>
#include <signal.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "sccs.h"
#include "protos.h"

SCCSID(@(#)initsocket.c	8.2	6/12/88)

/*
** init_socket
** initilize the socket to the socket server
*/
int
init_socket(void)
{
	register	int	from_socket;	/* file descriptor to attach to socket server */
	int	to_ioctl = 1;			/* used in ioctl call */
	struct	sockaddr_in	addr;		/* address where socket server is */


	char	hostname[BUFSIZ];		/* hostname */
	struct	servent		*server;
	struct	hostent		*myhost;
	int		len;

#ifndef  DEBUG
	if ( (len = fork()) != 0 ) {
#ifdef	DEBUG
		printf("lock driver becomes %d\n",len);
#endif	DEBUG
		if ( len == -1 ) {
			perror("ingres lock driver, fork");
			exit(errno);
		}
		exit(0);
	}
#endif
	if ( (from_socket = socket(AF_INET,SOCK_STREAM,0)) == -1 ) {
#ifdef	DEBUG
		perror("INIT_S socket");
#endif	DEBUG
		exit(errno);
	}
	len = BUFSIZ;

	if ( (server = getservbyname("ingreslock",(char *)0)) == 0 )
		exit(errno);

	len = gethostname(hostname, sizeof(hostname));

	if ( (myhost = gethostbyname(hostname)) == 0 )
		exit(errno);
	bzero((char *) &addr,sizeof(addr));
/*	bcopy(myhost->h_addr,(char *)&addr.sin_addr,myhost->h_length);    */
	bcopy(myhost->h_addr,(char *)&addr.sin_addr.s_addr,myhost->h_length);

	addr.sin_family = AF_INET;
	addr.sin_port = server ? server->s_port : htons(1524);
	len = sizeof(addr);
	if ( bind(from_socket,(struct sockaddr *)&addr,len) == -1 ) {
#ifdef	DEBUG
		perror("INIT_S bind, assuming driver already running");
#endif	DEBUG
		exit(0);
	}
	if ( listen(from_socket,10) == -1 ) {
		perror("Ingres lock, can't listen on port");
		exit(errno);
	}
        ioctl(from_socket,FIONBIO,&to_ioctl);   /* set socket to nonblocking */
	return ( from_socket );
}/* init_socket */
