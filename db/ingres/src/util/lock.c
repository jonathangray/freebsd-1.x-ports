#include <stdio.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <signal.h>
#include <setjmp.h>
#include "sccs.h"

#include <ingres.h>

#include "protos.h"

SCCSID(@(#)lock.c	8.2	5/1/86)

/*
** start_up_lock_driver
**	Attempt to start up a connection to the lock driver.
**	We connect to a know address (a socket server sits there).
**	If we get a connection on this location, than we are talking 
**	to the lock driver. If we timeout, then we assume the driver 
**	isn't there.
**
** Returns
**	File descriptor attached to the lock driver
**	-1 on any error.
**
** Trace Flags
**	28
*/
int
start_up_lock_driver(void)
{
	struct	sockaddr_in	addr;		/* address to attach to for server */
	register	int	to_driver;	/* we can talk to the lock driver on this one */
	struct		servent	*ing_ser;


	/*
	** Find out where the lock driver lives
	*/
	if ( (ing_ser = getservbyname("ingreslock",(char *)0)) == 0 ) {
#ifdef xATR1
		if ( tTf(28,4) )
			ingres_perror("set_up_lock getservbyname");
#endif
		return ( -1 );
	}

	/*
	** Make our end of the socket
	*/
	if ( (to_driver = socket(AF_INET,SOCK_STREAM,0)) == -1 ) {
#ifdef xATR1
		if ( tTf(28,4) )
			ingres_perror("set_up_lock socket");
#endif
		return ( -1 );
	}

	bzero((char *) &addr, sizeof(addr));
	addr.sin_addr.s_addr = inet_addr("127.0.0.1");
	addr.sin_family = AF_INET;
	addr.sin_port = ing_ser->s_port;


	/*
	** Connect to the lock_driver
	*/
	if ( connect(to_driver, (struct sockaddr *)&addr, sizeof (addr)) == -1 ) {
#ifdef xATR1
		if ( tTf(28,4) )
			ingres_perror("set_up_lock connect");
#endif
		close(to_driver);
		return ( -1 );
	}
	return ( to_driver );
}/* start_up_lock_driver */
