    /*********************************************************************\
    *  Copyright (c) 1991 by Wen-King Su (wen-king@vlsi.cs.caltech.edu)   *
    *                                                                     *
    *  You may copy or modify this file in any manner you wish, provided  *
    *  that this notice is always included, and that you hold the author  *
    *  harmless for any loss or damage resulting from the installation or *
    *  use of this software.                                              *
    \*********************************************************************/

#include "common.h"

static struct sockaddr_in INET_ZERO = { AF_INET };

#define DSIZE (sizeof(int)*8)
#define SAVE(A) do { int sav; sav = errno; A; errno = sav; } while (0)

#include <netdb.h>

int
#ifndef ANSI_PROTOTYPES
_x_udp(port)
    int *port;
#else /* ANSI_PROTOTYPES */
_x_udp(int *port)
#endif /* ANSI_PROTOTYPES */
{
    int f, len, zz;
    struct sockaddr_in me;
    struct sockaddr_in sin;

    me = sin = INET_ZERO;

    me.sin_port = htons((unsigned short) *port);
    me.sin_family = AF_INET;
 
    if ((f=socket(AF_INET,SOCK_DGRAM,0)) == -1)
	return(-1);
 
    if (setsockopt(f,SOL_SOCKET,SO_REUSEADDR,(char*)&zz,sizeof(zz)) < 0
       || bind(f,(struct sockaddr *) &me,(len = sizeof(me))) < 0
       || getsockname(f,(struct sockaddr *)&sin,&len) < 0)
    {
	SAVE(((void) close(f)));
	return(-1);
    }

    if (!*port)
	*port = ntohs((unsigned short) sin.sin_port);
	
    return(f);
}      

int
#ifndef ANSI_PROTOTYPES
_x_adr(host, port, his)
    char *host;
    int port;
    struct sockaddr_in *his;
#else /* ANSI_PROTOTYPES */
_x_adr(char *host, int port, struct sockaddr_in *his)
#endif
{
    char myhost[128];
    struct hostent *H;
    int i;
    char *s, *d;
 
    *his = INET_ZERO;
    if (!host)
	(void)gethostname(host = myhost,sizeof(myhost));
 
    if ((his->sin_addr.s_addr = inet_addr(host)) != -1)
	his->sin_family = AF_INET;
    else if((H = gethostbyname(host)))
    {
	for (s = (char *)H->h_addr, d = (char *)&his->sin_addr, i = H->h_length;
	     i--; *d++ = *s++)
	    ;
	his->sin_family = H->h_addrtype;
    }
    else
	return(-1);

    his->sin_port = htons((unsigned short) port);
 
    return(0);
}

int
#ifndef ANSI_PROTOTYPES
_x_select(rf, tt)
    fd_set *rf;
    long tt;
#else /* ANSI_PROTOTYPES */
_x_select(fd_set *rf, long tt)
#endif /* ANSI_PROTOTYPES */
{
    if (tt != -1)
    {
        struct timeval timeout;
 
        timeout.tv_sec  =  tt / 1000;
        timeout.tv_usec = (tt % 1000)*1000;

        return (select(DSIZE, rf, (fd_set*)0, (fd_set*)0, &timeout));
    }
       
    return (select(DSIZE, rf, (fd_set*)0, (fd_set*)0, (struct timeval *)0));
}
