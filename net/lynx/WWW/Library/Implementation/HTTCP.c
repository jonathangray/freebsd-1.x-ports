/*			Generic Communication Code		HTTCP.c
**			==========================
**
**	This code is in common between client and server sides.
**
**	16 Jan 92  TBL	Fix strtol() undefined on CMU Mach.
**	25 Jun 92  JFG  Added DECNET option through TCP socket emulation.
**	13 Sep 93  MD   Added correct return of vmserrorno for HTInetStatus.
**			Added decoding of vms error message for MULTINET.
**      7-DEC-1993 Bjorn S. Nilsson, ALEPH, CERN, VMS UCX ioctl() changes
**			(done of Mosaic)
**      19 Feb 94  Danny Mayer	Added Bjorn Fixes to Lynx version
**	 7 Mar 94  Danny Mayer  Added Fix UCX version for full domain name
*/


#include "HTUtils.h"
#include "tcp.h"		/* Defines SHORT_NAMES if necessary */
#include "HTAccess.h"
#include "HTParse.h"
#ifdef SHORT_NAMES
#define HTInetStatus		HTInStat
#define HTInetString 		HTInStri
#define HTParseInet		HTPaInet
#endif

#ifndef FD_SETSIZE
#ifdef UCX
#define FD_SETSIZE 32
#else
#define FD_SETSIZE 256
#endif /* Limit # sockets to 32 for UCX, BSN */
#endif

/*	Module-Wide variables
*/

PRIVATE char *hostname=0;		/* The name of this host */


/*	PUBLIC VARIABLES
*/

/* PUBLIC SockA HTHostAddress; */	/* The internet address of the host */
					/* Valid after call to HTHostName() */

/*	Encode INET status (as in sys/errno.h)			  inet_status()
**	------------------
**
** On entry,
**	where		gives a description of what caused the error
**	global errno	gives the error number in the unix way.
**
** On return,
**	returns		a negative status in the unix way.
*/
#ifndef PCNFS
#ifdef VMS
#if !defined(MULTINET) && !defined(UCX) && !defined(WIN_TCP)
extern int uerrno;	/* Deposit of error info (as per errno.h) */
extern volatile noshare int socket_errno; /* socket VMS error info 
                                             (used for translation of vmserrno) */
extern volatile noshare int vmserrno;	/* Deposit of VMS error info */
extern volatile noshare int errno;  /* noshare to avoid PSECT conflict */
#endif /* not MULTINET */
#else /* VMS */
#ifndef errno
extern int errno;
#endif /* errno */
#endif /* VMS */

#ifndef VM
#ifndef VMS
#ifndef NeXT
#ifndef THINK_C
extern char *sys_errlist[];		/* see man perror on cernvax */
extern int sys_nerr;
#endif  /* think c */
#endif	/* NeXT */
#endif  /* VMS */
#endif	/* VM */

#endif	/* PCNFS */

#ifdef UCX
/*
 * A routine to mimick the ioctl function for UCX.
 * Bjorn S. Nilsson, 25-Nov-1993. Based on an example in the UCX manual.
 */
#include <iodef.h>
#define IOC_OUT (int)0x40000000
extern int vaxc$get_sdc(), sys$qiow();

PUBLIC int ioctl ARGS3
	(int,	d, 
	int,	request,
	char *, argp)
{
  int sdc, status;
  unsigned short fun, iosb[4];
  char *p5, *p6;
  struct comm
    {
      int command;
      char *addr;
    } ioctl_comm;
  struct it2
    {
      unsigned short len;
      unsigned short opt;
      struct comm *addr;
    } ioctl_desc;
  if ((sdc = vaxc$get_sdc (d)) == 0)
    {
      errno = EBADF;
      return -1;
    }
  ioctl_desc.opt  = UCX$C_IOCTL;
  ioctl_desc.len  = sizeof(struct comm);
  ioctl_desc.addr = &ioctl_comm;
  if (request & IOC_OUT)
    {
      fun = IO$_SENSEMODE;
      p5 = 0;
      p6 = (char *)&ioctl_desc;
    }
  else
    {
      fun = IO$_SETMODE;
      p5 = (char *)&ioctl_desc;
      p6 = 0;
    }
  ioctl_comm.command = request;
  ioctl_comm.addr = argp;
  status = sys$qiow (0, sdc, fun, iosb, 0, 0,
    0, 0, 0, 0, p5, p6);
  if (!(status & 01))
    {
      errno = status;
      return -1;
    }
  if (!(iosb[0] & 01))
    {
      errno = iosb[0];
      return -1;
    }
  return 0;
}
#include <perror.h> /* RJF */
#endif /* VMS, UCX, BSN */


/*	Report Internet Error
**	---------------------
*/
#ifdef __STDC__
PUBLIC int HTInetStatus(char *where)
#else
PUBLIC int HTInetStatus(where)
    char    *where;
#endif
{
#ifdef VMS
#ifdef MULTINET
            socket_errno = vmserrno;
#endif
#endif 

    CTRACE(tfp, "TCP: Error %d in `errno' after call to %s() failed.\n\t%s\n",
	    errno,  where,

#ifdef VM
	    "(Error number not translated)");	/* What Is the VM equiv? */
#define ER_NO_TRANS_DONE
#endif
#ifdef VMS
#ifdef MULTINET
            vms_errno_string());
#else
	    "(Error number not translated)");
#endif
#define ER_NO_TRANS_DONE
#endif
#ifdef NeXT
	    strerror(errno));
#define ER_NO_TRANS_DONE
#endif
#ifdef THINK_C
	    strerror(errno));
#define ER_NO_TRANS_DONE
#endif

#ifndef ER_NO_TRANS_DONE
	    errno < sys_nerr ? sys_errlist[errno] : "Unknown error" );
#endif

#ifdef VMS
#ifndef MULTINET
    CTRACE(tfp, "         Unix error number (errno) = %ld dec\n", errno);
    CTRACE(tfp, "         VMS error (vaxc$errno)    = %lx hex\n", vaxc$errno);
#endif
#endif

#ifdef VMS
    /* uerrno and errno happen to be zero if vmserrno <> 0 */
#ifdef MULTINET
    return -vmserrno;
#else
    return -vaxc$errno;
#endif
#else
    return -errno;
#endif
}


/*	Parse a cardinal value				       parse_cardinal()
**	----------------------
**
** On entry,
**	*pp	    points to first character to be interpreted, terminated by
**		    non 0:9 character.
**	*pstatus    points to status already valid
**	maxvalue    gives the largest allowable value.
**
** On exit,
**	*pp	    points to first unread character
**	*pstatus    points to status updated iff bad
*/

PUBLIC unsigned int HTCardinal ARGS3
	(int *,		pstatus,
	char **,	pp,
	unsigned int,	max_value)
{
    int   n;
    if ( (**pp<'0') || (**pp>'9')) {	    /* Null string is error */
	*pstatus = -3;  /* No number where one expeceted */
	return 0;
    }

    n=0;
    while ((**pp>='0') && (**pp<='9')) n = n*10 + *((*pp)++) - '0';

    if (n>max_value) {
	*pstatus = -4;  /* Cardinal outside range */
	return 0;
    }

    return n;
}


#ifndef DECNET  /* Function only used below for a trace message */

/*	Produce a string for an Internet address
**	----------------------------------------
**
** On exit,
**	returns	a pointer to a static string which must be copied if
**		it is to be kept.
*/

PUBLIC CONST char * HTInetString ARGS1(SockA*,sin)
{
    static char string[16];
    sprintf(string, "%d.%d.%d.%d",
	    (int)*((unsigned char *)(&sin->sin_addr)+0),
	    (int)*((unsigned char *)(&sin->sin_addr)+1),
	    (int)*((unsigned char *)(&sin->sin_addr)+2),
	    (int)*((unsigned char *)(&sin->sin_addr)+3));
    return string;
}
#endif /* Decnet */


/*	Parse a network node address and port
**	-------------------------------------
**
** On entry,
**	str	points to a string with a node name or number,
**		with optional trailing colon and port number.
**	sin	points to the binary internet or decnet address field.
**
** On exit,
**	*sin	is filled in. If no port is specified in str, that
**		field is left unchanged in *sin.
*/
PUBLIC int HTParseInet ARGS2(SockA *,sin, CONST char *,str)
{
    char *port;
    char host[256];
    struct hostent  *phost;	/* Pointer to host - See netdb.h */
    strcpy(host, str);		/* Take a copy we can mutilate */



/*	Parse port number if present
*/    
    if (port=strchr(host, ':')) {
    	*port++ = 0;		/* Chop off port */
        if (port[0]>='0' && port[0]<='9') {

#ifdef unix
	    sin->sin_port = htons(atol(port));
#else /* VMS */
#ifdef DECNET
	    sin->sdn_objnum = (unsigned char) (strtol(port, (char**)0 , 10));
#else
	    sin->sin_port = htons(strtol(port, (char**)0 , 10));
#endif /* Decnet */
#endif /* Unix vs. VMS */

	} else {

#ifdef SUPPRESS		/* 1. crashes!?!.  2. Not recommended */
	    struct servent * serv = getservbyname(port, (char*)0);
	    if (serv) sin->sin_port = serv->s_port;
	    else if (TRACE) fprintf(stderr, "TCP: Unknown service %s\n", port);
#endif
	}
      }

#ifdef DECNET
    /* read Decnet node name. @@ Should know about DECnet addresses, but it's
       probably worth waiting until the Phase transition from IV to V. */

    sin->sdn_nam.n_len = min(DN_MAXNAML, strlen(host));  /* <=6 in phase 4 */
    strncpy (sin->sdn_nam.n_name, host, sin->sdn_nam.n_len + 1);

    if (TRACE) fprintf(stderr,  
	"DECnet: Parsed address as object number %d on host %.6s...\n",
		      sin->sdn_objnum, host);

#else  /* parse Internet host */

/*	Parse host number if present.
*/  
    if (*host>='0' && *host<='9') {   /* Numeric node address: */
	sin->sin_addr.s_addr = inet_addr(host); /* See arpa/inet.h */
	/* sin->sin_addr.s_addr = inet_addr(host).s_addr; /* See arpa/inet.h */

    } else {		    /* Alphanumeric node name: */
#ifdef MVS	/* Oustanding problem with crash in MVS gethostbyname */
	if(TRACE)fprintf(stderr, "HTTCP: Calling gethostbyname(%s)\n", host);
#endif
	phost=gethostbyname(host);	/* See netdb.h */
#ifdef MVS
	if(TRACE)fprintf(stderr, "HTTCP: gethostbyname() returned %d\n", phost);
#endif
	if (!phost) {
	    if (TRACE) fprintf(stderr, 
		    "HTTPAccess: Can't find internet node name `%s'.\n",host);
	    return -1;  /* Fail? */
	}
	memcpy(&sin->sin_addr, phost->h_addr, phost->h_length);
    }

    if (TRACE) fprintf(stderr,  
	"TCP: Parsed address as port %d, IP address %d.%d.%d.%d\n",
		(int)ntohs(sin->sin_port),
		(int)*((unsigned char *)(&sin->sin_addr)+0),
		(int)*((unsigned char *)(&sin->sin_addr)+1),
		(int)*((unsigned char *)(&sin->sin_addr)+2),
		(int)*((unsigned char *)(&sin->sin_addr)+3));

#endif  /* Internet vs. Decnet */

    return 0;	/* OK */
}


/*	Derive the name of the host on which we are
**	-------------------------------------------
**
*/
#ifdef __STDC__
PRIVATE void get_host_details(void)
#else
PRIVATE void get_host_details()
#endif

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 64		/* Arbitrary limit */
#endif

{
    char name[MAXHOSTNAMELEN+1];	/* The name of this host */
#ifdef UCX
    char *domain_name;                   /* The name of this host domain */
#endif
#ifdef NEED_HOST_ADDRESS		/* no -- needs name server! */
    struct hostent * phost;		/* Pointer to host -- See netdb.h */
#endif
    int namelength = sizeof(name);
    
    if (hostname) return;		/* Already done */
    gethostname(name, namelength);	/* Without domain */
    StrAllocCopy(hostname, name);
#ifdef UCX
    /*  UCX doesn't give the complete domain name. get rest from UCX$BIND_DOM
    **  Logical
    */
    if(strchr(hostname,'.') == NULL) {           /* Not full address */
        domain_name = getenv("UCX$BIND_DOMAIN");
        if(domain_name != NULL) {
            StrAllocCat(hostname, ".");
            StrAllocCat(hostname, domain_name);
        }
     }
#endif /* UCX */
    CTRACE(tfp, "TCP: Local host name is %s\n", hostname);

#ifndef DECNET  /* Decnet ain't got no damn name server 8#OO */
#ifdef NEED_HOST_ADDRESS		 /* no -- needs name server! */
    phost=gethostbyname(name);		 /* See netdb.h */
    if (!phost) {
	if (TRACE) fprintf(stderr, 
		"TCP: Can't find my own internet node address for `%s'!!\n",
		name);
	return;  /* Fail! */
    }
    StrAllocCopy(hostname, phost->h_name);
    memcpy(&HTHostAddress, &phost->h_addr, phost->h_length);
    if (TRACE) fprintf(stderr, "     Name server says that I am `%s' = %s\n",
	    hostname, HTInetString(&HTHostAddress));
#endif

#endif /* not Decnet */
}

#ifdef __STDC__
PUBLIC const char * HTHostName(void)
#else
PUBLIC char * HTHostName()
#endif
{
    get_host_details();
    return hostname;
}

/*
 * interruptable connect as implemented by Marc Andreesen and
 * hacked in by Lou Montulli
 */ 

PUBLIC int HTDoConnect ARGS4(char *,url, char *,protocol, int,default_port, 
								     int *,s)
{
  struct sockaddr_in soc_address;
  struct sockaddr_in *sin = &soc_address;
  int status;

  /* Set up defaults: */
  sin->sin_family = AF_INET;
  sin->sin_port = htons(default_port);

  /* Get node name and optional port number: */
  {
    char line[256];
    char *p1 = HTParse(url, "", PARSE_HOST);
    char *at_sign, *host;
    int status;

    /* if theres an @ then use the stuff after it as a hostname */
    if((at_sign = strchr(p1,'@')) != NULL)
	host = at_sign+1;
    else
	host = p1;

    sprintf (line, "Looking up %s.", host);
    HTProgress (line);

    status = HTParseInet(sin, host);
    if (status)
      {
        sprintf (line, "Unable to locate remote host %s.", host);
        HTProgress(line);
        free (p1);
        return HT_NO_DATA;
      }

    sprintf (line, "Making %s connection to %s.", protocol, host);
    HTProgress (line);
    free (p1);
  }

  /* Now, let's get a socket set up from the server for the data: */
  *s = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

  /*
   * Make the socket non-blocking, so the connect can be canceled.
   * This means that when we issue the connect we should NOT
   * have to wait for the accept on the other end.
   */
#if !defined(NO_IOCTL)
  {
    int ret;
    int val = 1;
    char line[256];

    ret = IOCTL(*s, FIONBIO, &val);
    if (ret == -1)
      {
        sprintf (line, "Could not make connection non-blocking.");
        HTProgress(line);
      }
  }
#endif /* not NO_IOCTL */

  /*
   * Issue the connect.  Since the server can't do an instantaneous accept
   * and we are non-blocking, this will almost certainly return a negative
   * status.
   */
  status = connect(*s, (struct sockaddr*)&soc_address, sizeof(soc_address));

  /*
   * According to the Sun man page for connect:
   *     EINPROGRESS         The socket is non-blocking and the  con-
   *                         nection cannot be completed immediately.
   *                         It is possible to select(2) for  comple-
   *                         tion  by  selecting the socket for writ-
   *                         ing.
   * According to the Motorola SVR4 man page for connect:
   *     EAGAIN              The socket is non-blocking and the  con-
   *                         nection cannot be completed immediately.
   *                         It is possible to select for  completion
   *                         by  selecting  the  socket  for writing.
   *                         However, this is only  possible  if  the
   *                         socket  STREAMS  module  is  the topmost
   *                         module on  the  protocol  stack  with  a
   *                         write  service  procedure.  This will be
   *                         the normal case.
   */
#ifdef SVR4
  if ((status < 0) && ((SOCKET_ERRNO == EINPROGRESS)||(SOCKET_ERRNO == EAGAIN)))
#else
  if ((status < 0) && (SOCKET_ERRNO == EINPROGRESS))
#endif /* SVR4 */
    {
      struct timeval timeout;
      int ret;

      timeout.tv_sec = 0;
      timeout.tv_usec = 100000;
      ret = 0;
      while (ret <= 0)
        {
          fd_set writefds;
          int intr=0;

          FD_ZERO(&writefds);
          FD_SET(*s, &writefds);
#ifdef __hpux
          ret = select(FD_SETSIZE, NULL, (int *)&writefds, NULL, &timeout);
#else
          ret = select(FD_SETSIZE, NULL, &writefds, NULL, &timeout);
#endif
          /*
           * Again according to the Sun and Motorola man pagse for connect:
           *     EALREADY            The socket is non-blocking and a  previ-
           *                         ous  connection attempt has not yet been
           *                         completed.
           * Thus if the SOCKET_ERRNO is NOT EALREADY we have a real error, and
           * should break out here and return that error.
           * Otherwise if it is EALREADY keep on trying to complete the
           * connection.
           */
          if ((ret < 0)&&(SOCKET_ERRNO != EALREADY))
            {
              status = ret;
              break;
            }
          else if (ret > 0)
            {
              /*
               * Extra check here for connection success, if we try to connect
               * again, and get EISCONN, it means we have a successful
               * connection.
               */
              status = connect(*s, (struct sockaddr*)&soc_address,
                               sizeof(soc_address));
#ifndef UCX
              if ((status < 0)&&(SOCKET_ERRNO == EISCONN))
#else
/*
 * A UCX feature: Instead of returning EISCONN UCX returns EADDRINUSE.
 * Test for this status also.
 */
              if ((status < 0)&&((SOCKET_ERRNO == EISCONN) ||
				 (SOCKET_ERRNO == EADDRINUSE)))
#endif /* VMS, UCX, BSN */
                {
                  status = 0;
                }

	      if (SOCKET_ERRNO == EALREADY)  /* new stuff LJM */
		  ret=0; /* keep going */
	      else
                  break;
            }
          /*
           * The select says we aren't ready yet.
           * Try to connect again to make sure.  If we don't get EALREADY
           * or EISCONN, something has gone wrong.  Break out and report it.
           * For some reason SVR4 returns EAGAIN here instead of EALREADY,
           * even though the man page says it should be EALREADY.
           */
          else
            {
              status = connect(*s, (struct sockaddr*)&soc_address,
                               sizeof(soc_address));
#if defined(SVR4)
              if ((status < 0)&&(SOCKET_ERRNO != EALREADY)&&
			(SOCKET_ERRNO != EAGAIN)&&(SOCKET_ERRNO != EISCONN))
#else
#ifndef UCX
              if ((status < 0)&&(SOCKET_ERRNO != EALREADY)&&
						(SOCKET_ERRNO != EISCONN))
#else
              /*
               * UCX pre 3 apparently returns errno = 18242 instead of
               * any of the EALREADY or EISCONN values.
               */
               if ((status < 0)&&(SOCKET_ERRNO != EALREADY)&&
				 (SOCKET_ERRNO != EISCONN)&&
                  		 (SOCKET_ERRNO != 18242))
#endif /* UCX, BSN */
#endif /* SVR4 */
                {
                  break;
                }
            }
          if(HTCheckForInterrupt())
            {
              if (TRACE)
                fprintf (stderr, "*** INTERRUPTED in middle of connect.\n");
              status = HT_INTERRUPTED;
              SOCKET_ERRNO = EINTR;
              break;
            }
        }
    }

  /*
   * Make the socket blocking again on good connect
   */
  if (status >= 0)
    {
#if !defined(NO_IOCTL)
      int ret;
      int val = 0;
      char line[256];

      ret = IOCTL(*s, FIONBIO, &val);
      if (ret == -1)
        {
          sprintf (line, "Could not restore socket to blocking.");
          HTProgress(line);
        }
#endif /* not NO_IOCTL */
    }
  /*
   * Else the connect attempt failed or was interrupted.
   * so close up the socket.
   */
  else
    {
        close(*s);
    }

  return status;
}

/* This is so interruptible reads can be implemented cleanly. */
int HTDoRead ARGS3(int,fildes, void *,buf, unsigned,nbyte)
{
  int ready, ret, intr=0;
  fd_set readfds;
  struct timeval timeout;
#ifdef UCX
  int nb;
#endif /* UCX, BSN */

  if (HTCheckForInterrupt())
    {
        SOCKET_ERRNO = EINTR;
        return (HT_INTERRUPTED);
    }


  timeout.tv_sec = 0;
  timeout.tv_usec = 100000;

#if !defined(NO_IOCTL)
  ready = 0;
#else
  ready = 1;
#endif /* bypass for NO_IOCTL */
  while (!ready)
    {
        FD_ZERO(&readfds);
        FD_SET(fildes, &readfds);
#ifdef __hpux
        ret = select(FD_SETSIZE, (int *)&readfds, NULL, NULL, &timeout);
#else
        ret = select(FD_SETSIZE, &readfds, NULL, NULL, &timeout);
#endif
        if (ret < 0)
          {
                return -1;
          }
        else if (ret > 0)
          {
                ready = 1;
          }
        else if(HTCheckForInterrupt())
          {
       	        SOCKET_ERRNO = EINTR;
                return HT_INTERRUPTED;
          }
    }

#if !defined(UCX) || !defined(VAXC)
  return SOCKET_READ (fildes, buf, nbyte);
#else                           /* VAXC and UCX problem only */
  errno = vaxc$errno = 0;
  nb = SOCKET_READ (fildes, buf, nbyte);
  CTRACE(tfp, "Read - nb,errno,vaxc$errno: %d %d %d\n", nb,errno,vaxc$errno);
  if ((nb <= 0) && TRACE)
     perror ("HTTCP.C:HTDoRead:read");          /* RJF */
  /*
   * An errno value of EPIPE and nb < 0 indicates end-of-file on VAXC
   */
  if ((nb <= 0) && (errno == EPIPE)) {
       nb = 0;
       errno = 0;
  }
  return nb;
#endif /* UCX, BSN */
}

