/*
 * based on Windows Sockets 1.1 specs
 * (ftp.microsoft.com:/Advsys/winsock/spec11/WINSOCK.TXT)
 */
 
#include <netdb.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#if defined(__FreeBSD__)
#include <netinet/in.h>
#endif
#include <arpa/inet.h>
#include <errno.h>
#include <netdb.h>
#include "winsock.h"

#define DEBUG_WINSOCK

/* XXX per task */
WORD wsa_errno;
int  wsa_initted;

WORD errno_to_wsaerrno(int errno)
{
        switch(errno) {
        case ENETDOWN:
                return WSAENETDOWN;
        case EAFNOSUPPORT:
                return WSAEAFNOSUPPORT;
        case EMFILE:
                return WSAEMFILE;
        case ENOBUFS:
                return WSAENOBUFS;
        case EPROTONOSUPPORT:
                return EPROTONOSUPPORT;
        case EPROTOTYPE:
                return WSAEPROTOTYPE;
        default:
#ifndef sun
#if defined(__FreeBSD__)
                fprintf(stderr, "winsock: errno_to_wsaerrno translation failure.\n\t: %s (%d)\n",
                        sys_errlist[errno], errno);
#else
                fprintf(stderr, "winsock: errno_to_wsaerrno translation failure.\n\t: %s (%d)\n",
                        strerror [errno], errno);
#endif
#else
		fprintf (stderr, "winsock: errno_to_wsaerrno translation failure.\n");
#endif
                break;
        }
}
 
SOCKET Winsock_accept(SOCKET s, struct sockaddr FAR *addr, int FAR *addrlen)
{
	accept(s, addr, addrlen);
}

int Winsock_bind(SOCKET s, struct sockaddr FAR *name, int namelen)
{
	bind(s, name, namelen);
}

int Winsock_closesocket(SOCKET s)
{
	close(s);
}

int Winsock_connect(SOCKET s, struct sockaddr FAR *name, int namelen)
{
	connect(s, name, namelen);
}

int Winsock_getpeername(SOCKET s, struct sockaddr FAR *name, int FAR *namelen)
{
	getpeername(s, name, namelen);
}

int Winsock_getsockname(SOCKET s, struct sockaddr FAR *name, int FAR *namelen)
{
	getsockname(s, name, namelen);
}

int Winsock_getsockopt(SOCKET s, int loptname, char FAR *optval, int FAR *optlen)
{
	getsockopt(s, 0, loptname, optval, optlen);
}

u_long Winsock_htonl(u_long hostlong)
{
    return( htonl(hostlong) );
}         

u_short Winsock_htons(u_short hostshort)
{
	return( htons(hostshort) );
}

u_long Winsock_inet_addr(char FAR *cp)
{
	return( inet_addr(cp) );
}

char *Winsock_inet_ntoa(struct in_addr in)
{
	return( inet_ntoa(in) );	
}

int Winsock_ioctlsocket(SOCKET s, long cmd, u_long FAR *argp)
{
	return( ioctl(s, cmd, argp) );
}

int Winsock_listen(SOCKET s, int backlog)
{
	listen(s, backlog);
}

u_long Winsock_ntohl(u_long netlong)
{
	return( ntohl(netlong) );
}

u_short Winsock_ntohs(u_short netshort)
{
	return( ntohs(netshort) );
}

int Winsock_recv(SOCKET s, char FAR *buf, int len, int flags)
{
	recv(s, buf, len, flags);
}

int Winsock_recvfrom(SOCKET s, char FAR *buf, int len, int flags, 
		struct sockaddr FAR *from, int FAR *fromlen)
{
	recvfrom(s, buf, len, flags, from, fromlen);
}

int Winsock_select(int nfds, fd_set FAR *readfds, fd_set FAR *writefds,
	fd_set FAR *exceptfds, struct timeval FAR *timeout)
{
	return(select(nfds, readfds, writefds, exceptfds, timeout));
}

int Winsock_send(SOCKET s, char FAR *buf, int len, int flags)
{
	send(s, buf, len, flags);
}

int Winsock_sendto(SOCKET s, char FAR *buf, int len, int flags,
		struct sockaddr FAR *to, int tolen)
{
	sendto(s, buf, len, flags, to, tolen);
}

int Winsock_setsockopt(SOCKET s, int level, int optname, const char FAR *optval, 
		int optlen)
{
	setsockopt(s, level, optname, optval, optlen);
}                                         

int Winsock_shutdown(SOCKET s, int how)
{
	shutdown(s, how);
}

SOCKET Winsock_socket(WORD af, WORD type, WORD protocol)
{
    int sock;

#ifdef DEBUG_WINSOCK
    printf("Winsock_socket: af=%d type=%d protocol=%d\n", af, type, protocol);
#endif
    
    if (!wsa_initted) {
            wsa_errno = WSANOTINITIALISED;
            return INVALID_SOCKET;
    }
    
    if ((sock = socket(af, type, protocol)) < 0) {
            wsa_errno = errno_to_wsaerrno(errno);
            return INVALID_SOCKET;
    }
    return sock;
}

struct hostent *Winsock_gethostbyaddr(const char FAR *addr, int len,  int type)
{
	return( gethostbyaddr(addr, len, type) );
}

struct hostent *Winsock_gethostbyname(const char FAR *name)
{
	return( gethostbyname(name) );
}

int Winsock_gethostname(char FAR *name, int namelen)
{
	return( gethostname(name, namelen) );
}          

struct protoent *Winsock_getprotobyname(char FAR *name)
{
	return( getprotobyname(name) );
}

struct protoent *Winsock_getprotobynumber(int number)
{
	return( getprotobynumber(number) );
}

struct servent *Winsock_getservbyname(const char FAR *name, const char FAR *proto)
{
	return( getservbyname(name, proto) );
}

struct servent *Winsock_getservbyport(int port, const char FAR *proto)
{
	return( getservbyport(port, proto) );
}

HANDLE WSAAsyncGetHostByAddr(HWND hWnd, u_int wMsg, const char FAR *addr,
		 int len, int type, const char FAR *buf, int buflen)
{

}                    

HANDLE WSAAsyncGetHostByName(HWND hWnd, u_int wMsg, const char FAR *name, 
			char FAR *buf, int buflen)
{

}                     

HANDLE WSAAsyncGetProtoByName(HWND hWnd, u_int wMsg, const char FAR *name, 
			char FAR *buf, int buflen)
{

}

HANDLE WSAAsyncGetProtoByNumber(HWND hWnd, u_int wMsg, int number, 
			char FAR *buf, int buflen)
{

}

HANDLE WSAAsyncGetServByName(HWND hWnd, u_int wMsg, const char FAR *name, 
			const char FAR *proto, char FAR *buf, int buflen)
{

}

HANDLE WSAAsyncGetServByPort(HWND hWnd, u_int wMsg, int port, const char FAR 
			*proto, char FAR *buf, int buflen)
{

}

int WSAAsyncSelect(SOCKET s, HWND hWnd, u_int wMsg, long lEvent)
{

}

int WSAFDIsSet(int fd, fd_set *set)
{
	return( FD_ISSET(fd, set) );
}

WSACancelAsyncRequest(HANDLE hAsyncTaskHandle)
{

}

WSACancelBlockingCall ( void )
{

}
          
int WSAGetLastError(void)
{
    return wsa_errno;
}

void WSASetLastError(int iError)
{
    wsa_errno = iError;
}

BOOL WSAIsBlocking (void)
{

}

FARPROC WSASetBlockingHook(FARPROC lpBlockFunc)
{

}

int WSAUnhookBlockingHook(void)
{

}

WSADATA Winsock_data = {
        0x0101,
        0x0101,
        "WINE Sockets",
#ifdef linux
        "LINUX/i386",
#endif
#ifdef __NetBSD__
        "NetBSD/i386",
#endif
#ifdef sunos
	"SunOS",
#endif
        128,
	1024,
        NULL
};

int WSAStartup(WORD wVersionRequested, LPWSADATA lpWSAData)
{
#ifdef DEBUG_WINSOCK
    fprintf(stderr, "WSAStartup: verReq=%x\n", wVersionRequested);
#endif

    if (LOBYTE(wVersionRequested) < 1 ||
        (LOBYTE(wVersionRequested) == 1 &&
         HIBYTE(wVersionRequested) < 1))
        return WSAVERNOTSUPPORTED;

    if (!lpWSAData)
        return WSAEINVAL;
    
    bcopy(&Winsock_data, lpWSAData, sizeof(Winsock_data));

    wsa_initted = 1;
    
    return(0);
}

int WSACleanup(void)
{
    wsa_initted = 0;
    return 0;
}
