/* -*-C-*-
 *
 * Project :	 TRACE
 *
 * File    :	 socklib.h
 *
 * Description
 *
 * Copyright (C) 1991 by Arne Helme, The TRACE project
 *
 * Rights to use this source is granted for all non-commercial and research
 * uses. Creation of derivate forms of this software may be subject to
 * restriction. Please obtain written permission from the author.
 *
 * This software is provided "as is" without any express or implied warranty.
 *
 * RCS:      $Header: /a/cvs/386BSD/ports/game/xpilot/socklib.h,v 1.1 1994/02/23 14:40:08 jkh Exp $
 * Log:      $Log: socklib.h,v $
 * Log:      Revision 1.1  1994/02/23 14:40:08  jkh
 * Log:      Initial revision
 * Log:
 * Revision 3.5  1993/10/24  22:33:59  bert
 * Added prototypes for the new DgramReply() routine.
 *
 * Revision 3.4  1993/10/21  11:11:05  bert
 * VMS patch from Curt Hjorring.
 * Removed Optimize_map() from the server.
 * Made toggleShield a new client option.
 *
 * Revision 3.3  1993/08/19  07:35:26  kenrsc
 * Added patch from bert (3f4changes)
 *
 * Revision 3.2  1993/08/02  12:51:20  bjoerns
 * Patchlevel 2.
 *
 * Revision 3.1  1993/08/02  12:41:43  bjoerns
 * Patchlevel 1.
 *
 * Revision 3.0  1993/05/21  18:36:41  bjoerns
 * New client server release.
 *
 * Revision 1.2  1993/05/18  16:49:31  kenrsc
 * Berts few changes !
 *
 * Revision 1.1  1993/04/22  10:21:33  bjoerns
 * Moved socklib from lib to src.
 *
 * Revision 1.1  1993/03/09  14:33:27  kenrsc
 * Hopefully we won't have a corrupted CVS directory anymore.
 *
 * Revision 1.1.1.1  1993/02/27  14:47:46  bjoerns
 * XPilot v2.0
 *
 * Revision 1.1.1.1  1993/01/19  17:19:59  bjoerns
 * XPilot v1.4
 *
 * Revision 1.3  1992/09/11  22:50:24  bjoerns
 * Applied NCD2 patch.
 *
 * Revision 1.2  1992/08/26  19:36:36  bjoerns
 * Incorporated NCD patch.
 *
 * Revision 1.1.1.1  1992/05/11  12:32:34  bjoerns
 * XPilot v1.0
 *
 * Revision 1.2  91/10/02  08:38:20  08:38:20  arne (Arne Helme)
 * "ANSI C prototypes added."
 * 
 * Revision 1.1  91/10/02  08:34:53  08:34:53  arne (Arne Helme)
 * Initial revision
 * 
 */

#ifndef _SOCKLIB_INCLUDED
#define _SOCKLIB_INCLUDED

/* Error values and their meanings */
#define SL_ESOCKET		0	/* socket system call error */
#define SL_EBIND		1	/* bind system call error */
#define SL_ELISTEN		2	/* listen system call error */
#define SL_EHOSTNAME		3	/* Invalid host name format */
#define SL_ECONNECT		5	/* connect system call error */
#define SL_ESHUTD		6	/* shutdown system call error */
#define SL_ECLOSE		7	/* close system call error */
#define SL_EWRONGHOST		8	/* message arrived from unspec. host */
#define SL_ENORESP		9	/* No response */
#define SL_ERECEIVE		10	/* Receive error */

#ifndef _SOCKLIB_LIBSOURCE
#ifdef VMS
#include <in.h>			/* for sockaddr_in */
#else
#include <netinet/in.h>			/* for sockaddr_in */
#endif
extern int
    sl_errno,
    sl_timeout_s,
    sl_timeout_us,
    sl_default_retries,
    sl_broadcast_enabled;
extern struct sockaddr_in
    sl_dgram_lastaddr;
#ifdef __STDC__
extern void	SetTimeout(int, int);
extern int	CreateServerSocket(int);
extern int	GetPortNum(int);
extern int	GetPeerName(int, char *, int);
extern int	CreateClientSocket(char *, int);
extern int	SocketAccept(int);
extern int	SocketLinger(int);
extern int	SetSocketReceiveBufferSize(int, int);
extern int	SetSocketSendBufferSize(int, int);
extern int	SetSocketNoDelay(int, int);
extern int	SetSocketNonBlocking(int, int);
extern int	GetSocketError(int);
extern int	SocketReadable(int);
extern int	SocketRead(int, char *, int);
extern int	SocketWrite(int, char *, int);
extern int	SocketClose(int);
extern int	CreateDgramSocket(int);
extern int	DgramConnect(int, char *, int);
extern int	DgramSend(int, char *, int, char *, int);
extern int	DgramReceiveAny(int, char *, int);
extern int	DgramReceive(int, char *, char *, int);
extern int	DgramReply(int, char *, int);
extern int	DgramSendRec(int, char *, int, char *, int, char *, int);
extern char	*DgramLastaddr(void);
extern char	*DgramLastname(void);
extern int	DgramLastport(void);
#else /* __STDC__ */
extern void	SetTimeout();
extern int	CreateServerSocket();
extern int	GetPortNum();
extern int	GetPeerName();
extern int	CreateClientSocket();
extern int	SocketAccept();
extern int	SocketLinger();
extern int	SetSocketReceiveBufferSize();
extern int	SetSocketSendBufferSize();
extern int	SetSocketNoDelay();
extern int	SetSocketNonBlocking();
extern int	GetSocketError();
extern int	SocketReadable();
extern int	SocketRead();
extern int	SocketWrite();
extern int	SocketClose();
extern int	CreateDgramSocket();
extern int	DgramConnect();
extern int	DgramSend();
extern int	DgramReceiveAny();
extern int	DgramReceive();
extern int	DgramReply();
extern int	DgramSendRec();
extern char	*DgramLastaddr();
extern char	*DgramLastname();
extern int	DgramLastport();
#endif /* __STDC__ */
#endif /* _SOCKLIB_LIBSOURCE */
#endif /* _SOCKLIB_INCLUDED */
