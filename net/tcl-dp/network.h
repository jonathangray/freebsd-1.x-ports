/*
 * network.h --
 *
 *	Declaration of functions provided by the network library
 *
 * Copyright 1989-1992 Regents of the University of California.
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 *
 */

#ifndef _T_NETWORK_H
#define _T_NETWORK_H

#include <tcl.h>
#include "util.h"

/* Disable timing info */
#ifndef TIMING_INFO
#define MakeTimerEntry(x)	(1)
#define	DeleteTimerEntry(x)	/* */
#define	StartTimer(x)		/* */
#define	EndTimer(x)		/* */
#endif	/* TIMING_INFO */

/* From network.c */

extern void Tcp_Init 		_ANSI_ARGS_((Tcl_Interp *interp));

extern int Tcp_PacketSend 	_ANSI_ARGS_((Tcl_Interp *interp,
					     OpenFile *filePtr,
					     char *message));
extern int Tcp_PacketReceive	_ANSI_ARGS_((Tcl_Interp *interp,
					     OpenFile *filePtr,
					     int block));

/* From address.c */

extern struct sockaddr_in *Tcm_FindAddr _ANSI_ARGS_((char *name));

extern char *Tcm_CreateAddress		_ANSI_ARGS_((unsigned long addr, 
						     int port));

extern int Tcm_AddressCmd		_ANSI_ARGS_((ClientData clientData,
						     Tcl_Interp * interp,
						     int argc,
						     char **argv));

/* From rpc.c */

extern int Tcm_RPC		_ANSI_ARGS_((Tcl_Interp *interp,
					     OpenFile *filePtr,
					     char *command,
					     int events, 
					     int timeout,
					     char *timeoutReturn));

extern int Tcm_RDO		_ANSI_ARGS_((Tcl_Interp *interp,
					     OpenFile *filePtr,
					     char *command));

/* From networkInit.c */

extern void networkInit		_ANSI_ARGS_((Tcl_Interp *interp));
extern void dpInit		_ANSI_ARGS_((Tcl_Interp *interp));


#endif

