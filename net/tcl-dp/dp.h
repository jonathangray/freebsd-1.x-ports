/*
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

#ifndef _DP_PROTOTYPES_
#define _DP_PROTOTYPES_

#include <tcl.h>

/* dpaddress.c */
extern char *Tdp_CreateAddress		_ANSI_ARGS_((unsigned long addr,
					     int port));
extern struct sockaddr *Tdp_FindAddr	_ANSI_ARGS_((char *name));
extern int Tdp_AddressCmd		_ANSI_ARGS_((ClientData notUsed,
					     Tcl_Interp *interp,
					     int argc, char **argv));

/* dpnetwork.c */
extern int Tdp_ConnectCmd		_ANSI_ARGS_((ClientData notUsed,
					     Tcl_Interp *interp,
					     int argc, char **argv));
extern int Tdp_ShutdownCmd		_ANSI_ARGS_((ClientData notUsed,
					     Tcl_Interp *interp,
					     int argc, char **argv));
extern int Tdp_AcceptCmd		_ANSI_ARGS_((ClientData notUsed,
					     Tcl_Interp *interp,
					     int argc, char **argv));
extern int Tdp_FileHandlerCmd		_ANSI_ARGS_((ClientData notUsed,
					     Tcl_Interp *interp,
					     int argc, char **argv));
extern int Tdp_FDIsReady		_ANSI_ARGS_((int fd));
extern int Tdp_IsReadyCmd		_ANSI_ARGS_((ClientData clientData,
					     Tcl_Interp *interp,
					     int argc, char *argv []));
extern int Tdp_PacketReceive		_ANSI_ARGS_((Tcl_Interp *interp,
					     char *fileHandle, int peek));
extern int Tdp_PacketReceiveCmd		_ANSI_ARGS_((ClientData clientData,
					     Tcl_Interp *interp,
					     int argc, char *argv []));
extern int Tdp_PacketSend		_ANSI_ARGS_((Tcl_Interp *interp,
					     char *fileHandle,
					     char *message));
extern int Tdp_PacketSendCmd		_ANSI_ARGS_((ClientData clientData,
					     Tcl_Interp *interp,
					     int argc, char *argv []));
extern int Tdp_ReceiveFromCmd		_ANSI_ARGS_((ClientData notUsed,
					     Tcl_Interp *interp,
					     int argc, char *argv []));
extern int Tdp_SendToCmd		_ANSI_ARGS_((ClientData clientData,
					     Tcl_Interp *interp,
					     int argc, char *argv []));
extern void Tdp_Tcp_Init		_ANSI_ARGS_((Tcl_Interp *interp));
extern void Tdp_CleanupFile		_ANSI_ARGS_((Tcl_Interp *interp,
					     char *file, int fd));

/* dpnetworkInit.c */
extern int Tdp_networkInit		_ANSI_ARGS_((Tcl_Interp *interp));
extern int Tdp_Init			_ANSI_ARGS_((Tcl_Interp *interp));
extern int Tdp_UpdateCmd		_ANSI_ARGS_((ClientData unused,
					     Tcl_Interp *interp,
					     int argc, char **argv));
extern int Tdp_WaitVariable		_ANSI_ARGS_((ClientData clientData,
					     Tcl_Interp *interp,
					     int argc, char *argv[]));

/* dprpc.c */
extern int Tdp_ReceiveRPC		_ANSI_ARGS_((Tcl_Interp *interp,
					     char *fileHandle,
					     char *command, int respond));
extern int Tdp_ReceiveRPCCmd		_ANSI_ARGS_((ClientData unused,
					     Tcl_Interp *interp,
					     int argc, char **argv));
extern int Tdp_ProcessRPCMessages	_ANSI_ARGS_((Tcl_Interp *interp,
					     char *fileHandle,
					     int wait));
extern int Tdp_ProcessRPCMessagesCmd	_ANSI_ARGS_((ClientData unused,
					     Tcl_Interp *interp,
					     int argc, char **argv));
extern int Tdp_RPC			_ANSI_ARGS_((Tcl_Interp *interp,
					     char *fileHandle,
					     char *command, int events,
					     int timeout,
					     char *timeoutReturn));
extern int Tdp_RPCCmd			_ANSI_ARGS_((ClientData clientData,
					     Tcl_Interp *interp,
					     int argc, char **argv));
extern int Tdp_ProcessRPCCommandCmd	_ANSI_ARGS_((ClientData unused,
					     Tcl_Interp *interp,
					     int argc, char **argv));
extern int Tdp_RDOCmd			_ANSI_ARGS_((ClientData unused,
					     Tcl_Interp *interp,
					     int argc, char **argv));
extern int Tdp_CancelRPCCmd		_ANSI_ARGS_((ClientData unused,
					     Tcl_Interp *interp,
					     int argc, char **argv));
extern int Tdp_RPCInit			_ANSI_ARGS_((Tcl_Interp *interp));
extern double ReadSysClock		_ANSI_ARGS_((void));

/* tkAppInit.c */
extern int Tcl_AppInit			_ANSI_ARGS_((Tcl_Interp *interp));

#endif
