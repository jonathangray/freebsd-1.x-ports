/*
 * ------------------------------------------------------------------------
 *  APPLICATION:  [incr Tcl]
 *      PURPOSE:  object-oriented extensions to Tcl
 *
 *  This segment provides common utility functions used throughout
 *  the other [incr Tcl] source files.
 *
 * ------------------------------------------------------------------------
 *  AUTHOR:  Michael J. McLennan       Phone: (610)712-2842
 *           AT&T Bell Laboratories   E-mail: michael.mclennan@att.com
 *
 *     RCS:  itcl_util.h,v 1.2 1994/03/25 19:01:17 mmc Exp
 * ========================================================================
 *                 Copyright (c) 1993  AT&T Bell Laboratories
 * ========================================================================
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that the copyright notice and warranty disclaimer appear in
 * supporting documentation, and that the names of AT&T Bell Laboratories
 * any of their entities not be used in advertising or publicity
 * pertaining to distribution of the software without specific, written
 * prior permission.
 * 
 * AT&T disclaims all warranties with regard to this software, including
 * all implied warranties of merchantability and fitness.  In no event
 * shall AT&T be liable for any special, indirect or consequential
 * damages or any damages whatsoever resulting from loss of use, data or
 * profits, whether in an action of contract, negligence or other
 * tortuous action, arising out of or in connection with the use or
 * performance of this software.
 * ========================================================================
 */
#ifndef ITCL_UTIL_H
#define ITCL_UTIL_H

#include "tcl.h"

typedef void (Itcl_FreeProc) _ANSI_ARGS_((ClientData cdata));

/*
 *  STACK INFO
 */
typedef struct Itcl_Stack {
	ClientData *values;     /* values on stack */
	int len;                /* number of values on stack */
	int max;                /* maximum size of stack */
	ClientData space[5];    /* initial space for stack data */
} Itcl_Stack;

/*
 *  FORWARD DECLARATIONS
 */
EXTERN void Itcl_InitStack _ANSI_ARGS_((Itcl_Stack *stack));
EXTERN void Itcl_DeleteStack _ANSI_ARGS_((Itcl_Stack *stack));
EXTERN void Itcl_PushStack _ANSI_ARGS_((ClientData cdata, Itcl_Stack *stack));
EXTERN ClientData Itcl_PopStack _ANSI_ARGS_((Itcl_Stack *stack));
EXTERN ClientData Itcl_PeekStack _ANSI_ARGS_((Itcl_Stack *stack));

EXTERN void Itcl_EventuallyFree _ANSI_ARGS_((ClientData cdata,
	Itcl_FreeProc *fproc));
EXTERN ClientData Itcl_PreserveData _ANSI_ARGS_((ClientData cdata));
EXTERN void Itcl_ReleaseData _ANSI_ARGS_((ClientData cdata));

EXTERN void Itcl_ParseSlotName _ANSI_ARGS_((char *name, char *cpart,
	char *spart, int bsize));
EXTERN char* Itcl_MakeSlotName _ANSI_ARGS_((char *cpart, char *spart));

EXTERN void Itcl_XferResult _ANSI_ARGS_((Tcl_Interp *to, Tcl_Interp *from,
	int status));

#endif
