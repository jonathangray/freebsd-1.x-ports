/*
 * ------------------------------------------------------------------------
 *  APPLICATION:  [incr Tcl]
 *      PURPOSE:  object-oriented extensions to Tcl
 *
 *  This segment handles variables which are local to the scope of a
 *  class.  Variables are classified as "public", "protected" or "common".
 *  Public variables can be manipulated from outside of the class
 *  scope via the special "config" formal argument.  Protected variables
 *  can only be accessed within the class scope.  Each object has its
 *  own copy of public and protected variables.  Common variables, on
 *  the other hand, are allocated once for each class and are shared
 *  among objects.
 *
 * ------------------------------------------------------------------------
 *  AUTHOR:  Michael J. McLennan       Phone: (610)712-2842
 *           AT&T Bell Laboratories   E-mail: michael.mclennan@att.com
 *
 *     RCS:  itcl_vars.h,v 1.2 1994/04/25 19:15:00 mmc Exp
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
#ifndef ITCL_VARS_H
#define ITCL_VARS_H

#include "tcl.h"
#include "tclInt.h"

/*
 *  VARIABLE INFO
 */
struct Itcl_Class;
struct Itcl_Object;
typedef struct Itcl_Variable {
	char *name;                  /* variable name */
	char *fullname;              /* class::variable name */
	struct Itcl_Class *cdefn;    /* class containing this variable */
	char *init;                  /* initial value */
	char *config;                /* configuration code (public vars) */
	struct Itcl_Variable *next;  /* next element in linked list */
} Itcl_Variable;

/*
 *  REGISTRY FOR VARIABLES IN EACH CLASS'S SCOPE
 */
typedef struct Itcl_VarScope {
    struct Itcl_Class *cdefn;    /* class scope for this registry */
    Tcl_HashTable allVars;       /* all variables in this scope */
    struct Itcl_VarScope *next;  /* next record in linked list */
} Itcl_VarScope;

/*
 *  REPRESENTATION OF CLASS SCOPE DURING EXECUTION
 */
typedef struct Itcl_ExecScope {
	int frameActive;         /* non-zero => frame in use */
    CallFrame frame;         /* call frame representing scope */
    CallFrame *savedVFPtr;   /* varFramePtr in "from" interp */
} Itcl_ExecScope;

/*
 *  FORWARD DECLARATIONS
 */
EXTERN Itcl_Variable* Itcl_CreateVariable _ANSI_ARGS_((
	struct Itcl_Class* cdefn, char* name, char* init, char* config));
EXTERN void Itcl_DeleteVariable _ANSI_ARGS_((Itcl_Variable **vptr));

EXTERN Itcl_VarScope* Itcl_CreateVarScope _ANSI_ARGS_((
	struct Itcl_Object* odefn, struct Itcl_Class *scope));
EXTERN void Itcl_DeleteVarScope _ANSI_ARGS_((Itcl_VarScope* vscope));

EXTERN int Itcl_GlobalCmd _ANSI_ARGS_((ClientData cdata, Tcl_Interp *interp,
    int argc, char **argv));
EXTERN char* Itcl_GetVar _ANSI_ARGS_((Itcl_Variable *vdefn,
	Tcl_HashTable *vlist));

EXTERN Var* Itcl_CreateTclVar _ANSI_ARGS_((Itcl_Variable *vdefn,
	Tcl_HashTable *vlist));
EXTERN void Itcl_CreateTclVarLinks _ANSI_ARGS_((Itcl_Variable *vdefn,
	Tcl_HashTable *vlist, Tcl_HashTable *scope, int flags));
EXTERN void Itcl_DeleteTclVars _ANSI_ARGS_((Interp *iPtr,
	Tcl_HashTable *vlist));

EXTERN void Itcl_EnterExecScope _ANSI_ARGS_((Tcl_Interp *interp,
	Itcl_ExecScope *exscope, Itcl_VarScope *vscope, Tcl_Interp *from));
EXTERN void Itcl_LeaveExecScope _ANSI_ARGS_((Tcl_Interp *interp,
	Itcl_ExecScope *exscope));

EXTERN void Itcl_InstallCallFrame _ANSI_ARGS_((Tcl_Interp *interp,
	CallFrame *frame, Itcl_VarScope *vscope));
EXTERN void Itcl_SetArgVar _ANSI_ARGS_((Tcl_Interp *interp,
	char *name, char *value));
EXTERN void Itcl_RemoveCallFrame _ANSI_ARGS_((Tcl_Interp *interp,
	CallFrame *frame));

#endif
