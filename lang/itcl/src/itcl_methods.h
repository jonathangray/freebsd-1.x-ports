/*
 * ------------------------------------------------------------------------
 *  APPLICATION:  [incr Tcl]
 *      PURPOSE:  object-oriented extensions to Tcl
 *
 *  This segment handles "methods" which are procedures defined within
 *  the scope of a class.  In a class definition for [incr Tcl], the
 *  term "method" is used for a procedure that has access to object-
 *  specific data, while the term "proc" is used for a procedure that
 *  has access only to common class data.  Here the term "method" is
 *  used more generically to apply to both.
 *
 * ------------------------------------------------------------------------
 *  AUTHOR:  Michael J. McLennan       Phone: (610)712-2842
 *           AT&T Bell Laboratories   E-mail: michael.mclennan@att.com
 *
 *     RCS:  itcl_methods.h,v 1.1.1.1 1994/03/21 22:09:48 mmc Exp
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
#ifndef ITCL_METHODS_H
#define ITCL_METHODS_H

#include "tcl.h"
#include "tclInt.h"

typedef int (Itcl_MethodProc) _ANSI_ARGS_((ClientData cdata, Tcl_Interp *interp,
	int argc, char **argv));

/*
 *  METHOD INFO
 */
struct Itcl_Class;
typedef struct Itcl_Method {
	char *name;                 /* method name */
	struct Itcl_Class *cdefn;   /* class containing this method */
	Arg *arglist;               /* list of arg names and initial values */
	int type;                   /* method type (ITCL_*_METHOD) */
	union {
		char *body;                  /* body (TCL statements) */
		Itcl_MethodProc *builtin;    /* body (built-in C code) */
		struct Itcl_Method *actual;  /* reference to inherited method */
	} values;
	int invoked;                /* see object construction/destruction */
} Itcl_Method;

#define ITCL_TCL_METHOD        1
#define ITCL_BUILTIN_METHOD    2
#define ITCL_INHERITED_METHOD  3

/*
 *  FORWARD DECLARATIONS
 */
EXTERN Itcl_Method* Itcl_CreateMethod _ANSI_ARGS_((struct Itcl_Class *cdefn,
	char* name, char* arglist, char* body));
EXTERN Itcl_Method* Itcl_CreateBiMethod _ANSI_ARGS_((struct Itcl_Class *cdefn,
	char* name, Itcl_MethodProc *proc));
EXTERN Itcl_Method* Itcl_CreateRefMethod _ANSI_ARGS_((struct Itcl_Class *cdefn,
	Itcl_Method *mdefn));
EXTERN void Itcl_DeleteMethod _ANSI_ARGS_((Itcl_Method *mdefn));

EXTERN Arg* Itcl_CreateArg _ANSI_ARGS_ ((char* name, char* init));
EXTERN char* Itcl_ArgList _ANSI_ARGS_ ((Arg* arg));

EXTERN int Itcl_ExecMethod _ANSI_ARGS_((ClientData cdata, Tcl_Interp *interp,
    int argc, char **argv));
EXTERN int Itcl_ExecProc _ANSI_ARGS_((ClientData cdata, Tcl_Interp *interp,
    int argc, char **argv));

EXTERN int Itcl_UnknownCmd _ANSI_ARGS_((ClientData cdata, Tcl_Interp *interp,
    int argc, char **argv));
EXTERN int Itcl_PreviousCmd _ANSI_ARGS_((ClientData cdata, Tcl_Interp *interp,
    int argc, char **argv));
EXTERN int Itcl_VirtualCmd _ANSI_ARGS_((ClientData cdata, Tcl_Interp *interp,
    int argc, char **argv));
EXTERN int Itcl_UplevelCmd _ANSI_ARGS_((ClientData cdata, Tcl_Interp *interp,
    int argc, char **argv));

#endif
