/*
 * ------------------------------------------------------------------------
 *  APPLICATION:  [incr Tcl]
 *      PURPOSE:  object-oriented extensions to Tcl
 *
 *  This segment handles class definitions.  Classes are composed of
 *  data members (public/protected/common) and the member functions
 *  (methods/procs) that operate on them.  Each class has its own
 *  interpreter which is used to parse the class definition and
 *  implement the private class scope.
 *
 * ------------------------------------------------------------------------
 *  AUTHOR:  Michael J. McLennan       Phone: (610)712-2842
 *           AT&T Bell Laboratories   E-mail: michael.mclennan@att.com
 *
 *     RCS:  itcl_class.h,v 1.1.1.1 1994/03/21 22:09:47 mmc Exp
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
#ifndef ITCL_CLASS_H
#define ITCL_CLASS_H

#include "tcl.h"

#include "itcl_util.h"
#include "itcl_methods.h"
#include "itcl_vars.h"

/*
 *  CLASS INFO
 */
struct Itcl_ClassInfo;
typedef struct Itcl_Class {
	char *name;                   /* class name */
	struct Itcl_ClassInfo *info;  /* list containing this class */
	Tcl_Interp *interp;           /* interpreter representing class scope */
	struct Itcl_Class **supers;   /* list of superclasses */
	Tcl_HashTable methods;        /* list of class methods */
	Tcl_HashTable allMethods;     /* methods for all classes in hierarchy */
	Tcl_HashTable procs;          /* list of class procs (static methods) */
	Tcl_HashTable allProcs;       /* procs for all classes in hierarchy */
	Itcl_Variable *publics;       /* list of public variables */
	Itcl_Variable *protecteds;    /* list of protected variables */
	Itcl_Variable *commons;       /* list of common variables */
	Tcl_HashTable data;           /* data for common variables */
	Itcl_VarScope *vscope;        /* scope for common class data */
	int unique;                   /* unique number for #auto generation */
} Itcl_Class;

/*
 *  CLASS HIERARCHY TRAVERSAL
 */
typedef struct Itcl_HierIter {
	Itcl_Class *current;          /* current position in hierarchy */
	Itcl_Stack stack;             /* stack used for traversal */
} Itcl_HierIter;

/*
 *  FORWARD DECLARATIONS
 */
EXTERN Itcl_Class* Itcl_CreateClass _ANSI_ARGS_((char* name,
	struct Itcl_ClassInfo *info));
EXTERN void Itcl_DeleteClass _ANSI_ARGS_((ClientData cdata));

EXTERN void Itcl_AddMethod _ANSI_ARGS_((Itcl_Class *cdefn,
	Itcl_Method *mdefn));
EXTERN void Itcl_AddProc _ANSI_ARGS_((Itcl_Class *cdefn,
	Itcl_Method *mdefn));
EXTERN Itcl_Method* Itcl_FindMethod _ANSI_ARGS_((char *cname, char *mname,
    Itcl_Class *cdefn));
EXTERN Itcl_Method* Itcl_FindProc _ANSI_ARGS_((char *cname, char *mname,
    Itcl_Class *cdefn));

EXTERN void Itcl_AddVariable _ANSI_ARGS_((Itcl_Variable **vlist,
    Itcl_Variable *vdefn));
EXTERN Itcl_Variable* Itcl_FindPublicVar _ANSI_ARGS_((char *cname,
	char *vname, Itcl_Class *cdefn));
EXTERN Itcl_Variable* Itcl_FindProtectedVar _ANSI_ARGS_((char *cname,
	char *vname, Itcl_Class *cdefn));
EXTERN Itcl_Variable* Itcl_FindCommonVar _ANSI_ARGS_((char *cname,
	char *vname, Itcl_Class *cdefn));

EXTERN void Itcl_AddMethodCmd _ANSI_ARGS_((Itcl_Class *cdefn,
	char *name, Itcl_Method* mdefn));
EXTERN void Itcl_AddProcCmd _ANSI_ARGS_((Itcl_Class *cdefn,
	char *name, Itcl_Method* mdefn));

EXTERN void Itcl_InitHierIter _ANSI_ARGS_((Itcl_HierIter *iter,
	Itcl_Class *cdefn));
EXTERN void Itcl_DeleteHierIter _ANSI_ARGS_((Itcl_HierIter *iter));
EXTERN Itcl_Class* Itcl_AdvanceHierIter _ANSI_ARGS_((Itcl_HierIter *iter));

#endif
