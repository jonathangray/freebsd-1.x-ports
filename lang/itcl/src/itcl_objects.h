/*
 * ------------------------------------------------------------------------
 *  APPLICATION:  [incr Tcl]
 *      PURPOSE:  object-oriented extensions to Tcl
 *
 *  This segment handles "objects" which are instanciated from class
 *  definitions.  Objects contain public/protected data members from
 *  all classes in a derivation hierarchy.
 *
 * ------------------------------------------------------------------------
 *  AUTHOR:  Michael J. McLennan       Phone: (610)712-2842
 *           AT&T Bell Laboratories   E-mail: michael.mclennan@att.com
 *
 *     RCS:  itcl_objects.h,v 1.1.1.1 1994/03/21 22:09:48 mmc Exp
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
#ifndef ITCL_OBJECTS_H
#define ITCL_OBJECTS_H

#include "tcl.h"
#include "tclInt.h"

#include "itcl_class.h"

/*
 *  OBJECT INFO
 */
typedef struct Itcl_Object {
	char *name;                 /* object name */
	Itcl_Class *cdefn;          /* most specific class */
	Tcl_Interp *interp;         /* interpreter managing object */
	Tcl_HashTable data;         /* public/protected variables */
	Itcl_VarScope *vscopes;     /* list of variable scopes */
} Itcl_Object;

/*
 *  FORWARD DECLARATIONS
 */
EXTERN Itcl_Object* Itcl_CreateObject _ANSI_ARGS_((char* name,
	Itcl_Class *cdefn));
EXTERN void Itcl_DeleteObject _ANSI_ARGS_((ClientData cdata));

#endif
