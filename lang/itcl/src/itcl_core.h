/*
 * ------------------------------------------------------------------------
 *  APPLICATION:  [incr Tcl]
 *      PURPOSE:  object-oriented extensions to Tcl
 *
 *  [incr tcl] provides object-oriented extensions to TCL, much as
 *  C++ provides object-oriented extensions to C.  It provides a means
 *  of encapsulating related procedures together with their shared data
 *  in a local namespace that is hidden from the outside world.  It
 *  promotes code re-use through inheritance.  More than anything else,
 *  it encourages better organization of Tcl applications through the
 *  object-oriented paradigm, leading to code that is easier to
 *  understand and maintain.
 *  
 *  USAGE:
 *  
 *    itcl_class <className> {
 *        inherit <superclass> ?<superclass>...?
 *
 *        constructor {<arglist>} { <body> }
 *        destructor { <body> }
 *
 *        method <name> {<arglist>} { <body> }
 *        proc <name> {<arglist>} { <body> }
 *
 *        public <varname> ?<init>? ?<config>?
 *        protected <varname> ?<init>?
 *        common <varname> ?<init>?
 *
 *        ?<Tcl-commands>?
 *    }
 *
 *    itcl_info classes ?<pattern>?
 *    itcl_info objects ?<pattern>? ?-class <className>? ?-isa <className>?
 *
 *    <className> <objName> ?<constructor-args>?
 *    <className> #auto ?<constructor-args>?
 *    <className> :: <command> ?<args>...?
 *
 *    <objName> <method> ?<args>...?
 *
 *
 *  BUILT-IN CLASS MEMBER FUCTIONS:
 *
 *    info <option> ?<args>...?
 *    previous <command> ?<args>...?
 *    global <varName> ?<varName>...?
 *
 *    isa <className>
 *    delete
 *
 * ------------------------------------------------------------------------
 *  AUTHOR:  Michael J. McLennan       Phone: (610)712-2842
 *           AT&T Bell Laboratories   E-mail: michael.mclennan@att.com
 *
 *     RCS:  itcl_core.h,v 1.1.1.1 1994/03/21 22:09:47 mmc Exp
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
#ifndef ITCL_CORE_H
#define ITCL_CORE_H

#include "tcl.h"

#include "itcl_util.h"
#include "itcl_class.h"
#include "itcl_objects.h"

/*
 *  MANAGEMENT INFO
 */
typedef struct Itcl_ClassInfo {
	Tcl_Interp *main;           /* interpreter that manages this info */
	Tcl_HashTable classes;      /* list of all known classes */
	Tcl_HashTable objects;      /* list of all known objects */
	Itcl_Stack cdefnStack;      /* stack of class definitions being built */
	Itcl_Stack objStack;        /* stack of objects invoked during execution */
} Itcl_ClassInfo;

#endif
