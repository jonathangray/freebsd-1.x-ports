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
 *     RCS:  itcl_vars.c,v 1.3 1994/04/25 19:14:57 mmc Exp
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
#include "itcl_util.h"
#include "itcl_vars.h"
#include "itcl_class.h"
#include "itcl_core.h"

#define ITCL_VAR_LINK 0x4000

static Var* NewVar _ANSI_ARGS_((void));
static void CleanupVar _ANSI_ARGS_((Var* varPtr, Var* arrayPtr));

#ifndef lint
static char sccsid[] = "@(#)itcl_vars.c	1.15 (10/25/93) Michael J. McLennan";
#endif


/*
 * ------------------------------------------------------------------------
 *  Itcl_CreateVariable()
 *
 *  Creates a new class variable definition and returns a pointer to it.
 * ------------------------------------------------------------------------
 */
Itcl_Variable*
Itcl_CreateVariable(cdefn,name,init,config)
	Itcl_Class* cdefn;   /* class containing this variable */
	char* name;          /* name of new variable */
	char* init;          /* initial value for variable */
	char* config;        /* TCL code invoked when variable is config'd */
{
	Itcl_Variable *vdefn;
	char *fullname;

	vdefn = (Itcl_Variable*)ckalloc(sizeof(Itcl_Variable));
	vdefn->cdefn = cdefn;

	vdefn->name = (char*)ckalloc((unsigned)(strlen(name)+1));
	strcpy(vdefn->name, name);

	fullname = Itcl_MakeSlotName(cdefn->name, name);
	vdefn->fullname = (char*)ckalloc((unsigned)(strlen(fullname)+1));
	strcpy(vdefn->fullname, fullname);

	vdefn->init   = NULL;
	vdefn->config = NULL;

	if (init)
	{
		vdefn->init = (char*)ckalloc((unsigned)(strlen(init)+1));
		strcpy(vdefn->init, init);
	}
	if (config)
	{
		vdefn->config = (char*)ckalloc((unsigned)(strlen(config)+1));
		strcpy(vdefn->config, config);
	}
	vdefn->next = NULL;
	return vdefn;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_DeleteVariable()
 *
 *  Destroys the given class variable, freeing all resources associated
 *  with it.
 * ------------------------------------------------------------------------
 */
void
Itcl_DeleteVariable(vptr)
	Itcl_Variable **vptr;  /* pointer to variable definition to be destroyed */
{
	Itcl_Variable *next = (*vptr)->next;

	ckfree((*vptr)->name);
	ckfree((*vptr)->fullname);
	if ((*vptr)->init)
		ckfree((*vptr)->init);
	if ((*vptr)->config)
		ckfree((*vptr)->config);
	ckfree((char*)(*vptr));

	*vptr = next;  /* close up linked list */
}


/*
 * ------------------------------------------------------------------------
 *  Itcl_CreateVarScope()
 *
 *  Creates a new variable scope, which is basically a hash table
 *  connecting the varible names that are used in a particular class
 *  scope to the actual variables in the object/class data.  Variable
 *  names have different meanings in different scopes, since each
 *  class can redefine variables that override (or "shadow") variables
 *  in a base class.  If the "odefn" pointer is NULL, only the common
 *  class variables are added to the scope.
 *  Returns a pointer to the new scope object.
 * ------------------------------------------------------------------------
 */
Itcl_VarScope*
Itcl_CreateVarScope(odefn,cdefn)
	Itcl_Object *odefn;  /* for object-specific public/protected vars */
	Itcl_Class *cdefn;   /* class representing scope */
{
	Itcl_VarScope *vscope;
	Itcl_Class *cd;
	Itcl_Variable *vdefn;
	Itcl_HierIter hier;

	/*
	 *  Allocate and initialize the variable scope definition.
	 */
	vscope = (Itcl_VarScope*)ckalloc(sizeof(Itcl_VarScope));

	Tcl_InitHashTable(&vscope->allVars, TCL_STRING_KEYS);
	vscope->cdefn = cdefn;
	vscope->next  = NULL;

	/*
	 *  Scan through all public/protected/common variable definitions
	 *  and create a list of variables recognized at this scope.
	 */
	Itcl_InitHierIter(&hier,cdefn);
	while ((cd=Itcl_AdvanceHierIter(&hier)) != NULL)
	{
		if (odefn)
		{
			for (vdefn=cd->publics; vdefn; vdefn=vdefn->next)
				Itcl_CreateTclVarLinks(vdefn, &odefn->data,
					&vscope->allVars, ITCL_VAR_LINK);

			for (vdefn=cd->protecteds; vdefn; vdefn=vdefn->next)
				Itcl_CreateTclVarLinks(vdefn, &odefn->data,
					&vscope->allVars, ITCL_VAR_LINK);
		}
		for (vdefn=cd->commons; vdefn; vdefn=vdefn->next)
			Itcl_CreateTclVarLinks(vdefn, &cd->data,
					&vscope->allVars, ITCL_VAR_LINK);
	}
	Itcl_DeleteHierIter(&hier);

	return vscope;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_DeleteVarScope()
 *
 *  Destroys the given variable scope, freeing all data associated
 *  with it.  Usually invoked when an object definition is destroyed.
 * ------------------------------------------------------------------------
 */
void
Itcl_DeleteVarScope(vscope)
	Itcl_VarScope *vscope;  /* variable scope data */
{
	if (vscope)
	{
		Interp *iPtr = (Interp*)vscope->cdefn->interp;
		Itcl_DeleteTclVars(iPtr, &vscope->allVars);
		ckfree((char*)vscope);
	}
}


/*
 * ------------------------------------------------------------------------
 *  Itcl_GlobalCmd()
 *
 *  Installed into the interpreter associated with a class, and invoked
 *  to access global variables in the external interpreter.  Makes the
 *  usual "global" command refer to global variables in the outside
 *  world, instead of global variables in the class scope.
 *
 *    global <varName> ?<varName>...?
 *
 * ------------------------------------------------------------------------
 */
int
Itcl_GlobalCmd(clientData, interp, argc, argv)
	ClientData clientData;   /* class definition */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	Itcl_Class *cdefn = (Itcl_Class*)clientData;
	register Interp *iPtr = (Interp*)interp;
	register Interp *exPtr = (Interp*)cdefn->info->main;

	Var *link, *gVar;
	Tcl_HashEntry *entry, *gEntry;
	Tcl_HashTable *vlist;
	int newEntry;

	Tcl_SetResult(interp, "", TCL_STATIC);
	if (argc < 2)
	{
		Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0], " varName ?varName...?\"",
			(char*)NULL);
		return TCL_ERROR;
	}

	/*
	 *  Create links for all names declared as globals.
	 */
	for (argc--, argv++; argc > 0; argc--, argv++)
	{
		gEntry = Tcl_CreateHashEntry(&exPtr->globalTable, *argv, &newEntry);
		if (newEntry)
		{
			gVar = NewVar();
			Tcl_SetHashValue(gEntry, gVar);
			gVar->hPtr = gEntry;
		}
		else
			gVar = (Var*)Tcl_GetHashValue(gEntry);

		vlist = (iPtr->varFramePtr)
			? &iPtr->varFramePtr->varTable
			: &iPtr->globalTable;

		entry = Tcl_CreateHashEntry(vlist, *argv, &newEntry);
		if (newEntry)
			link = NewVar();
		else
		{
			link = (Var*)Tcl_GetHashValue(entry);
			if (link->flags & VAR_UPVAR)
			{
				Var *upvarPtr;
				upvarPtr = link->value.upvarPtr;

				if (upvarPtr == gVar)
					return TCL_OK;

				upvarPtr->refCount--;
				if (upvarPtr->flags & VAR_UNDEFINED)
					CleanupVar(upvarPtr, (Var*)NULL);
			}
			else if (!(link->flags & VAR_UNDEFINED))
			{
				Tcl_AppendResult(interp, "variable \"", *argv,
					"\" already exists",
					(char*)NULL);
				return TCL_ERROR;
			}
		}
		link->flags = (link->flags & ~VAR_UNDEFINED) | VAR_UPVAR;
		link->value.upvarPtr = gVar;
		gVar->refCount++;
		Tcl_SetHashValue(entry, link);
		link->hPtr = entry;
	}
	return TCL_OK;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_GetVar()
 *
 *  Returns the value of the specified data member, or NULL if that
 *  member is not found on the given list.
 * ------------------------------------------------------------------------
 */
char*
Itcl_GetVar(vdefn,vlist)
	Itcl_Variable *vdefn;  /* desired variable */
	Tcl_HashTable *vlist;  /* variable list representing scope */
{
	Tcl_HashEntry *vEntry;

	vEntry = Tcl_FindHashEntry(vlist,vdefn->fullname);
	if (vEntry)
	{
		Var *var = (Var*)Tcl_GetHashValue(vEntry);
		if (var->flags & VAR_UPVAR)
		{
			var = var->value.upvarPtr;
			if (var) return var->value.string;
		}
		else if (var->flags & VAR_ARRAY)
			return "<array>";
		else if (var->flags & VAR_UNDEFINED)
			return "<undefined>";
		else
			return var->value.string;
	}
	return (char*)NULL;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_CreateTclVar()
 *
 *  Creates the structure representing a Tcl variable and installs
 *  the variable into the hash table representing a scope.  Provides
 *  a cleaner interface to the Tcl "NewVar" routine.
 * ------------------------------------------------------------------------
 */
Var*
Itcl_CreateTclVar(vdefn,vlist)
	Itcl_Variable* vdefn;   /* class containing this variable */
	Tcl_HashTable *vlist;   /* list of Tcl variables */
{
	Var *newvar;
	Tcl_HashEntry *entry;
	int newEntry;

	newvar = NewVar();
	if (vdefn->init)
	{
		int newSize = strlen(vdefn->init)+1;

		newSize  = (newSize < 24) ? 24 : newSize;
		newvar->valueSpace   = newSize;
		newvar->value.string = ckalloc((unsigned)newSize);
		strcpy(newvar->value.string, vdefn->init);
		newvar->flags &= ~VAR_UNDEFINED;
	}

	/*
	 *  Set refCount=1 so that this variable won't look
	 *  like garbage and accidentally get destroyed by
	 *  TclDeleteVars() after a method invocation.
	 */
	newvar->refCount = 1;

	entry = Tcl_CreateHashEntry(vlist,vdefn->fullname,&newEntry);
	Tcl_SetHashValue(entry, (ClientData)newvar);
	newvar->hPtr = entry;

	return newvar;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_CreateTclVarLinks()
 *
 *  Creates the structure representing an "upvar" link to a Tcl variable
 *  and installs the variable into the hash table representing a scope.
 *  Actually, two such links are created:  One for the full variable
 *  name, and another for the short name if it is not already known.
 *  If "flags" are specified, they are added to the flags for the "upvar"
 *  structure; flags are usually included to signal that these links
 *  should be protected from destruction in Itcl_RemoveCallFrame().
 *  Provides a cleaner interface to the Tcl "NewVar" routine.
 * ------------------------------------------------------------------------
 */
void
Itcl_CreateTclVarLinks(vdefn,vlist,scope,flags)
	Itcl_Variable* vdefn;   /* class containing this variable */
	Tcl_HashTable *vlist;   /* variable list containing actual Var */
	Tcl_HashTable *scope;   /* variable list representing scope */
	int flags;              /* flags added to "upvar" links */
{
	int newEntry;
	Tcl_HashEntry *vEntry, *lEntry;
	Var *orgVar, *linkVar;

	/*
	 *  Find (or create) the original variable to link to.
	 */
	vEntry = Tcl_CreateHashEntry(vlist, vdefn->fullname, &newEntry);
	if (newEntry)
	{
		orgVar = NewVar();
		Tcl_SetHashValue(vEntry, orgVar);
		orgVar->hPtr = vEntry;
	}
	else
		orgVar = (Var*)Tcl_GetHashValue(vEntry);

	/*
	 *  Create a link variable for the explicit variable name.
	 */
	linkVar = NewVar();
	linkVar->flags = (linkVar->flags & ~VAR_UNDEFINED) | VAR_UPVAR | flags;
	linkVar->value.upvarPtr = orgVar;
	if (flags != 0)           /* special [incr Tcl] flags? */
		linkVar->refCount++;  /* then protect link from being deleted */
	orgVar->refCount++;

	lEntry = Tcl_CreateHashEntry(scope, vdefn->fullname, &newEntry);
	Tcl_SetHashValue(lEntry, (ClientData)linkVar);
	linkVar->hPtr = lEntry;

	/*
	 *  Create a link variable for the short variable name
	 *  (without scope::) if this name is not already known.
	 */
	if (!Tcl_FindHashEntry(scope,vdefn->name))
	{
		linkVar = NewVar();
		linkVar->flags = (linkVar->flags & ~VAR_UNDEFINED) | VAR_UPVAR | flags;
		linkVar->value.upvarPtr = orgVar;
		if (flags != 0)           /* special [incr Tcl] flags? */
			linkVar->refCount++;  /* then protect link from being deleted */
		orgVar->refCount++;

		lEntry = Tcl_CreateHashEntry(scope, vdefn->name, &newEntry);
		Tcl_SetHashValue(lEntry, (ClientData)linkVar);
		linkVar->hPtr = lEntry;
	}
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_DeleteTclVars()
 *
 *  Destroys all variables in the given hash table.  Decrements the
 *  variable reference count (thereby releasing [incr Tcl]'s claim on
 *  the variable), and then calls the usual TclDeleteVars() to invoke
 *  traces and destroy the variables.
 * ------------------------------------------------------------------------
 */
void
Itcl_DeleteTclVars(iPtr, vlist)
	Interp* iPtr;          /* interpreter responsible for vars (for traces) */
	Tcl_HashTable *vlist;  /* variable list to be destroyed */
{
	Tcl_HashSearch place;
	Tcl_HashEntry *entry;
	Var *varPtr;

	/*
	 *  Decrement all reference counts to release [incr Tcl]'s claim
	 *  to these variables.
	 */
	entry = Tcl_FirstHashEntry(vlist, &place);
	while (entry)
	{
		varPtr = (Var*)Tcl_GetHashValue(entry);
		varPtr->refCount--;

		entry = Tcl_NextHashEntry(&place);
	}
	TclDeleteVars(iPtr,vlist);
}


/*
 * ------------------------------------------------------------------------
 *  Itcl_EnterExecScope()
 *
 *  Modifies an interpreter to recognize a new scope of execution.
 *  This routine is needed to allow multiple interpreters to share a
 *  single stack of CallFrames.  Upon entering a new execution scope,
 *  the "varFramePtr" in the new interpreter is set to the current
 *  stack in the calling interpreter.  This gives the "upvar" and
 *  "uplevel" commands in the new interpreter access to all existing
 *  CallFrames, regardless of which interpreter they belong to.
 *
 *  This routine establishes a new execution scope and returns a token
 *  representing the new scope.  This token must be passed to
 *  Itcl_LeaveExecScope() to change the scope back to the calling
 *  interpreter.  Specifically, this routine does the following:
 *
 *    1) Patches the "varFramePtr" in the execution interpreter
 *       to recognize CallFrames in the calling interpreter.
 *
 *    2) If a variable scope is specified, a new CallFrame is
 *       created and initialized with variables in that scope.
 *
 * ------------------------------------------------------------------------
 */
void
Itcl_EnterExecScope(interp,exscope,vscope,from)
	Tcl_Interp* interp;       /* execution interpreter for new scope */
	Itcl_ExecScope *exscope;  /* token to be initialized */
	Itcl_VarScope *vscope;    /* list of variables at this scope */
	Tcl_Interp* from;         /* calling interpreter */
{
	Interp *eiPtr = (Interp*)interp;
	Interp *ciPtr = (Interp*)from;

	/*
	 *  Reset the result in the new interp so that error
	 *  info gets wiped clean.
	 */
	Tcl_ResetResult(interp);

	/*
	 *  Patch in CallFrame stack from calling interpreter.
	 */
	exscope->savedVFPtr = eiPtr->varFramePtr;
	eiPtr->varFramePtr  = ciPtr->varFramePtr;

	/*
	 *  If a variable scope was specificied, install a new CallFrame
	 *  initialized for this scope.
	 */
	exscope->frameActive = 0;
	if (vscope)
	{
		Itcl_InstallCallFrame(interp, &exscope->frame, vscope);
		exscope->frameActive = ~0;
	}
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_LeaveExecScope()
 *
 *  Cleans up after a previous call to Itcl_EnterExecScope().  Marks
 *  the end of execution in a particular scope.  If a CallFrame was
 *  established when the scope was entered, it is removed at this point.
 *  The execution interpreter is also patched to resume its previous
 *  CallFrame stack.
 * ------------------------------------------------------------------------
 */
void
Itcl_LeaveExecScope(interp,exscope)
	Tcl_Interp* interp;       /* execution interpreter for new scope */
	Itcl_ExecScope *exscope;  /* token to be initialized */
{
	Interp *eiPtr = (Interp*)interp;

	/*
	 *  If a CallFrame is active, then remove it.
	 */
	if (exscope->frameActive)
		Itcl_RemoveCallFrame(interp, &exscope->frame);

	/*
	 *  Patch in CallFrame stack back to its original state.
	 */
	eiPtr->varFramePtr  = exscope->savedVFPtr;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_InstallCallFrame()
 *
 *  Initializes a Tcl CallFrame and installs it into the given
 *  interpreter.  The variable table is automatically populated with
 *  a series of variables from the given reference frame.  This action
 *  prepares the interpreter for a method/proc invocation.
 * ------------------------------------------------------------------------
 */
void
Itcl_InstallCallFrame(interp,frame,vscope)
	Tcl_Interp* interp;     /* interpreter containing data */
	CallFrame *frame;       /* raw call frame to be installed */
	Itcl_VarScope *vscope;  /* list of variables at this scope */
{
	Interp *iPtr = (Interp*)interp;

	int newEntry;
	char *vname;
	Tcl_HashSearch place;
	register Tcl_HashEntry *entry, *copy;

	/*
	 *  Install the class's call frame for the proc invocation.
	 *  (Borrowing heavily from the "InterpProc" routine in "tclProc.c")
	 */
	if (iPtr->varFramePtr != NULL)
		frame->level = iPtr->varFramePtr->level + 1;
	else
		frame->level = 1;

	frame->argc = 0;
	frame->argv = (char**)NULL;
	frame->callerPtr = iPtr->framePtr;
	frame->callerVarPtr = iPtr->varFramePtr;
	iPtr->framePtr = frame;
	iPtr->varFramePtr = frame;

	/*
	 *  Populate the variable list in the frame with variables from
	 *  the current scope.
	 */
	Tcl_InitHashTable(&frame->varTable, TCL_STRING_KEYS);

	if (vscope)
	{
		entry = Tcl_FirstHashEntry(&vscope->allVars, &place);
		while (entry)
		{
			vname = Tcl_GetHashKey(&vscope->allVars,entry);
			copy  = Tcl_CreateHashEntry(&frame->varTable, vname, &newEntry);
			Tcl_SetHashValue(copy, Tcl_GetHashValue(entry));

			entry = Tcl_NextHashEntry(&place);
		}
	}
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_SetArgVar()
 *
 *  Similar to Tcl_SetVar() in that it sets a variable with the given
 *  name to the specified value.  But this routine also checks the
 *  local variables in the current call frame to make sure that the
 *  requested variable is not already installed as a data member from
 *  an [incr Tcl] class.  If it is, the reference to the data member
 *  is removed, and a new variable is created in its place.  Used to
 *  initialize formal arguments for methods/procs.  Makes sure that
 *  formal arguments do not clobber data members having the same names.
 * ------------------------------------------------------------------------
 */
void
Itcl_SetArgVar(interp,name,value)
	Tcl_Interp* interp;  /* interpreter containing data */
	char *name;          /* name of argument being set */
	char *value;         /* new value for argument variable */
{
	Interp *iPtr = (Interp*)interp;
	Tcl_HashEntry *entry;
	Var *var;

	/*
	 *  If interp has a CallFrame, then check for a variable
	 *  already installed with the argument name.  If any such
	 *  variable exists, and if it is being used by [incr Tcl],
	 *  then unlink it before setting the argument variable.
	 */
	if (iPtr->framePtr)
	{
		entry = Tcl_FindHashEntry(&iPtr->framePtr->varTable, name);
		if (entry)
		{
			var = (Var*)Tcl_GetHashValue(entry);

			if ((var->flags & VAR_UPVAR) && (var->flags & ITCL_VAR_LINK))
				Tcl_DeleteHashEntry(entry);
		}
	}
	Tcl_SetVar(interp, name, value, 0);
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_RemoveCallFrame()
 *
 *  Removes the top-most frame for a Tcl interpreter, and destroys all
 *  variables except for "upvar" links marked as ITCL_VAR_LINKs.
 *  These links represent class/object data created by [incr Tcl],
 *  and are left intact for the next proc/method invocation.
 * ------------------------------------------------------------------------
 */
void
Itcl_RemoveCallFrame(interp,frame)
	Tcl_Interp* interp;  /* interpreter containing data */
	CallFrame *frame;    /* call frame to be cleaned */
{
	Interp *iPtr = (Interp*)interp;

	int newEntry;
	char *vname;
	Tcl_HashSearch place;
	Tcl_HashEntry *entry, *copy;
	Tcl_HashTable trash;
	Var *var;

	/*
	 *  Remove the call frame from the interpreter.  It's important
	 *  to do this so that traces invoked during the deletion don't
	 *  see the partially deleted frame.
	 */
	iPtr->framePtr = frame->callerPtr;
	iPtr->varFramePtr = frame->callerVarPtr;

	/*
	 *  Scan through all variables in the given frame and look
	 *  for "upvar" links with a magic link count.  Protect these
	 *  variables, but transfer all others to a list for deletion.
	 */
	Tcl_InitHashTable(&trash, TCL_STRING_KEYS);

	entry = Tcl_FirstHashEntry(&frame->varTable, &place);
	while (entry)
	{
		var = (Var*)Tcl_GetHashValue(entry);

		/*
		 *  Protect class/object data members
		 */
		if ((var->flags & VAR_UPVAR) && (var->flags & ITCL_VAR_LINK))
			;  /* protected data member */

		/*
		 *  Destroy all other variables.
		 */
		else
		{
			vname = Tcl_GetHashKey(&frame->varTable,entry);
			copy = Tcl_CreateHashEntry(&trash,vname,&newEntry);
			Tcl_SetHashValue(copy, (ClientData)var);
			Tcl_DeleteHashEntry(entry);
		}
		entry = Tcl_NextHashEntry(&place);
	}
	TclDeleteVars(iPtr, &trash);
	Tcl_DeleteHashTable(&frame->varTable);
}

/*
 * ========================================================================
 *  VARIABLE-HANDLING PROCEDURES
 * ========================================================================
 *  The following procedures are copied directly from tclVar.c in the
 *  standard TCL distribution, and must be updated whenever TCL is
 *  updated.  These procedures must be coped here since they are static
 *  (private) in the original distribution and cannot be accessed in
 *  any other way.  It seems to me that a clean interface to low-level
 *  Tcl routines would be a useful thing.
 * ========================================================================
 */

/* 
 * tclVar.c --
 *
 *	This file contains routines that implement Tcl variables
 *	(both scalars and arrays).
 *
 *	The implementation of arrays is modelled after an initial
 *	implementation by Karl Lehenbauer, Mark Diekhans and
 *	Peter da Silva.
 *
 * Copyright 1987-1991 Regents of the University of California
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

/*
 *----------------------------------------------------------------------
 *
 * NewVar --
 *
 *	Create a new variable with a given amount of storage
 *	space.
 *
 * Results:
 *	The return value is a pointer to the new variable structure.
 *	The variable will not be part of any hash table yet.  Its
 *	initial value is empty.
 *
 * Side effects:
 *	Storage gets allocated.
 *
 *----------------------------------------------------------------------
 */

static Var *
NewVar()
{
    register Var *varPtr;

    varPtr = (Var *) ckalloc(sizeof(Var));
    varPtr->valueLength = 0;
    varPtr->valueSpace = 0;
    varPtr->value.string = NULL;
    varPtr->hPtr = NULL;
    varPtr->refCount = 0;
    varPtr->tracePtr = NULL;
    varPtr->searchPtr = NULL;
    varPtr->flags = VAR_UNDEFINED;
    return varPtr;
}

/*
 *----------------------------------------------------------------------
 *
 * CleanupVar --
 *
 *	This procedure is called when it looks like it may be OK
 *	to free up the variable's record and hash table entry, and
 *	those of its containing parent.  It's called, for example,
 *	when a trace on a variable deletes the variable.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	If the variable (or its containing array) really is dead then
 *	its record, and possibly its hash table entry, gets freed up.
 *
 *----------------------------------------------------------------------
 */

static void
CleanupVar(varPtr, arrayPtr)
    Var *varPtr;		/* Pointer to variable that may be a
				 * candidate for being expunged. */
    Var *arrayPtr;		/* Array that contains the variable, or
				 * NULL if this variable isn't an array
				 * element. */
{
    if ((varPtr->flags & VAR_UNDEFINED) && (varPtr->refCount == 0)
	    && (varPtr->tracePtr == NULL)) {
	if (varPtr->hPtr != NULL) {
	    Tcl_DeleteHashEntry(varPtr->hPtr);
	}
	ckfree((char *) varPtr);
    }
    if (arrayPtr != NULL) {
	if ((arrayPtr->flags & VAR_UNDEFINED) && (arrayPtr->refCount == 0)
		&& (arrayPtr->tracePtr == NULL)) {
	    if (arrayPtr->hPtr != NULL) {
		Tcl_DeleteHashEntry(arrayPtr->hPtr);
	    }
	    ckfree((char *) arrayPtr);
	}
    }
    return;
}
