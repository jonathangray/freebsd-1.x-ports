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
 *     RCS:  itcl_objects.c,v 1.1.1.1 1994/03/21 22:09:48 mmc Exp
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
#include "itcl_objects.h"
#include "itcl_core.h"

#ifndef lint
static char sccsid[] = "@(#)itcl_objects.c	1.7 (10/14/93) Michael J. McLennan";
#endif


/*
 * ------------------------------------------------------------------------
 *  Itcl_CreateObject()
 *
 *  Creates a new object instance belonging to the given class.
 *  Automatically creates and intializes public/protected variables,
 *  including the built-in protected "this" variable containing the
 *  object name.  Returns a pointer to the new object data.
 * ------------------------------------------------------------------------
 */
Itcl_Object*
Itcl_CreateObject(name,cdefn)
	char* name;         /* name of new object */
	Itcl_Class *cdefn;  /* class for new object */
{
	Itcl_Class *cd;
	Itcl_Object *odefn;
	Itcl_Variable *vdefn, thisvar;
	Itcl_VarScope *vscope, *vs;
	Itcl_HierIter hier;
	Tcl_HashEntry *entry;
	int newEntry;

	odefn = (Itcl_Object*)ckalloc(sizeof(Itcl_Object));
	odefn->name = (char*)ckalloc((unsigned)(strlen(name)+1));
	strcpy(odefn->name, name);

	odefn->cdefn = cdefn;  Itcl_PreserveData((ClientData)cdefn);
	odefn->interp = cdefn->info->main;
    Tcl_InitHashTable(&odefn->data,TCL_STRING_KEYS);

	/*
	 *  Add new object to the list of known objects.
	 */
	entry = Tcl_CreateHashEntry(&cdefn->info->objects, name, &newEntry);
	Tcl_SetHashValue(entry, (ClientData)odefn);

	/*
	 *  Scan through all public/protected variable definitions
	 *  and create the actual variables that maintain the object
	 *  state.
	 */
	Itcl_InitHierIter(&hier,cdefn);
	while ((cd=Itcl_AdvanceHierIter(&hier)) != NULL)
	{
		for (vdefn=cd->publics; vdefn; vdefn=vdefn->next)
			Itcl_CreateTclVar(vdefn, &odefn->data);

		for (vdefn=cd->protecteds; vdefn && vdefn->next; vdefn=vdefn->next)
			Itcl_CreateTclVar(vdefn, &odefn->data);

		/*
		 *  The last protected variable is always the built-in
		 *  "this" variable.  Treat this definition specially.
		 *  Substitute the actual object name into the definition.
		 */
		thisvar = *vdefn;
		thisvar.init = odefn->name;
		Itcl_CreateTclVar(&thisvar, &odefn->data);
	}
	Itcl_DeleteHierIter(&hier);

	/*
	 *  Create a variable scope for each class in the hierarchy.
	 *  Make a linked list of all scopes, with the most specific
	 *  class at the start of the list.
	 */
	odefn->vscopes = NULL;

	Itcl_InitHierIter(&hier,cdefn);
	while ((cd=Itcl_AdvanceHierIter(&hier)) != NULL)
	{
		vscope = Itcl_CreateVarScope(odefn,cd);
		if (odefn->vscopes)
		{
			for (vs=odefn->vscopes; vs->next; vs=vs->next)
				;
			vs->next = vscope;  /* add to end of list */
		}
		else
			odefn->vscopes = vscope;
	}
	Itcl_DeleteHierIter(&hier);

	return odefn;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_DeleteObject()
 *
 *  Destroys the given object instance, freeing all data associated
 *  with it.  Removes the object name from the list of all known
 *  [incr Tcl] objects in the associated interpreter.
 * ------------------------------------------------------------------------
 */
void
Itcl_DeleteObject(cdata)
	ClientData cdata;  /* object instance data */
{
	Itcl_VarScope *vs, *vsnext;
	Itcl_Object *odefn = (Itcl_Object*)cdata;
	Interp *iPtr = (Interp*)odefn->interp;

	Itcl_ClassInfo *info = odefn->cdefn->info;
	Tcl_HashEntry *entry = Tcl_FindHashEntry(&info->objects, odefn->name);
	if (entry) Tcl_DeleteHashEntry(entry);

	vs = odefn->vscopes;
	while (vs)
	{
		vsnext = vs->next;
		Itcl_DeleteVarScope(vs);
		vs = vsnext;
	}
	Itcl_DeleteTclVars(iPtr, &odefn->data);
	Itcl_ReleaseData((ClientData)odefn->cdefn);

	ckfree(odefn->name);
	ckfree((char*)odefn);
}
