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
 *     RCS:  itcl_class.c,v 1.2 1994/03/25 19:01:08 mmc Exp
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
#include "itcl_class.h"
#include "itcl_core.h"

static int Itcl_InheritCmd _ANSI_ARGS_((ClientData cdata,
	Tcl_Interp *interp, int argc, char **argv));
static int Itcl_ConstructorCmd _ANSI_ARGS_((ClientData cdata,
	Tcl_Interp *interp, int argc, char **argv));
static int Itcl_DestructorCmd _ANSI_ARGS_((ClientData cdata,
	Tcl_Interp *interp, int argc, char **argv));
static int Itcl_MethodCmd _ANSI_ARGS_((ClientData cdata,
	Tcl_Interp *interp, int argc, char **argv));
static int Itcl_ProcCmd _ANSI_ARGS_((ClientData cdata,
	Tcl_Interp *interp, int argc, char **argv));
static int Itcl_PublicCmd _ANSI_ARGS_((ClientData cdata,
	Tcl_Interp *interp, int argc, char **argv));
static int Itcl_ProtectedCmd _ANSI_ARGS_((ClientData cdata,
	Tcl_Interp *interp, int argc, char **argv));
static int Itcl_CommonCmd _ANSI_ARGS_((ClientData cdata,
	Tcl_Interp *interp, int argc, char **argv));

static int Itcl_BiIsa _ANSI_ARGS_((ClientData cdata,
	Tcl_Interp *interp, int argc, char **argv));
static int Itcl_BiDelete _ANSI_ARGS_((ClientData cdata,
	Tcl_Interp *interp, int argc, char **argv));
static int Itcl_BiInfo _ANSI_ARGS_((ClientData cdata,
	Tcl_Interp *interp, int argc, char **argv));

static int Itcl_GetClass _ANSI_ARGS_((Itcl_Class *cdefn,
	Tcl_Interp *interp, int argc, char **argv));
static int Itcl_GetInherit _ANSI_ARGS_((Itcl_Class *cdefn,
	Tcl_Interp *interp, int argc, char **argv));
static int Itcl_GetHeritage _ANSI_ARGS_((Itcl_Class *cdefn,
	Tcl_Interp *interp, int argc, char **argv));
static int Itcl_GetConstructor _ANSI_ARGS_((Itcl_Class *cdefn,
	Tcl_Interp *interp, int argc, char **argv));
static int Itcl_GetDestructor _ANSI_ARGS_((Itcl_Class *cdefn,
	Tcl_Interp *interp, int argc, char **argv));
static int Itcl_GetMethods _ANSI_ARGS_((Itcl_Class *cdefn,
	Tcl_Interp *interp, int argc, char **argv));
static int Itcl_GetProcs _ANSI_ARGS_((Itcl_Class *cdefn,
	Tcl_Interp *interp, int argc, char **argv));
static int Itcl_GetBody _ANSI_ARGS_((Itcl_Class *cdefn,
	Tcl_Interp *interp, int argc, char **argv));
static int Itcl_GetPublics _ANSI_ARGS_((Itcl_Class *cdefn,
	Tcl_Interp *interp, int argc, char **argv));
static int Itcl_GetProtecteds _ANSI_ARGS_((Itcl_Class *cdefn,
	Tcl_Interp *interp, int argc, char **argv));
static int Itcl_GetCommons _ANSI_ARGS_((Itcl_Class *cdefn,
	Tcl_Interp *interp, int argc, char **argv));

#ifndef lint
static char sccsid[] = "@(#)itcl_class.c	1.26 (10/14/93) Michael J. McLennan";
#endif


/*
 * ------------------------------------------------------------------------
 *  Itcl_CreateClass()
 *
 *  Creates a new class definition in an empty state.  Automatically
 *  adds the following built-in methods:
 *
 *      isa <class>
 *      delete
 *
 *  and the following class-level procs:
 *
 *      info <option> ?<args>...?
 *
 *  Returns a pointer to the new class definition.
 * ------------------------------------------------------------------------
 */
Itcl_Class*
Itcl_CreateClass(name,info)
	char* name;            /* name of new class */
	Itcl_ClassInfo *info;  /* list of classes containing this class */
{
	Itcl_Class *cdefn;
	Itcl_Variable *vdefn;

	cdefn = (Itcl_Class*)ckalloc(sizeof(Itcl_Class));

	cdefn->name = (char*)ckalloc((unsigned)(strlen(name)+1));
	strcpy(cdefn->name, name);

	/*
	 *  As long as this class definition exists, keep core info around.
	 */
	cdefn->info = info;  Itcl_PreserveData((ClientData)info);

	Tcl_InitHashTable(&cdefn->methods,TCL_STRING_KEYS);
	Tcl_InitHashTable(&cdefn->allMethods,TCL_STRING_KEYS);
	Tcl_InitHashTable(&cdefn->procs,TCL_STRING_KEYS);
	Tcl_InitHashTable(&cdefn->allProcs,TCL_STRING_KEYS);
	Tcl_InitHashTable(&cdefn->data,TCL_STRING_KEYS);

	cdefn->interp     = Tcl_CreateInterp();
	cdefn->supers     = NULL;
	cdefn->publics    = NULL;
	cdefn->protecteds = NULL;
	cdefn->commons    = NULL;
	cdefn->unique     = 0;
	cdefn->vscope     = Itcl_CreateVarScope((Itcl_Object*)NULL,cdefn);

	/*
	 *  Add definition of protected "this" variable.
	 */
	vdefn = Itcl_CreateVariable(cdefn,"this","<objectName>",(char*)NULL);
	Itcl_AddVariable(&cdefn->protecteds, vdefn);

	/*
	 *  Add commands for parsing class definitions.
	 */
	Tcl_CreateCommand(cdefn->interp, "inherit", Itcl_InheritCmd,
		(ClientData)info, (Tcl_CmdDeleteProc*)NULL);

	Tcl_CreateCommand(cdefn->interp, "constructor", Itcl_ConstructorCmd,
		(ClientData)info, (Tcl_CmdDeleteProc*)NULL);

	Tcl_CreateCommand(cdefn->interp, "destructor", Itcl_DestructorCmd,
		(ClientData)info, (Tcl_CmdDeleteProc*)NULL);

	Tcl_CreateCommand(cdefn->interp, "method", Itcl_MethodCmd,
		(ClientData)info, (Tcl_CmdDeleteProc*)NULL);

	Tcl_CreateCommand(cdefn->interp, "proc", Itcl_ProcCmd,
		(ClientData)info, (Tcl_CmdDeleteProc*)NULL);

	Tcl_CreateCommand(cdefn->interp, "public", Itcl_PublicCmd,
		(ClientData)info, (Tcl_CmdDeleteProc*)NULL);

	Tcl_CreateCommand(cdefn->interp, "protected", Itcl_ProtectedCmd,
		(ClientData)info, (Tcl_CmdDeleteProc*)NULL);

	Tcl_CreateCommand(cdefn->interp, "common", Itcl_CommonCmd,
		(ClientData)info, (Tcl_CmdDeleteProc*)NULL);

	/*
	 *  Remove unwanted commands from class interpreter.  These
	 *  commands manipulate common resources, and are better handled
	 *  at the global scope:
	 *
	 *    upvar - main interpreter has access to all call frames,
	 *            and to the only valid list of global variables
	 *
	 *     open -
	 *    close - fileID's should be valid across all interps
	 *      etc -
	 *
	 *  history - command line facility, not needed in class interps
	 *
	 */
	Tcl_DeleteCommand(cdefn->interp, "upvar");

	Tcl_DeleteCommand(cdefn->interp, "open");
	Tcl_DeleteCommand(cdefn->interp, "close");
	Tcl_DeleteCommand(cdefn->interp, "gets");
	Tcl_DeleteCommand(cdefn->interp, "puts");
	Tcl_DeleteCommand(cdefn->interp, "read");
	Tcl_DeleteCommand(cdefn->interp, "eof");
	Tcl_DeleteCommand(cdefn->interp, "flush");
	Tcl_DeleteCommand(cdefn->interp, "seek");
	Tcl_DeleteCommand(cdefn->interp, "tell");

	Tcl_DeleteCommand(cdefn->interp, "history");

	/*
	 *  Replace "uplevel" command so that it is aware of interpreters
	 *  used for [incr Tcl] classes.
	 */
	Tcl_DeleteCommand(cdefn->interp, "uplevel");
	Tcl_CreateCommand(cdefn->interp, "uplevel", Itcl_UplevelCmd,
		Itcl_PreserveData((ClientData)info), Itcl_ReleaseData);

	/*
	 *  Add "previous" command to access commands in the scope of
	 *  the most immediate base class in the class hierarchy.
	 */
	Tcl_CreateCommand(cdefn->interp, "previous", Itcl_PreviousCmd,
		(ClientData)cdefn, (Tcl_CmdDeleteProc*)NULL);

	/*
	 *  Add "virtual" command to access commands in the scope of
	 *  the most specific class for an object.
	 */
	Tcl_CreateCommand(cdefn->interp, "virtual", Itcl_VirtualCmd,
		(ClientData)cdefn, (Tcl_CmdDeleteProc*)NULL);

	/*
	 *  Add "global" command to access global variables in main
	 *  interpreter from within class scope interpreter.
	 */
	Tcl_CreateCommand(cdefn->interp, "global", Itcl_GlobalCmd,
		(ClientData)cdefn, (Tcl_CmdDeleteProc*)NULL);

	/*
	 *  Add "unknown" command to class interpreter to look for
	 *  methods in superclasses and outside interpreter.
	 */
	Tcl_CreateCommand(cdefn->interp, "unknown", Itcl_UnknownCmd,
		(ClientData)cdefn, (Tcl_CmdDeleteProc*)NULL);

	/*
	 *  Move the usual "info" command to "tcl_info" so that it can still
	 *  be accessed as a fall-back within Itcl_BiInfo().
	 */
	(void)Tcl_Eval(cdefn->interp, "rename info tcl_info");

	/*
	 *  Add built-in methods and procs...
	 */
	Itcl_AddMethod(cdefn, Itcl_CreateBiMethod(cdefn,"isa",Itcl_BiIsa));
	Itcl_AddMethod(cdefn, Itcl_CreateBiMethod(cdefn,"delete",Itcl_BiDelete));
	Itcl_AddProc(cdefn,   Itcl_CreateBiMethod(cdefn,"info",Itcl_BiInfo));

	return cdefn;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_DeleteClass()
 *
 *  Destroys the given class definition, freeing all resources
 *  associated with it.
 * ------------------------------------------------------------------------
 */
void
Itcl_DeleteClass(cdata)
	ClientData cdata;  /* class definition to be destroyed */
{
	Itcl_Class *cdefn = (Itcl_Class*)cdata;
	Interp *iPtr = (Interp*)cdefn->interp;

	Tcl_HashSearch place;
	Tcl_HashEntry *entry;
	Itcl_Method *mdefn;
	Itcl_Class **slist;

	/*
	 *  Remove this class from the list of known classes, and
	 *  release claim on core info.
	 */
	entry = Tcl_FindHashEntry(&cdefn->info->classes, cdefn->name);
	if (entry) Tcl_DeleteHashEntry(entry);

	Itcl_ReleaseData((ClientData)cdefn->info);
	ckfree(cdefn->name);

	/*
	 *  Destroy all methods...
	 */
	entry = Tcl_FirstHashEntry(&cdefn->methods,&place);
	while (entry)
	{
		mdefn = (Itcl_Method*)Tcl_GetHashValue(entry);
		Itcl_DeleteMethod(mdefn);
		entry = Tcl_NextHashEntry(&place);
	}

	entry = Tcl_FirstHashEntry(&cdefn->procs,&place);
	while (entry)
	{
		mdefn = (Itcl_Method*)Tcl_GetHashValue(entry);
		Itcl_DeleteMethod(mdefn);
		entry = Tcl_NextHashEntry(&place);
	}

	Tcl_DeleteHashTable(&cdefn->methods);
	Tcl_DeleteHashTable(&cdefn->allMethods);
	Tcl_DeleteHashTable(&cdefn->procs);
	Tcl_DeleteHashTable(&cdefn->allProcs);

	/*
	 *  Destroy common data and variable definitions...
	 */
	Itcl_DeleteVarScope(cdefn->vscope);
	Itcl_DeleteTclVars(iPtr, &cdefn->data);

	while (cdefn->publics)
		Itcl_DeleteVariable(&cdefn->publics);

	while (cdefn->protecteds)
		Itcl_DeleteVariable(&cdefn->protecteds);

	while (cdefn->commons)
		Itcl_DeleteVariable(&cdefn->commons);

	Tcl_DeleteInterp(cdefn->interp);

	/*
	 *  Release the claim on all superclasses.
	 */
	if (cdefn->supers)
	{
		for (slist=cdefn->supers; *slist; slist++)
			Itcl_ReleaseData((ClientData)*slist);
		ckfree((char*)cdefn->supers);
	}
	ckfree((char*)cdefn);
}


/*
 * ------------------------------------------------------------------------
 *  Itcl_AddMethod()
 *
 *  Attaches a method to a list of methods associated with a class.
 * ------------------------------------------------------------------------
 */
void
Itcl_AddMethod(cdefn,mdefn)
	Itcl_Class *cdefn;     /* class definition to be updated */
	Itcl_Method *mdefn;    /* method definition to be added */
{
	int newEntry;
	Tcl_HashEntry *mEntry;
	Itcl_Method *oldmdefn;

	if (mdefn)
	{
		mEntry = Tcl_CreateHashEntry(&cdefn->methods, mdefn->name, &newEntry);
		if (!newEntry)
		{
			oldmdefn = (Itcl_Method*)Tcl_GetHashValue(mEntry);
			Itcl_DeleteMethod(oldmdefn);
		}
		Tcl_SetHashValue(mEntry, (ClientData)mdefn);
	}
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_AddProc()
 *
 *  Attaches a proc to a list of proc associated with a class.
 * ------------------------------------------------------------------------
 */
void
Itcl_AddProc(cdefn,mdefn)
	Itcl_Class *cdefn;     /* class definition to be updated */
	Itcl_Method *mdefn;    /* proc definition to be added */
{
	int newEntry;
	Tcl_HashEntry *mEntry;
	Itcl_Method *oldmdefn;

	if (mdefn)
	{
		mEntry = Tcl_CreateHashEntry(&cdefn->procs, mdefn->name, &newEntry);
		if (!newEntry)
		{
			oldmdefn = (Itcl_Method*)Tcl_GetHashValue(mEntry);
			Itcl_DeleteMethod(oldmdefn);
		}
		Tcl_SetHashValue(mEntry, (ClientData)mdefn);
	}
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_FindMethod()
 *
 *  Searches for a method with the given name in a class definition.
 *  Only the list of object-specific "methods" is consulted.
 *  If the optional class name is specified, then it is treated as a
 *  superclass name, and the method is sought only in this class
 *  definition.  Returns a pointer to the method definition if found,
 *  and NULL otherwise.
 * ------------------------------------------------------------------------
 */
Itcl_Method*
Itcl_FindMethod(cname,mname,cdefn)
	char *cname;          /* name of class (or "") */
	char *mname;          /* name of method */
	Itcl_Class *cdefn;    /* class definition to be searched */
{
	Itcl_Method *rval = NULL;  /* assume method will not be found */

	Tcl_HashEntry *mEntry;
	Itcl_HierIter hier;

	Itcl_InitHierIter(&hier,cdefn);
	if (cname && (*cname != '\0'))
	{
		while ((cdefn=Itcl_AdvanceHierIter(&hier)) != NULL)
			if (strcmp(cname,cdefn->name) == 0)
				break;

		if (cdefn)
		{
			mEntry = Tcl_FindHashEntry(&cdefn->methods,mname);
			if (mEntry)
				rval = (Itcl_Method*)Tcl_GetHashValue(mEntry);
		}
	}
	else
	{
		while ((cdefn=Itcl_AdvanceHierIter(&hier)) != NULL)
		{
			mEntry = Tcl_FindHashEntry(&cdefn->methods,mname);
			if (mEntry)
			{
				rval = (Itcl_Method*)Tcl_GetHashValue(mEntry);
				break;
			}
		}
	}
	Itcl_DeleteHierIter(&hier);

	return rval;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_FindProc()
 *
 *  Searches for a proc with the given name in a class definition.
 *  Only the list of class-specific "procs" is consulted.
 *  If the optional class name is specified, then it is treated as a
 *  superclass name, and the method is sought only in this class
 *  definition.  Returns a pointer to the method definition if found,
 *  and NULL otherwise.
 * ------------------------------------------------------------------------
 */
Itcl_Method*
Itcl_FindProc(cname,mname,cdefn)
	char *cname;          /* name of class (or "") */
	char *mname;          /* name of method */
	Itcl_Class *cdefn;    /* class definition to be searched */
{
	Itcl_Method *rval = NULL;  /* assume method will not be found */

	Tcl_HashEntry *mEntry;
	Itcl_HierIter hier;

	Itcl_InitHierIter(&hier,cdefn);
	if (cname && (*cname != '\0'))
	{
		while ((cdefn=Itcl_AdvanceHierIter(&hier)) != NULL)
			if (strcmp(cname,cdefn->name) == 0)
				break;

		if (cdefn)
		{
			mEntry = Tcl_FindHashEntry(&cdefn->procs,mname);
			if (mEntry)
				rval = (Itcl_Method*)Tcl_GetHashValue(mEntry);
		}
	}
	else
	{
		while ((cdefn=Itcl_AdvanceHierIter(&hier)) != NULL)
		{
			mEntry = Tcl_FindHashEntry(&cdefn->procs,mname);
			if (mEntry)
			{
				rval = (Itcl_Method*)Tcl_GetHashValue(mEntry);
				break;
			}
		}
	}
	Itcl_DeleteHierIter(&hier);

	return rval;
}


/*
 * ------------------------------------------------------------------------
 *  Itcl_AddVariable()
 *
 *  Adds a variable definition to a given list of variables in a class.
 * ------------------------------------------------------------------------
 */
void
Itcl_AddVariable(vlist,vdefn)
	Itcl_Variable **vlist;   /* variable list to be updated */
	Itcl_Variable *vdefn;    /* variable definition to be added */
{
	if (vdefn)
	{
		vdefn->next = *vlist;
		*vlist = vdefn;
	}
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_FindPublicVar()
 *
 *  Searches for a public variable with the given name.  If the optional
 *  class name is specified, then it is treated as a superclass name,
 *  and the variable is sought in this class definition.  Returns a
 *  pointer to the variable definition if found, and NULL otherwise.
 * ------------------------------------------------------------------------
 */
Itcl_Variable*
Itcl_FindPublicVar(cname,vname,cdefn)
	char *cname;        /* name of class (or "") */
	char *vname;        /* name of variable */
	Itcl_Class *cdefn;  /* class definition */
{
	Itcl_Variable *rval = NULL;

	Itcl_Variable *vlist;
	Itcl_HierIter hier;

	Itcl_InitHierIter(&hier,cdefn);
	if (cname && (*cname != '\0'))
	{
		while ((cdefn=Itcl_AdvanceHierIter(&hier)) != NULL)
			if (strcmp(cname,cdefn->name) == 0)
				break;

		if (cdefn)
		{
			for (vlist=cdefn->publics; vlist; vlist=vlist->next)
				if ((*vlist->name == *vname) &&
				    (strcmp(vlist->name,vname) == 0))
				{
					rval = vlist;
					break;
				}
		}
	}
	else
	{
		while ((cdefn=Itcl_AdvanceHierIter(&hier)) != NULL)
		{
			for (vlist=cdefn->publics; vlist; vlist=vlist->next)
				if ((*vlist->name == *vname) &&
				    (strcmp(vlist->name,vname) == 0))
				{
					rval = vlist;
					break;
				}
			if (rval) break;
		}
	}
	Itcl_DeleteHierIter(&hier);

	return rval;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_FindProtectedVar()
 *
 *  Searches for a protected variable with the given name.  If the optional
 *  class name is specified, then it is treated as a superclass name,
 *  and the variable is sought in this class definition.  Returns a
 *  pointer to the variable definition if found, and NULL otherwise.
 * ------------------------------------------------------------------------
 */
Itcl_Variable*
Itcl_FindProtectedVar(cname,vname,cdefn)
	char *cname;        /* name of class (or "") */
	char *vname;        /* name of variable */
	Itcl_Class *cdefn;  /* class definition */
{
	Itcl_Variable *rval = NULL;

	Itcl_Variable *vlist;
	Itcl_HierIter hier;

	Itcl_InitHierIter(&hier,cdefn);
	if (cname && (*cname != '\0'))
	{
		while ((cdefn=Itcl_AdvanceHierIter(&hier)) != NULL)
			if (strcmp(cname,cdefn->name) == 0)
				break;

		if (cdefn)
		{
			for (vlist=cdefn->protecteds; vlist; vlist=vlist->next)
				if ((*vlist->name == *vname) &&
				    (strcmp(vlist->name,vname) == 0))
				{
					rval = vlist;
					break;
				}
		}
	}
	else
	{
		while ((cdefn=Itcl_AdvanceHierIter(&hier)) != NULL)
		{
			for (vlist=cdefn->protecteds; vlist; vlist=vlist->next)
				if ((*vlist->name == *vname) &&
				    (strcmp(vlist->name,vname) == 0))
				{
					rval = vlist;
					break;
				}
			if (rval) break;
		}
	}
	Itcl_DeleteHierIter(&hier);

	return rval;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_FindCommonVar()
 *
 *  Searches for a common variable with the given name.  If the optional
 *  class name is specified, then it is treated as a superclass name,
 *  and the variable is sought in this class definition.  Returns a
 *  pointer to the variable definition if found, and NULL otherwise.
 * ------------------------------------------------------------------------
 */
Itcl_Variable*
Itcl_FindCommonVar(cname,vname,cdefn)
	char *cname;        /* name of class (or "") */
	char *vname;        /* name of variable */
	Itcl_Class *cdefn;  /* class definition */
{
	Itcl_Variable *rval = NULL;

	Itcl_Variable *vlist;
	Itcl_HierIter hier;

	Itcl_InitHierIter(&hier,cdefn);
	if (cname && (*cname != '\0'))
	{
		while ((cdefn=Itcl_AdvanceHierIter(&hier)) != NULL)
			if (strcmp(cname,cdefn->name) == 0)
				break;


		if (cdefn)
		{
			for (vlist=cdefn->commons; vlist; vlist=vlist->next)
				if ((*vlist->name == *vname) &&
				    (strcmp(vlist->name,vname) == 0))
				{
					rval = vlist;
					break;
				}
		}
	}
	else
	{
		while ((cdefn=Itcl_AdvanceHierIter(&hier)) != NULL)
		{
			for (vlist=cdefn->commons; vlist; vlist=vlist->next)
				if ((*vlist->name == *vname) &&
				    (strcmp(vlist->name,vname) == 0))
				{
					rval = vlist;
					break;
				}
			if (rval) break;
		}
	}
	Itcl_DeleteHierIter(&hier);

	return rval;
}


/*
 * ------------------------------------------------------------------------
 *  Itcl_AddMethodCmd()
 *
 *  Adds the command to handle a method with the given name (short name
 *  or full name) to the interpreter associated with a class.  Usually
 *  invoked just after the class has been defined and member functions
 *  are being scrutinized for inheritance.
 * ------------------------------------------------------------------------
 */
void
Itcl_AddMethodCmd(scope,name,mdefn)
	Itcl_Class *scope;    /* class definition representing scope */
	char *name;           /* real name for method (short name or full name) */
	Itcl_Method *mdefn;   /* method definition */
{
	Itcl_Method *realmd;

	/*
	 *  If this is an inherited method, then find the real
	 *  method definition.
	 */
	realmd = mdefn;
	while (realmd->type == ITCL_INHERITED_METHOD)
		realmd = realmd->values.actual;

	if (realmd->type == ITCL_BUILTIN_METHOD)
		Tcl_CreateCommand(scope->interp, name, realmd->values.builtin,
			(ClientData)mdefn->cdefn, (Tcl_CmdDeleteProc*)NULL);
	else
		Tcl_CreateCommand(scope->interp, name,
			Itcl_ExecMethod, (ClientData)mdefn,
			(Tcl_CmdDeleteProc*)NULL);
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_AddProcCmd()
 *
 *  Adds the command to handle a proc with the given name (short name
 *  or full name) to the interpreter associated with a class.  Usually
 *  invoked just after the class has been defined and member functions
 *  are being scrutinized for inheritance.
 * ------------------------------------------------------------------------
 */
void
Itcl_AddProcCmd(scope,name,mdefn)
	Itcl_Class *scope;    /* class definition representing scope */
	char *name;           /* real name for proc (short name or full name) */
	Itcl_Method *mdefn;   /* proc definition */
{
	Itcl_Method *realmd;

	/*
	 *  If this is an inherited proc, then find the real
	 *  proc definition.
	 */
	realmd = mdefn;
	while (realmd->type == ITCL_INHERITED_METHOD)
		realmd = realmd->values.actual;

	if (realmd->type == ITCL_BUILTIN_METHOD)
		Tcl_CreateCommand(scope->interp, name, realmd->values.builtin,
			(ClientData)mdefn->cdefn, (Tcl_CmdDeleteProc*)NULL);
	else
		Tcl_CreateCommand(scope->interp, name,
			Itcl_ExecProc, (ClientData)mdefn,
			(Tcl_CmdDeleteProc*)NULL);
}


/*
 * ------------------------------------------------------------------------
 *  Itcl_InitHierIter()
 *
 *  Initializes an iterator for traversing the hierarchy of the given
 *  class.  Subsequent calls to Itcl_AdvanceHierIter() will return
 *  the superclasses in order from most-to-least specific.
 * ------------------------------------------------------------------------
 */
void
Itcl_InitHierIter(iter,cdefn)
	Itcl_HierIter *iter;  /* iterator used for traversal */
	Itcl_Class *cdefn;    /* class definition for start of traversal */
{
	Itcl_InitStack(&iter->stack);
	Itcl_PushStack((ClientData)cdefn, &iter->stack);
	iter->current = cdefn;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_DeleteHierIter()
 *
 *  Destroys an iterator for traversing class hierarchies, freeing
 *  all memory associated with it.
 * ------------------------------------------------------------------------
 */
void
Itcl_DeleteHierIter(iter)
	Itcl_HierIter *iter;  /* iterator used for traversal */
{
	Itcl_DeleteStack(&iter->stack);
	iter->current = NULL;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_AdvanceHierIter()
 *
 *  Moves a class hierarchy iterator forward to the next superclass.
 *  Returns a pointer to the current class definition, or NULL when
 *  the end of the hierarchy has been reached.
 * ------------------------------------------------------------------------
 */
Itcl_Class*
Itcl_AdvanceHierIter(iter)
	Itcl_HierIter *iter;  /* iterator used for traversal */
{
	register Itcl_Class **slist;

	iter->current = (Itcl_Class*)Itcl_PopStack(&iter->stack);

	if (iter->current && iter->current->supers)
		for (slist=iter->current->supers; *slist; slist++)
			Itcl_PushStack((ClientData)*slist, &iter->stack);

	return iter->current;
}


/*
 * ========================================================================
 *  CLASS DEFINITION PARSING COMMANDS
 * ========================================================================
 */

/*
 * ------------------------------------------------------------------------
 *  Itcl_InheritCmd()
 *
 *  Invoked by TCL during the parsing of a class definition whenever
 *  the "inherit" command is invoked to define one or more superclasses.
 *  Handles the following syntax:
 *
 *      inherit <superclass> ?<superclass>...?
 *
 * ------------------------------------------------------------------------
 */
static int
Itcl_InheritCmd(clientData, interp, argc, argv)
	ClientData clientData;   /* info for all known classes */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	int status = TCL_OK;

	Itcl_ClassInfo *info = (Itcl_ClassInfo*)clientData;
	Itcl_Class *cdefn = (Itcl_Class*)Itcl_PeekStack(&info->cdefnStack);

	int i, j;
	Itcl_Class **slist, *cd, *badcd;
	Tcl_HashEntry *entry;
	Itcl_HierIter hier;
	Itcl_Stack classes, stack;

	Tcl_SetResult(interp, "", TCL_STATIC);
	if (argc < 2)
	{
		Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0], " class ?class...?\"",
			(char*)NULL);
		return TCL_ERROR;
	}
	if (cdefn->supers)
	{
		Tcl_AppendResult(interp, "inheritance \"", (char*)NULL);
		for (slist=cdefn->supers; *slist; slist++)
			Tcl_AppendResult(interp, (*slist)->name, " ", (char*)NULL);

		Tcl_AppendResult(interp, "\" already defined for class \"",
			cdefn->name, "\"", (char*)NULL);
		return TCL_ERROR;
	}

	/*
	 *  Allocate space for the list of superclasses, including
	 *  space for a null terminator.  Validate each superclass
	 *  and add it to the list.  Add superclasses in reverse
	 *  order, since order will be reversed again using a stack
	 *  to traverse the hierarchy.
	 */
	cdefn->supers = (Itcl_Class**)ckalloc(
		(unsigned)(argc*sizeof(Itcl_Class*))
	);

	slist = cdefn->supers + argc-1;  /* add from end of list backwards */
	*slist-- = NULL;                 /* null terminated list */
	for (argc--,argv++; argc > 0; argc--,argv++)
	{
		/*
		 *  Invoke the class name as a command, giving the
		 *  autoloader a chance to load the class definition.
		 */
		(void)Tcl_Eval(cdefn->info->main, *argv);

		/*
		 *  Make sure that the superclass name is known.
		 */
		entry = Tcl_FindHashEntry(&info->classes, *argv);
		if (!entry)
		{
			Tcl_AppendResult(interp, "cannot inherit from unknown class \"",
				*argv, "\"", (char*)NULL);

			for (slist++; *slist; slist++)
				Itcl_ReleaseData((ClientData)*slist);
			cdefn->supers[0] = NULL;

			return TCL_ERROR;
		}
		*slist-- = (Itcl_Class*)Itcl_PreserveData(Tcl_GetHashValue(entry));
	}

	/*
	 *  Scan through the entire class hierarchy and make sure that
	 *  no class appears twice.
	 */
	Itcl_InitStack(&classes);
	Itcl_InitHierIter(&hier,cdefn);

	while ((cd=Itcl_AdvanceHierIter(&hier)) != NULL)
		Itcl_PushStack((ClientData)cd, &classes);

	for (i=0; i < classes.len; i++)
	{
		for (j=i+1; j < classes.len; j++)
			if (classes.values[i] == classes.values[j])
				break;

		if (j < classes.len)
			break;
	}

	/*
	 *  Same base class found twice in the hierarchy?
	 *  Then flag error.  Show the list of multiple paths
	 *  leading to the same base class.
	 */
	if (i < classes.len)
	{
		status = TCL_ERROR;
		badcd = (Itcl_Class*)classes.values[i];
		Tcl_AppendResult(interp, "class \"", cdefn->name,
			"\" inherits base class \"", badcd->name,
			"\" more than once:",
			(char*)NULL);

		cd = cdefn;
		Itcl_InitStack(&stack);
		Itcl_PushStack((ClientData)cd, &stack);

		while (stack.len > 0)  /* find paths leading to bad base class */
		{
			cd = (Itcl_Class*)Itcl_PopStack(&stack);

			if (cd == badcd)
			{
				Tcl_AppendResult(interp, "\n  ", (char*)NULL);
				for (i=0; i < stack.len; i++)
					if (stack.values[i] == NULL)
						Tcl_AppendResult(interp,
							((Itcl_Class*)stack.values[i-1])->name,
							"->",
							(char*)NULL);

				Tcl_AppendResult(interp, badcd->name, (char*)NULL);
			}
			else if (!cd)
				(void)Itcl_PopStack(&stack);

			else if (cd->supers)
			{
				Itcl_PushStack((ClientData)cd, &stack);
				Itcl_PushStack((ClientData)NULL, &stack);
				for (slist=cd->supers; *slist; slist++)
					Itcl_PushStack((ClientData)*slist, &stack);
			}
		}
		Itcl_DeleteStack(&stack);
	}
	Itcl_DeleteStack(&classes);
	Itcl_DeleteHierIter(&hier);

	return status;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_ConstructorCmd()
 *
 *  Invoked by TCL during the parsing of a class definition whenever
 *  the "constructor" command is invoked to define the constructor
 *  for an object.  Handles the following syntax:
 *
 *      constructor {<arglist>} { <body> }
 *
 * ------------------------------------------------------------------------
 */
static int
Itcl_ConstructorCmd(clientData, interp, argc, argv)
	ClientData clientData;   /* info for all known classes */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	Itcl_ClassInfo *info = (Itcl_ClassInfo*)clientData;
	Itcl_Class *cdefn = (Itcl_Class*)Itcl_PeekStack(&info->cdefnStack);

	Tcl_SetResult(interp, "", TCL_STATIC);
	if (argc != 3)
	{
		Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0], " args body\"",
			(char*)NULL);
		return TCL_ERROR;
	}
	if (Itcl_FindMethod(cdefn->name,argv[0],cdefn))
	{
		Tcl_AppendResult(interp, "\"", argv[0],
			"\" already defined in class \"", cdefn->name, "\"",
			(char*)NULL);
		return TCL_ERROR;
	}
	Itcl_AddMethod(cdefn, Itcl_CreateMethod(cdefn,argv[0],argv[1],argv[2]));

	return TCL_OK;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_DestructorCmd()
 *
 *  Invoked by TCL during the parsing of a class definition whenever
 *  the "destructor" command is invoked to define the destructor
 *  for an object.  Handles the following syntax:
 *
 *      destructor { <body> }
 *
 * ------------------------------------------------------------------------
 */
static int
Itcl_DestructorCmd(clientData, interp, argc, argv)
	ClientData clientData;   /* info for all known classes */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	Itcl_ClassInfo *info = (Itcl_ClassInfo*)clientData;
	Itcl_Class *cdefn = (Itcl_Class*)Itcl_PeekStack(&info->cdefnStack);

	Tcl_SetResult(interp, "", TCL_STATIC);
	if (argc != 2)
	{
		Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0], " body\"",
			(char*)NULL);
		return TCL_ERROR;
	}
	if (Itcl_FindMethod(cdefn->name,argv[0],cdefn))
	{
		Tcl_AppendResult(interp, "\"", argv[0],
			"\" already defined in class \"", cdefn->name, "\"",
			(char*)NULL);
		return TCL_ERROR;
	}
	Itcl_AddMethod(cdefn, Itcl_CreateMethod(cdefn,argv[0],(char*)NULL,argv[1]));

	return TCL_OK;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_MethodCmd()
 *
 *  Invoked by TCL during the parsing of a class definition whenever
 *  the "method" command is invoked to define an object method.
 *  Handles the following syntax:
 *
 *      method <name> {<arglist>} { <body> }
 *
 * ------------------------------------------------------------------------
 */
static int
Itcl_MethodCmd(clientData, interp, argc, argv)
	ClientData clientData;   /* info for all known classes */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	Itcl_ClassInfo *info = (Itcl_ClassInfo*)clientData;
	Itcl_Class *cdefn = (Itcl_Class*)Itcl_PeekStack(&info->cdefnStack);

	Tcl_SetResult(interp, "", TCL_STATIC);
	if (argc != 4)
	{
		Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0], " name args body\"",
			(char*)NULL);
		return TCL_ERROR;
	}
	if (Itcl_FindMethod(cdefn->name,argv[1],cdefn) ||
	    Itcl_FindProc(cdefn->name,argv[1],cdefn))
	{
		Tcl_AppendResult(interp, "method \"",
			argv[1], "\" already defined in class \"", cdefn->name, "\"",
			(char*)NULL);
		return TCL_ERROR;
	}
	Itcl_AddMethod(cdefn, Itcl_CreateMethod(cdefn,argv[1],argv[2],argv[3]));

	return TCL_OK;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_ProcCmd()
 *
 *  Installed into the interpreter associated with a class, and invoked
 *  to process the "proc" command.  Almost identical to the usual
 *  Tcl_ProcCmd(), but sets the command handler to Itcl_ExecProc().
 *  This special handler automatically installs "common" variables
 *  into the stack frame before executing the proc body.
 *
 *      proc <name> {<arglist>} { <body> }
 * ------------------------------------------------------------------------
 */
static int
Itcl_ProcCmd(clientData, interp, argc, argv)
	ClientData clientData;   /* class definition */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	Itcl_ClassInfo *info = (Itcl_ClassInfo*)clientData;
	Itcl_Class *cdefn = (Itcl_Class*)Itcl_PeekStack(&info->cdefnStack);

	Tcl_SetResult(interp, "", TCL_STATIC);
	if (argc != 4)
	{
		Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0], " name args body\"",
			(char*)NULL);
		return TCL_ERROR;
	}
	if (Itcl_FindMethod(cdefn->name,argv[1],cdefn) ||
	    Itcl_FindProc(cdefn->name,argv[1],cdefn))
	{
		Tcl_AppendResult(interp, "proc \"",
			argv[1], "\" already defined in class \"", cdefn->name, "\"",
			(char*)NULL);
		return TCL_ERROR;
	}
	Itcl_AddProc(cdefn, Itcl_CreateMethod(cdefn,argv[1],argv[2],argv[3]));

	return TCL_OK;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_PublicCmd()
 *
 *  Invoked by TCL during the parsing of a class definition whenever
 *  the "public" command is invoked to define a public variable.
 *  Handles the following syntax:
 *
 *      public <varname> ?<init>? ?<config>?
 *
 * ------------------------------------------------------------------------
 */
static int
Itcl_PublicCmd(clientData, interp, argc, argv)
	ClientData clientData;   /* info for all known classes */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	Itcl_ClassInfo *info = (Itcl_ClassInfo*)clientData;
	Itcl_Class *cdefn = (Itcl_Class*)Itcl_PeekStack(&info->cdefnStack);
	Itcl_Variable *vdefn;
	char *init, *config;

	Tcl_SetResult(interp, "", TCL_STATIC);
	if ((argc < 2) || (argc > 4))
	{
		Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0], " varname ?init? ?config?\"",
			(char*)NULL);
		return TCL_ERROR;
	}
	if (Itcl_FindPublicVar(cdefn->name,argv[1],cdefn) ||
	    Itcl_FindProtectedVar(cdefn->name,argv[1],cdefn) ||
	    Itcl_FindCommonVar(cdefn->name,argv[1],cdefn))
	{
		Tcl_AppendResult(interp, "variable name \"",
			argv[1], "\" already defined in class \"", cdefn->name, "\"",
			(char*)NULL);
		return TCL_ERROR;
	}
	init   = (argc >= 3) ? argv[2] : NULL;
	config = (argc >= 4) ? argv[3] : NULL;
	vdefn  = Itcl_CreateVariable(cdefn,argv[1],init,config);

	Itcl_AddVariable(&cdefn->publics, vdefn);
	return TCL_OK;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_ProtectedCmd()
 *
 *  Invoked by TCL during the parsing of a class definition whenever
 *  the "protected" command is invoked to define a protected variable.
 *  Handles the following syntax:
 *
 *      protected <varname> ?<init>?
 *
 * ------------------------------------------------------------------------
 */
static int
Itcl_ProtectedCmd(clientData, interp, argc, argv)
	ClientData clientData;   /* info for all known classes */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	Itcl_ClassInfo *info = (Itcl_ClassInfo*)clientData;
	Itcl_Class *cdefn = (Itcl_Class*)Itcl_PeekStack(&info->cdefnStack);
	Itcl_Variable *vdefn;
	char *init;

	Tcl_SetResult(interp, "", TCL_STATIC);
	if ((argc < 2) || (argc > 3))
	{
		Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0], " varname ?init?\"",
			(char*)NULL);
		return TCL_ERROR;
	}
	if (Itcl_FindPublicVar(cdefn->name,argv[1],cdefn) ||
	    Itcl_FindProtectedVar(cdefn->name,argv[1],cdefn) ||
	    Itcl_FindCommonVar(cdefn->name,argv[1],cdefn))
	{
		Tcl_AppendResult(interp, "variable name \"",
			argv[1], "\" already defined in class \"", cdefn->name, "\"",
			(char*)NULL);
		return TCL_ERROR;
	}
	init  = (argc >= 3) ? argv[2] : NULL;
	vdefn = Itcl_CreateVariable(cdefn,argv[1],init,(char*)NULL);

	Itcl_AddVariable(&cdefn->protecteds, vdefn);
	return TCL_OK;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_CommonCmd()
 *
 *  Invoked by TCL during the parsing of a class definition whenever
 *  the "common" command is invoked to define a variable that is
 *  common to all objects in the class.  Handles the following syntax:
 *
 *      common <varname> ?<init>?
 *
 * ------------------------------------------------------------------------
 */
static int
Itcl_CommonCmd(clientData, interp, argc, argv)
	ClientData clientData;   /* info for all known classes */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	Interp *iPtr = (Interp*)interp;
	Itcl_ClassInfo *info = (Itcl_ClassInfo*)clientData;
	Itcl_Class *cdefn = (Itcl_Class*)Itcl_PeekStack(&info->cdefnStack);
	Itcl_Variable *vdefn;
	char *init;

	Tcl_SetResult(interp, "", TCL_STATIC);
	if ((argc < 2) || (argc > 3))
	{
		Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0], " varname ?init?\"",
			(char*)NULL);
		return TCL_ERROR;
	}
	if (Itcl_FindPublicVar(cdefn->name,argv[1],cdefn) ||
	    Itcl_FindProtectedVar(cdefn->name,argv[1],cdefn) ||
	    Itcl_FindCommonVar(cdefn->name,argv[1],cdefn))
	{
		Tcl_AppendResult(interp, "variable name \"",
			argv[1], "\" already defined in class \"", cdefn->name, "\"",
			(char*)NULL);
		return TCL_ERROR;
	}
	init  = (argc >= 3) ? argv[2] : NULL;
	vdefn = Itcl_CreateVariable(cdefn,argv[1],init,(char*)NULL);

	Itcl_AddVariable(&cdefn->commons, vdefn);
	Itcl_CreateTclVar(vdefn, &cdefn->data);

	/*
	 *  Create links for this variable in the current stack frame,	
	 *  so that the variable can be initialized within the class
	 *  definition after it has been defined.  Avoid setting
	 *  protection flags so that these links will be discarded
	 *  when the stack frame is destroyed.
	 */
	Itcl_CreateTclVarLinks(vdefn, &cdefn->data,
		&iPtr->varFramePtr->varTable, 0);

	return TCL_OK;
}


/*
 * ========================================================================
 *  BUILT-IN OBJECT METHODS
 * ========================================================================
 */

/*
 * ------------------------------------------------------------------------
 *  Itcl_BiIsa()
 *
 *  HANDLES:  isa <className>
 *
 *  Invoked whenever the user issues the "isa" method for an object.
 *  Checks to see if the object has the given <className> anywhere
 *  in its heritage and returns 1 if so.  Returns 0 otherwise.
 * ------------------------------------------------------------------------
 */
static int
Itcl_BiIsa(clientData, interp, argc, argv)
	ClientData clientData;   /* class definition */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	Itcl_Class *cdefn = (Itcl_Class*)clientData;
	Itcl_HierIter hier;

	Tcl_SetResult(interp, "", TCL_STATIC);
	if (argc != 2)
	{
		Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0], " className\"",
			(char*)NULL);
		return TCL_ERROR;
	}

	Itcl_InitHierIter(&hier,cdefn);
	while ((cdefn=Itcl_AdvanceHierIter(&hier)) != NULL)
		if (strcmp(cdefn->name,argv[1]) == 0)
			break;

	if (cdefn)
		Tcl_SetResult(interp, "1", TCL_STATIC);
	else
		Tcl_SetResult(interp, "0", TCL_STATIC);

	Itcl_DeleteHierIter(&hier);
	return TCL_OK;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_BiDelete()
 *
 *  HANDLES:  delete
 *
 *  Invoked whenever the user issues the "delete" method for an object.
 *  Invokes the destructor method for the object and then destroys
 *  the object instance.
 * ------------------------------------------------------------------------
 */
static int
Itcl_BiDelete(clientData, interp, argc, argv)
	ClientData clientData;   /* class definition */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	int status = TCL_OK;
	Itcl_Class *cdefn = (Itcl_Class*)clientData;
	Itcl_Object *odefn = (Itcl_Object*)Itcl_PeekStack(&cdefn->info->objStack);
	Itcl_Method *mdefn, *realmd;
	Tcl_HashEntry *entry;
	Itcl_Class *cd;
	Itcl_HierIter hier;
	Itcl_Stack reverse;

	Tcl_SetResult(interp, "", TCL_STATIC);
	if (argc != 1)
	{
		Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0], "\"",
			(char*)NULL);
		return TCL_ERROR;
	}

	/*
	 *  Find all method definitions for destructors in superclasses.
	 *  Mark each destructor as not having been invoked.  After
	 *  the current class destructor has been executed, we will
	 *  go back and execute destructors that have still not been
	 *  invoked.
	 */
	Itcl_InitHierIter(&hier,cdefn);
	Itcl_AdvanceHierIter(&hier);

	while ((cd=Itcl_AdvanceHierIter(&hier)) != NULL)
	{
		entry = Tcl_FindHashEntry(&cd->allMethods,"destructor");
		mdefn = (entry)
			? (Itcl_Method*)Tcl_GetHashValue(entry)
			: (Itcl_Method*)NULL;

		realmd = mdefn;
		while (realmd && realmd->type == ITCL_INHERITED_METHOD)
			realmd = realmd->values.actual;

		if (realmd) realmd->invoked = 0;
	}
	Itcl_DeleteHierIter(&hier);

	/*
	 *  Execute the destructor for the most specific class.
	 */
	entry = Tcl_FindHashEntry(&cdefn->allMethods,"destructor");
	mdefn = (entry)
		? (Itcl_Method*)Tcl_GetHashValue(entry)
		: (Itcl_Method*)NULL;

	if (mdefn)
		status = Itcl_ExecMethod((ClientData)mdefn, interp, 0, (char**)NULL);

	/*
	 *  If the destructor succeeded, then invoke any superclass
	 *  destructors that might have been missed.  Take care to
	 *  invoke destructors in reverse order.
	 */
	if (status == TCL_OK)
	{
		Itcl_InitHierIter(&hier,cdefn);
		Itcl_AdvanceHierIter(&hier);

		Itcl_InitStack(&reverse);
		while ((cd=Itcl_AdvanceHierIter(&hier)) != NULL)
		{
			entry = Tcl_FindHashEntry(&cd->allMethods,"destructor");
			mdefn = (entry)
				? (Itcl_Method*)Tcl_GetHashValue(entry)
				: (Itcl_Method*)NULL;

			if (mdefn)
				Itcl_PushStack((ClientData)mdefn, &reverse);
		}
		Itcl_DeleteHierIter(&hier);

		while ((mdefn=(Itcl_Method*)Itcl_PopStack(&reverse)) != NULL)
		{
			realmd = mdefn;
			while (realmd->type == ITCL_INHERITED_METHOD)
				realmd = realmd->values.actual;

			if (!realmd->invoked)
			{
				status = Itcl_ExecMethod((ClientData)mdefn, interp,
					0, (char**)NULL);

				if (status != TCL_OK)
				{
					char msg[128];
					sprintf(msg,
						"\n    implicitly invoked by \"%.40s::destructor\"",
						cdefn->name);
					Tcl_AddErrorInfo(interp, msg);
					break;
				}
			}
		}
		Itcl_DeleteStack(&reverse);
	}

	/*
	 *  If the destructor succeeded, remove the instance command
	 *  from the main intepreter.  This will automatically destroy
	 *  the object instance data.
	 */
	if (status == TCL_OK)
	{
		Tcl_DeleteCommand(cdefn->info->main, odefn->name);
		Tcl_SetResult(interp, "", TCL_STATIC);
	}
	return status;
}

/*
 * ------------------------------------------------------------------------
 *  CLASS INFO
 * ------------------------------------------------------------------------
 */
typedef int (Itcl_AspectInfoProc) _ANSI_ARGS_((Itcl_Class *cdefn,
	Tcl_Interp *interp, int argc, char **argv));

typedef struct Itcl_AspectInfo {
	char *aspect;                /* aspect of class information */
	Itcl_AspectInfoProc *proc;   /* routine to get aspect information */
} Itcl_AspectInfo;

#define iTCL_ASPECTS sizeof(Itcl_Aspects)/sizeof(Itcl_AspectInfo)

static Itcl_AspectInfo Itcl_Aspects[] = {
	"class",        Itcl_GetClass,
	"inherit",      Itcl_GetInherit,
	"heritage",     Itcl_GetHeritage,
	"method",       Itcl_GetMethods,
	"proc",         Itcl_GetProcs,
	"body",         Itcl_GetBody,
	"public",       Itcl_GetPublics,
	"protected",    Itcl_GetProtecteds,
	"common",       Itcl_GetCommons,
};
static Itcl_AspectN = iTCL_ASPECTS;

/*
 * ------------------------------------------------------------------------
 *  Itcl_BiInfo()
 *
 *  HANDLES:  info <option> ?<args>...?
 *
 *  Invoked whenever the user issues the "info" method for an object.
 *  If the <option> is not recognized as part of the class information,
 *  the query is passed along to the usual TCL "info" command
 *  (renamed "tcl_info").
 * ------------------------------------------------------------------------
 */
static int
Itcl_BiInfo(clientData, interp, argc, argv)
	ClientData clientData;   /* class definition */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	Itcl_Class *cdefn = (Itcl_Class*)clientData;
	int i, len, status;
	char *elem;

	Tcl_SetResult(interp, "", TCL_STATIC);
	if (argc < 2)
	{
		Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0], " option ?args...?\"",
			(char*)NULL);
		return TCL_ERROR;
	}

	len = strlen(argv[1]);
	for (i=0; i < Itcl_AspectN; i++)
		if (strncmp(Itcl_Aspects[i].aspect,argv[1],len) == 0)
			break;

	if (i < Itcl_AspectN)
		status = (*Itcl_Aspects[i].proc)(cdefn,interp,argc-2,argv+2);
	else
	{
		argv[0] = "tcl_info";  /* use usual TCL info command */
		elem = Tcl_Merge(argc,argv);
		status = Tcl_Eval(cdefn->interp, elem);
		ckfree(elem);

		if ((status == TCL_ERROR) &&
			(strncmp(cdefn->interp->result,"bad option",10) == 0))
		{
			char *p, *opts;
			for (p=cdefn->interp->result; *p && (*p != ':'); p++)
				;
			if (*p) p += 12;  /* skip over "bad option 'xxx': should be" */
			opts = (char*)ckalloc((unsigned)(strlen(p)+1));
			strcpy(opts, p);

			Tcl_SetResult(interp, "", TCL_STATIC);
			Tcl_AppendResult(interp, "bad option \"", argv[1],
				"\": should be ", (char*)NULL);

			for (i=0; i < Itcl_AspectN; i++)
				Tcl_AppendResult(interp, Itcl_Aspects[i].aspect, ", ",
					 (char*)NULL);

			Tcl_AppendResult(interp, "or the usual: ", opts, (char*)NULL);
			ckfree(opts);
		}
		else
			Itcl_XferResult(interp, cdefn->interp, status);
	}
	return status;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_GetClass()
 *
 *  Sets the interpreter result to the most specific class name for this
 *  object.  Returns a status TCL_OK/TCL_ERROR to indicate success/failure.
 * ------------------------------------------------------------------------
 */
/* ARGSUSED */
static int
Itcl_GetClass(cdefn, interp, argc, argv)
	Itcl_Class *cdefn;    /* class definition */
	Tcl_Interp *interp;   /* current interpreter */
	int argc;             /* number of arguments */
	char **argv;          /* argument strings */
{
	if (argc != 0)
	{
		Tcl_SetResult(interp, "wrong # args: should be \"info class\"",
			TCL_STATIC);
		return TCL_ERROR;
	}
	Tcl_SetResult(interp, cdefn->name, TCL_VOLATILE);
	return TCL_OK;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_GetInherit()
 *
 *  Sets the interpreter result to the list of base classes for this
 *  class, as they are declared in the "inherit" statement.
 *  Returns a status TCL_OK/TCL_ERROR to indicate success/failure.
 * ------------------------------------------------------------------------
 */
/* ARGSUSED */
static int
Itcl_GetInherit(cdefn, interp, argc, argv)
	Itcl_Class *cdefn;    /* class definition */
	Tcl_Interp *interp;   /* current interpreter */
	int argc;             /* number of arguments */
	char **argv;          /* argument strings */
{
	Itcl_Class **slist;

	if (argc != 0)
	{
		Tcl_SetResult(interp, "wrong # args: should be \"info inherit\"",
			TCL_STATIC);
		return TCL_ERROR;
	}

	/*
	 *  Superclasses are stored in reverse order, so make sure to
	 *  reverse them again in the return list.
	 */
	Tcl_SetResult(interp, "", TCL_STATIC);

	if (cdefn->supers)
	{
		for (slist=cdefn->supers; *slist && *(slist+1); slist++)
			;
		for ( ; slist >= cdefn->supers; slist--)
			Tcl_AppendElement(interp, (*slist)->name);
	}
	return TCL_OK;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_GetHeritage()
 *
 *  Sets the interpreter result to the entire derivation hierarchy for
 *  this class, presented in the order that classes are traversed for
 *  finding data and member functions.  Returns a status TCL_OK/TCL_ERROR
 *  to indicate success/failure.
 * ------------------------------------------------------------------------
 */
/* ARGSUSED */
static int
Itcl_GetHeritage(cdefn, interp, argc, argv)
	Itcl_Class *cdefn;    /* class definition */
	Tcl_Interp *interp;   /* current interpreter */
	int argc;             /* number of arguments */
	char **argv;          /* argument strings */
{
	Itcl_HierIter hier;

	if (argc != 0)
	{
		Tcl_SetResult(interp, "wrong # args: should be \"info heritage\"",
			TCL_STATIC);
		return TCL_ERROR;
	}

	/*
	 *  Traverse through the derivation hierarchy and return
	 *  superclass names.
	 */
	Tcl_SetResult(interp, "", TCL_STATIC);

	Itcl_InitHierIter(&hier,cdefn);
	while ((cdefn=Itcl_AdvanceHierIter(&hier)) != NULL)
		Tcl_AppendElement(interp, cdefn->name);

	Itcl_DeleteHierIter(&hier);
	return TCL_OK;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_GetMethods()
 *
 *  Sets the interpreter result to contain information for methods
 *  in the class, parsing the following arguments:
 *
 *     ?methodName? ?-args? ?-body?
 *
 *  If the ?methodName? is not specified, then a list of all known
 *  methods is returned.  Otherwise, the information (args/body) for
 *  a specific method is returned.  Returns a status TCL_OK/TCL_ERROR
 *  to indicate success/failure.
 * ------------------------------------------------------------------------
 */
static int
Itcl_GetMethods(cdefn, interp, argc, argv)
	Itcl_Class *cdefn;    /* class definition */
	Tcl_Interp *interp;   /* current interpreter */
	int argc;             /* number of arguments */
	char **argv;          /* argument strings */
{
	char *methodName = NULL;
	int methodArgs = 0;
	int methodBody = 0;
	char *elem = NULL;

	char cname[85], mname[85];
	Tcl_HashSearch place;
	Tcl_HashEntry *mEntry;
	Itcl_Method *mdefn, *realmd;
	Itcl_HierIter hier;

	/*
	 *  Process args:  ?methodName? ?-args? ?-body?
	 */
	Tcl_SetResult(interp, "", TCL_STATIC);
	if (argc > 0)
	{
		methodName = *argv;
		argc--; argv++;
	}
	for ( ; argc > 0; argc--, argv++)
	{
		if (strcmp(*argv,"-args") == 0)
			methodArgs = ~0;
		else if (strcmp(*argv,"-body") == 0)
			methodBody = ~0;
		else
		{
			Tcl_AppendResult(interp, "bad option \"", *argv,
				"\": should be -args or -body",
				(char*)NULL);
			return TCL_ERROR;
		}
	}

	/*
	 *  Return info for a specific method.
	 */
	if (methodName)
	{
		Itcl_ParseSlotName(methodName,cname,mname,85);
		mdefn = Itcl_FindMethod(cname,mname,cdefn);
		if (mdefn)
		{
			int i, valc = 0;
			char *valv[5];

			/*
			 *  If this is an inherited method, then find the real
			 *  method definition.
			 */
			realmd = mdefn;
			while (realmd->type == ITCL_INHERITED_METHOD)
				realmd = realmd->values.actual;

			if (realmd->type == ITCL_BUILTIN_METHOD)
			{
				if (!methodArgs && !methodBody)
				{
					valv[valc++] = Itcl_MakeSlotName(realmd->cdefn->name,
						realmd->name);
					methodArgs = methodBody = ~0;
				}
				if (methodArgs || methodBody)
					valv[valc++] = "<built-in>";
			}
			else
			{
				if (!methodArgs && !methodBody)
				{
					valv[valc++] = Itcl_MakeSlotName(realmd->cdefn->name,
						realmd->name);
					methodArgs = methodBody = ~0;
				}
				if (methodArgs)
				{
					elem = Itcl_ArgList(realmd->arglist);
					valv[valc++] = elem;
				}
				if (methodBody)
					valv[valc++] = realmd->values.body;
			}

			/*
			 *  If the result list has a single element, then
			 *  return it using Tcl_SetResult() so that it will
			 *  look like a string and not a list with one element.
			 */
			if (valc == 1)
				Tcl_SetResult(interp,valv[0],TCL_VOLATILE);
			else
				for (i=0; i < valc; i++)
					Tcl_AppendElement(interp,valv[i]);

			if (elem) ckfree(elem);
		}
	}

	/*
	 *  Return the list of available methods.
	 */
	else
	{
		Itcl_InitHierIter(&hier, cdefn);
		while ((cdefn=Itcl_AdvanceHierIter(&hier)) != NULL)
		{
			mEntry = Tcl_FirstHashEntry(&cdefn->methods,&place);
			while (mEntry)
			{
				mdefn = (Itcl_Method*)Tcl_GetHashValue(mEntry);
				if (mdefn->type != ITCL_INHERITED_METHOD)
				{
					elem = Itcl_MakeSlotName(mdefn->cdefn->name, mdefn->name);
					Tcl_AppendElement(interp, elem);
				}
				mEntry = Tcl_NextHashEntry(&place);
			}
		}
		Itcl_DeleteHierIter(&hier);
	}
	return TCL_OK;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_GetProcs()
 *
 *  Sets the interpreter result to contain information for procs
 *  in the class, parsing the following arguments:
 *
 *     ?procName? ?-args? ?-body?
 *
 *  If the ?procName? is not specified, then a list of all known
 *  procs is returned.  Otherwise, the information (args/body) for
 *  a specific proc is returned.  Returns a status TCL_OK/TCL_ERROR
 *  to indicate success/failure.
 * ------------------------------------------------------------------------
 */
static int
Itcl_GetProcs(cdefn, interp, argc, argv)
	Itcl_Class *cdefn;    /* class definition */
	Tcl_Interp *interp;   /* current interpreter */
	int argc;             /* number of arguments */
	char **argv;          /* argument strings */
{
	char *procName = NULL;
	int procArgs = 0;
	int procBody = 0;
	char *elem = NULL;

	char cname[85], mname[85];
	Tcl_HashSearch place;
	Tcl_HashEntry *mEntry;
	Itcl_Method *mdefn, *realmd;
	Itcl_HierIter hier;

	/*
	 *  Process args:  ?procName? ?-args? ?-body?
	 */
	Tcl_SetResult(interp, "", TCL_STATIC);
	if (argc > 0)
	{
		procName = *argv;
		argc--; argv++;
	}
	for ( ; argc > 0; argc--, argv++)
	{
		if (strcmp(*argv,"-args") == 0)
			procArgs = ~0;
		else if (strcmp(*argv,"-body") == 0)
			procBody = ~0;
		else
		{
			Tcl_AppendResult(interp, "bad option \"", *argv,
				"\": should be -args or -body",
				(char*)NULL);
			return TCL_ERROR;
		}
	}

	/*
	 *  Return info for a specific proc.
	 */
	if (procName)
	{
		Itcl_ParseSlotName(procName,cname,mname,85);
		mdefn = Itcl_FindProc(cname,mname,cdefn);
		if (mdefn)
		{
			int i, valc = 0;
			char *valv[5];

			/*
			 *  If this is an inherited method, then find the real
			 *  method definition.
			 */
			realmd = mdefn;
			while (realmd->type == ITCL_INHERITED_METHOD)
				realmd = realmd->values.actual;

			if (realmd->type == ITCL_BUILTIN_METHOD)
			{
				if (!procArgs && !procBody)
				{
					valv[valc++] = Itcl_MakeSlotName(realmd->cdefn->name,
						realmd->name);
					procArgs = procBody = ~0;
				}
				if (procArgs || procBody)
					valv[valc++] = "<built-in>";
			}
			else
			{
				if (!procArgs && !procBody)
				{
					valv[valc++] = Itcl_MakeSlotName(realmd->cdefn->name,
						realmd->name);
					procArgs = procBody = ~0;
				}
				if (procArgs)
				{
					elem = Itcl_ArgList(realmd->arglist);
					valv[valc++] = elem;
				}
				if (procBody)
					valv[valc++] = realmd->values.body;
			}

			/*
			 *  If the result list has a single element, then
			 *  return it using Tcl_SetResult() so that it will
			 *  look like a string and not a list with one element.
			 */
			if (valc == 1)
				Tcl_SetResult(interp,valv[0],TCL_VOLATILE);
			else
				for (i=0; i < valc; i++)
					Tcl_AppendElement(interp,valv[i]);

			if (elem) ckfree(elem);
		}
	}

	/*
	 *  Return the list of available procs.
	 */
	else
	{
		Itcl_InitHierIter(&hier, cdefn);
		while ((cdefn=Itcl_AdvanceHierIter(&hier)) != NULL)
		{
			mEntry = Tcl_FirstHashEntry(&cdefn->procs,&place);
			while (mEntry)
			{
				mdefn = (Itcl_Method*)Tcl_GetHashValue(mEntry);
				if (mdefn->type != ITCL_INHERITED_METHOD)
				{
					elem = Itcl_MakeSlotName(mdefn->cdefn->name, mdefn->name);
					Tcl_AppendElement(interp, elem);
				}
				mEntry = Tcl_NextHashEntry(&place);
			}
		}
		Itcl_DeleteHierIter(&hier);
	}
	return TCL_OK;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_GetBody()
 *
 *  Handles the usual "info body" request, returning the body for a
 *  specific proc.  Included here for backward compatibility, since
 *  otherwise Tcl would complain that class procs are not real "procs".
 *  Returns a status TCL_OK/TCL_ERROR to indicate success/failure.
 * ------------------------------------------------------------------------
 */
static int
Itcl_GetBody(cdefn, interp, argc, argv)
	Itcl_Class *cdefn;    /* class definition */
	Tcl_Interp *interp;   /* current interpreter */
	int argc;             /* number of arguments */
	char **argv;          /* argument strings */
{
	char cname[85], mname[85];
	Itcl_Method *mdefn, *realmd;

	Tcl_SetResult(interp, "", TCL_STATIC);
	if (argc != 1)
	{
		Tcl_SetResult(interp, "wrong # args: should be \"info body procname\"",
			TCL_STATIC);
		return TCL_ERROR;
	}

	Itcl_ParseSlotName(argv[0],cname,mname,85);
	mdefn = Itcl_FindProc(cname,mname,cdefn);
	if (!mdefn)
	{
		Tcl_AppendResult(interp, "\"", argv[0], "\" isn't a procedure",
			(char*)NULL);
		return TCL_ERROR;
	}

	/*
	 *  If this is an inherited method, then find the real
	 *  method definition.
	 */
	realmd = mdefn;
	while (realmd->type == ITCL_INHERITED_METHOD)
		realmd = realmd->values.actual;

	switch (realmd->type)
	{
	case ITCL_BUILTIN_METHOD:
		Tcl_SetResult(interp,"<built-in>",TCL_STATIC);
		break;

	case ITCL_TCL_METHOD:
		Tcl_SetResult(interp,realmd->values.body,TCL_STATIC);
		break;
	}
	return TCL_OK;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_GetPublics()
 *
 *  Sets the interpreter result to contain information for public
 *  variables in the class, parsing the following arguments:
 *
 *     ?varName? ?-init? ?-value? ?-config?
 *
 *  If the ?varName? is not specified, then a list of all known public
 *  variables is returned.  Otherwise, the information (init/value/config)
 *  for a specific variable is returned.  Returns a status
 *  TCL_OK/TCL_ERROR to indicate success/failure.
 * ------------------------------------------------------------------------
 */
static int
Itcl_GetPublics(cdefn, interp, argc, argv)
	Itcl_Class *cdefn;    /* class definition */
	Tcl_Interp *interp;   /* current interpreter */
	int argc;             /* number of arguments */
	char **argv;          /* argument strings */
{
	char *varName = NULL;
	int varInit = 0;
	int varCheck = 0;
	int varValue = 0;

	char cname[85], vname[85];
	Itcl_Variable *vdefn;
	Itcl_HierIter hier;
	Itcl_Object *odefn;

	/*
	 *  Process args:  ?varName? ?-init? ?-value? ?-config?
	 */
	Tcl_SetResult(interp, "", TCL_STATIC);
	if (argc > 0)
	{
		varName = *argv;
		argc--; argv++;
	}
	for ( ; argc > 0; argc--, argv++)
	{
		if (strcmp(*argv,"-init") == 0)
			varInit = ~0;
		else if (strcmp(*argv,"-value") == 0)
			varValue = ~0;
		else if (strcmp(*argv,"-config") == 0)
			varCheck = ~0;
		else
		{
			Tcl_AppendResult(interp, "bad option \"", *argv,
				"\": should be -init, -value or -config",
				(char*)NULL);
			return TCL_ERROR;
		}
	}

	/*
	 *  Return info for a specific variable.
	 */
	if (varName)
	{
		int i, valc = 0;
		char *valv[5];

		Itcl_ParseSlotName(varName,cname,vname,85);
		if ((vdefn=Itcl_FindPublicVar(cname,vname,cdefn)) != NULL)
		{
			if (!varInit && !varCheck && !varValue)
			{
				valv[valc++] = vdefn->fullname;
				varInit = varCheck = varValue = ~0;
			}
			if (varInit)
				valv[valc++] = (vdefn->init) ? vdefn->init : "";

			if (varValue)
			{
				char *val;
				odefn = (Itcl_Object*)Itcl_PeekStack(&cdefn->info->objStack);
				if (!odefn)
				{
					Tcl_SetResult(interp, "cannot return object-specific info without object context", TCL_STATIC);
					return TCL_ERROR;
				}
				val = Itcl_GetVar(vdefn, &odefn->data);
				valv[valc++] = (val) ? val : "";
			}

			if (varCheck)
				valv[valc++] = (vdefn->config) ? vdefn->config : "";

			/*
			 *  If the result list has a single element, then
			 *  return it using Tcl_SetResult() so that it will
			 *  look like a string and not a list with one element.
			 */
			if (valc == 1)
				Tcl_SetResult(interp,valv[0],TCL_VOLATILE);
			else
				for (i=0; i < valc; i++)
					Tcl_AppendElement(interp,valv[i]);
		}
	}

	/*
	 *  Return the list of public variables.
	 */
	else
	{
		Itcl_InitHierIter(&hier, cdefn);
		while ((cdefn=Itcl_AdvanceHierIter(&hier)) != NULL)
		{
			for (vdefn=cdefn->publics; vdefn; vdefn=vdefn->next)
				Tcl_AppendElement(interp, vdefn->fullname);
		}
		Itcl_DeleteHierIter(&hier);
	}
	return TCL_OK;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_GetProtecteds()
 *
 *  Sets the interpreter result to contain information for protected
 *  variables in the class, parsing the following arguments:
 *
 *     ?varName? ?-init? ?-value?
 *
 *  If the ?varName? is not specified, then a list of all known protected
 *  variables is returned.  Otherwise, the information (init/value)
 *  for a specific variable is returned.  Returns a status
 *  TCL_OK/TCL_ERROR to indicate success/failure.
 * ------------------------------------------------------------------------
 */
static int
Itcl_GetProtecteds(cdefn, interp, argc, argv)
	Itcl_Class *cdefn;    /* class definition */
	Tcl_Interp *interp;   /* current interpreter */
	int argc;             /* number of arguments */
	char **argv;          /* argument strings */
{
	char *varName = NULL;
	int varInit = 0;
	int varValue = 0;

	char cname[85], vname[85];
	Itcl_Variable *vdefn;
	Itcl_HierIter hier;
	Itcl_Object *odefn;

	/*
	 *  Process args:  ?varName? ?-init? ?-value?
	 */
	Tcl_SetResult(interp, "", TCL_STATIC);
	if (argc > 0)
	{
		varName = *argv;
		argc--; argv++;
	}
	for ( ; argc > 0; argc--, argv++)
	{
		if (strcmp(*argv,"-init") == 0)
			varInit = ~0;
		else if (strcmp(*argv,"-value") == 0)
			varValue = ~0;
		else
		{
			Tcl_AppendResult(interp, "bad option \"", *argv,
				"\": should be -init or -value",
				(char*)NULL);
			return TCL_ERROR;
		}
	}

	/*
	 *  Return info for a specific variable.
	 */
	if (varName)
	{
		int i, valc = 0;
		char *valv[5];

		Itcl_ParseSlotName(varName,cname,vname,85);
		if ((vdefn=Itcl_FindProtectedVar(cname,vname,cdefn)) != NULL)
		{
			if (!varInit && !varValue)
			{
				valv[valc++] = vdefn->fullname;
				varInit = varValue = ~0;
			}
			if (varInit)
				valv[valc++] = (vdefn->init) ? vdefn->init : "";

			if (varValue)
			{
				char *val;
				odefn = (Itcl_Object*)Itcl_PeekStack(&cdefn->info->objStack);
				if (!odefn)
				{
					Tcl_SetResult(interp, "cannot return object-specific info without object context", TCL_STATIC);
					return TCL_ERROR;
				}
				val = Itcl_GetVar(vdefn, &odefn->data);
				valv[valc++] = (val) ? val : "";
			}

			/*
			 *  If the result list has a single element, then
			 *  return it using Tcl_SetResult() so that it will
			 *  look like a string and not a list with one element.
			 */
			if (valc == 1)
				Tcl_SetResult(interp,valv[0],TCL_VOLATILE);
			else
				for (i=0; i < valc; i++)
					Tcl_AppendElement(interp,valv[i]);
		}
	}

	/*
	 *  Return the list of protected variables.
	 */
	else
	{
		Itcl_InitHierIter(&hier, cdefn);
		while ((cdefn=Itcl_AdvanceHierIter(&hier)) != NULL)
		{
			for (vdefn=cdefn->protecteds; vdefn; vdefn=vdefn->next)
				Tcl_AppendElement(interp, vdefn->fullname);
		}
		Itcl_DeleteHierIter(&hier);
	}
	return TCL_OK;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_GetCommons()
 *
 *  Sets the interpreter result to contain information for common
 *  variables in the class, parsing the following arguments:
 *
 *     ?varName? ?-init? ?-value?
 *
 *  If the ?varName? is not specified, then a list of all known common
 *  variables is returned.  Otherwise, the information (init/value)
 *  for a specific variable is returned.  Returns a status
 *  TCL_OK/TCL_ERROR to indicate success/failure.
 * ------------------------------------------------------------------------
 */
static int
Itcl_GetCommons(cdefn, interp, argc, argv)
	Itcl_Class *cdefn;    /* class definition */
	Tcl_Interp *interp;   /* current interpreter */
	int argc;             /* number of arguments */
	char **argv;          /* argument strings */
{
	char *varName = NULL;
	int varInit = 0;
	int varValue = 0;

	char cname[85], vname[85];
	Itcl_Variable *vdefn;
	Itcl_HierIter hier;

	/*
	 *  Process args:  ?varName? ?-init? ?-value?
	 */
	Tcl_SetResult(interp, "", TCL_STATIC);
	if (argc > 0)
	{
		varName = *argv;
		argc--; argv++;
	}
	for ( ; argc > 0; argc--, argv++)
	{
		if (strcmp(*argv,"-init") == 0)
			varInit = ~0;
		else if (strcmp(*argv,"-value") == 0)
			varValue = ~0;
		else
		{
			Tcl_AppendResult(interp, "bad option \"", *argv,
				"\": should be -init or -value",
				(char*)NULL);
			return TCL_ERROR;
		}
	}

	/*
	 *  Return info for a specific variable.
	 */
	if (varName)
	{
		int i, valc = 0;
		char *valv[5];

		Itcl_ParseSlotName(varName,cname,vname,85);
		if ((vdefn=Itcl_FindCommonVar(cname,vname,cdefn)) != NULL)
		{
			if (!varInit && !varValue)
			{
				valv[valc++] = vdefn->fullname;
				varInit = varValue = ~0;
			}
			if (varInit)
				valv[valc++] = (vdefn->init) ? vdefn->init : "";

			if (varValue)
			{
				char *val = Itcl_GetVar(vdefn, &vdefn->cdefn->data);
				valv[valc++] = (val) ? val : "";
			}

			/*
			 *  If the result list has a single element, then
			 *  return it using Tcl_SetResult() so that it will
			 *  look like a string and not a list with one element.
			 */
			if (valc == 1)
				Tcl_SetResult(interp,valv[0],TCL_VOLATILE);
			else
				for (i=0; i < valc; i++)
					Tcl_AppendElement(interp,valv[i]);
		}
	}

	/*
	 *  Return the list of common variables.
	 */
	else
	{
		Itcl_InitHierIter(&hier, cdefn);
		while ((cdefn=Itcl_AdvanceHierIter(&hier)) != NULL)
		{
			for (vdefn=cdefn->commons; vdefn; vdefn=vdefn->next)
				Tcl_AppendElement(interp, vdefn->fullname);
		}
		Itcl_DeleteHierIter(&hier);
	}
	return TCL_OK;
}
