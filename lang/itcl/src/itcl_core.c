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
 *     RCS:  itcl_core.c,v 1.2 1994/03/25 19:01:10 mmc Exp
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
#include "itcl.h"
#include "itcl_util.h"
#include "itcl_core.h"

/*
 *  FORWARD DECLARATIONS
 */
static void Itcl_DelClassInfo _ANSI_ARGS_((ClientData cdata));

static int Itcl_ClassCmd _ANSI_ARGS_((ClientData cdata,
	Tcl_Interp *interp, int argc, char **argv));
static int Itcl_ClassUseCmd _ANSI_ARGS_((ClientData cdata,
	Tcl_Interp *interp, int argc, char **argv));
static int Itcl_AccessClassScope _ANSI_ARGS_((Itcl_Class *cdefn,
	Tcl_Interp *interp, int argc, char **argv));

static int Itcl_InstanceCmd _ANSI_ARGS_((ClientData cdata,
	Tcl_Interp *interp, int argc, char **argv));

static int Itcl_InfoCmd _ANSI_ARGS_((ClientData cdata,
	Tcl_Interp *interp, int argc, char **argv));

#ifndef lint
static char sccsid[] = "@(#)itcl_core.c	1.22 (10/27/93) Michael J. McLennan";
#endif


/*
 * ------------------------------------------------------------------------
 *  Itcl_Init()
 *
 *  Should be invoked whenever a new interpeter is created to add
 *  [incr Tcl] facilities.  Creates a new Itcl_ClassInfo structure
 *  to track all classes/objects, and adds the "itcl_class" and
 *  "itcl_info" commands to the given interpreter.
 * ------------------------------------------------------------------------
 */
int
Itcl_Init(interp)
	Tcl_Interp *interp;  /* interpreter to be updated */
{
	Itcl_ClassInfo *info;
	Tcl_CmdInfo cmdInfo;

	/*
	 *  Install [incr Tcl] facilities if not already installed.
	 */
	if (Tcl_GetCommandInfo(interp, "itcl_class", &cmdInfo))
	{
		Tcl_SetResult(interp, "already installed: itcl_class", TCL_STATIC);
		return TCL_ERROR;
	}

	info = (Itcl_ClassInfo*)ckalloc(sizeof(Itcl_ClassInfo));
	info->main = interp;
	Tcl_InitHashTable(&info->classes,TCL_STRING_KEYS);
	Tcl_InitHashTable(&info->objects,TCL_STRING_KEYS);

	Itcl_InitStack(&info->cdefnStack);
	Itcl_InitStack(&info->objStack);

	Itcl_EventuallyFree((ClientData)info, Itcl_DelClassInfo);

	/*
	 *  Install "itcl_class" and "itcl_info" commands.
	 */
	Tcl_CreateCommand(interp, "itcl_class", Itcl_ClassCmd,
		Itcl_PreserveData((ClientData)info), Itcl_ReleaseData);

	Tcl_CreateCommand(interp, "itcl_info", Itcl_InfoCmd,
		Itcl_PreserveData((ClientData)info), Itcl_ReleaseData);

	/*
	 *  Replace "uplevel" command so that it is aware of interpreters
	 *  used for [incr Tcl] classes.
	 */
	Tcl_DeleteCommand(interp, "uplevel");
	Tcl_CreateCommand(interp, "uplevel", Itcl_UplevelCmd,
		Itcl_PreserveData((ClientData)info), Itcl_ReleaseData);

	return TCL_OK;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_DelClassInfo()
 *
 *  Invoked when the management info for [incr Tcl] is no longer being
 *  used in an interpreter.  This will only occur when all class
 *  manipulation commands are removed from the interpreter.
 * ------------------------------------------------------------------------
 */
static void
Itcl_DelClassInfo(cdata)
	ClientData cdata;    /* client data for class command */
{
	Itcl_ClassInfo *info = (Itcl_ClassInfo*)cdata;

	Tcl_HashSearch place;
	Tcl_HashEntry *entry;

	/*
	 *  Destroy all known objects.
	 */
	entry = Tcl_FirstHashEntry(&info->objects,&place);
	while (entry)
	{
		Itcl_ReleaseData(Tcl_GetHashValue(entry));
		entry = Tcl_NextHashEntry(&place);
	}
	Tcl_DeleteHashTable(&info->objects);

	/*
	 *  Destroy all known classes.
	 */
	entry = Tcl_FirstHashEntry(&info->classes,&place);
	while (entry)
	{
		Itcl_ReleaseData(Tcl_GetHashValue(entry));
		entry = Tcl_NextHashEntry(&place);
	}
	Tcl_DeleteHashTable(&info->classes);

	Itcl_DeleteStack(&info->cdefnStack);
	Itcl_DeleteStack(&info->objStack);

	ckfree((char*)info);
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_ClassCmd()
 *
 *  Invoked by TCL whenever the user issues a "itcl_class" command to
 *  specify a new class definition.  Handles the following syntax:
 *
 *    itcl_class <className> {
 *        inherit <superclass>
 *
 *        constructor {<arglist>} { <body> }
 *        destructor { <body> }
 *
 *        method <name> {<arglist>} { <body> }
 *        public <varname> ?<init>? ?<config>?
 *        protected <varname> ?<init>?
 *        common <varname> ?<init>?
 *    }
 *
 * ------------------------------------------------------------------------
 */
static int
Itcl_ClassCmd(clientData, interp, argc, argv)
	ClientData clientData;   /* info for all known classes */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	Itcl_ClassInfo *info = (Itcl_ClassInfo*)clientData;
	int newEntry;
	char *fullname;
	Tcl_HashEntry *classEntry, *mEntry;
	Tcl_HashSearch place;
	Itcl_Class *cdefn, *cd;
	Itcl_Method *mdefn;
	Itcl_HierIter hier;
	Itcl_ExecScope exscope;

	Tcl_SetResult(interp, "", TCL_STATIC);
	if (argc != 3)
	{
		Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0], " name { definition }\"",
			(char*)NULL);
		return TCL_ERROR;
	}

	/*
	 *  If the class definition is already known, then flag an error.
	 */
	classEntry = Tcl_CreateHashEntry(&info->classes, argv[1], &newEntry);
	if (!newEntry)
	{
		Tcl_AppendResult(interp, "class \"", argv[1], "\" already exists",
			(char*)NULL);
		return TCL_ERROR;
	}

	/*
	 *  Allocate a new class and attempt to parse class definition.
	 */
	cdefn = Itcl_CreateClass(argv[1],info);
	Itcl_PushStack((ClientData)cdefn, &info->cdefnStack);
	Itcl_EnterExecScope(cdefn->interp, &exscope, cdefn->vscope, interp);

	if (Tcl_Eval(cdefn->interp, argv[2]) != TCL_OK)
	{
		char msg[85];
		Itcl_XferResult(interp, cdefn->interp, TCL_ERROR);

		sprintf(msg, "\n    (class \"%.40s\" body line %d)",
			argv[1], cdefn->interp->errorLine);
		Tcl_AddErrorInfo(interp, msg);

		Itcl_LeaveExecScope(cdefn->interp, &exscope);
		Itcl_DeleteClass( Itcl_PopStack(&info->cdefnStack) );
		return TCL_ERROR;
	}

	/*
	 *  At this point, parsing of the class definition has succeeded.
	 *  Make a list of all member functions in the class, including
	 *  inherited functions.  Along the way, register commands in
	 *  the class interpreter.  Start with this class...
	 */
	mEntry = Tcl_FirstHashEntry(&cdefn->methods,&place);
	while (mEntry)
	{
		mdefn = (Itcl_Method*)Tcl_GetHashValue(mEntry);
		mEntry = Tcl_CreateHashEntry(&cdefn->allMethods,mdefn->name,&newEntry);
		Tcl_SetHashValue(mEntry, (ClientData)mdefn);
		Itcl_AddMethodCmd(cdefn, mdefn->name, mdefn);

		fullname = Itcl_MakeSlotName(cdefn->name,mdefn->name);
		mEntry = Tcl_CreateHashEntry(&cdefn->allMethods,fullname,&newEntry);
		Tcl_SetHashValue(mEntry, (ClientData)mdefn);
		Itcl_AddMethodCmd(cdefn, fullname, mdefn);

		mEntry = Tcl_NextHashEntry(&place);
	}

	mEntry = Tcl_FirstHashEntry(&cdefn->procs,&place);
	while (mEntry)
	{
		mdefn = (Itcl_Method*)Tcl_GetHashValue(mEntry);
		mEntry = Tcl_CreateHashEntry(&cdefn->allProcs,mdefn->name,&newEntry);
		Tcl_SetHashValue(mEntry, (ClientData)mdefn);
		Itcl_AddProcCmd(cdefn, mdefn->name, mdefn);

		fullname = Itcl_MakeSlotName(cdefn->name,mdefn->name);
		mEntry = Tcl_CreateHashEntry(&cdefn->allProcs,fullname,&newEntry);
		Tcl_SetHashValue(mEntry, (ClientData)mdefn);
		Itcl_AddProcCmd(cdefn, fullname, mdefn);

		mEntry = Tcl_NextHashEntry(&place);
	}

	/*
	 *  Continue building the list of member functions by scanning
	 *  through all base classes.  Along the way, add definitions
	 *  for inherited member functions and explicit commands for
	 *  these functions to the interpreter so that they can be used
	 *  directly.
	 */
	Itcl_InitHierIter(&hier,cdefn);
	Itcl_AdvanceHierIter(&hier);    /* skip this class */

	while ((cd=Itcl_AdvanceHierIter(&hier)) != NULL)
	{
		/*
		 *  Handle METHODS in all classes...
		 */
		mEntry = Tcl_FirstHashEntry(&cd->methods,&place);
		while (mEntry)
		{
			mdefn = (Itcl_Method*)Tcl_GetHashValue(mEntry);

			fullname = Itcl_MakeSlotName(cd->name,mdefn->name);
			mEntry = Tcl_CreateHashEntry(&cdefn->allMethods,fullname,&newEntry);
			Tcl_SetHashValue(mEntry, (ClientData)mdefn);

			if (Tcl_FindHashEntry(&cdefn->allMethods, mdefn->name))
				Itcl_AddMethodCmd(cdefn, fullname, mdefn);
			else
			{
				Itcl_AddMethodCmd(cdefn, fullname, mdefn);

				mdefn = Itcl_CreateRefMethod(cdefn,mdefn);
				Itcl_AddMethod(cdefn, mdefn);

				fullname = Itcl_MakeSlotName(cdefn->name,mdefn->name);
				Itcl_AddMethodCmd(cdefn, mdefn->name, mdefn);
				Itcl_AddMethodCmd(cdefn, fullname, mdefn);

				mEntry = Tcl_CreateHashEntry(&cdefn->allMethods,
					mdefn->name, &newEntry);
				Tcl_SetHashValue(mEntry, (ClientData)mdefn);
			}
			mEntry = Tcl_NextHashEntry(&place);
		}

		/*
		 *  Handle PROCS in all classes...
		 */
		mEntry = Tcl_FirstHashEntry(&cd->procs,&place);
		while (mEntry)
		{
			mdefn = (Itcl_Method*)Tcl_GetHashValue(mEntry);

			fullname = Itcl_MakeSlotName(cd->name,mdefn->name);
			mEntry = Tcl_CreateHashEntry(&cdefn->allProcs,fullname,&newEntry);
			Tcl_SetHashValue(mEntry, (ClientData)mdefn);

			if (Tcl_FindHashEntry(&cdefn->allProcs, mdefn->name))
				Itcl_AddProcCmd(cdefn, fullname, mdefn);
			else
			{
				Itcl_AddProcCmd(cdefn, fullname, mdefn);

				mdefn = Itcl_CreateRefMethod(cdefn,mdefn);
				Itcl_AddProc(cdefn, mdefn);

				fullname = Itcl_MakeSlotName(cdefn->name,mdefn->name);
				Itcl_AddProcCmd(cdefn, mdefn->name, mdefn);
				Itcl_AddProcCmd(cdefn, fullname, mdefn);

				mEntry = Tcl_CreateHashEntry(&cdefn->allProcs,
					mdefn->name, &newEntry);
				Tcl_SetHashValue(mEntry, (ClientData)mdefn);
			}
			mEntry = Tcl_NextHashEntry(&place);
		}
	}
	Itcl_DeleteHierIter(&hier);

	/*
	 *  Create a scope containing common variable names for
	 *  proc invocations.
	 */
	Itcl_DeleteVarScope(cdefn->vscope);  /* destroy bogus scope */
	cdefn->vscope = Itcl_CreateVarScope((Itcl_Object*)NULL,cdefn);

	/*
	 *  Remove commands for parsing class definitions.
	 */
	Itcl_LeaveExecScope(cdefn->interp, &exscope);
	Tcl_DeleteCommand(cdefn->interp, "inherit");
	Tcl_DeleteCommand(cdefn->interp, "constructor");
	Tcl_DeleteCommand(cdefn->interp, "destructor");
	Tcl_DeleteCommand(cdefn->interp, "method");
	Tcl_DeleteCommand(cdefn->interp, "proc");
	Tcl_DeleteCommand(cdefn->interp, "public");
	Tcl_DeleteCommand(cdefn->interp, "protected");
	Tcl_DeleteCommand(cdefn->interp, "common");

	/*
	 *  Install the class in the list of known classes.
	 *  Make the command in the main interpreter to manage a class:
	 *
	 *    <className>
	 *    <className> <objName> ?<constructor-args>?
	 *    <className> #auto ?<constructor-args>?
	 *    <className> :: ?<constructor-args>?
	 */
	Itcl_EventuallyFree((ClientData)cdefn, Itcl_DeleteClass);

	Tcl_CreateCommand(interp, cdefn->name, Itcl_ClassUseCmd,
		Itcl_PreserveData((ClientData)cdefn), Itcl_ReleaseData);

	Tcl_SetHashValue(classEntry, (ClientData)cdefn);
	(void)Itcl_PopStack(&info->cdefnStack);

	Tcl_SetResult(interp, "", TCL_STATIC);
	return TCL_OK;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_ClassUseCmd()
 *
 *  Invoked by TCL whenever the user issues the command associated with
 *  a class name.  Handles the following syntax:
 *
 *    <className>
 *    <className> <objName> ?<args>...?
 *    <className> #auto ?<args>...?
 *    <className> :: <command> ?<args>...?
 *
 *  Without any arguments, the command does nothing; this allows the
 *  class name to be invoked by itself to prompt the autoloader to
 *  load the class definition.  With the "::" argument, the remaining
 *  arguments are invoked as a command in the interpreter managing
 *  class scope.  The other forms of the command create a new instance
 *  in this class.
 *
 * ------------------------------------------------------------------------
 */
static int
Itcl_ClassUseCmd(clientData, interp, argc, argv)
	ClientData clientData;   /* class definition */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	char buffer[256];  /* buffer used to build unique names */

	int status = TCL_OK;
	Itcl_Class *cdefn = (Itcl_Class*)clientData;
	Itcl_Object *odefn;
	Itcl_Class *cd;
	Itcl_HierIter hier;
	register Itcl_Method *mdefn, *realmd;
	register Tcl_HashEntry *entry;
	char c, *objName;

	/*
	 *  If the command is invoked without an object name, then do nothing.
	 *  This supports autoloading, so that the class name can be invoked
	 *  as a command by itself, prompting the autoloader to load the
	 *  class definition.
	 */
	Tcl_SetResult(interp, "", TCL_STATIC);
	if (argc == 1)
		return TCL_OK;

	/*
	 *  If the first arg is "::", invoke the remaining args as a command
	 *  in the interpreter implementing class scope.  This provides
	 *  access to functions common to all objects in a class.
	 */
	c = *argv[1];
	if ((c == ':') && (strcmp(argv[1],"::") == 0))
	{
		if (argc < 2)
		{
			Tcl_AppendResult(interp, "wrong # args: should be \"",
				argv[0], " :: proc ?args?\"",
				(char*)NULL);
			return TCL_ERROR;
		}
		return Itcl_AccessClassScope(cdefn,interp,argc-2,argv+2);
	}

	/*
	 *  Otherwise, a new object instance is created.  If the first
	 *  arg is "#auto", the provide an automatic name for the object.
	 */
	if ((c == '#') && (strcmp(argv[1],"#auto") == 0))
	{
		do {
			sprintf(buffer,"%.200s%d", cdefn->name, cdefn->unique++);
		} while (Tcl_FindHashEntry(&cdefn->info->objects, buffer));
		objName = buffer;
	}
	else
		objName = argv[1];

	/*
	 *  Make sure that the object name is not already in use.
	 */
	if (Tcl_FindHashEntry(&cdefn->info->objects, objName))
	{
		Tcl_AppendResult(interp, "object already exists: ", objName,
			(char*)NULL);
		return TCL_ERROR;
	}

	/*
	 *  Create a new object and initialize its data.
	 */
	odefn = Itcl_CreateObject(objName,cdefn);

	/*
	 *  Add a command to the current interpreter with the object name.
	 *  This is done before invoking the constructor so that the
	 *  command can be used during construction to query info.
	 */
	Itcl_EventuallyFree((ClientData)odefn, Itcl_DeleteObject);
	Tcl_CreateCommand(interp, objName, Itcl_InstanceCmd,
		Itcl_PreserveData((ClientData)odefn), Itcl_ReleaseData);

	/*
	 *  Find all method definitions for constructors in superclasses.
	 *  Mark each constructor as not having been invoked.  After
	 *  the current class constructor has been executed, we will
	 *  go back and execute constructors that have still not been
	 *  invoked.
	 */
	Itcl_InitHierIter(&hier,cdefn);
	Itcl_AdvanceHierIter(&hier);

	while ((cd=Itcl_AdvanceHierIter(&hier)) != NULL)
	{
		entry = Tcl_FindHashEntry(&cd->allMethods,"constructor");
		mdefn = (entry)
			? (Itcl_Method*)Tcl_GetHashValue(entry)
			: (Itcl_Method*)NULL;

		realmd = mdefn;
		while (realmd && (realmd->type == ITCL_INHERITED_METHOD))
			realmd = realmd->values.actual;

		if (realmd) realmd->invoked = 0;
	}
	Itcl_DeleteHierIter(&hier);

	/*
	 *  Execute the constructor for the most specific class.
	 *  If construction is successful, return the object name.
	 *  Otherwise, return the error message.
	 */
	Itcl_PushStack( Itcl_PreserveData((ClientData)odefn),
		&cdefn->info->objStack);

	entry = Tcl_FindHashEntry(&cdefn->allMethods,"constructor");
	mdefn = (entry)
		? (Itcl_Method*)Tcl_GetHashValue(entry)
		: (Itcl_Method*)NULL;

	if (mdefn)
		status = Itcl_ExecMethod((ClientData)mdefn, interp, argc-1, argv+1);

	/*
	 *  If the constructor succeeded, then invoke any superclass
	 *  constructors that might have been missed.
	 */
	if (status == TCL_OK)
	{
		Itcl_InitHierIter(&hier,cdefn);
		Itcl_AdvanceHierIter(&hier);

		while ((cd=Itcl_AdvanceHierIter(&hier)) != NULL)
		{
			entry = Tcl_FindHashEntry(&cd->allMethods,"constructor");
			mdefn = (entry)
				? (Itcl_Method*)Tcl_GetHashValue(entry)
				: (Itcl_Method*)NULL;

			realmd = mdefn;
			while (realmd && (realmd->type == ITCL_INHERITED_METHOD))
				realmd = realmd->values.actual;

			if (realmd && !realmd->invoked)
			{
				status = Itcl_ExecMethod((ClientData)mdefn, interp,
					0, (char**)NULL);

				if (status != TCL_OK)
				{
					char msg[128];
					sprintf(msg,
						"\n    implicitly invoked by \"%.40s::constructor\"",
						cdefn->name);
					Tcl_AddErrorInfo(interp, msg);
					break;
				}
			}
		}
		Itcl_DeleteHierIter(&hier);
	}
	Itcl_ReleaseData( Itcl_PopStack(&cdefn->info->objStack) );

	/*
	 *  If the constructor failed, then unlink the object.
	 */
	if (status == TCL_OK)
		Tcl_SetResult(interp, objName, TCL_VOLATILE);
	else
		Tcl_DeleteCommand(interp, objName);

	return status;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_AccessClassScope()
 *
 *  Executes the given (argc,argv) as a command in the interpreter
 *  associated with a class.  Returns the result in the given
 *  interpreter.
 * ------------------------------------------------------------------------
 */
static int
Itcl_AccessClassScope(cdefn, interp, argc, argv)
	Itcl_Class *cdefn;       /* class definition */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	int status;
	char *cmd;
	Itcl_ExecScope exscope;

	/*
	 *  Make sure that the requested command is not a method.
	 *  Methods require object data and can only be invoked with
	 *  a specific object context.
	 */
	Tcl_SetResult(interp, "", TCL_STATIC);
	if (Tcl_FindHashEntry(&cdefn->allMethods,argv[0]))
	{
		Tcl_AppendResult(interp, "cannot invoke method \"", argv[0],
			"\" without object context",
			(char*)NULL);
		return TCL_ERROR;
	}

	/*
	 *  Execute the given command in the class interpreter and
	 *  return the result.  Since we must do a direct "eval", be
	 *  careful to set up the execution scope, so that the class
	 *  interp will have access to call frames in the calling interp.
	 */
	cmd = Tcl_Merge(argc,argv);

	Itcl_EnterExecScope(cdefn->interp, &exscope, cdefn->vscope, interp);
	status = Tcl_Eval(cdefn->interp, cmd);
	Itcl_XferResult(interp, cdefn->interp, status);
	Itcl_LeaveExecScope(cdefn->interp, &exscope);

	ckfree(cmd);
	return status;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_InstanceCmd()
 *
 *  Invoked by TCL whenever the user issues a command associated with
 *  an object instance.  Handles the following syntax:
 *
 *    <objName> <method> <args>...
 *
 * ------------------------------------------------------------------------
 */
static int
Itcl_InstanceCmd(clientData, interp, argc, argv)
	ClientData clientData;   /* object definition */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	Itcl_Object *odefn = (Itcl_Object*)clientData;
	Itcl_Class *cdefn = odefn->cdefn;

	Itcl_Method *mdefn;
	Tcl_HashEntry *entry;
	int status;

	Tcl_SetResult(interp, "", TCL_STATIC);
	if (argc < 2)
	{
		Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0], " method ?args?\"",
			(char*)NULL);
		return TCL_ERROR;
	}

	/*
	 *  Find the definition for the requested method.
	 *  Make sure the "info" proc is recognized as a method.
	 */
	entry = Tcl_FindHashEntry(&cdefn->allMethods,argv[1]);
	mdefn = (entry)
		? (Itcl_Method*)Tcl_GetHashValue(entry)
		: (Itcl_Method*)NULL;

	if (!mdefn)  /* try to recognize "info" as a method */
	{
		entry = Tcl_FindHashEntry(&cdefn->allProcs,argv[1]);
		mdefn = (entry)
			? (Itcl_Method*)Tcl_GetHashValue(entry)
			: (Itcl_Method*)NULL;

		if (mdefn && (strcmp(mdefn->name,"info") != 0))
			mdefn = NULL;
	}

	if (mdefn)
	{
		/*
		 *  If this is the last object in an orphan class, the
		 *  "delete" method will cause the object to die and the
		 *  class definition to go with it.  Protect the class
		 *  definition while the method is being executed.
		 */
		Itcl_PreserveData((ClientData)cdefn);

		/*
		 *  Execute method locally in class interpreter.
		 */
		Itcl_PushStack( Itcl_PreserveData((ClientData)odefn),
			&cdefn->info->objStack);

		status = Itcl_ExecMethod((ClientData)mdefn, interp, argc-1, argv+1);

		Itcl_ReleaseData( Itcl_PopStack(&cdefn->info->objStack) );
		Itcl_ReleaseData((ClientData)cdefn);

		return status;
	}

	Tcl_AppendResult(interp, "unknown method \"", argv[1],
		"\" invoked for object \"", argv[0], "\"",
		(char*)NULL);
	return TCL_ERROR;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_InfoCmd()
 *
 *  Invoked by TCL whenever the user issues a "itcl_info" command to
 *  query information about existing classes or objects.  Handles the
 *  following syntax:
 *
 *    itcl_info classes ?<pattern>?
 *    itcl_info objects ?<pattern>? ?-class <className>? ?-isa <className>?
 *
 * ------------------------------------------------------------------------
 */
static int
Itcl_InfoCmd(clientData, interp, argc, argv)
	ClientData clientData;   /* info for all known classes */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	Itcl_ClassInfo *info = (Itcl_ClassInfo*)clientData;
	char *pattern = NULL;
	char *className = NULL;
	char *isaName = NULL;

	char c, *name;
	int length;
	Tcl_HashSearch pos;
	Tcl_HashEntry *entry;
	Itcl_Class *cdefn;
	Itcl_Object *odefn;
	Itcl_HierIter hier;

	Tcl_SetResult(interp, "", TCL_STATIC);
	if (argc < 2)
	{
		Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0], " option ?args...?\"",
			(char*)NULL);
		return TCL_ERROR;
	}
	c = argv[1][0];
	length = strlen(argv[1]);

	/*
	 *  HANDLE:  itcl_info classes ?<pattern>?
	 */
	if ((c == 'c') && strncmp(argv[1], "classes", length) == 0)
	{
		if (argc == 3)
			pattern = argv[2];
		else if (argc > 3)
		{
			Tcl_AppendResult(interp, "wrong # args: should be \"",
				argv[0], " classes ?pattern?\"",
				(char*)NULL);
			return TCL_ERROR;
		}

		/*
		 *  Return a list of classes matching the given pattern.
		 */
		entry = Tcl_FirstHashEntry(&info->classes,&pos);
		while (entry)
		{
			name = Tcl_GetHashKey(&info->classes,entry);
			if (!pattern || Tcl_StringMatch(name,pattern))
				Tcl_AppendElement(interp, name);

			entry = Tcl_NextHashEntry(&pos);
		}
		return TCL_OK;
	}

	/*
	 *  HANDLE:  itcl_info objects ?<pattern>? ?-class <className>?
	 *               ?-isa <className>?
	 */
	else if ((c == 'o') && strncmp(argv[1], "objects", length) == 0)
	{
		int ac = argc-2;
		char **av = argv+2;

		if ((ac > 0) && (**av != '-'))
		{
			pattern = *av;
			av++; ac--;
		}
		while (ac > 0)
		{
			if ((ac == 2) && (strcmp(*av,"-class") == 0))
				className = av[1];
			else if ((ac == 2) && (strcmp(*av,"-isa") == 0))
				isaName = av[1];
			else
			{
				Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
					" objects ?pattern? ?-class className? ?-isa className?\"",
					(char*)NULL);
				return TCL_ERROR;
			}
			ac -= 2;  /* parsed two args above */
		}

		/*
		 *  Return a list of objects matching the given pattern.
		 */
		entry = Tcl_FirstHashEntry(&info->objects,&pos);
		while (entry)
		{
			name = Tcl_GetHashKey(&info->objects,entry);
			odefn = (Itcl_Object*)Tcl_GetHashValue(entry);

			if (!pattern || Tcl_StringMatch(name,pattern))
			{
				if (!className || (strcmp(odefn->cdefn->name,className) == 0))
				{
					Itcl_InitHierIter(&hier,odefn->cdefn);
					while ((cdefn=Itcl_AdvanceHierIter(&hier)) != NULL)
						if (!isaName || (strcmp(cdefn->name,isaName) == 0))
							break;
					Itcl_DeleteHierIter(&hier);

					if (cdefn)
						Tcl_AppendElement(interp, name);
				}
			}
			entry = Tcl_NextHashEntry(&pos);
		}
		return TCL_OK;
	}

	/*
	 *  Flag improper usage.
	 */
	Tcl_AppendResult(interp, "bad option \"", argv[1],
		"\": must be classes or objects",
		(char*)NULL);
	return TCL_ERROR;
}
