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
 *     RCS:  itcl_methods.c,v 1.2 1994/03/25 19:01:13 mmc Exp
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
#include <memory.h>

#include "itcl_util.h"
#include "itcl_methods.h"
#include "itcl_class.h"
#include "itcl_objects.h"
#include "itcl_core.h"

/*
 *  FORWARD DECLARATIONS
 */
static int Itcl_ParseConfig _ANSI_ARGS_((Tcl_Interp *interp,
	int argc, char **argv, Itcl_Object *odefn,
	int *rargc, Itcl_Variable ***rvars, char ***rvals));

static int Itcl_HandleConfig _ANSI_ARGS_((Tcl_Interp *interp,
	int argc, Itcl_Variable **vars, char **vals, Itcl_Object *odefn));

#ifndef lint
static char sccsid[] = "@(#)itcl_methods.c	1.23 (10/25/93) Michael J. McLennan";
#endif


/*
 * ------------------------------------------------------------------------
 *  Itcl_CreateMethod()
 *
 *  Creates a new class method definition and returns a pointer to it.
 * ------------------------------------------------------------------------
 */
Itcl_Method*
Itcl_CreateMethod(cdefn,name,arglist,body)
	Itcl_Class *cdefn;   /* interpreter handling class */
	char* name;          /* name of new method */
	char* arglist;       /* space-separated list of arg names */
	char* body;          /* TCL statements forming body of method */
{
	Itcl_Method *mdefn;

	mdefn = (Itcl_Method*)ckalloc(sizeof(Itcl_Method));
	mdefn->cdefn = cdefn;

	mdefn->name = (char*)ckalloc((unsigned)(strlen(name)+1));
	strcpy(mdefn->name, name);

	mdefn->type = ITCL_TCL_METHOD;
	mdefn->values.body = (char*)ckalloc((unsigned)(strlen(body)+1));
	strcpy(mdefn->values.body, body);

	mdefn->arglist = NULL;
	mdefn->invoked = 0;

	if (arglist)
	{
		int argc;
		char **argv;
		if (Tcl_SplitList(cdefn->interp,arglist,&argc,&argv) != TCL_OK)
		{
			Itcl_DeleteMethod(mdefn);
			return (Itcl_Method*)NULL;
		}
		else
		{
			int i, fargc;
			char **fargv, *concat;
			Arg *newarg;

			for (i=argc-1; i >= 0; i--)
			{
				if (Tcl_SplitList(cdefn->interp,argv[i],&fargc,&fargv)
				    != TCL_OK)
				{
					Itcl_DeleteMethod(mdefn);
					return (Itcl_Method*)NULL;
				}
				else
				{
					switch (fargc)
					{
					case 0:     /* empty argument (ignore) */
						newarg = NULL;
						break;

					case 1:     /* just arg name */
						newarg = Itcl_CreateArg(fargv[0],(char*)NULL);
						break;

					case 2:     /* arg name/value */
						newarg = Itcl_CreateArg(fargv[0],fargv[1]);
						break;

					default:    /* arg name/values */
						concat = Tcl_Merge(fargc-1,fargv+1);
						newarg = Itcl_CreateArg(fargv[0],concat);
						ckfree(concat);
						break;
					}
					if (newarg)  /* attach new arg to arglist */
					{
						newarg->nextPtr = mdefn->arglist;
						mdefn->arglist = newarg;
					}
				}
				ckfree((char*)fargv);
			}
		}
		ckfree((char*)argv);
	}
	return mdefn;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_CreateBiMethod()
 *
 *  Creates a new class method definition and returns a pointer to it.
 * ------------------------------------------------------------------------
 */
Itcl_Method*
Itcl_CreateBiMethod(cdefn,name,proc)
	Itcl_Class *cdefn;      /* class containing this method */
	char* name;             /* name of new method */
	Itcl_MethodProc *proc;  /* built-in proc handling method */
{
	Itcl_Method *mdefn;

	mdefn = (Itcl_Method*)ckalloc(sizeof(Itcl_Method));
	mdefn->cdefn = cdefn;

	mdefn->name = (char*)ckalloc((unsigned)(strlen(name)+1));
	strcpy(mdefn->name, name);

	mdefn->type = ITCL_BUILTIN_METHOD;
	mdefn->values.builtin = proc;

	mdefn->arglist = NULL;
	mdefn->invoked = 0;

	return mdefn;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_CreateRefMethod()
 *
 *  Creates a new class method definition for an inherited method
 *  and returns a pointer to it.
 * ------------------------------------------------------------------------
 */
Itcl_Method*
Itcl_CreateRefMethod(cdefn,mdefn)
	Itcl_Class *cdefn;    /* class containing this method */
	Itcl_Method *mdefn;   /* inherited method */
{
	Itcl_Method *md;

	md = (Itcl_Method*)ckalloc(sizeof(Itcl_Method));
	md->cdefn = cdefn;

	md->name = (char*)ckalloc((unsigned)(strlen(mdefn->name)+1));
	strcpy(md->name, mdefn->name);

	md->type = ITCL_INHERITED_METHOD;
	md->values.actual = mdefn;

	md->arglist = NULL;
	md->invoked = 0;

	return md;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_DeleteMethod()
 *
 *  Destroys the given class method, freeing all resources associated
 *  with it.
 * ------------------------------------------------------------------------
 */
void
Itcl_DeleteMethod(mdefn)
	Itcl_Method *mdefn;  /* pointer to method definition to be destroyed */
{
	Tcl_HashEntry *mEntry;
	Arg *arg, *next;

	if (mdefn)
	{
		if (mdefn->cdefn)
		{
			mEntry = Tcl_FindHashEntry(&mdefn->cdefn->methods,mdefn->name);
			if (mEntry) Tcl_DeleteHashEntry(mEntry);

			mEntry = Tcl_FindHashEntry(&mdefn->cdefn->procs,mdefn->name);
			if (mEntry) Tcl_DeleteHashEntry(mEntry);
		}

		for (arg=mdefn->arglist; arg; arg=next)
		{
			next = arg->nextPtr;
			ckfree((char*)arg);
		}
		ckfree(mdefn->name);

		if (mdefn->type == ITCL_TCL_METHOD)
			ckfree(mdefn->values.body);

		ckfree((char*)mdefn);
	}
}


/*
 * ------------------------------------------------------------------------
 *  Itcl_CreateArg()
 *
 *  Creates a new TCL "Arg" structure and fills it with the given
 *  information.  Returns a pointer to the new "Arg" structure.
 * ------------------------------------------------------------------------
 */
Arg*
Itcl_CreateArg(name,init)
	char* name;     /* name of new argument */
	char* init;     /* initial value */
{
	Arg *newarg = NULL;
	int nameLen, valueLen;
	unsigned len;

	nameLen  = (name) ? strlen(name)+1 : 0;
	valueLen = (init) ? strlen(init)+1 : 0;
	len = sizeof(Arg)-sizeof(newarg->name) + nameLen + valueLen;
	newarg = (Arg*)ckalloc(len);

	strcpy(newarg->name, name);

	if (init)
	{
		newarg->defValue = newarg->name + nameLen;
		strcpy(newarg->defValue, init);
	}
	else
		newarg->defValue = NULL;

	return newarg;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_ArgList()
 *
 *  Returns a string representation for the given argument list.
 * ------------------------------------------------------------------------
 */
char*
Itcl_ArgList(arg)
	Arg* arg;     /* first argument in arglist */
{
	int listlen = 0;
	int listmax = 10;
	char **list;

	int i;
	char *elem, *argv[2];

	list = (char**)ckalloc((unsigned)(listmax*sizeof(char*)));
	while (arg)
	{
		argv[0] = arg->name;
		argv[1] = (arg->defValue) ? arg->defValue : "";
		elem = Tcl_Concat(2,argv);
		list[listlen++] = elem;

		if (listlen >= listmax)
		{
			char **newlist;
			listmax *= 2;
			newlist = (char**)ckalloc((unsigned)(listmax*sizeof(char*)));
			memcpy((char*)newlist, (char*)list, listlen*sizeof(char*));
			ckfree((char*)list);
			list = newlist;
		}
		arg = arg->nextPtr;
	}
	elem = Tcl_Merge(listlen,list);

	for (i=0; i < listlen; i++)  /* free workspace */
		ckfree(list[i]);
	ckfree((char*)list);

	return elem;
}


/*
 * ------------------------------------------------------------------------
 *  Itcl_ExecMethod()
 *
 *  Invoked by TCL to handle the execution of a user-defined method.
 *  A method is similar to the usual TCL proc, but has access to
 *  public and protected data within its class and all superclasses.
 *
 *  In order to handle class scoping properly, the method should
 *  actually be executed in the interpreter associated with the class
 *  definition containing the method; however, the result must be copied
 *  to the calling interpreter or it would be lost.
 * ------------------------------------------------------------------------
 */
int
Itcl_ExecMethod(clientData, interp, argc, argv)
	ClientData clientData;   /* method definition */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	Itcl_Method *mdefn = (Itcl_Method*)clientData;
	Itcl_Class *cview  = mdefn->cdefn;
	Itcl_Object *odefn = (Itcl_Object*)Itcl_PeekStack(&cview->info->objStack);

	int result;
	char **args, *value;
	Arg *argm;
	Itcl_VarScope *vscope;
	Itcl_Class *mview;
	Itcl_ExecScope exscope;

	int defargc;
	char **defargv = NULL;
	int configc = 0;
	Itcl_Variable **configVars = NULL;
	char **configVals = NULL;

	/*
	 *  If this method was inherited, then find the real method
	 *  definition.
	 */
	while (mdefn->type == ITCL_INHERITED_METHOD)
		mdefn = mdefn->values.actual;

	mview = mdefn->cdefn;   /* scope that method was defined in */
	mdefn->invoked = ~0;    /* method has been invoked at least once */

	/*
	 *  Install the object's call frame for the new method invocation.
	 */
	for (vscope=odefn->vscopes; vscope; vscope=vscope->next)
		if (vscope->cdefn == mview)
			break;

	Itcl_EnterExecScope(mview->interp, &exscope, vscope, interp);
	exscope.frame.argc = argc;
	exscope.frame.argv = argv;

	Tcl_SetResult(interp, "", TCL_STATIC);

	/*
	 *  If the method is "builtin" then simply invoke it with the
	 *  given arguments.
	 */
	if (mdefn->type == ITCL_BUILTIN_METHOD)
	{
		result = (*mdefn->values.builtin)((ClientData)mview,
			mview->interp, argc, argv);

		Itcl_XferResult(interp, mview->interp, result);
		goto methodDone;
	}

	/*
	 *  Otherwise, interpret the body of the method as TCL code.
	 *  Match the actual arguments against the procedure's formal
	 *  parameters to compute local variables.
	 */
	for (argm=mdefn->arglist, args=argv+1, argc--;
		argm != NULL;
		argm=argm->nextPtr, args++, argc--)
	{
		/*
		 *  Handle the special case of the last formal being "args".
		 *  When it occurs, assign it a list consisting of all the
		 *  remaining actual arguments.
		 */
		if ((argm->nextPtr == NULL) &&
		    (strcmp(argm->name, "args") == 0))
		{
			if (argc < 0) argc = 0;
			value = Tcl_Merge(argc, args);
			Itcl_SetArgVar(mview->interp, argm->name, value);
			ckfree(value);
			argc = 0;
			break;
		}

		/*
		 *  Handle the special case of the last formal being "config".
		 *  When it occurs, treat all remaining arguments as public
		 *  variable assignments.  Set the local "config" variable
		 *  to the list of public variables assigned.
		 */
		else if ((argm->nextPtr == NULL) &&
		         (strcmp(argm->name, "config") == 0))
		{
			if (argc > 0)  /* still have some arguments left? */
			{
				int vi;
				char **vlist;

				result = Itcl_ParseConfig(mview->interp, argc,args,
					odefn, &configc,&configVars,&configVals);

				if (result != TCL_OK)
				{
					Itcl_XferResult(interp, mview->interp, result);
					goto methodDone;
				}

				vlist = (char**)ckalloc((unsigned)(configc*sizeof(char*)));
				for (vi=0; vi < configc; vi++)
					vlist[vi] = configVars[vi]->fullname;

				value = Tcl_Merge(configc, vlist);
				Itcl_SetArgVar(mview->interp, "config", value);
				ckfree((char*)value);
				ckfree((char*)vlist);

				argc = 0;  /* all remaining args handled */
			}
			else if (argm->defValue)  /* otherwise, handle default values */
			{
				int vi;
				char **vlist;

				Tcl_SplitList(interp, argm->defValue, &defargc,&defargv);

				result = Itcl_ParseConfig(mview->interp, defargc,defargv,
					odefn, &configc,&configVars,&configVals);

				if (result != TCL_OK)
				{
					Itcl_XferResult(interp, mview->interp, result);
					goto methodDone;
				}

				vlist = (char**)ckalloc((unsigned)(configc*sizeof(char*)));
				for (vi=0; vi < configc; vi++)
					vlist[vi] = configVars[vi]->fullname;

				value = Tcl_Merge(configc, vlist);
				Itcl_SetArgVar(mview->interp, "config", value);
				ckfree((char*)value);
				ckfree((char*)vlist);
			}
			else
				Itcl_SetArgVar(mview->interp, "config", "");
		}

		/*
		 *  Resume the usual processing of arguments...
		 */
		else if (argc > 0)        /* take next arg as value */
		{
			value = *args;
			Itcl_SetArgVar(mview->interp, argm->name, value);
		}
		else if (argm->defValue)  /* ...or use default value */
		{
			value = argm->defValue;
			Itcl_SetArgVar(mview->interp, argm->name, value);
		}
		else
		{
			Tcl_AppendResult(interp, "no value given for parameter \"",
				argm->name, "\" in method \"",
				mdefn->cdefn->name, "::", mdefn->name, "\"",
				(char*)NULL);
			result = TCL_ERROR;
			goto methodDone;
		}
	}
	if (argc > 0)
	{
		Tcl_AppendResult(interp, "called method \"",
			mdefn->cdefn->name, "::", mdefn->name,
			"\" with too many arguments", (char*)NULL);
		result = TCL_ERROR;
		goto methodDone;
	}

	/*
	 *  Handle any "config" assignments.
	 */
	if (configc > 0)
	{
		result = Itcl_HandleConfig(mview->interp,
			configc, configVars, configVals, odefn);

		if (result != TCL_OK)
		{
			Itcl_XferResult(interp, mview->interp, result);
			goto methodDone;
		}
	}

	/*
	 *  Invoke the commands in the procedure's body.
	 */
	result = Tcl_Eval(mview->interp, mdefn->values.body);

	Itcl_XferResult(interp, mview->interp, result);

	if (result == TCL_RETURN)
		result = TCL_OK;
	else if (result == TCL_ERROR)
	{
		char msg[160];
		sprintf(msg,
			"\n    (object \"%.40s\" method \"%.40s::%.40s\" body line %d)",
			odefn->name, cview->name, mdefn->name, mview->interp->errorLine);
		Tcl_AddErrorInfo(interp, msg);
	}
	else if (result == TCL_BREAK)
	{
		Tcl_SetResult(interp,
			"invoked \"break\" outside of a loop", TCL_STATIC);
		result = TCL_ERROR;
	}
	else if (result == TCL_CONTINUE)
	{
		Tcl_SetResult(interp,
			"invoked \"continue\" outside of a loop", TCL_STATIC);
		result = TCL_ERROR;
	}

	/*
	 *  Remove the call frame from the interpreter and return.
	 */
methodDone:
	if (defargv)    ckfree((char*)defargv);
	if (configVars) ckfree((char*)configVars);
	if (configVals) ckfree((char*)configVals);
	Itcl_LeaveExecScope(mview->interp, &exscope);

	return result;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_ParseConfig()
 *
 *  Parses a set of arguments as "-variable value" assignments.  Checks
 *  to make sure that variable names are recognized in the most specific
 *  class scope, so that an inherited method with a "config" parameter
 *  will work correctly. Returns a list of public variable names and their
 *  corresponding values; both lists should passed to Itcl_HandleConfig()
 *  to perform assignments, and freed when no longer in use.  Returns a
 *  status TCL_OK/TCL_ERROR and returns error messages in the interpreter.
 * ------------------------------------------------------------------------
 */
static int
Itcl_ParseConfig(interp,argc,argv,odefn,rargc,rvars,rvals)
	Tcl_Interp *interp;      /* interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
	Itcl_Object *odefn;      /* object whose public vars are being config'd */
	int *rargc;              /* return: number of variables accessed */
	Itcl_Variable ***rvars;  /* return: list of variables */
	char ***rvals;           /* return: list of values */
{
	int result = TCL_OK;

	char cname[85],vname[85];
	Itcl_Variable *vdefn;

	if (argc < 0) argc = 0;
	*rargc = 0;
	*rvars = (Itcl_Variable**)ckalloc((unsigned)(argc*sizeof(Itcl_Variable*)));
	*rvals = (char**)ckalloc((unsigned)(argc*sizeof(char*)));

	Tcl_SetResult(interp, "", TCL_STATIC);
	while (argc-- > 0)
	{
		/*
		 *  Next argument should be "-variable"
		 */
		if (**argv != '-')
		{
			Tcl_AppendResult(interp, "syntax error in config assignment \"",
				*argv, "\": should be \"-variable value\"",
				(char*)NULL);
			result = TCL_ERROR;
			break;
		}
		else if (argc-- <= 0)
		{
			Tcl_AppendResult(interp, "syntax error in config assignment \"",
				*argv, "\": should be \"-variable value\" (missing value)",
				(char*)NULL);
			result = TCL_ERROR;
			break;
		}

		Itcl_ParseSlotName((*argv)+1, cname, vname, sizeof(cname));
		if ((vdefn=Itcl_FindPublicVar(cname,vname,odefn->cdefn)) == NULL)
		{
			Tcl_AppendResult(interp,
				"syntax error in config assignment \"",
				*argv, "\": unrecognized variable",
				(char*)NULL);
			result = TCL_ERROR;
			break;
		}
		else
		{
			(*rvars)[*rargc] = vdefn;      /* variable definition */
			(*rvals)[*rargc] = *(argv+1);  /* config value */
			(*rargc)++;
			argv += 2;
		}
	}
	return result;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_HandleConfig()
 *
 *  Handles the assignment of "config" values to public variables.
 *  The list of assignments is parsed in Itcl_ParseConfig(), but the
 *  actual assignments are performed here.  If the variables have any
 *  associated "config" code, it is invoked here as well.  If errors
 *  are detected during assignment or "config" code execution, the
 *  variable is set back to its previous value and an error is returned.
 *
 *  Returns a status TCL_OK/TCL_ERROR, and returns any error messages
 *  in the given interpreter.
 * ------------------------------------------------------------------------
 */
static int
Itcl_HandleConfig(interp,argc,vars,vals,odefn)
	Tcl_Interp *interp;      /* interpreter currently in control */
	int argc;                /* number of assignments */
	Itcl_Variable **vars;    /* list of public variable definitions */
	char **vals;             /* list of public variable values */
	Itcl_Object *odefn;      /* object whose public vars are being config'd */
{
	int result = TCL_OK;
	char *lastval = NULL;
	Itcl_Class *cdefn = odefn->cdefn;

	int i;
	char *val;
	Itcl_VarScope *vscope;
	Itcl_ExecScope exscope;

	/*
	 *  Transfer execution scope from the given interpreter to the
	 *  class scope for "config" processing.  For inherited methods,
	 *  these two scopes will be different.  The method will be
	 *  defined in a base class, but it could be invoked from a
	 *  derived class which will have an expanded set of variables
	 *  available for configuration.
	 */
	vscope = odefn->vscopes;  /* most-specific scope */
	Itcl_EnterExecScope(cdefn->interp, &exscope, vscope, interp);

	Tcl_SetResult(cdefn->interp, "", TCL_STATIC);
	for (i=0; i < argc; i++)
	{
		val = Tcl_GetVar2(cdefn->interp, vars[i]->fullname, (char*)NULL, 0);
		if (!val) val = "";

		lastval = (char*)ckalloc((unsigned)(strlen(val)+1));
		strcpy(lastval,val);

		/*
		 *  Set the variable to the specified value.
		 */
		if (!Tcl_SetVar2(cdefn->interp, vars[i]->fullname, (char*)NULL,
		     vals[i], 0))
		{
			char msg[160];
			sprintf(msg,
				"\n    (error in config for public variable \"%.100s\")",
				vars[i]->fullname);
			Tcl_AddErrorInfo(cdefn->interp, msg);
			result = TCL_ERROR;
			break;
		}

		/*
		 *  If the variable has a "config" condition, then
		 *  execute it in the scope of its associated class.
		 *  If it fails, return an error immediately.
		 */
		if (vars[i]->config)
		{
			Itcl_Class *cd = vars[i]->cdefn;
			Itcl_VarScope *vsc;
			Itcl_ExecScope exsc;
			int status;

			for (vsc=odefn->vscopes; vsc; vsc=vsc->next)
				if (vsc->cdefn == cd)
					break;

			Itcl_EnterExecScope(cd->interp, &exsc, vsc, cdefn->interp);
			Tcl_SetResult(cd->interp, "", TCL_STATIC);
			status = Tcl_Eval(cd->interp, vars[i]->config);
			Itcl_LeaveExecScope(cd->interp, &exsc);

			if (status != TCL_OK)
			{
				char msg[160];

				Itcl_XferResult(cdefn->interp, cd->interp, status);
				sprintf(msg,
					"\n    (error in config for public variable \"%.100s\")",
					vars[i]->fullname);
				Tcl_AddErrorInfo(cdefn->interp, msg);
				Tcl_SetVar2(cdefn->interp, vars[i]->fullname, (char*)NULL,
					lastval, 0);
				result = TCL_ERROR;
				break;
			}
		}
		if (lastval) { ckfree(lastval); lastval = NULL; }
	}

	/*
	 *  Clean up and return.
	 */
	if (lastval) { ckfree(lastval); lastval = NULL; }
	Itcl_LeaveExecScope(cdefn->interp, &exscope);

	if (result != TCL_OK)
		Itcl_XferResult(interp, cdefn->interp, result);

	return result;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_ExecProc()
 *
 *  Invoked by TCL to handle the execution of a user-defined proc.
 *  A class proc is similar to the usual TCL proc, but has access to
 *  common data within its class and all superclasses.  A class proc
 *  is also similar to a method, but does not have access to public
 *  and protected variables, which are specific to a particular object.
 *
 *  In order to handle class scoping properly, the method should
 *  actually be executed in the interpreter associated with the class
 *  definition containing the method; however, the result must be copied
 *  to the calling interpreter or it would be lost.
 * ------------------------------------------------------------------------
 */
int
Itcl_ExecProc(clientData, interp, argc, argv)
	ClientData clientData;   /* method definition */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	Itcl_Method *mdefn = (Itcl_Method*)clientData;
	Itcl_Class *cview  = mdefn->cdefn;

	int result;
	char **args, *value;
	Arg *argm;
	Itcl_Class *mview;
	Itcl_ExecScope exscope;

	/*
	 *  If this method was inherited, then find the real method
	 *  definition.
	 */
	while (mdefn->type == ITCL_INHERITED_METHOD)
		mdefn = mdefn->values.actual;

	mview = mdefn->cdefn;   /* scope that proc was defined in */
	mdefn->invoked = ~0;    /* proc has been invoked at least once */

	/*
	 *  Install the class's call frame for the proc invocation.
	 */
	Itcl_EnterExecScope(mview->interp, &exscope, mview->vscope, interp);
	exscope.frame.argc = argc;
	exscope.frame.argv = argv;

	Tcl_SetResult(interp, "", TCL_STATIC);

	/*
	 *  If the proc is "builtin" then simply invoke it with the
	 *  given arguments.
	 */
	if (mdefn->type == ITCL_BUILTIN_METHOD)
	{
		result = (*mdefn->values.builtin)((ClientData)mview,
			mview->interp, argc, argv);

		Itcl_XferResult(interp, mview->interp, result);
		goto procDone;
	}

	/*
	 *  Otherwise, interpret the body of the proc as TCL code.
	 *  Match the actual arguments against the procedure's formal
	 *  parameters to compute local variables.
	 */
	for (argm=mdefn->arglist, args=argv+1, argc--;
		argm != NULL;
		argm=argm->nextPtr, args++, argc--)
	{
		/*
		 *  Handle the special case of the last formal being "args".
		 *  When it occurs, assign it a list consisting of all the
		 *  remaining actual arguments.
		 */
		if ((argm->nextPtr == NULL) &&
		    (strcmp(argm->name, "args") == 0))
		{
			if (argc < 0) argc = 0;
			value = Tcl_Merge(argc, args);
			Itcl_SetArgVar(mview->interp, argm->name, value);
			ckfree(value);
			argc = 0;
			break;
		}

		else if (argc > 0)        /* take next arg as value */
			value = *args;

		else if (argm->defValue)  /* ...or use default value */
			value = argm->defValue;

		else
		{
			Tcl_AppendResult(interp, "no value given for parameter \"",
				argm->name, "\" in proc \"",
				mdefn->cdefn->name, "::", mdefn->name, "\"",
				(char*)NULL);
			result = TCL_ERROR;
			goto procDone;
		}
		Itcl_SetArgVar(mview->interp, argm->name, value);
	}
	if (argc > 0)
	{
		Tcl_AppendResult(interp, "called proc \"",
			mdefn->cdefn->name, "::", mdefn->name,
			"\" with too many arguments", (char*)NULL);
		result = TCL_ERROR;
		goto procDone;
	}

	/*
	 *  Invoke the commands in the procedure's body.
	 */
	result = Tcl_Eval(mview->interp, mdefn->values.body);

	Itcl_XferResult(interp, mview->interp, result);

	if (result == TCL_RETURN)
		result = TCL_OK;
	else if (result == TCL_ERROR)
	{
		char msg[160];
		sprintf(msg, "\n    (proc \"%.40s::%.40s\" body line %d)",
			cview->name, mdefn->name, mview->interp->errorLine);
		Tcl_AddErrorInfo(interp, msg);
	}
	else if (result == TCL_BREAK)
	{
		Tcl_SetResult(interp,
			"invoked \"break\" outside of a loop", TCL_STATIC);
		result = TCL_ERROR;
	}
	else if (result == TCL_CONTINUE)
	{
		Tcl_SetResult(interp,
			"invoked \"continue\" outside of a loop", TCL_STATIC);
		result = TCL_ERROR;
	}

	/*
	 *  Remove the call frame from the interpreter and return.
	 */
procDone:
	Itcl_LeaveExecScope(mview->interp, &exscope);
	return result;
}


/*
 * ------------------------------------------------------------------------
 *  Itcl_UnknownCmd()
 *
 *  Installed into the interpreter associated with a class, and invoked
 *  whenever a command is "unknown" within a class.  An attempt is made
 *  to invoke the command in the interpreter for the superclass, or in
 *  the outside interpreter if this is a top-level class.
 *
 *    unknown <command> <args>...
 *
 * ------------------------------------------------------------------------
 */
int
Itcl_UnknownCmd(clientData, interp, argc, argv)
	ClientData clientData;   /* class definition */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	Itcl_Class *cdefn = (Itcl_Class*)clientData;
	Itcl_Class *super = NULL;

	int status;
	char *concat, *cmd;
	Itcl_HierIter hier;
	Itcl_ExecScope exscope;
	Tcl_CmdInfo cmdInfo;

	Tcl_SetResult(interp, "", TCL_STATIC);
	if (argc < 2)
	{
		Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0], " command ?args?\"",
			(char*)NULL);
		return TCL_ERROR;
	}

	/*
	 *  Search the class derivation hierarchy for a superclass that
	 *  recognizes the command.  If one is found, then invoke the
	 *  command in that superclass.
	 */
	concat = Tcl_Merge(argc-1,argv+1);
	if (cdefn->supers)
	{
		Itcl_InitHierIter(&hier,cdefn);
		Itcl_AdvanceHierIter(&hier);

		while ((super=Itcl_AdvanceHierIter(&hier)) != NULL)
			if (Tcl_GetCommandInfo(super->interp, argv[1], &cmdInfo))
				break;

		Itcl_DeleteHierIter(&hier);
	}

	if (super)
	{
		cmd  = concat;
		status = Tcl_Eval(super->interp, cmd);
		Itcl_XferResult(interp, super->interp, status);
	}

	/*
	 *  Otherwise, try to invoke the command in the outside interpreter,
	 *  stripping off any "::" prefix at the global level.  Be careful
	 *  to switch execution scope back to outside interpreter, so that
	 *  it will have access to the existing call frame stack.
	 */
	else
	{
		Tcl_Interp* next = cdefn->info->main;
		Itcl_EnterExecScope(next, &exscope, (Itcl_VarScope*)NULL, interp);

		cmd = (strncmp(concat,"::",2) == 0) ? concat+2 : concat;
		status = Tcl_Eval(next, cmd);
		Itcl_XferResult(interp, next, status);

		Itcl_LeaveExecScope(next, &exscope);
	}
	ckfree(concat);
	return status;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_PreviousCmd()
 *
 *  Installed into the interpreter associated with a class, and invoked
 *  to access commands in the "previous" scope.  This is very similar
 *  to what happens in the "unknown" command, except that the user
 *  is deliberately requesting the change in scope.  If the current
 *  class has no superclass, an error is returned.
 *
 *    previous <command> <args>...
 *
 * ------------------------------------------------------------------------
 */
int
Itcl_PreviousCmd(clientData, interp, argc, argv)
	ClientData clientData;   /* class definition */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	Itcl_Class *cdefn = (Itcl_Class*)clientData;

	Tcl_SetResult(interp, "", TCL_STATIC);
	if (argc < 2)
	{
		Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0], " command ?args?\"",
			(char*)NULL);
		return TCL_ERROR;
	}

	/*
	 *  If there is no superclass, return an error.
	 */
	if (!cdefn->supers)
	{
		Tcl_AppendResult(interp,
			"no previous class in inheritance hierarchy for \"",
			cdefn->name, "\"",
			(char*)NULL);
		return TCL_ERROR;
	}

	/*
	 *  Otherwise, invoke the command in the previous scope.
	 */
	return Itcl_UnknownCmd(clientData, interp, argc, argv);
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_VirtualCmd()
 *
 *  Installed into the interpreter associated with a class, and invoked
 *  to access commands in the "virtual" (most-specific) scope.
 *
 *    virtual <command> <args>...
 *
 * ------------------------------------------------------------------------
 */
int
Itcl_VirtualCmd(clientData, interp, argc, argv)
	ClientData clientData;   /* class definition */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	Itcl_Class *cdefn = (Itcl_Class*)clientData;
	Itcl_Object *odefn = (Itcl_Object*)Itcl_PeekStack(&cdefn->info->objStack);

	int status;
	char *concat;
	Itcl_VarScope *vscope = NULL;
	Itcl_ExecScope exscope;

	Tcl_SetResult(interp, "", TCL_STATIC);
	if (argc < 2)
	{
		Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0], " command ?args?\"",
			(char*)NULL);
		return TCL_ERROR;
	}

	if (!odefn)
	{
		Tcl_SetResult(interp, "virtual commands only make sense within the context of a specific object", TCL_STATIC);
		return TCL_ERROR;
	}

	/*
	 *  Execute in the scope of the most-specific class for the
	 *  current object.
	 */
	cdefn = odefn->cdefn;
	for (vscope=odefn->vscopes; vscope; vscope=vscope->next)
		if (vscope->cdefn == cdefn)
			break;

	Itcl_EnterExecScope(cdefn->interp, &exscope, vscope, interp);
	concat = Tcl_Merge(argc-1,argv+1);
	status = Tcl_Eval(cdefn->interp, concat);

	ckfree(concat);
	Itcl_XferResult(interp, cdefn->interp, status);
	Itcl_LeaveExecScope(cdefn->interp, &exscope);

	return status;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_UplevelCmd()
 *
 *  Replaces the usual Tcl "uplevel" command.  Nearly identical to the
 *  usual implementation, but has [incr Tcl] info as client data, and
 *  is therefore aware of all [incr Tcl] interpreters.  Determines the
 *  interpreter that contains a particular call frame, and therefore
 *  executes the "uplevel" command in the proper interpreter.
 * ------------------------------------------------------------------------
 */
int
Itcl_UplevelCmd(clientData, interp, argc, argv)
	ClientData clientData;   /* info for all classes */
	Tcl_Interp *interp;      /* current interpreter */
	int argc;                /* number of arguments */
	char **argv;             /* argument strings */
{
	Itcl_ClassInfo *info = (Itcl_ClassInfo*)clientData;

	int result;
	Itcl_Class *cdefn;
	Itcl_ExecScope exscope;

	CallFrame *framePtr, *fp, *savedVarFramePtr;
	Tcl_Interp *ex_interp;
	Interp *iPtr, *ex_iPtr;
	Tcl_HashSearch place;
	Tcl_HashEntry *entry;

	Tcl_SetResult(interp, "", TCL_STATIC);
	if (argc < 2)
	{
		Tcl_AppendResult(interp, "wrong # args: should be \"",
			argv[0], " ?level? command ?arg ...?\"",
			(char*)NULL);
		return TCL_ERROR;
	}

	/*
	 *  Find the call frame for the appropriate level.
	 *    result=-1 => syntax error
	 *    result=0  => no level found (default level "1" used)
	 *    result=1  => level specified
	 */
	result = TclGetFrame(interp, argv[1], &framePtr);
	if (result == -1)
		return TCL_ERROR;

	argc -= (result+1);  /* adjust (argc,argv) for level parsing */
	if (argc == 0)       /* no command to execute? */
	{
		Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			" ?level? command ?arg ...?\"", (char*)NULL);
		return TCL_ERROR;
	}
	argv += (result+1);  /* adjust argv AFTER error message above */

	/*
	 *  Find the interpreter containing the desired call frame.
	 */
	ex_interp = NULL;
	if (framePtr == NULL)  /* global frame? */
		ex_interp = info->main;
	else
	{
		iPtr = (Interp*)info->main;
		for (fp=iPtr->framePtr; fp; fp=fp->callerPtr)
			if (fp == framePtr)
			{
				ex_interp = info->main;
				break;
			}

		entry = Tcl_FirstHashEntry(&info->classes, &place);
		while (entry && !ex_interp)
		{
			cdefn = (Itcl_Class*)Tcl_GetHashValue(entry);
			iPtr  = (Interp*)cdefn->interp;
			for (fp=iPtr->framePtr; fp; fp=fp->callerPtr)
				if (fp == framePtr)
				{
					ex_interp = cdefn->interp;
					break;
				}

			entry = Tcl_NextHashEntry(&place);
		}
	}

	/*
	 *  Modify the interpreter state to execute in the given frame.
	 *  Execute the residual arguments as a command.
	 */
	Itcl_EnterExecScope(ex_interp, &exscope, (Itcl_VarScope*)NULL, interp);
	ex_iPtr = (Interp*)ex_interp;
	savedVarFramePtr = ex_iPtr->varFramePtr;
	ex_iPtr->varFramePtr = framePtr;

	if (argc == 1)
		result = Tcl_Eval(ex_interp, argv[0]);
	else
	{
		char *cmd;
		cmd = Tcl_Concat(argc, argv);
		result = Tcl_Eval(ex_interp, cmd);
		ckfree(cmd);
	}

	if (result == TCL_ERROR)
	{
		char msg[60];
		sprintf(msg, "\n    (\"uplevel\" body line %d)", ex_interp->errorLine);
		Tcl_AddErrorInfo(ex_interp, msg);
	}
	Itcl_XferResult(interp, ex_interp, result);

	ex_iPtr->varFramePtr = savedVarFramePtr;
	Itcl_LeaveExecScope(ex_interp, &exscope);

	return result;
}
