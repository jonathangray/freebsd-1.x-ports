/*
 * ext.c --
 *
 *	This module implements an extension library loading
 *	mechanism for the Tcl library.
 *
 * Copyright 1993 by AT&T Bell Laboratories.
 * Permission to use, copy, modify, and distribute this software
 * and its documentation for any purpose and without fee is hereby
 * granted, provided that the above copyright notice appear in all
 * copies and that both that the copyright notice and warranty
 * disclaimer appear in supporting documentation, and that the
 * names of AT&T Bell Laboratories any of their entities not be used
 * in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.
 *
 * AT&T disclaims all warranties with regard to this software, including
 * all implied warranties of merchantability and fitness.  In no event
 * shall AT&T be liable for any special, indirect or consequential
 * damages or any damages whatsoever resulting from loss of use, data
 * or profits, whether in an action of contract, negligence or other
 * tortuous action, arising out of or in connection with the use or
 * performance of this software.
 *
 * Extension command created by George Howlett.
 */

#include "extConfig.h"
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#include <sys/stat.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <ctype.h>
#include <tcl.h>

static Tcl_HashTable handleTable;	/* Table of shared library handles.
					 * Indicates if the extension library
					 * has been previously loaded and 
					 * initialized */
static int initialized = 0;
static char *stdLoadPaths = "/lib /usr/lib /usr/local/lib";

typedef int (ExtInitProc) _ANSI_ARGS_((Tcl_Interp *interp));
typedef void *Xldr_Handle;

/*
 *----------------------------------------------------------------------
 *
 * System dependent routines
 *
 * This currently works with the following operating systems only:
 *
 * 	HP-UX 9.01  shl_load, shl_unload, shl_findsym
 *	SunOS 4.x   dlopen, dlclose, dlfindsym, dlerror
 *	SunOS 5.x
 *
 * This interface uses four routines to manipulate shared libraries.
 *
 *	static void *Xldr_Load(char *path);
 *	static int Xldr_Unload(void *handle);
 *	static void *Xldr_FindProc(void *handle, char *symbol);
 *	static char *Xldr_Error(void *handle);
 *
 *----------------------------------------------------------------------
 */

/* HP-UX */

#ifdef hpux

#include <dl.h>

/*
 *----------------------------------------------------------------------
 *
 * Xldr_Load --
 *
 *	Loads an HP-UX shared library into the process.
 *
 *	Currently set for deferred bindings.  I don't know what
 *	effects this will have on performance.
 *
 * Results:
 *	If successful, returns a handle to the shared library.
 *	Otherwise, returns NULL.
 *
 *----------------------------------------------------------------------
 */
static void *
Xldr_Load(path)
    char *path;
{
    return shl_load(path, BIND_IMMEDIATE | BIND_FIRST | BIND_VERBOSE, 0L);
}

/*
 *----------------------------------------------------------------------
 *
 * Xldr_Unload --
 *
 *	Unloads a Sun shared library from the process.
 *
 * Results:
 *	If successful, returns 0, otherwise -1.
 *
 *----------------------------------------------------------------------
 */
static int
Xldr_Unload(handle)
    void *handle;
{
    return shl_unload(handle);
}

/*
 *----------------------------------------------------------------------
 *
 * Xldr_FindProc --
 *
 *	Finds the address associated with the symbol given.
 *
 *	Type is set to TYPE_PROCEDURE to ensure that the symbol
 *	is a procedure.
 *
 * Results:
 *	If successful, returns the address of the symbol.
 *	Otherwise, returns NULL.
 *
 *----------------------------------------------------------------------
 */
static ExtInitProc *
Xldr_FindProc(handle, symbol)
    void *handle;
    char *symbol;
{
    int result;
    void *value;

    result = shl_findsym((shl_t)&handle, symbol, TYPE_UNDEFINED, &value);
    if (result < 0) {
	return (void *) NULL;
    }
    return (ExtInitProc *) value;
}

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

/*
 *----------------------------------------------------------------------
 *
 * Xldr_Error --
 *
 *	Returns an error message of the last load error.
 *
 *----------------------------------------------------------------------
 */
static char *
Xldr_Error()
{
    extern char *sys_errlist[];

    return sys_errlist[errno];
}

#else /*hpux*/

/* SunOS 4.x, SunOS 5.x, etal.  Anything that uses "dlopen" */

#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif

/*
 *----------------------------------------------------------------------
 *
 * Xldr_Load --
 *
 *	Loads a Sun shared library in the process.
 *
 *	According to the SunOS 4.x manual for dlopen(3), the flag should 
 *	always be 1.
 *
 * Results:
 *	If successful, returns a handle to the shared library.
 *	Otherwise, returns NULL.
 *
 *----------------------------------------------------------------------
 */
static void *
Xldr_Load(path)
    char *path;
{
    return dlopen(path, 1);
}

/*
 *----------------------------------------------------------------------
 *
 * Xldr_Unload --
 *
 *	Unloads a Sun shared library from the process.
 *
 * Results:
 *	If successful, returns 0, otherwise -1.
 *
 *----------------------------------------------------------------------
 */
static int
Xldr_Unload(handle)
    void *handle;
{
    int result;
    
    result = dlclose(handle);
    if (result != 0) {
	result = -1;
    }
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * Xldr_FindProc --
 *
 *	Finds the address associated with the symbol given.
 *
 *	There's no way to ensure that the symbol is a procedure.
 *
 * Results:
 *	If successful, returns the address of the symbol.
 *	Otherwise, returns NULL.
 *
 *----------------------------------------------------------------------
 */
static ExtInitProc *
Xldr_FindProc(handle, symbol)
    void *handle;
    char *symbol;
{
    return (ExtInitProc *) dlsym(handle, symbol);
}

/*
 *----------------------------------------------------------------------
 *
 * Xldr_Error --
 *
 *	Returns an error message of the last load error.
 *
 *----------------------------------------------------------------------
 */
static char *
Xldr_Error()
{
    return dlerror();
}

#endif /* hpux */



#ifndef EXTENSION_VERSION
#define EXTENSION_VERSION "1.0"
#endif


/*
 *----------------------------------------------------------------------
 *
 * FindLibrary --
 *
 *	Finds the library given for a search path.  If the path variable
 *	is set, it should contain a list of directories
 *	representing the search path.  Other the standard library
 *	directories will be searched.
 *
 *	The directories are stat-ed for their existence.
 *
 * Results:
 *	If successful, returns the full path of the library, otherwise
 *	NULL.
 *
 * Side Effects:
 *	The string returned is malloc-ed. It is the responsibility of
 *	the caller to free this string.
 *
 *----------------------------------------------------------------------
 */
static char *
FindLibrary(interp, name)
    Tcl_Interp *interp;
    char *name;
{
    char *pathName;
    struct stat statInfo;
    char *library;

    if (*name != '/') {
	int length;
	int found = 0;
	int numDirs;
	char **dirArr;
	char *pathList;
	register int i;
	
	pathList = Tcl_GetVar2(interp, "tcl_extloadpath", (char *)NULL, 
	       TCL_GLOBAL_ONLY);
	if (pathList == NULL) {
	    pathList = stdLoadPaths; /* use default path */
	}
	if (Tcl_SplitList(interp, pathList, &numDirs, &dirArr) != TCL_OK) {
	    Tcl_AppendResult(interp, "can't split \"", pathList, "\"",
			     (char *)NULL);
	    return NULL;
	}
	for (i = 0; i < numDirs; i++) {
	    length = strlen(dirArr[i]) + strlen(name) + 2;
	    pathName = (char *)malloc(sizeof(char) * length);
	    sprintf(pathName, "%s/%s", dirArr[i], name);
	    if (stat(pathName, &statInfo) >= 0) {
		found = 1;
		break;
	    }
	    free(pathName);
	}
	free((char *)dirArr);
	if (!found) {
	    Tcl_AppendResult(interp, "can't find \"", name,
		"\" in library path", (char *)NULL);
	    return NULL;
	}
    } else {
	if (stat(name, &statInfo) < 0) {
	    Tcl_AppendResult(interp, "can't find \"", name, "\": ",
		Tcl_PosixError(interp), (char *)NULL);
	    return NULL;
	}
	pathName = (char *)malloc(sizeof(char) * (strlen(name) + 1));

	strcpy(pathName, name);
    }
    return (pathName);
}

/*
 *----------------------------------------------------------------------
 *
 * LoadLibrary --
 *
 *	Loads the shared object file representing into the current
 *	process.
 *
 * Results:
 *	Returns a standard Tcl result. If the load was successful, or
 *	previously successful, TCL_OK is returned. Otherwise TCL_ERROR
 *	is returned and an error message is left in interp->result.
 *
 * Side Effects:
 *	The shared library is loaded into to the current process.
 *
 *----------------------------------------------------------------------
 */
static int
LoadLibrary(interp, libName)
    Tcl_Interp *interp;
    char *libName;
{
    void *handle;
    Tcl_HashEntry *entryPtr;
    char *pathName;

    pathName = FindLibrary(interp, libName);
    if (pathName == NULL) {
	return TCL_ERROR;
    }
    entryPtr = Tcl_FindHashEntry(&handleTable, pathName);
    if (entryPtr != NULL) {
	handle = (void *)Tcl_GetHashValue(entryPtr);
	if (handle != NULL) {
	    free(pathName);
	    return TCL_OK;		/* Already loaded */
	}
    }
    handle = Xldr_Load(pathName);
    if (handle == NULL) {
	Tcl_AppendResult(interp, "can't load library \"", pathName,
			 "\": ", Xldr_Error(), (char *)NULL);
	free(pathName);
	return TCL_ERROR;
    }
    if (entryPtr == NULL) {
	int dummy;

	entryPtr = Tcl_CreateHashEntry (&handleTable, pathName, &dummy);
    }
    Tcl_SetHashValue(entryPtr, (ClientData)handle);
    free(pathName);
    return TCL_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * InitLibrary --
 *
 *	Searches for and calls the designated library initialization
 *	routine. 
 *
 * Results:
 *	Returns a standard Tcl result. If the load was successful, or
 *	previously successful, TCL_OK is returned. Otherwise TCL_ERROR
 *	is returned and an error message is left in interp->result.
 *
 * Side Effects:
 *	The shared library is loaded into to the current process.
 *
 *----------------------------------------------------------------------
 */
static int
InitLibrary(interp, procName)
    Tcl_Interp *interp;
    char *procName;
{
    ExtInitProc *initProc;
    Tcl_HashEntry *entryPtr;
    Tcl_HashSearch cursor;
    void *handle;
    
    /*
     * Find the address of the initialization procedure, searching all
     * the libraries loaded so far.
     */
    initProc = NULL;
    for (entryPtr = Tcl_FirstHashEntry(&handleTable, &cursor); 
	 entryPtr != NULL; entryPtr = Tcl_NextHashEntry(&cursor)) {
	handle = (void *)Tcl_GetHashValue(entryPtr);
	initProc = Xldr_FindProc(handle, procName);
	if (initProc != NULL) {
	    break;
	}
    }
    if (initProc == NULL) {
	Tcl_AppendResult(interp, "can't find init routine \"", procName,
		 "\": ", Xldr_Error(), (char *)NULL);
	return TCL_ERROR;
    }
    /* 
     * Finally, run the initialization procedure.
     */
    return ((*initProc) (interp));
}

/*
 *----------------------------------------------------------------------
 *
 * LoadExtension --
 *
 *	Loads the shared object file representing the extension and 
 *	calls its initialization procedure.
 *
 * Results:
 *	Returns a standard Tcl result. If the load was successful, or
 *	previously successful, TCL_OK is returned. Otherwise TCL_ERROR
 *	is returned and an error message is left in interp->result.
 *
 * Side Effects:
 *	The shared library is attached to the process.
 *
 *----------------------------------------------------------------------
 */
static int
LoadExtension(interp, name)
    Tcl_Interp *interp;
    char *name;
{
    char *libraries;
    register int i;
    char **libArr;
    int numLibs;
    int result = TCL_ERROR;

    libraries = Tcl_GetVar2(interp, "tcl_extensions", name, TCL_GLOBAL_ONLY);
    if (libraries == NULL) {
	Tcl_AppendResult(interp, "can't find extension \"", name, 
			 "\" in tcl_extensions", (char *)NULL);
	return TCL_ERROR;
    }
    if (Tcl_SplitList(interp, libraries, &numLibs, &libArr) != TCL_OK) {
	return TCL_ERROR;
    }
    if (numLibs < 2) {
	Tcl_AppendResult(interp, "two few items in extension entry for \"", 
			 name, "\"", (char *)NULL);
	goto error;
    }
    /* 
     * The following items are names of libraries.  Find each library
     * (absolute path) from the item and try to load it.
     */
    for (i = 1; i < numLibs; i++) {
	if (LoadLibrary(interp, libArr[i]) != TCL_OK) {
	    goto error;
	}
    }
    /* 
     * Run the initialization procedure.
     */
    if (InitLibrary(interp, libArr[0]) != TCL_OK) {
	goto error;
    }
    result = TCL_OK;
  error:
    free ((char *)libArr);
    return result;
}

/*
 *--------------------------------------------------------------
 *
 * ExtensionCmd --
 *
 *	This procedure is invoked to process the Tcl command
 *	that loads extensions in the Tcl shell. See the user
 *	documentation for details on what it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */
static int
ExtensionCmd(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    char c;
    int length;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
	    " option ?args? \"", (char *)NULL);
	return TCL_ERROR;
    }
    if (!initialized) {
	static char initCmd[] = " \n\
	    if [info exists env(TCL_EXTMAP)] { \n\
                set file $env(TCL_EXTMAP) \n\
	    } else { \n\
                set file [info library]/extensions.tcl \n\
            } \n\
            if [file readable $file] { \n\
                source $file \n\
            } \n\
        ";
	if (Tcl_Eval(interp, initCmd) != TCL_OK) {
	    return TCL_ERROR;
        }
	Tcl_ResetResult(interp);
	Tcl_InitHashTable(&handleTable, TCL_STRING_KEYS);
	initialized = 1;
    }
    c = argv[1][0];
    length = strlen(argv[1]);
    if ((c == 'a') && (strncmp(argv[1], "add", length) == 0)) {
	register int i;

	for (i = 2; i < argc; i++) {
	    if (LoadExtension(interp, argv[i]) != TCL_OK) {
		return TCL_ERROR;
	    }
	}
    } else if ((c == 'l') && (length > 2) && 
	       (strncmp(argv[1], "lload", length) == 0)){
	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			     " load library\"", (char *)NULL);
	    return TCL_ERROR;
	}
	if (LoadLibrary(interp, argv[2]) != TCL_OK) {
	    return TCL_ERROR;
	}
    } else if ((c == 'l') && (length > 2) && 
	(strncmp(argv[1], "linit", length) == 0)){
	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"", argv[0],
			     " init library\"", (char *)NULL);
	    return TCL_ERROR;
	}
	if (InitLibrary(interp, argv[2]) != TCL_OK) {
	    return TCL_ERROR;
	}
    } else if ((c == 'l') && (length > 2) &&
	       (strncmp(argv[1], "libs", length) == 0)) {
	Tcl_HashSearch cursor;
	Tcl_HashEntry *entryPtr;
	char *pathName;

	for (entryPtr = Tcl_FirstHashEntry(&handleTable, &cursor);
	    entryPtr != NULL; entryPtr = Tcl_NextHashEntry(&cursor)) {
	    pathName = Tcl_GetHashKey(&handleTable, entryPtr);
	    Tcl_AppendElement(interp, pathName);
	}
    } else {
	Tcl_AppendResult(interp, "unknown option \"", argv[1],
	    "\": should be add, libs, linit, or lload", (char *)NULL);
	return TCL_ERROR;
    }

    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * Extension_Init --
 *
 *	This procedure is invoked to initialized the Tcl command
 *	that corresponds to the "extension" command.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	Creates the new "extension" command.
 *
 *--------------------------------------------------------------
 */
int
Extension_Init(interp)
    Tcl_Interp *interp;
{
    Tcl_CmdInfo info;

    if (!Tcl_GetCommandInfo(interp, "extension", &info)) {
	Tcl_CreateCommand(interp, "extension", ExtensionCmd, (ClientData)0,
	    (Tcl_CmdDeleteProc *)NULL);
    }
    return TCL_OK;
}

