/*
 * ------------------------------------------------------------------------
 *  APPLICATION:  [incr Tcl]
 *      PURPOSE:  object-oriented extensions to Tcl
 *
 *  This segment provides common utility functions used throughout
 *  the other [incr Tcl] source files.
 *
 * ------------------------------------------------------------------------
 *  AUTHOR:  Michael J. McLennan       Phone: (610)712-2842
 *           AT&T Bell Laboratories   E-mail: michael.mclennan@att.com
 *
 *     RCS:  itcl_util.c,v 1.2 1994/03/25 19:01:15 mmc Exp
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
#include <string.h>
#include <memory.h>
#include "tclInt.h"

#include "itcl_util.h"

/*
 *  STACK INFO
 */
typedef struct Itcl_PreservedData {
	ClientData data;        /* reference to data */
	int usage;              /* number of active uses */
	Itcl_FreeProc *fproc;   /* procedure used to free data */
} Itcl_PreservedData;

static Tcl_HashTable *Itcl_PreservedList = NULL;

#ifndef lint
static char sccsid[] = "@(#)itcl_util.c	1.7 (3/21/94) Michael J. McLennan";
#endif


/*
 * ------------------------------------------------------------------------
 *  Itcl_InitStack()
 *
 *  Initializes a stack structure, allocating a certain amount of memory
 *  for the stack and setting the stack length to zero.
 * ------------------------------------------------------------------------
 */
void
Itcl_InitStack(stack)
	Itcl_Stack *stack;     /* stack to be initialized */
{
	stack->values = stack->space;
	stack->max = sizeof(stack->space)/sizeof(ClientData);
	stack->len = 0;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_DeleteStack()
 *
 *  Destroys a stack structure, freeing any memory that may have been
 *  allocated to represent it.
 * ------------------------------------------------------------------------
 */
void
Itcl_DeleteStack(stack)
	Itcl_Stack *stack;     /* stack to be deleted */
{
	if (stack->values != stack->space)  /* allocated extra memory? */
		ckfree((char*)stack->values);   /* then free it */

	stack->values = NULL;
	stack->len = stack->max = 0;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_PushStack()
 *
 *  Pushes a piece of client data onto the top of the given stack.
 * ------------------------------------------------------------------------
 */
void
Itcl_PushStack(cdata,stack)
	ClientData cdata;      /* data to be pushed onto stack */
	Itcl_Stack *stack;     /* stack */
{
	ClientData *newStack;

	if (stack->len+1 >= stack->max)
	{
		stack->max = (stack->max == 0) ? 5 : 2*stack->max;
		newStack = (ClientData*)
			ckalloc((unsigned)(stack->max*sizeof(ClientData)));

		if (stack->values)
		{
			memcpy((char*)newStack, (char*)stack->values,
				stack->len*sizeof(ClientData));

			if (stack->values != stack->space)
				ckfree((char*)stack->values);
		}
		stack->values = newStack;
	}
	stack->values[stack->len++] = cdata;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_PopStack()
 *
 *  Pops a bit of client data from the top of the given stack.
 * ------------------------------------------------------------------------
 */
ClientData
Itcl_PopStack(stack)
	Itcl_Stack *stack;  /* stack to be manipulated */
{
	return (stack->values && (stack->len > 0))
		? stack->values[--stack->len]
		: (ClientData)NULL;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_PeekStack()
 *
 *  Gets the current value from the top of the given stack.
 * ------------------------------------------------------------------------
 */
ClientData
Itcl_PeekStack(stack)
	Itcl_Stack *stack;  /* stack to be examined */
{
	return (stack->values && (stack->len > 0))
		? stack->values[stack->len-1]
		: (ClientData)NULL;
}


/*
 * ------------------------------------------------------------------------
 *  Itcl_EventuallyFree()
 *
 *  Registers a piece of data so that it will be freed when no longer
 *  in use.  The data is registered with an initial usage count of "0".
 *  Future calls to Itcl_PreserveData() increase this usage count, and
 *  calls to Itcl_ReleaseData() decrease the count until it reaches
 *  zero and the data is freed.
 *
 *  The Tk library has a similar function that could be used instead.
 *  This function is provided simply to avoid a dependency on the
 *  Tk library, to make porting easier for Tcl-only users.
 * ------------------------------------------------------------------------
 */
void
Itcl_EventuallyFree(cdata,fproc)
	ClientData cdata;      /* data to be freed when not in use */
	Itcl_FreeProc *fproc;  /* procedure called to free data */
{
	int newEntry;
	Tcl_HashEntry *entry;
	Itcl_PreservedData *chunk;

	/*
	 *  If a list has not yet been created to manage bits of
	 *  preserved data, then create it.
	 */
	if (!Itcl_PreservedList)
	{
		Itcl_PreservedList = (Tcl_HashTable*)ckalloc(
			(unsigned)sizeof(Tcl_HashTable)
		);
		Tcl_InitHashTable(Itcl_PreservedList,TCL_ONE_WORD_KEYS);
	}

	/*
	 *  Create a new entry representing the preserved data.
	 */
	entry = Tcl_CreateHashEntry(Itcl_PreservedList,(char*)cdata,&newEntry);
	if (newEntry)
	{
		chunk = (Itcl_PreservedData*)ckalloc(
			(unsigned)sizeof(Itcl_PreservedData)
		);
		chunk->data  = cdata;
		chunk->usage = 0;
		chunk->fproc = fproc;
		Tcl_SetHashValue(entry, (ClientData)chunk);
	}
	else
		panic("same data being Itcl_EventuallyFree()'d twice!");
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_PreserveData()
 *
 *  Increases the usage count for a piece of data that was registered
 *  previously via Itcl_EventuallyFree().  Future calls to
 *  Itcl_ReleaseData() will cause the data to be freed.
 *
 *  The Tk library has a similar function that could be used instead.
 *  This function is provided simply to avoid a dependency on the
 *  Tk library, to make porting easier for Tcl-only users.
 * ------------------------------------------------------------------------
 */
ClientData
Itcl_PreserveData(cdata)
	ClientData cdata;      /* data to be preserved */
{
	Tcl_HashEntry *entry = NULL;
	Itcl_PreservedData *chunk;

	/*
	 *  Find the data in the global list and bump its usage count.
	 */
	if (Itcl_PreservedList)
		entry = Tcl_FindHashEntry(Itcl_PreservedList,(char*)cdata);

	if (!entry)
		panic("Itcl_PreserveData() called for data not registered or already freed!");

	chunk = (Itcl_PreservedData*)Tcl_GetHashValue(entry);
	chunk->usage++;

	return cdata;
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_ReleaseData()
 *
 *  Decreases the usage count for a piece of data that was registered
 *  previously via Itcl_EventuallyFree().  When the usage count reaches
 *  zero, the data is automatically freed.
 *
 *  The Tk library has a similar function that could be used instead.
 *  This function is provided simply to avoid a dependency on the
 *  Tk library, to make porting easier for Tcl-only users.
 * ------------------------------------------------------------------------
 */
void
Itcl_ReleaseData(cdata)
	ClientData cdata;      /* data to be released */
{
	Tcl_HashEntry *entry = NULL;
	Itcl_PreservedData *chunk;

	/*
	 *  Find the data in the global list and decrement its usage count.
	 */
	if (Itcl_PreservedList)
		entry = Tcl_FindHashEntry(Itcl_PreservedList,(char*)cdata);

	if (!entry)
		panic("Itcl_ReleaseData() called for data not registered or already freed!");

	chunk = (Itcl_PreservedData*)Tcl_GetHashValue(entry);
	if (--chunk->usage <= 0)
	{
		(*chunk->fproc)(chunk->data);
		Tcl_DeleteHashEntry(entry);
		ckfree((char*)chunk);
	}
}


/*
 * ------------------------------------------------------------------------
 *  Itcl_ParseSlotName()
 *
 *  Parses the name of the form "class::slot", copying the "class" and
 *  slot segments into the given storage space.
 * ------------------------------------------------------------------------
 */
void
Itcl_ParseSlotName(name,cpart,spart,bsize)
	char *name;          /* name of slot */
	char *cpart;         /* returns "class" part */
	char *spart;         /* returns "slot" part */
	int bsize;           /* size of cpart/spart buffers */
{
	register char *sep = name;
	register int colons = 0;

	while (*sep != '\0')
		if (*sep++ == ':')
		{
			if (++colons == 2)
			{
				strncpy(spart,sep,bsize);
				sep -= 2; *sep = '\0';
				strncpy(cpart,name,bsize);
				*sep = ':';
				break;
			}
		}
		else
			colons = 0;

	if (colons != 2)                /* no "::" found? */
	{
		*cpart = '\0';              /* then no "class" part */
		strncpy(spart,name,bsize);  /* entire name is "slot" part */
	}
}

/*
 * ------------------------------------------------------------------------
 *  Itcl_MakeSlotName()
 *
 *  Constructs a string of the form "class::slot" and returns a pointer
 *  to it.  The string is kept in an internal buffer that is overwritten
 *  on subsequent calls.
 * ------------------------------------------------------------------------
 */
char*
Itcl_MakeSlotName(cpart,spart)
	char *cpart;         /* returns "class" part */
	char *spart;         /* returns "slot" part */
{
	static char buffer[165];

	strncpy(buffer,cpart,80);
	strcat(buffer,"::");
	strncat(buffer,spart,80);

	return buffer;
}


/*
 * ------------------------------------------------------------------------
 *  Itcl_XferResult()
 *
 *  Transfers the result from one interpreter to another, taking care
 *  to transport error information as well.
 * ------------------------------------------------------------------------
 */
void
Itcl_XferResult(to,from,status)
	Tcl_Interp *to;      /* interp to transfer to */
	Tcl_Interp *from;    /* interp to transfer from */
	int status;          /* associated with result in "from" interp */
{
	Interp *fiPtr = (Interp*)from;

	int i, argc;
	char **argv;
	char *einfo;

	if (to != from)
	{
		/*
		 *  If the result represents an error, then propagate the
		 *  errorInfo and errorCode information.
		 */
		if ((status != TCL_OK) && (fiPtr->flags & ERR_IN_PROGRESS))
		{
		    if ((einfo=Tcl_GetVar(from,"errorInfo",TCL_GLOBAL_ONLY)) != NULL)
				Tcl_AddErrorInfo(to, einfo);

		    if ((einfo=Tcl_GetVar(from,"errorCode",TCL_GLOBAL_ONLY)) != NULL)
			{
				if (Tcl_SplitList(from, einfo, &argc, &argv) == TCL_OK)
				{
					Tcl_SetErrorCode(to, argv[0], (char*)NULL);
					for (i=1; i < argc; i++)
						Tcl_SetVar2(to, "errorCode", (char*)NULL, argv[i],
							TCL_GLOBAL_ONLY |
							TCL_LIST_ELEMENT |
							TCL_APPEND_VALUE);
					ckfree((char*)argv);
				}
			}
		}

		Tcl_SetResult(to, from->result, TCL_VOLATILE);
		Tcl_ResetResult(from);  /* clear errorInfo */
	}
}
