/*
 * bltList.c --
 *
 *	Generic linked list management routines.
 *
 * Copyright 1991-1994 by AT&T Bell Laboratories.
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
 */

#include "blt.h"

/*
 *----------------------------------------------------------------------
 *
 * Blt_InitLinkedList --
 *
 *	Initialized a linked list.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_InitLinkedList(listPtr, type)
    Blt_LinkedList *listPtr;
    int type;
{

    listPtr->numEntries = 0;
    listPtr->headPtr = listPtr->tailPtr = (Blt_ListEntry *)NULL;
    listPtr->type = type;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_CreateLinkedList --
 *
 *	Creates a new linked list structure and initializes its pointers
 *
 * Results:
 *	Returns a pointer to the newly created list structure.
 *
 *----------------------------------------------------------------------
 */
Blt_LinkedList *
Blt_CreateLinkedList(type)
    int type;
{
    Blt_LinkedList *listPtr;

    listPtr = (Blt_LinkedList *)malloc(sizeof(Blt_LinkedList));
    if (listPtr != NULL) {
	Blt_InitLinkedList(listPtr, type);
    }
    return (listPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_CreateListEntry --
 *
 *	Creates a list entry holder.  This routine does not insert
 *	the entry into the list, nor does it no attempt to maintain
 *	consistency of the keys.  For example, more than one entry
 *	may use the same key.
 *
 * Results:
 *	The return value is the pointer to the newly created entry.
 *
 * Side Effects:
 *	The key is not copied, only the Uid is kept.  It is assumed
 *	this key will not change in the life of the entry.
 *
 *----------------------------------------------------------------------
 */
Blt_ListEntry *
Blt_CreateListEntry(key)
    char *key;			/* Unique key to reference object */
{
    register Blt_ListEntry *entryPtr;

    entryPtr = (Blt_ListEntry *)malloc(sizeof(Blt_ListEntry));
    if (entryPtr != (Blt_ListEntry *)NULL) {
	entryPtr->keyPtr = key;
	entryPtr->clientData = (ClientData)NULL;
	entryPtr->nextPtr = entryPtr->prevPtr = (Blt_ListEntry *)NULL;
    }
    return (entryPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_DestroyListEntry --
 *
 *	Free the memory allocated for the entry.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_DestroyListEntry(entryPtr)
    Blt_ListEntry *entryPtr;
{
    free((char *)entryPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ClearList --
 *
 *	Removes all the entries from a list, removing pointers to the
 *	objects and keys (not the objects or keys themselves).
 *	The entry counter is reset to zero.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_ClearList(listPtr)
    Blt_LinkedList *listPtr;	/* List to clear */
{
    register Blt_ListEntry *oldPtr;
    register Blt_ListEntry *entryPtr = listPtr->headPtr;

    while (entryPtr != (Blt_ListEntry *)NULL) {
	oldPtr = entryPtr;
	entryPtr = entryPtr->nextPtr;
	Blt_DestroyListEntry(oldPtr);
    }
    Blt_InitLinkedList(listPtr, listPtr->type);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_DeleteLinkedList
 *
 *     Frees all list structures
 *
 * Results:
 *	Returns a pointer to the newly created list structure.
 *
 *----------------------------------------------------------------------
 */
void
Blt_DeleteLinkedList(listPtr)
    Blt_LinkedList *listPtr;
{
    Blt_ClearList(listPtr);
    free((char *)listPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_LinkListAfter --
 *
 *	Inserts an entry following a given entry.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_LinkListAfter(listPtr, entryPtr, afterPtr)
    Blt_LinkedList *listPtr;
    Blt_ListEntry *entryPtr;
    Blt_ListEntry *afterPtr;
{
    /*
     * If the list keys are strings, change the key to a Tk_Uid
     */
    if (listPtr->type == TCL_STRING_KEYS) {
	entryPtr->keyPtr = Tk_GetUid(entryPtr->keyPtr);
    }
    if (listPtr->headPtr == (Blt_ListEntry *)NULL) {
	listPtr->tailPtr = listPtr->headPtr = entryPtr;
    } else {
	if (afterPtr == (Blt_ListEntry *)NULL) {
	    afterPtr = listPtr->tailPtr;
	}
	entryPtr->nextPtr = afterPtr->nextPtr;
	entryPtr->prevPtr = afterPtr;
	if (afterPtr == listPtr->tailPtr) {
	    listPtr->tailPtr = entryPtr;
	} else {
	    afterPtr->nextPtr->prevPtr = entryPtr;
	}
	afterPtr->nextPtr = entryPtr;
    }
    listPtr->numEntries++;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_LinkListBefore --
 *
 *	Inserts an entry preceding a given entry.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_LinkListBefore(listPtr, entryPtr, beforePtr)
    Blt_LinkedList *listPtr;	/* List to contain new entry */
    Blt_ListEntry *entryPtr;	/* New entry to be inserted */
    Blt_ListEntry *beforePtr;	/* Entry to link before */
{
    /*
     * If the list keys are strings, change the key to a Tk_Uid
     */
    if (listPtr->type == TCL_STRING_KEYS) {
	entryPtr->keyPtr = Tk_GetUid(entryPtr->keyPtr);
    }
    if (listPtr->headPtr == (Blt_ListEntry *)NULL) {
	listPtr->tailPtr = listPtr->headPtr = entryPtr;
    } else {
	if (beforePtr == (Blt_ListEntry *)NULL) {
	    beforePtr = listPtr->headPtr;
	}
	entryPtr->prevPtr = beforePtr->prevPtr;
	entryPtr->nextPtr = beforePtr;
	if (beforePtr == listPtr->headPtr) {
	    listPtr->headPtr = entryPtr;
	} else {
	    beforePtr->prevPtr->nextPtr = entryPtr;
	}
	beforePtr->prevPtr = entryPtr;
    }
    listPtr->numEntries++;
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_UnlinkListEntry --
 *
 *	Unlinks an entry from the given list. The entry itself is
 *	not deallocated, but only removed from the list.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_UnlinkListEntry(listPtr, entryPtr)
    Blt_LinkedList *listPtr;
    Blt_ListEntry *entryPtr;
{
    if (listPtr->headPtr == entryPtr) {
	listPtr->headPtr = entryPtr->nextPtr;
    }
    if (listPtr->tailPtr == entryPtr) {
	listPtr->tailPtr = entryPtr->prevPtr;
    }
    if (entryPtr->nextPtr != NULL) {
	entryPtr->nextPtr->prevPtr = entryPtr->prevPtr;
    }
    if (entryPtr->prevPtr != NULL) {
	entryPtr->prevPtr->nextPtr = entryPtr->nextPtr;
    }
    listPtr->numEntries--;
}

#ifdef notdef
/*
 *----------------------------------------------------------------------
 *
 * Blt_SetListValue --
 *
 *	Sets the entry data pointer to the given value.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	The data is not copied, only the pointer is kept.  It is assumed
 *	this data will remain valid as long as the entry.
 *
 *----------------------------------------------------------------------
 */
void
Blt_SetListValue(entryPtr, clientData)
    Blt_ListEntry *entryPtr;	/* Pointer to the entry */
    ClientData clientData;	/* Data attached to key */
{
    entryPtr->clientData = clientData;
}

#endif
/*
 *----------------------------------------------------------------------
 *
 * Blt_ListIndex --
 *
 *	Find the position of an entry in the list.
 *
 * Results:
 *	Returns the index to the entry.  If no entry matching
 *	the key given is found, then -1 is returned.
 *
 *----------------------------------------------------------------------
 */
int
Blt_ListIndex(listPtr, searchPtr)
    Blt_LinkedList *listPtr;	/* List to search */
    Blt_ListEntry *searchPtr;	/* Entry to match */
{
    register int count = 0;
    register Blt_ListEntry *entryPtr;	/* Entry to match */

    for (entryPtr = listPtr->headPtr; entryPtr != NULL;
	entryPtr = entryPtr->nextPtr) {
	if (searchPtr == entryPtr)
	    return (count);
	count++;
    }
    return (-1);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_FindListEntry --
 *
 *	Find the first entry matching the key given.
 *
 * Results:
 *	Returns the pointer to the entry.  If no entry matching
 *	the key given is found, then NULL is returned.
 *
 *----------------------------------------------------------------------
 */
Blt_ListEntry *
Blt_FindListEntry(listPtr, searchKey)
    Blt_LinkedList *listPtr;	/* List to search */
    char *searchKey;		/* Key to match */
{
    register Blt_ListEntry *entryPtr;
    Tk_Uid newPtr;

    newPtr = searchKey;
    if (listPtr->type == TCL_STRING_KEYS) {
	newPtr = Tk_GetUid(searchKey);
    }
    for (entryPtr = listPtr->headPtr; entryPtr != NULL;
	entryPtr = entryPtr->nextPtr) {
	if (newPtr == entryPtr->keyPtr)
	    return (entryPtr);
    }
    return (Blt_ListEntry *) NULL;
}

#ifdef notdef
/*
 *----------------------------------------------------------------------
 *
 * Blt_FirstListEntry --
 *
 *	Find the first entry in the list and return its pointer.
 *	In addition, update the given search pointer.
 *
 * Results:
 *	Returns a pointer to the first entry in the list. If the
 *      list is empty, NULL is returned.  The search pointer (used in
 *	subsequent searches) is set to the appropriate value.
 *
 *----------------------------------------------------------------------
 */
Blt_ListEntry *
Blt_FirstListEntry(listPtr, entryPtrPtr)
    Blt_LinkedList *listPtr;	/* The list we are searching */
    Blt_ListEntry **entryPtrPtr;/* Search pointer to set */
{
    if (listPtr == (Blt_LinkedList *)NULL)
	return (Blt_ListEntry *) NULL;

    if (listPtr->headPtr == (Blt_ListEntry *)NULL)
	return (Blt_ListEntry *) NULL;

    if (entryPtrPtr != (Blt_ListEntry **)NULL) {
	*entryPtrPtr = listPtr->headPtr;
    }
    return (listPtr->headPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_ListLastEntry --
 *
 *	Find the last entry in the list using the given entry as
 *	a cursor to the last entry and return its pointer.
 *	In addition, the cursor position is updated.
 *
 * Results:
 *	A pointer to the last object in the list is returned. If the
 *      list is at end, NULL is returned.  The search pointer (used in
 *	subsequent searches) is set to the appropriate value.
 *
 *----------------------------------------------------------------------
 */
Blt_ListEntry *
Blt_ListLastEntry(entryPtrPtr)
    Blt_ListEntry **entryPtrPtr;/* Search pointer of current position */
{
    if ((entryPtrPtr == (Blt_ListEntry **)NULL) ||
	(*entryPtrPtr == (Blt_ListEntry *)NULL)) {
	return (Blt_ListEntry *) NULL;
    }
    *entryPtrPtr = (*entryPtrPtr)->prevPtr;
    return (*entryPtrPtr);
}

/*
 *----------------------------------------------------------------------
 *
 * Blt_NextListEntry --
 *
 *	Find the next entry in the list using the given search pointer
 *	as the current location and return its pointer.
 *	In addition, update the given search pointer.
 *
 * Results:
 *	A pointer to the next object in the list is returned. If the
 *      list is at end, NULL is returned.  The search pointer (used in
 *	subsequent searches) is set to the appropriate value.
 *
 *----------------------------------------------------------------------
 */
Blt_ListEntry *
Blt_NextListEntry(entryPtrPtr)
    Blt_ListEntry **entryPtrPtr;/* Search pointer indicates current position */
{
    if ((entryPtrPtr == NULL) || (*entryPtrPtr == NULL)) {
	return (Blt_ListEntry *) NULL;
    }
    *entryPtrPtr = (*entryPtrPtr)->nextPtr;
    return (*entryPtrPtr);
}

#endif
/*
 *----------------------------------------------------------------------
 *
 * Blt_DeleteListEntry --
 *
 *	Find the entry and free the memory allocated for the entry.
 *
 * Results:
 *	None.
 *
 *----------------------------------------------------------------------
 */
void
Blt_DeleteListEntry(listPtr, entryPtr)
    Blt_LinkedList *listPtr;
    Blt_ListEntry *entryPtr;
{
    Blt_UnlinkListEntry(listPtr, entryPtr);
    Blt_DestroyListEntry(entryPtr);
}
