/*
 * bltList.h --
 *
 * Copyright 1993-1994 by AT&T Bell Laboratories.
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
#ifndef _BLT_LIST_H
#define _BLT_LIST_H

/*
 * A Blt_ListEntry is the container structure for the Blt_LinkedList.
 */
typedef struct Blt_ListEntry {
    struct Blt_ListEntry *prevPtr;	/* Link to the previous entry */
    struct Blt_ListEntry *nextPtr;	/* Link to the next entry */
    Tk_Uid keyPtr;		/* Pointer to the (character string) key */
    ClientData clientData;	/* Pointer to the data object */
} Blt_ListEntry;

/*
 * A Blt_LinkedList is a doubly chained list structure.
 */
typedef struct Blt_LinkedList {
    Blt_ListEntry *headPtr;	/* Pointer to first element in list */
    Blt_ListEntry *tailPtr;	/* Pointer to last element in list */
    int numEntries;		/* Number of elements in list */
    int type;			/* Type of keys in list */
} Blt_LinkedList;

EXTERN void Blt_InitLinkedList _ANSI_ARGS_((Blt_LinkedList *listPtr, int type));
EXTERN Blt_LinkedList *Blt_CreateLinkedList _ANSI_ARGS_((int type));
EXTERN void Blt_DeleteLinkedList _ANSI_ARGS_((Blt_LinkedList *listPtr));
EXTERN Blt_ListEntry *Blt_CreateListEntry _ANSI_ARGS_((char *key));
EXTERN void Blt_DestroyListEntry _ANSI_ARGS_((Blt_ListEntry *entryPtr));
EXTERN void Blt_ClearList _ANSI_ARGS_((Blt_LinkedList *listPtr));
EXTERN void Blt_LinkListAfter _ANSI_ARGS_((Blt_LinkedList *listPtr,
	Blt_ListEntry *entryPtr, Blt_ListEntry *afterPtr));
EXTERN void Blt_LinkListBefore _ANSI_ARGS_((Blt_LinkedList *listPtr,
	Blt_ListEntry *entryPtr, Blt_ListEntry *beforePtr));
EXTERN void Blt_UnlinkListEntry _ANSI_ARGS_((Blt_LinkedList *listPtr,
	Blt_ListEntry *entryPtr));
EXTERN Blt_ListEntry *Blt_FindListEntry _ANSI_ARGS_((Blt_LinkedList *listPtr,
	char *name));
EXTERN void Blt_DeleteListEntry _ANSI_ARGS_((Blt_LinkedList *listPtr,
	Blt_ListEntry *entryPtr));

#define Blt_FirstListEntry(listPtr) ((listPtr)->headPtr)
#define Blt_LastListEntry(listPtr)  ((listPtr)->tailPtr)
#define Blt_PrevListEntry(entryPtr) ((entryPtr)->prevPtr)
#define Blt_NextListEntry(entryPtr) ((entryPtr)->nextPtr)
#define Blt_GetListKey(entryPtr)    ((entryPtr)->keyPtr)
#define Blt_GetListLength(listPtr)  ((listPtr)->numEntries)
#define Blt_GetListValue(entryPtr)  ((entryPtr)->clientData)
#define Blt_SetListValue(entryPtr, valuePtr) \
	((entryPtr)->clientData = (ClientData)(valuePtr))

#endif /* _BLT_LIST_H */
