/*
 * address.c
 *
 *	This file implements the "address" extension of Tcl-DP.
 *
 * Copyright 1992 Regents of the University of California.
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include "tcl.h"

/*
 * One record of the following type is kept for each inetAddr:
 */

typedef struct InetAddr {
    char name[10];
    struct sockaddr_in addr;
    struct InetAddr *next;
} InetAddr;

static InetAddr *addrList;

/*
 *--------------------------------------------------------------
 *
 * Tcm_CreateAddress --
 *
 *	Create and initialize an inet address struct.  This routine can be
 *	called by C routines and guarantees that if the inet address 
 *	struct already exists for a given addr and port, a new 
 *	struct won't be created.
 *
 * Results:
 *	Handle (an address handle) to created inet address struct.
 *
 * Side effects:
 *	Memory is allocated.
 *
 *--------------------------------------------------------------
 */
char *
Tcm_CreateAddress(addr, port)
    unsigned long addr;		/* Address as a 4 byte int. */
    int port;			/* Port number (host order) */
{
    static int addrNum;
    InetAddr *newAddr;
    InetAddr *searchAddr;

    /*
     * First, see if it already exists
     */
    for (searchAddr = addrList; searchAddr != NULL; searchAddr = searchAddr->next) {
	if ((searchAddr->addr.sin_addr.s_addr == addr) &&
	    (searchAddr->addr.sin_port == htons(port))) {
	    return (searchAddr->name);
	}
    }

    /*
     * Alloc it, give it a name, and link it into the list.
     */
    newAddr = (InetAddr *) ckalloc(sizeof(InetAddr));
    sprintf(newAddr->name, "addr%d", addrNum++);
    newAddr->addr.sin_family = AF_INET;
    newAddr->addr.sin_addr.s_addr = addr;
    newAddr->addr.sin_port = htons(port);
    newAddr->next = addrList;
    addrList = newAddr;

    return (newAddr->name);
}

/*
 *--------------------------------------------------------------
 *
 * Tcm_FindAddr --
 *
 *	Find and return the internet address structure with the
 *	associated name.
 *
 * Results:
 *	Pointer to the internet structure for the address, or NULL
 *	if it can't be found.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */
struct sockaddr *
Tcm_FindAddr(name)
    char *name;
{
    InetAddr *addrPtr;

    /*
     * Find the entry (error if there isn't one).
     */
    for (addrPtr = addrList; addrPtr != NULL; addrPtr = addrPtr->next) {
	if (strcmp(addrPtr->name, name) == 0) {
	    return (struct sockaddr *)(&addrPtr->addr);
	}
    }
    return ((struct sockaddr *) NULL);
}

/*
 *--------------------------------------------------------------
 *
 * Tcm_AddressCmd --
 *
 *	This procedure is invoked to process the "address" Tcl/Tk
 *	command.  See the user documentation for details on what
 *	it does.
 *
 * Results:
 *	A standard Tcl result.
 *
 * Side effects:
 *	See the user documentation.
 *
 *--------------------------------------------------------------
 */
 /* ARGSUSED */
int
Tcm_AddressCmd(notUsed, interp, argc, argv)
    ClientData notUsed;
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    char c;
    int len;

    if (argc < 2) {
	Tcl_AppendResult(interp, "wrong # args: should be \"",
			 argv[0], " option ?arg arg ...?\"", (char *) NULL);
	return TCL_ERROR;
    }
    c = argv[1][0];
    len = strlen(argv[1]);

    /*------------------------ CREATE -----------------------------*/
    if ((c == 'c') && (strncmp(argv[1], "create", len) == 0)) {
	struct hostent *hp;
	unsigned long addr;
	int port;
	char *name;

	/*
	 * Check syntax and validity of args.
	 */
	if (argc != 4) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
			     argv[0], " create host port\"", (char *) NULL);
	    return TCL_ERROR;
	}
	hp = gethostbyname(argv[2]);
	if (hp == NULL) {
	    addr = inet_addr(argv[2]);
	    if (addr == -1) {
		Tcl_AppendResult(interp, argv[0], " create: unknown host \"",
				 argv[2], "\"", (char *) NULL);
		return TCL_ERROR;
	    }
	} else {
	    bcopy((char *) hp->h_addr_list[0], (char *) &addr, sizeof(addr));
	}
	if (Tcl_GetInt(interp, argv[3], &port) != TCL_OK) {
	    return TCL_ERROR;
	}
	/*
	 * Looks good -- alloc a new struct and initialize it.
	 */
	name = Tcm_CreateAddress(addr, port);
	Tcl_AppendResult(interp, name, (char *) NULL);

    /*------------------------ DELETE -----------------------------*/
    } else if ((c == 'd') && (strncmp(argv[1], "delete", len) == 0)) {
	InetAddr *prevPtr;
	InetAddr *addrPtr;

	/*
	 * Check args
	 */
	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
			     argv[0], " delete address\"", (char *) NULL);
	    return TCL_ERROR;
	}
	/*
	 * Find the entry (error if there isn't one).
	 */
	for (prevPtr = NULL, addrPtr = addrList;;
	     prevPtr = addrPtr, addrPtr = addrPtr->next) {
	    if (addrPtr == NULL) {
		Tcl_AppendResult(interp, argv[0], " delete: invalid address \"",
				 argv[2], "\"", (char *) NULL);
		return TCL_ERROR;
	    }
	    if (strcmp(addrPtr->name, argv[2]) == 0) {
		break;
	    }
	}

	/*
	 * Delete this one from the list.
	 */
	if (prevPtr == NULL) {
	    addrList = addrPtr->next;
	} else {
	    prevPtr->next = addrPtr->next;
	}
	free((char *) addrPtr);

    /*------------------------ INFO -------------------------------*/
    } else if ((c == 'i') && (strncmp(argv[1], "info", len) == 0)) {
	InetAddr *addrPtr;
	int f1, f2, f3, f4;

	/*
	 * Check args
	 */
	if (argc != 3) {
	    Tcl_AppendResult(interp, "wrong # args: should be \"",
			     argv[0], " info address\"", (char *) NULL);
	    return TCL_ERROR;
	}
	/*
	 * Find the entry (error if there isn't one).
	 */
	for (addrPtr = addrList;; addrPtr = addrPtr->next) {
	    if (addrPtr == NULL) {
		Tcl_AppendResult(interp, argv[0], " info: invalid address \"",
				 argv[2], "\"", (char *) NULL);
		return TCL_ERROR;
	    }
	    if (strcmp(addrPtr->name, argv[2]) == 0) {
		break;
	    }
	}

	/*
	 * Get the info from this one...
	 */
#ifdef BIG_ENDIAN
	f1 = (addrPtr->addr.sin_addr.s_addr >> 24) & 0xff;
	f2 = (addrPtr->addr.sin_addr.s_addr >> 16) & 0xff;
	f3 = (addrPtr->addr.sin_addr.s_addr >> 8) & 0xff;
	f4 = addrPtr->addr.sin_addr.s_addr & 0xff;
#else
	f4 = (addrPtr->addr.sin_addr.s_addr >> 24) & 0xff;
	f3 = (addrPtr->addr.sin_addr.s_addr >> 16) & 0xff;
	f2 = (addrPtr->addr.sin_addr.s_addr >> 8) & 0xff;
	f1 = addrPtr->addr.sin_addr.s_addr & 0xff;
#endif
	sprintf(interp->result, "%d.%d.%d.%d %d", f1, f2, f3, f4,
		ntohs(addrPtr->addr.sin_port));
    } else {
	Tcl_AppendResult(interp, argv[0], "unknown option \"",
			 argv[1], "\"", (char *) NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}
