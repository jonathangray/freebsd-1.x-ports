/*
 * dpaddress.c
 *
 *	This file implements the "dp_address" extension of Tcl-DP.
 *
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

#include <stdio.h>
#include "dpInt.h"
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

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
 * Tdp_CreateAddress --
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
Tdp_CreateAddress(addr, port)
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
 * Tdp_FindAddr --
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
Tdp_FindAddr(name)
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
 * Tdp_AddressCmd --
 *
 *	This procedure is invoked to process the "dp_address" Tcl/Tk
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
Tdp_AddressCmd(notUsed, interp, argc, argv)
    ClientData notUsed;
    Tcl_Interp *interp;		/* Current interpreter. */
    int argc;			/* Number of arguments. */
    char **argv;		/* Argument strings. */
{
    char c;
    int len;
    char tmp[256];

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
	    memcpy((char *)&addr, (char *)hp->h_addr_list[0], sizeof(addr));
	}
	if (Tcl_GetInt(interp, argv[3], &port) != TCL_OK) {
	    return TCL_ERROR;
	}
	/*
	 * Looks good -- alloc a new struct and initialize it.
	 */
	name = Tdp_CreateAddress(addr, port);
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
	int f1, f2, f3, f4, addr;

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
        addr = htonl(addrPtr->addr.sin_addr.s_addr);
	f1 = (addr >> 24) & 0xff;
	f2 = (addr >> 16) & 0xff;
	f3 = (addr >> 8) & 0xff;
	f4 = addr & 0xff;
	sprintf(tmp, "%d.%d.%d.%d %d", f1, f2, f3, f4,
		ntohs(addrPtr->addr.sin_port));
	Tcl_SetResult(interp, tmp, TCL_VOLATILE);
    } else {
	Tcl_AppendResult(interp, argv[0], "unknown option \"",
			 argv[1], "\"", (char *) NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}
