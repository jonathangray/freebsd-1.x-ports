#include "copyright.h"

/* $Header: /a/cvs/386BSD/ports/editor/emacs/oldXMenu/Error.c,v 1.1 1993/08/26 06:10:15 rgrimes Exp $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 * 	XMenuError -	Returns a string description of the current
 *			XMenu error status flag.
 *
 *	Author:		Tony Della Fera, DEC
 *			August, 1985
 *
 */

#include "XMenuInt.h"

char *
XMenuError()
{
    char message[128];		/* Error message buffer. */

    if ((_XMErrorCode < XME_CODE_COUNT) && (_XMErrorCode >= 0)) {
	return(_XMErrorList[_XMErrorCode]);
    }
    sprintf(message, "Unknown _XMErrorCode: %d", _XMErrorCode);
    return(message);
}

