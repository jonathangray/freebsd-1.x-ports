/*
 * Copyright 1993 Network Computing Devices, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this 
 * software without specific, written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)extensions.c,v 1.3 1993/08/16 19:00:33 greg Exp $
 */

#include "scope.h"
#include "au.h"

typedef struct {
    char       *name;
    void        (*init_proc) ();
    void        (*decode_request) ();
    void        (*decode_reply) ();
    void        (*decode_event) ();
    void        (*decode_error) ();
    int         num_events;
    int         num_errors;
    int         major_opcode;
    int         first_event;
    int         first_error;
}           ExtensionStr, *ExtensionPtr;

ExtensionStr KnownExtensions[] =
{
    NULL,
};

int
GetExtensionPtr(name)
    char       *name;
{
    ExtensionPtr ep;

    for (ep = KnownExtensions; ep->name; ep++)
	if (!strcmp(ep->name, name))
	    return ((int) ep);
    return (0);
}

void
InitExtension(ptr, major_opcode, first_event, first_error)
    int         ptr;
    int         major_opcode;
    int         first_event;
    int         first_error;
{
    ExtensionPtr ep;

    if (ep = (ExtensionPtr) ptr) {
	ep->major_opcode = major_opcode;
	ep->first_event = first_event;
	ep->first_error = first_error;
	(*ep->init_proc) (major_opcode, first_event, first_error);
    }
}

void
InitExtensionRequestsReplies(p)
    TYPE        p;
{
    ExtensionPtr ep;

    for (ep = KnownExtensions; ep->name; ep++)
	if (ep->major_opcode)
	    DefineEValue(p, (AuInt32) ep->major_opcode, ep->name);
}

void
ExtensionDecodeRequest(fd, major_op, minor_op, buf)
    FD          fd;
    short       major_op;
    short       minor_op;
    unsigned char *buf;
{
    ExtensionPtr ep;

    for (ep = KnownExtensions; ep->name; ep++)
	if (major_op == ep->major_opcode) {
	    (*ep->decode_request) (fd, major_op, minor_op, buf);
	    return;
	}
    warn("Extended request opcode");
}

void
ExtensionDecodeReply(fd, major_op, minor_op, buf)
    FD          fd;
    short       major_op;
    short       minor_op;
    unsigned char *buf;
{
    ExtensionPtr ep;

    for (ep = KnownExtensions; ep->name; ep++)
	if (major_op == ep->major_opcode) {
	    (*ep->decode_reply) (fd, major_op, minor_op, buf);
	    return;
	}
    warn("Extended reply opcode");
}

void
ExtensionDecodeEvent(fd, event, buf)
    FD          fd;
    short       event;
    unsigned char *buf;
{
    ExtensionPtr ep;

    for (ep = KnownExtensions; ep->name; ep++)
	if ((event >= ep->first_event) &&
		(event < (ep->first_event + ep->num_events))) {
	    buf[0] -= ep->first_event;
	    (*ep->decode_event) (fd, event - ep->first_event, buf);
	    return;
	}
    warn("Extended event");
}

void
ExtensionDecodeError(fd, error, buf)
    FD          fd;
    short       error;
    unsigned char *buf;
{
    ExtensionPtr ep;

    for (ep = KnownExtensions; ep->name; ep++)
	if ((error >= ep->first_error) &&
		(error < (ep->first_error + ep->num_errors))) {
	    buf[0] -= ep->first_error;
	    (*ep->decode_error) (fd, error - ep->first_error, buf);
	    return;
	}
    warn("Extended error");
}
