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
 * $NCDId: @(#)decodeau.c,v 1.13 1993/08/16 19:00:31 greg Exp $
 */

/*
 * decoding and switich routines for Audio protocol
 * 
 * based on *
 * 
 * Decoding and switching routines for the X11 protocol	* *
 * 
 * James Peterson, 1988			      		* (c) Copyright MCC,
 * 1988 		      		* *
 * 
**********************************************************
 * 
 * 
 */

#include "scope.h"
#include "au.h"

/*
 * There are 4 types of things in Audio: requests, replies, errors, and
 * events.
 * 
 * Each of them has a format defined by a small integer that defines the type of
 * the thing.
 * 
 * Requests have an opcode in the first byte. Events have a code in the first
 * byte. Errors have a code in the second byte (the first byte is 0) Replies
 * ...
 * 
 * Replies have a sequence number in bytes 2 and 3.  The sequence number should
 * be used to identify the request that was sent, and from that request we
 * can determine the type of the reply.
 */


/* ************************************************************ */
/* */
/* */
/* ************************************************************ */


/*
 * We need to keep the sequence number for a request to match it with an
 * expected reply.  The sequence number is associated only with the
 * particular connection that we have. We would expect these replies to be
 * handled as a FIFO queue.
 */

struct QueueEntry
{
    struct QueueEntry *Next;
    AuInt32            SequenceNumber;
    short           Request;
    AuInt32            TagInfo;
};

/* free space list of Q entries */

static struct QueueEntry *FreeQEntries = NULL;

/* ************************************************************ */
struct QueueEntry *
NewQEntry(SequenceNumber, major_op, minor_op, taginfo)
AuInt32            SequenceNumber;
short           major_op;
short           minor_op;
AuInt32            taginfo;
{
    struct QueueEntry *p;

    /* Get a Queue Entry */
    if (FreeQEntries == NULL)
    {
	/* create new queue entry */
	p = (struct QueueEntry *) malloc((AuInt32) (sizeof(*p)));
    }
    else
    {
	/* reuse an old queue entry */
	p = FreeQEntries;
	FreeQEntries = FreeQEntries->Next;
    }

    /* fill in its values */
    p->Next = NULL;
    p->SequenceNumber = SequenceNumber;
    p->Request = (major_op << 8) + minor_op;
    p->TagInfo = taginfo;
    return (p);
}

/* ************************************************************ */

/* define a queue of entries for each FD */

struct QueueHeader
{
    struct QueueEntry *Head;
    struct QueueEntry *Tail;
};

struct QueueHeader ReplyQ[StaticMaxFD];

/* ************************************************************ */

InitReplyQ()
{
    short           i;

    for (i = 0; i < StaticMaxFD; i++)
    {
	ReplyQ[i].Head = NULL;
	ReplyQ[i].Tail = NULL;
    }
}

FlushReplyQ(fd)
FD              fd;
{
    struct QueueEntry *p;
    struct QueueEntry *NextQEntry;

    /* go down the reply queue and free all entries */
    for (p = ReplyQ[fd].Head; p != NULL; p = NextQEntry)
    {
	NextQEntry = p->Next;

	/* put freed entry on list of free entries (for later reuse)  */
	p->Next = FreeQEntries;
	FreeQEntries = p;
    }

    ReplyQ[fd].Head = NULL;
    ReplyQ[fd].Tail = NULL;
}


DumpReplyQ(fd)
FD              fd;
{
    fprintf(stderr, "ReplyQ[%d] = { Head 0x%x; Tail 0x%x }\n",
	    fd, ReplyQ[fd].Head, ReplyQ[fd].Tail);
    {
	struct QueueEntry *p;

	for (p = ReplyQ[fd].Head; p != NULL; p = p->Next)
	    fprintf(stderr, "0x%x = { Next 0x%x; SequenceNumber %d; Request %d }\n",
		    p, p->Next, p->SequenceNumber, p->Request);
    }
}

/* ************************************************************ */
/* */
/* */
/* ************************************************************ */

/*
 * A reply is expected to the type of request given for the fd associated
 * with this one
 */

SequencedReplyExpected(fd, SequenceNumber, major_op, minor_op, taginfo)
FD              fd;
AuInt32            SequenceNumber;
short           major_op;
short           minor_op;
AuInt32            taginfo;
{
    struct QueueEntry *p;

    debug(8, (stderr, "Reply expected: sequence %d and request type (%d, %d) for fd %d\n",
	      SequenceNumber, major_op, minor_op, fd));
    /* create a new queue entry */
    p = NewQEntry(SequenceNumber, major_op, minor_op, taginfo);

    /* find the server associated with this client */
    fd = FDPair(fd);
    if (fd < 0 || fd >= StaticMaxFD)
	return;

    /* attach the new queue entry to the end of the queue for the Server */
    if (ReplyQ[fd].Tail != NULL)
	(ReplyQ[fd].Tail)->Next = p;
    else
	ReplyQ[fd].Head = p;
    ReplyQ[fd].Tail = p;

    debug(8, (stderr, "Save sequence %d and request type %d for fd %d\n",
	      p->SequenceNumber, p->Request, fd));
}


static FD       Lastfd;
static AuInt32     LastSequenceNumber;
static short    LastReplyType;
AuInt32            ReplyTagInfo;
int             IsSendEvent;

/*
 * search for the type of request that is associated with a reply to the
 * given sequence number for this fd
 */

short
CheckReplyTable(fd, SequenceNumber)
FD              fd;
short           SequenceNumber;
{
    struct QueueEntry *p;
    struct QueueEntry *trailer;

    if (debuglevel & 128)
	DumpReplyQ(fd);
    for (trailer = NULL, p = ReplyQ[fd].Head;
	 p != NULL;
	 trailer = p, p = p->Next)
    {
	/* look for matching sequence number in queue of this fd */
	if (SequenceNumber == ((short) (0xFFFF & p->SequenceNumber)))
	{
	    /* save the Request type */
	    Lastfd = fd;
	    LastSequenceNumber = p->SequenceNumber;
	    LastReplyType = p->Request;
	    ReplyTagInfo = p->TagInfo;

	    /* pull the queue entry out of the queue for this fd */
	    if (trailer == NULL)
		ReplyQ[fd].Head = p->Next;
	    else
		trailer->Next = p->Next;
	    if (ReplyQ[fd].Tail == p)
		ReplyQ[fd].Tail = trailer;


	    /* put freed entry on list of free entries (for later reuse)  */
	    p->Next = FreeQEntries;
	    FreeQEntries = p;

	    debug(8, (stderr, "Reply on fd %d for sequence %d is type %d\n",
		      fd, SequenceNumber, LastReplyType));
	    return (LastReplyType);
	}
    }

    /* not expecting a reply for that sequence number */
    debug(8, (stderr, "Reply on fd %d for sequence %d is not found\n",
	      fd, SequenceNumber));
    return (0);
}


/* ************************************************************ */
/*
 * A reply is expected to the type of request given for the sequence number
 * associated with this fd
 */

ReplyExpected(fd, major_op)
FD              fd;
short           major_op;
{
    SequencedReplyExpected(fd, CS[fd].SequenceNumber, major_op, 0, 0);
}

ExtendedReplyExpected(fd, major_op, minor_op, taginfo)
FD              fd;
short           major_op;
short           minor_op;
AuInt32            taginfo;
{
    SequencedReplyExpected(fd, CS[fd].SequenceNumber, major_op, minor_op,
			   taginfo);
}

/* ************************************************************ */
/* another reply is expected for the same reply as we just had */
/* This is only used with ListFontsWithInfo */

KeepLastReplyExpected()
{
    SequencedReplyExpected(Lastfd, LastSequenceNumber, LastReplyType, 0, 0);
}

/* ************************************************************ */
/* */
/* */
/* ************************************************************ */


DecodeRequest(fd, buf, n)
FD              fd;
unsigned char  *buf;
AuInt32            n;
{
    short           major_op;
    short           minor_op;
    AuInt32            Seq;

    major_op = IByte(&buf[0]);
    minor_op = IByte(&buf[1]);
    CS[fd].SequenceNumber += 1;

    if (silent)
	return;

    Seq = ILong(&CS[fd].SequenceNumber);
    bcopy((char *) &Seq, (char *) SBf, sizeof(AuInt32));
    SetIndentLevel(PRINTCLIENT);

    if (Verbose > 3)
	DumpItem("Request", fd, buf, n);
    if (((unsigned short) major_op) > 0x007f)	/* XXX ? */
	ExtensionDecodeRequest(fd, major_op, minor_op, buf);
    else
	switch (major_op)
	{
	    case Au_ListDevices:
		ListDevices(buf);
		ReplyExpected(fd, major_op);
		break;
	    case Au_GetDeviceAttributes:
		GetDeviceAttributes(buf);
		ReplyExpected(fd, major_op);
		break;
	    case Au_SetDeviceAttributes:
		SetDeviceAttributes(buf);
		break;
	    case Au_CreateBucket:
		CreateBucket(buf);
		break;
	    case Au_DestroyBucket:
		DestroyBucket(buf);
		break;
	    case Au_ListBuckets:
		ListBuckets(buf);
		ReplyExpected(fd, major_op);
		break;
	    case Au_GetBucketAttributes:
		GetBucketAttributes(buf);
		ReplyExpected(fd, major_op);
		break;
	    case Au_SetBucketAttributes:
		SetBucketAttributes(buf);
		break;
	    case Au_CreateRadio:
		CreateRadio(buf);
		break;
	    case Au_DestroyRadio:
		DestroyRadio(buf);
		break;
	    case Au_ListRadios:
		ListRadios(buf);
		break;
	    case Au_GetRadioAttributes:
		GetRadioAttributes(buf);
		break;
	    case Au_SetRadioAttributes:
		SetRadioAttributes(buf);
		break;
	    case Au_CreateFlow:
		CreateFlow(buf);
		break;
	    case Au_DestroyFlow:
		DestroyFlow(buf);
		break;
	    case Au_GetFlowAttributes:
		GetFlowAttributes(buf);
		break;
	    case Au_SetFlowAttributes:
		SetFlowAttributes(buf);
		break;
	    case Au_GetElements:
		GetElements(buf);
		ReplyExpected(fd, major_op);
		break;
	    case Au_SetElements:
		SetElements(buf);
		break;
	    case Au_GetElementStates:
		GetElementStates(buf);
		ReplyExpected(fd, major_op);
		break;
	    case Au_SetElementStates:
		SetElementStates(buf);
		break;
	    case Au_GetElementParameters:
		GetElementParameters(buf);
		break;
	    case Au_SetElementParameters:
		SetElementParameters(buf);
		break;
	    case Au_WriteElement:
		WriteElement(buf);
		break;
	    case Au_ReadElement:
		ReadElement(buf);
		ReplyExpected(fd, major_op);
		break;
	    case Au_GrabComponent:
		GrabComponent(buf);
		break;
	    case Au_UngrabComponent:
		UngrabComponent(buf);
		break;
	    case Au_SendEvent:
		SendEvent(buf);
		break;
	    case Au_GetAllowedUsers:
		GetAllowedUsers(buf);
		break;
	    case Au_SetAllowedUsers:
		SetAllowedUsers(buf);
		break;
	    case Au_ListExtensions:
		ListExtensions(buf);
		break;
	    case Au_QueryExtension:
		QueryExtension(buf);
		break;
	    case Au_GetCloseDownMode:
		GetCloseDownMode(buf);
		ReplyExpected(fd, major_op);
		break;
	    case Au_SetCloseDownMode:
		SetCloseDownMode(buf);
		break;
	    case Au_KillClient:
		KillClient(buf);
		break;
	    case Au_GetServerTime:
		GetServerTime(buf);
		ReplyExpected(fd, major_op);
		break;
	    case Au_NoOperation:
		NoOperation(buf);
		break;
	    default:
		warn("Unimplemented request opcode");
		break;
	}
}

/* ************************************************************ */
/* */
/* */
/* ************************************************************ */

DecodeReply(fd, buf, n)
FD              fd;
unsigned char  *buf;
AuInt32            n;
{
    short           SequenceNumber;
    short           major_op;
    short           minor_op;

    SequenceNumber = IShort(&buf[2]);
    major_op = CheckReplyTable(fd, SequenceNumber);
    if (silent)
	return;
    if (!major_op)
    {
	warn("Unexpected reply");
	return;
    }
    minor_op = major_op & (short) 0x00ff;
    major_op = ((unsigned short) major_op) >> 8;
    SetIndentLevel(PRINTSERVER);
    if (Verbose > 3)
	DumpItem("Reply", fd, buf, n);
    if (((unsigned short) major_op) > 0x007f)
    {				/* XXX ? */
	RBf[0] = minor_op;	/* for PrintField in Reply proc */
	ExtensionDecodeReply(fd, major_op, minor_op, buf);
    }
    else
    {
	RBf[0] = major_op;	/* for PrintField in Reply proc */
	switch (major_op)
	{
	    case Au_ReadElement:
		ReadElementReply(buf);
		break;
	    case Au_GetDeviceAttributes:
		GetDeviceAttributesReply(buf);
		break;
	    case Au_GetBucketAttributes:
		GetBucketAttributesReply(buf);
		break;
	    case Au_GetElements:
		GetElementsReply(buf);
		break;
	    case Au_GetElementStates:
		GetElementStatesReply(buf);
		break;
	    case Au_ListDevices:
		ListDevicesReply(buf);
		break;
	    case Au_ListBuckets:
		ListBucketsReply(buf);
		break;
	    case Au_GetCloseDownMode:
		GetCloseDownModeReply(buf);
		break;
	    case Au_GetServerTime:
		GetServerTimeReply(buf);
		break;
	    default:
		warn("Unimplemented reply opcode");
		break;
	}
    }
}

/* ************************************************************ */
/* */
/* */
/* ************************************************************ */

DecodeError(fd, buf, n)
FD              fd;
unsigned char  *buf;
AuInt32            n;
{
    short           error;

    if (silent)
	return;

    error = IByte(&buf[1]);
    SetIndentLevel(PRINTSERVER);
    if (Verbose > 3)
	DumpItem("Error", fd, buf, n);
    (void) CheckReplyTable(fd, (short) IShort(&buf[2]));
    if ((error < AuBadRequest) || (error > AuLastError))
	ExtensionDecodeError(fd, error, buf);
    else
	switch (error)
	{
	    case AuBadRequest:
		RequestError(buf);
		break;
	    case AuBadValue:
		ValueError(buf);
		break;
	    case AuBadDevice:
		DeviceError(buf);
		break;
	    case AuBadBucket:
		BucketError(buf);
		break;
	    case AuBadFlow:
		FlowError(buf);
		break;
	    case AuBadElement:
		ElementError(buf);
		break;
	    case AuBadMatch:
		MatchError(buf);
		break;
	    case AuBadAccess:
		AccessError(buf);
		break;
	    case AuBadAlloc:
		AllocError(buf);
		break;
	    case AuBadConnection:
		ConnectionError(buf);
		break;
	    case AuBadIDChoice:
		IDChoiceError(buf);
		break;
	    case AuBadName:
		NameError(buf);
		break;
	    case AuBadLength:
		LengthError(buf);
		break;
	    case AuBadImplementation:
		ImplementationError(buf);
		break;
	    default:
		warn("Unimplemented error code");
		break;
	}
}

/* ************************************************************ */
/* */
/* */
/* ************************************************************ */

DecodeEvent(fd, buf, n)
FD              fd;
unsigned char  *buf;
AuInt32            n;
{
    short           event;

    if (silent)
	return;

    event = IByte(&buf[0]);
    SetIndentLevel(PRINTSERVER);
    if (Verbose > 3)
	DumpItem("Event", fd, buf, n);
    if (IsSendEvent = (event & 0x80))
	buf[0] = event &= 0x7f;
    if (event < AuFirstEventType || event > AuLastEventType)
	ExtensionDecodeEvent(fd, event, buf);
    else
	switch (event)
	{
	    case AuEventTypeElementNotify:
		ElementNotifyEvent(buf);
		break;
	    case AuEventTypeMonitorNotify:
		MonitorNotifyEvent(buf);
		break;
	    case AuEventTypeBucketNotify:
		BucketNotifyEvent(buf);
		break;
	    case AuEventTypeDeviceNotify:
		DeviceNotifyEvent(buf);
		break;
	    default:
		warn("Unimplemented event code");
		break;
	}
}
