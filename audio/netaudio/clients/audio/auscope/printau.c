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
 * $NCDId: @(#)printau.c,v 1.21 1993/12/28 22:15:53 greg Exp $
 */

/* ************************************************** *
 *						      *
 *  Audio Request, Reply, Event, Error Printing	      *
 *  based on xscope by
 *						      *
 *	James Peterson, 1988			      *
 *	(c) Copyright MCC, 1988 		      *
 *						      *
 * ************************************************** */

#include "scope.h"
#include "au.h"

/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

/*
  In printing the contents of the fields of the Audio packets, some
  fields are of obvious value, and others are of lesser value.  To
  control the amount of output, we generate our output according
  to the level of Verbose-ness that was selected by the user.

  Verbose = 0 ==  Headers only, time and request/reply/... names.

  Verbose = 1 ==  Very useful content fields.

  Verbose = 2 ==  Almost everything.

  Verbose = 3 ==  Every single bit and byte.

*/

/*
  To aid in making the choice between level 1 and level 2, we
  define the following define, which does not print relatively
  unimportant fields.
*/

#define printField(a,b,c,d,e) if (Verbose > 1) PrintField(a,b,c,d,e)


#define PRINT_REQUEST							       \
{									       \
    PrintField(buf, 0, 1, REQUEST, REQUESTHEADER);			       \
									       \
    if (Verbose < 1)							       \
	return;								       \
									       \
    if (Verbose > 1)							       \
	PrintField(SBf, 0, 4, INT32, "sequence number");		       \
									       \
    printField(buf, 2, 2, CONST2(2), "request length");			       \
}

#define PRINT_RESOURCE_REQUEST(type, string)				       \
{									       \
    PRINT_REQUEST;							       \
									       \
    PrintField(buf, 4, 4, type, string);				       \
}

#define PRINT_REPLY							       \
{									       \
    PrintField(RBf, 0, 1, REPLY, REPLYHEADER);				       \
									       \
    if (Verbose < 1)							       \
	return;								       \
									       \
    if (Verbose > 1)							       \
    {									       \
	PrintField (buf, 2, 2, CARD16, "sequence number");		       \
        PrintField (buf, 4, 4, CARD32, "reply length");			       \
    }									       \
}

/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

PrintSetUpMessage(buf)
    unsigned char *buf;
{
    short       n;
    short       d;

    enterprocedure("PrintSetUpMessage");
    if (silent)
	return;
    if (Verbose < 1)
	return;
    SetIndentLevel(PRINTCLIENT);
    PrintField(buf, 0, 1, BYTEMODE, "byte-order");
    PrintField(buf, 2, 2, CARD16, "major-version");
    PrintField(buf, 4, 2, CARD16, "minor-version");
    printField(buf, 6, 2, DVALUE2(n), "length of name");
    n = IShort(&buf[6]);
    printField(buf, 8, 2, DVALUE2(d), "length of data");
    d = IShort(&buf[8]);
    PrintString8(&buf[12], (AuInt32) n, "authorization-protocol-name");
    PrintString8(&buf[pad((AuInt32) (12 + n))], (AuInt32) d, "authorization-protocol-data");
}

PrintSetUpReply(buf)
    unsigned char *buf;
{
    enterprocedure("PrintSetUpReply");
    SetIndentLevel(PRINTSERVER);
    if (IByte(&buf[0]))
	PrintSuccessfulSetUpReply(buf);
    else
	PrintFailedSetUpReply(buf);
}

PrintFailedSetUpReply(buf)
    unsigned char *buf;
{
    short       n;

    PrintField(buf, 0, 1, 0, "SetUp Failed");
    if (Verbose < 1)
	return;
    printField(buf, 1, 1, DVALUE1(n), "length of reason in bytes");
    n = IByte(&buf[1]);
    PrintField(buf, 2, 2, CARD16, "major-version");
    PrintField(buf, 4, 2, CARD16, "minor-version");
    printField(buf, 6, 2, DVALUE2((n + p) / 4), "length of data");
    PrintString8(&buf[8], (AuInt32) n, "reason");
}

PrintSuccessfulSetUpReply(buf)
unsigned char      *buf;
{
    short               numBytesVendor,
                        numFormats,
                        numElementTypes,
                        numWaveForms,
                        numActions,
                        numDevices,
                        numBuckets,
                        numRadios;
    unsigned char      *p;

    if (silent)
	return;

    if (Verbose < 1)
	return;
    PrintField(buf, 2, 2, CARD16, "protocol-major-version");
    PrintField(buf, 4, 2, CARD16, "protocol-minor-version");
    printField(buf, 6, 2, DVALUE2(8 + (v + p + m) / 4), "length of data");
    PrintField(buf, 8, 4, CARD32, "release-number");
    PrintField(buf, 12, 4, CARD32, "resource-id-base");
    PrintField(buf, 16, 4, CARD32, "resource-id-mask");
    PrintField(buf, 20, 2, CARD16, "minimum sample rate");
    PrintField(buf, 22, 2, CARD16, "maximum sample rate");
    PrintField(buf, 26, 2, CARD16, "maximum-request-length");
    PrintField(buf, 28, 1, CARD8, "max number of tracks");

    numBytesVendor = IShort(&buf[24]);
    numFormats = IByte(&buf[29]);
    numElementTypes = IByte(&buf[30]);
    numWaveForms = IByte(&buf[31]);
    numActions = IByte(&buf[32]);
    numDevices = IByte(&buf[33]);
    numBuckets = IByte(&buf[34]);
    numRadios = IByte(&buf[35]);

    p = &buf[SIZEOF(auConnSetupPrefix) + SIZEOF(auConnSetup)];

    PrintString8(p, numBytesVendor, "vendor");
    p += pad(numBytesVendor);

    (void) PrintListENUMERATED(p, numFormats, FORMAT, "formats", 1);
    p += pad(numFormats);

    (void) PrintListENUMERATED(p, numElementTypes, ELEMENT_TYPE,
			       "element types", 1);
    p += pad(numElementTypes);

    (void) PrintListENUMERATED(p, numWaveForms, WAVE_FORM, "wave forms", 1);
    p += pad(numWaveForms);

    (void) PrintListENUMERATED(p, numActions, ACTION_TYPE, "actions", 1);
    p += pad(numActions);

    p += PrintList(p, numDevices, DEVICE_ATTRIBUTES, "devices");
    p += PrintList(p, numBuckets, BUCKET_ATTRIBUTES, "buckets");
}


/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

char       *REQUESTHEADER = "............REQUEST";
char       *ERRORHEADER = "..............ERROR";
char       *REPLYHEADER = "..............REPLY";

char       *EVENTHDR = "..............EVENT";

/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

/* Error Printing procedures */

RequestError(buf)
    unsigned char *buf;
{
    PrintField(buf, 1, 1, ERROR, ERRORHEADER) /* Request */ ;
    if (Verbose < 1)
	return;
    printField(buf, 2, 2, CARD16, "sequence number");
    PrintField(buf, 4, 4, TIMESTAMP, "time");
    PrintField(buf, 10, 2, CARD16, "minor opcode");
    PrintField(buf, 12, 1, CARD8, "major opcode");
}

ValueError(buf)
    unsigned char *buf;
{
    PrintField(buf, 1, 1, ERROR, ERRORHEADER) /* Value */ ;
    if (Verbose < 1)
	return;
    printField(buf, 2, 2, CARD16, "sequence number");
    PrintField(buf, 4, 4, TIMESTAMP, "time");
    PrintField(buf, 8, 4, INT32, "bad value");
    PrintField(buf, 10, 2, CARD16, "minor opcode");
    PrintField(buf, 12, 1, CARD8, "major opcode");
}

DeviceError(buf)
    unsigned char *buf;
{
    PrintField(buf, 1, 1, ERROR, ERRORHEADER) /* Device */ ;
    if (Verbose < 1)
	return;
    printField(buf, 2, 2, CARD16, "sequence number");
    PrintField(buf, 4, 4, TIMESTAMP, "time");
    PrintField(buf, 4, 4, CARD32, "bad device id");
    PrintField(buf, 10, 2, CARD16, "minor opcode");
    PrintField(buf, 12, 1, CARD8, "major opcode");
}

BucketError(buf)
    unsigned char *buf;
{
    PrintField(buf, 1, 1, ERROR, ERRORHEADER) /* Bucket */ ;
    if (Verbose < 1)
	return;
    printField(buf, 2, 2, CARD16, "sequence number");
    PrintField(buf, 4, 4, TIMESTAMP, "time");
    PrintField(buf, 4, 4, CARD32, "bad bucket id");
    PrintField(buf, 10, 2, CARD16, "minor opcode");
    PrintField(buf, 12, 1, CARD8, "major opcode");
}

FlowError(buf)
    unsigned char *buf;
{
    PrintField(buf, 1, 1, ERROR, ERRORHEADER) /* Flow */ ;
    if (Verbose < 1)
	return;
    printField(buf, 2, 2, CARD16, "sequence number");
    PrintField(buf, 4, 4, TIMESTAMP, "time");
    PrintField(buf, 8, 4, CARD32, "bad flow id");
    PrintField(buf, 10, 2, CARD16, "minor opcode");
    PrintField(buf, 12, 1, CARD8, "major opcode");
}

ElementError(buf)
    unsigned char *buf;
{
    PrintField(buf, 1, 1, ERROR, ERRORHEADER) /* Element */ ;
    if (Verbose < 1)
	return;
    printField(buf, 2, 2, CARD16, "sequence number");
    PrintField(buf, 4, 4, TIMESTAMP, "time");
    PrintField(buf, 8, 4, CARD32, "bad element id");
    PrintField(buf, 10, 2, CARD16, "minor opcode");
    PrintField(buf, 12, 1, CARD8, "major opcode");
}

MatchError(buf)
    unsigned char *buf;
{
    PrintField(buf, 1, 1, ERROR, ERRORHEADER) /* Match */ ;
    if (Verbose < 1)
	return;
    printField(buf, 2, 2, CARD16, "sequence number");
    PrintField(buf, 4, 4, TIMESTAMP, "time");
    PrintField(buf, 10, 2, CARD16, "minor opcode");
    PrintField(buf, 12, 1, CARD8, "major opcode");
}

AccessError(buf)
    unsigned char *buf;
{
    PrintField(buf, 1, 1, ERROR, ERRORHEADER) /* Access */ ;
    if (Verbose < 1)
	return;
    printField(buf, 2, 2, CARD16, "sequence number");
    PrintField(buf, 4, 4, TIMESTAMP, "time");
    PrintField(buf, 10, 2, CARD16, "minor opcode");
    PrintField(buf, 12, 1, CARD8, "major opcode");
}

AllocError(buf)
    unsigned char *buf;
{
    PrintField(buf, 1, 1, ERROR, ERRORHEADER) /* Alloc */ ;
    if (Verbose < 1)
	return;
    printField(buf, 2, 2, CARD16, "sequence number");
    PrintField(buf, 4, 4, TIMESTAMP, "time");
    PrintField(buf, 10, 2, CARD16, "minor opcode");
    PrintField(buf, 12, 1, CARD8, "major opcode");
}

ConnectionError(buf)
    unsigned char *buf;
{
    PrintField(buf, 1, 1, ERROR, ERRORHEADER) /* Connection */ ;
    if (Verbose < 1)
	return;
    printField(buf, 2, 2, CARD16, "sequence number");
    PrintField(buf, 4, 4, TIMESTAMP, "time");
    PrintField(buf, 10, 2, CARD16, "minor opcode");
    PrintField(buf, 12, 1, CARD8, "major opcode");
}

IDChoiceError(buf)
    unsigned char *buf;
{
    PrintField(buf, 1, 1, ERROR, ERRORHEADER) /* IDChoice */ ;
    if (Verbose < 1)
	return;
    printField(buf, 2, 2, CARD16, "sequence number");
    PrintField(buf, 4, 4, TIMESTAMP, "time");
    PrintField(buf, 8, 4, CARD32, "bad resource id");
    PrintField(buf, 10, 2, CARD16, "minor opcode");
    PrintField(buf, 12, 1, CARD8, "major opcode");
}

NameError(buf)
    unsigned char *buf;
{
    PrintField(buf, 1, 1, ERROR, ERRORHEADER) /* Name */ ;
    if (Verbose < 1)
	return;
    printField(buf, 2, 2, CARD16, "sequence number");
    PrintField(buf, 4, 4, TIMESTAMP, "time");
    PrintField(buf, 10, 2, CARD16, "minor opcode");
    PrintField(buf, 12, 1, CARD8, "major opcode");
}

LengthError(buf)
    unsigned char *buf;
{
    PrintField(buf, 1, 1, ERROR, ERRORHEADER) /* Length */ ;
    if (Verbose < 1)
	return;
    printField(buf, 2, 2, CARD16, "sequence number");
    PrintField(buf, 4, 4, TIMESTAMP, "time");
    PrintField(buf, 10, 2, CARD16, "minor opcode");
    PrintField(buf, 12, 1, CARD8, "major opcode");
}

ImplementationError(buf)
    unsigned char *buf;
{
    PrintField(buf, 1, 1, ERROR, ERRORHEADER) /* Implementation */ ;
    if (Verbose < 1)
	return;
    printField(buf, 2, 2, CARD16, "sequence number");
    PrintField(buf, 4, 4, TIMESTAMP, "time");
    PrintField(buf, 10, 2, CARD16, "minor opcode");
    PrintField(buf, 12, 1, CARD8, "major opcode");
}

/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

/* Event Printing procedures */

ElementNotifyEvent(buf)
    unsigned char *buf;
{
    PrintField(buf, 0, 1, EVENT, EVENTHEADER) /* ElementNotify */ ;
    if (Verbose < 1)
	return;
    printField(buf, 2, 2, CARD16, "sequence number");
    PrintField(buf, 4, 4, TIMESTAMP, "time");
    PrintField(buf, 8, 4, FLOWID, "flow");
    PrintField(buf, 12, 2, CARD16, "element-number");
    PrintField(buf, 14, 2, NOTIFY_EVENT_KIND, "kind");
    PrintField(buf, 16, 2, STATE_TYPE, "previous-state");
    PrintField(buf, 18, 2, STATE_TYPE, "current-state");
    PrintField(buf, 20, 2, REASON, "reason");
    PrintField(buf, 24, 4, CARD32, "number-of-bytes");
}

MonitorNotifyEvent(buf)
    unsigned char *buf;
{
    PrintField(buf, 0, 1, EVENT, EVENTHEADER) /* MonitorNotify */ ;
    if (Verbose < 1)
	return;
    printField(buf, 2, 2, CARD16, "sequence number");
    PrintField(buf, 4, 4, TIMESTAMP, "time");
    PrintField(buf, 8, 4, FLOWID, "flow");
    PrintField(buf, 12, 2, CARD16, "element-number");
    PrintField(buf, 14, 1, FORMAT, "format");
    PrintField(buf, 15, 1, CARD8, "num tracks");
    PrintField(buf, 16, 2, CARD16, "count");
    PrintField(buf, 18, 2, CARD16, "num fields");
}

BucketNotifyEvent(buf)
    unsigned char *buf;
{
    PrintField(buf, 0, 1, EVENT, EVENTHEADER) /* ElementNotify */ ;
    if (Verbose < 1)
	return;
    printField(buf, 2, 2, CARD16, "sequence number");
    PrintField(buf, 4, 4, TIMESTAMP, "time");
/* XXX */
    PrintField(buf, 8, 4, FLOWID, "flow");
    PrintField(buf, 12, 2, CARD16, "element-number");
    PrintField(buf, 14, 2, CARD16, "kind");
    PrintField(buf, 16, 2, CARD16, "previous-state");
    PrintField(buf, 18, 2, CARD16, "current-state");
    PrintField(buf, 20, 4, CARD32, "number-of-bytes");
}

DeviceNotifyEvent(buf)
    unsigned char *buf;
{
    PrintField(buf, 0, 1, EVENT, EVENTHEADER) /* ElementNotify */ ;
    if (Verbose < 1)
	return;
    printField(buf, 2, 2, CARD16, "sequence number");
    PrintField(buf, 4, 4, TIMESTAMP, "time");
/* XXX */
    PrintField(buf, 8, 4, FLOWID, "flow");
    PrintField(buf, 12, 2, CARD16, "element-number");
    PrintField(buf, 14, 2, CARD16, "kind");
    PrintField(buf, 16, 2, CARD16, "previous-state");
    PrintField(buf, 18, 2, CARD16, "current-state");
    PrintField(buf, 20, 4, CARD32, "number-of-bytes");
}

/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

/* Request and Reply Printing procedures */


void
ListDevices(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;

    PrintDEVICE_ATTRIBUTES(&buf[SIZEOF(auReq)]);
}

void
ListDevicesReply(buf)
unsigned char  *buf;
{
    PRINT_REPLY;

    PrintList(&buf[SIZEOF(auListDevicesReply)],
	      ILong(&buf[8]), DEVICE_ATTRIBUTES, "devices");
}

void
GetDeviceAttributes(buf)
unsigned char  *buf;
{
    PRINT_RESOURCE_REQUEST(DEVICEID, "device");
}

void
GetDeviceAttributesReply(buf)
unsigned char  *buf;
{
    PRINT_REPLY;

    PrintDEVICE_ATTRIBUTES(&buf[32]);
}

void
SetDeviceAttributes(buf)
unsigned char  *buf;
{
    PRINT_RESOURCE_REQUEST(DEVICEID, "device");

    PrintDEVICE_ATTRIBUTES(&buf[SIZEOF(auResourceReq)]);
}

void
CreateBucket(buf)
unsigned char  *buf;
{
    PRINT_RESOURCE_REQUEST(BUCKETID, "bucket");

    PrintBUCKET_ATTRIBUTES(&buf[SIZEOF(auResourceReq)]);
}

void
DestroyBucket(buf)
unsigned char  *buf;
{
    PRINT_RESOURCE_REQUEST(BUCKETID, "bucket");
}

void
ListBuckets(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;

    PrintBUCKET_ATTRIBUTES(&buf[SIZEOF(auReq)]);
}

void
ListBucketsReply(buf)
unsigned char  *buf;
{
    PRINT_REPLY;

    PrintList(&buf[SIZEOF(auListBucketsReply)],
	      ILong(&buf[8]), BUCKET_ATTRIBUTES, "buckets");
}

void
GetBucketAttributes(buf)
unsigned char  *buf;
{
    PRINT_RESOURCE_REQUEST(BUCKETID, "bucket");
}

void
GetBucketAttributesReply(buf)
unsigned char  *buf;
{
    PRINT_REPLY;

    PrintBUCKET_ATTRIBUTES(&buf[32]);
}

void
SetBucketAttributes(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;
}

void
CreateRadio(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;
}

void
DestroyRadio(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;
}

void
ListRadios(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;
}

void
GetRadioAttributes(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;
}

void
SetRadioAttributes(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;
}

void
CreateFlow(buf)
unsigned char  *buf;
{
    PRINT_RESOURCE_REQUEST(FLOWID, "flow");
}

void
DestroyFlow(buf)
unsigned char  *buf;
{
    PRINT_RESOURCE_REQUEST(FLOWID, "flow");
}

void
GetFlowAttributes(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;
}

void
SetFlowAttributes(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;
}

void
GetElements(buf)
unsigned char  *buf;
{
    PRINT_RESOURCE_REQUEST(FLOWID, "flow");
}

void
GetElementsReply(buf)
unsigned char  *buf;
{
    PRINT_REPLY;

    PrintField(buf, 1, 1, BOOL, "clocked");
    PrintField(buf, 8, 4, FLOWID, "flow");
    PrintList(&buf[SIZEOF(auGetElementsReply)],
	      ILong(&buf[12]), ELEMENT, "elements");
}

void
SetElements(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;

    PrintField(buf, 1, 1, BOOL, "clocked");
    PrintField(buf, 4, 4, FLOWID, "flow");
    PrintList(&buf[SIZEOF(auSetElementsReq)], ILong(&buf[8]), ELEMENT,
	      "elements");
}

void
GetElementStates(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;

    PrintList(&buf[SIZEOF(auGetElementStatesReq)], ILong(&buf[4]), GET_STATE,
	      "states");
}

void
GetElementStatesReply(buf)
unsigned char  *buf;
{
    PRINT_REPLY;

    PrintList(&buf[SIZEOF(auGetElementStatesReply)], ILong(&buf[8]), STATE,
	      "states");
}

void
SetElementStates(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;

    PrintList(&buf[SIZEOF(auSetElementStatesReq)], ILong(&buf[4]), STATE,
	      "states");
}

void
GetElementParameters(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;
}

void
SetElementParameters(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;

    PrintList(&buf[SIZEOF(auSetElementParametersReq)], ILong(&buf[4]),
	      PARAMETER, "parameters");
}

void
WriteElement(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;

    PrintField(buf, 4, 4, FLOWID, "flow");
    PrintField(buf, 1, 1, ELEMENT_NUM, "element");
    PrintField(buf, 8, 4, CARD32, "num bytes");
    PrintField(buf, 12, 1, TRANSFER_STATE, "state");
}

void
ReadElement(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;

    PrintField(buf, 4, 4, FLOWID, "flow");
    PrintField(buf, 1, 1, ELEMENT_NUM, "element");
    PrintField(buf, 8, 4, CARD32, "num bytes");
}

void
ReadElementReply(buf)
unsigned char  *buf;
{
    PRINT_REPLY;

    PrintField(buf, 8, 4, CARD32, "num bytes");
}

void
GrabComponent(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;
}

void
UngrabComponent(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;
}

void
SendEvent(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;
}

void
GetAllowedUsers(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;
}

void
SetAllowedUsers(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;
}

void
ListExtensions(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;
}

void
QueryExtension(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;
}

void
GetCloseDownMode(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;
}

void
GetCloseDownModeReply(buf)
unsigned char  *buf;
{
    PRINT_REPLY;

    PrintField(buf, 1, 1, CLOSEMODE, "mode");
}

void
SetCloseDownMode(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;

    PrintField(buf, 1, 1, CLOSEMODE, "mode");
}

void
KillClient(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;
}

void
GetServerTime(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;
}

void
GetServerTimeReply(buf)
unsigned char  *buf;
{
    PRINT_REPLY;

    PrintField(buf, 8, 4, TIMESTAMP, "time");
}

void
NoOperation(buf)
unsigned char  *buf;
{
    PRINT_REQUEST;
}
