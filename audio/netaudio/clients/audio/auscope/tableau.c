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
 * $NCDId: @(#)tableau.c,v 1.17 1993/08/16 19:00:56 greg Exp $
 */

/* ************************************************** *
 *						      *
 *  Table initialization for Audio protocol	      *
 *  based on                             	      *
 *  Table initialization for X11 protocol	      *
 *						      *
 *	James Peterson, 1988			      *
 *	(c) Copyright MCC, 1988 		      *
 *						      *
 * ************************************************** */

#include "scope.h"
#include "au.h"

/*
  To initialize for the Audio protocol, we need to create data structures
  describing the data types used by X11.
*/

/*
  There are about 100-128 data types for Audio.  This start with the simple
  INT8, INT16, INT32 (byte, short, long), and the CARD8, CARD16, CARD32
  (unsigned) and extend to records like RGB (a resource id, 3 color
  values and a bitmask to select a subset of the 3 color values).  Each
  data type has an assigned type index.  The type index identifies the
  type (with a #define in x11.h) and is used to access an entry in an
  array of type descriptors (TD).  Each type descriptor has the type name,
  the kind of type, and a procedure to print an object of that type.
  The print procedure for a type <foo> is named Print<foo>.  The kind of
  type is

  BUILTIN:      one of the primitive types.
  ENUMERATED:   value should be one of a small set of values.  This type
                needs a list of allowed values (and their print names).
  SET:          value is a bitmask of a small set of values.  Each value
                is a one-bit mask (and its print name).
  RECORD:       value is a record of fields of other types.

  The Type Descriptor array allows us to print a value if we know its type
  (index) and the bytes in memory that are its value.
*/


InitializeAudio()
{
    InitReplyQ();

    InitBuiltInTypes();
    InitEnumeratedTypes();
    InitSetTypes();
    InitValuesTypes();
    InitRecordTypes();
}

/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

/* define the various types */

TYPE 
DefineType(typeid, class, name, printproc)
    short       typeid;
    short       class;
    char       *name;
    int         (*printproc) ();
{
    TD[typeid].Name = name;
    TD[typeid].Type = class;
    TD[typeid].ValueList = NULL;
    TD[typeid].PrintProc = printproc;
    return (&TD[typeid]);
}

/* ************************************************************ */
/* define an Enumerated Value (or a Set Value) */

DefineEValue(type, value, name)
    TYPE        type;
    AuInt32        value;
    char       *name;
{
    struct ValueListEntry *p;

    /* define the new value */
    p = (struct ValueListEntry *)
	malloc((AuInt32) (sizeof(struct ValueListEntry)));
    p->Name = name;
    p->Value = value;

    /* add an new value to the list. */
    if (type->ValueList == NULL || type->ValueList->Value > p->Value) {
	p->Next = type->ValueList;
	type->ValueList = p;
    } else {
	/* keep the list sorted, smallest to largest */
	struct ValueListEntry *q = type->ValueList;

	while (q->Next != NULL && q->Next->Value < p->Value)
	    q = q->Next;
	p->Next = q->Next;
	q->Next = p;
    }
}

/* ************************************************************ */
/* a Values list is like an enumerated Value, but has a type and length
   in addition to a value and name.  It is used to print a Values List */

/* A Values List is a bitmask (like a set), but if the bit is set on, then
   we have an associated value.  We need to know the length and type of the
   associated value for each bit */

DefineValues(type, value, length, ctype, name)
    TYPE        type;
    AuInt32        value;
    char       *name;
{
    struct ValueListEntry *p;

    p = (struct ValueListEntry *)
	malloc((AuInt32) (sizeof(struct ValueListEntry)));
    p->Name = name;
    p->Type = ctype;
    p->Length = length;
    p->Value = value;

    /* add an new value to the list. */
    if (type->ValueList == NULL || type->ValueList->Value > p->Value) {
	p->Next = type->ValueList;
	type->ValueList = p;
    } else {
	/* keep the list sorted, smallest to largest  */
	struct ValueListEntry *q = type->ValueList;

	while (q->Next != NULL && q->Next->Value < p->Value)
	    q = q->Next;
	p->Next = q->Next;
	q->Next = p;
    }
}



/* ************************************************************ */

InitBuiltInTypes()
{
    (void) DefineType(INT8, BUILTIN, "INT8", PrintINT8);
    (void) DefineType(INT16, BUILTIN, "INT16", PrintINT16);
    (void) DefineType(INT32, BUILTIN, "INT32", PrintINT32);
    (void) DefineType(UINT8, BUILTIN, "UINT8", PrintUINT8);
    (void) DefineType(UINT16, BUILTIN, "UINT16", PrintUINT16);
    (void) DefineType(UINT32, BUILTIN, "UINT32", PrintUINT32);
    (void) DefineType(CARD8, BUILTIN, "CARD8", PrintCARD8);
    (void) DefineType(CARD16, BUILTIN, "CARD16", PrintCARD16);
    (void) DefineType(CARD32, BUILTIN, "CARD32", PrintCARD32);
    (void) DefineType(BYTE, BUILTIN, "BYTE", PrintBYTE);
    (void) DefineType(CHAR8, BUILTIN, "CHAR8", PrintCHAR8);
    (void) DefineType(STRING16, BUILTIN, "STRING16", PrintSTRING16);
    (void) DefineType(STR, BUILTIN, "STR", PrintSTR);
    (void) DefineType(AUID, BUILTIN, "AUID", PrintAUID);
    (void) DefineType(FLOWID, BUILTIN, "FLOWID", PrintFLOWID);
    (void) DefineType(DEVICEID, BUILTIN, "DEVICEID", PrintDEVICEID);
    (void) DefineType(BUCKETID, BUILTIN, "BUCKETID", PrintBUCKETID);
    (void) DefineType(ELEMENT_NUM, BUILTIN, "ELEMENT_NUM", PrintELEMENT_NUM);
    (void) DefineType(NUM_SAMPLES, BUILTIN, "NUM_SAMPLES", PrintNUM_SAMPLES);
    (void) DefineType(TIMESTAMP, BUILTIN, "TIMESTAMP", PrintTIMESTAMP);
    (void) DefineType(RESOURCEID, BUILTIN, "RESOURCEID", PrintRESOURCEID);
    (void) DefineType(EVENTFORM, BUILTIN, "EVENTFORM", PrintEVENTFORM);
}

/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

InitEnumeratedTypes()
{
    TYPE            p;

    p = DefineType(REQUEST, ENUMERATED, "REQUEST", PrintENUMERATED);
    DefineEValue(p, Au_ListDevices, "ListDevices");
    DefineEValue(p, Au_GetDeviceAttributes, "GetDeviceAttributes");
    DefineEValue(p, Au_SetDeviceAttributes, "SetDeviceAttributes");

    DefineEValue(p, Au_CreateBucket, "CreateBucket");
    DefineEValue(p, Au_DestroyBucket, "DestroyBucket");
    DefineEValue(p, Au_ListBuckets, "ListBuckets");
    DefineEValue(p, Au_GetBucketAttributes, "GetBucketAttributes");
    DefineEValue(p, Au_SetBucketAttributes, "SetBucketAttributes");

    DefineEValue(p, Au_CreateFlow, "CreateFlow");
    DefineEValue(p, Au_DestroyFlow, "DestroyFlow");

    DefineEValue(p, Au_GetFlowAttributes, "GetFlowAttributes");
    DefineEValue(p, Au_SetFlowAttributes, "SetFlowAttributes");
    DefineEValue(p, Au_GetElements, "GetElements");
    DefineEValue(p, Au_SetElements, "SetElements");
    DefineEValue(p, Au_GetElementStates, "GetElementStates");
    DefineEValue(p, Au_SetElementStates, "SetElementStates");
    DefineEValue(p, Au_GetElementParameters, "GetElementParameters");
    DefineEValue(p, Au_SetElementParameters, "SetElementParameters");

    DefineEValue(p, Au_WriteElement, "WriteElement");
    DefineEValue(p, Au_ReadElement, "ReadElement");

    DefineEValue(p, Au_ListExtensions, "ListExtensions");
    DefineEValue(p, Au_QueryExtension, "QueryExtension");
    DefineEValue(p, Au_GetCloseDownMode, "GetCloseDownMode");
    DefineEValue(p, Au_SetCloseDownMode, "SetCloseDownMode");
    DefineEValue(p, Au_KillClient, "KillClient");
    DefineEValue(p, Au_GetServerTime, "GetServerTime");
    DefineEValue(p, Au_NoOperation, "NoOperation");

    InitExtensionRequestsReplies(p);

    p = DefineType(REPLY, ENUMERATED, "REPLY", PrintENUMERATED);
    DefineEValue(p, Au_ListDevices, "ListDevices");
    DefineEValue(p, Au_GetDeviceAttributes, "GetDeviceAttributes");
    DefineEValue(p, Au_ListBuckets, "ListBuckets");
    DefineEValue(p, Au_GetBucketAttributes, "GetBucketAttributes");
    DefineEValue(p, Au_GetFlowAttributes, "GetFlowAttributes");
    DefineEValue(p, Au_GetElements, "GetElements");
    DefineEValue(p, Au_GetElementStates, "GetElementStates");
    DefineEValue(p, Au_GetElementParameters, "GetElementParameters");
    DefineEValue(p, Au_ReadElement, "ReadElement");
    DefineEValue(p, Au_ListExtensions, "ListExtensions");
    DefineEValue(p, Au_QueryExtension, "QueryExtension");
    DefineEValue(p, Au_GetCloseDownMode, "GetCloseDownMode");
    DefineEValue(p, Au_GetServerTime, "GetServerTime");

    InitExtensionRequestsReplies(p);

    p = DefineType(ERROR, ENUMERATED, "ERROR", PrintENUMERATED);
    DefineEValue(p, AuBadRequest, "Request");
    DefineEValue(p, AuBadValue, "Value");
    DefineEValue(p, AuBadDevice, "Device");
    DefineEValue(p, AuBadBucket, "Bucket");
    DefineEValue(p, AuBadFlow, "Flow");
    DefineEValue(p, AuBadElement, "Element");
    DefineEValue(p, AuBadMatch, "Match");
    DefineEValue(p, AuBadAccess, "Access");
    DefineEValue(p, AuBadAlloc, "Alloc");
    DefineEValue(p, AuBadConnection, "Connection");
    DefineEValue(p, AuBadIDChoice, "IDChoice");
    DefineEValue(p, AuBadName, "Name");
    DefineEValue(p, AuBadLength, "Length");
    DefineEValue(p, AuBadImplementation, "Implementation");

    p = DefineType(EVENT, ENUMERATED, "EVENT", PrintENUMERATED);
    DefineEValue(p, AuEventTypeElementNotify, "ElementNotify");
    DefineEValue(p, AuEventTypeMonitorNotify, "MonitorNotify");
    DefineEValue(p, AuEventTypeBucketNotify, "BucketNotify");
    DefineEValue(p, AuEventTypeDeviceNotify, "DeviceNotify");

    p = DefineType(BOOL, ENUMERATED, "BOOL", PrintENUMERATED);
    DefineEValue(p, AuFalse, "False");
    DefineEValue(p, AuTrue, "True");

    p = DefineType(HOSTFAMILY, ENUMERATED, "HOSTFAMILY", PrintENUMERATED);
    DefineEValue(p, AuNetworkInternet, "Internet");
    DefineEValue(p, AuNetworkDECnet, "DECnet");
    DefineEValue(p, AuNetworkChaos, "Chaos");

    p = DefineType(NO_YES, ENUMERATED, "NO_YES", PrintENUMERATED);
    DefineEValue(p, 0L, "No");
    DefineEValue(p, 1L, "Yes");
    DefineEValue(p, 2L, "Default");

    p = DefineType(ALLORNONE, ENUMERATED, "ALLORNONE", PrintENUMERATED);
    DefineEValue(p, 0L, "None");
    DefineEValue(p, 1L, "All");

    p = DefineType(OFF_ON, ENUMERATED, "OFF_ON", PrintENUMERATED);
    DefineEValue(p, 0L, "Off");
    DefineEValue(p, 1L, "On");
    DefineEValue(p, 2L, "Default");

    p = DefineType(CLOSEMODE, ENUMERATED, "CLOSEMODE", PrintENUMERATED);
    DefineEValue(p, AuCloseDownDestroy, "Destroy");
    DefineEValue(p, AuCloseDownRetainPermanent, "RetainPermanent");
    DefineEValue(p, AuCloseDownRetainTemporary, "RetainTemporary");

    p = DefineType(BYTEMODE, ENUMERATED, "BYTEMODE", PrintENUMERATED);
    DefineEValue(p, 0x42L, "MSB first");
    DefineEValue(p, 0x6CL, "LSB first");

    p = DefineType(BYTEORDER, ENUMERATED, "BYTEORDER", PrintENUMERATED);
    DefineEValue(p, 0L, "LSB first");
    DefineEValue(p, 1L, "MSB first");

    p = DefineType(FORMAT, ENUMERATED, "FORMAT", PrintENUMERATED);
    DefineEValue(p, AuFormatULAW8, "ULAW8");
    DefineEValue(p, AuFormatLinearUnsigned8, "LinearUnsigned8");
    DefineEValue(p, AuFormatLinearSigned8, "LinearSigned8");
    DefineEValue(p, AuFormatLinearSigned16MSB, "LinearSigned16MSB");
    DefineEValue(p, AuFormatLinearUnsigned16MSB, "LinearUnsigned16MSB");
    DefineEValue(p, AuFormatLinearSigned16LSB, "LinearSigned16LSB");
    DefineEValue(p, AuFormatLinearUnsigned16LSB, "LinearUnsigned16LSB");

    p = DefineType(ELEMENT_TYPE, ENUMERATED, "ELEMENT_TYPE", PrintENUMERATED);
    DefineEValue(p, AuElementTypeImportClient, "ImportClient");
    DefineEValue(p, AuElementTypeImportDevice, "ImportDevice");
    DefineEValue(p, AuElementTypeImportBucket, "ImportBucket");
    DefineEValue(p, AuElementTypeImportWaveForm, "ImportWaveForm");
    DefineEValue(p, AuElementTypeImportRadio, "ImportRadio");
    DefineEValue(p, AuElementTypeBundle, "Bundle");
    DefineEValue(p, AuElementTypeMultiplyConstant, "MultiplyConstant");
    DefineEValue(p, AuElementTypeAddConstant, "AddConstant");
    DefineEValue(p, AuElementTypeSum, "Sum");
    DefineEValue(p, AuElementTypeExportClient, "ExportClient");
    DefineEValue(p, AuElementTypeExportDevice, "ExportDevice");
    DefineEValue(p, AuElementTypeExportBucket, "ExportBucket");
    DefineEValue(p, AuElementTypeExportRadio, "ExportRadio");
    DefineEValue(p, AuElementTypeExportMonitor, "ExportMonitor");

    p = DefineType(WAVE_FORM, ENUMERATED, "WAVE_FORM", PrintENUMERATED);
    DefineEValue(p, AuWaveFormSquare, "Square");
    DefineEValue(p, AuWaveFormSine, "Sine");
    DefineEValue(p, AuWaveFormSaw, "Saw");
    DefineEValue(p, AuWaveFormConstant, "Constant");

    p = DefineType(ACTION_TYPE, ENUMERATED, "ACTION_TYPE", PrintENUMERATED);
    DefineEValue(p, AuElementActionChangeState, "ChangeState");
    DefineEValue(p, AuElementActionSendNotify, "SendNotify");
    DefineEValue(p, AuElementActionNoop, "Noop");

    p = DefineType(COMPONENT_KIND, ENUMERATED, "COMPONENT_KIND",
		   PrintENUMERATED);
    DefineEValue(p, AuComponentKindPhysicalInput, "PhysicalInput");
    DefineEValue(p, AuComponentKindPhysicalOutput, "PhysicalOutput");
    DefineEValue(p, AuComponentKindBucket, "Bucket");
    DefineEValue(p, AuComponentKindRadio, "Radio");

    p = DefineType(COMPONENT_USE, SET, "COMPONENT_USE", PrintSET);
    DefineEValue(p, AuComponentUseImportMask, "Import");
    DefineEValue(p, AuComponentUseExportMask, "Export");

    p = DefineType(COMPONENT_ACCESS, SET, "COMPONENT_ACCESS", PrintSET);
    DefineEValue(p, AuAccessImportMask, "Import");
    DefineEValue(p, AuAccessExportMask, "Export");
    DefineEValue(p, AuAccessDestroyMask, "Destroy");
    DefineEValue(p, AuAccessListMask, "List");

    p = DefineType(STRING_TYPE, ENUMERATED, "STRING_TYPE", PrintENUMERATED);
    DefineEValue(p, AuStringLatin1, "Latin1");
    DefineEValue(p, AuStringCompoundText, "CompoundText");

    p = DefineType(DEVICE_LOCATION, SET, "DEVICE_LOCATION", PrintSET);
    DefineEValue(p, AuDeviceLocationLeftMask, "Left");
    DefineEValue(p, AuDeviceLocationCenterMask, "Center");
    DefineEValue(p, AuDeviceLocationRightMask, "Right");
    DefineEValue(p, AuDeviceLocationTopMask, "Top");
    DefineEValue(p, AuDeviceLocationMiddleMask, "Middle");
    DefineEValue(p, AuDeviceLocationBottomMask, "Bottom");
    DefineEValue(p, AuDeviceLocationBackMask, "Back");
    DefineEValue(p, AuDeviceLocationFrontMask, "Front");
    DefineEValue(p, AuDeviceLocationInternalMask, "Internal");
    DefineEValue(p, AuDeviceLocationExternalMask, "External");

    p = DefineType(DEVICE_LINE_MODE, ENUMERATED, "DEVICE_LINE_MODE",
		   PrintENUMERATED);
    DefineEValue(p, AuDeviceLineModeNone, "None");
    DefineEValue(p, AuDeviceLineModeLow, "Low");
    DefineEValue(p, AuDeviceLineModeHigh, "High");

    p = DefineType(STATE_TYPE, ENUMERATED, "STATE_TYPE", PrintENUMERATED);
    DefineEValue(p, AuStateStop, "Stop");
    DefineEValue(p, AuStateStart, "Start");
    DefineEValue(p, AuStatePause, "Pause");
    DefineEValue(p, AuStateAny, "Any");

    p = DefineType(NOTIFY_EVENT_KIND, ENUMERATED, "NOTIFY_EVENT_KIND",
		   PrintENUMERATED);
    DefineEValue(p, AuElementNotifyKindLowWater, "LowWater");
    DefineEValue(p, AuElementNotifyKindHighWater, "HighWater");
    DefineEValue(p, AuElementNotifyKindState, "State");

    p = DefineType(TRANSFER_STATE, ENUMERATED, "TRANSFER_STATE",
		   PrintENUMERATED);
    DefineEValue(p, AuTransferStateReady, "Ready");
    DefineEValue(p, AuTransferStatePending, "Pending");
    DefineEValue(p, AuTransferStateEnd, "End");

    p = DefineType(REASON, ENUMERATED, "REASON", PrintENUMERATED);
    DefineEValue(p, AuReasonUser, "User");
    DefineEValue(p, AuReasonUnderrun, "Underrun");
    DefineEValue(p, AuReasonOverrun, "Overrrun");
    DefineEValue(p, AuReasonEOF, "EOF");
    DefineEValue(p, AuReasonWatermark, "Watermark");
    DefineEValue(p, AuReasonHardware, "Hardware");
    DefineEValue(p, AuReasonAny, "Any");
}

/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

InitSetTypes()
{

/* XXX */

}


/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

/* Print Routines for builtin record types */

PrintHOST(buf)
    unsigned char *buf;
{
    short       n;

    PrintField(buf, 0, 1, HOSTFAMILY, "family");
    PrintField(buf, 2, 2, DVALUE2(n), "length of address");
    n = IShort(&buf[2]);
    (void) PrintList(&buf[4], (AuInt32) n, BYTE, "address");
    return (pad((AuInt32) (4 + n)));
}


/* ************************************************************ */

InitRecordTypes()
{
    (void) DefineType(HOST, RECORD, "HOST", PrintHOST);
    (void) DefineType(DEVICE_ATTRIBUTES, RECORD, "DEVICE_ATTRIBUTES",
		      PrintDEVICE_ATTRIBUTES);
    (void) DefineType(BUCKET_ATTRIBUTES, RECORD, "BUCKET_ATTRIBUTES",
		      PrintBUCKET_ATTRIBUTES);
    (void) DefineType(ELEMENT, RECORD, "ELEMENT", PrintELEMENT);
    (void) DefineType(ACTION, RECORD, "ACTION", PrintACTION);
    (void) DefineType(STATE, RECORD, "STATE", PrintSTATE);
    (void) DefineType(GET_STATE, RECORD, "GET_STATE", PrintGET_STATE);
    (void) DefineType(PARAMETER, RECORD, "PARAMETER", PrintPARAMETER);
    (void) DefineType(TRACK, RECORD, "TRACK", PrintTRACK);
}



/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

InitValuesTypes()
{
}
