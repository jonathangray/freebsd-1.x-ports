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
 * $NCDId: @(#)au.h,v 1.19 1993/11/03 19:23:35 greg Exp $
 */

/* ************************************************************ *
 *						     		*
 *  Type definitions and Connection State for the Audio protocol *
 *  based on				                        *
 *  Type definitions and Connection State for the  X11 protocol *
 *						      		*
 *	James Peterson, 1988			      		*
 *	(c) Copyright MCC, 1988 		      		*
 *						      		*
 * ************************************************************ */


/* Some field contents are constants, not just types */

#define CONST1(n)  CARD8
#define CONST2(n)  CARD16
#define CONST4(n)  CARD32

/* Some field contents define the components of an expression */

#define DVALUE1(expression)  CARD8
#define DVALUE2(expression)  CARD16
#define DVALUE4(expression)  CARD32


/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

/* Built-in Types */

enum {
	BYTE,			/* 8-bit value */
	INT8,			/* 8-bit signed integer */
	INT16,			/* 16-bit signed integer */
	INT32,			/* 32-bit signed integer */
	UINT8,			/* 8-bit unsigned integer */
	UINT16,			/* 16-bit usigned integer */
	UINT32,			/* 32-bit usigned integer */
	CARD8,			/* 8-bit unsigned integer */
	CARD16,			/* 16-bit unsigned integer */
	CARD32,			/* 32-bit unsigned integer */
	STRING8,		/* List of CARD8 */
	STRING16,		/* List of CHAR2B */

	AUID,			/* CARD32 plus 0 = None */
        FLOWID,			/* CARD32 plus 0 = None */
	DEVICEID,		/* CARD32 plus 0 = None */
	BUCKETID,		/* CARD32 plus 0 = None */

	ELEMENT_NUM,		/* CARD8 plus 255 = All */
	NUM_SAMPLES,		/* CARD32 plus 0 = Unlimited */

	TIMESTAMP,		/* CARD32 plus 0 as the current time */

	RESOURCEID,		/* CARD32 plus 0 = AllTemporary */

	EVENTFORM,		/* event format */
	CHAR8,			/* CARD8 interpreted as a character */
	STR,			/* String of CHAR8 with preceding length */

/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

/* Defined types */

	BOOL,
	HOSTFAMILY,
	NO_YES,
	ALLORNONE,
	OFF_ON,
	CLOSEMODE,
	HOST,
	BYTEMODE,
	BYTEORDER,
	FORMAT,
	ELEMENT_TYPE,
	WAVE_FORM,
	ACTION_TYPE,
	DEVICE_ATTRIBUTES,
	BUCKET_ATTRIBUTES,
	STRING_TYPE,
	COMPONENT_KIND,
	COMPONENT_USE,
	COMPONENT_ACCESS,
	DEVICE_LOCATION,
	DEVICE_LINE_MODE,
	ELEMENT,
	ACTION,
	STATE,
	GET_STATE,
	STATE_TYPE,
	NOTIFY_EVENT_KIND,
	TRANSFER_STATE,
	REASON,
	PARAMETER,
	TRACK,

	REQUEST,
	REPLY,
	ERROR,
	EVENT,

/* Include Extension type files here */
/*************************************/

/*************************************/

	MAXTYPEID
} TypeID;

#define EVENTHEADER (EVENTHDR)

/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

/* declaration of the existance of print routines for the basic types */

extern int PrintINT8();
extern int PrintINT16();
extern int PrintINT32();
extern int PrintUINT8();
extern int PrintUINT16();
extern int PrintUINT32();
extern int PrintCARD8();
extern int PrintCARD16();
extern int PrintCARD32();
extern int PrintBYTE();
extern int PrintCHAR8();
extern int PrintSTRING16();
extern int PrintTEXTITEM8();
extern int PrintTEXTITEM16();
extern int PrintSTR();
extern int PrintAUID();
extern int PrintFLOWID();
extern int PrintDEVICEID();
extern int PrintBUCKETID();
extern int PrintTIMESTAMP();
extern int PrintRESOURCEID();
extern int PrintEVENTFORM();
extern int PrintENUMERATED();
extern int PrintSET();
extern int PrintDEVICE_ATTRIBUTES();
extern int PrintBUCKET_ATTRIBUTES();
extern int PrintSTRING_TYPE();
extern int PrintCOMPONENT_KIND();
extern int PrintCOMPONENT_USE();
extern int PrintCOMPONENT_ACCESS();
extern int PrintDEVICE_LOCATION();
extern int PrintDEVICE_LINE_MODE();
extern int PrintELEMENT();
extern int PrintACTION();
extern int PrintELEMENT_NUM();
extern int PrintNUM_SAMPLES();
extern int PrintSTATE();
extern int PrintGET_STATE();
extern int PrintPARAMETER();
extern int PrintTRACK();

/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

/*  Type Definition Table

    Each item in the Audio Protocol has a type.  There are about 120
    different types.  We need to be able to print each item in a 
    format and interpretation which is appropriate for the type of
    that item.  To do so, we build a table describing each type.
    Each type has a name, possibly a list of named values and a
    procedure which knows how to print that type.
*/

/* Types of Types */

#define BUILTIN    1
#define ENUMERATED 2
#define SET        3
#define RECORD     5


/* Enumerated and Set types need a list of Named Values */

struct ValueListEntry
{
    struct ValueListEntry  *Next;
    char   *Name;
    short   Type;
    short   Length;
    AuInt32    Value;
};

struct TypeDef
{
    char   *Name;
    short   Type /* BUILTIN, ENUMERATED, SET, or RECORD */ ;
    struct ValueListEntry  *ValueList;
    int     (*PrintProc)();
};

typedef struct TypeDef *TYPE;

struct TypeDef  TD[MAXTYPEID];

/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

/* Reply Buffer: Pseudo-buffer used to provide the opcode for the
                 request to which this is a reply: Set by DecodeReply
		 and used in the PrintField of the Reply procedure */
unsigned char    RBf[2];


/* Sequence Buffer: Pseudo-buffer used to provide the sequence number for a
                 request: Set by DecodeReply and used in a PrintField of 
		 the Request procedure */
unsigned char    SBf[4];


#define PRINTSERVER 5	       /* indent output as if it comes from server */
#define PRINTCLIENT 1	       /* indent output as if it comes from client */

/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

/* 
  In general, we are called with a buffer of bytes and are supposed to
  try to make sense of these bytes according to the Audio protocol.  There
  are two different types of communication: requests from the client to
  the server and replies/errors/events from the server to the client.
  We must interpret these two differently.

  Also, we must consider that the bytes on the communication socket may
  be sent asynchronously in any amount.  This means that we must be prepared
  to read in and save some bytes until we get enough to do something with
  them.  For example, suppose that we get a buffer from a client.  We would
  expect it to be a request.  The request may be 24 bytes long.  We may find,
  however, that only 16 bytes have actually arrived -- the other 8 are stuck
  in a buffer somewhere.  We must be prepared to simply hold the 16 bytes we
  have until more bytes arrive.

  In general, we do two things: we wait for some number of bytes, and
  then we interpret this set of bytes.  To interpret this data we use 
  a modified state machine.  We keep two pieces of information:

  (1) the number of bytes that we need
  (2) what to do with those bytes.

  This last piece of information is the "state" of the interpretation.
  We keep the state as a pointer to the procedure that is to be executed.


  CLIENTS:

  The data going from the client to the x11 server consists of a
  set-up message followed by an infinite stream of variable length
  requests.  

  Our overall flow is then:

  (a) Wait for 12 bytes.
  (b) Interpret these first 12 bytes of the set-up message to get the
      length of the rest of the message.
  (c) Wait for the rest of the set-up message.
  (d) Interpret and print the set-up message.
  
  *** end of set-up phase -- start normal request loop ***

  (e) Wait for 4 bytes.
  (f) Interpret these 4 bytes to get the length of the rest of the command.
  (g) Wait for the rest of the command.
  (h) Interpret and print the command.
  (i) Go back to step (e).

  SERVERS:

  Again, we have a set-up reply followed by an infinite stream of variable
  length replies/errors/events.

  Our overall flow is then:

  (a) Wait for 8 bytes.
  (b) Interpret the 8 bytes to get the length of the rest of the set-up reply.
  (c) Wait for the rest of the set-up reply.
  (d) Interpret and print the set-up reply.

  *** end of set-up phase -- start normal reply/error/event loop ***

  We have the following properties of Audio replies, errors, and events:

  Replies:  32 bytes plus a variable amount.  Byte 0 is 1.
            Bytes 2-3 are a sequence number; bytes 4-7 are length (n).  The
	    complete length of the reply is 32 + 4 * n.

  Errors:   32 bytes.  Byte 0 is 0.
            Byte 1 is an error code; bytes 2-3 are a sequence number.
	    Bytes 8-9 are a major opcode; byte 10 is a minor opcode.

  Events:   32 bytes.  Byte 0 is 2, 3, 4, ....

  Looking at this we have two choices:  wait for one byte and then separately
  wait for replies, errors, and events, or wait for 32 bytes, then separately
  process each type.  We may have to wait for more, in the event of a reply.
  This latter seems more effective.  It appears reply/error/event formats
  were selected to allow waiting for 32 bytes, and it will allow short packets
  which are only 32 bytes long, to be processed completely in one step.
  
  Thus, For normal reply/error/event processing we have 

  (e) Wait for 32 bytes.
  (f) Interpret these 32 bytes.  If possible, go back to step (e).
  (g) If the packet is a reply with bytes 4-7 non-zero, wait for the
      remainder of the the reply.
  (h) Interpret and print the longer reply.  Go back to step (e).
  

  The similarity in approach to how both the client and server are handled
  suggests we can use the same control structure to drive the interpretation
  of both types of communication client->server and server->client.  
  Accordingly, we package up the relevant variables in a ConnState
  record.  The ConnState record contains the buffer of saved bytes (if any),
  the size and length of this buffer, the number of bytes we are waiting for
  and what to do when we get them.  A separate ConnState record is kept
  for the client and server.

  In addition, we may have several different client or server connections.
  Thus we need an array of all the necessary state for each client or server.
  A client/server is identified with a file descriptor (fd), so we use the
  fd to identify the client/server and use it as an index into an array of
  state variables.
*/

enum ConnectionByteOrderState {LSBFIRST, MSBFIRST};

struct ConnState
{
    unsigned char   *SavedBytes;
    AuInt32    SizeofSavedBytes;
    AuInt32    NumberofSavedBytes;

    AuInt32    NumberofBytesNeeded;
    AuInt32    (*ByteProcessing)();

    AuInt32    SequenceNumber;
    enum    ConnectionByteOrderState ByteOrder;
};

struct ConnState    CS[StaticMaxFD];

enum ConnectionByteOrderState CurrentConnectionByteOrder;


/* ************************************************************ */
/*								*/
/*								*/
/* ************************************************************ */

/* declaraction of the types of some common functions */

extern AuUint32    ILong();
extern unsigned short   IShort();
extern unsigned short   IChar2B();
extern unsigned short   IByte();
extern Boolean          IBool();

extern PrintString8();
extern PrintString16();
extern PrintListSTR();

extern AuInt32    PrintList();
extern AuInt32    pad();

#if defined(__STDC__) && !defined(UNIXCPP)
#define SIZEOF(x) sz_##x
#else
#define SIZEOF(x) sz_/**/x
#endif /* if ANSI C compiler else not */

/**
 * Sizes of various protocol structures.  This list is the result of the
 * following command:
 *
 *	grep '^#define sz_' Aproto.h
 */
#define sz_auConnClientPrefix		12
#define sz_auConnSetupPrefix		8
#define sz_auConnSetup			28
#define sz_auString 			8
#define sz_auCommonPart			(20 + sz_auString)
#define sz_auDevicePart			16
#define sz_auDeviceAttributes		(sz_auCommonPart + sz_auDevicePart)
#define sz_auBucketPart			8
#define sz_auBucketAttributes		(sz_auCommonPart + sz_auBucketPart)
#define sz_auRadioPart			4
#define sz_auRadioAttributes		(sz_auCommonPart + sz_auRadioPart)
#define sz_auReq			4
#define sz_auResourceReq		8
#define sz_auGetDeviceAttributesReply	32
#define sz_auGetBucketAttributesReply	32
#define sz_auListBucketsReply	32
#define sz_auListDevicesReply	32
#define sz_auGetElementsReply	32
#define sz_auElementAction 		12
#define sz_auElementActionList		4
#define sz_auElementImportClient	(16 + sz_auElementActionList)
#define sz_auElementImportDevice	(12 + sz_auElementActionList)
#define sz_auElementImportBucket	(16 + sz_auElementActionList)
#define sz_auElementImportWaveForm		(16 + sz_auElementActionList)
#define sz_auInputTrack			4
#define sz_auElementBundle		4
#define sz_auElementMultiplyConstant	8
#define sz_auElementAddConstant		8
#define sz_auElementSum			4
#define sz_auElementExportClient	(20 + sz_auElementActionList)
#define sz_auElementExportDevice	(16 + sz_auElementActionList)
#define sz_auElementExportBucket	(16 + sz_auElementActionList)
#define sz_auElementExportMonitor	12
#define sz_auElementMAX			sz_auElementExportClient
#define sz_auElement			sz_auElementMAX
#define sz_auSetElementsReq		12
#define sz_auElementState		8
#define sz_auSetElementStatesReq 	8
#define sz_auGetElementStatesReq 	8
#define sz_auGetElementStatesReply		32
#define sz_auWriteElementReq		16
#define sz_auReadElementReq		12
#define sz_auReadElementReply		32
#define sz_auElementParameters 		8
#define sz_auSetElementParametersReq	8
#define sz_auSetCloseDownModeReq 	4
#define sz_auGetCloseDownModeReply	32
#define sz_auEvent			32
#define sz_auError			32
#define sz_auReply			32
