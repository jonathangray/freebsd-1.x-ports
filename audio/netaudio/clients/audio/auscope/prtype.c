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
 * $NCDId: @(#)prtype.c,v 1.18 1993/08/16 19:00:41 greg Exp $
 */

/*
 * ************************************************** *
 * 
 * type printing for audio protocol based on *
 * 
 * Type Printing for X11 protocol		      * *
 * 
 * James Peterson, 1988			      * (c) Copyright MCC, 1988 		      * *
 * 
**************************************************
 */

#include "scope.h"
#include "au.h"

/*
 * For each of the types we need a way to print that type. Types are of
 * varieties:
 * 
 * (1) BUILTIN -- we have a separate routine to interpret and print each
 * built-in type. (2) ENUMERATED, SET -- we have one routine which prints,
 * given the data and the list of values. (3) RECORDS -- a separate routine
 * for each to print each field.
 * 
 */

/* ************************************************************ */
/* */
/* */
/* ************************************************************ */


/* print representation of a character for debugging */
char           *
printrep(c)
unsigned short  c;
{
    static char     pr[8];

    if (c < 32)
    {
	/* control characters */
	pr[0] = '^';
	pr[1] = c + 64;
	pr[2] = '\0';
    }
    else if (c < 127)
    {
	/* printing characters */
	pr[0] = c;
	pr[1] = '\0';
    }
    else if (c == 127)
	return ("<del>");
    else if (c <= 0377)
    {
	/* upper 128 codes from 128 to 255;  print as \ooo - octal  */
	pr[0] = '\\';
	pr[3] = '0' + (c & 7);
	c = c >> 3;
	pr[2] = '0' + (c & 7);
	c = c >> 3;
	pr[1] = '0' + (c & 3);
	pr[4] = '\0';
    }
    else
    {
	/* very large number -- print as 0xffff - 4 digit hex */
	(void) sprintf(pr, "0x%04x", c);
    }
    return (pr);
}


/* ************************************************************ */
/* */
/* */
/* ************************************************************ */

/*
 * we use indentation for two purposes:
 * 
 * (1) To show substructure of records inside records ... (2) To separate the
 * bytes from the client (on the left) from those from the server (on the
 * right).
 * 
 * Each indention level is one tab (8 spaces).
 */

#define MaxIndent 10
static char     Leader[MaxIndent + 1];
static short    CurrentLevel = 0;

SetIndentLevel(which)
short           which;
{
    short           i;

    if (which > MaxIndent)
	which = MaxIndent;
    if (which < 0)
	which = 0;
    if (which == CurrentLevel)
	return;

    /* set the indent level to <which> */
    /* -> set the Print Leader to <which> tabs */
    for (i = 0; i < which; i++)
	Leader[i] = '\t';
    Leader[which] = '\0';
    CurrentLevel = which;
}

ModifyIndentLevel(amount)
short           amount;
{
    SetIndentLevel(CurrentLevel + amount);
}

short
SizeofLeader()
{
    return (CurrentLevel * 8);
}

PrintLeaderName(name)
char           *name;
{
    fprintf(stdout, "%s%20s: ", Leader, name);
}

/* ************************************************************ */
/* */
/* */
/* ************************************************************ */

/* if we want verbose enough output, we will dump the buffer in hex */

DumpItem(name, fd, buf, n)
char           *name;
FD              fd;
unsigned char  *buf;
AuInt32            n;
{
    if (n == 0)
	return;

    if (silent)
	return;

    fprintf(stdout, "%s%20s (fd %d): ", Leader, name, fd);

    DumpHexBuffer(buf, n);
    fprintf(stdout, "\n");
}

/* ************************************************************ */
/* */
/* */
/* ************************************************************ */

PrintINT8(buf)
unsigned char  *buf;
{
    /* print a INT8 -- 8-bit signed integer */
    char            n = IByte(buf);

    fprintf(stdout, "%d", (int) n);
    return (1);
}

PrintINT16(buf)
unsigned char  *buf;
{
    /* print a INT16 -- 16-bit signed integer */
    short           n = IShort(buf);

    fprintf(stdout, "%d", (int) n);
    return (2);
}

PrintINT32(buf)
unsigned char  *buf;
{
    /* print a INT32 -- 32-bit signed integer */
    AuInt32            n = ILong(buf);

    fprintf(stdout, "%d", n);
    return (4);
}

/* ************************************************************ */

PrintUINT8(buf)
unsigned char  *buf;
{
    unsigned short  n;

    n = IByte(buf);
    fprintf(stdout, "%u", n);
    return (1);
}

PrintUINT16(buf)
unsigned char  *buf;
{
    AuUint32   n;

    n = IShort(buf);
    fprintf(stdout, "%u", n);
    return (2);
}

PrintUINT32(buf)
unsigned char  *buf;
{
    AuUint32   n;

    n = ILong(buf);
    fprintf(stdout, "%u", n);
    return (4);
}

/* ************************************************************ */

PrintCARD8(buf)
unsigned char  *buf;
{
    /* print a CARD8 -- 8-bit unsigned integer */
    short           n = IByte(buf);

    fprintf(stdout, "%02x", (unsigned) (n & 0xff));
    return (1);
}

PrintCARD16(buf)
unsigned char  *buf;
{
    /* print a CARD16 -- 16-bit unsigned integer */
    AuUint32   n = IShort(buf);

    fprintf(stdout, "%04x", (unsigned) (n & 0xffff));
    return (2);
}

PrintCARD32(buf)
unsigned char  *buf;
{
    /* print a CARD32 -- 32-bit unsigned integer */
    AuUint32   n = ILong(buf);

    fprintf(stdout, "%08x", n);
    return (4);
}

/* ************************************************************ */

PrintBYTE(buf)
unsigned char  *buf;
{
    /* print a BYTE -- 8-bit value */
    short           n = IByte(buf);

    fprintf(stdout, "%02x", n);
    return (1);
}


PrintCHAR8(buf)
unsigned char  *buf;
{
    /* print a CHAR8 -- 8-bit character */
    unsigned short  n = IByte(buf);

    fprintf(stdout, "%s", printrep(n));
    return (1);
}


PrintSTRING16(buf)
unsigned char  *buf;
{
    /* print a CHAR2B -- 16-bit character which is never byte-swapped */
    unsigned short  n = IChar2B(buf);

    fprintf(stdout, "%s", printrep(n));
    return (1);
}

PrintSTR(buf)
unsigned char  *buf;
{
    /* STR have the length (1 byte) then a string of CHAR8 */
    short           n;
    short           i;

    n = IByte(buf++);
    fprintf(stdout, "%d \"", n);
    for (i = 0; i < n; i++)
	fprintf(stdout, "%s", printrep(buf[i]));
    fprintf(stdout, "\"");
    return (n + 1);
}

/* ************************************************************ */


int
PrintAUID(buf)
unsigned char  *buf;
{
    /* print a AUID -- CARD32  plus 0 = None */
    AuInt32            n = ILong(buf);

    if (n == 0)
	fprintf(stdout, "None");
    else
	fprintf(stdout, "AuID %08x", n);

    return 4;
}

int
PrintFLOWID(buf)
unsigned char  *buf;
{
    /* print a FLOWID -- CARD32  plus 0 = None */
    AuInt32            n = ILong(buf);

    if (n == 0)
	fprintf(stdout, "None");
    else
	fprintf(stdout, "AuFlowID %08x", n);

    return 4;
}

int
PrintDEVICEID(buf)
unsigned char  *buf;
{
    /* print a DEVICEID -- CARD32  plus 0 = None */
    AuInt32            n = ILong(buf);

    if (n == 0)
	fprintf(stdout, "None");
    else
	fprintf(stdout, "AuDeviceID %08x", n);

    return 4;
}

int
PrintBUCKETID(buf)
unsigned char  *buf;
{
    /* print a BUCKETID -- CARD32  plus 0 = None */
    AuInt32            n = ILong(buf);

    if (n == 0)
	fprintf(stdout, "None");
    else
	fprintf(stdout, "AuBucketID %08x", n);

    return 4;
}



PrintTIMESTAMP(buf)
unsigned char  *buf;
{
    /* print a TIMESTAMP -- CARD32 plus 0 as the current time */
    AuInt32            n = ILong(buf);

    if (n == 0)
	fprintf(stdout, "CurrentTime");
    else
	fprintf(stdout, "TIM %08x", n);
}


PrintRESOURCEID(buf)
unsigned char  *buf;
{
    /* print a RESOURCEID -- CARD32 plus 0 = AllTemporary */
    AuInt32            n = ILong(buf);

    if (n == 0)
	fprintf(stdout, "AllTemporary");
    else
	fprintf(stdout, "RID %08x", n);
}


/* this is an interesting cheat -- we call DecodeEvent to print an event */
/* should work, but its never been tried */
PrintEVENTFORM(buf)
unsigned char  *buf;
{
    /* print an EVENT_FORM -- event format */
    DecodeEvent(-1, buf, (AuInt32) -1);
}

/* ************************************************************ */

PrintENUMERATED(buf, length, ValueList)
unsigned char  *buf;
short           length;
struct ValueListEntry *ValueList;
{
    AuInt32            n;
    struct ValueListEntry *p;

    if (length == 1)
	n = IByte(buf);
    else if (length == 2)
	n = IShort(buf);
    else
	n = ILong(buf);

    p = ValueList;
    while (p != NULL && p->Value != n)
	p = p->Next;

    if (p != NULL)
	fprintf(stdout, "%s", p->Name);
    else
	fprintf(stdout, "**INVALID** (%d)", n);
}

/* ************************************************************ */

PrintSET(buf, length, ValueList)
unsigned char  *buf;
short           length;
struct ValueListEntry *ValueList;
{
    AuUint32   n;
    struct ValueListEntry *p;
    Boolean         MatchesAll = false;
    Boolean         FoundOne = false;

    if (length == 1)
	n = IByte(buf);
    else if (length == 2)
	n = IShort(buf);
    else
	n = ILong(buf);

    if (n != 0)
    {
	/* first check if the value matches ALL of the bits. */
	MatchesAll = true;
	for (p = ValueList; MatchesAll && (p != NULL); p = p->Next)
	{
	    if ((p->Value & n) == 0)
		MatchesAll = false;
	}

	if (!MatchesAll)
	    /* if it matches some, but not all, print only those it matches */
	    for (p = ValueList; p != NULL; p = p->Next)
	    {
		if ((p->Value & n) != 0)
		{
		    if (FoundOne)
			fprintf(stdout, " | ");
		    fprintf(stdout, "%s", p->Name);
		    FoundOne = true;
		}
	    }
    }

    if (MatchesAll)
	fprintf(stdout, "<ALL>");
    else if (!FoundOne)
	fprintf(stdout, "0");
}

/* ************************************************************ */
/* */
/* */
/* ************************************************************ */

PrintField(buf, start, length, FieldType, name)
unsigned char  *buf;
short           start;
short           length;
short           FieldType;
char           *name;
{
    if (length == 0)
	return;

    PrintLeaderName(name);

    if (debuglevel & 8)
	DumpHexBuffer(&(buf[start]), (AuInt32) length);

    switch (TD[FieldType].Type)
    {
	case BUILTIN:
	    (*TD[FieldType].PrintProc) (&buf[start]);
	    break;

	case ENUMERATED:
	    PrintENUMERATED(&buf[start], length, TD[FieldType].ValueList);
	    break;

	case SET:
	    PrintSET(&buf[start], length, TD[FieldType].ValueList);
	    break;

	case RECORD:
	    ModifyIndentLevel(1);
	    fprintf(stdout, "\n");
	    if (Verbose < 3)
		return;
	    (*TD[FieldType].PrintProc) (&buf[start]);
	    ModifyIndentLevel(-1);
	    break;
    }
    fprintf(stdout, "\n");
    (void) fflush(stdout);
}

/* ************************************************************ */
/* */
/* */
/* ************************************************************ */

/*
 * print a list of things.  The things are of type <ListType>. They start at
 * <buf>.  There are <number> things in the list
 */

AuInt32
PrintList(buf, number, ListType, name)
unsigned char  *buf;
AuInt32            number;
short           ListType;
char           *name;
{
    AuInt32            n;
    AuInt32            i;
    AuInt32            sum;

    if (number == 0)
	return (0);

    fprintf(stdout, "%s%20s: (%d)\n", Leader, name, number);
    if (Verbose < 2)
	return (0);

    ModifyIndentLevel(1);
    sum = 0;
    for (i = 0; i < number; i++)
    {
	switch (TD[ListType].Type)
	{
	    case BUILTIN:
		fprintf(stdout, "%s%20s  ", Leader, "");
		n = (*TD[ListType].PrintProc) (buf);
		fputc('\n', stdout);
		break;
	    case RECORD:
		n = (*TD[ListType].PrintProc) (buf);
		break;
	    default:
		fprintf(stdout, "**INVALID**");
		n = 0;
		break;
	}
	buf = buf + n;
	sum = sum + n;
	fprintf(stdout, "%s---\n", Leader);
    }

    ModifyIndentLevel(-1);
    return (sum);
}

/*
 * print a list of enumerated things.  The things are of type <ListType>.
 * They start at <buf>.  There are <number> things in the list
 */

AuInt32
PrintListENUMERATED(buf, number, ListType, name, length)
unsigned char  *buf;
AuInt32            number;
short           ListType,
                length;
char           *name;
{
    AuInt32            i;
    AuInt32            sum;

    if (number == 0)
	return (0);

    fprintf(stdout, "%s%20s: (%d)\n", Leader, name, number);
    if (Verbose < 2)
	return (0);

    ModifyIndentLevel(2);
    sum = 0;
    for (i = 0; i < number; i++)
    {
	fprintf(stdout, "%s", Leader);
	PrintENUMERATED(buf, length, TD[ListType].ValueList);
	fputc('\n', stdout);
	buf = buf + length;
	sum = sum + length;
    }

    fprintf(stdout, "%s---\n", Leader);
    ModifyIndentLevel(-2);
    return (sum);
}

/* ************************************************************ */
/* */
/* */
/* ************************************************************ */

/*
 * print a list of STRs.  Similar to PrintList They start at <buf>.  There
 * are <number> things in the list
 */

PrintListSTR(buf, number, name)
unsigned char  *buf;
AuInt32            number;
char           *name;
{
    AuInt32            n;
    AuInt32            i;
    AuInt32            sum;

    if (number == 0)
	return;

    fprintf(stdout, "%s%20s: (%d)\n", Leader, name, number);
    if (Verbose < 2)
	return;

    ModifyIndentLevel(1);
    sum = 0;
    for (i = 0; i < number; i++)
    {
	fprintf(stdout, "%s", Leader);
	n = PrintSTR(buf);
	buf = buf + n;
	sum = sum + n;
	fprintf(stdout, "\n");
    }

    ModifyIndentLevel(-1);
    return;
}


/* ************************************************************ */
/* */
/* */
/* ************************************************************ */


PrintBytes(buf, number, name)
unsigned char   buf[];
AuInt32            number;
char           *name;
{
    /* print a list of BYTE -- 8-bit character */
    AuInt32            i;
    short           column;

    if (number == 0)
	return;

    fprintf(stdout, "%s%20s: ", Leader, name);
    column = SizeofLeader() + 25;
    for (i = 0; i < number; i++)
    {
	if (column > 80)
	{
	    if (Verbose < 2)
		break;
	    fprintf(stdout, "\n%s%20s: ", Leader, "");
	    column = SizeofLeader() + 25;
	}
	fprintf(stdout, "%02x ", ((unsigned int) buf[i]));
	column += 3;
    }
    fprintf(stdout, "\n");

    return;
}


/* ************************************************************ */
/* */
/* */
/* ************************************************************ */


/* print a String of CHAR8 -- 8-bit characters */

PrintString8(buf, number, name)
unsigned char   buf[];
AuInt32            number;
char           *name;
{
    AuInt32            i;

    if (number == 0)
	return;

    fprintf(stdout, "%s%20s: \"", Leader, name);
    for (i = 0; i < number; i++)
	fprintf(stdout, "%s", printrep(buf[i]));
    fprintf(stdout, "\"\n");
}


/* print a String of CHAR2B -- 16-bit characters */

PrintString16(buf, number, name)
unsigned char   buf[];
AuInt32            number;
char           *name;
{
    AuInt32            i;
    unsigned short  c;

    if (number == 0)
	return;

    fprintf(stdout, "%s%20s: \"", Leader, name);
    for (i = 0; i < number; i += 2)
    {
	c = IChar2B(&buf[i]);
	fprintf(stdout, "%s", printrep(c));
    }
    fprintf(stdout, "\"\n");
}

/* ************************************************************ */
/* */
/* */
/* ************************************************************ */

/*
 * A Value List is two things:
 * 
 * (1) A controlling bitmask.  For each one bit in the control, a value is in
 * the list. (2) A list of values.
 */

PrintValues(control, clength, ctype, values, name)
unsigned char  *control;
short           clength;
short           ctype;
unsigned char  *values;
char           *name;
{
    AuInt32            cmask;
    struct ValueListEntry *p;

    /* first get the control mask */
    if (clength == 1)
	cmask = IByte(control);
    else if (clength == 2)
	cmask = IShort(control);
    else
	cmask = ILong(control);

    /* now if it is zero, ignore and return */
    if (cmask == 0)
	return;

    /* there are bits in the controlling bitmask, figure out which */
    /* the ctype is a set type, so this code is similar to PrintSET */
    fprintf(stdout, "%s%20s:\n", Leader, name);
    ModifyIndentLevel(1);
    for (p = TD[ctype].ValueList; p != NULL; p = p->Next)
    {
	if ((p->Value & cmask) != 0)
	{
	    short           m = 4 - p->Length;

	    PrintField(values, m, p->Length, p->Type, p->Name);
	    values += 4;
	}
    }
    ModifyIndentLevel(-1);
}

/* ************************************************************ */
/* */
/* */
/* ************************************************************ */

#define MAXline 78

DumpHexBuffer(buf, n)
unsigned char  *buf;
AuInt32            n;
{
    short           i;
    short           column;
    char            h[6] /* one hex or octal character */ ;

    column = 27 + SizeofLeader();
    for (i = 0; i < n; i++)
    {
	/* get the hex representations */
	(void) sprintf(h, "%02x", (0xff & buf[i]));

	/* check if these characters will fit on this line */
	if ((column + strlen(h) + 1) > MAXline)
	{
	    /* line will be too long -- print it */
	    fprintf(stdout, "\n");
	    column = 0;
	}
	fprintf(stdout, "%s ", h);
	column += 3;
    }
}

static int
PrintAU_STRING(buf, v)
unsigned char  *buf,
              **v;
{
    int             len = ILong(&buf[4]);

    PrintField(buf, 0, 1, STRING_TYPE, "type");
    PrintString8(*v, len, "description");
    *v += pad(len);

    return SIZEOF(auString);
}

#define IfPrintField(a, b, c, d, e, m)					       \
{									       \
    if (mask & (m))							       \
	PrintField(a, b, c, d, e);					       \
}

static int
PrintCOMMON_PART(buf, v)
unsigned char  *buf,
              **v;
{
    AuUint32   mask = ILong(&buf[0]);

    IfPrintField(buf, 8, 4, AUID, "id", AuCompCommonIDMask);
    IfPrintField(buf, 12, 1, COMPONENT_KIND, "kind", AuCompCommonKindMask);
    IfPrintField(buf, 13, 1, COMPONENT_USE, "use", AuCompCommonUseMask);
    IfPrintField(buf, 14, 1, FORMAT, "format", AuCompCommonFormatMask);
    IfPrintField(buf, 15, 1, CARD8, "num tracks", AuCompCommonNumTracksMask);
    IfPrintField(buf, 16, 4, COMPONENT_ACCESS, "access",
		 AuCompCommonAccessMask);

    if (mask & AuCompCommonDescriptionMask)
	PrintAU_STRING(&buf[20], v);

    return SIZEOF(auCommonPart);
}

int
PrintDEVICE_ATTRIBUTES(buf)
unsigned char  *buf;
{
    unsigned char  *p = buf,
                   *v;
    AuUint32   mask = ILong(&buf[0]);

    v = &buf[SIZEOF(auDeviceAttributes)];
    p += PrintCOMMON_PART(buf, &v);

    IfPrintField(p, 0, 4, DEVICE_LOCATION, "location",
		 AuCompDeviceLocationMask);
    IfPrintField(p, 4, 4, CARD32, "gain", AuCompDeviceGainMask);
    IfPrintField(p, 8, 2, CARD16, "min_sample_rate",
		 AuCompDeviceMinSampleRateMask);
    IfPrintField(p, 10, 2, CARD16, "max_sample_rate",
		 AuCompDeviceMaxSampleRateMask);
    IfPrintField(p, 12, 1, DEVICE_LINE_MODE, "lineMode",
		 AuCompDeviceLineModeMask);

    if (mask & AuCompDeviceChildrenMask)
	v += PrintList(v, IByte(&p[13]), AUID, "children");

    return v - buf;
}

int
PrintBUCKET_ATTRIBUTES(buf)
unsigned char  *buf;
{
    unsigned char  *p = buf,
                   *v;
    AuUint32   mask = ILong(&buf[0]);

    v = &buf[SIZEOF(auBucketAttributes)];
    p += PrintCOMMON_PART(buf, &v);

    IfPrintField(p, 0, 2, CARD16, "sample rate", AuCompBucketSampleRateMask);
    IfPrintField(p, 4, 4, CARD32, "num samples", AuCompBucketNumSamplesMask);

    return v - buf;
}

int
PrintELEMENT_NUM(buf)
unsigned char  *buf;
{
    unsigned char           n = IByte(buf);

    if (n == AuElementAll)
	fprintf(stdout, "All");
    else
	PrintCARD8(buf);

    return 1;
}

int
PrintNUM_SAMPLES(buf)
unsigned char  *buf;
{
    unsigned char           n = ILong(buf);

    if (n == AuUnlimitedSamples)
	fprintf(stdout, "Unlimited");
    else
	PrintCARD32(buf);

    return 1;
}

int
PrintACTION(buf)
unsigned char  *buf;
{
    PrintField(buf, 5, 1, STATE_TYPE, "trigger state");
    PrintField(buf, 6, 1, STATE_TYPE, "trigger prev state");
    PrintField(buf, 7, 1, REASON, "trigger reason");
    PrintField(buf, 8, 1, ACTION_TYPE, "action");

    if (IByte(&buf[8]) == AuElementActionChangeState)
    {
	PrintField(buf, 0, 4, FLOWID, "flow");
	PrintField(buf, 4, 1, ELEMENT_NUM, "element");
	PrintField(buf, 9, 1, STATE_TYPE, "new state");
    }

    return SIZEOF(auElementAction);
}

int
PrintELEMENT(buf)
unsigned char  *buf;
{
    int             extra = 0;

    PrintField(buf, 0, 2, ELEMENT_TYPE, "type");

    switch (IShort(&buf[0]))
    {
	case AuElementTypeImportClient:
	    PrintField(buf, 2, 2, CARD16, "sample rate");
	    PrintField(buf, 4, 1, FORMAT, "format");
	    PrintField(buf, 5, 1, CARD8, "num tracks");
	    PrintField(buf, 6, 1, BOOL, "discard");
	    PrintField(buf, 8, 4, CARD32, "max samples");
	    PrintField(buf, 12, 4, CARD32, "low water mark");
	    extra += PrintList(&buf[SIZEOF(auElement)], ILong(&buf[16]), ACTION,
			       "actions");
	    break;
	case AuElementTypeImportDevice:
	    PrintField(buf, 2, 2, CARD16, "sample rate");
	    PrintField(buf, 4, 4, NUM_SAMPLES, "num samples");
	    PrintField(buf, 8, 4, DEVICEID, "device");
	    extra += PrintList(&buf[SIZEOF(auElement)], ILong(&buf[12]), ACTION,
			       "actions");
	    break;
	case AuElementTypeImportBucket:
	    PrintField(buf, 2, 2, CARD16, "sample rate");
	    PrintField(buf, 4, 4, NUM_SAMPLES, "num samples");
	    PrintField(buf, 8, 4, BUCKETID, "bucket");
	    PrintField(buf, 12, 4, CARD32, "offset");
	    extra += PrintList(&buf[SIZEOF(auElement)], ILong(&buf[16]), ACTION,
			       "actions");
	    break;
	case AuElementTypeImportWaveForm:
	    PrintField(buf, 2, 2, CARD16, "sample rate");
	    PrintField(buf, 4, 4, NUM_SAMPLES, "num samples");
	    PrintField(buf, 8, 1, WAVE_FORM, "wave form");
	    PrintField(buf, 12, 4, CARD32, "frequency");
	    extra += PrintList(&buf[SIZEOF(auElement)], ILong(&buf[16]), ACTION,
			       "actions");
	    break;
	case AuElementTypeBundle:
	    extra += PrintList(&buf[SIZEOF(auElement)], IShort(&buf[2]),
			       TRACK, "tracks");
	    break;
	case AuElementTypeMultiplyConstant:
	case AuElementTypeAddConstant:
	    PrintField(buf, 2, 2, CARD16, "input");
	    PrintField(buf, 4, 4, CARD32, "constant");
	    break;
	case AuElementTypeSum:
	    extra +=
		(PrintList(&buf[SIZEOF(auElement)], IShort(&buf[2]), CARD16,
			       "inputs") + 3) & ~3;
	    break;
	case AuElementTypeExportClient:
	    PrintField(buf, 2, 2, CARD16, "sample rate");
	    PrintField(buf, 4, 2, CARD16, "input");
	    PrintField(buf, 8, 1, FORMAT, "format");
	    PrintField(buf, 9, 1, CARD8, "num tracks");
	    PrintField(buf, 10, 1, BOOL, "discard");
	    PrintField(buf, 12, 4, CARD32, "max samples");
	    PrintField(buf, 16, 4, CARD32, "high water mark");
	    extra += PrintList(&buf[SIZEOF(auElement)], ILong(&buf[20]), ACTION,
			       "actions");
	    break;
	case AuElementTypeExportDevice:
	    PrintField(buf, 2, 2, CARD16, "sample rate");
	    PrintField(buf, 4, 2, CARD16, "input");
	    PrintField(buf, 8, 4, NUM_SAMPLES, "num samples");
	    PrintField(buf, 12, 4, DEVICEID, "device");
	    extra += PrintList(&buf[SIZEOF(auElement)], ILong(&buf[16]), ACTION,
			       "actions");
	    break;
	case AuElementTypeExportBucket:
	    PrintField(buf, 2, 2, CARD16, "input");
	    PrintField(buf, 4, 4, NUM_SAMPLES, "num samples");
	    PrintField(buf, 8, 4, BUCKETID, "bucket");
	    PrintField(buf, 12, 4, CARD32, "offset");
	    extra += PrintList(&buf[SIZEOF(auElement)], ILong(&buf[16]), ACTION,
			       "actions");
	    break;
	case AuElementTypeExportMonitor:
	    PrintField(buf, 2, 2, CARD16, "event rate");
	    PrintField(buf, 4, 2, CARD16, "input");
	    PrintField(buf, 8, 1, FORMAT, "format");
	    PrintField(buf, 9, 1, CARD8, "num tracks");
	    break;
    }

    return SIZEOF(auElement) + extra;
}

int
PrintSTATE(buf)
unsigned char  *buf;
{
    PrintField(buf, 0, 4, FLOWID, "flow");
    PrintField(buf, 4, 1, ELEMENT_NUM, "element");
    PrintField(buf, 5, 1, STATE_TYPE, "state");

    return SIZEOF(auElementState);
}

int
PrintGET_STATE(buf)
unsigned char  *buf;
{
    PrintField(buf, 0, 4, FLOWID, "flow");
    PrintField(buf, 4, 1, ELEMENT_NUM, "element");

    return SIZEOF(auElementState);
}

int
PrintPARAMETER(buf)
unsigned char  *buf;
{
    int extra;

    PrintField(buf, 0, 4, FLOWID, "flow");
    PrintField(buf, 4, 1, ELEMENT_NUM, "element");
    extra = PrintList(&buf[SIZEOF(auElementParameters)], IByte(&buf[5]), CARD32,
		      "parameters");

    return SIZEOF(auElementParameters);
}

int
PrintTRACK(buf)
unsigned char  *buf;
{
    PrintField(buf, 0, 1, ELEMENT_NUM, "element");
    PrintField(buf, 1, 1, CARD8, "track number");

    return SIZEOF(auInputTrack);
}
