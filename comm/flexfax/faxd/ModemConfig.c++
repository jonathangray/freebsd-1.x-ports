/*	$Header: /a/cvs/386BSD/ports/comm/flexfax/faxd/ModemConfig.c++,v 1.1 1993/08/31 23:42:46 ljo Exp $
/*
 * Copyright (c) 1990, 1991, 1992, 1993 Sam Leffler
 * Copyright (c) 1991, 1992, 1993 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Sam Leffler and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Sam Leffler and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 * 
 * IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */
#include "ModemConfig.h"
#include "t.30.h"
#include <string.h>

/*
 * We append the "@" symbol to the dial string so that
 * the modem will wait 5 seconds before attempting to
 * connect.  This makes it possible to get return codes
 * that more indicate if the phone was answered w/o a
 * carrier or wether the phone wasn't answered at all.
 * We then assume that if the phone was answered and
 * there was no carrier that a person must have answered.
 * (Kudos to Stuart Lynne for this trick.)
 *
 * Unfortunately, ZyXEL's don't seem to detect a busy
 * signal when the @ symbol is used to terminate the
 * dialing string.  Instead calls to a busy line come
 * back with a NO CARRIER status which causes us to
 * toss the job.  Consequently we must fall back to the
 * normal method and hope the conservative call status
 * checking in the mainstream server is sufficient.
 */
ModemConfig::ModemConfig()
    : type("unknown")
    , dialCmd("DT%s@")			// %s = phone number
    , noAutoAnswerCmd("S0=0")
    , echoOffCmd("E0")
    , verboseResultsCmd("V1")
    , resultCodesCmd("Q0")
    , onHookCmd("H0")
    , softResetCmd("Z")
    , waitTimeCmd("S7=30")		// wait time is 30 seconds
    , pauseTimeCmd("S8=2")		// comma pause time is 2 seconds
    , class2AbortCmd("K")		// class 2 session abort
{
    class2BORCmd = "BOR=" | fxStr(BOR_C_DIR + BOR_BD_DIR, "%u");
    class2XmitWaitForXON = FALSE;	// default suits most Class 2 modems

    // default volume setting commands
    setVolumeCmds("M0 L0M1 L1M1 L2M1 L3M1");

    answerCmd[FaxModem::ANSTYPE_ANY] = "A";

    flowControl = FaxModem::FLOW_XONXOFF;// expect XON/XOFF flow control
    maxRate = FaxModem::BR19200;	// reasonable for most modems
    // default to current baud rate
    recvRate[FaxModem::CALLTYPE_ERROR] = FaxModem::BR0;
    recvRate[FaxModem::CALLTYPE_FAX] = FaxModem::BR0;
    recvRate[FaxModem::CALLTYPE_DATA] = FaxModem::BR0;
    recvRate[FaxModem::CALLTYPE_VOICE] = FaxModem::BR0;
    sendFillOrder = FILLORDER_LSB2MSB;	// default to CCITT bit order
    recvFillOrder = FILLORDER_LSB2MSB;	// default to CCITT bit order
    frameFillOrder = FILLORDER_LSB2MSB;	// default to CCITT bit order
    hostFillOrder = FILLORDER_MSB2LSB;	// default to most common

    resetDelay = 2600;			// 2.6 second delay after reset
    baudRateDelay = 0;			// delay after setting baud rate

    t1Timer = TIMER_T1;			// T.30 T1 timer (ms)
    t2Timer = TIMER_T2;			// T.30 T2 timer (ms)
    t4Timer = TIMER_T4;			// T.30 T4 timer (ms)
    dialResponseTimeout = 3*60*1000;	// dialing command timeout (ms)
    answerResponseTimeout = 3*60*1000;	// answer command timeout (ms)
    pageStartTimeout = 3*60*1000;	// page send/receive timeout (ms)
    pageDoneTimeout = 3*60*1000;	// page send/receive timeout (ms)

    class1TCFResponseDelay = 75;	// 75ms delay between TCF and ack/nak
    class1SendPPMDelay = 75;		// 75ms delay before sending PPM
    class1SendTCFDelay = 75;		// 75ms delay between sending DCS & TCF
    class1TrainingRecovery = 1500;	// 1.5sec delay after failed training
    class1RecvAbortOK = 200;		// 200ms after abort before flushing OK
    class1FrameOverhead = 4;		// flags + station id + 2-byte FCS
    class1RecvIdentTimer = t1Timer;	// default to standard protocol

    maxPacketSize = 16*1024;		// max write to modem
    interPacketDelay = 0;		// delay between modem writes
}

ModemConfig::~ModemConfig()
{
}

#define	streq(a,b)	(strcasecmp(a,b)==0)

static fxBool getBoolean(const char* cp)
    { return (streq(cp, "on") || streq(cp, "yes")); }

static BaudRate
getRate(const char* cp)
{
    return (streq(cp, "300")	? FaxModem::BR300 :
	    streq(cp, "1200")	? FaxModem::BR1200 :
	    streq(cp, "2400")	? FaxModem::BR2400 :
	    streq(cp, "4800")	? FaxModem::BR4800 :
	    streq(cp, "9600")	? FaxModem::BR9600 :
	    streq(cp, "19200")	? FaxModem::BR19200 :
	    streq(cp, "38400")	? FaxModem::BR38400 :
	    streq(cp, "57600")	? FaxModem::BR57600 :
				  FaxModem::BR19200);	// default
}

static u_int
getFill(const char* cp)
{
    return (streq(cp, "LSB2MSB") ? FILLORDER_LSB2MSB :
	    streq(cp, "MSB2LSB") ? FILLORDER_MSB2LSB :
				   (u_int) -1);
}

static FlowControl
getFlow(const char* cp)
{
    return (streq(cp, "xonxoff") || streq(cp, "xon/xoff")
			? FaxModem::FLOW_XONXOFF :
	    streq(cp, "rtscts") || streq(cp, "rts/cts")
			? FaxModem::FLOW_RTSCTS :
			  FaxModem::FLOW_XONXOFF);	// default
}

void
ModemConfig::setVolumeCmds(const fxStr& tag)
{
    u_int l = 0;
    setVolumeCmd[FaxModem::OFF]    = tag.token(l, " \t");
    setVolumeCmd[FaxModem::QUIET]  = tag.token(l, " \t");
    setVolumeCmd[FaxModem::LOW]    = tag.token(l, " \t");
    setVolumeCmd[FaxModem::MEDIUM] = tag.token(l, " \t");
    setVolumeCmd[FaxModem::HIGH]   = tag.token(l, " \t");
}

void
ModemConfig::parseItem(const char* tag, const char* value)
{
    if (streq(tag, "ModemType"))		type = value;
    else if (streq(tag, "ModemResetCmds") || streq(tag, "ModemResetCmd"))
	resetCmds = value;
    else if (streq(tag, "ModemDialCmd"))	dialCmd = value;
    else if (streq(tag, "ModemAnswerCmd") || streq(tag, "ModemAnswerAnyCmd"))
	answerCmd[FaxModem::ANSTYPE_ANY] = value;
    else if (streq(tag, "ModemAnswerFaxCmd"))
	answerCmd[FaxModem::ANSTYPE_FAX] = value;
    else if (streq(tag, "ModemAnswerDataCmd"))
	answerCmd[FaxModem::ANSTYPE_DATA] = value;
    else if (streq(tag, "ModemAnswerVoiceCmd"))
	answerCmd[FaxModem::ANSTYPE_VOICE] = value;
    else if (streq(tag, "ModemFlowControlCmd"))	flowControlCmd = value;
    else if (streq(tag, "ModemSetupAACmd"))	setupAACmd = value;
    else if (streq(tag, "ModemSetupDTRCmd"))	setupDTRCmd = value;
    else if (streq(tag, "ModemSetupDCDCmd"))	setupDCDCmd = value;
    else if (streq(tag, "ModemNoAutoAnswerCmd"))noAutoAnswerCmd = value;
    else if (streq(tag, "ModemSetVolumeCmd"))	setVolumeCmds(value);
    else if (streq(tag, "ModemEchoOffCmd"))	echoOffCmd = value;
    else if (streq(tag, "ModemVerboseResultsCmd"))verboseResultsCmd = value;
    else if (streq(tag, "ModemResultCodesCmd"))	resultCodesCmd = value;
    else if (streq(tag, "ModemOnHookCmd"))	onHookCmd = value;
    else if (streq(tag, "ModemSoftResetCmd"))	softResetCmd = value;
    else if (streq(tag, "ModemHayesQueryCmd"))	hayesQueryCmd = value;
    else if (streq(tag, "ModemWaitTimeCmd"))	waitTimeCmd = value;
    else if (streq(tag, "ModemCommaPauseTimeCmd"))pauseTimeCmd = value;
    else if (streq(tag, "ModemFlowControl"))	flowControl = getFlow(value);
    else if (streq(tag, "ModemMaxRate"))	maxRate = getRate(value);
    else if (streq(tag, "ModemRecvRate") || streq(tag, "ModemFaxRecvRate"))
	recvRate[FaxModem::CALLTYPE_FAX] = getRate(value);
    else if (streq(tag, "ModemDataRecvRate"))
	recvRate[FaxModem::CALLTYPE_DATA] = getRate(value);
    else if (streq(tag, "ModemVoiceRecvRate"))
	recvRate[FaxModem::CALLTYPE_VOICE] = getRate(value);
    else if (streq(tag, "ModemRecvFillOrder"))	recvFillOrder = getFill(value);
    else if (streq(tag, "ModemSendFillOrder"))	sendFillOrder = getFill(value);
    else if (streq(tag, "ModemFrameFillOrder"))	frameFillOrder = getFill(value);
    else if (streq(tag, "ModemHostFillOrder"))	hostFillOrder = getFill(value);
    else if (streq(tag, "ModemResetDelay"))	resetDelay = atoi(value);
    else if (streq(tag, "ModemBaudRateDelay"))	baudRateDelay = atoi(value);
    else if (streq(tag, "ModemXONXOFF"))	// old way
	flowControl = (getBoolean(value) ?
			FaxModem::FLOW_XONXOFF : FaxModem::FLOW_RTSCTS);
    else if (streq(tag, "ModemMaxPacketSize"))	maxPacketSize = atoi(value);
    else if (streq(tag, "ModemInterPacketDelay"))interPacketDelay = atoi(value);
    else if (streq(tag, "ModemMfrQueryCmd"))	mfrQueryCmd = value;
    else if (streq(tag, "ModemModelQueryCmd"))	modelQueryCmd = value;
    else if (streq(tag, "ModemRevQueryCmd"))	revQueryCmd = value;

    else if (streq(tag, "FaxT1Timer"))		t1Timer = atoi(value);
    else if (streq(tag, "FaxT2Timer"))		t2Timer = atoi(value);
    else if (streq(tag, "FaxT4Timer"))		t4Timer = atoi(value);
    else if (streq(tag, "ModemDialResponseTimeout"))
	dialResponseTimeout = atoi(value);
    else if (streq(tag, "ModemAnswerResponseTimeout"))
	answerResponseTimeout = atoi(value);
    else if (streq(tag, "ModemPageStartTimeout"))
	pageStartTimeout = atoi(value);
    else if (streq(tag, "ModemPageDoneTimeout"))
	pageDoneTimeout = atoi(value);

    else if (streq(tag, "Class1TCFResponseDelay"))
	class1TCFResponseDelay = atoi(value);
    else if (streq(tag, "Class1SendPPMDelay"))
	class1SendPPMDelay = atoi(value);
    else if (streq(tag, "Class1SendTCFDelay"))
	class1SendTCFDelay = atoi(value);
    else if (streq(tag, "Class1TrainingRecovery"))
	class1TrainingRecovery = atoi(value);
    else if (streq(tag, "Class1RecvAbortOK"))
	class1RecvAbortOK = atoi(value);
    else if (streq(tag, "Class1FrameOverhead"))
	class1FrameOverhead = atoi(value);
    else if (streq(tag, "Class1RecvIdentTimer"))
	class1RecvIdentTimer = atoi(value);

	// Class 2-specific configuration controls
    else if (streq(tag, "Class2BORCmd"))
	class2BORCmd = value;
    else if (streq(tag, "Class2RELCmd"))
	class2RELCmd = value;
    else if (streq(tag, "Class2CQCmd"))
	class2CQCmd = value;
    else if (streq(tag, "Class2AbortCmd"))
	class2AbortCmd = value;
    else if (streq(tag, "Class2RecvDataTrigger"))
	class2RecvDataTrigger = value;
    else if (streq(tag, "Class2XmitWaitForXON"))
	class2XmitWaitForXON = getBoolean(value);

	// for backwards compatibility
    else if (streq(tag, "WaitForCarrier"))
	waitTimeCmd = "S7=" | fxStr(value);
    else if (streq(tag, "CommaPauseTime"))
	pauseTimeCmd = "S8=" | fxStr(value);
}
