/*
 * Copyright 1993 Network Computing Devices, Inc.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
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
 * $NCDId: @(#)ausun.c,v 1.16 1994/01/21 22:58:02 greg Exp $
 */

#include "dixstruct.h"				/* for RESTYPE */
#include "os.h"					/* for xalloc/xfree and NULL */
#include <fcntl.h>
#include <stropts.h>
#ifndef SVR4
#include <sun/audioio.h>
#else						/* SVR4 */
#include <sys/audioio.h>
#endif						/* SVR4 */
#include <audio/audio.h>
#include <audio/Aproto.h>
#include "au.h"

static int      devAudio,
                devAudioCtl;
static AuUint8 *auOutputMono,
               *auOutputStereo,
               *auInputMono,
               *emptyOutput;
static AuInt16  outputGain,
                inputGain;
static AuBool   updateGains;

extern AuInt32  auMinibufSamples;

#define	SERVER_CLIENT		0
#define MINIBUF_SIZE		800

#define auMinSampleRate		8000
#define auMaxSampleRate		8000

#define auPhysicalOutputChangableMask AuCompDeviceGainMask

#define auPhysicalOutputValueMask					       \
    (AuCompCommonAllMasks |						       \
     AuCompDeviceMinSampleRateMask |					       \
     AuCompDeviceMaxSampleRateMask |					       \
     AuCompDeviceGainMask |						       \
     AuCompDeviceLocationMask |						       \
     AuCompDeviceChildrenMask)

#define auPhysicalInputChangableMask AuCompDeviceGainMask

#define auPhysicalInputValueMask					       \
    (AuCompCommonAllMasks |						       \
     AuCompDeviceMinSampleRateMask |					       \
     AuCompDeviceMaxSampleRateMask |					       \
     AuCompDeviceLocationMask |						       \
     AuCompDeviceGainMask)						       \

#define auBucketChangableMask	0
#define auBucketValueMask	AuCompBucketAllMasks

#ifndef BUILTIN_BUCKETS
#define NUM_BUILTIN_BUCKETS	0
#else						/* BUILTIN_BUCKETS */
static struct
{
    AuUint8        *data,
                    format,
                    numTracks;
    AuUint32        sampleRate,
                    numSamples;
    char          **comment;
}
                builtinBuckets[] =
{
    boingSamples,
    boingFormat,
    boingNumTracks,
    boingSampleRate,
    boingNumSamples,
    &boingComment,
};

#define NUM_BUILTIN_BUCKETS						       \
    (sizeof(builtinBuckets) / sizeof(builtinBuckets[0]))
#endif						/* BUILTIN_BUCKETS */

static int
createServerComponents(auServerDeviceListSize, auServerBucketListSize,
		       auServerRadioListSize, auServerMinRate,
		       auServerMaxRate)
AuUint32       *auServerDeviceListSize,
               *auServerBucketListSize,
               *auServerRadioListSize,
               *auServerMinRate,
               *auServerMaxRate;
{
    AuDeviceID      stereo,
                    mono;
    ComponentPtr    d,
                   *p;
    int             i;
    extern RESTYPE  auComponentType;
    extern ComponentPtr *auServerDevices,	/* array of devices */
                   *auServerBuckets,		/* array of server owned
						 * buckets */
                   *auServerRadios,		/* array of server owned
						 * radios */
                    auDevices,			/* list of all devices */
                    auBuckets,			/* list of all buckets */
                    auRadios;			/* list of all radios */
    extern AuUint32 auNumServerDevices,		/* number of devices */
                    auNumActions,		/* number of defined actions */
                    auNumServerBuckets,		/* number of server owned
						 * buckets */
                    auNumServerRadios;		/* number of server owned
						 * radios */

    *auServerMinRate = auMinSampleRate;
    *auServerMaxRate = auMaxSampleRate;

    auNumServerDevices = *auServerDeviceListSize = *auServerBucketListSize =
	*auServerRadioListSize = 0;

    stereo = FakeClientID(SERVER_CLIENT);
    mono = FakeClientID(SERVER_CLIENT);

    AU_ALLOC_DEVICE(d, 1, 0);
    d->id = mono;
    d->changableMask = auPhysicalOutputChangableMask;
    d->valueMask = auPhysicalOutputValueMask;
    d->kind = AuComponentKindPhysicalOutput;
    d->use = AuComponentUseExportMask;
    d->access = AuAccessExportMask | AuAccessListMask;
    d->format = auNativeFormat;
    d->numTracks = 1;
    d->description.type = AuStringLatin1;
    d->description.string = "Mono Channel Output";
    d->description.len = strlen(d->description.string);
    d->minSampleRate = auMinSampleRate;
    d->maxSampleRate = auMaxSampleRate;
    d->location = AuDeviceLocationCenterMask | AuDeviceLocationInternalMask;
    d->lastWrite = (AuUint8 *) d + PAD4(sizeof(ComponentRec));
    d->numChildren = 0;
    d->minibuf = auOutputMono;
    d->minibufSize = auMinibufSamples * auNativeBytesPerSample *
	d->numTracks;
    d->physicalDeviceMask = PhysicalOutputMono;
    AU_ADD_DEVICE(d);

    AU_ALLOC_DEVICE(d, 2, 1);
    d->id = stereo;
    d->changableMask = auPhysicalOutputChangableMask;
    d->valueMask = auPhysicalOutputValueMask;
    d->kind = AuComponentKindPhysicalOutput;
    d->use = AuComponentUseExportMask;
    d->access = AuAccessExportMask | AuAccessListMask;
    d->format = auNativeFormat;
    d->numTracks = 2;
    d->description.type = AuStringLatin1;
    d->description.string = "Stereo Channel Output";
    d->description.len = strlen(d->description.string);
    d->minSampleRate = auMinSampleRate;
    d->maxSampleRate = auMaxSampleRate;
    d->location = AuDeviceLocationCenterMask | AuDeviceLocationInternalMask;
    d->lastWrite = (AuUint8 *) d + PAD4(sizeof(ComponentRec));
    d->numChildren = 1;
    d->children = (AuID *) (d->lastWrite +
			    PAD4((d->numTracks) * auNativeBytesPerSample));
    d->childSwap = (char *) (d->children + d->numChildren);
    d->children[0] = mono;
    d->minibuf = auOutputStereo;
    d->minibufSize = auMinibufSamples * auNativeBytesPerSample *
	d->numTracks;
    d->physicalDeviceMask = PhysicalOutputStereo;
    AU_ADD_DEVICE(d);

    AU_ALLOC_DEVICE(d, 1, 0);
    d->id = FakeClientID(SERVER_CLIENT);
    d->changableMask = auPhysicalInputChangableMask;
    d->valueMask = auPhysicalInputValueMask;
    d->kind = AuComponentKindPhysicalInput;
    d->use = AuComponentUseImportMask;
    d->access = AuAccessImportMask | AuAccessListMask;
    d->format = auNativeFormat;
    d->numTracks = 1;
    d->description.type = AuStringLatin1;
    d->description.string = "Mono Channel Input";
    d->description.len = strlen(d->description.string);
    d->minSampleRate = auMinSampleRate;
    d->maxSampleRate = auMaxSampleRate;
    d->location = AuDeviceLocationRightMask | AuDeviceLocationLeftMask |
	AuDeviceLocationExternalMask;
    d->lastRead = (AuUint8 *) d + PAD4(sizeof(ComponentRec));
    d->numChildren = 0;
    d->gain = AuFixedPointFromFraction(inputGain * 100, AUDIO_MAX_GAIN);
    d->minibuf = auInputMono;
    d->minibufSize = auMinibufSamples * auNativeBytesPerSample *
	d->numTracks;
    d->physicalDeviceMask = PhysicalInputMono;
    AU_ADD_DEVICE(d);

    /* set the array of server devices */
    if (!(auServerDevices =
       (ComponentPtr *) aualloc(sizeof(ComponentPtr) * auNumServerDevices)))
	return AuBadAlloc;

    p = auServerDevices;
    d = auDevices;

    while (d)
    {
	*p++ = d;
	d = d->next;
    }

#ifdef BUILTIN_BUCKETS
    for (i = 0; i < NUM_BUILTIN_BUCKETS; i++)
    {
	ALLOC_BUCKET(d);
	d->data = builtinBuckets[i].data;
	d->format = builtinBuckets[i].format;
	d->numTracks = builtinBuckets[i].numTracks;
	d->sampleRate = builtinBuckets[i].sampleRate;
	d->numSamples = builtinBuckets[i].numSamples;
	d->description.string = *builtinBuckets[i].comment;

	d->id = FakeClientID(SERVER_CLIENT);
	d->changableMask = auBucketChangableMask;
	d->valueMask = auBucketValueMask;
	d->kind = AuComponentKindBucket;
	d->use = AuComponentUseImportMask;
	d->access = AuAccessImportMask | AuAccessListMask;
	d->description.type = AuStringLatin1;
	d->description.len = strlen(d->description.string);
	d->minibufSize = auMinibufSamples * auNativeBytesPerSample *
	    d->numTracks;
	d->physicalDeviceMask = NotAPhysicalDevice;
	d->dataSize = d->numSamples * sizeofFormat(d->format) * d->numTracks;
	d->dataEnd = d->data + d->dataSize;
	d->read = d->write = d->data;
	d->destroyed = AuFalse;
	ADD_BUCKET(d);
    }

    /* set the array of server buckets */
    if (!(auServerBuckets = (ComponentPtr *) aualloc(sizeof(ComponentPtr) *
						     auNumServerBuckets)))
	return AuBadAlloc;

    p = auServerBuckets;
    d = auBuckets;

    while (d)
    {
	*p++ = d;
	d = d->next;
    }
#endif					       /* BUILTIN_BUCKETS */

    return AuSuccess;
}

#if 0
static AuUint32
setSampleRate(rate)
AuUint32        rate;
{
}

static void
eventPosted()
{
}
#endif

static void
serverReset()
{
    /*
     * ignore SIGPOLL so that the close of /dev/audio will wait until the
     * audio drains
     */
    signal(SIGPOLL, SIG_IGN);
    close(devAudio);
    close(devAudioCtl);
}

/**
  * Gains are mapped thusly:
  *
  *   Software   0 - 49     50 - 100
  *   Hardware   0 - 49     50 - 255
  */
static void
setPhysicalOutputGain(gain)
AuFixedPoint    gain;
{
    AuInt16         g = AuFixedPointIntegralAddend(gain);

    if (g < 50)
	outputGain = g;
    else
	/* (gain - 50) * (205 / 50) + 50 */
	outputGain = ((0x41999 * (g - 50)) >> 16) + 50;

    updateGains = AuTrue;
}

static          AuFixedPoint
getPhysicalOutputGain()
{
    if (outputGain < 50)
	return AuFixedPointFromSum(outputGain, 0);

    /* (gain - 50) * (50 / 205) + 50 */
    return (outputGain - 50) * 0x3e70 + 0x320000;
}

static void
setPhysicalInputGainAndLineMode(gain, lineMode)
AuFixedPoint    gain;
AuUint8         lineMode;
{
    AuInt16         g = AuFixedPointIntegralAddend(gain);

    if (g < 50)
	inputGain = g;
    else
	/* (gain - 50) * (205 / 50) + 50 */
	inputGain = ((0x41999 * (g - 50)) >> 16) + 50;

    updateGains = AuTrue;
}

static void
writeEmptyOutput()
{
    AuBlock         l = AuBlockAudio();

    write(devAudio, emptyOutput, 0);
    write(devAudio, emptyOutput, auMinibufSamples);
    AuUnBlockAudio(l);
}

static void
enableProcessFlow()
{
    ioctl(devAudio, I_FLUSH, FLUSHRW);		/* flush pending io */
    ioctl(devAudioCtl, I_SETSIG, S_MSG);       /* enable signal */
    writeEmptyOutput();
}

static void
disableProcessFlow()
{
    ioctl(devAudioCtl, I_SETSIG, 0);		/* disable signal */
    ioctl(devAudio, AUDIO_DRAIN, 0);		/* drain everything out */
}

static void
writeOutput(p)
AuInt16        *p;
{
    AuBlock         l;

    AuNativeToULAW8(p, 1, auMinibufSamples);

    l = AuBlockAudio();
    write(devAudio, p, 0);
    write(devAudio, p, auMinibufSamples);
    AuUnBlockAudio(l);
}

static void
writePhysicalOutputsMono()
{
    writeOutput(auOutputMono);
}

static void
writePhysicalOutputsStereo()
{
    AuInt32         i;
    AuInt16        *s,
                   *d;

    s = d = (AuInt16 *) auOutputStereo;

    for (i = 0; i < auMinibufSamples; i++, s += 2)
	*d++ = (s[0] + s[1]) >> 1;

    writeOutput(auOutputStereo);
}

static void
writePhysicalOutputsBoth()
{
    AuInt32         i;
    AuInt16        *s,
                   *m;

    s = (AuInt16 *) auOutputStereo;
    m = (AuInt16 *) auOutputMono;

    for (i = 0; i < auMinibufSamples; i++, s += 2)
	*m++ = (s[0] + s[1] + *m) / 3;

    writeOutput(auOutputMono);
}

static void
readPhysicalInputs()
{
    read(devAudio, auInputMono, auMinibufSamples);
    AuULAW8ToNative(auInputMono, 1, auMinibufSamples);
}

static void
setWritePhysicalOutputFunction(flow, funct)
CompiledFlowPtr flow;
void            (**funct) ();
{
    AuUint32        mask = flow->physicalDeviceMask;

    if ((mask & (PhysicalOutputMono | PhysicalOutputStereo)) ==
	(PhysicalOutputMono | PhysicalOutputStereo))
	*funct = writePhysicalOutputsBoth;
    else if (mask & PhysicalOutputMono)
	*funct = writePhysicalOutputsMono;
    else if (mask & PhysicalOutputStereo)
	*funct = writePhysicalOutputsStereo;
    else
	*funct = writeEmptyOutput;
}

static void
processAudioSignal()
{
    if (updateGains)
    {
	audio_info_t    info;

	AUDIO_INITINFO(&info);
	ioctl(devAudioCtl, I_SETSIG, 0);       /* disable signal */
	info.play.gain = outputGain;
	info.record.gain = inputGain;
	ioctl(devAudioCtl, AUDIO_SETINFO, &info);
	ioctl(devAudioCtl, I_SETSIG, S_MSG);   /* re-enable signal */

	updateGains = AuFalse;
    }

    AuProcessData();
}

#define	PhysicalOneTrackBufferSize					       \
    PAD4(auMinibufSamples * auNativeBytesPerSample * 1)
#define	PhysicalTwoTrackBufferSize					       \
    PAD4(auMinibufSamples * auNativeBytesPerSample * 2)

AuBool
AuInitPhysicalDevices()
{
    static AuUint8 *physicalBuffers;
    AuUint32        physicalBuffersSize;
    audio_info_t    info;
    extern AuUint32 auPhysicalOutputBuffersSize;
    extern AuUint8 *auPhysicalOutputBuffers;

#ifdef AUDIO_GETDEV
    if ((devAudio = open("/dev/audio", O_RDWR)) != -1)
    {
	int type;
	extern AuBool AuInitPhysicalDevices_dbri();

	ioctl(devAudio, AUDIO_GETDEV, &type);
	close(devAudio);

	if (type == AUDIO_DEV_SPEAKERBOX || type == AUDIO_DEV_CODEC)
	    return AuInitPhysicalDevices_dbri();
    }
#endif /* AUDIO_GETDEV */

    if (physicalBuffers)
	aufree(physicalBuffers);

    if (emptyOutput)
	aufree(emptyOutput);

    auMinibufSamples = MINIBUF_SIZE;

    if (!(emptyOutput = (AuUint8 *) aualloc(auMinibufSamples)))
	return AuFalse;

    auset(emptyOutput, 0xff, auMinibufSamples);

    physicalBuffersSize =
	2 * PhysicalOneTrackBufferSize + PhysicalTwoTrackBufferSize;

    if (!(physicalBuffers = (AuUint8 *) aualloc(physicalBuffersSize)))
	return AuFalse;

    auInputMono = physicalBuffers + 0 * PhysicalOneTrackBufferSize;
    auOutputMono = physicalBuffers + 1 * PhysicalOneTrackBufferSize;
    auOutputStereo = physicalBuffers + 2 * PhysicalOneTrackBufferSize;

    auPhysicalOutputBuffers = auOutputMono;
    auPhysicalOutputBuffersSize = physicalBuffersSize -
	PhysicalOneTrackBufferSize;

    devAudio = open("/dev/audio", O_RDWR);
    devAudioCtl = open("/dev/audioctl", O_RDWR);

    if (devAudio == -1 || devAudioCtl == -1)
	return AuFalse;

    signal(SIGPOLL, processAudioSignal);

    AuRegisterCallback(AuCreateServerComponentsCB, createServerComponents);
    AuRegisterCallback(AuSetPhysicalOutputGainCB, setPhysicalOutputGain);
    AuRegisterCallback(AuGetPhysicalOutputGainCB, getPhysicalOutputGain);
    AuRegisterCallback(AuSetPhysicalInputGainAndLineModeCB,
		       setPhysicalInputGainAndLineMode);
    AuRegisterCallback(AuEnableProcessFlowCB, enableProcessFlow);
    AuRegisterCallback(AuDisableProcessFlowCB, disableProcessFlow);
    AuRegisterCallback(AuReadPhysicalInputsCB, readPhysicalInputs);
    AuRegisterCallback(AuSetWritePhysicalOutputFunctionCB,
		       setWritePhysicalOutputFunction);

#if 0
    AuRegisterCallback(AuSetSampleRateCB, setSampleRate);
    AuRegisterCallback(AuEventPostedCB, eventPosted);
#endif

    ioctl(devAudioCtl, AUDIO_GETINFO, &info);
    outputGain = info.play.gain;
    inputGain = info.record.gain;

    /* bogus resource so we can have a cleanup function at server reset */
    AddResource(FakeClientID(SERVER_CLIENT),
		CreateNewResourceType(serverReset), 0);

    return AuTrue;
}



