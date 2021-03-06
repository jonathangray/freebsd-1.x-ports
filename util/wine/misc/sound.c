static char RCSId[] = "$Id: sound.c,v 1.1.1.3 1994/05/19 07:59:49 hsu Exp $";
static char Copyright[] = "Copyright  Robert J. Amstadt, 1993";

#include <stdlib.h>
#include <stdio.h>
#include "prototypes.h"

int OpenSound(void)
{
  printf("OpenSound()\n");
  return -1;
}

void CloseSound(void)
{
  printf("CloseSound()\n");
}

int SetVoiceQueueSize(int nVoice, int nBytes)
{
  printf("SetVoiceQueueSize (%d,%d)\n",nVoice,nBytes);
  return 0;
}

int SetVoiceNote(int nVoice, int nValue, int nLength, int nCdots)
{
  printf("SetVoiceNote (%d,%d,%d,%d)\n",nVoice,nValue,nLength,nCdots);
  return 0;
}

int SetVoiceAccent(int nVoice, int nTempo, int nVolume, int nMode, int nPitch)
{
  printf("SetVoiceAccent(%d,%d,%d,%d,%d)\n", nVoice, nTempo, 
	 nVolume, nMode, nPitch);
  return 0;
}

int SetVoiceEnvelope(int nVoice, int nShape, int nRepeat)
{
  printf("SetVoiceEnvelope(%d,%d,%d)\n",nVoice,nShape,nRepeat);
  return 0;
}

int SetSoundNoise(int nSource, int nDuration)
{
  printf("SetSoundNoise(%d,%d)\n",nSource,nDuration);
  return 0;
}

int SetVoiceSound(int nVoice, long lFrequency, int nDuration)
{
  printf("SetVoiceSound(%d, %d, %d)\n",nVoice,lFrequency, nDuration);
  return 0;
}

int StartSound(void)
{
  return 0;
}

int StopSound(void)
{
  return 0;
}

int WaitSoundState(int x)
{
	fprintf(stderr, "WaitSoundState(%d)\n", x);
}

SyncAllVoices(void)
{
	fprintf(stderr, "SyncAllVoices()\n");
}

int CountVoiceNotes(int x)
{
	fprintf(stderr, "CountVoiceNotes(%d)\n", x);
}

LPINT GetThresholdEvent(void)
{
	fprintf(stderr, "GetThresholdEvent()\n");
}

GetThresholdStatus(void)
{
	fprintf(stderr, "GetThresholdStatus()\n");
}

int SetVoiceThreshold(int a, int b)
{
	fprintf(stderr, "SetVoiceThreshold(%d,%d)\n", a, b);
}

void DoBeep(void)
{
	fprintf(stderr, "BEEP!\n");
}

/*
11   pascal  WAITSOUNDSTATE(word) WaitSoundState(1)
12   pascal  SYNCALLVOICES() SyncAllVoices()
13   pascal  COUNTVOICENOTES(word) CountVoiceNotes(1)
14   pascal  GETTHRESHOLDEVENT() GetThresholdEvent()
15   pascal  GETTHRESHOLDSTATUS() GetThresholdStatus()
16   pascal  SETVOICETHRESHOLD(word word) SetVoiceThreshold(1 2)
*/
