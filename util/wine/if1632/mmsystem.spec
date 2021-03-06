# $Id: mmsystem.spec,v 1.1.1.2 1994/07/05 08:17:46 hsu Exp $
#
name	mmsystem
id	11
length 1226

1      pascal  MMSYSTEM_WEP(word word word ptr) MMSYSTEM_WEP(1 2 3 4)
2      pascal  SNDPLAYSOUND(ptr word) sndPlaySound(1 2)
5      pascal  MMSYSTEMGETVERSION() mmsystemGetVersion()
6      pascal  DriverProc(long word word long long) DriverProc(1 2 3 4 5)
30     pascal  OUTPUTDEBUGSTR(ptr) OutputDebugStr(1)
31     pascal  DriverCallback(long word word word long long long) DriverCallback(1 2 3 4 5 6 7)
#32    pascal  STACKENTER()
#33    pascal  STACKLEAVE()
#34    pascal  MMDRVINSTALL()
101    pascal  JOYGETNUMDEVS() JoyGetNumDevs()
102    pascal  JOYGETDEVCAPS(word ptr word) JoyGetDevCaps(1 2 3)
103    pascal  JOYGETPOS(word ptr) JoyGetPos(1 2)
104    pascal  JOYGETTHRESHOLD(word ptr) JoyGetThreshold(1 2)
105    pascal  JOYRELEASECAPTURE(word) JoyReleaseCapture(1)
106    pascal  JOYSETCAPTURE(word word word word) JoySetCapture(1 2 3 4)
107    pascal  JOYSETTHRESHOLD(word word) JoySetThreshold(1 2)
109    pascal  JOYSETCALIBRATION(word) JoySetCalibration(1)
201    pascal  MIDIOUTGETNUMDEVS() midiOutGetNumDevs()
202    pascal  MIDIOUTGETDEVCAPS(word ptr word) midiOutGetDevCaps(1 2 3)
203    pascal  MIDIOUTGETERRORTEXT(word ptr word) midiOutGetErrorText(1 2 3)
204    pascal  MIDIOUTOPEN(ptr word ptr long long long) midiOutOpen(1 2 3 4 5 6)
205    pascal  MIDIOUTCLOSE(word) midiOutClose(1)
206    pascal  MIDIOUTPREPAREHEADER(word ptr word) midiOutPrepareHeader(1 2 3)
207    pascal  MIDIOUTUNPREPAREHEADER(word ptr word) midiOutUnprepareHeader(1 2 3)
208    pascal  MIDIOUTSHORTMSG(word long) midiOutShortMsg(1 2)
209    pascal  MIDIOUTLONGMSG(word ptr word) midiOutLongMsg(1 2 3)
210    pascal  MIDIOUTRESET(word) midiOutReset(1)
211    pascal  MIDIOUTGETVOLUME(word ptr) midiOutGetVolume(1 2)
212    pascal  MIDIOUTSETVOLUME(word long) midiOutSetVolume(1 2)
215    pascal  MIDIOUTGETID(word ptr) midiOutGetID(1 2)
301    pascal  MIDIINGETNUMDEVS() midiInGetNumDevs()
302    pascal  MIDIINGETDEVCAPS(word ptr word) midiInGetDevCaps(1 2 3)
303    pascal  MIDIINGETERRORTEXT(word ptr word) midiInGetErrorText(1 2 3)
304    pascal  MIDIINOPEN(ptr word ptr long long long) midiInOpen(1 2 3 4 5 6)
305    pascal  MIDIINCLOSE(word) midiInClose(1)
306    pascal  MIDIINPREPAREHEADER(word ptr word) midiInPrepareHeader(1 2 3)
307    pascal  MIDIINUNPREPAREHEADER(word ptr word) midiInUnprepareHeader(1 2 3)
309    pascal  MIDIINSTART(word) midiInStart(1)
310    pascal  MIDIINSTOP(word) midiInStop(1)
311    pascal  MIDIINRESET(word) midiInReset(1)
312    pascal  MIDIINGETID(word ptr) midiInGetID(1 2)
350    pascal  AUXGETNUMDEVS() auxGetNumDevs()
351    pascal  AUXGETDEVCAPS(word ptr word) auxGetDevCaps(1 2 3)
352    pascal  AUXGETVOLUME(word ptr) auxGetVolume(1 2)
353    pascal  AUXSETVOLUME(word long) auxSetVolume(1 2)
401    pascal  WAVEOUTGETNUMDEVS() waveOutGetNumDevs()
402    pascal  WAVEOUTGETDEVCAPS(word ptr word) waveOutGetDevCaps(1 2 3)
403    pascal  WAVEOUTGETERRORTEXT(word ptr word) waveOutGetErrorText(1 2 3)
404    pascal  WAVEOUTOPEN(ptr word ptr long long long) waveOutOpen(1 2 3 4 5 6)
405    pascal  WAVEOUTCLOSE(word) waveOutClose(1)
406    pascal  WAVEOUTPREPAREHEADER(word ptr word) waveOutPrepareHeader(1 2 3)
407    pascal  WAVEOUTUNPREPAREHEADER(word ptr word) waveOutUnprepareHeader(1 2 3)
408    pascal  WAVEOUTWRITE(word ptr word) waveOutWrite(1 2 3)
409    pascal  WAVEOUTPAUSE(word) waveOutPause(1)
410    pascal  WAVEOUTRESTART(word) waveOutRestart(1)
411    pascal  WAVEOUTRESET(word) waveOutReset(1)
412    pascal  WAVEOUTGETPOSITION(word ptr word) waveOutGetPosition(1 2 3)
413    pascal  WAVEOUTGETPITCH(word ptr) waveOutGetPitch(1 2)
414    pascal  WAVEOUTSETPITCH(word long) waveOutSetPitch(1 2)
415    pascal  WAVEOUTGETVOLUME(word ptr) waveOutGetVolume(1 2)
416    pascal  WAVEOUTSETVOLUME(word long) waveOutSetVolume(1 2)
417    pascal  WAVEOUTGETPLAYBACKRATE(word ptr) waveOutGetPlaybackRate(1 2)
418    pascal  WAVEOUTSETPLAYBACKRATE(word long) waveOutSetPlaybackRate(1 2)
419    pascal  WAVEOUTBREAKLOOP(word) waveOutBreakLoop(1)
420    pascal  WAVEOUTGETID(word ptr) waveOutGetID(1 2)
501    pascal  WAVEINGETNUMDEVS() waveInGetNumDevs()
502    pascal  WAVEINGETDEVCAPS(word ptr word) waveInGetDevCaps(1 2 3)
503    pascal  WAVEINGETERRORTEXT(word ptr word) waveInGetErrorText(1 2 3)
504    pascal  WAVEINOPEN(ptr word ptr long long long) waveInOpen(1 2 3 4 5 6)
505    pascal  WAVEINCLOSE(word) waveInClose(1)
506    pascal  WAVEINPREPAREHEADER(word ptr word) waveInPrepareHeader(1 2 3)
507    pascal  WAVEINUNPREPAREHEADER(word ptr word) waveInUnprepareHeader(1 2 3)
508    pascal  WAVEINADDBUFFER(word ptr word) waveInAddBuffer(1 2 3)
509    pascal  WAVEINSTART(word) waveInStart(1)
510    pascal  WAVEINSTOP(word) waveInStop(1)
511    pascal  WAVEINRESET(word) waveInReset(1)
512    pascal  WAVEINGETPOSITION(word ptr word) waveInGetPosition(1 2 3)
513    pascal  WAVEINGETID(word ptr) waveInGetID(1 2)
601    pascal  timeGetSystemTime(ptr word) timeGetSystemTime(1 2)
602    pascal  timeSetEvent(word word ptr long word) timeSetEvent(1 2 3 4 5)
603    pascal  timeKillEvent(word) timeKillEvent(1)
604    pascal  timeGetDevCaps(ptr word) timeGetDevCaps(1 2)
605    pascal  timeBeginPeriod(word) timeBeginPeriod(1)
606    pascal  timeEndPeriod(word) timeEndPeriod(1)
607    pascal  timeGetTime() timeGetTime()
701    pascal  MCISENDCOMMAND(word word long long) mciSendCommand(1 2 3 4)
702    pascal  MCISENDSTRING(ptr ptr word word) mciSendString(1 2 3 4)
703    pascal  MCIGETDEVICEID(ptr) mciSendCommand(1)
706    pascal  MCIGETERRORSTRING(long ptr word) mciGetErrorString(1 2 3)
#900   pascal  MMTASKCREATE()
#902   pascal  MMTASKBLOCK()
#903   pascal  MMTASKSIGNAL()
#904   pascal  MMGETCURRENTTASK()
#905   pascal  MMTASKYIELD()
1100   pascal  DRVOPEN(ptr ptr long) DrvOpen(1 2 3)
1101   pascal  DRVCLOSE(word long long) DrvClose(1 2 3)
1102   pascal  DRVSENDMESSAGE(word word long long) DrvSendMessage(1 2 3 4)
1103   pascal  DRVGETMODULEHANDLE(word) DrvGetModuleHandle(1)
1104   pascal  DRVDEFDRIVERPROC(long word word long long) DrvDefDriverProc(1 2 3 4 5)
1210   pascal  MMIOOPEN(ptr ptr long) mmioOpen(1 2 3)
1211   pascal  MMIOCLOSE(word word) mmioClose(1 2)
1212   pascal  MMIOREAD(word ptr long) mmioRead(1 2 3)
1213   pascal  MMIOWRITE(word ptr long) mmioWrite(1 2 3)
1214   pascal  MMIOSEEK(word long word) mmioSeek(1 2 3)
1215   pascal  MMIOGETINFO(word ptr word) mmioGetInfo(1 2 3)
1216   pascal  MMIOSETINFO(word ptr word) mmioSetInfo(1 2 3)
1217   pascal  MMIOSETBUFFER(word ptr long word) mmioSetBuffer(1 2 3 4)
1218   pascal  MMIOFLUSH(word word) mmioFlush(1 2)
1219   pascal  MMIOADVANCE(word ptr word) mmioAdvance(1 2 3)
1220   pascal  MMIOSTRINGTOFOURCC(ptr word) mmioStringToFOURCC(1 2)
1221   pascal  MMIOINSTALLIOPROC(long ptr long) mmioInstallIOProc(1 2 3)
1222   pascal  MMIOSENDMESSAGE(word word long long) mmioSendMessage(1 2 3 4)
1223   pascal  MMIODESCEND(word ptr ptr word) mmioDescend(1 2 3 4)
1224   pascal  MMIOASCEND(word ptr word) mmioAscend(1 2 3)
1225   pascal  MMIOCREATECHUNK(word ptr word) mmioCreateChunk(1 2 3)
1226   pascal  MMIORENAME(ptr ptr ptr long) mmioRename(1 2 3 4)


