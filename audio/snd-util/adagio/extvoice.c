/*
 * Handle patch loading for external synth.
 *
 * Routines here are for Kawaii K1.
 *
 * Two functions are called from here:
 *	int loadext(int pgm, char *data);
 *		is called from gusvoice.c to load the
 *		patch in the array data (which has been
 *		read from a file) down to the synth as
 *		program # pgm. (Call reopen_midi_in()
 *		if necessary to to handshaking with synth.)
 *	int setup_ext(int readable);
 *		is called from phase2.c after all patches
 *		have been loaded to do extra setup work
 *		for the synth (the Kawaii K1 has to be
 *		sent a "multi" patch).  The argument
 *		readable will be 0 unless tracing output
 *		was requested when calling mp. (Should
 *		call reopen_midi_out(), probably, if
 *		reopen_midi_in() was called by loadext().)
 *
 * To add support for a different synth, here is a list
 * (perhaps only partial) of things to do:
 *    Revise loadext and setup_ext (obviously).
 *    In gusvoice.c, fix the file suffix to be used for
 *	patch files (change ".k1s" to ?).  Substitute the
 *	size of a patch file for K1SINGLESIZE.
 *    Compose initialization data for the array ext_voice
 *	to say what patches can be assumed resident in the synth.
 *    Rewrite the routine set_ext_prog() in cfg.l to determine
 *	the program # that should be used for patches requested
 *	in cfg files (the present routine for the K1 looks for
 *	a program number 0-63 that is not used by any
 *	ext_voice[...].prog in the ext_voice array).
 */
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <ctype.h>
#ifndef __386BSD__
#include <malloc.h>
#endif
#include <fcntl.h>
#include <string.h>
#include "adagio.h"
#include "allphase.h"
#include "midicode.h"
#include "midi.h"

#ifdef K1
#include "k1voice.h"
#define K1SINGLESIZE (87+10)
#else
struct voice_type ext_voice[MAX_TONE_VOICES];
#endif

/* in gusvoice.c */
int find_tone_bank(int dev, int pgm, int bank);
/*
 * these are in phase2.c
 */
void sbflush(void);
int midigetc(void);
int midipending(void);
void midicmdch(char c);
void midich(char c);
int inonenb();
void reopen_midi_in();
void reopen_midi_out();


#ifdef K1
/* form of a multi patch on the Kawai K1 */
  struct patch_multi {
      char name[10];
      char volume;
      char single[8];
      char zonelow[8];
      char zonehigh[8];
      char poly[8];
      char rcvchan[8];
      char transpose[8];
      char tune[8];
      char level[8];
      char checksum;
  }
kmpatch = {
    {
	'M', 'i', 's', 'c', ' ', ' ', ' ', ' ', ' ', ' '
    }
    ,
    99,
    {
	0, 1, 2, 3, 4, 5, 6, 7
    }
    ,
    {
	0, 0, 0, 0, 0, 0, 0, 0
    }
    ,
    {
	127, 127, 127, 127, 127, 127, 127, 127
    }
    ,
    {
	0x10, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50, 0x50
    }
    ,
    {
	0x40, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07
    }
    ,
    {
	24, 24, 24, 24, 24, 24, 24, 24
    }
    ,
    {
	50, 50, 50, 50, 50, 50, 50, 50
    }
    ,
    {
	100, 100, 100, 100, 100, 100, 100, 100
    }
    ,
    0x74
};

/* send intro to K1 sysex command */
void k1send1(int function, int voice)
{
    midicmdch(0xf0);
    midich(0x40);		/* Kawai id */
    midich(0x00);		/* channel = 0 to 15 */
    midich(function);		/* function */
    midich(0x00);		/* group */
    midich(0x03);		/* machine id number of K1 */
    midich(0x00);		/* subcommand 1 = internal */
    midich(voice);		/* subcommand 2 = voice/program */
}

/* byte to K1 and update checksum */
void mchk(int c)
{
    midich(c);
    kmpatch.checksum += c & 0x7f;
}

/* k1msone - send a single voice to the K1 */
void k1msone(int iv, int readable)
{
    int n;

    k1send1(0x20, iv);
    kmpatch.checksum = 0xa5;

    for (n = 0; n < 10; n++)
	mchk(kmpatch.name[n]);
    mchk(kmpatch.volume);
    for (n = 0; n < 8; n++)
	mchk(kmpatch.single[n]);
    for (n = 0; n < 8; n++)
	mchk(kmpatch.zonelow[n]);
    for (n = 0; n < 8; n++)
	mchk(kmpatch.zonehigh[n]);
    for (n = 0; n < 8; n++)
	mchk(kmpatch.poly[n]);
    for (n = 0; n < 8; n++)
	mchk(kmpatch.rcvchan[n]);
    for (n = 0; n < 8; n++)
	mchk(kmpatch.transpose[n]);
    for (n = 0; n < 8; n++)
	mchk(kmpatch.tune[n]);
    for (n = 0; n < 8; n++)
	mchk(kmpatch.level[n]);
    midich(kmpatch.checksum & 0x7f);
    if (readable)
	printf("checksum = 0x%02x\n", kmpatch.checksum & 0x7f);
    midicmdch(MIDI_EOX);

}

/* dk1msone - show what is sent to the K1, for debugging */
void dk1msone(int iv)
{
    int n;

    printf("kmpatch%d [] = {\n\t", iv);
    for (n = 0; n < 10; n++)
	printf("'%c'[%d],", kmpatch.name[n], kmpatch.name[n]);
    printf("\n\t%d,\n\t", kmpatch.volume);
    for (n = 0; n < 8; n++)
	printf("%d,", kmpatch.single[n]);
    printf("\n\t");
    for (n = 0; n < 8; n++)
	printf("%d,", kmpatch.zonelow[n]);
    printf("\n\t");
    for (n = 0; n < 8; n++)
	printf("%d,", kmpatch.zonehigh[n]);
    printf("\n\t");
    for (n = 0; n < 8; n++)
	printf("0x%02x,", kmpatch.poly[n]);
    printf("\n\t");
    for (n = 0; n < 8; n++)
	printf("0x%02x,", kmpatch.rcvchan[n]);
    printf("\n\t");
    for (n = 0; n < 8; n++)
	printf("%d,", kmpatch.transpose[n]);
    printf("\n\t");
    for (n = 0; n < 8; n++)
	printf("%d,", kmpatch.tune[n]);
    printf("\n\t");
    for (n = 0; n < 8; n++)
	printf("%d,", kmpatch.level[n]);
    printf("\n}\n");

}

static void
send_k1_single(int pgm, char *data)
{
	int cksum;
	int n, dumpsize;

	/* the .k1s patch files are in K1 dump
	 * format, which includes an 8 byte
	 * prelude + checksum and EOX at end
	 */
	dumpsize = K1SINGLESIZE - 10;

	k1send1(0x20, pgm);
	cksum = 0xa5;

	/* offset past 8 byte prelude for real
	 * voice data
	 */
	for(n = 0; n < dumpsize; n++) {
		midich(data[n+8] & 0x7f);
		cksum += data[n+8] & 0x7f;
	}
	midich(cksum & 0x7f);	/* checksum */
	midicmdch(MIDI_EOX);
}

static void
send_k1_patch(int readable, int multi, int pgm, char *data)
{
#ifdef MIDIIN
	unsigned char k1ack[7] =
	{0xf0, 0x40, 0x00, 0x40, 0x00, 0x03, 0xf7};
	int n = 0, tries = 5;

#ifdef DEBUGK1INIT
fprintf(stderr,"K1 init");
#endif
	while (tries--) {
#endif /* MIDIIN */

#ifdef DEBUGK1INIT
fprintf(stderr," (try)");
#endif
	    if (multi) {
	    	if (readable) dk1msone(64);
	    	k1msone(64, readable);
	    }
	    else send_k1_single(pgm, data);
	    sbflush();
	    usleep(100000);

#ifdef MIDIIN
	    if (readable)
		printf("  (k1 answering: ");
	    for (n = 0; n < 7; n++) {
		int c = inonenb();
		if (c == MIDI_CTRL) {
		    (void) inonenb();
		    (void) inonenb();
		    c = inonenb();
		}
		if (c == -1) {
#ifdef DEBUGK1INIT
fprintf(stderr," (time out)");
#endif
		    if (verbose)
			printf("timed out waiting for k1 to acknowledge patch\n");
	    	    (void) ioctl(seq_fd, SNDCTL_SEQ_RESET, 0);
		    reopen_midi_in();
		    break;
		}
		if (readable)
		    printf(" 0x%02x", c);
		if (c != k1ack[n]) {
		    while (midipending()) {
			c = inonenb();
			if (readable)
			    printf(" 0x%02x", c);
		    }
		    if (verbose && !readable)
			printf("k1 did not correctly acknowledge my patch on try %d\n", 5 - tries);
		    break;
		}
	    }
	    if (readable)
		printf(")\n");
	    if (n == 7)
		break;
	    if (!tries) {
		fprintf(stderr, "cannot send patch to k1\n");
		exit(1);
	    } else {
		usleep(100000);
		while (midipending())
		    (void) midigetc();
	    }
	}
#endif /* MIDIIN */
}
#endif /* K1 */

int
loadext(int pgm, char *data)
{
#ifdef K1
	reopen_midi_in();
	send_k1_patch(0, 0, pgm, data);
	return(1);
#else
	return(0);
#endif
}


int
setup_ext(int readable)
{
#ifdef K1
    int n;

    reopen_midi_in();

    /* set up K1 patch. */

    for (n = 0; n < NUM_CHANS; n++) {
	int v, tpgm;
	int leftright = 1;
	int bank = (ext_voice_bank[n] >> 8) & 0x7f;

	if (ext_pan[n] != -1) {
	    if (ext_pan[n] < 54) leftright = 2;
	    else if (ext_pan[n] > 74) leftright = 0;
	}


	if (ext_chan[n]) {
	    int etot = ext_chan[n] - 1;
	    int solo = 0;
	    int kbmode = etot ? 1 : 2;
	    int trnsps;

	    if (etot < 0 || etot > 7) {
		fprintf(stderr, "extvoice: bad value %d of etot\n", etot);
	    }
	    v = ext_program[etot] - 1;
	    if (v < 0) {
		fprintf(stderr, "extvoice: bad value %d of v\n", v);
	    }
	    tpgm = find_tone_bank(ext_index, v, bank);

	    if (!ext_voice[tpgm].loaded) {
		fprintf(stderr, "extvoice: voice %d not loaded\n", tpgm);
	    }

	    kmpatch.poly[etot] = solo | ((kbmode & 1) << 6) | (leftright << 4);

	    trnsps = ext_voice[tpgm].trnsps - 64;
	    if (trnsps < -24 || trnsps > 24) {
		fprintf(stderr, "extvoice: bad value %d of transpose\n", trnsps);
		trnsps = 0;
	    }
	    kmpatch.transpose[etot] = trnsps + 24;
	    kmpatch.level[etot] = ((main_volume[n]/2+64) * 100) / 128;
	    /* finally, the voice/timbre for the section */
	    kmpatch.single[etot] = ext_voice[tpgm].prog;
	}
    }

    if (recording_track) {
/* The first channel has been reserved for the player; initialize
 * it now to respond to the keyboard and use the program specified
 * on the command line, if any.  Tone down the volume a little.
 */
	int kbmode = 0;
	int prog = ext_voice[recording_program].prog;
	if (prog < 0) prog = 0;
	kmpatch.single[0] = prog;
	kmpatch.rcvchan[0] = (0x3f & kmpatch.rcvchan[0]) | ((kbmode & 2) << 5);
	kmpatch.poly[0] = 0 | ((kbmode & 1) << 6) | (1 << 4);
	kmpatch.transpose[0] = (ext_voice[recording_program].trnsps - 64) + 24;
	kmpatch.level[0] = 90;
    }

	send_k1_patch(readable, 1, 0, NULL);

#ifdef DEBUGK1INIT
fprintf(stderr," (prog)");
#endif
	midicmdch(MIDI_CH_PROGRAM + 0);
	midich(64);
	sbflush();
	usleep(200000);
#ifdef DEBUGK1INIT
fprintf(stderr," (prog fl)");
#endif
#ifdef MIDIIN
	while (midipending()) {
	    int c = 0xff & midigetc();
	    if (verbose)
		printf(" k1 says 0x%02x for some reason\n", c);
	}
#ifdef DEBUGK1INIT
fprintf(stderr," (input fl)");
#endif

	/* Read-write mode seems to exacerbate midi output problems,
	 * so if it's not required, go back to write-only.
	 */
	if (!recording_track) {
	    (void) ioctl(seq_fd, SNDCTL_SEQ_RESET, 0);
	    reopen_midi_out();
	}
#endif /* MIDIIN */

#ifdef DEBUGK1INIT
fprintf(stderr, " done\n");
#endif

#endif /* K1 */
    return(1);
}

