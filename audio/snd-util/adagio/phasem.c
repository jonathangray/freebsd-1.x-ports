/*
 * phasem.c
 *
 */


#include <stdio.h>
#include <ctype.h>
#define NO_LC_DEFINES
#include "midifile.h"
#include "midi.h"
#ifndef __386BSD__
#include <malloc.h>
#endif
#include "midicode.h"
#include "adagio.h"
#include "userio.h"
#include "cmdline.h"
#include "allphase.h"

#ifdef K1
#define HELP_EXT_REVERB
#endif

int loadvoice(int, int, int, int, int);
void txt_error(char *);

#define MAINTAIN_PRECISION
/****************************************************************************
* The following are used to simulate fixed point with the radix point
* 4 bits from the right:
****************************************************************************/
#define unity 16
#define round(x) (((x)+8)>>4)
#define precise(x) ((x)<<4)

/* The durations of most drum notes are held to
 * 8 centiseconds, or whatever value is substituted
 * below.  This also determines the separation of
 * strokes in drum rolls.
 */
#define MINDRUM 8


/****************************************************************************
* Variables set by command line switches
****************************************************************************/

extern int ad_print;		/* adagio output */
extern int vverbose;		/* tracing output */


static int max_m_notes = -1;		/* -1 is flag that space must be allocated */

/****************************************************************
* data structure m_notes: the midi stream is stored as an array
* of 4-byte records, each of which is either a time or midi
* data.	 Midi data always begins with a control byte (high
* order bit set), and it is assumed times are positive (high
* order bit clear), so the two are easy to distinguish
* IF THE COMPILER PUTS THESE BITS IN THE SAME PLACE.  It looks
* like the high order byte of the time lines up with the last
* byte of a 4 byte array, so we will always set the high order
* bit of the last array byte when the first 3 bytes are filled
* with MIDI data.  This is refered to as the "tag" bit.
* WARNING: Lattice C longs are UNSIGNED, therefore always
* positive.  Test the high order bit with a mask.
****************************************************************/

#define MIDI_CMD_BIT		0x80
#define HIGH_BIT		0x80000000
#define istime(note) (!(((note)->when) & HIGH_BIT))

  typedef union m_note_struct {
      unsigned char n[4];
      unsigned long when;
  }
*m_note_type, m_note_node;

static m_note_type event_buff;	/* pointer to allocated buffer */
static m_note_type next;	/* pointer to next entry in buffer */
static m_note_type last;	/* pointer to last entry in buffer */

static int m_track = -1;

/****************************************************************************
*	Routines local to this module
****************************************************************************/
void initfuncs();
static void bend_filter();
static void byteorder();
static void ctrl_filter();
static int event_bend();
static void filter();
static long getdur();
static unsigned long getnext();
static unsigned long diffnext();
static char map_ctrl();
static char map_nctrl();
event_type phasem();
static void put_pitch();
void rec_init();
void card_init();
static void stowevent();
event_type rec_final();
/*************************/

/* Keep how many spare notes of polyphony for
 * gus (WPOLYPHONY) and external synth (XPOLYPHONY).
 * If negative, simultaneous notes can exceed
 * polyphony of device.
 */
#define WPOLSPARE -4
#define XPOLSPARE 0

#ifdef WPOLYPHONY
static int wave_polyphony = 0;
static int max_wave_polyphony = 0;
static int wave_poly[MAX_GM_VOICES];
static int pedal_wave_poly[MAX_GM_VOICES];
#endif
#ifdef XPOLYPHONY
static int max_ext_polyphony = 0;
static int pedal_xpoly[NUM_CHANS];
#endif

/* keep track of pedal while calculating polyphony */
static int m_pedal[NUM_CHANS];

static unsigned long redirection;

/*
 * Sometimes program changes come on a later track than the
 * notes they are supposed to affect.  On the first pass through
 * the midi file, we make a table of program changes and the
 * midi times (in 16ths of a centisecond) at which they occurred,
 * for each channel.  Then when the notes get put into the score
 * on a later pass, we can find the right programs by searching
 * the table.
 */
#define MAX_PROGS_PER_CHAN 128

static int program_history[NUM_CHANS][MAX_PROGS_PER_CHAN];
static unsigned long program_history_time[NUM_CHANS][MAX_PROGS_PER_CHAN];
static int program_history_count[NUM_CHANS] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};

/*
 * Find the program for a note played at time=now.
 */
static int
find_program(int chan, unsigned long now)
{
  int i;
  int old_prog = 0;

  if (piano_only) return(0);
/* printf("look for prog on %d at %d", chan, now); */
  for (i = 0; i <= program_history_count[chan]; i++) {
    if (program_history_time[chan][i] > now) break;
    old_prog = program_history[chan][i];
  }
/* printf(" -- found %d\n", old_prog); */
  return (old_prog);
}

/*
 * Enter a program change into the table.
 */
static void
remember_program(int chan, int prog, int bank)
{
      if ( program_history[chan][program_history_count[chan]] ==
	(prog | (bank <<8)) ) return;
      if (program_history_time[chan][program_history_count[chan]] > Mf_realtime) return;
      if (program_history_count[chan] < MAX_PROGS_PER_CHAN - 1) program_history_count[chan]++;
      program_history[chan][program_history_count[chan]] = (prog | (bank << 8));
      program_history_time[chan][program_history_count[chan]] = Mf_realtime;
/**
if (bank && chan == 11)
 printf("rem: prog %d bank %d on chan %d at time %d (count %d)\n",
prog, bank, chan, Mf_realtime, program_history_count[chan]);
**/
}


extern long *shm_running_time;
static int count_expression_controls = 0, count_volume_controls = 0;

void prtime();
void txt_program(int chan, int prog);
static void enter_prog(int chan, int prog, int bank);

/* following "txt_" functions are called from within midifile routines */

void txt_header(int format, int ntrks, int division)
{
    int chan, prog;

    m_track = -1;
    count_expression_controls = 0;
    count_volume_controls = 0;

    /* Enter any channel redirection assignments. */
    for (chan = 0; chan < NUM_CHANS; chan++) {
	prog = (channel_direction[chan] >> 7) & 0x7f;
	if (prog) {
	    enter_prog(chan, prog - 1, 0);
	    remember_program(chan, prog, 0);
	}
    }

    if (!vverbose)
	return;
    printf("*Header format=%d ntrks=%d division=%d\n", format, ntrks, division);
}

void txt_trackstart()
{
    int chan;
    for (chan = 0; chan < NUM_CHANS; chan++) m_pedal[chan] = 0;
    m_track++;
    *shm_running_time = -(m_track+1);
    if (!vverbose)
	return;
    printf("Start track %d\n", m_track + 1);
}

void txt_trackend()
{
    /*if (round(Mf_realtime) > running_time) running_time = round(Mf_realtime);*/
    if (!vverbose)
	return;
    printf("End track %d\n", m_track + 1);
}

/* info to remember for doing drum rolls: */

static int repeat_drum = -1;	/* the "pitch" which is really a drum roll trigger */
static int repeat_chan = -1;	/* trigger must be on same channel (10 or 16) as rolled note */
static int rd_chan, rd_pitch, rd_vol, repeat_vol; /* this is the percussion note to roll */
	/* we're not going to do the roll until noteoff, so remember noteon time */
static unsigned long rd_time;

void txt_noteon(int chan, int pitch, int vol)
{
#ifdef WPOLYPHONY
    int notes_required = 1;
    int midi_program = program[chan] - 1;
    if (PERCCHAN(chan)) midi_program = pitch + 128;
    else if (midi_program < 0) midi_program = 0;
#endif

    if (round(Mf_realtime) > running_time) running_time = round(Mf_realtime);
    /* When note is on a percussion channel, remember it if it's
     * a roll trigger (> 81) and also if it's a regular note, in case
     * there is a later roll trigger. (If a note > 81 is not intended as
     * a roll, well, we've just spiced up the music a little.)
     */
    if (PERCCHAN(chan)) {
	if (pitch > 81 && setting_drum_rolls) {
	    repeat_drum = pitch;
	    repeat_chan = chan;
	    repeat_vol = vol;
	    return;
	} /*else if (pitch < 35)*/
	/* Some percussion pitches < 35 are real notes, but they're not in
	 * the GM documents I've seen, and I don't understand them, so they
	 * are herewith discarded.
	 */
	    /*return;*/
	else if (vol) {
	    rd_chan = chan;
	    rd_pitch = pitch;
	    rd_vol = vol;
	    rd_time = Mf_realtime;
	}
	if (program[chan] == NO_VOI) /* Show that a note was played on this channel. */
	    program[chan] = 0;
    } else if (program[chan] == NO_VOI)
	program[chan] = 0;

#ifdef XPOLYPHONY
    /* When note is to be sent off board for this voice, keep track of how many
     * notes are on, because if there come to be too many, we may change our
     * mind(s) and play this voice on a sound card instead.
     */
    if (ext_chan[chan] && program[chan] == ext_program[ext_chan[chan] - 1]) {
	if (!vol) {
	    ext_polyphony--;
	    ext_poly[chan]--;
	} else if (ext_polyphony + pedal_xpoly[chan] + XPOLSPARE < XPOLYPHONY) {
	    ext_polyphony++;
	    ext_poly[chan]++;
	    if (ext_polyphony + pedal_xpoly[chan] > max_ext_polyphony)
		max_ext_polyphony = ext_polyphony + pedal_xpoly[chan];
	    if (m_pedal[chan]) pedal_xpoly[chan]++;
	} else if (gus_dev >= 0 || sb_dev >= 0) {
	    /* The external synth's polyphony has been exceeded.  This channel will
	     * be played on board, after all.  (We could shift the ext_ array values
	     * now to re-use the room ...)
	     */
	    ext_chan[chan] = 0;
	    ext_polyphony -= ext_poly[chan];
#ifdef WPOLYPHONY
	    notes_required += ext_poly[chan] + pedal_xpoly[chan];
#endif
	    ext_poly[chan] = 0;
	    if (verbose)
		printf("%s on channel %d shifted from ext. synth to card.\n",
		    gm_voice[program[chan]-1].vname, chan+1);
	}
    }
#endif

#ifdef WPOLYPHONY
    /* When note is to be played board for this voice, keep track of how many
     * notes are on, because if there come to be too many, we may play the
     * program on fm instead of the gus.
     */
    if (
#ifdef FM_PERC_PRIORITY
	program[chan] &&
	!PERCCHAN(chan) &&
#else
	(program[chan] || PERCCHAN(chan)) &&
#endif
	(!ext_chan[chan] || program[chan] != ext_program[ext_chan[chan] - 1]) &&
	gus_dev >= 0 && wave_poly[midi_program] >= 0) {
	if (chorus_depth[chan]) notes_required <<= 1;
	if (reverberation[chan] > REVERB_THRESHHOLD) notes_required <<= 1;
	if (!vol) {
	    wave_poly[midi_program] -= notes_required;
	    if (wave_poly[midi_program] >= 0)
		wave_polyphony -= notes_required;
	    else {
		wave_polyphony -= notes_required + wave_poly[midi_program];
		wave_poly[midi_program] = 0;
	    }
/**
printf(" - %d[%d] (tot. %d)\n", wave_poly[midi_program], midi_program, wave_polyphony);
**/
	} else if (wave_polyphony + notes_required + pedal_wave_poly[midi_program] +
				WPOLSPARE <= setting_gus_voices) {
	    wave_polyphony += notes_required;
	    wave_poly[midi_program] += notes_required;
	    if (wave_polyphony + pedal_wave_poly[midi_program] > max_wave_polyphony)
		max_wave_polyphony = wave_polyphony + pedal_wave_poly[midi_program] ;
	    if (!PERCCHAN(chan) && m_pedal[chan])
		pedal_wave_poly[midi_program] += notes_required;
/**
printf(" + %d[%d] (tot. %d)\n", wave_poly[midi_program], midi_program, wave_polyphony);
**/
	} else if (sb_dev >= 0) {
	    /* The wave synth's polyphony has been exceeded.  This program will
	     * be played on the fm board.
	     */
	    wave_polyphony -= wave_poly[midi_program];
	    if (wave_polyphony < 0) wave_polyphony = 0;
	    wave_poly[midi_program] = -1;
	    if (verbose) {
		if (gm_voice[midi_program].vname != NULL)
		  printf("%s on channel %d shifted from wave to fm card.\n",
		    gm_voice[midi_program].vname, chan+1);
	    }
	}
    }
#endif

    stowevent(NOTEON, chan, pitch, vol);
}

/* time (centiseconds) between end of one note of drum roll and beginning of next */
/*#define ROLL_SEP 2*/
#define ROLL_SEP 4

void txt_noteoff(int chan, int pitch, int vol)
{
#ifdef WPOLYPHONY
    int midi_program = program[chan] - 1;
    if (PERCCHAN(chan)) midi_program = pitch + 128;
    else if (midi_program < 0) midi_program = 0;
#endif

    if (round(Mf_realtime) > running_time) running_time = round(Mf_realtime);

    if (PERCCHAN(chan)) {
    /* If this is the "noteoff" of a drum roll trigger, execute the roll. */
	if (repeat_drum == pitch && chan == repeat_chan && chan == rd_chan) {
	    /* Save the time to restore it later. */
	    unsigned long now = Mf_realtime;
	    /* How many notes in roll?  (I'm just guessing here.) */
	    int roll_cnt = repeat_drum - 81 + 1;
	    /* Make end of roll louder than the beginning (actually, I don't
	     * think this has an audible effect).
	     */
	    int vol_inc = (repeat_vol - rd_vol) / roll_cnt;
	    /* Start the roll when the trigger went on. */
	    Mf_realtime = rd_time;
	    /* Insert new noteon and noteoff events into the midi stream to make
	     * the roll.  (This gives midi events that are out of time sequence,
	     * potentially causing some confusion in later processing.)
	     */
	    while (1) {
		/* Forward by the duration of a roll note to end the current one. */
		Mf_realtime += precise(MINDRUM);
		/* We may store an extra noteoff event here, but it doesn't matter. */
		stowevent(NOTEON, chan, rd_pitch, 0);
		if (!roll_cnt--)
		    break;
		/* Forward a bit more before starting the next note (at slightly
		 * increased volume).
		 */
		Mf_realtime += precise(ROLL_SEP);
		rd_vol += vol_inc;
		stowevent(NOTEON, chan, rd_pitch, rd_vol);
	    }
	    /* Note no more roll until next roll trigger. */
	    repeat_drum = -1;
	    /* Restore the right midi time. */
	    Mf_realtime = now;
	} else if (pitch > 81 && setting_drum_rolls)
	    /* End of roll trigger on wrong channel or doesn't correspond to
	     * start of trigger?  How confusing ...
	     */
	    return;
    }

#ifdef XPOLYPHONY
    /* One fewer simultaneously sounding notes to burden external synth. */
    if (ext_chan[chan] && program[chan] == ext_program[ext_chan[chan] - 1]) {
	ext_polyphony--;
	ext_poly[chan]--;
    }
#endif
#ifdef WPOLYPHONY
    /* When note is to be played board for this voice, keep track of how many
     * notes are on, because if there come to be too many, we may play the
     * program on fm instead of the gus.
     */
    if (
#ifdef FM_PERC_PRIORITY
	program[chan] &&
	!PERCCHAN(chan) &&
#else
	(program[chan] || PERCCHAN(chan)) &&
#endif
	(!ext_chan[chan] || program[chan] != ext_program[ext_chan[chan] - 1]) &&
	gus_dev >= 0 && wave_poly[midi_program] >= 0) {
	int notes_required = 1;
	if (chorus_depth[chan]) notes_required <<= 1;
	if (reverberation[chan] > REVERB_THRESHHOLD) notes_required <<= 1;
	wave_poly[midi_program] -= notes_required;
	if (wave_poly[midi_program] >= 0)
		wave_polyphony -= notes_required;
	else {
		wave_polyphony -= notes_required + wave_poly[midi_program];
		wave_poly[midi_program] = 0;
	}
/**
printf(" - %d[%d] (tot. %d)\n", wave_poly[midi_program], midi_program, wave_polyphony);
**/
    }
#endif

    stowevent(NOTEON, chan, pitch, 0);
}

void txt_pressure(int chan, int pitch, int press)
{
    stowevent(PRESSURE, chan, pitch, press);
}

static int bank_msb[NUM_CHANS];

void txt_parameter(int chan, int control, int value)
{

#ifdef WPOLYPHONY
    int midi_program = program[chan] - 1;
    if (midi_program < 0) midi_program = 0;
#endif

    if (control > 127) {
	fprintf(stderr, "txt_parameter: bad controller %d\n", control);
	return;
    }
    if (round(Mf_realtime) > running_time) running_time = round(Mf_realtime);
    switch (control) {
	case SUSTAIN:
		m_pedal[chan] = (value == 127);
#ifdef XPOLYPHONY
		if (!m_pedal[chan]) pedal_xpoly[chan] = 0;
#endif
#ifdef WPOLYPHONY
		if (!m_pedal[chan]) pedal_wave_poly[midi_program] = 0;
#endif
		break;
	case EXPRESSION:
		count_expression_controls++;
		break;
	case VOLUME:
		main_volume[chan] = value;
		count_volume_controls++;
		break;
	case PAN:
		if (!setting_pstereo) break;
		ext_pan[chan] = value;
		break;
	case CHORUS_DEPTH:
		chorus_depth[chan] = value;
		break;
	case REVERBERATION:
		reverberation[chan] = value;
		break;
	case 0:
		/*voice_bank[chan] = (value << 8);*/
		bank_msb[chan] = (value << 8);
		break;
	case 32:
		if (bank_msb[chan])
			voice_bank[chan] = bank_msb[chan] | value;
		bank_msb[chan] = 0;
		break;
    }
    stowevent(CONTROLLER, chan, control, value);
}

void txt_pitchbend(int chan, int msb, int lsb)
{
    stowevent(PITCHBEND, chan, msb, lsb);
}

void txt_program(int chan, int prog)
{
    /* If channel was redirected, ignore program messages. */
    if ((channel_direction[chan] >> 7) & 0x7f) return;

    enter_prog(chan, prog, (voice_bank[chan]>>8)&0x7f);
    remember_program(chan, prog + 1, voice_bank[chan]);
    voice_bank[chan] = 0;

/** Now program is recovered by find_program(), so
  * there is no need to store this event.
    stowevent(PROGRAM, chan, prog + 1, 0);
**/
}

void txt_chanpressure(int chan, int press)
{
    stowevent(CHANPRESSURE, chan, press, 0);
}

void txt_sysex(int leng, char *mess)
{
    /* Loading SysEx's for fm voices is just an experiment.  More useful to do
     * this for gus.
     */
    if (mess[1] == ID_LINUX && leng == LSYSEX_SIZE) {
	int i, chan = mess[2];
	fm_sysex[chan] = (unsigned char *) malloc(LSYSEX_SIZE);
	for (i = 0; i < LSYSEX_SIZE; i++)
	    fm_sysex[chan][i] = mess[i];
	program[chan] = chan + 240 + 1;
	stowevent(PROGRAM, chan, chan + 240 + 1, 0);
	if (verbose && !ad_print)
	    printf("  Loaded Linux sysex on channel %d.\n", chan + 1);
    }
    if (!vverbose)
	return;
    prtime();
    printf("Sysex, leng=%d\n", leng);
}

void txt_metamisc(int type, int leng, char *mess)
{
    if (!vverbose)
	return;
    prtime();
    printf("Meta event, unrecognized, type=0x%02x leng=%d\n", type, leng);
}

void txt_metaspecial(int type, int leng, char *mess)
{
    if (!verbose)
	return;
    prtime();
    printf("Meta event, sequencer-specific, type=0x%02x leng=%d\n", type, leng);
}


void txt_metatext(int type, int leng, char *mess)
{
    static char *ttype[] =
    {
	NULL,
	"note",			/* type=0x01 */
	"Copyright Notice",	/* type=0x02 */
	"Sequence/Track Name",
	"Instrument Name",	/* ...       */
	"Lyric",
	"Marker",
	"Cue Point",		/* type=0x07 */
	"note"
    };
    int unrecognized = (sizeof(ttype) / sizeof(char *)) - 1;
    int n, c;
    unsigned char *p = (unsigned char *)mess;
    struct meta_text_type *meta, *mlast;
    char *meta_string;

    if (verbose && Mf_realtime > 0 && (type == 1||type == 5||type == 6||type == 7)) {
	meta = (struct meta_text_type *)malloc(sizeof(struct meta_text_type));
	if (leng > 72) leng = 72;
	meta_string = (char *)malloc(leng+1);
	for (n = 0; n < leng; n++) {
	    c = *p++;
	    meta_string[n] = (isprint(c) || isspace(c)) ? c : '.';
	}
	meta_string[leng] = '\0';
	meta->type = type;
	meta->text = meta_string;
	meta->time = round(Mf_realtime);
	if (meta_text_list == NULL) {
	    meta->next = meta_text_list;
	    meta_text_list = meta;
	}
	else {
	    for (mlast = meta_text_list; mlast->next != NULL; mlast = mlast->next)
		if (mlast->next->time > meta->time) break;
	    meta->next = mlast->next;
	    mlast->next = meta;
	}
	if (round(Mf_realtime) > running_time) running_time = round(Mf_realtime);
	return;
    }
    else if (!verbose) return;

    if (type < 1 || type > unrecognized)
	type = unrecognized;
    prtime();
    printf("  %s, track %d: ", ttype[type], m_track + 1);
    for (n = 0; n < leng; n++) {
	c = *p++;
	printf((isprint(c) || isspace(c)) ? "%c" : "\\0x%02x", c);
    }
    printf("\n");
}

void txt_metaseq(int num)
{
    if (!vverbose)
	return;
    prtime();
    printf("Meta event, sequence number = %d\n", num);
}

void txt_metaeot()
{
    if (!vverbose)
	return;
    prtime();
    printf("Meta event, end of track\n");
}

void txt_keysig(int sf, int mi)
{
    if (!vverbose)
	return;
    prtime();
    printf("Key signature, sharp/flats=%d  minor=%d\n", sf, mi);
}

void txt_tempo(long tempo)
{
    if (!vverbose)
    /* Tempo changes are taken care of internally by midifile functions now,
     * so there is nothing to do here.
     */
	return;
    prtime();
    printf("Tempo, microseconds-per-MIDI-quarter-note=%d\n", tempo);
}

void txt_timesig(int nn, int dd, int cc, int bb)
{
    int denom = 1;

    if (!vverbose)
	return;
    while (dd-- > 0)
	denom *= 2;
    prtime();
    printf("Time signature=%d/%d  MIDI-clocks/click=%d  32nd-notes/24-MIDI-clocks=%d\n",
	   nn, denom, cc, bb);
}

void txt_smpte(int hr, int mn, int se, int fr, int ff)
{
    if (!vverbose)
    /* SMPTE is, however, not taken care of anywhere.  I don't have any
     * midi files with SMPTE events in them ...
     */
	return;
    prtime();
    printf("SMPTE, hour=%d minute=%d second=%d frame=%d fract-frame=%d\n",
	   hr, mn, se, fr, ff);
}

void txt_arbitrary(int leng, char *mess)
{
    if (!vverbose)
	return;
    prtime();
    printf("Arbitrary bytes, leng=%d\n", leng);
}

void prtime()
{
    if (ad_print)
	printf("*");
    if (!vverbose)
	return;
    printf("Time=%ld (%ldcs) ", Mf_currtime, Mf_realtime);
}

/* Allow midifile functions to call back here to above "txt_" functions. */
void initfuncs()
{
    Mf_error = txt_error;
    Mf_header = txt_header;
    Mf_trackstart = txt_trackstart;
    Mf_trackend = txt_trackend;
    Mf_noteon = txt_noteon;
    Mf_noteoff = txt_noteoff;
    Mf_pressure = txt_pressure;
    Mf_parameter = txt_parameter;
    Mf_pitchbend = txt_pitchbend;
    Mf_program = txt_program;
    Mf_chanpressure = txt_chanpressure;
    Mf_sysex = txt_sysex;
    Mf_metamisc = txt_metamisc;
    Mf_seqnum = txt_metaseq;
    Mf_eot = txt_metaeot;
    Mf_timesig = txt_timesig;
    Mf_smpte = txt_smpte;
    Mf_tempo = txt_tempo;
    Mf_keysig = txt_keysig;
    Mf_seqspecific = txt_metaspecial;
    Mf_text = txt_metatext;
    Mf_arbitrary = txt_arbitrary;
}
/****************************/

/*
 * We are finished now with the interface to the midifile functions.
 * The next portion of code is for constructing the list of midi
 * events.  It is from Dannenberg's record.c.
 *
 * record.c -- keyboard to adagio recorder
 *
 * the interface consists of three routines:
 *	    rec_init()		-- initialization
 *	    stowevent()		-- store each midi event as encountered
 *	    rec_final()		-- called to finish up
 */

/*****************************************************************************
*	    Change Log
*  Date	    | Change
*-----------+-----------------------------------------------------------------
* 27-Feb-86 | Created changelog
*	    | Use pedal information when computing durations (code taken
*	    |  from transcribe.c)
* 23-Mar-86 | Determine size of transcription when rec_init is called.
* 21-May-86 | Major rewrite to use continuous controls (code taken
*	    |  from transcribe.c)
*****************************************************************************/

static void stowevent(int command, int chan, int s1, int s2)
{
    next->when = Mf_realtime;
    next++;
    next->n[0] = command | chan;
    next->n[1] = s1;
    next->n[2] = s2;
    next->n[3] = MIDI_CMD_BIT | m_track;	/* set tag bit */
    next++;

    if (next >= last) {
	fprintf(stderr, "out of space\n");
	exit(1);
    }
}




/* From here, we deal with constructing the list of notes from the stored
 * list of midi events.  The code is derived from Dannenberg's transcribe.c
 */

/****************************************************************************
*				bend_filter
* Inputs:
*	m_note_type m_note: the current m_note
*	m_note_type last: the last recorded event
*	long now: the current time
* Effect:
*	remove pitch bend events in same 0.01 sec time slot
* Implementation:
*	If the current event is a pitch bend that bends again
*	in the same time slot, make it a no-op by replacing it with
*	the time.
****************************************************************************/

static void bend_filter(m_note, last, now)
m_note_type m_note;		/* current m_note */
m_note_type last;		/* the last recorded event */
unsigned long now;		/* the current time */
{
    /* first see if there is another bend in this time
     * slot.
     */
    m_note_type m_note2 = m_note + 1;
    while (m_note2 < last) {
	if (istime(m_note2) && (m_note2->when > now)) {
	    break;		/* new time slot */
	} else if (m_note->n[0] == m_note2->n[0]) {
	    m_note->when = now;
	    return;		/* found another bend */
	}
	m_note2++;
    }
}

/****************************************************************************
*				byteorder
* Effect:
*	check out assumptions about byte order and placement
****************************************************************************/

static void byteorder()
{
    if ((sizeof(event_buff[0]) != 4) ||
	(sizeof(event_buff[0].when) != 4) ||
	(sizeof(event_buff[0].n[0]) != 1)) {
	fprintf(stderr, "implementation error: size problem\n");
	exit(1);
    }
    event_buff[0].n[0] = 0x12;
    event_buff[0].n[1] = 0x34;
    event_buff[0].n[2] = 0x56;
    event_buff[0].n[3] = 0x78;
    if (vverbose)
	printf("layout is 0x%lx\n", event_buff[0].when);
    if ((event_buff[0].when != 0x78563412) &&
	(event_buff[0].when != 0x12345678)) {
	fprintf(stderr, "implementation error: layout problem\n");
	exit(1);
    }
}

/****************************************************************************
*				ctrl_filter
* Inputs:
*	m_note_type m_note: the current m_note
*	m_note_type last: the last recorded event
*	long now: the current time
* Effect:
*	remove ctrl change events in same 0.01 sec time slot
* Implementation:
*	If the current event is a control change that changes again
*	in the same time slot, make it a no-op by replacing it with
*	the time.
****************************************************************************/

static void ctrl_filter(m_note, last, now)
m_note_type m_note;		/* the current m_note */
m_note_type last;		/* the last recorded event */
unsigned long now;		/* the current time */
{
    /* see if there is another control change in this time
     * slot.
     */
    m_note_type m_note2 = m_note + 1;
    while (m_note2 < last) {
	if (istime(m_note2) && (m_note2->when > now)) {
	    break;		/* new time slot */
	} else if ((m_note->n[0] == m_note2->n[0]) &&
		   (m_note->n[1] == m_note2->n[1])) {
	    m_note->when = now;
	    return;		/* found another change */
	}
	m_note2++;
    }
}

/****************************************************************************
*				event_bend
* Inputs:
*	m_note_type m_note: pointer to a pitch bend event
* Outputs:
*	returns int: a 14 bit pitch bend number
****************************************************************************/

static int event_bend(m_note)
m_note_type m_note;
{
    return (int) (m_note->n[1] + (m_note->n[2] << 7));
}

/****************************************************************************
*				filter
* Inputs:
*	m_note_type last: the last m_note recorded
* Effect: allow only one control change per time slot (0.01 sec)
* Implementation:
*	call ctrl_filter and bend_filter to overwrite control changes with
*	noop data (the current time is used as a noop)
****************************************************************************/

static void filter(last)
m_note_type last;
{
    m_note_type m_note;		/* loop control variable */
    unsigned long now = 0;	/* last time seen */
    int command;		/* command pointed to by m_note */
    int chan;			/* channel pointed to by m_note */

    for (m_note = event_buff; m_note <= last; m_note++) {
	if (istime(m_note)) {
	    now = m_note->when;
	} else {
	    command = m_note->n[0] & MIDI_CODE_MASK;
	    chan = m_note->n[0] & MIDI_CHN_MASK;

	    if (command == MIDI_CTRL &&
		m_note->n[1] == SUSTAIN) {
		/* do nothing */ ;
	    } else if (command == MIDI_CTRL) {
		ctrl_filter(m_note, last, now);
	    } else if (command == MIDI_TOUCH) {
		bend_filter(m_note, last, now);	/* bend and touch use the */
	    } else if (command == MIDI_BEND) {	/*  same filter routines  */
		bend_filter(m_note, last, now);
	    }
	}
    }
}


/****************************************************************************
*				getdur
* Inputs:
*	int i: index of the m_note
*	m_note_type last: pointer to the last event recorded
*	int ped: true if pedal is down at event i
*	long now: the time at event i
* Outputs:
*	returns long: the duration of m_note i
* Assumes:
*	assumes i is a m_note
* Implementation:
*	This is tricky because of pedal messages.  The m_note is kept on by
*	either the key or the pedal.  Keep 2 flags, key and ped.  Key is
*	turned off when a key is released, ped goes off and on with pedal.
*	Note ends when (1) both key and ped are false, (2) key is
*	pressed (this event will also start another m_note).
****************************************************************************/

/* This routine is elaborated from Dannenberg's version by the addition
 * of logic to check that noteoff events correspond only to noteon events
 * on the same midi track, and also to cut off notes for solo (monophonic,
 * one-note-at-a-time) instruments when the next note for that instrument
 * starts.
 */
static long getdur(i, last, ped, now, solo)
int i;
m_note_type last;
int ped;
unsigned long now;
int solo;
{
    int key = true;		/* flag that says if m_note is on */
    unsigned long start = now;
    unsigned long lastnow = now;
    long duration = 0;
    int thistrack = event_buff[i].n[3] & 0x7f;
    int chan = event_buff[i].n[0] & MIDI_CHN_MASK;
    int pitch = event_buff[i].n[1];
    m_note_type m_note = &(event_buff[i + 1]);
    int m_noteon;		/* true if a m_noteon message received on chan */
    int keyon;			/* true if m_noteon message had non-zero velocity */
    int newsolo;		/* true if m_noteon is for same solo instrument */

    if (PERCCHAN(chan)) return(precise(MINDRUM));

    /* search from the next event (i+1) to the end of the buffer:
     */
    for (; m_note < last; m_note++) {
	if (istime(m_note)) {
	    now = m_note->when;
	    duration = now - start;
	} else {

	    /* Shouldn't go off the end of the track. */
	    if (really_verbose && thistrack != (m_note->n[3] & 0x7f) && !PERCCHAN(chan))
		fprintf(stderr, "Warning: no note termination, track %d, channel %d\n",
			thistrack, chan + 1);

	    if (thistrack != (m_note->n[3] & 0x7f)) return (lastnow - start);

	    /* Assume ... */
	    newsolo = m_noteon = keyon = false;

	    /* Only a later event on the same channel can terminate a note. */
	    if ((m_note->n[0] & MIDI_CHN_MASK) == chan) {
	        unsigned char code_byte = m_note->n[0] & MIDI_CODE_MASK;

		/* A repeated note terminates previous note, even if pedal is down. */
		m_noteon = (code_byte == MIDI_ON_NOTE) &&
		    (m_note->n[1] == pitch);

		/* A new note terminates previous one for a non-polyphonic instrument. */
#ifdef USE_SOLO_CUTOFF
		if (solo)
		    newsolo = (code_byte == MIDI_ON_NOTE) &&
			(m_note->n[2] != 0);
#endif

		/* Starting repeated note? */
		keyon = m_noteon && (m_note->n[2] != 0);

		/* Usual convention that noteon, vel=0 is same as noteoff. */
		if ((m_noteon && (m_note->n[2] == 0)) ||
		    ((code_byte == MIDI_OFF_NOTE) &&
		     (m_note->n[1] == pitch)))
		    key = false;

		/* Keep track of pedal. */
		if ((code_byte == MIDI_CTRL) &&
		    m_note->n[1] == SUSTAIN)
		    ped = ( m_note->n[2] == 127 );

		/* Terminate, now, if finger & foot both up, or repeated note, or new
		 * note on solo instrument.
		 */
		if (!key && !ped) return(duration);
#ifdef USE_SOLO_CUTOFF
		if (keyon || newsolo) return(duration);
#else
		if (keyon) return(duration);
#endif
	    }
	}
	lastnow = now;
    }
    return last->when - start;
}

/****************************************************************************
*				getnext
* Inputs:
*	int i: the index of the current m_note
*	m_note_type last: pointer to last valid data
*	long now: the current time
* Outputs:
*	returns long: the time of the next m_note, program, or control change
*		(returns time of last event if nothing else is found)
****************************************************************************/

/* This is for filling in digits after 'n' in an adagio score. */
static unsigned long getnext(i, last, now)
int i;				/* the index of the current m_note */
m_note_type last;		/* pointer to last valid data */
unsigned long now;		/* the current time */
{
    int thistrack = event_buff[i].n[3] & 0x7f;

    i++;			/* advance to next item */
    for (; event_buff + i < last; i++) {
	m_note_type m_note = &(event_buff[i]);
	int cmd = m_note->n[0] & MIDI_CODE_MASK;

	if (istime(m_note)) {
	    now = m_note->when;
	} else {
	    if ((m_note->n[3] & 0x7f) != thistrack)
		return (0);
	    if (((cmd == MIDI_ON_NOTE) &&
		 (m_note->n[2] != 0)) /* m_note on */ ||
		(cmd == MIDI_CH_PROGRAM) /* program change */ ||
		((cmd == MIDI_CTRL) &&
		 (map_nctrl(m_note->n[1]) < 7)
		 /*(m_note->n[1] != SUSTAIN)*//* control change */ ) ||
		(cmd == MIDI_TOUCH) ||
		(cmd == MIDI_BEND)) {
		return now;
	    }
	}
    }
    return last->when;
}

/****************************************************************************
*				map_ctrl
* Inputs:
*	int control: a midi control number
* Outputs:
*	returns char: an adagio control change command letter, NULL if
*		control change is not one of PORTARATE, PORTASWITCH,
*		MODWHEEL, FOOT
****************************************************************************/

static char map_ctrl(control)
int control;
{
    switch (control) {
    case PORTARATE:
	return 'J';
    case PORTASWITCH:
	return 'K';
    case MODWHEEL:
	return 'M';
    case FOOT:
	return 'X';
    default:
	return 0;
    }
}

/* Convert midi controller number to index into ctrlval array. */
static char map_nctrl(control)
int control;
{
    switch (control) {
    case PORTARATE:
	return 1;
    case PORTASWITCH:
	return 2;
    case MODWHEEL:
	return 3;
    case FOOT:
	return 5;
    case VOLUME:
	return 7;
    case PAN:
	return 8;
    case EXPRESSION:
	return 9;
    default:
	return 15;
    }
}

static unsigned long diffnext(i, last, now, nxlie)
int i;				/* the index of the current m_note */
m_note_type last;		/* pointer to last valid data */
unsigned long now;		/* the current time */
int *nxlie;
{
    long interval;
    interval = getnext(i, last, now) - now;
    if (interval < 0) {
	*nxlie = true;
	return 0;
    }
    *nxlie = false;
    return (interval);
}

/* beginning of interface to adagio "phase1" routines */


static void reverse();
static event_type nalloc();
static event_type ctrlalloc();
static void ins_event();

/* Include here code that is (mostly) common to ad and mp.
 * Some minor differences are ifdef'd with respect to the
 * following define.
 */
#define I_AM_MP
#include "admp.c"

static void doctrl(n, v)
int n, v;
{
    ctrlval[n] = v;
    ctrlflag[n] = true;
    ctrlflag[0] = true;		/* ctrlflag[0] set if any flag is set */
}

/****************************************************************************
*	phasem (adapted from "output" routine of transcribe and "phase1")
* Inputs:
*	m_note_type last: the last data in the buffer
*	int absflag: set to true if first line of the adagio score should
*		include the absolute time
* Effect:
*	write adagio score using data in event_buff
* Implementation:
*	NOTE: put all program changes in rests
*	use N(ext) notation for all timing
*	output no more than one continuous parameter change per
*	clock tick for each continuous change parameter
****************************************************************************/

static int ped[NUM_CHANS];

event_type phasem(last, absflag)
m_note_type last;
int absflag;
{
    int i;			/* loop counter */
    int command;		/* the current command */
    int chan;			/* the midi channel of the current event */
    int lastchan = 0;		/* the default adagio channel (1) */
    int adlastchan = 0;		/* the default adagio channel (1) */
    /*int ped = false;*/		/* flag maintains state of pedal */
    int how_many = last - event_buff;
    unsigned long now = 0;	/* the time of the next event */
    int bad_ctrl_flag = false;	/* set to true if unknown ctrl read */
    int nxlie = false;
/* variables from "phase1" */
    event_type score = NULL;	/* the translated note list */
    int out_of_memory = false;	/* set when no more memory */
    int thecontrolval = 0;

    if (!ad_init()) {		/* something bad happened in init(), STOP */
	fprintf(stderr, "WOOPS; something strange happened in INIT()!  ...exiting\n");
	exit(1);
	return NULL;		/* make lint happy */
    }
    lineno = 0;			/*(no use is currently made of lineno)*/


    /* set the initial absolute time, all other times are relative */
    if (absflag && ad_print)
	printf("t%ld ", round(event_buff[0].when));

    for (i = 0; i < how_many; i++) {

	/* Tracing output? */
	if (vverbose) {
	    printf("ev %d: %x %x %x (%ld)\n", i, event_buff[i].n[0],
	     event_buff[i].n[1], event_buff[i].n[2], event_buff[i].when);
	}

	/* Advance clock for timing event. */
	if (istime(event_buff + i)) {
	    now = event_buff[i].when;
	    if (vverbose)
		printf("i = %d, now = %ld\n", i, now);
	} else {
	/* it's not a time -- must be a note or a control */

	    command = event_buff[i].n[0] & MIDI_CODE_MASK;
	    chan = event_buff[i].n[0] & MIDI_CHN_MASK;

	    /* When notes were not sequential, can't depend on 'n's in adagio score. */
	    if (nxlie &&
		(command != MIDI_ON_NOTE || event_buff[i].n[2] != 0)
		) {
		if (ad_print)
		    printf("t%ld ", round(now));
		nxlie = false;
	    }
	    pitch_flag = false;		/* assume it's not a note */
	    thetime = now;
	    voice = chan + 1;
	    thetrack = event_buff[i].n[3] & 0x7f;
	    redirection = track_direction[thetrack];
	    theeffect = 0;		/* assume no special effect */
	    dur = 0;			/* assumption */
	    rest_flag = true;		/* assumption */

	    program[chan] = find_program(chan, thetime);
	    if (redirection) {
		int r_chan = (redirection >> 2) & 0x1f;
		if (!r_chan || r_chan == voice) {
		    int d_prog = (program[chan] & 0xff);
		    program[chan] = (redirection >> 7) & 0x7f;
		    if ( ((redirection&3) == 3 || (!(redirection&3) && !d_prog)) &&
				!ext_chan[chan])
			enter_prog(chan, program[chan] - 1, 0);
		}
	    }
	    voice_bank[chan] = program[chan] >> 8;
	    /* discard voice bank info */
	    program[chan] &= 0xff;
	    /* Ac. Grand default program */
	    if (!program[chan] && !PERCCHAN(chan) && command == MIDI_ON_NOTE) {
		txt_program(chan, 0);
		program[chan] = 1;
	    }
	    if (program[chan] != last_prog[chan]) new_prog = program[chan];

	    if (new_prog > 0 && ad_print) {
		    printf("r z%d n%ld", new_prog,
			   round(diffnext(i, last, now, &nxlie)));
		    printf(" v%d\n", chan + 1);
	    }


	    if (command == MIDI_CTRL) {
		thecontrol = event_buff[i].n[1];
		thecontrolval = event_buff[i].n[2];
		if (really_verbose && thecontrol > 127) fprintf(stderr,
			"phasem: bad controller %d\n", thecontrol);
		if (thecontrol == VOLUME &&
		    /*thetime > 0 &&*/
		    count_volume_controls > NUM_CHANS &&
		    count_volume_controls > 2*count_expression_controls)
			thecontrol = EXPRESSION;
	    }
	    else
		thecontrol = -1;

	    /* Beginning of a note? */
	    if (command == MIDI_ON_NOTE && event_buff[i].n[2] != 0) {
		int solo;
		pitch = event_buff[i].n[1];
		pitch_flag = true;
		rest_flag = false;

		/* I don't see any point to distinguishing solo percussion instruments. */
		if (!(PERCCHAN(chan)) && program[chan] > 0 && !no_solo)
		    solo = gm_voice[program[chan]-1].flags;
		else
		    /* here it's percussion, piano, or (?) uninitialized */
		    solo = 0;

		/* Find duration -- ignore note if less than 1cs (should I do this?) */
		if ((dur = getdur(i, last, ped[chan], now, solo)) < unity) {
		    if (PERCCHAN(chan) && dur > 0) dur = unity;
		    else if (dur >= unity/2) dur = unity;
		    else {
		        if (really_verbose) printf("note of precise dur %d skipped\n", dur);
		        continue;
		    }
		}

		loud = event_buff[i].n[2];

		/* Write one line of adagio score. */
		if (ad_print) {
		    /*(R.D. likes middle C to be 48, for some reason.)*/
		    put_pitch(pitch - 12);
		    printf(" u%ld l%d n%ld", round(dur), loud,
			   round(diffnext(i, last, now, &nxlie)));
		    if (adlastchan != chan) {
			printf(" v%d\n", chan + 1);
			adlastchan = chan;
		    } else
			printf("\n");
		}

	    } else if (command == MIDI_CH_PROGRAM) {
		new_prog = event_buff[i].n[1];

		/* Change programs on an adagio rest. */
		if (ad_print) {
		    printf("r z%d n%ld", event_buff[i].n[1],
			   round(diffnext(i, last, now, &nxlie)));
		    printf(" v%d\n", chan + 1);
		}

	    } else if (command == MIDI_CTRL &&
		       event_buff[i].n[1] == SUSTAIN) {
		ped[chan] = (event_buff[i].n[2] != 0);

	    /* Controller commands are buffered up in the ctrlval array, so they can
	     * be issued in proper synchrony with notes (or rests).
	     */
	    } else if (command == MIDI_CTRL) {
		char c = map_ctrl(thecontrol);
		int n = map_nctrl(thecontrol);

		/* Fill in pan values early for the sake of the K1, because panning can
		 * only be done statically, before starting to play.
		 */
		if (thecontrol == PAN && ext_pan[chan] == -1 &&
			setting_pstereo &&
			!user_ctrl_height[USER_PAN][chan])
		    ext_pan[chan] = thecontrolval;
		else if (thecontrol == CHORUS_DEPTH &&
			!user_ctrl_height[USER_CHORUS_DEPTH][chan])
		    chorus_depth[chan] = thecontrolval;
		else if (thecontrol == VOLUME &&
			!user_ctrl_height[USER_MAIN_VOLUME][chan])
		    main_volume[chan] = thecontrolval;
		else if (thecontrol == REVERBERATION &&
			!user_ctrl_height[USER_REVERBERATION][chan])
		    reverberation[chan] = thecontrolval;

		else if (thecontrol == NRPN_M) {
		    urpn1[chan] = thecontrolval;
		}
		else if (thecontrol == NRPN_L) {
		    urpn2[chan] = thecontrolval;
		}
		else if (thecontrol == DATA_ENTRY && PERCCHAN(chan)) {
		    if (urpn1[chan] == 1) {
			if (urpn2[chan] == 8) vibrato_rate[chan] = thecontrolval;
			else if (urpn2[chan] == 9) vibrato_depth[chan] = thecontrolval;
		    }
		    else if (urpn1[chan] == 28 && urpn2[chan] < NUM_DRUMS)
			drum_pan[urpn2[chan]] = thecontrolval;
		    else if (urpn1[chan] == 29 && urpn2[chan] < NUM_DRUMS)
			drum_reverberation[urpn2[chan]] = thecontrolval;
		    else if (urpn1[chan] == 30 && urpn2[chan] < NUM_DRUMS)
			drum_chorus_depth[urpn2[chan]] = thecontrolval;
		    urpn1[chan] = urpn2[chan] = 0;
		}


		/* Store command in ctrlval array. */
		if (n != 0)
		    doctrl(n, event_buff[i].n[2]);

		/* Write line of adagio score. */
		if (c != 0) {
		    if (ad_print)
			printf("%c%d n%d\n", c,
			       event_buff[i].n[2], round(diffnext(i, last, now, &nxlie)));
		} else if (n == 0)
		    bad_ctrl_flag = true;

	    /* Channel aftertouch? */
	    } else if (command == MIDI_TOUCH) {
		doctrl(4, event_buff[i].n[1]);
		if (ad_print)
		    printf("O%d n%d\n", event_buff[i].n[1],
			   round(diffnext(i, last, now, &nxlie)));

	    /* I never have decided what, if anything, to do about polyphonic aftertouch. */
	    } else if (command == MIDI_POLY_TOUCH) {
		/* ignore for now ...
		doctrl(4, event_buff[i].n[1]);
		if (ad_print) printf("O%d n%d\n", event_buff[i].n[1],
			round(diffnext(i,last,now,&nxlie)));
		*/

	    /* Pitchbend is treated as a controller. */
	    } else if (command == MIDI_BEND) {
		doctrl(6, event_bend(&event_buff[i]));
		if (ad_print) {
		    printf("Y%d n%d", (event_bend(&event_buff[i]) >> 6),
			   round(diffnext(i, last, now, &nxlie)));
		    if (adlastchan != chan)
			printf(" v%d\n", chan + 1);
		    else
			printf("\n");
		}

	    /* Noteoff commands are taken account of by getdur(), so we can just
	     * ignore them, here.
	     */
	    } else if (command != MIDI_ON_NOTE) {
		/* I don't think we ever get here. */
		if (verbose)
		    fprintf(stderr, "Command 0x%x ignored\n", command);
	    }

	    /* Now if any action is called for, insert a note, a rest, or a control. */
	    if (pitch_flag || ctrlflag[0] || lastchan != chan || new_prog > 0)
		out_of_memory = parsenote(&score);

	    lastchan = chan;
	}
    }

    /* haven't seen this message in a long time -- don't think it happens anymore */
    if (bad_ctrl_flag && verbose)
	fprintf(stderr,
	  "Some unrecognized control changes were omitted from file.\n");

    /* this is a relic from the msdos version */
    if (out_of_memory) {
	fprintf(stderr, "Out of note memory at line %d,\n", lineno - 1);
	fprintf(stderr, "    the rest of your file will be ignored.\n");
    }

    if (verbose && !ad_print)
	printf(
		  "Phase 1 completed; %d note(s), %d ctrl(s) have been translated.\n\n",
		  note_count, ctrl_count);

    /* Done. */
    free(event_buff);
    max_m_notes = -1;
    reverse(&score);
    return(score);
}

/****************************************************************************
*				put_pitch
* Inputs:
*	FILE *fp: an open file
*	int p: a pitch number
* Effect: write out the pitch name for a given number
****************************************************************************/

static void put_pitch(p)
int p;
{
    static char *ptos[] =
    {"c", "cs", "d", "ef", "e", "f", "fs", "g",
     "gs", "a", "bf", "b"};
    printf("%s%d", ptos[p % 12], p / 12);
}

/**********************************************************************
*			rec_final
* Inputs:
*	int absflag: output absolute time of first m_note if true
* Effect:
*	Write recorded data to a file (No, this doesn't happen now --gl)
**********************************************************************/

/* The midifile functions have extracted all the midi events from the
 * file now, and it's time to filter out excessive pitchbends and controls
 * and construct the list of notes.
 */
event_type rec_final(absflag)
int absflag;
{
    next->when = Mf_realtime;
    last = next;
    *shm_running_time = running_time;
    if (verbose && !ad_print) {
	printf(" %d:%02d minutes, %d times and events recorded.\n",
		running_time / 6000, (running_time/100) % 60,
		last - event_buff);
#ifdef XPOLYPHONY
	if (ext_dev >= 0) printf(" %d max ext polyphony/track.\n", max_ext_polyphony);
#endif
#ifdef WPOLYPHONY
	if (gus_dev >= 0) printf(" %d max gus polyphony/track.\n", max_wave_polyphony);
#endif
    }
    filter(last);
    return (phasem(last, absflag));
}

/****************************************************************************
*				rec_init
* Inputs:
*	char *file:  pointer to file name from command line (if any)
*	int bender: true if pitch bend should be enabled (I removed this --gl.)
* Outputs:
*	returns true if initialization succeeds (No, now no return value --gl.)
* Effect:
*	prepares module to record midi input
****************************************************************************/

void rec_init()
{
    if (max_m_notes == -1) {	/* allocate space 1st time rec_init called */
	max_m_notes = space / sizeof(m_note_node);
	event_buff = (m_note_type) malloc(sizeof(m_note_node) * max_m_notes);
	if (event_buff == NULL) {
	    fprintf(stderr, "Internal error allocating record space.");
	    exit(1);
	}
	byteorder();
	if (really_verbose && !ad_print)
	    printf("Space for %d events has been allocated.\n", max_m_notes);
    }
    next = event_buff;
    last = event_buff + max_m_notes - 2;


    return;
}
