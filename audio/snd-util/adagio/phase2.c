/* disable expression change within notes: */
#define NO_DYNAMIC_EXPRESSION
#define MAINTAIN_PRECISION
#define round(x) (((x)+8)>>4)

/* phase2.c -- this module plays notes compiled and sorted in phase1 or phasem */

/* There are 3 layers of routines here: (1) phase1 with various initialization
 * functions and the play_score routine that it calls, (2) f_note and other "f_"
 * functions called by play_score, which direct the information flow either to
 * stdout, to the external port, or to (3) fm_note and other "fm_" functions which
 * play the sound cards (including the gus, though it is not an fm device).
 */
/* (following for testing gus volume calculation) */
/* #define USE_OWN_GUS_VOL */

/* The following defines are configurable options.  Probably, they don't need to
 * be changed.
 */

/* In waiting for driver to empty sequencer queue, when centiseconds
 * to sleep goes below VIB_THRESH, vibrato is turned off in order to
 * lower data rate.
 */
#define VIB_THRESH 50

/* restart midi output when this number room in queue (I have problems
 * keeping output going to the external port -- I'm not sure this low
 * value of 32 (max 512) really helps, but sometimes it has seemed to): */
#define WAKETHRESH 32

/*
 * size of chunks written to /dev/sequencer -- making this too large
 * causes missing notes in output to external midi port
 */
#define SBOUTCHUNK 128
/*#define SBOUTCHUNK 64*/

/* don't use running status for external midi out: */
#define NO_RUNNING_STATUS

/* double notes with pitch bend for chorus depth: */
#define TRY_CHORUS

/* always reload fm voices to the driver from std and drums libraries: */
#define RELOAD_FM

/* use cute stereo effects: */
#define PSEUDO_STEREO

/* display info about noteon/off and records of dynamic voice allocation: */
#define END_DIAGNOSTICS

/* pitch vibrato for selected voices (see vibrato.h) */
#define TRY_VIBRATO

/* let gus play echo notes for external synth (define also in phasem.c) */
#ifdef K1
#define HELP_EXT_REVERB
#endif
#define MAX_DELAY 24

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <ctype.h>
#ifndef __386BSD__
#include <malloc.h>
#endif
#include <fcntl.h>
#include <string.h>
#include <sys/time.h>
#include <signal.h>
#include "adagio.h"
#ifndef XMP
#include "userio.h"
#include "cmdline.h"
#endif
#include "pitch.h"
#include "allphase.h"
#include "midicode.h"
#include "midi.h"
#define NO_LC_DEFINES
#include "midifile.h"
#include "sblast.h"

#ifdef XMP
#define STOPNOW _exit(0)
#else
#define STOPNOW exit(-1)
#endif

/* nominal very long ending time for songs, in centiseconds */
#define MAXTIME 10000000

/* in extvoice.c */
void setup_ext(int readable);
/* in gusvoice.c */
int find_tone_bank(int dev, int pgm, int bank);
int loadvoice(int, int, int, int, int);

#ifndef XMP
/* business for using functions in cmdline.c */
#define n_t_sw 2
static char *t_switches[n_t_sw] =
{"-t", "-trace"};
#define n_m_sw 2
static char *m_switches[n_m_sw] =
{"-m", "-midi"};
#define nmsw 2
static char *msw[nmsw] =
{"-i", "-init"};
#endif

#ifdef USE_SHM
extern int *shm_setting_pstereo;
extern int *shm_setting_gus_tuning;
extern int *shm_setting_gus_volume;
extern int *shm_setting_meter_color;
extern int *shm_setting_reverb;
extern int *shm_setting_chorus_spread;
extern int *shm_setting_vibrato_depth;
extern int *shm_setting_vibrato_speed;
extern int *shm_setting_vibrato_sweep;
extern int *shm_expression;
extern int *shm_main_volume;
extern int *shm_chorus_depth;
extern int *shm_reverberation;
extern int *shm_ext_pan;
#endif

/* variables set by command line options */
static int initflag = false;
static int use_damper = false;	/* currently always false */
static int readable = false;	/* tracing output */
static int local_extflag;

/* keeping time (in centiseconds) */
static unsigned long ptime = 0;		/* playing time clock */
static unsigned long sb_time = 0;	/* recording time clock */
static unsigned long lasttime = 0;	/* recording time */

/* this is updated by low-level routines and consulted by
 * play_score to see if it is currently feasible to play extra
 * notes for reverberation or chorusing effects
 */
static int spare_cells[MAXCARDS];

/* records for optional diagnostic display at end of playing */
static int stat_imp_cell_off[MAXCARDS];
static int stat_note_on[MAXCARDS];
static int stat_note_off[MAXCARDS];
static int stat_chorus_dropped[MAXCARDS];
static int stat_echoes_dropped[MAXCARDS];
#ifdef USE_OWN_GUS_VOL
static int gus_clip_statistic = 0;
#endif

unsigned char inst_note_count[NUM_CHANS];
unsigned short inst_note_velocity[NUM_CHANS];

static int user_scale = false;	/* true if user-defined scale */
static int bend[NUM_CHANS];	/* current pitch bend on channel */
static pitch_table pit_tab[128];/* scale definition */

static char playing_music = true;


/****************************************************************************
* Routines local to this module
****************************************************************************/
/* (this list is only high-level routines) */
static void off_init();
#ifndef XMP
static void tuninginit();
static void read_tuning();
#endif
static int note_offs();
static void off_schedule();
static void f_note();
static void f_touch();
static void f_program();
static void f_ctrl();
static void f_bend();



/* Records resulting from reading the fm patch libraries during
 * initialization.
 */

/* miscelleneous low-level forward declarations */
static void midisync(void);
static void fm_program(int, int, int, int);
static void fm_ctrl(int, int, int, int, int);
static void fm_bend(int, int, int);
static void card_touch_all(int, int, int);
static void fm_noteon(int, int, int, int, int, int, unsigned long);
static void fm_noteoff(int, int, int, int, int, int, int, unsigned long);
static int new_cell(int, unsigned long, int, int, int, int, int, int, int, int);
#ifdef TRY_VIBRATO
static void card_vibrate(int, unsigned long);
#endif
static void propagate_bend(unsigned long ctime);
#ifdef MIDIIN
static void listen();
int midigetc(void);
int midipending(void);
static void midimessage();
#endif
#ifdef XMP
static void send_meter_packet(int always);
#endif
static void show_markers();

static int xmp_epoch = -1;
static unsigned long xxmp_epoch = 0;
static unsigned time_expired = 0;
static struct timeval tv;

static void
time_sync()
{
	unsigned long jiffies;
/**
static unsigned last_time_expired = 0;
**/

	gettimeofday (&tv, NULL);
	jiffies = tv.tv_sec*100 + tv.tv_usec/10000;
	if (xmp_epoch < 0) {
		xxmp_epoch = jiffies;
		xmp_epoch = 0;
	}
	time_expired = jiffies - xxmp_epoch;
/**
if (!(ptime&0x3f) || time_expired - last_time_expired > 100) {
printf("time %d last %d sec %d usec %d\n",
time_expired, last_time_expired, tv.tv_sec, tv.tv_usec);
last_time_expired = time_expired;
}
**/
}

/* buffered output to /dev/sequencer */
#define Q_SZ 2048
static unsigned char sbbuf[Q_SZ];
static char buf[8];
static unsigned sbptr = 0, rtpbs = 0;
static unsigned wbptr = 0, rtpbw = 0;
static int vib_switch = true;

/*
 * Flush output to /dev/sequencer.  This routine has become a monster.
 * The idea is to monitor the output queue to avoid filling it completely.
 * The next two defines are for how many times when the queue is
 * checked it may remain unchanged before we decide that it is just stuck
 * and is never going to empty, then how much free room we maintain in
 * the queue.
 */
#define SBMONWAIT 40
#define OUTQSP 128

#define OUT_Q_USLEEP 50000

void sbflush(void)
{
    static unsigned long pptime = 0;
    int qroom, lastroom = 0;
    int count = SBMONWAIT;
    int sz;

    if (!playing_music) {
	wbptr = rtpbw = 0;
	return;
    }

    /* end of events written into our own buffer */
    sbptr = wbptr;

    /* nothing in the buffer? */
    if (sbptr - rtpbs == 0) return;

    /* Temporize, so long as writing the buffer to /dev/sequencer would result in less than
     * OUTQSP bytes free room in the queue.
     */
    while (true) {
	int sec_ahead, csec_ahead;

	/* bytes in buffer which we'd like to send to driver */
	sz = sbptr - rtpbs;

	/* ask driver how much room in its queue */
        if (ioctl(seq_fd, SNDCTL_SEQ_GETOUTCOUNT, &qroom) == -1) {
	    fprintf(stderr, "GETOUTCOUNT failed\n");
	    STOPNOW;
	}
	/* convert from 4-byte events to bytes */
	qroom <<= 2;

	/* shall we send some?  or wait for more room to become available. */
	if (qroom - OUTQSP >= SBOUTCHUNK || qroom - OUTQSP >= sz) {
	    unsigned long  *delay;
	    int maxsz = qroom - OUTQSP;
	    int i = 0;
	    /* most bytes we can send is maxsz */
	    if (maxsz > sz) maxsz = sz;
	    /* look through buffer to avoid splitting up 8-byte events */
	    while (i < maxsz) {
		unsigned char sq_ev = sbbuf[rtpbs+i];
		/* keep track of last sync time sent, in order to calculate
		 * how much time to sleep when driver queue is too full
		 */
		if (sq_ev == SEQ_WAIT) {
		    delay = (unsigned long *)(sbbuf+rtpbs+i);
		    pptime = (*delay >> 8) & 0xffffff;
		}
		if (sq_ev >= 0xf0) i += 8;
		else i += 4;
	    }
	    /* reset end of stuff to be written from buffer */
	    sbptr = rtpbs+i;
	    /* go write it now */
	    break;
	}

	/* here driver queue is too full, but if there's room in buffer,
	 * we can leave and go calculate some more events to send
	 */
	if (Q_SZ - wbptr > SBOUTCHUNK) {
	    rtpbw = wbptr;
	    return;
	}

/* this count business is temporarily disabled ... */
	/* just starting the count? if so, note how much room there is */
	if (count == SBMONWAIT) lastroom = qroom;

	/* get current song time to estimate how far ahead we are of
	 * what event driver is now executing
	 */
#ifndef NO_USE_SLEEP
	time_sync();
	/* estimate how long the events already sent to the driver
	 * will take to be executed, and sleep half that time
	 */
	if (pptime >= time_expired)
	    csec_ahead = (pptime - time_expired) / 2;
	else
	    goto no_sleep;
#ifdef THIS_PIECE_OF_CODE_IS_WRONG      /* Ache */
	/* if we've gotten behind, it's hopeless, so abort */
	else {
	    fprintf(stderr, "I've gotten behind by %dcsec at time %d csec\n",
	    time_expired - pptime, time_expired);
fprintf(stderr, "sz %d, wbptr %d, rtpbs %d, ptime %d, qroom %d\n",
sz,wbptr,rtpbs,ptime,qroom);
	    STOPNOW;
	}
#endif
/**
if (vib_switch && csec_ahead <= VIB_THRESH) fprintf(stderr,"vib off\n");
if (!vib_switch && csec_ahead > VIB_THRESH) fprintf(stderr,"vib on\n");
**/
	/* driver cannot keep up with high data rate, so if we're not
	 * much ahead of the game, take the precaution of turning off
	 * vibrato to reduce number of events to send
	 */
	vib_switch = (csec_ahead > VIB_THRESH);
	sec_ahead = csec_ahead / 100;
	if (sec_ahead) {
/**
if (really_verbose) printf("pptime %d, ptime %d, time_expired %d\n",
pptime, ptime, time_expired);
		if (really_verbose) printf("sleeping %d secs\n", sec_ahead);
**/
		sleep(sec_ahead);
	}
	else if (csec_ahead > 0) usleep(csec_ahead * 10000);
no_sleep:
#endif

#ifndef XMP
	show_markers();
#endif
	/* timeout?  if so, maybe there is a problem */
	if (count-- == 0) {

	    /* it's ok when progress has been made, so restart the count */
#ifndef OUT_Q_USLEEPX
	    count = SBMONWAIT;
#else
/* following stuff is disabled now */
	    if (qroom > lastroom) count = SBMONWAIT;
	    /* if we cannot write without overrunning the queue, give up */
	    else if (qroom < (sbptr-rtpbs)) {
		fprintf(stderr,
		  "problem with output queue ... only %d room at t=%d\n", qroom, ptime);
		(void) ioctl(seq_fd, SNDCTL_SEQ_RESET, 0);
		STOPNOW;
		ptime = sb_time = 0;
		break;
	    /* here, we are not able to maintain the free space in the queue
	     * that we wanted to, but at least there is room to hold what we
	     * want to output, so we'll write it out anyway
	     */
	    } else break;
#endif
	}
#ifdef MIDIIN
	/* while waiting for output, get any stuff pending from the midi
	 * input port
	 */
	listen();
#endif
    }

    /* now flush the buffer */
    if (write(seq_fd, sbbuf+rtpbs, sbptr-rtpbs) == -1) {
	perror("write /dev/sequencer");
	STOPNOW;
    }
    /* new start point for write from buffer */
    rtpbs = sbptr;
    /* rtpbw is beginning of chunk to be written into buffer */
    if (wbptr == sbptr) wbptr = rtpbw = rtpbs = 0;
    else rtpbw = wbptr;

    /* how much left in buffer still to send */
    sz = wbptr - rtpbs;
    /* make more room to write at end of buffer */
    if (Q_SZ - wbptr <= SBOUTCHUNK && rtpbs >= SBOUTCHUNK) {
	memmove(sbbuf, sbbuf+rtpbs, sz);
	rtpbs = 0;
	wbptr = rtpbw = sbptr = sz;
    }
}

/* write an 8-byte event to the output buffer */
static void sqwrite(char *msg)
{
    if (wbptr - rtpbw + 8 >= SBOUTCHUNK)
	sbflush();
    memcpy(&sbbuf[wbptr], msg, 8);
    wbptr += 8;
}
/* write a 4-byte event to the output buffer */
static void sbwrite(char *msg)
{
    if (wbptr - rtpbw + 4 >= SBOUTCHUNK)
	sbflush();

    memcpy(&sbbuf[wbptr], msg, 4);
    wbptr += 4;
}
/* write a 4-byte event to the output buffer, and attempt
 * to keep all parts of a midi command to the output port
 * together in the same write to the driver
 */
static void s12write(char *msg)
{
    if (wbptr - rtpbw + 12 >= SBOUTCHUNK)
	sbflush();

    memcpy(&sbbuf[wbptr], msg, 4);
    wbptr += 4;
}
/* reset driver's idea of now to now (do this just
 * before starting to play)
 */
static void sb_resync()
{
    buf[0] = SEQ_SYNCTIMER;
    sbwrite(buf);
    sbflush();
    ptime = sb_time = lasttime = 0;
    xmp_epoch = -1;
    time_sync();
    midisync();
}

/* this is called from extvoice.c */
void reopen_midi_in()
{
#ifdef MIDIIN
    int n;
    /* At the time /dev/sequencer was opened as write-only, it was not
     * known whether we needed midi input.  Now we know, so reopen it
     * as read-write, if need be.
     */
    if (midi_is_open_for_input) return;
    if (ext_dev >= 0) {
	if (seq_fd >= 0) close(seq_fd);
	/* It has to be non-blocking, to keep enough events in the driver's
	 * queue to make sure gus volume is set _immediately_ after a note
	 * has started.  Otherwise, can hear the volume the driver sets,
	 * which is too loud.
	 */
	if ((seq_fd = open("/dev/sequencer", O_NONBLOCK|O_RDWR, 0)) == -1) {
	    fprintf(stderr, "couldn't reopen /dev/sequencer for input\n");
	    STOPNOW;
	}
	midi_is_open_for_input = true;
    }
    /* A small threshold minimizes problem of lost note-off events to
     * the midi output port.
     */
    n = WAKETHRESH;
    ioctl(seq_fd, SNDCTL_SEQ_TRESHOLD, &n);
#endif
}

/* this is called from extvoice.c */
void reopen_midi_out()
{
    int n;
    if (!midi_is_open_for_input) return;
    if (seq_fd >= 0) close(seq_fd);
    if ((seq_fd = open("/dev/sequencer", O_NONBLOCK|O_WRONLY, 0)) == -1) {
	perror("reopen /dev/sequencer to write");
	exit(1);
    }
    midi_is_open_for_input = false;
    n = WAKETHRESH;
    ioctl(seq_fd, SNDCTL_SEQ_TRESHOLD, &n);
}

/* one 7-bit byte to the external midi port */
void midich(char c)
{
    /* if the 8th bit is set, there is an error somewhere */
    if (c < 0) fprintf(stderr, "error in midich: 0x%02x??\n", c);

    buf[0] = SEQ_MIDIPUTC;
    buf[1] = c;
    buf[2] = ext_dev;
    buf[3] = 0;
    sbwrite(buf);
}
/* one 8-bit byte to the external midi port */
void midicmdch(char c)
{
#ifndef NO_RUNNING_STATUS
    static char lastcmd = 0;
    if (c == lastcmd)
	return;
    lastcmd = c;
#endif
    /* if the 8th bit is not set, that's a problem */
    if (c > 0) fprintf(stderr, "error in midicmdch: 0x%02x??\n", c);
    buf[0] = SEQ_MIDIPUTC;
    buf[1] = c;
    buf[2] = ext_dev;
    buf[3] = 0;
    s12write(buf);
}

#ifdef MIDIIN
/* non-blocking input of one byte from the external midi port */
int inonenb()
{
    if (!midipending()) {
	usleep(100000);
	if (!midipending()) {
	    usleep(1000000);
	    if (!midipending()) {
		usleep(1000000);
		if (!midipending())
		    return (-1);
	    }
	}
    }
    return ((midigetc() & 0xff));
}
#endif

/* routines called by phase2 */
static void crd_init();
static void cells_init();
static void voices_init();
static void channel_init();
static void play_score();

/* The pointer to the note list has to be passed indirectly from
 * phase1 to play_score, because the latter is called from inside
 * the midifile library when recording a track.
 */
static event_type the_score;
/* next two routines are called from inside midifile library */
FILE *fp;
int myputc(int c)
{
    return (putc(c, fp));
}

int mywritetrack(int track)
{
    play_score();
    return (1);
}


static int got_signal = 0;
void (*oldhup)();
void (*oldint)();
void (*oldterm)();
#ifdef SIGTSTP
void (*oldstp)();
#endif

static void
set_got_signal(int arg)
{
	if (got_signal) {
	    got_signal = arg;
#ifndef XMP
	    if (arg == SIGINT
#ifdef SIGTSTP
		|| arg == SIGTSTP
#endif
	       )
		fprintf(stderr, " be patient ...\n");
#endif
	    return;
	}
	got_signal = arg;
#ifndef XMP
	fprintf(stderr, " stopping ...\n");
#endif
}

static void
stop_notes(int arg)
{
	int room;

#ifdef SIGTSTP
	if (arg != SIGTSTP)
#endif
	    note_offs(MAXTIME);
	sbflush();
	while ((ioctl(seq_fd, SNDCTL_SEQ_GETOUTCOUNT, &room) != -1) &&
		(room < seq_max_queue)) {
#ifdef OUT_Q_USLEEP
	    usleep(OUT_Q_USLEEP);
#endif
#ifdef XMP
	    send_meter_packet(false);
#endif
	}
#ifdef SIGTSTP
	if (arg != SIGTSTP)
#endif
	{
	    if (ext_dev >= 0) (void)ioctl(seq_fd, SNDCTL_SEQ_RESET, 0);
	    STOPNOW;
	}
#ifdef SIGTSTP
	else {
	    unsigned saved_time;

	    time_sync();
	    saved_time = time_expired;
	    signal(SIGTSTP, oldstp);
	    kill(0, SIGTSTP);
	    /* ... */
	    got_signal = 0;
	    signal(SIGTSTP, set_got_signal);
	    time_sync();
	    xxmp_epoch += time_expired - saved_time;
	}
#endif
}


/****************************************************************************
*				    phase2
* Inputs:
*	event_type root: Root of play list
* Effect:
*	Plays the music
****************************************************************************/

void phase2(score)
event_type score;
{
#ifdef XMP
    char *recording_parms = "";
#else
    char *recording_parms = cl_option("-r");
    char *controller_opt = cl_option("-R");

    if (controller_opt != NULL) {
	int i, n = atoi(controller_opt);
	if (n < 0) n = 0; if (n > 127) n = 127;
	for (i = 0; i < NUM_CHANS; i++)
	    reverberation[i] =
	    user_ctrl_height[USER_REVERBERATION][i] = n;
    }
    controller_opt = cl_option("-C");
    if (controller_opt != NULL) {
	int i, n = atoi(controller_opt);
	if (n < 0) n = 0; if (n > 127) n = 127;
	for (i = 0; i < NUM_CHANS; i++)
	    chorus_depth[i] =
	    user_ctrl_height[USER_CHORUS_DEPTH][i] = n;
    }

    readable = (cl_nswitch(t_switches, n_t_sw) != NULL);
    playing_music = (cl_nswitch(m_switches, n_m_sw) == NULL);
    initflag = (cl_nswitch(msw, nmsw) == NULL);
#endif

    if (ext_dev >= 0 && !recording_track) {
	int chan;
	for (chan = 0; chan < NUM_CHANS; chan++) if (ext_chan[chan]) break;
	if (chan == NUM_CHANS) ext_dev = -1;
    }
    local_extflag = extflag;
    if (ext_dev < 0 || !playing_music) local_extflag = false;
    if (local_extflag) playing_music = true;
    /*if (readable) playing_music = false;*/

    if (recording_track) {
	int i;
	recording_channel = atoi(recording_parms);
	for (i = 0; recording_parms[i] != '\0' && recording_parms[i] != ','; i++);
	if (recording_parms[i] == ',')
	    recording_program =
		atoi(recording_parms + i + 1);
	if (verbose)
	    printf("recording program %d!\n", recording_program);
	if (recording_channel < 1 || recording_channel > 16)
	    recording_channel = 1;
	if (recording_program < 0 || recording_program > 127)
	    recording_program = 0;
	if (verbose)
	    printf("recording program is #%d\n", recording_program);
    }

    off_init();				/* allocate some off-event structures */
#ifndef XMP
    tuninginit();			/* process any user tuning file */
#endif
    if (playing_music) cells_init();	/* mark up dynamically allocated voices */
    voices_init();			/* verbose display of
					 *  what voices are on what channels
					 */
    if (local_extflag) setup_ext(readable);
    if (playing_music) crd_init();	/* init gus and fm cards */

    channel_init();			/* initialize midi controllers and pitch bend */
    the_score = score;			/* pass note list to play_score */

    oldhup = signal(SIGHUP, set_got_signal);
    oldint = signal(SIGINT, set_got_signal);
    oldterm = signal(SIGTERM, set_got_signal);
#ifdef SIGTSTP
    oldstp = signal(SIGTSTP, set_got_signal);
#endif

    /* To start playing, either pass call-backs to midifile library,
     * or if not recording a track, call play_score directly.
     */
    if (recording_track) {
	Mf_putc = myputc;
	Mf_writetrack = mywritetrack;
	if ((fp = fopen("mpout.mid", "w")) == 0L) STOPNOW;
	mfwrite(0, 1, Mf_division, fp);
    } else
	play_score();

    signal(SIGINT, oldhup);
    signal(SIGINT, oldint);
    signal(SIGTERM, oldterm);
#ifdef SIGTSTP
    signal(SIGTSTP, oldstp);
#endif

    /* display final diagnostic report */
    if (verbose) {
#ifdef END_DIAGNOSTICS
	void hist_report();
#endif
	printf("\n");
	if (sb_dev >= 0)
	    printf("%d fm notes terminated due to conflicts.\n", stat_imp_cell_off[sb_dev]);
	if (gus_dev >= 0)
	    printf("%d gus notes terminated due to conflicts.\n", stat_imp_cell_off[gus_dev]);
#ifdef XPOLYPHONY
	if (ext_dev >= 0)
	    printf("%d ext notes terminated due to conflicts.\n", stat_imp_cell_off[ext_index]);
#endif
#ifdef END_DIAGNOSTICS
	if (really_verbose) {
	    printf("Notes on:  gus %d  fm %d  ext %d\n",
	       gus_dev >= 0 ? stat_note_on[gus_dev] : 0,
	       sb_dev >= 0 ? stat_note_on[sb_dev] : 0,
	       ext_dev >= 0 ? stat_note_on[ext_dev] : 0);
	    printf("Notes off: gus %d  fm %d  ext %d\n",
	       gus_dev >= 0 ? stat_note_off[gus_dev] : 0,
	       sb_dev >= 0 ? stat_note_off[sb_dev] : 0,
	       ext_dev >= 0 ? stat_note_off[ext_dev] : 0);
	    printf("Chorus notes dropped:\n           gus %d  fm %d  ext %d\n",
	       gus_dev >= 0 ? stat_chorus_dropped[gus_dev] : 0,
	       sb_dev >= 0 ? stat_chorus_dropped[sb_dev] : 0,
	       ext_dev >= 0 ? stat_chorus_dropped[ext_dev] : 0);
	    printf("Reverb notes dropped:\n           gus %d  fm %d  ext %d\n",
	       gus_dev >= 0 ? stat_echoes_dropped[gus_dev] : 0,
	       sb_dev >= 0 ? stat_echoes_dropped[sb_dev] : 0,
	       ext_dev >= 0 ? stat_echoes_dropped[ext_dev] : 0);
	    hist_report();
	}
#endif
#ifdef USE_OWN_GUS_VOL
	printf("gus clips: %d\n", gus_clip_statistic);
#endif
    }
}


/*
 * Initialize gus and fm cards and note info about
 * cells (i.e, dynamically allocated voices).
 */
void crd_init()
{
    int n;
    void gus_max_voices(int);

    /* 24 max voices is the default, but current 1.99.3 driver does not
     * report it correctly.  We have to set the number here, instead of
     * earlier in cardinit() called from admp.c, since /dev/sequencer
     * may have been closed and reopened since then.
     */
    if (gus_dev >= 0) {
	gus_max_voices(setting_gus_voices);
	card_info[gus_dev].nr_voices = setting_gus_voices;
	spare_cells[gus_dev] = card_info[gus_dev].nr_voices;
    }

    if (sb_dev >= 0) {
	spare_cells[sb_dev] = card_info[sb_dev].nr_voices;
    }

    if (ext_dev >= 0) {
	card_info[ext_index].nr_voices =
	spare_cells[ext_index] =
#ifdef XPOLYPHONY
		XPOLYPHONY;
#else
		0;
#endif
    }

    /* If any 4op patch was loaded to the driver from one of the fm
     * patch libraries, we must ask for 4op mode.
     */
    if ((n = sb_dev) >= 0 && need_4op_mode &&
		card_info[n].synth_subtype == FM_TYPE_OPL3) {
	ioctl(seq_fd, SNDCTL_FM_4OP_ENABLE, &n);
	if (ioctl(seq_fd, SNDCTL_SYNTH_INFO, &card_info[sb_dev]) == -1) {
	    fprintf(stderr, "cannot get info on soundcard\n");
	    perror("/dev/sequencer");
	    STOPNOW;
	}
	card_info[sb_dev].device = sb_dev;
	/* We have 12 voices, despite what the driver reports. */
	if (card_info[sb_dev].nr_voices == 6) {
#ifdef FM_4OP_12
	    card_info[sb_dev].nr_voices = 12;
#endif
	    spare_cells[sb_dev] = 6;
	}
	else spare_cells[sb_dev] = card_info[sb_dev].nr_voices;
    }

    if (verbose && sb_dev >= 0)
	printf("%d fm voices, %d instruments.\n",
	       card_info[sb_dev].nr_voices,
	       card_info[sb_dev].instr_bank_size);
    if (verbose && gus_dev >= 0)
	printf("%d gus voices, %d instruments.\n",
	       card_info[gus_dev].nr_voices,
	       card_info[gus_dev].instr_bank_size);
#ifdef XPOLYPHONY
    if (verbose && ext_dev >= 0)
	printf("%d ext voices.\n",
	       card_info[ext_index].nr_voices);
#endif
}

/*
 * Return tone name.
 */
char *name_of_tone(int prog, int b)
{ char *nm = NULL;

  switch (prog) {
    case   4: if (b==8) nm = "Detuned EP 1"; break;
    case   5: if (b==8) nm = "Detuned EP 2"; break;
    case   6: if (b==8) nm = "Coupled Harpsichord"; break;
    case  14: if (b==8) nm = "Church Bell"; break;
    case  16: if (b==8) nm = "Detuned Organ 1"; break;
    case  17: if (b==8) nm = "Detuned Organ 2"; break;
    case  19: if (b==8) nm = "Church Organ 2"; break;
    case  21: if (b==8) nm = "Accordion Italian"; break;
    case  24: if (b==8) nm = "Ukulele"; break;
    case  25: if (b==8) nm = "12 String Guitar";
	      else if (b==16) nm = "Mandolin"; break;
    case  26: if (b==8) nm = "Hawaiian Guitar"; break;
    case  27: if (b==8) nm = "Chorus Guitar"; break;
    case  28: if (b==8) nm = "Funk Guitar"; break;
    case  30: if (b==8) nm = "Dist Guitar w/ Feedback"; break;
    case  31: if (b==8) nm = "Guitar Feedback"; break;
    case  38: if (b==8) nm = "Synth Bass 3"; break;
    case  39: if (b==8) nm = "Synth Bass 4"; break;
    case  48: if (b==8) nm = "Orchestra"; break;
    case  50: if (b==8) nm = "Synth Strings 3"; break;
    case  61: if (b==8) nm = "Brass 2"; break;
    case  62: if (b==8) nm = "Synth Brass 3"; break;
    case  63: if (b==8) nm = "Synth Brass 4"; break;
    case  80: if (b==8) nm = "Sine Wave"; break;
    default: nm = NULL; break;
  }
  return(nm);
}
/*
 * Display voices and channels if -v flag was given, and
 * set up Kawaii K1.
 */
void voices_init()
{
    int n;
#ifdef PSEUDO_STEREO
#ifdef USE_SHM
    setting_pstereo = *shm_setting_pstereo;
#endif
    if (setting_pstereo) {
	int scount = 0;
	for (n = 0; n < NUM_CHANS; n++) {
	    if (ext_pan[n] >= 0) break;
	    if (program[n] > 0) scount++;
	    /*if (ext_chan[n] > 0) scount++;*/
	}
	if (n == NUM_CHANS && scount > 3) for (n = 0; n < NUM_CHANS; n++) {
	    if (n % 2) scount = 255 - 8*(n-1);
	    else scount = 8*n;
	    if (scount > 128) scount -= 128;
	    if (scount >= 128) scount = 127;
	    ext_pan[n] = scount;
	}
    }
#endif

    /* Display each channel. */

    if (verbose) for (n = 0; n < NUM_CHANS; n++) {
	int v, xv, tpgm;
	char *gs_name;
	static char *pan[3] = {"right", "center", "left"};
	int leftright = 1;
	int bank = (voice_bank[n] >> 8) & 0x7f;

	if (ext_pan[n] != -1) {
	    if (ext_pan[n] < 54) leftright = 2;
	    else if (ext_pan[n] > 74) leftright = 0;
	}

	if (!local_extflag) ext_chan[n] = 0;
	/* "ext_program" has the first program requested for each
	 * channel, or else -1 or 0 if there was no program request
	 * (0 if notes were played on the channel)
	 */
	v = program[n] - 1;
	/* if notes were played but there was no program request, use
	 * program 0, Ac. Gr. Piano, as default.
	 */
	if (v < 0) v = 0;

	gs_name = gm_voice[v].vname;

	if ((sb_dev >= 0 || gus_dev >= 0) && PERCCHAN(n) && v >= 0) {
		printf("  channel %2d: %s percussion at %s", n + 1,
			(sb_dev>=0 && gus_dev>= 0)? "gus & fm" :
		       ( (sb_dev >= 0) ? "fm" : "gus"),
			pan[leftright]);
		if (program[n] > 0) printf(" (drum set #%d)", v);
		printf(".\n");
	}
	if (PERCCHAN(n)) continue;

	tpgm = v;
	xv = -1;

	/* Following is just an experiment and not useful, currently. */
/** must redo this
	if (sb_dev >= 0 && fm_sysex[n] != NULL) {
	    int i;
	    struct sbi_instrument instr;

	    program[n] = 240 + n + 1;
	    instr.channel = 240 + n;
	    instr.device = sb_dev;
	    instr.key = FM_PATCH;
	    for (i = SBOFFSET; i < SBVOICESIZE; i++)
		instr.operators[i - SBOFFSET] = fm_sysex[n][i + 8];
	    if (write(seq_fd, (char *) &instr, sizeof(instr)) == -1) {
		fprintf(stderr, "can't load instrument\n");
		STOPNOW;
	    }
	    if (verbose)
		printf("  channel %2d: sysex %s\n", n + 1, fm_sysex[n] + 4 + 8);
	}
**/
#ifdef K1
	if (ext_chan[n]) {
	    int etot = ext_chan[n] - 1;
	    char *extname;
	    int xbank = (ext_voice_bank[n] >> 8) & 0x7f;
	    xv = ext_program[etot] - 1;
	    tpgm = find_tone_bank(ext_index, xv, xbank);
	    if ( tpgm == v || (gs_name = name_of_tone(v, xbank)) == NULL )
	        gs_name = gm_voice[v].vname;
	    if (ext_voice[tpgm].vname != NULL)
		extname = ext_voice[tpgm].vname;
	    else extname = gs_name;
	    if (ext_voice[tpgm].loaded)
		printf("  channel %2d: %s[%d,%d] to K1 channel %d at %s.\n", n + 1,
	            extname, xv, ext_voice[tpgm].bank,
		    etot + 1, pan[leftright]);
	}
#else
	if (ext_chan[n]) {
	    xv = v;
	    printf("  channel %2d: %s[%d,%d] external.\n", n + 1,
		       gs_name, xv, bank);
	}
#endif
	if (program[n] > 0 && xv != v) {
	    char *voice_name = NULL, *device_name = NULL;

	    if (gus_dev >= 0) {
		tpgm = find_tone_bank(gus_dev, v, bank);
		if (gus_voice[tpgm].loaded && gus_voice[tpgm].vname != NULL) {
		    device_name = "gus";
		    voice_name = gus_voice[tpgm].vname;
		    bank = gus_voice[tpgm].bank;
		}
	    }
	    if (sb_dev >= 0 && device_name == NULL) {
		tpgm = find_tone_bank(sb_dev, v, bank);
		if (fm_voice[tpgm].loaded) {
		    device_name = "fm";
		    voice_name = fm_voice[tpgm].vname;
		    bank = fm_voice[tpgm].bank;
		}
	    }
	    if (device_name == NULL) {
/** want to display channels with programs but no notes?
		if (ext_chan[n]) continue;
		device_name = "nothing";
**/
		continue;
	    }
	    if ( tpgm == v || (gs_name = name_of_tone(v, bank)) == NULL )
	        gs_name = gm_voice[v].vname;
	    if (voice_name == NULL) voice_name = gs_name;

	    printf("  channel %2d: %s[%d,%d] on %s at %s.\n", n + 1,
		       voice_name, v, bank,
			device_name, pan[leftright]);
	}
    }
}


/*
 * Send default values to all synths.
 */
void channel_init()
{
    int i;

    /* Initialize all midi channels with reasonable start values: */
    for (i = 1; i <= NUM_CHANS; i++) {
	int card;
	int d = ext_chan[i - 1];

	if (sb_dev >= 0) card = sb_dev;
	else if (gus_dev >= 0) card = gus_dev;
	else continue;

	if (program[i - 1] <= 0) program[i - 1] = 1;
	bend[i - 1] = 1 << 13;
	if (!user_ctrl_height[USER_MAIN_VOLUME][i-1])
	    main_volume[i-1] = NORMAL_VOLUME;
	if (!user_ctrl_height[USER_EXPRESSION][i-1])
	    expression[i-1] = NORMAL_EXPR;
/**
else printf("preset expression is %d on chan %d\n", expression[i-1], i);
**/
	if (!user_ctrl_height[USER_REVERBERATION][i-1])
	    reverberation[i-1] = 0;
/**
else printf("preset reverb is %d on chan %d\n", reverberation[i-1], i);
**/
	if (!initflag) continue;

	f_program(card, 0, i, program[i - 1]);
	f_bend(card, 0, i, 1 << 13);
	f_touch(card, 0, i, 0);
	f_ctrl(card, 0, i, PORTARATE, 99);
	f_ctrl(card, 0, i, PORTASWITCH, 0);
	f_ctrl(card, 0, i, MODWHEEL, modulation_wheel[i-1]);
	f_ctrl(card, 0, i, FOOT, 99);
	f_ctrl(card, 0, i, VOLUME, NORMAL_VOLUME);

	if (!d || ext_dev < 0)
	    continue;

	card = ext_index;
	program[i - 1] = ext_program[i - 1];
	if (program[i - 1] <= 0)
	    ext_program[i - 1] = program[i - 1] = 1;

	f_program(card, d, i, program[i - 1]);
	f_bend(card, d, i, 1 << 13);
	f_touch(card, d, i, 0);
	f_ctrl(card, d, i, PORTARATE, 99);
	f_ctrl(card, d, i, PORTASWITCH, 0);
	f_ctrl(card, d, i, MODWHEEL, modulation_wheel[i-1]);
	f_ctrl(card, d, i, FOOT, 99);
	f_ctrl(card, d, i, VOLUME, NORMAL_VOLUME);
    }
}

#ifdef MIDIIN
/*
 * During recording, calculate the midi delta time.  Use the total
 * time, sb_time, instead of just time since last delta, to maintain
 * accuracy.
 */
unsigned long getdelta()
{
    static double last_ticks = 0;
    double ticks = ((double) sb_time * 10000 * (double) Mf_division) / (double) Mf_currtempo;
    unsigned long delta_ticks = (unsigned long) (ticks - last_ticks);
    last_ticks = ticks;
    return (delta_ticks);
}

/*
 * Get anything that has arrived at the midi input port and write it
 * out to the track file if we are recording, else discard it.
 */
void listen()
{
    if (!midi_is_open_for_input) return;
    while (midipending()) {
	if (recording_track)
	    midimessage();
	else
	    (void) midigetc();
    }
}
#endif


#ifdef XMP

#ifdef USE_SHM
#define RINGSIZE 64
extern int *talk_in, *talk_out;

struct mp_talk {
	unsigned mptime;
	unsigned char count[NUM_CHANS];
	unsigned short vel[NUM_CHANS];
	int expr[NUM_CHANS];
};

extern struct mp_talk **mpd;
#endif

static int inst_drum_9_pan = -1;
static int inst_drum_15_pan = -1;
static int inst_drum_9_reverb = 0;
static int inst_drum_15_reverb = 0;
static int inst_drum_9_cdepth = -1;
static int inst_drum_15_cdepth = -1;
/*
 * Send packet of information to mother xmp process for meter display.
 */
static void
send_meter_packet(int always)
{   static int last_write_time = -1;
    int i;
#ifdef USE_SHM
    int r;
    static void send_user_control();
#endif

    if ( (ptime/10) != last_write_time || always ) {

#ifdef USE_SHM
	if (last_write_time < 0) {
		*talk_in = 0;
	}
	else if (*talk_in - *talk_out >= RINGSIZE - 2) return;

	r = *talk_in % RINGSIZE;

	for (i = 0; i < NUM_CHANS; i++) {
	    int val;
	    mpd[r]->count[i] = curr_note_count[i];
	    mpd[r]->vel[i] = curr_note_velocity[i];
	    val = -1;
	    switch (*shm_setting_meter_color) {
		case USER_EXPRESSION:
			val = expression[i];
			break;
		case USER_MAIN_VOLUME:
			val = main_volume[i];
			break;
		case USER_CHORUS_DEPTH:
			if (PERCCHAN(i) && i == 9 && inst_drum_9_cdepth >= 0)
				val = inst_drum_9_cdepth;
			else if (PERCCHAN(i) && i == 15 && inst_drum_15_cdepth >= 0)
				val = inst_drum_15_cdepth;
			else val = chorus_depth[i];
			break;
		case USER_REVERBERATION:
			if (PERCCHAN(i) && i == 9 && inst_drum_9_reverb > 0)
				val = inst_drum_9_reverb;
			else if (PERCCHAN(i) && i == 15 && inst_drum_15_reverb > 0)
				val = inst_drum_15_reverb;
			else val = reverberation[i];
			if (val < 0) val = 0;
			break;
		case USER_PAN:
			if (PERCCHAN(i) && i == 9 && inst_drum_9_pan >= 0)
				val = inst_drum_9_pan;
			else if (PERCCHAN(i) && i == 15 && inst_drum_15_pan >= 0)
				val = inst_drum_15_pan;
			else val = ext_pan[i];
			if (val < 0) val = 64;
			break;
	    }
	    mpd[r]->expr[i] = val;
	    if ((val = shm_expression[i])) {
		if (val < 8) val = 0;
		user_ctrl_height[USER_EXPRESSION][i] = val;
	    }
	    if ((val = shm_main_volume[i])) {
		if (val < 8) val = 0;
		user_ctrl_height[USER_MAIN_VOLUME][i] = val;
	    }
	    if ((val = shm_chorus_depth[i])) {
		if (val < 8) val = 0;
		user_ctrl_height[USER_CHORUS_DEPTH][i] = val;
	    }
	    if ((val = shm_reverberation[i])) {
		if (val < 8) val = 0;
		user_ctrl_height[USER_REVERBERATION][i] = val;
	    }
	    if ((val = shm_ext_pan[i])) {
		if (val < 8) val = 0;
		user_ctrl_height[USER_PAN][i] = val;
	    }
	}
	mpd[r]->mptime = ptime;

	if (*talk_in - *talk_out < RINGSIZE - 2) (*talk_in)++;

	send_user_control();
#else
	write(1, curr_note_count, sizeof(curr_note_count));
	write(1, curr_note_velocity, sizeof(curr_note_velocity));
	write(1, &ptime, sizeof(ptime));
	switch (setting_meter_color) {
	    case USER_EXPRESSION: write(1, expression, sizeof(expression)); break;
	    case USER_MAIN_VOLUME: write(1, main_volume, sizeof(main_volume)); break;
	    case USER_CHORUS_DEPTH: write(1, chorus_depth, sizeof(chorus_depth)); break;
	    case USER_REVERBERATION: write(1, reverberation, sizeof(reverberation)); break;
	    case USER_PAN: write(1, ext_pan, sizeof(ext_pan)); break;
	}
	fflush(stdout);
#endif
	last_write_time = ptime / 10;
	for (i = 0; i < NUM_CHANS; i++) {
	    curr_note_count[i] = inst_note_count[i];
	    curr_note_velocity[i] = inst_note_velocity[i];
	    if (curr_note_count[i] && curr_note_velocity[i]/curr_note_count[i] > 50) {
		curr_note_count[i] = (3*curr_note_count[i]) / 2;
		curr_note_velocity[i] = (3*curr_note_velocity[i]) / 2;
	    }
	}
    }
}
#endif

/*
 * Send a control message.
 */
static void
send_control(int override, int card, int d, int chan, int ctrl, int ctrl_type, int val)
{
    switch (ctrl_type) {
    case 1:
	f_ctrl(card, d, chan + 1, PORTARATE, val);
	break;
    case 2:
	f_ctrl(card, d, chan + 1, PORTASWITCH, val);
	break;
    case 3:
	modulation_wheel[chan] = val;
	f_ctrl(card, d, chan + 1, MODWHEEL, val);
	break;
    case 4:
	f_touch(card, d, chan + 1, val);
	break;
    case 5:
	if (use_damper)
	    f_ctrl(card, d, chan + 1, FOOT, val);
	break;
    case 6:
	f_bend(card, d, chan + 1, val);
	break;
    case 7:
	if (user_ctrl_height[USER_MAIN_VOLUME][chan] && !override) break;
	main_volume[chan] = val;
	f_ctrl(card, d, chan + 1, VOLUME, val);
	break;
    case 8:
#ifdef USE_SHM
	setting_pstereo = *shm_setting_pstereo;
#endif
	if (!setting_pstereo) break;
	if (user_ctrl_height[USER_PAN][chan] && !override) break;
	ext_pan[chan] = val;
	f_ctrl(card, d, chan + 1, PAN, val);
	break;
    case 9:
	if (user_ctrl_height[USER_EXPRESSION][chan] && !override) break;
	expression[chan] = val;
	f_ctrl(card, d, chan + 1, EXPRESSION, val);
	break;
    case 15:
    /* for all controller events other than the above */
	if (ctrl == REVERBERATION) {
	    if (user_ctrl_height[USER_REVERBERATION][chan] && !override) break;
	    reverberation[chan] = val;
	}
	else if (ctrl == CHORUS_DEPTH) {
	    if (user_ctrl_height[USER_CHORUS_DEPTH][chan] && !override) break;
	    chorus_depth[chan] = val;
	}
	else if (ctrl == NRPN_M) {
	    urpn1[chan] = val;
	}
	else if (ctrl == NRPN_L) {
	    urpn2[chan] = val;
	}
	else if (ctrl == DATA_ENTRY && PERCCHAN(chan)) {
	    if (urpn1[chan] == 1) {
		if (urpn2[chan] == 8) vibrato_rate[chan] = val;
		else if (urpn2[chan] == 9) vibrato_depth[chan] = val;
	    }
	    else if (urpn1[chan] == 28 && urpn2[chan] < NUM_DRUMS)
		drum_pan[urpn2[chan]] = val;
	    else if (urpn1[chan] == 29 && urpn2[chan] < NUM_DRUMS)
		drum_reverberation[urpn2[chan]] = val;
	    else if (urpn1[chan] == 30 && urpn2[chan] < NUM_DRUMS)
		drum_chorus_depth[urpn2[chan]] = val;
	    urpn1[chan] = urpn2[chan] = 0;
	}
	f_ctrl(card, d, chan + 1, ctrl, val);
	break;
    default:
	break;
    }
}

#ifdef XMP
#ifdef USE_SHM
/*
 * Send controller messages for any changes in user
 * controllers (from xmp).
 */
static void
send_user_control()
{
    int chan, ctl, midi_ctl = 0, val, newval, ctl_type = 0;

    for (ctl = 0; ctl < USER_NUM_CTLS; ctl++)
    for (chan = 0; chan < NUM_CHANS; chan++)
    if ((newval = user_ctrl_height[ctl][chan])) {
	/* This is what comes of having three(!) different sets
	 * of numbers referring to the midi controllers.
	 */
	switch (ctl) {
	    case USER_EXPRESSION: val = expression[chan];
		midi_ctl = EXPRESSION;
		ctl_type = 9;
		break;
	    case USER_MAIN_VOLUME: val = main_volume[chan];
		midi_ctl = ctl_type = VOLUME;
		break;
	    case USER_CHORUS_DEPTH: val = chorus_depth[chan];
		midi_ctl = CHORUS_DEPTH;
		ctl_type = 15;
		break;
	    case USER_REVERBERATION: val = reverberation[chan];
		midi_ctl = REVERBERATION;
		ctl_type = 15;
		break;
	    case USER_PAN: val = ext_pan[chan];
		midi_ctl = PAN;
		ctl_type = 8;
		if (newval < 4) newval = -1;
		break;
	    default:
		perror("send_user_control");
		STOPNOW;
		break;
	}
	if (val == newval) continue;
/**
printf("chan %d, val %d, newval %d, midi_ctl %d\n",
chan+1, val, newval, midi_ctl);
**/
	if (sb_dev >= 0)
	send_control(true, sb_dev, 0, chan, midi_ctl, ctl_type, newval);
	if (gus_dev >= 0)
	send_control(true, gus_dev, 0, chan, midi_ctl, ctl_type, newval);
	if (ext_dev >= 0 && ext_chan[chan])
	send_control(true, ext_index, ext_chan[chan], chan, midi_ctl, ctl_type, newval);
    }
}
#endif
#endif

static int
get_pan(int chan, int pitch)
{
	int ret;
#ifdef XMP
	if ((ret=user_ctrl_height[USER_PAN][chan])) return(ret);
#endif
	if (PERCCHAN(chan) && pitch != NO_PITCH && pitch < NUM_DRUMS &&
	    (ret=drum_pan[pitch]) >= 0) return(ret);
	return(ext_pan[chan]);
}
static int
get_reverberation(int chan, int pitch)
{
	int ret;
#ifdef XMP
	if ((ret=user_ctrl_height[USER_REVERBERATION][chan])) return(ret);
#endif
	if (PERCCHAN(chan) && pitch != NO_PITCH && pitch < NUM_DRUMS &&
	    (ret=drum_reverberation[pitch]) >= 0) return(ret);
	return(reverberation[chan]);
}
static int
get_chorus_depth(int chan, int pitch)
{
	int ret;
#ifdef XMP
	if ((ret=user_ctrl_height[USER_CHORUS_DEPTH][chan])) return(ret);
#endif
	if (PERCCHAN(chan) && pitch != NO_PITCH && pitch < NUM_DRUMS &&
	    (ret=drum_chorus_depth[pitch]) >= 0) return(ret);
	return(chorus_depth[chan]);
}

static void
show_markers()
{
    struct meta_text_type *meta;

    if (!verbose || meta_text_list == NULL) return;

    time_sync();

    for (meta = meta_text_list; meta != NULL; meta = meta->next)
	if (meta->time <= time_expired) {
	    printf("%s\n", meta->text);
	    meta_text_list = meta->next;
	    free(meta->text);
	    free(meta);
	}
	else if (meta->time > time_expired) break;
}

/*
 * This loop plays the music.
 */
static void
play_score()
{
    event_type event;
    /* Go to the next event indirectly using the_next_event variable,
     * so that some notes can be played twice, for reverberation
     * effect, without allocating a whole new note structure.
     */
    event_type the_next_event, the_last_event;
    struct voice_type *card_voice = NULL;

    /* Argument was passed indirectly from phase1(). */
    event = the_score;
    the_last_event = (event_type)NULL;

#ifdef MIDIIN
    /* Write track initialization info. */
    if (recording_track) {
	unsigned char data[4];
	(void) mf_write_tempo(0, Mf_currtempo);
	data[0] = recording_program;
	(void) mf_write_midi_event(getdelta(), MIDI_CH_PROGRAM, recording_channel - 1, data, 1);
    }
#endif

    /* Unless we resynchronize /dev/sequencer just before starting to
     * play, the first few notes sound hurried.
     */
    if (playing_music) sb_resync();

    /* The playing loop starts here. */
    while (event != NULL) {	/* play it, Sam */
	/* d is the channel, 1-16, to use for the external synth, or
	 * else 0 if the event is for a sound card.
	 */
	int d = event->ndest;
	/* card is either sb_dev or gus_dev, or -1 if event is not for
	 * either card.
	 */
	int card = event->ncard;
	/* the local midi channel, from 0-15 */
	int chan = vc_voice(event->nvoice);
	int delay = event->nline;
#ifdef MAINTAIN_PRECISION
	unsigned long new_time = round(event->ntime);
#else
	unsigned long new_time = event->ntime;
#endif

	/* where to go next: This may be changed in case of an echo note. */
	the_next_event = event->next;
	if (the_last_event != (event_type)NULL) free(the_last_event);
	the_last_event = event;

	/* If this channel is not marked as in use for the external synth,
	 * make sure the event can't be sent out the midi port.
	 */
if (d && !ext_chan[chan])
fprintf(stderr, "Sam: ext dest %d but chan %d not ext!\n", d, chan);
if (d && ext_dev < 0)
fprintf(stderr, "Sam: ext dest %d but no ext dev!\n", d);
	if (!ext_chan[chan] || ext_dev < 0) d = 0;

	/* When the event is for the external synth, point card to one past
	 * the index of the last real sound card, pro forma, for the sake
	 * of indexing some arrays.
	 */
if (d && card != ext_index)
fprintf(stderr, "Sam: ext dest %d but note headed for dev %d!\n", d, card);
	if (d) card = ext_index;

	if (card == sb_dev) card_voice = fm_voice;
	else if (card == gus_dev) card_voice = gus_voice;
	else card_voice = NULL;

#ifdef XMP
/* note that if TRY_VIBRATO is not defined, this has to be changed to generate
 * a write packet at ptime=0
 */
	send_meter_packet(false);
#endif

	while (ptime < new_time) {
	    ptime++;
	    propagate_bend(ptime);
	    note_offs(ptime);
	    if (!vib_switch) continue;
#ifdef TRY_VIBRATO
	    if (gus_dev >= 0) card_vibrate(gus_dev, ptime);
	    if (sb_dev >= 0) card_vibrate(sb_dev, ptime);
#endif
#ifdef XMP
	    send_meter_packet(false);
#endif
	}

#ifdef MIDIIN
	/* Record or purge midi input. */
	listen();
#endif
	show_markers();

	if (is_note(event)) {	/* play a note */
	    int prog = event->u.note.nprogram;
	    int pitch = event->u.note.npitch;
	    int vel = event->u.note.nloud;
	    int dur = event->u.note.ndur;
	    int effect = event->u.note.neffect;
	    int bank = event->u.note.nbank;
	    int reverb, reverb_adjust, tone_program;
	    int do_echo_flag = false;

	    /* Discard any event which has no destination at all. */
	    if (card < 0 && pitch != NO_PITCH) {
		if (really_verbose) printf("Sam: no card for note!\n");
		event = the_next_event;
		continue;
	    }
/**
#ifdef K1
if (card == ext_index) dur -= 1;
if (dur < 1) {
		event = the_next_event;
		continue;
}
#endif
**/
	    /* Determine reference to patch. */
	    if (PERCCHAN(chan)) tone_program = (pitch == NO_PITCH)? 0 : pitch + 128;
	    else tone_program = prog - 1;

	    /* The patches for bank > 0 are
	     * indexed in linked lists in the voice tables.
	     */
	    tone_program = find_tone_bank(card, tone_program, bank);
	    if (card_voice != NULL) bank = card_voice[tone_program].bank;

	    /* We cannot send to the external synth the second note generated
	     * when there is chorus depth, because it's simultaneous with and
	     * at the same pitch as the first note, so discard it.
	     */
	    if (d && (effect & CHORUS2_EFFECT)) {
		stat_chorus_dropped[card]++;
		event = the_next_event;
		continue;
	    }
	    /* Discard notes for which I don't have patches. */
	    if (pitch != NO_PITCH && card_voice != NULL &&
			card_voice[tone_program].volume < 0) {
		event = the_next_event;
		continue;
	    }
	    /* Discard gus notes for which there is no patch. */
	    if (pitch != NO_PITCH && card == gus_dev) {
		char *voice_name = NULL;
		voice_name = gus_voice[tone_program].vname;
		if (voice_name == NULL || voice_name[0] == '\0') {
			event = the_next_event;
			continue;
		}
	    }

#ifdef TRY_CHORUS
	    /* Also discard the second chorus note when we don't have enough cells. */
#ifdef USE_SHM
	    if (!d && (spare_cells[card] < 1 || !*shm_setting_chorus_spread))
#else
	    if (!d && (spare_cells[card] < 1 || !setting_chorus_spread))
#endif
#else
	    /* ... or always discard it */
	    if (!d)
#endif
	    {
		if (effect & CHORUS2_EFFECT) {
		    stat_chorus_dropped[card]++;
		    event = the_next_event;
		    continue;
	        }
		/* Don't bend pitch of first chorus note when the first one
		 * had to be discarded.
		 */
		effect &= ~CHORUS1_EFFECT; 
	    }

#ifdef USE_SHM
	    setting_reverb = *shm_setting_reverb;
#endif
	    reverb = get_reverberation(chan, pitch);

	    reverb_adjust = (reverb * setting_reverb) / 50;

#if 0
/*#ifdef K1 -- try this for all ext. synth ... why not? */
/* No effects on K1, so fake reverb. */
	    if (d && reverb > REVERB_THRESHHOLD && setting_reverb && !gm_voice[prog-1].flags)
		dur += 6 * (1 + (reverb*setting_reverb)/1000);
/*#endif*/
#endif

	    if (!(effect & ECHO_EFFECT)
	      && setting_reverb			/* when xmp setting requests it */
	      && reverb > REVERB_THRESHHOLD	/* or when there is little reverb */
	      && (!PERCCHAN(chan)||pitch > 32)	/* Reverb is relatively inaudible
						 * at low pitches, so why waste
						 * the polyphony? */
	      && pitch != NO_PITCH		/* Don't echo rests. */
	      && vel > 10) {			/* Reverb less audible at low vol */

		if (card == ext_index) {
#ifdef HELP_EXT_REVERB
		    if (gus_dev >= 0 && gus_voice[tone_program].loaded &&
			spare_cells[gus_dev] > 2) do_echo_flag = true;
		    else
#endif
		    stat_echoes_dropped[card]++;
		}
		else if (spare_cells[card] > 2) do_echo_flag = true;
		else stat_echoes_dropped[card]++;
	    }

	    /* Generate an extra note for reverberation effect. */
	    if (do_echo_flag) {
		event_type lag, lead;
		/* Values for current note are already in temporary
		 * variables, so we are free to modify the event without
		 * affecting anything this time around the play loop.
		 */
		int percussive_flag;
		int vel_adjust;
		int echo_delay;

		echo_delay = (reverb_adjust * card_voice[tone_program].echo_delay) >> 9;
		vel_adjust = (reverb_adjust * card_voice[tone_program].echo_atten) >> 9;
#ifdef HELP_EXT_REVERB
		if (card == ext_index) {
			event->ncard = gus_dev;
			event->ndest = 0;
		echo_delay = (reverb_adjust * gus_voice[tone_program].echo_delay) >> 9;
		vel_adjust = (reverb_adjust * gus_voice[tone_program].echo_atten) >> 9;
		}
#endif
		if (event->ncard == gus_dev) percussive_flag =
		    !(gus_voice[tone_program].modes & WAVE_SUSTAIN_ON);
		else percussive_flag = PERCCHAN(chan);

		/* Echo is softer, later, and longer.  Adjust amounts
		 * to taste.
		 */
		if (percussive_flag) {
		   vel_adjust += vel /16;
		   vel_adjust = vel / 4 - 2 * vel_adjust;
		   echo_delay /= 2;
		}
		else {
		   vel_adjust = vel / 3 - vel_adjust;
		}


		if (vel_adjust < 1) vel_adjust = 1;
		event->u.note.nloud = vel_adjust;

		if (echo_delay < 1) echo_delay = 1;
		if (echo_delay > MAX_DELAY - 2) {
			/*if (verbose) printf("long echo %d csec\n", echo_delay);*/
			echo_delay = MAX_DELAY - 2;
		}
#ifdef MAINTAIN_PRECISION
		event->ntime += (echo_delay<<4);
#else
		event->ntime += echo_delay;
#endif
		event->nline = echo_delay;

		event->u.note.ndur += echo_delay / 2;

		/* Flag it as an echo so it will be panned from a
		 * slightly different direction.
		 */
		event->u.note.neffect |= ECHO_EFFECT;
		/* Link the echo note back in further down the note list,
		 * so it will be played in the future.
		 */
		lag = event;
		lead = event->next;
		while (lead) {
		    if (event->ntime <= lead->ntime)
			break;
		    lag = lead;
		    lead = lead->next;
		}
		/* If it wasn't put after any future events, then it is
		 * the very next event.
		 */
		if (lag == event) {
		    the_next_event = event;
		} else {
		    event->next = lag->next;
		    lag->next = event;
		}
		/* don't free the memory */
		the_last_event = (event_type)NULL;
	    }


	    /* Change the pitch or duration, as specified in the fm patch. */
	    if (card == sb_dev && pitch != NO_PITCH) {
		    if (fm_voice[tone_program].fix_dur)
			dur = fm_voice[tone_program].fix_dur;
		    if (fm_voice[tone_program].fix_key)
			pitch = fm_voice[tone_program].fix_dur;
		    if (fm_voice[tone_program].trnsps)
			pitch += fm_voice[tone_program].trnsps - 64;
	    }
	    /* Change the pitch or duration, as specified in the gus patch. */
	    if (card == gus_dev && pitch != NO_PITCH) {
		if (PERCCHAN(chan)) {
		    if (gus_voice[tone_program].fix_dur) {
		        if (dur >= 8) dur -= 8;
		        else dur = 0;
			dur += gus_voice[tone_program].fix_dur;
		    }
		}
		else {
		    if (gus_voice[tone_program].fix_dur)
			dur = gus_voice[tone_program].fix_dur;
		    if (gus_voice[tone_program].trnsps)
			pitch += gus_voice[tone_program].trnsps - 64;
		}
	    }
	    /* If values are out of bounds, skip this note. */
	    if (pitch != NO_PITCH && (pitch < 0 || pitch > 127 || dur < 1)) {
		fprintf(stderr,
		   "Sam: bad pitch or duration: pitch %d, dur %d, chan %d, dev %d\n",
		   pitch, dur, chan+1, card);
		event = the_next_event;
		continue;
	    }

	    /* check for correct program (preset) */
	    if ((d || !PERCCHAN(chan)) && prog != program[chan]) {
		f_program(card, d, chan + 1, prog);
		program[chan] = prog;
	    }
	    /* if it is a note (not a rest) play it */
	    if (pitch != NO_PITCH) {
		int cell = 0;
		/* Request a voice ("cell") to be allocated, even for notes
		 * that go to the external synth, if we are doing voice
		 * allocation for it, too.
		 */
		cell = new_cell(card, ptime, d, chan, prog-1, tone_program,
			pitch, vel, effect, delay);
		/* Currently, we always get a cell from new_cell(), but this might
		 * change, in which case we will want to skip the note.
		 */
		if (cell == -1) {
		    event = the_next_event;
		    continue;
		}

		/* Start the note up. */
		f_note(card, d, chan + 1, cell, pitch, vel, ptime);
		/* Put it on the list to be turned off later. */
		off_schedule(card, d, ptime + dur, chan, cell, pitch, ptime);
#ifdef XVOICE
		/* If doubling the note is requested in the fm patch, do
		 * that now, polyphony permitting.
		 */
		if (card == sb_dev && spare_cells[sb_dev] > 1) {
		    int xprogram = fm_voice[tone_program].modes;
		    if (xprogram && fm_voice[xprogram - 1].loaded) {
			f_program(card, d, chan + 1, xprogram);
			cell = new_cell(card, ptime, d, chan, prog-1, 
				xprogram-1, pitch, vel, effect, delay);
			if (cell == -1) {
			    event = the_next_event;
			    continue;
			}
			f_note(card, d, chan + 1, cell, pitch, vel, ptime);
			off_schedule(card, d, ptime + dur, chan, cell, pitch, ptime);
			f_program(card, d, chan + 1, prog);
		    }
		}
#endif
	    }

	/* here, the event was not a note */
	} else {		/* send control info */
	    int val = event->u.ctrl.value;
	    int ctrl = event->u.ctrl.control;
	    int ctrl_type = vc_ctrl(event->nvoice);

	    send_control(false, card, d, chan, ctrl, ctrl_type, val);
	    if (gus_dev >= 0 && card != gus_dev)
		send_control(false, gus_dev, 0, chan, ctrl, ctrl_type, val);
	    if (sb_dev >= 0 && card != sb_dev)
		send_control(false, sb_dev, 0, chan, ctrl, ctrl_type, val);
	}

	event = the_next_event;
	if (got_signal)
		stop_notes(got_signal);
    }				/* play it, Sam */

    /* At end of the song, all notes off. */
    if (running_time) while (ptime <= running_time/* && note_offs(ptime)*/) {
	ptime++;
	note_offs(ptime);
	show_markers();
#ifdef XMP
	send_meter_packet(false);
#endif
    }
    note_offs(MAXTIME);

    if (the_last_event != (event_type)NULL) free(the_last_event);

    /* Finish up by flushing the output buffer, getting any last
     * midi input, waiting for all events in driver queue to be
     * processed, write end of track info to track file, and
     * finally allowing a little extra time for the last notes
     * played on the gus to fade away.
     */
    if (playing_music) {
	int room;
	sbflush();
#ifdef MIDIIN
	listen();
#endif
	while ((ioctl(seq_fd, SNDCTL_SEQ_GETOUTCOUNT, &room) != -1) &&
		(room < seq_max_queue)) {
#ifdef OUT_Q_USLEEP
	    usleep(OUT_Q_USLEEP);
#endif
#ifdef XMP
	    send_meter_packet(false);
#endif
#ifdef MIDIIN
	    listen();
	}
	if (recording_track)
	    (void) mf_write_meta_event(getdelta(), 0x2f/*end_of_track*/, NULL, 0);
#else
	}
#endif


#ifdef OUT_Q_USLEEP
	if (playing_music && running_time) while (time_expired <= running_time) {
	    usleep(OUT_Q_USLEEP);
#ifdef XMP
	    send_meter_packet(false);
#endif
	    time_sync();
	    show_markers();
	}
#endif
/**
	if (verbose) printf("time expired %dcs = %d:%02d vs run time %d\n",
		time_expired,
		time_expired/6000, (time_expired/100)%60, running_time );
**/
	if (ext_dev >= 0) {
		usleep(500000);
		(void)ioctl(seq_fd, SNDCTL_SEQ_RESET, 0);
	}
    }
}


/* noteoff.c -- this module keeps track of pending note offs for adagio */

/*****************************************************************************
*	    Change Log
*  Date	    | Change
*-----------+-----------------------------------------------------------------
* 31-Dec-85 | Created changelog
* 31-Dec-85 | Add c:\ to include directives
*  1-Jan-86 | Declare malloc char * for lint consistency
* 21-Jan-86 | note_offs can now turn off more than one note per call
*****************************************************************************/


/* off_type is a structure containing note-off information */

typedef struct off_struct {
    unsigned long when;
    int voice;
    int pitch;
    char dest;
    char card;
    int cell;
    unsigned long stime;
    struct off_struct *next;
} *off_type;

static off_type free_off = NULL;	/* free list of off_type structures */
static off_type off_events = NULL;	/* active list */

/****************************************************************************
*	Routines declared in this module
****************************************************************************/

static off_type off_alloc();
static void off_free();

/****************************************************************************
*				note_offs
* Inputs:
*	long time: the current time
* Outputs:
*	return true if off list has more notes
* Effect: turn off notes if it is time
* Assumes:
* Implementation:
*	Find scheduled note off events in off_events, compare with time
****************************************************************************/

static int note_offs(mtime)
unsigned long mtime;
{
    off_type temp;
    while (off_events != NULL && (ptime = off_events->when) <= mtime) {
	f_note(off_events->card, off_events->dest, (off_events->voice) + 1,
	       off_events->cell, off_events->pitch, 0, off_events->stime);
	temp = off_events;
	off_events = off_events->next;
	off_free(temp);
    }
    if (mtime < MAXTIME)
	ptime = mtime;
    return (off_events != NULL);
}

/****************************************************************************
*				off_alloc
* Outputs:
*	returns off_type: an allocated note off structure
* Effect:
*	allocates a structure using malloc
****************************************************************************/

static off_type off_alloc()
{
    return (off_type) malloc(sizeof(struct off_struct));
}

/****************************************************************************
*				off_free
* Inputs:
*	off_type off: a structure to deallocate
* Effect:
*	returns off to freelist
****************************************************************************/

static void off_free(off)
off_type off;
{
    off->next = free_off;
    free_off = off;
}

/****************************************************************************
*				off_init
* Effect: initialize this module
* Assumes:
*	only called once, otherwise storage is leaked
****************************************************************************/

static void off_init()
{
    int i;

    if (free_off != (off_type)NULL) return;

    for (i = 0; i < 50; i++)
	off_free(off_alloc());
}

/****************************************************************************
*				off_schedule
* Inputs:
*	long offtime: time to turn note off
*	int voice: the midi channel
*	int pitch: the pitch
* Effect:
*	schedules a note to be turned off
* Assumes:
*	note_offs will be called frequently to actually turn off notes
****************************************************************************/

static void off_schedule(card, dest, offtime, voice, cell, pitch, stime)
unsigned long offtime;
int card, dest, voice, cell, pitch;
unsigned long stime;
{
    off_type off, ptr, prv;
    /* allocate off */
    if ((off = free_off) == NULL) {
	off = off_alloc();
    } else
	free_off = off->next;

    if (off == NULL) {
	fprintf(stderr, "out of space for note off events");
	STOPNOW;
    }
    off->when = offtime;
    off->voice = voice;
    off->pitch = pitch;
    off->dest = dest;
    off->card = card;
    off->cell = cell;
    off->stime = stime;
    /* insert into list of off events */
    ptr = off_events;
    if (ptr == NULL || offtime <= ptr->when) {
	off->next = ptr;
	off_events = off;
    } else {
	while (ptr != NULL && offtime > ptr->when) {
	    prv = ptr;
	    ptr = ptr->next;
	}
	prv->next = off;
	off->next = ptr;
    }
/*
 *    printf("off_schedule(%ld, %d, %d): \n", offtime, voice, pitch);
 *    for (ptr = off_events; ptr != NULL; ptr = ptr->next) {
 *	printf("    %ld: %d, %d\n", ptr->when, ptr->voice, ptr->pitch);
 *    }
 */
}

/*
 * This is the same as a routine in the midifile library.  I don't
 * think it's necessary to duplicate it here any more.
 */
void AWriteVarLen(value)
register long value;
{
    register long buffer;

    buffer = value & 0x7f;
    while ((value >>= 7) > 0) {
	buffer <<= 8;
	buffer |= 0x80;
	buffer += (value & 0x7f);
    }

    while (true) {
	putchar(buffer);
	if (buffer & 0x80)
	    buffer >>= 8;
	else
	    break;
    }
}

/* Calculate time delta for -m option output. */
void deltatime()
{
    float csecs = (float) (ptime - lasttime);

    AWriteVarLen((long) (((csecs * 10.0) / 4.0 * 96) / 120));
    lasttime = ptime;
}


/****************************************************************************
*				   f_note
* Inputs:
*	int channel: midi channel on which to send data
*	int pitch: midi pitch code
*	int velocity: velocity with which to sound it (0=> release)
* Effect:
*	Prints a midi note-play request out
****************************************************************************/
/* (arguments have been added to Dannenburg's version) */
static void f_note(card, d, channel, cell, pitch, velocity, stime)
int card, d, channel, cell, pitch, velocity;
unsigned long stime;
{
    if (readable)
	printf("Time=%d  Note %s, chan=%d pitch=%d vol=%d\n",
	       ptime, velocity ? "on" : "off", channel, pitch, velocity);
    else if (playing_music)
	fm_noteon(card, d, channel - 1, cell, pitch, velocity, stime);
    else {
	deltatime();
	putchar(NOTEON + channel - 1);
	putchar(pitch);
	putchar(velocity);
    }

    if (user_scale) {
	/* check for correct pitch bend */
	if ((pit_tab[pitch].pbend != bend[MIDI_CHANNEL(channel)]) &&
	    (velocity != 0)) {
	    f_bend(card, d, channel, pit_tab[pitch].pbend);
	    bend[channel] = pit_tab[pitch].pbend;
	}
	pitch = pit_tab[pitch].ppitch;
    }
}

/****************************************************************************
*				   f_bend
* Inputs:
*	int channel: midi channel on which to send data
*	int value: pitch bend value
* Effect:
*	Prints a midi pitch bend message
****************************************************************************/

static void f_bend(card, d, channel, value)
int card, d, channel, value;
{

    if (readable)
	printf("Time=%d  Pitchbend, chan=%d value=%d\n",
	       ptime, channel, value);
    else if (playing_music) {
	if (d) {
	    midisync();
	    midicmdch(PITCHBEND + d - 1);
	    midich(value & 0x7f);
	    midich((value >> 7) & 0x7f);
	    return;
	}
	fm_bend(card, channel - 1, value - 8192);
    } else {
	deltatime();
	putchar(PITCHBEND + channel - 1);
	putchar(value & 0x7f);
	putchar((value >> 7) & 0x7f);
    }

    bend[MIDI_CHANNEL(channel)] = value;
}

/****************************************************************************
*				   f_ctrl
* Inputs:
*	int channel: midi channel on which to send data
*	int control: control number
*	int value: control value
* Effect:
*	Prints a midi control change message
****************************************************************************/

static void f_ctrl(card, d, channel, control, value)
int card, d, channel, control, value;
{
    if (readable)
	printf("Time=%d  Parameter, chan=%d ctrl=%d value=%d\n",
	       ptime, channel, control, value);
    else if (playing_music)
	fm_ctrl(card, d, channel - 1, control, value);
    else {
	deltatime();
	putchar(CONTROLLER + channel - 1);
	putchar(control);
	putchar(value);
    }
}

/****************************************************************************
*				 f_program
* Inputs:
*	int channel: Channel on which to send midi program change request
*	int program: Program number to send (decremented by 1 before
*			being sent as midi data)
* Effect:
*	Prints a program change request out the channel
****************************************************************************/

static void f_program(card, d, channel, program)
int card, d;			/* destination */
int channel;			/* midi channel */
int program;			/* the program number */
{
    if (readable)
	printf("Time=%d  Program, chan=%d program=%d\n",
	       ptime, channel, program);
    else if (playing_music) {
	fm_program(card, d, channel - 1, program - 1);
    }
    else {
	deltatime();
	putchar(PROGRAM + channel - 1);
	putchar(program - 1);
    }
}

/****************************************************************************
*				   f_touch
* Inputs:
*	int channel: midi channel on which to send data
*	int value: control value
* Effect:
*	Prints a midi after touch message
****************************************************************************/

static void f_touch(card, d, channel, value)
int card, d, channel, value;
{
    if (readable)
	printf("Time=%d  Channel pressure, chan=%d value=%d\n",
	       ptime, channel, value);
    else if (playing_music) {
	if (d) {
	    midisync();
	    midicmdch(CHANPRESSURE + d - 1);
	    midich(value);
if (value > 127) fprintf(stderr, "neg. pressure %d\n", value);
	} else
	    card_touch_all(card, channel - 1, value);
    } else {
	deltatime();
	putchar(CHANPRESSURE + channel - 1);
	putchar(value);
    }
}

#ifndef XMP
/*****************************************************************
*			set_pitch_default
*****************************************************************/
static void set_pitch_default()
{
    int i;

    for (i = 0; i < 128; i++) {
	pit_tab[i].pbend = 8192;
	pit_tab[i].ppitch = i;
    }
}

/*****************************************************************
*			read_tuning
*****************************************************************/

static void read_tuning(filename)
char *filename;
{
    int index, pit, lineno = 0;
    float bend;
    FILE *fpp;

    user_scale = true;
    set_pitch_default();
    fpp = fileopen(filename, "tun", "r", "Tuning definition file");
    while ((fscanf(fpp, "%d %d %f\n", &index, &pit, &bend) > 2) &&
	   (lineno < 128)) {
	lineno++;
	if (index >= -12 && index <= 115) {
	    pit_tab[index + 12].pbend = (int) (8192 * bend / 100 + 8192);
	    pit_tab[index + 12].ppitch = pit;
	}
    }
}
#endif

#ifndef XMP
/****************************************************************************
*				   tuninginit
* Effect:
* Read tuning file
****************************************************************************/

static void tuninginit()
{
    char *filename;

    filename = cl_option("-tune");
    if (filename != NULL) {
	read_tuning(filename);
    }
/*
    if (user_scale) {
        int i;
	for (i = 0; i < NUM_CHANS; i++) {
	    f_bend(0, i+1, 8192);
	    bend[i] = 8192;
	}
    }
*/
}
#endif




/* Here are the routines for low level output to the soundcards. */


/*
 * Some of following code was originally adapted from:
 * fmplay by Hannu Savolainen (hsavolai@cs.helsinki.fi)
 *
 */

#ifndef __386BSD__
#include <sys/ultrasound.h>
#else
#include <machine/ultrasound.h>
#endif

/* gus mode bits */
#define Bit8 0
#define Bit16 4
#define LoopOff 0
#define LoopOn 8
#define UniDir 0
#define BiDir 16
#define Forw 0
#define Backw 64
#define Up 0
#define Down 64

/* Current midi program, pitch bend, and channel pressure. */
static int chn_bend[MAXCARDS][NUM_CHANS][MAX_DELAY];
static int chn_press[MAXCARDS][NUM_CHANS];

/* records used in cell allocation (except the "cell_has" arrays
 * remember info already sent to the cards, to avoid duplication)
 */
static int cell_alloc[MAXCARDS][MAXCELLS];
static long cell_time[MAXCARDS][MAXCELLS];
static long cell_endtime[MAXCARDS][MAXCELLS];
static int cell_chan[MAXCARDS][MAXCELLS];
static int cell_prog[MAXCARDS][MAXCELLS];
static int cell_tone_prog[MAXCARDS][MAXCELLS];
static int cell_dest[MAXCARDS][MAXCELLS];
static int cell_has_vel[MAXCARDS][MAXCELLS];
static int cell_has_prog[MAXCARDS][MAXCELLS];
static int cell_has_bend[MAXCARDS][MAXCELLS];
static int cell_has_touch[MAXCARDS][MAXCELLS];
static int cell_pitch[MAXCARDS][MAXCELLS];
static int cell_vel[MAXCARDS][MAXCELLS];
static int cell_hist[MAXCARDS][MAXCELLS];
static int cell_pan[MAXCARDS][MAXCELLS];
static int cell_has_pan[MAXCARDS][MAXCELLS];
static int cell_effect[MAXCARDS][MAXCELLS];
static int cell_has_main_volume[MAXCARDS][MAXCELLS];
static int cell_has_expression[MAXCARDS][MAXCELLS];
#ifdef TRY_VIBRATO
static int cell_vibrato[MAXCARDS][MAXCELLS];
#endif
static int cell_delay[MAXCARDS][MAXCELLS];

#ifdef END_DIAGNOSTICS
void hist_report()
{
    int i, j;

    if (sb_dev >= 0) {
	printf("     =====================================================\n");
	j = card_info[sb_dev].nr_voices;

	printf("fm cells: |");
	for (i = 0; i < 9; i++) printf("%4d |", cell_hist[sb_dev][i]);
	printf("\n          |");
	for (i = 0; i < 9; i++) printf("[%3d]|", i);
	if (j > 9) {
	    printf("\n           -----------------------------------------------");
	    printf("\n          |");
	    for (i = 9; i < j; i++) printf("%4d |", cell_hist[sb_dev][i]);
	    printf("\n          |");
	    for (i = 9; i < j; i++) printf("[%3d]|", i);
	}
	printf("\n");
    }
    if (gus_dev >= 0) {
	printf("     =====================================================\n");
	j = card_info[gus_dev].nr_voices;

	printf("wv cells: |");
	for (i = 0; i < j / 3; i++) printf("%4d |", cell_hist[gus_dev][i]);
	printf("\n          |");
	for (i = 0; i < j / 3; i++) printf("[%3d]|", i);
	printf("\n           -----------------------------------------------");
	printf("\n          |");
	for (i = j / 3; i < j*2/3; i++) printf("%4d |", cell_hist[gus_dev][i]);
	printf("\n          |");
	for (i = j / 3; i < j*2/3; i++) printf("[%3d]|", i);
	printf("\n           -----------------------------------------------");
	printf("\n          |");
	for (i = j*2/3; i < j; i++) printf("%4d |", cell_hist[gus_dev][i]);
	printf("\n          |");
	for (i = j*2/3; i < j; i++) printf("[%3d]|", i);
	printf("\n");
    }
#ifdef XPOLYPHONY
    if (ext_dev >= 0) {
	printf("     =====================================================\n");
	j = card_info[ext_index].nr_voices;

	printf("xm cells: |");
	for (i = 0; i < j; i++) printf("%4d |", cell_hist[ext_index][i]);
	printf("\n          |");
	for (i = 0; i < j; i++) printf("[%3d]|", i);
	printf("\n");
    }
#endif
}
#endif

/*
 * Mark cells as not yet in use.
 */
void cells_init()
{
    int i, j;

    for (i = 0; i < MAXCARDS; i++)
	for (j = 0; j < MAXCELLS; j++) {
	    cell_alloc[i][j] = false;
	    cell_prog[i][j] = cell_has_prog[i][j] = cell_endtime[i][j] =
		cell_tone_prog[i][j] = cell_has_bend[i][j] = cell_has_pan[i][j] =
		cell_time[i][j] = cell_chan[i][j] = cell_pitch[i][j] = -1;
	    cell_has_touch[i][j] = 0;
	}
}

/*
 * Allocate a cell to play a note on.
 */
int new_cell(int card, unsigned long stime, int dest, int chan, int prog,
		int tone_program, int pitch, int vel, int effect, int delay)
{
    int i, best = 0, bperc = -1, oldest = 0, oldecho = -1;
#ifdef USE_SOLO_CUTOFF
    int same = -1;
#endif
    int o3_mode = 0;
    int pan = get_pan(chan, pitch);

    /* If it's a 4op fm voice, note this so we know to allocate one of the first
     * six cells.
     */
    if (card == sb_dev && fm_voice[tone_program].mem_req == OPL3_PATCH)
	o3_mode = 1;

    /* Look through all the cells for the most suitable. */
    for (i = 0; i < card_info[card].nr_voices; i++) {

	/* Stop here if past the last cell eligible for 4op patch. */
	if (o3_mode && i > 5) break;

#ifdef USE_SOLO_CUTOFF
	/* A cell that is playing the same non-polyphonic voice is good,
	 * because we will cut off the release of the last note when we
	 * play this next one.  (But an echo or chorus note should not cut
	 * off the note it is paired with, so the pitch should differ.  Also,
	 * if the instrument is moving horizontally, we'll consider it to
	 * be a different instrument, so pan position should be the same.)
	 */
	if (!PERCCHAN(chan) &&
	    cell_chan[card][i] == chan &&
	    cell_tone_prog[card][i] == tone_program &&
	    gm_voice[cell_prog[card][i]].flags && !no_solo &&
	    cell_pitch[card][i] != pitch &&
	    !(cell_effect[card][i] & ECHO_EFFECT) &&
	    !(effect & ECHO_EFFECT) &&
	    cell_pan[card][i] == ext_pan[chan])
	    same = i;
#endif

	/* In case a live note must be cut off, good candidates are
	 * a note that has been playing a long time or a drum note
	 * (and preferably an old drum note).
	 */
	if (cell_alloc[card][i]) {
	    if (cell_time[card][i] < cell_time[card][oldest]) {
		oldest = i;
		if (cell_effect[card][oldest] & ECHO_EFFECT) oldecho = i;
	    }
	    if (PERCCHAN(cell_chan[card][i])) {
		if (bperc < 0) bperc = i;
		else if (cell_time[card][i] < cell_time[card][bperc]) bperc = i;
	    }
	/* Of the dead notes, best is one that expired a long time ago,
	 * so that we get a better chance to hear the note releases.
	 */
	} else if (cell_endtime[card][i] < cell_endtime[card][best])
	    best = i;
    }

    /* When all cells are in use, the longest playing one is good ... */
    if (cell_alloc[card][best]) {
	best = oldest;
	/* and better is an old echo note. */
	if (oldecho >= 0) best = oldecho;
    }
    /* but since we have to cut off a note, a drum is still better (well,
     * this isn't obvious, but I think missing drum beats are easy to
     * overlook).
     */
    if (cell_alloc[card][best] && bperc >= 0) best = bperc;

#ifdef USE_SOLO_CUTOFF
    /* But a note on the same non-polyphonic instrument ought to be cut
     * off (if it is no longer playing, its release ought to be cut off),
     * so this is a still better choice.
     */
    if (same >= 0) best = same;
#endif

    /* If the cell we are going to use is still playing a note,
     * turn it off now.
     */
    if (cell_alloc[card][best]) {
#ifdef USE_SOLO_CUTOFF
	if (best != same)
#endif
	stat_imp_cell_off[card]++;

#ifndef XPOLYPHONY
	if (card != ext_index)
#endif
	fm_noteon(card, cell_dest[card][best], cell_chan[card][best], best,
		cell_pitch[card][best], 0,
		(unsigned long) cell_time[card][best]);
#ifdef CUTOFF_DEBUG
if (card == ext_index)
printf("xm %d cut off: pitch = %d, stime = %d\n",
best, cell_pitch[card][best], cell_time[card][best]);
#endif
	/* if we just cut off a chorus note, cut off its twin also */
	if ( cell_effect[card][best] & (CHORUS1_EFFECT | CHORUS2_EFFECT) )
	    for (i = 0; i < card_info[card].nr_voices; i++)
	    if (i != best &&
		cell_alloc[card][i] &&
		(cell_effect[card][i] & (CHORUS1_EFFECT | CHORUS2_EFFECT)) &&
	(cell_effect[card][i]&ECHO_EFFECT)==(cell_effect[card][best]&ECHO_EFFECT) &&
		cell_chan[card][i] == cell_chan[card][best] &&
		cell_pitch[card][i] == cell_pitch[card][best])
	    fm_noteon(card, cell_dest[card][i], cell_chan[card][i], i,
		cell_pitch[card][i], 0,
		(unsigned long) cell_time[card][i]);
    }

    /* Do the bookkeeping for the cell that has been allocated. */
    cell_time[card][best] = (long) stime;
    cell_endtime[card][best] = MAXTIME;
    cell_chan[card][best] = chan;
    cell_dest[card][best] = dest;
    if (PERCCHAN(chan)) {
	    cell_prog[card][best] = pitch + 128;
    } else
	cell_prog[card][best] = prog;
    cell_tone_prog[card][best] = tone_program;
    cell_pitch[card][best] = pitch;
    cell_vel[card][best] = vel;
    cell_pan[card][best] = pan;
    cell_effect[card][best] = effect;
    cell_alloc[card][best] = true;
    spare_cells[card]--;
#ifdef TRY_VIBRATO
    cell_vibrato[card][best] = 0;
#endif
    if (!(effect&ECHO_EFFECT)) delay = 0;
    else if (!delay) {
	fprintf(stderr,"new_cell: echo no delay?\n");
    }

    cell_delay[card][best] = delay;
    cell_hist[card][best]++;

    return (best);
}

/*
 * Ask for 14-32 gus voices.
 */
void gus_max_voices(int num)
{
    buf[0] = SEQ_PRIVATE;
    buf[1] = gus_dev;
    buf[2] = _GUS_NUMVOICES;
    buf[3] = 0;
    *(unsigned short *) &buf[4] = num;
    buf[6] = buf[7] = 0;
    sqwrite(buf);
}

/*
 * Send pitch bend request to card.
 */
void card_bend(int card, int chan, int cell)
{
    int amount = chn_bend[card][chan][(ptime-cell_delay[card][cell]) % MAX_DELAY];
    int cents;
    int depth;
    int pitch = cell_pitch[card][chan];
    int chorus_spread = 0;

#ifdef TRY_VIBRATO
    amount += cell_vibrato[card][cell];
#endif
    depth = get_chorus_depth(chan, pitch);

    if (card == gus_dev)
	chorus_spread = gus_voice[cell_tone_prog[card][cell]].chorus_spread;
    else if (card == sb_dev)
	chorus_spread = fm_voice[cell_tone_prog[card][cell]].chorus_spread;
    chorus_spread -= 64;
    if (chorus_spread < 0) chorus_spread = 0;

#ifdef USE_SHM
    cents = (*shm_setting_chorus_spread * depth * chorus_spread) / 64;
#else
    cents = (setting_chorus_spread * depth * chorus_spread) / 64;
#endif

    if (cell_effect[card][cell] & CHORUS1_EFFECT)
	amount += cents;
    else if (cell_effect[card][cell] & CHORUS2_EFFECT)
	amount -= cents;
    else if (PERCCHAN(chan)) return;

    if (cell_has_bend[card][cell] == amount) return;
    cell_has_bend[card][cell] = amount;

    midisync();

    buf[0] = SEQ_EXTENDED;
    buf[1] = SEQ_CONTROLLER;
    buf[2] = card;
    buf[3] = cell;
    buf[4] = CTRL_PITCH_BENDER;
    *(short *) &buf[5] = amount;
    buf[7] = 0;
    sqwrite(buf);
}

/*
 * Send expression request to card.
 */
void card_expression(int card, int chan, int cell)
{
    int amount = expression[chan];
    int percussive_flag = PERCCHAN(chan);


#ifdef NO_NEW_GUS_VOL
    amount *= 3;
#endif
    if (card == gus_dev) percussive_flag =
	!(gus_voice[ cell_tone_prog[card][cell] ].modes & WAVE_SUSTAIN_ON);

    if (cell_effect[card][cell] & ECHO_EFFECT) {
	int reverb, reverb_adjust;
	reverb = get_reverberation(chan, cell_pitch[card][cell]);
	reverb_adjust = (reverb * setting_reverb) / 500;
	amount /= 2;
	amount -= reverb_adjust;
	if (amount < 1) amount = 1;
	if (percussive_flag) amount /= 16;
    }

    if (cell_has_expression[card][cell] == amount) return;
    cell_has_expression[card][cell] = amount;

    midisync();

    buf[0] = SEQ_EXTENDED;
    buf[1] = SEQ_CONTROLLER;
    buf[2] = card;
    buf[3] = cell;
    buf[4] = CTRL_EXPRESSION;
    *(short *) &buf[5] = amount;
    buf[7] = 0;
    sqwrite(buf);
}
/*
 * Send main volume request to card.
 */
void card_main_volume(int card, int chan, int cell)
{
    int amount = main_volume[chan];

#ifdef NO_NEW_GUS_VOL
	amount = 256;
#endif
    if (cell_has_main_volume[card][cell] == amount) return;
    cell_has_main_volume[card][cell] = amount;

    midisync();

    buf[0] = SEQ_EXTENDED;
    buf[1] = SEQ_CONTROLLER;
    buf[2] = card;
    buf[3] = cell;
    buf[4] = CTRL_MAIN_VOLUME;
    *(short *) &buf[5] = amount;
    buf[7] = 0;
    sqwrite(buf);
}

static void propagate_bend(unsigned long ctime)
{
	int chan, card;
	for (card = 0; card < MAXCARDS; card++)
	for (chan = 0; chan < NUM_CHANS; chan++)
	chn_bend[card][chan][ctime % MAX_DELAY] =
	chn_bend[card][chan][(ctime-1) % MAX_DELAY];
}

#ifdef TRY_VIBRATO
#define FORTE_VIB_MUL 100
#define FORTE_VIB_DIV 2

#include "vibrato.h"
/*
 * Bend pitch for pitch vibrato.
 */
void card_vibrate(int card, unsigned long ctime)
{
    int prog, tone_prog, chan, i, vs, period, depth;
    int sweep;

#ifdef USE_SHM
    setting_vibrato_depth = *shm_setting_vibrato_depth;
    setting_vibrato_speed = *shm_setting_vibrato_speed;
    setting_vibrato_sweep = *shm_setting_vibrato_sweep;
#endif

    sweep = setting_vibrato_sweep;

    /* Check each cell to see if it's playing a note ... */
    for (i = 0; i < card_info[card].nr_voices; i++)
	if ( (i&1) == (ctime&1) && cell_alloc[card][i]) {

	    /* No vibrato on echoes. */
	    /*if (cell_effect[card][i]&ECHO_EFFECT) continue;*/
	    chan = cell_chan[card][i];

	    /* No vibrato drums (vibraslap?). */
	    if (PERCCHAN(chan)) continue;

	    /* Set period and depth ad hoc for each instrument. */
	    prog = cell_prog[card][i];
	    if (prog > 127) continue;
	    tone_prog = cell_tone_prog[card][i];

	    if (card == gus_dev) period =
		gus_voice[tone_prog].vibrato_sweep / FORTE_VIB_DIV;
	    else period = vibrato[prog].period;

	    if (vibrato_rate[chan] >= 0)
		period -= ((vibrato_rate[chan] - 64)*period)/64;

	    period = ( (100 - setting_vibrato_speed) * period) / 50;

	    if (!period) continue;
	    if (period < 4) period = 4;

	    /* How far into this note are we? */
	    vs = ctime - cell_time[card][i];

	    /* Introduce a little variation into starting time of
	     * vibrato for choral effect.
	     */
	    vs -= (chan%8) + (cell_pitch[card][i]%6) + vibrato[prog].delay;

	    /* Time to start the vibrato? */
	    if (vs < 1) continue;

	    if (sweep) period += ( (vs / period + 1) % sweep);

	    /* How far into current period? */
	    vs = (vs % period);

	    /* Change slope of triangle wave modulation at 90 and 270 degrees. */
	    if (vs > period / 4) {
	        if (vs < (3 * period) / 4)
	    	vs = period / 2 - vs;
	        else
	    	vs = vs - period;
	    }

	    if (card == gus_dev) depth = FORTE_VIB_MUL * gus_voice[tone_prog].vibrato_depth;
	    else depth = vibrato[prog].depth;

	    depth += modulation_wheel[chan] * 16;

	    if (vibrato_depth[chan] >= 0)
		depth += ((vibrato_depth[chan] - 64)*depth)/64;

	    depth = (setting_vibrato_depth * depth) / 50;

	    if (depth < 1) continue;

	    /* Height of triangle. */
	    vs = (vs * depth) / (period / 4);

	    if (!(cell_effect[card][i] & CHORUS2_EFFECT)) vs = -vs;

	    if (card == gus_dev) vs -= depth / 2;

	    /* Make record for offsetting other bend events. */
	    cell_vibrato[card][i] = vs;

	    /* Do it. */
	    card_bend(card, chan, i);
    }
}
#endif

/*
 * Dynamic change of pitch bend.  Bend all card cells playing notes.
 */
void fm_bend(int card, int chan, int value)
{
    int i;

    if (card == sb_dev) {
	chn_bend[sb_dev][chan][ptime % MAX_DELAY] = value;
	for (i = 0; i < card_info[sb_dev].nr_voices; i++)
	    if (cell_alloc[sb_dev][i] && cell_chan[sb_dev][i] == chan)
		card_bend(sb_dev, chan, i);
    }
    else if (card == gus_dev) {
#ifdef USE_SHM
	chn_bend[gus_dev][chan][ptime % MAX_DELAY] = value + *shm_setting_gus_tuning;
#else
	chn_bend[gus_dev][chan][ptime % MAX_DELAY] = value + setting_gus_tuning;
#endif
	for (i = 0; i < card_info[gus_dev].nr_voices; i++)
	    if (cell_alloc[gus_dev][i] && cell_chan[gus_dev][i] == chan)
		card_bend(gus_dev, chan, i);
    }
}

/*
 * Send channel aftertouch to card.
 */
void card_touch(int card, int chan, int cell)
{
    int touch = (0xe0&chn_press[card][chan]);

    if (cell_has_touch[card][cell] == touch) return;
    cell_has_touch[card][cell] = touch;

    buf[0] = SEQ_EXTENDED;
    buf[1] = SEQ_AFTERTOUCH;
    buf[2] = card;
    buf[3] = cell;
    buf[4] = chn_press[card][chan];
    buf[5] = buf[6] = buf[7] = 0;
    sqwrite(buf);
}

/*
 * Dynamic change of aftertouch.  Touch all card cells playing notes.
 */
void card_touch_all(int card, int chan, int value)
{
    int i;

    midisync();
    if (sb_dev >= 0) {
	chn_press[sb_dev][chan] = value;
	for (i = 0; i < card_info[sb_dev].nr_voices; i++)
	    if (cell_alloc[sb_dev][i] && cell_chan[sb_dev][i] == chan)
		card_touch(sb_dev, chan, i);
    }
    if (gus_dev >= 0) {
	chn_press[gus_dev][chan] = value;
	for (i = 0; i < card_info[gus_dev].nr_voices; i++)
	    if (cell_alloc[gus_dev][i] && cell_chan[gus_dev][i] == chan)
		card_touch(gus_dev, chan, i);
    }
}

/*
 * Dynamic volume change to fm card.  (Needs work -- too loud.)
 */
void sb_vol(int chan, int prog, int cell, int vol)
{
    /* vol = (main_volume[chan] * expression[chan] * vol) >> 14; */
    if (vol > 127) vol = 127;
    /* if (vol == cell_has_vel[sb_dev][cell]) return; */
    cell_has_vel[sb_dev][cell] = vol;
/**
    buf[0] = SEQ_EXTENDED;
    buf[1] = SEQ_NOTEON;
    buf[2] = sb_dev;
    buf[3] = cell;
    buf[4] = 255;
    buf[5] = vol;
    buf[6] = buf[7] = 0;
    sqwrite(buf);
**/
}

#ifdef USE_OWN_GUS_VOL

/*
 * Calculate gus volume from note velocity, main volume, expression,
 * and intrinsic patch volume given in patch library.  Expression is
 * multiplied in, so it emphasizes differences in note velocity,
 * while main volume is added in -- I don't know whether this is right,
 * but it seems reasonable to me.  (In the previous stage, main volume
 * controller messages were changed to expression controller messages,
 * if they were found to be used for dynamic volume adjustments, so here,
 * main volume can be assumed to be constant throughout a song.)
 *
 * Intrinsic patch volume is added in, but if over 64 is also multiplied
 * in, so we can give a big boost to very weak voices like nylon
 * guitar and the basses.
 */
unsigned short
gvol(int vel, int mainv, int xpn, int voicev)
{
	int i, m, n, x;

mainv = (mainv - 16) * 2;
	/*if (xpn <= 0 || vel == 0) return(11 << 8);*/
	xpn += 8;

	/* A voice volume of 64 is considered neutral, so adjust
	 * the main volume if something other than this neutral
	 * value was assigned in the patch library.
	 */
	x = mainv + 6*(voicev - 64);
	/*if (x < 0) x = 0;*/

	/* Boost expression by voice volume above neutral. */
	/* if (voicev > 65) xpn += (voicev-64)/2; */
	if (voicev > 65) xpn += voicev-64;
	xpn += (voicev-64)/2;

	/* Combine multiplicative and level components. */
	x = vel*xpn*2 + (voicev/4)*x;

	/* Further adjustment by installation-specific master
	 * volume control (default 100).
	 */
#ifdef USE_SHM
	setting_gus_volume = *shm_setting_gus_volume;
#endif
	x = (x*setting_gus_volume*setting_gus_volume)/10000;

	if (x < 1) return(10 << 8);
	else if (x > 65535) {
		x = 65535;
		gus_clip_statistic++;
	}

	/* Convert to gus's logarithmic form with 4 bit exponent i
	 * and 8 bit mantissa m.
	 */
	n = x;
	i = 7;
	if (n < 128) {
		while (i > 0 && n < (1<<i)) i--;
	}
	else while (n > 255) {
		n >>= 1;
		i++;
	}
	/* Mantissa is part of linear volume not expressed in
	 * exponent.  (This is not quite like real logs -- I wonder
	 * if it's right.)
	 */
	m = x - (1<<i);

	/* Adjust mantissa to 8 bits. */
	if (m > 0) {
		if (i > 8) m >>= i-8;
		else if (i < 8) m <<= 8-i;
	}

	if (i < 11) return(11 << 8);
	return((i << 8) + m);
}


/*
 * Send volume to gus.
 */
void gus_vol(int chan, int prog, int cell, int vol, int flag)
{
    int top_vol = gvol(vol, main_volume[chan],
	expression[chan], gus_voice[prog].volume);
    if (top_vol > 4095) {
	gus_clip_statistic++;
    }
    buf[0] = SEQ_PRIVATE;
    buf[1] = gus_dev;
    buf[2] = _GUS_VOICEVOL2;
    buf[3] = cell;
    *(unsigned short *) &buf[4] = top_vol;
    buf[6] = buf[7] = 0;
    sqwrite(buf);

    if (!flag) return;
    buf[0] = SEQ_PRIVATE;
    buf[1] = gus_dev;
    buf[2] = _GUS_VOICEVOL;
    buf[3] = cell;
    *(unsigned short *) &buf[4] = top_vol;
    buf[6] = buf[7] = 0;
    sqwrite(buf);
}

#endif

/*
 * Calculate and send pan to gus.  Reduce pan controller range 0-127
 * to gus range 0-15.  Add pseudo-stereo effects.
 */
void gus_pan(int cell, int pan)
{
    int disturb = 0;
    int reverb;

    reverb = get_reverberation(cell_chan[gus_dev][cell], cell_pitch[gus_dev][cell]);

    if (pan < 0) pan = 64;
    /* Remember requested value (not what is actually sent to gus,
     * so new_cell() can permit note releases to sound when instrument
     * is changing locations.
     */
    cell_pan[gus_dev][cell] = pan;
    /* Reduce range: 64, center, must come out to 7, center. */
    pan = ((pan + 1) >> 3) - 1;
    if (pan < 0)
	pan = 0;
    if (pan > 15) {
	fprintf(stderr,"warning: pan out of range\n");
	return;
    }

#ifdef PSEUDO_STEREO
    if (cell_effect[gus_dev][cell] & CHORUS1_EFFECT) {
	pan -= 4;
	if (pan < 0) pan = 0;
    }
    else if (cell_effect[gus_dev][cell] & CHORUS2_EFFECT) {
	pan += 4;
	if (pan > 15) pan = 15;
    }
#ifdef USE_SHM
    setting_pstereo = *shm_setting_pstereo;
#endif

    /* Echoes come from a different direction. */
    if (cell_effect[gus_dev][cell] & ECHO_EFFECT)
	disturb = (setting_reverb * reverb) / 600;
    /* If they're close up (no reverb) and you are behind the pianist,
     * high notes come from the right, so we'll spread piano etc. notes
     * out horizontally according to their pitches.
     */
    else if (cell_prog[gus_dev][cell] < 21 &&
	reverb <= REVERB_THRESHHOLD) {
	    int n = (cell_pitch[gus_dev][cell] - 32)/8;
	    if (n < 0) n = 0;
	    if (n > 8) n = 8;
	    if (setting_pstereo) pan = pan/2 + n;
	}
    /* For other types of instruments, the music sounds more alive if
     * notes come from slightly different directions.  However, instruments
     * do drift around in a sometimes disconcerting way, so the following
     * might not be such a good idea.
     */
    else if (setting_pstereo) disturb = (cell_vel[gus_dev][cell]/32 % 2) +
	(cell_pitch[gus_dev][cell] % 2); /* /16? */

    if (pan < 7) pan += disturb;
    else pan -= disturb;
#endif
    if (pan < 0) pan = 0;
    else if (pan > 15) pan = 15;

    /* Don't send redundant pans. */
    if (cell_has_pan[gus_dev][cell] == pan) return;
    cell_has_pan[gus_dev][cell] = pan;

    buf[0] = SEQ_PRIVATE;
    buf[1] = gus_dev;
    buf[2] = _GUS_VOICEBALA;
    buf[3] = cell;
    *(unsigned short *) &buf[4] = pan;
    *(unsigned short *) &buf[6] = 0;
    sqwrite(buf);
}

/*
 * Start or stop a note -- all devices. (vol=0 stops the note.)
 */
void fm_noteon(int card, int d, int chan, int cell, int pitch, int vol, unsigned long stime)
{
    int nominal_pitch;

    /* In case the program is not changed, it will be the one requested. */
    int prog = cell_prog[card][cell];
    int tone_program = cell_tone_prog[card][cell];
    int pitch_adjust = 0;

#ifdef XMP
    int col = chan;
    if (setting_meter_column == 1) {
	if (PERCCHAN(chan)) col = 12 + (pitch % 4);
	else col = pitch % 12;
    }
    else if (setting_meter_column == 2) {
	if (PERCCHAN(chan)) col = 14;
	else col = (prog%128) / 8;
    }
    if (PERCCHAN(chan) && pitch < NUM_DRUMS) {
	if (chan == 9) {
		inst_drum_9_pan = drum_pan[pitch];
		inst_drum_9_reverb = drum_reverberation[pitch];
		inst_drum_9_cdepth = drum_chorus_depth[pitch];
	}
	else if (chan == 15) {
		inst_drum_15_pan = drum_pan[pitch];
		inst_drum_15_reverb = drum_reverberation[pitch];
		inst_drum_15_cdepth = drum_chorus_depth[pitch];
	}
    }
#endif

    /* time delta to /dev/sequencer */
    if (vol) midisync();

#ifdef XMP
    /* for xmp meter */
    if (vol) {
	curr_note_count[col]++;
	curr_note_velocity[col] += vol;
	inst_note_count[col]++;
	inst_note_velocity[col] += vol;
    }
#endif
    /* note goes to external synth? */
    if (card == ext_index && vol) {
	/* bookkeeping */
	stat_note_on[ext_index]++;
	spare_cells[ext_index]--;
	midicmdch(NOTEON + d - 1);
	midich(pitch);
	midich(vol);
#ifdef K1
if (d < 1 || d > 8) fprintf(stderr, "ext note on bad channel\n");
#endif
if (pitch > 127) fprintf(stderr, "neg. pitch %d\n", pitch);
if (vol > 127) fprintf(stderr, "neg. vol %d\n", vol);
	return;
    }

    /* Adjust velocity of an fm note according to current main volume
     * and expression. (Dynamic volume change doesn't seem to be working.)
     */

    if (card == sb_dev && vol) {
	vol += (expression[chan] - 64) / 16;
	if (vol < 8) vol = 8;
	else if (vol > 127) vol = 127;
    }


    /* We are about to adjust the pitch and program, but for bookkeeping purposes,
     * remember the requested pitch.
     */
    nominal_pitch = pitch;

    if (card == gus_dev || card == sb_dev) {
	int depth = chorus_depth[chan];

	if (PERCCHAN(chan) && pitch < NUM_DRUMS &&
	    drum_chorus_depth[pitch] >= 0) depth = drum_chorus_depth[pitch];

	if (card == gus_dev) pitch_adjust = gus_voice[tone_program].chorus_spread;
	else pitch_adjust = fm_voice[tone_program].chorus_spread;

	if (pitch_adjust >= 64 || !depth) pitch_adjust = 0;
	else {
	    pitch_adjust = (127 / (64 - pitch_adjust)) + 1;
	    pitch_adjust = (depth / pitch_adjust) + 1;
	}
    }
/**
if (!PERCCHAN(chan) && pitch_adjust) {
fprintf(stderr,"vol=%d, adj=%d, pgm=%d, chorus_spread=%d, chan=%d cell=%d cell_chan=%d\n",
	vol, pitch_adjust, tone_program,
	gus_voice[tone_program].chorus_spread, chan+1, cell,cell_chan[card][cell]+1);
pitch_adjust = 0;
}
**/

    /* Special treatment of percussion notes for fm: */
    if (card == sb_dev && PERCCHAN(chan)) {
	/* Requested pitch is really program, and pitch for device is whatever
	 * was specified in the patch library.
	 */
	if (fm_voice[tone_program].loaded) {
	    pitch = fm_voice[tone_program].fix_key;
	} else
	    return;

    /* Special treatment of percussion notes for gus: */
    } else if (card == gus_dev && PERCCHAN(chan)) {
	/* as for fm, except for details */
	if (gus_voice[tone_program].loaded) {
	    pitch = gus_voice[tone_program].fix_key;
	} else
	    return;
    }

    if (pitch_adjust && (card == gus_dev || card == sb_dev)) {
	int effect = cell_effect[card][cell];
	if (effect & CHORUS1_EFFECT) {
	    pitch += pitch_adjust/2;
	    effect &= ~CHORUS1_EFFECT;
	}
	if (effect & CHORUS2_EFFECT) {
	    pitch -= (pitch_adjust+1)/2;
	    effect &= ~CHORUS1_EFFECT;
	}
	cell_effect[card][cell] = effect;
    }

    /* Stop the note? */
    if (!vol) {
	/* Try to detect case in which note was prematurely terminated
	 * by a call from new_cell(), since we don't want to turn it
	 * off twice (the cell will be in use for playing some other
	 * note, so this would be very undesirable).
	 */
#ifndef XPOLYPHONY
	if (card != ext_index)
#endif
	if (!cell_alloc[card][cell]
	    || cell_time[card][cell] != (long) stime
	    || cell_chan[card][cell] != chan
	    || cell_pitch[card][cell] != nominal_pitch) {
#ifdef CUTOFF_DEBUG
if (card == ext_index)
printf("xm cell %d not offed: ptime %d = %d, chan %d = %d, pitch %d = %d\n",
cell, cell_time[card][cell], stime, cell_chan[card][cell], chan,
cell_pitch[card][cell], nominal_pitch);
#endif
	    return;
	}

#ifdef XMP
	if (inst_note_count[col]) {
	    inst_note_count[col]--;
	    if (inst_note_velocity[col] > cell_vel[card][cell])
		inst_note_velocity[col] -= cell_vel[card][cell];
	    else inst_note_velocity[col] = 0;
	}
#endif

	fm_noteoff(card, d, chan, prog, cell, pitch, 0, stime);
	return;
    }

    /* The remainder of this routine deals with the cells on the cards,
     * which we can actually control.  If this is a note for the
     * external synth, all we can to is turn it on or off, so we're done.
     */
    if (card == ext_index) return;

#define STATIC_STEREO
    /* Set the program, if different from the one requested. */
    if (cell_has_prog[card][cell] != tone_program) {
	cell_has_prog[card][cell] = tone_program;
	buf[0] = SEQ_EXTENDED;
	buf[1] = SEQ_PGMCHANGE;
	buf[2] = card;
	buf[3] = cell;
	if (card == gus_dev) buf[4] = gus_voice[tone_program].prog;
	else if (card == sb_dev) buf[4] = fm_voice[tone_program].prog;
	else buf[4] = prog;
#ifdef STATIC_STEREO
	if (card == sb_dev && (cell_effect[card][cell]&ECHO_EFFECT)
		&& tone_program < 256
		&& fm_voice[tone_program+256].loaded)
	    buf[4] = fm_voice[tone_program+256].prog;
#endif
	buf[5] = buf[6] = buf[7] = 0;
	sqwrite(buf);
    }

    /* Do any pitch bending. */
    card_bend(card, chan, cell);

    /* Do any aftertouch. */
    card_touch(card, chan, cell);

    /* bookkeeping */
    stat_note_on[card]++;

    if (card == gus_dev) {
#ifdef USE_OWN_GUS_VOL
	gus_vol(chan, prog, cell, vol, 0);
#else
	card_expression(card, chan, cell);
	card_main_volume(card, chan, cell);
#endif
    }
    else if (card == sb_dev)
	sb_vol(chan, prog, cell, (expression[chan]*vol)/128);

    /* request note on to driver */
    buf[0] = SEQ_EXTENDED;
    buf[1] = SEQ_NOTEON;
    buf[2] = card;
    buf[3] = cell;
    buf[4] = pitch;
#ifdef USE_OWN_GUS_VOL
    if (card == gus_dev)
	buf[5] = 255;
    else
#else
    buf[5] = vol;
#endif
    buf[6] = buf[7] = 0;
    sqwrite(buf);
    cell_has_vel[card][cell] = vol;

    /* Set direction: 64 for center, if there were no pan controller
     * messages.
     */
/**
    if (ext_pan[chan] >= 0) gus_pan(cell, ext_pan[chan]);
    else gus_pan(cell, 64);
**/
    if (card == gus_dev) gus_pan(cell, cell_pan[card][cell]);
}

/*
 * Turn off a note -- all devices.  (Actually, for the external synth it's already
 * been turned off, and we're just doing bookkeeping here.)
 */
void fm_noteoff(int card, int d, int chan, int prog, int cell, int pitch,
		int vol, unsigned long stime)
{
    midisync();
    stat_note_off[card]++;
    cell_has_vel[card][cell] = vol;

    if (card == sb_dev || card == gus_dev) {
	buf[0] = SEQ_EXTENDED;
	buf[1] = SEQ_NOTEOFF;
	buf[2] = card;
	buf[3] = cell;
	buf[4] = pitch;
	buf[5] = vol;
	buf[6] = buf[7] = 0;
	sqwrite(buf);
    }
    else if (card == ext_index) {
	midicmdch(NOTEON + d - 1);
	midich(pitch);
	midich(0);
/** trying to prevent hanging notes: 
	sbflush();
	midicmdch(NOTEON + d - 1);
	midich(pitch);
	midich(0);
 **/
    }
    cell_alloc[card][cell] = false;
    spare_cells[card]++;
    /**cell_endtime[card][cell] = (long) stime;**/
    cell_endtime[card][cell] = (long) ptime;
}

/*
 * Send a controller message -- all devices.
 */
void fm_ctrl(int card, int d, int chan, int control, int value)
{
    int i;

    midisync();

    if (card == ext_index) {
	if (d < 1) {
		fprintf(stderr, "fm_ctrl: bad ext chan\n");
		STOPNOW;
	}
#ifdef K1
/* no expression controller for K1, so use volume */
	if (control == EXPRESSION) {
	    control = VOLUME;
	    /* much below 64 is inaudible */
	    value = 64 + value/2;
	}
	else if (control == VOLUME) return;
#endif
	midicmdch(CONTROLLER + d - 1);
	midich(control);
	midich(value);
	return;
    }
    if (control == EXPRESSION)
	for (i = 0; i < card_info[card].nr_voices; i++)
	    if (cell_alloc[card][i] && cell_chan[card][i] == chan) {
		if (card == sb_dev)
		    sb_vol(chan, cell_prog[card][i], i, cell_vel[card][i]);
		else if (card == gus_dev)
#ifdef USE_OWN_GUS_VOL
		    gus_vol(chan, cell_prog[card][i], i, cell_vel[card][i], 1);
#else
#ifdef NO_DYNAMIC_EXPRESSION
		    { /* nothing */ }
#else
		    card_expression(card, chan, i);
#endif
#endif
	    }

    if (card != gus_dev) return;

    if (control == PAN)
	for (i = 0; i < card_info[card].nr_voices; i++)
	    if (cell_alloc[card][i] && cell_chan[card][i] == chan)
		gus_pan(i, value);
}

/*
 * Send change program to (non-K1) external synth.
 */
void fm_program(int card, int d, int chan, int program)
{
#ifndef K1
/* K1 can't remember channel programs */
    if (d) {
	midicmdch(PROGRAM + d);
	midich(program);
	return;
    }
#endif
}

/*#define MAX_ALLOW_SYNC 5*/
/*
 * Send sync time to /dev/sequencer -- centiseconds
 * since last time sync.
 */
void midisync(void)
{
    unsigned long jiffies;

    static unsigned long prevtime = 0;

    if (ptime == 0) prevtime = 0;
    jiffies = ptime;
    if (jiffies & 0xff000000) {
	fprintf(stderr, "error: excessively long sync time\n");
	STOPNOW;
    }
    if (jiffies > prevtime) {
#ifdef MAX_ALLOW_SYNC
	if (ext_dev >= 0 && jiffies - prevtime > MAX_ALLOW_SYNC) {
	    jiffies = prevtime + MAX_ALLOW_SYNC;
	}
#endif
	prevtime = jiffies;
	jiffies = (jiffies << 8) | SEQ_WAIT;
	sbwrite((char *) &jiffies);
#ifdef MAX_ALLOW_SYNC
	if (ptime > prevtime) midisync();
#endif
    } else if (jiffies < prevtime) {
	fprintf(stderr, "(warning: %dcsec negative sync time)\n", prevtime - jiffies);
	prevtime = jiffies;
    }
}

#ifdef MIDIIN

/*
 * Ask driver if there is midi input pending.
 */
int midipending(void)
{
    int tmp;
    if (ext_dev < 0)
	return (0);
    ioctl(seq_fd, SNDCTL_SEQ_GETINCOUNT, &tmp);
    return(tmp);
}

/*
 * Get one byte from midi input port.
 */
int midigetc(void)
{
    int l, havemidi;
    int attempts;

    attempts = 0;
    havemidi = 0;
    while (!havemidi) {
	attempts++;
	if (attempts > 2)
	    fprintf(stderr, "problem reading midi input after %d attempts\n", attempts);
	if ((l = read(seq_fd, buf, 4)) == -1) {
	    perror("midi get /dev/sequencer");
	    STOPNOW;
	}
	if (l != 4) {
	    fprintf(stderr, "error: could only read %d of 4 bytes seq. input\n", l);
	    STOPNOW;
	}
	if (buf[0] == SEQ_WAIT) {
	    sb_time = buf[1] + (buf[2] << 8) + (buf[3] << 16);
	} else if (buf[0] == SEQ_MIDIPUTC)
	    havemidi = 1;
    }
    return (buf[1]);
}

/*
 * Non-blocking input of one byte from midi input port.
 */
int midigetnb()
{
    if (midipending())
	return (midigetc());
    return (-1);
}

/* following 2 routines adapted from midifile.c,
 * by Tim Thompson and M. Czeisperger
 */
/*
 * Call a midifile library routine to write a midi message to
 * the recording file.
 */
static void chanmessage(status, c1, c2)
int status;
int c1, c2;
{
    int chan = recording_channel - 1;
    unsigned char data[2];

    data[0] = c1;
    data[1] = c2;

    switch (status & 0xf0) {
    case 0x80:
	mf_write_midi_event(getdelta(), MIDI_OFF_NOTE, chan, data, 2);
	break;
    case 0x90:
	mf_write_midi_event(getdelta(), MIDI_ON_NOTE, chan, data, 2);
	break;
    case 0xa0:
	mf_write_midi_event(getdelta(), MIDI_POLY_TOUCH, chan, data, 2);
	break;
    case 0xb0:
	if (c1 != 123)
	    mf_write_midi_event(getdelta(), MIDI_CTRL, chan, data, 2);
	break;
    case 0xc0:
	mf_write_midi_event(getdelta(), MIDI_CH_PROGRAM, chan, data, 1);
	break;
    case 0xd0:
	mf_write_midi_event(getdelta(), MIDI_TOUCH, chan, data, 1);
	break;
    case 0xe0:
	mf_write_midi_event(getdelta(), MIDI_BEND, chan, data, 2);
	break;
    }
}

/*
 * Get bytes from midi input port, so long as any are available,
 * and build up a midi message from them.  When message is complete,
 * call chanmessage() to write it out to the recording file.
 */
static void midimessage()
{
    /* This array is indexed by the high half of a status byte.  It's */
    /* value is either the number of bytes needed (1 or 2) for a channel */
    /* message, or 0 (meaning it's not  a channel message). */
    static int chantype[] =
    {
	0, 0, 0, 0, 0, 0, 0, 0,	/* 0x00 through 0x70 */
	2, 2, 2, 2, 1, 1, 2, 0	/* 0x80 through 0xf0 */
    };
    static int c = -1, c1 = -1, c2 = -1;
    static int running = 0;	/* 1 when running status used */
    static int status = 0;	/* status value (e.g. 0x90==note-on) */
    int needed;


    if (c == -1) {
	c = midigetnb();
	if (c == -1)
	    return;
    }
    if ((c & 0x80) == 0) {	/* running status? */
	if (status == 0) {
	    mferror("warning: unexpected midi input running status");
	    c = c1 = c2 = -1;
	    return;
	}
	running = 1;
    } else {
	status = c;
	running = 0;
    }

    needed = chantype[(status >> 4) & 0xf];

    if (needed) {		/* ie. is it a channel message? */

	if (running)
	    c1 = c;
	else if (c1 == -1) {
	    c1 = midigetnb();
	    if (c1 == -1)
		return;
	}
	if (c1 & 0x80) {
	    c = c1;
	    c1 = c2 = -1;
	    return;
	}
	if (needed > 1) {
	    if (c2 == -1)
		c2 = midigetnb();
	    if (c2 == -1)
		return;
	} else
	    c2 = 0;

	if (c2 & 0x80) {
	    c = c2;
	    c1 = c2 = -1;
	    return;
	}
	chanmessage(status, c1, c2);
	c = c1 = c2 = -1;
    } else
	mferror("apparent non-channel message");
}
#endif
