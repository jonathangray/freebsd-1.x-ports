/* wn_event.c - input event processing */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char wn_wn_replay_c_sccsid[] = "@(#)wn_replay.c	1.14 25/4/92 (UKC)";

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/file.h>
#include <stdlib.h>
#include <string.h>

#ifdef __STDC__
#include <unistd.h>
#endif

#include <local/ukcprog.h>

#include "wn.h"
#include "wn_priv.h"
#include "wn_replay.h"
#include "wn_init.h"

#define PUTSHORT(s, fp)	{ putc(((s) & 0xff), (fp)); putc(((s) >> 8) & 0xff, (fp)); }
#define GETSHORT(s, fp)	{ (s) = getc(fp) & 0xff; (s) |= (getc(fp) & 0xff) << 8; }

/*  We write out times in units of ms/SCALE.  It is more efficient if
 *  SCALE is a power of two, and the units should be in the region of
 *  a tenth of a second.
 */
#define TIME_SCALE		(2 * 65526)

/*  The smallest number of time units that it's worth recording a delay
 *  for.
 */
#define MIN_TIME_DELTA	1

#define BYTE_DELTA_BIAS		0x80

#define AE_CHANGE_WN	'a'
#define AE_ZERO_DELTA	'b'
#define AE_BYTE_DELTA	'c'
#define AE_SHORT_DELTA	'd'
#define AE_DELAY	'e'

#define AE_BASE		'z'

int _wn_Recording = FALSE;
int _wn_Replaying = FALSE;

static FILE *Dumpfp = NULL;

static FILE *Record_fp = NULL;
static FILE *Replay_fp = NULL;

static int Record_flush = TRUE;

static wn_replay_cursor_mode_t Replay_cursor_mode = WN_RP_NO_FEEDBACK;

void
wn_set_replay_cursor_mode(mode)
wn_replay_cursor_mode_t mode;
{
	Replay_cursor_mode = mode;
}

int
wn_set_record_file(filename)
const char *filename;
{
	if (strcmp(filename, "-") == 0)
		Record_fp = stdout;
	else if (strcmp(filename, "---") == 0) {
		Record_fp = stdout;
		Record_flush = TRUE;
	}
	else if ((Record_fp = fopen(filename, "w")) == NULL)
		return -1;
	_wn_Recording = TRUE;
	return 0;
}

int
wn_set_replay_file(filename)
const char *filename;
{
	if (strcmp(filename, "-") == 0)
		Replay_fp = stdin;
	else if ((Replay_fp = fopen(filename, "r")) == NULL)
		return -1;
	_wn_Replaying = TRUE;
	return 0;
}

int
wn_record_event(ev)
const event_t *ev;
{
	static int want_timing = -1;
	static int last_wn = 0;
	static int last_x = 0;
	static int last_y = 0;
	static struct timeval last_time = { 0, 0 };
	FILE *fp;
	struct timeval current_time;
	int dx, dy, ae;
	unsigned bit;

	fp = Record_fp;

	if (want_timing == -1)
		want_timing = getenv("WN_NO_TIMING") == NULL;

	if (want_timing) {
		gettimeofday(&current_time, (struct timezone *)NULL);
		if (last_time.tv_sec != 0) {
			long delta;
			int units;

			delta = (current_time.tv_sec - last_time.tv_sec) * 1000000 +
					    current_time.tv_usec - last_time.tv_usec;
			units = delta / TIME_SCALE;
			if (units > 255)
				units = 255;
			if (units >= MIN_TIME_DELTA) {
				putc(AE_DELAY, fp);
				putc(units, fp);
			}
		}
		last_time = current_time;
	}

	if (ev->ev_wn != last_wn) {
		putc(AE_CHANGE_WN, fp);
		PUTSHORT(ev->ev_wn, fp);
	}

	if (ev->ev_x == last_x && ev->ev_y == last_y) {
		putc(AE_ZERO_DELTA, fp);
	}
	else {
		dx = ev->ev_x - last_x;
		dy = ev->ev_y - last_y;
		if (dx >= -BYTE_DELTA_BIAS && dx < BYTE_DELTA_BIAS &&
		    dy >= -BYTE_DELTA_BIAS && dy < BYTE_DELTA_BIAS) {
			putc(AE_BYTE_DELTA, fp);
			putc(dx + BYTE_DELTA_BIAS, fp);
			putc(dy + BYTE_DELTA_BIAS, fp); 
			if (Dumpfp != NULL)
				fprintf(Dumpfp, "b [%d,%d] -> [%d,%d]\n",
						dx, dy, ev->ev_x, ev->ev_y);
		}
		else {
			putc(AE_SHORT_DELTA, fp);
			PUTSHORT(dx, fp);
			PUTSHORT(dy, fp);
			if (Dumpfp != NULL)
				fprintf(Dumpfp, "s [%d,%d] -> [%d,%d]\n",
						dx, dy, ev->ev_x, ev->ev_y);
		}
		last_x = ev->ev_x;
		last_y = ev->ev_y;
	}

	for (ae = AE_BASE, bit = 1; (ev->ev_type & bit) == 0; ++ae, bit <<= 1)
		;
	putc(ae, fp);

	switch (ev->ev_type) {
	case EV_KEY:
		putc(ev->ev_char, fp);
		break;
	case EV_BUTTON_DOWN:
	case EV_BUTTON_UP:
		putc(ev->ev_flags, fp);
		break;
	}

	if (Record_flush)
		fflush(fp);
	if (ferror(fp)) {
		fprintf(stderr, "wn: error recording event (%s)\n", _wn_reason());
		return -1;
	}
	return 0;
}

int
wn_get_recorded_event(ev)
event_t *ev;
{
	static short last_wn = 0;
	static int last_x = 0;
	static int last_y = 0;
	static int last_buttons = 0;
	FILE *fp;
	short dx, dy;
	int type, ch, flags;

	fp = Replay_fp;

	if ((ch = getc(fp)) == EOF) {
		fprintf(stderr,
			     "wn: EOF in replay file - switching to normal input\n");
		_wn_Replaying = FALSE;
		return -1;
	}

	if (ch == AE_DELAY) {
		struct timeval delay;
		int units;
		static long max_wait = -1;

		if (max_wait == -1) {
			const char *s = getenv("WN_MAX_WAIT");
			max_wait = (s == NULL) ? 0 : atoi(s) * 1000;
		}

		units = (unsigned char)getc(fp);
		delay.tv_usec = units * TIME_SCALE;
		if (max_wait != 0 && delay.tv_usec > max_wait)
			delay.tv_usec = max_wait;
		if (delay.tv_usec > 10000) {
			for (delay.tv_sec = 0; delay.tv_usec >= 1000000;
								++delay.tv_sec)
				delay.tv_usec -= 1000000;
			select(0, (fd_set *)NULL, (fd_set *)NULL, (fd_set *)NULL,
									     &delay);
		}
		ch = getc(fp);
	}

	if (ch == AE_CHANGE_WN) {
		GETSHORT(last_wn, fp);
		ch = getc(fp);
	}

	if (ch == AE_ZERO_DELTA) {
		ch = getc(fp);
		dx = dy = 0;
	}
	else if (ch == AE_BYTE_DELTA) {
		dx = (int)(unsigned char)getc(fp) - BYTE_DELTA_BIAS;
		dy = (int)(unsigned char)getc(fp) - BYTE_DELTA_BIAS;
		last_x += dx;
		last_y += dy;
		ch = getc(fp);
		if (Dumpfp != NULL) {
			fprintf(Dumpfp, "s [%d,%d] -> [%d,%d]\n",
						dx, dy, last_x, last_y);
			fflush(Dumpfp);
		}
	}
	else if (ch == AE_SHORT_DELTA) {
		GETSHORT(dx, fp);
		GETSHORT(dy, fp);
		last_x += dx;
		last_y += dy;
		ch = getc(fp);
		if (Dumpfp != NULL) {
			fprintf(Dumpfp, "s [%d,%d] -> [%d,%d]\n",
						dx, dy, last_x, last_y);
			fflush(Dumpfp);
		}
	}
	else
		dx = dy = 0;

	type = 1 << (ch - AE_BASE);

	switch(type) {
	case EV_KEY:
		flags = 0;
		ev->ev_char = getc(fp);
		break;
	case EV_BUTTON_DOWN:
		flags = getc(fp);
		last_buttons |= (flags & B_ANY);
		break;
	case EV_BUTTON_UP:
		flags = getc(fp);
		last_buttons &= ~(flags & B_ANY);
		break;
	case EV_OTHER_INPUT:
	case EV_INTERRUPT:
	case EV_SELECTION_REQUEST:
		/* these are meaningless when coming from a recording */
		flags = 0;
		type = EV_OTHER_INPUT;
		break;
	default:
		flags = 0;
		break;
	}

	if (ferror(fp)) {
		fprintf(stderr, "wn: read error in replay file - switching to normal input (%s)\n",
								_wn_reason());
		return -1;
	}

	ev->ev_type = type;
	ev->ev_flags = flags;
	ev->ev_x = last_x;
	ev->ev_y = last_y;
	ev->ev_buttons = last_buttons;
	ev->ev_wn = last_wn;

	if (Replay_cursor_mode == WN_RP_WARP_MOUSE && (dx != 0 || dy != 0))
		wn_warp_mouse(ev->ev_wn, ev->ev_x, ev->ev_y);
	
	return 0;
}
