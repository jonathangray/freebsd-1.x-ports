/*
 * Copyright (C) 1990 Regents of the University of California.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of the University of
 * California not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission.  the University of California makes no representations
 * about the suitability of this software for any purpose.  It is provided
 * "as is" without express or implied warranty.
 */

#include "version.h"
#include "patchlevel.h"

/* shorthand */
# define rootwin(x)	RootWindow(XtDisplay(x), XtWindow(x))

/*
 * number of milliseconds to sleep between
 * checking to see what the current track
 * is.
 */
# define TIMER_PERIOD		((unsigned long) 250)

# define BACKWARDS	-1
# define FORWARDS	1

extern Widget		top_setup();
extern Widget		main_setup();

extern void		cb_cdrom_play();
extern void		cb_cdrom_pause();
extern void		cb_cdrom_stop();
extern void		cb_cdrom_previous();
extern void		cb_cdrom_next();
extern void		cb_cdrom_eject();
#ifdef sgi
extern void		cb_cdrom_audio();
#endif
extern void		cb_cdrom_cycle();
extern void		cb_cdrom_quit();
extern void		cb_cdrom_shuffle();
extern void		cb_cdrom_rewind();
extern void		cb_cdrom_ff();
extern void		cb_cdrom_program();

extern int		cdrom_get_curtime();
extern void		cdrom_timer_on();
extern void		cdrom_timer_off();
extern int		cdrom_play();
extern void		cdrom_reset();
extern void		cdrom_rewind();
extern void		cdrom_ff();

extern void		logo_setup();
extern void		track_button_update();
extern void		track_button_set();
extern void		track_button_reset();
extern void		timer_button_update();
extern void		timer_button_set();
extern void		timer_button_reset();

extern void		button_setup();
extern void		top_start();
extern void		update_status();
extern void		play_button_set();
extern void		play_button_reset();
extern void		pause_button_reset();
extern void		eject_button_reset();
extern void		shuffle_button_set();
extern void		shuffle_button_reset();
extern void		pgm_button_set();
extern void		pgm_button_reset();
extern void     	update_title();


extern void		leds_stop();
extern void		leds_update();

extern void		shuffle_setup();
extern unsigned char	shuffle_next_track();
extern unsigned char	shuffle_prev_track();

extern void		program_form_setup();
extern void		popup_program_form();
extern int      	program_time_remaining();
extern unsigned char	program_resume();
extern unsigned char	program_goto_next_track();
extern unsigned char	program_goto_prev_track();
extern unsigned char	program_next_track();
extern unsigned char	program_prev_track();
extern void		program_add_track();
extern void		program_cancel();

extern int		cdrom_open();
extern int		cdrom_init();


extern Boolean		display_timer;

extern XtAppContext	appc;

extern char		*file;
extern char		*device;
extern char             info_filename[];
extern char     	*disc_title;
extern unsigned int     timer_fsecs;


extern float		volbase;
extern float		volpcent;
extern int		replayThreshold;
extern float    	scanPauseInterval;
extern int      	scanSkipInterval;
extern float      	pausePauseInterval;
extern int      	pauseSkipInterval;
