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

#include <stdio.h>
# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Toggle.h>

# include "cdrom_globs.h"
#ifdef __FreeBSD__
# include "cdrom_freebsd.h"
#endif
#ifdef sun
# include "cdrom_sun.h"
#endif
#ifdef sgi
# include "cdrom_sgi.h"
#endif

# include "play.xbm"
# include "pause.xbm"
# include "stop.xbm"
# include "prev.xbm"
# include "next.xbm"
# include "eject.xbm"
#ifdef sgi
# include "audio.xbm"
#endif

# include "quit.xbm"
# include "cycle.xbm"
# include "shuffle.xbm"
# include "rewind.xbm"
# include "ff.xbm"
# include "pgm.xbm"

void     title_dialog_setup();
static	void     play_button_setup();
static	void     stop_button_setup();
static	void     pause_button_setup();
static	void     prev_button_setup();
static	void     next_button_setup();
static	void     eject_button_setup();
#ifdef sgi
void     audio_button_setup();
#endif
static	void     quit_button_setup();
static	void     cycle_button_setup();
static	void     shuffle_button_setup();
static	void     rew_button_setup();
static	void     ff_button_setup();
static	void     pgm_button_setup();
void     buttons_reset();

static Widget	buttons_form_widget;
static Widget	play_button_widget;
static Widget	stop_button_widget;
static Widget	pause_button_widget;
static Widget	eject_button_widget;
#ifdef sgi
static Widget	audio_button_widget;
#endif
static Widget	shuffle_button_widget;
static Widget	cycle_button_widget;
static Widget	pgm_button_widget;

/* 
 * these are global for the "scan" functions to query their state;
 * they and the code that they work with should probably be be switched
 * to repeater widgets once R5 becomes universal...
 */
Widget	rew_button_widget;
Widget	ff_button_widget;


void
button_setup(parent_widget)
	Widget		parent_widget;
{
	Arg		args[1];

	buttons_form_widget = XtCreateManagedWidget("buttonsForm",
						    formWidgetClass,
						    parent_widget,
						    (ArgList) NULL, 0);

	play_button_setup(buttons_form_widget);

	pause_button_setup(buttons_form_widget);

	stop_button_setup(buttons_form_widget);

	prev_button_setup(buttons_form_widget);

	next_button_setup(buttons_form_widget);

	eject_button_setup(buttons_form_widget);

#ifdef sgi
	audio_button_setup(buttons_form_widget);
#endif

	quit_button_setup(buttons_form_widget);

	cycle_button_setup(buttons_form_widget);

	shuffle_button_setup(buttons_form_widget);

	rew_button_setup(buttons_form_widget);

	ff_button_setup(buttons_form_widget);

	pgm_button_setup(buttons_form_widget);


	/* set the initial state of the buttons */
	buttons_reset();
}

static void
play_button_setup(parent_widget)
	Widget		parent_widget;
{
	Pixmap		play_button_pixmap;
	Arg		args[1];

	play_button_widget = XtCreateManagedWidget("playButton",
						   toggleWidgetClass,
						   parent_widget,
						   (ArgList) NULL, 0);

	play_button_pixmap = XCreateBitmapFromData(XtDisplay(play_button_widget),
						   rootwin(play_button_widget),
						   play_bits,
						   play_width, play_height);

	XtSetArg(args[0], XtNbitmap, (XtArgVal) play_button_pixmap);
	XtSetValues(play_button_widget, args, 1);

	XtAddCallback(play_button_widget, XtNcallback, cb_cdrom_play, 0);

}

static void
pause_button_setup(parent_widget)
	Widget		parent_widget;
{
	Pixmap		pause_button_pixmap;
	Arg		args[1];

	pause_button_widget = XtCreateManagedWidget("pauseButton",
						    toggleWidgetClass,
						    parent_widget,
						    (ArgList) NULL, 0);

	pause_button_pixmap = XCreateBitmapFromData(XtDisplay(pause_button_widget),
						    rootwin(pause_button_widget),
						    pause_bits,
						    pause_width, pause_height);

	XtSetArg(args[0], XtNbitmap, (XtArgVal) pause_button_pixmap);
	XtSetValues(pause_button_widget, args, 1);

	XtAddCallback(pause_button_widget, XtNcallback, cb_cdrom_pause, 0);
}

static void
stop_button_setup(parent_widget)
	Widget		parent_widget;
{
	Pixmap		stop_button_pixmap;
	Arg		args[1];

	stop_button_widget = XtCreateManagedWidget("stopButton",
						   toggleWidgetClass,
						   parent_widget,
						   (ArgList) NULL, 0);

	stop_button_pixmap = XCreateBitmapFromData(XtDisplay(stop_button_widget),
						   rootwin(stop_button_widget),
						   stop_bits,
						   stop_width, stop_height);

	XtSetArg(args[0], XtNbitmap, (XtArgVal) stop_button_pixmap);
	XtSetValues(stop_button_widget, args, 1);

	XtAddCallback(stop_button_widget, XtNcallback, cb_cdrom_stop, 0);

}

static void
prev_button_setup(parent_widget)
	Widget		parent_widget;
{
	Widget		prev_button_widget;
	Pixmap		prev_button_pixmap;
	Arg		args[1];

	prev_button_widget = XtCreateManagedWidget("prevButton",
						  toggleWidgetClass,
						  parent_widget,
						  (ArgList) NULL, 0);

	prev_button_pixmap = XCreateBitmapFromData(XtDisplay(prev_button_widget),
						  rootwin(prev_button_widget),
						  prev_bits,
						  prev_width, prev_height);

	XtSetArg(args[0], XtNbitmap, (XtArgVal) prev_button_pixmap);
	XtSetValues(prev_button_widget, args, 1);

	XtAddCallback(prev_button_widget, XtNcallback, cb_cdrom_previous, 0);
}

static void
next_button_setup(parent_widget)
	Widget		parent_widget;
{
	Widget		next_button_widget;
	Pixmap		next_button_pixmap;
	Arg		args[1];

	next_button_widget = XtCreateManagedWidget("nextButton",
						 toggleWidgetClass,
						 parent_widget,
						 (ArgList) NULL, 0);

	next_button_pixmap = XCreateBitmapFromData(XtDisplay(next_button_widget),
						 rootwin(next_button_widget),
						 next_bits,
						 next_width, next_height);

	XtSetArg(args[0], XtNbitmap, (XtArgVal) next_button_pixmap);
	XtSetValues(next_button_widget, args, 1);

	XtAddCallback(next_button_widget, XtNcallback, cb_cdrom_next, 0);
}

static void
eject_button_setup(parent_widget)
	Widget		parent_widget;
{
	Pixmap		eject_button_pixmap;
	Arg		args[1];

	eject_button_widget = XtCreateManagedWidget("ejectButton",
						    toggleWidgetClass,
						    parent_widget,
						    (ArgList) NULL, 0);

	eject_button_pixmap = XCreateBitmapFromData(XtDisplay(eject_button_widget),
						    rootwin(eject_button_widget),
						    eject_bits,
						    eject_width, eject_height);

	XtSetArg(args[0], XtNbitmap, (XtArgVal) eject_button_pixmap);
	XtSetValues(eject_button_widget, args, 1);

	XtAddCallback(eject_button_widget, XtNcallback, cb_cdrom_eject, 0);
}

#ifdef sgi
static void
audio_button_setup(parent_widget)
	Widget		parent_widget;
{
	Pixmap		audio_button_pixmap;
	Arg		args[1];

	audio_button_widget = XtCreateManagedWidget("audioButton",
						    toggleWidgetClass,
						    parent_widget,
						    (ArgList) NULL, 0);

	audio_button_pixmap = XCreateBitmapFromData(XtDisplay(audio_button_widget),
						    rootwin(audio_button_widget),
						    audio_bits,
						    audio_width, audio_height);

	XtSetArg(args[0], XtNbitmap, (XtArgVal) audio_button_pixmap);
	XtSetValues(audio_button_widget, args, 1);

	XtAddCallback(audio_button_widget, XtNcallback, cb_cdrom_audio, 0);
}
#endif

static void
quit_button_setup(parent_widget)
	Widget		parent_widget;
{
	Widget		quit_button_widget;
	Pixmap		quit_button_pixmap;
	Arg		args[1];

	quit_button_widget = XtCreateManagedWidget("quitButton",
						    toggleWidgetClass,
						    parent_widget,
						    (ArgList) NULL, 0);

	quit_button_pixmap = XCreateBitmapFromData(XtDisplay(quit_button_widget),
						    rootwin(quit_button_widget),
						    quit_bits,
						    quit_width, quit_height);

	XtSetArg(args[0], XtNbitmap, (XtArgVal) quit_button_pixmap);
	XtSetValues(quit_button_widget, args, 1);

	XtAddCallback(quit_button_widget, XtNcallback, cb_cdrom_quit, 0);
}

static void
cycle_button_setup(parent_widget)
	Widget		parent_widget;
{
	Widget		cycle_button_widget;
	Pixmap		cycle_button_pixmap;
	Arg		args[1];

	cycle_button_widget = XtCreateManagedWidget("cycleButton",
						    toggleWidgetClass,
						    parent_widget,
						    (ArgList) NULL, 0);

	cycle_button_pixmap = XCreateBitmapFromData(XtDisplay(cycle_button_widget),
						    rootwin(cycle_button_widget),
						    cycle_bits,
						    cycle_width, cycle_height);

	XtSetArg(args[0], XtNbitmap, (XtArgVal) cycle_button_pixmap);
	XtSetValues(cycle_button_widget, args, 1);

	XtAddCallback(cycle_button_widget, XtNcallback, cb_cdrom_cycle, 0);
}

static void
shuffle_button_setup(parent_widget)
	Widget		parent_widget;
{
	Pixmap		shuffle_button_pixmap;
	Arg		args[1];

	shuffle_button_widget = XtCreateManagedWidget("shuffleButton",
						      toggleWidgetClass,
						      parent_widget,
						      (ArgList) NULL, 0);

	shuffle_button_pixmap = XCreateBitmapFromData(XtDisplay(shuffle_button_widget),
						      rootwin(shuffle_button_widget),
						      shuffle_bits,
						      shuffle_width, shuffle_height);

	XtSetArg(args[0], XtNbitmap, (XtArgVal) shuffle_button_pixmap);
	XtSetValues(shuffle_button_widget, args, 1);

	XtAddCallback(shuffle_button_widget, XtNcallback, cb_cdrom_shuffle, 0);
}


static void
rew_button_setup(parent_widget)
        Widget          parent_widget;
{
        Pixmap          rew_button_pixmap;
        Arg             args[1];

        rew_button_widget = XtCreateManagedWidget("rewButton",
                                                  toggleWidgetClass,
                                                  parent_widget,
                                                  (ArgList) NULL, 0);

        rew_button_pixmap = XCreateBitmapFromData(XtDisplay(rew_button_widget),
                                                  rootwin(rew_button_widget),
                                                  rewind_bits,
                                                  rewind_width, rewind_height);

        XtSetArg(args[0], XtNbitmap, (XtArgVal) rew_button_pixmap);
        XtSetValues(rew_button_widget, args, 1);

        XtAddCallback(rew_button_widget, XtNcallback, cb_cdrom_rewind, 0);
}



static void
ff_button_setup(parent_widget)
        Widget          parent_widget;
{
        Pixmap          ff_button_pixmap;
        Arg             args[1];

        ff_button_widget = XtCreateManagedWidget("ffButton",
                                                 toggleWidgetClass,
                                                 parent_widget,
                                                 (ArgList) NULL, 0);

        ff_button_pixmap = XCreateBitmapFromData(XtDisplay(ff_button_widget),
                                                 rootwin(ff_button_widget),
                                                 ff_bits,
                                                 ff_width, ff_height);

        XtSetArg(args[0], XtNbitmap, (XtArgVal) ff_button_pixmap);
        XtSetValues(ff_button_widget, args, 1);

        XtAddCallback(ff_button_widget, XtNcallback, cb_cdrom_ff, 0);
}

static void
pgm_button_setup(parent_widget)
        Widget          parent_widget;
{
        Pixmap          pgm_button_pixmap;
        Arg             args[1];
	extern Widget	top_form;

        pgm_button_widget = XtCreateManagedWidget("pgmButton",
                                                 toggleWidgetClass,
                                                 parent_widget,
                                                 (ArgList) NULL, 0);

        pgm_button_pixmap = XCreateBitmapFromData(XtDisplay(pgm_button_widget),
                                                 rootwin(pgm_button_widget),
                                                 pgm_bits,
                                                 pgm_width, pgm_height);

        XtSetArg(args[0], XtNbitmap, (XtArgVal) pgm_button_pixmap);
        XtSetValues(pgm_button_widget, args, 1);

        XtAddCallback(pgm_button_widget, XtNcallback, cb_cdrom_program,
		(XtPointer) top_form);
}

void
play_button_set() {
	Arg		args[1];

	XtSetArg(args[0], XtNstate, True);
	XtSetValues(play_button_widget, args, 1);
}

void
play_button_reset() {
	Arg		args[1];

	XtSetArg(args[0], XtNstate, False);
	XtSetValues(play_button_widget, args, 1);
}

void
pause_button_reset() {
	Arg		args[1];

	XtSetArg(args[0], XtNstate, False);
	XtSetValues(pause_button_widget, args, 1);
}

void
stop_button_reset() {
	Arg		args[1];

	XtSetArg(args[0], XtNstate, False);
	XtSetValues(stop_button_widget, args, 1);
}

void
eject_button_reset() {
	Arg		args[1];

	XtSetArg(args[0], XtNstate, False);
	XtSetValues(eject_button_widget, args, 1);
}

void
eject_button_set() {
	Arg		args[1];

	XtSetArg(args[0], XtNstate, True);
	XtSetValues(eject_button_widget, args, 1);
}

#ifdef sgi
void
audio_button_reset() {
	Arg		args[1];

	XtSetArg(args[0], XtNstate, False);
	XtSetValues(audio_button_widget, args, 1);
}

void
audio_button_set() {
	Arg		args[1];

	XtSetArg(args[0], XtNstate, True);
	XtSetValues(audio_button_widget, args, 1);
}

int
audio_button_state() {
	Arg		args[1];
	Boolean		state;

	XtSetArg(args[0], XtNstate, &state);
	XtGetValues(audio_button_widget, args, 1);
	return(state);
}
#endif

void
shuffle_button_set() {
	Arg		args[1];

	XtSetArg(args[0], XtNstate, True);
	XtSetValues(shuffle_button_widget, args, 1);
}

void
shuffle_button_reset() {
	Arg		args[1];

	XtSetArg(args[0], XtNstate, False);
	XtSetValues(shuffle_button_widget, args, 1);
}

void
cycle_button_reset() {
	Arg		args[1];

	XtSetArg(args[0], XtNstate, False);
	XtSetValues(cycle_button_widget, args, 1);
}

void
pgm_button_set() {
	Arg		args[2];

	XtSetArg(args[0], XtNstate, True);
	XtSetValues(pgm_button_widget, args, 1);
}

void
pgm_button_reset() {
	Arg		args[2];

	XtSetArg(args[0], XtNstate, False);
	XtSetValues(pgm_button_widget, args, 1);
}

void
buttons_reset() {
	Arg		args[2];
#ifdef sun
	Boolean		state;
#endif

#ifdef sgi
	if (cdi.scsi_audio) {
		XtSetArg(args[0], XtNstate, True);
		XtSetValues(audio_button_widget, args, 1);
	} else {
		XtSetArg(args[0], XtNstate, False);
		XtSetValues(audio_button_widget, args, 1);
	}
#endif

	switch (cdrom_status()) {
	case -1:
		XtSetArg(args[0], XtNstate, True);
		XtSetValues(eject_button_widget, args, 1);
		cdi.state |= CDROM_STATE_EJECTED;
		disc_title = NODISCSTR;
		update_title();
		break;

	case CDROM_PLAYING:
		XtSetArg(args[0], XtNstate, True);
		XtSetValues(play_button_widget, args, 1);
		cdi.state |= CDROM_STATE_PLAY;
		cdrom_timer_on();
		break;

	case CDROM_PAUSED:
#ifdef sun
		/* 
		 * The Sun doesn't have a "stop" state, so handle the
		 * initial startup condition.
		 */
		XtSetArg(args[0], XtNstate, &state);
		XtGetValues(pause_button_widget, args, 1);
		if (state == False) {
			XtSetArg(args[0], XtNstate, True);
			XtSetValues(stop_button_widget, args, 1);
			cdi.state |= CDROM_STATE_STOP;
			break;
		} 
#endif
		XtSetArg(args[0], XtNstate, True);
		XtSetValues(play_button_widget, args, 1);
		XtSetArg(args[0], XtNstate, True);
		XtSetValues(pause_button_widget, args, 1);
		cdi.state |= CDROM_STATE_PLAY;
		cdi.curtrack = cdrom_get_curtrack();
		track_button_update(); /* make track button read on startup */
		timer_button_update(); /* make timer button read on startup */
		cdi.state |= CDROM_STATE_PAUSE;
		break;

	default:
		XtSetArg(args[0], XtNstate, True);
		XtSetValues(stop_button_widget, args, 1);
		cdi.state |= CDROM_STATE_STOP;
		break;
	}
}
