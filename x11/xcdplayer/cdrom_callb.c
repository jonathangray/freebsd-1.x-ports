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

# include <X11/Intrinsic.h>
# include <X11/StringDefs.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Toggle.h>

# include <stdio.h>

# include "debug.h"
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

void		cdrom_new_disc();

/*
 * cb_cdrom_play checks to see if we're playing; if so do nothing.
 * if not playing turns on playing.  if paused then return.
 * otherwise start cd playing.
 */
/*ARGSUSED*/
void
cb_cdrom_play(widget, client_data, call_data)
	Widget		widget;
	XtPointer	client_data;
	XtPointer	call_data;
{
	if (cdrom_open() == -1) {
		debug_printf(1, "cb_cdrom_play: error from cdrom_open\n");
		/* Turn the play button back off */
		play_button_reset();
		eject_button_set();
		return;
	}

	if (cdi.state & CDROM_STATE_EJECTED) {
		cdrom_new_disc(widget);
	}

	/* 
	 * toggle button weirdness; play and stop are radio toggles, ie. if
	 * you hit stop, and you are playing, play is supposed to stop, so
	 * toggle buttons automatically invoke the notify procedure of other
	 * members of a toggle group when another member of the toggle group
	 * is activated, before the requested notification takes place. 
	 * The practical upshot of this is that this routine can be called
	 * even if you didn't press play, and so it really takes no action.
	 */
	if (cdi.state & CDROM_STATE_PLAY) {
		debug_printf(1, "cb_cdrom_play: already playing track %d\n",
			    cdi.curtrack);
		return;
	}

	if (cdi.curtrack == 0) 	/* (no track selected) */
	{
	    cdi.curtrack = cdi.mintrack;
	    track_button_update();
	}

	cdi.state |= CDROM_STATE_PLAY;

	if (cdi.state & CDROM_STATE_PAUSE) {
		debug_printf(1, "cb_cdrom_play: paused on track %d\n",
			    cdi.curtrack);
		return;
	}

	timer_fsecs = 0;
	cdi.duration = 0;

	if (cdi.state & CDROM_STATE_SHUFFLE) 
		cdi.curtrack = shuffle_next_track();

	else if (cdi.state & CDROM_STATE_PROGRAM)
	{
	    if ((cdi.curtrack = program_resume()) == 0) {

		/* cancel program */
		debug_printf(1, "cb_cdrom_play: cancelling program\n");
		program_cancel();
	    	cdi.curtrack = cdi.mintrack;
	    }
	    else {
		debug_printf(1, "cb_cdrom_play: resetting timer button\n");
	        timer_button_reset();
	    }
	}

	if (cdrom_play() != -1) {
		cdi.state &= ~CDROM_STATE_STOP;
	}

	timer_button_update();
}

/*
 * cb_cdrom_pause toggles pausing on or off.
 */
/*ARGSUSED*/
void
cb_cdrom_pause(widget, client_data, call_data)
	Widget		widget;
	XtPointer	client_data;
	XtPointer	call_data;

{
	if (cdrom_open() == -1) {
		debug_printf(1, "cb_cdrom_pause: error from cdrom_open\n");
		pause_button_reset();
		return;
	}

#ifdef sgi
	if (cdrom_status() == CDROM_NO_STATUS) {
		cdi.state |= CDROM_STATE_EJECTED;
		buttons_reset();
		pause_button_reset();
		return;
	}
#endif

	if (cdi.state & CDROM_STATE_EJECTED) {
		cdrom_new_disc(widget);
	}

	if (cdi.state & CDROM_STATE_PAUSE) {
		cdi.state &= ~CDROM_STATE_PAUSE;

		if (cdi.state & CDROM_STATE_PROGRAM) 
			cdi.curtrack = program_resume();

		else if (cdi.curtrack == 0) 	/* (no track selected) */
	    {
	        cdi.curtrack = cdi.mintrack;
	        track_button_update();
	    }

		debug_printf(1, "cb_cdrom_pause: resuming track %d\n",
			    cdi.curtrack);

		/*
		 * if we use next or prev after a pause we can't
		 * just resume but have to move to the track.
		 */
		if ((cdi.curtrack == cdrom_get_curtrack())  &&
		    (cdi.duration > 0))
		{
			if (cdrom_resume() != -1) {
				cdi.state &= ~CDROM_STATE_EJECTED;
				eject_button_reset();
				cdi.state &= ~CDROM_STATE_STOP;
			}

			cdrom_timer_on();

			return;
		}

		if (cdrom_play() != -1) {
			cdi.state &= ~CDROM_STATE_EJECTED;
			eject_button_reset();
			cdi.state &= ~CDROM_STATE_STOP;
		}

		return;
	}

	cdi.state |= CDROM_STATE_PAUSE;

	debug_printf(1, "cb_cdrom_pause: pausing on track %d\n",
		    cdi.curtrack);

	if (cdrom_pause() != -1) {
		cdi.state &= ~CDROM_STATE_STOP;
		cdi.state |= CDROM_STATE_PLAY;
		eject_button_reset();
		play_button_set();
	}

	cdrom_timer_off();
}

/*
 * cb_cdrom_stop checks to see if we're playing; if not then
 * do nothing.  sets the current track to the first audio track.
 * turns off play, pause, stops the cd, and closes it so that the
 * disc can be ejected with the eject button on the drive.
 */
/*ARGSUSED*/
void
cb_cdrom_stop(widget, client_data, call_data)
	Widget		widget;
	XtPointer	client_data;
	XtPointer	call_data;
{
	Arg	args[1];
	Boolean	state;

	if (cdrom_open() == -1) {
		debug_printf(1, "cb_cdrom_stop: error from cdrom_open\n");
		/* Turn the stop button back off */
		stop_button_reset();
		eject_button_set();
		return;
	}

#ifdef sgi
	if (cdrom_status() == CDROM_NO_STATUS) {
		cdi.state |= CDROM_STATE_EJECTED;
		buttons_reset();
		/* Turn the stop button back off */
		stop_button_reset();
		return;
	}
#endif

	/* toggle button weirdness; see comment in cb_cdrom_play for details */
	XtSetArg(args[0], XtNstate, &state);
	XtGetValues(widget, args, 1);

	if (state == False) {
		debug_printf(1, "cb_cdrom_stop: already stopped\n");
		return;
	}

	debug_printf(1, "cb_cdrom_stop: resetting disc\n");

	cdrom_reset();

	if (cdi.state & CDROM_STATE_SHUFFLE) {
		debug_printf(1, "cb_cdrom_shuffle: shuffle on\n");
		cdi.state |= CDROM_STATE_SHUFFLE;
		shuffle_setup();
	}
}

/*
 * cb_cdrom_previous decrments the current track.  if paused or stopped
 * then return.  otherwise start playing the (new) current track.
 */
/*ARGSUSED*/
void
cb_cdrom_previous(widget, client_data, call_data)
	Widget		widget;
	XtPointer	client_data;
	XtPointer	call_data;
{
	if (cdrom_open() == -1) {
		debug_printf(1, "cb_cdrom_previous: error from cdrom_open\n");
		return;
	}

#ifdef sgi
	if (cdrom_status() == CDROM_NO_STATUS) {
		cdi.state |= CDROM_STATE_EJECTED;
		buttons_reset();
		return;
	}
#endif

	cdi.state &= ~CDROM_STATE_EJECTED;
	eject_button_reset();
	cdi.state &= ~CDROM_STATE_STOP;

	cdrom_timer_off();

	/*
	 * if playing less than replayThreshold seconds, back up to 
	 * previous track; otherwise start at beginning of current track:
	 */
	if (cdi.duration < replayThreshold)
	{
	    if ((cdi.program != NULL) &&
	    	 (cdi.state & CDROM_STATE_PLAY) &&
		 ((cdi.state & CDROM_STATE_PAUSE) == 0))
	    {
	    	if (program_prev_track() == 0) 
	            debug_printf(1, "cb_cdrom_previous: no previous selections in program\n");
	    	else
		{
	             debug_printf(1, "cb_cdrom_previous: going to prev selection\n");
		     cdi.curtrack = program_goto_prev_track();
		}
	    }
	    else if (cdi.curtrack > cdi.mintrack) /* can't go below 1st track:*/
	        cdi.curtrack--;
	}

	timer_fsecs = 0;
	cdi.duration = 0;

	if (cdi.state & CDROM_STATE_SHUFFLE)
		cdi.curtrack = shuffle_prev_track();

	track_button_update();
	timer_button_update();

	if (cdi.state & CDROM_STATE_PAUSE) {
		debug_printf(1, "cb_cdrom_previous: paused on track %d\n",
			    cdi.curtrack);
		if (cdrom_play() != -1) {
			cdi.state &= ~CDROM_STATE_STOP;
		}
		if (cdrom_pause() != -1) {
			cdi.state &= ~CDROM_STATE_STOP;
		}
		cdrom_timer_off();
		return;
	}

	if ((cdi.state & CDROM_STATE_PLAY) == 0) {
		debug_printf(1, "cb_cdrom_previous: stopped on track %d\n",
			    cdi.curtrack);
		return;
	}

	debug_printf(1, "cb_cdrom_previous: playing track %d\n",
		     cdi.curtrack);


	/* restart playing if not paused and currently playing */
	(void) cdrom_play();
}


/*
 * cb_cdrom_next incrments the current track.  if paused or stopped
 * then return.  otherwise start playing the (new) current track.
 */
/*ARGSUSED*/
void
cb_cdrom_next(widget, client_data, call_data)
	Widget		widget;
	XtPointer	client_data;
	XtPointer	call_data;
{
	if (cdrom_open() == -1) {
		debug_printf(1, "cb_cdrom_next: error from cdrom_open\n");
		return;
	}

#ifdef sgi
	if (cdrom_status() == CDROM_NO_STATUS) {
		cdi.state |= CDROM_STATE_EJECTED;
		buttons_reset();
		return;
	}
#endif

	if (cdi.state & CDROM_STATE_EJECTED) {
		cdrom_new_disc(widget);
	}

	cdi.state &= ~CDROM_STATE_EJECTED;
	eject_button_reset();
	cdi.state &= ~CDROM_STATE_STOP;

	if (cdi.state & CDROM_STATE_SHUFFLE) {
		if (cdi.currand == cdi.ntracks) {
			debug_printf(1, "cb_cdrom_next: at last track\n");
			return;
		}
	}
	else if ((cdi.program != NULL) &&
	    	 (cdi.state & CDROM_STATE_PLAY) &&
		 ((cdi.state & CDROM_STATE_PAUSE) == 0))
	{
	    if (program_next_track() == 0)
	        debug_printf(1, "cb_cdrom_next: no further selections in program\n");
	    else
	    {
	        debug_printf(1, "cb_cdrom_next: going to next selection\n");
	    	cdi.curtrack = program_goto_next_track();
	    }
	}
	else {
		if (cdi.curtrack >= cdi.maxtrack) {
			debug_printf(1, "cb_cdrom_next: at last track\n");
			return;
		}
		else
			cdi.curtrack++;
	}

	cdrom_timer_off();

	timer_fsecs = 0;
	cdi.duration = 0;


	track_button_update();
	timer_button_update();

	if (cdi.state & CDROM_STATE_PAUSE) {
		debug_printf(1, "cb_cdrom_next: paused on track %d\n",
			    cdi.curtrack);
		if (cdrom_play() != -1) {
			cdi.state &= ~CDROM_STATE_STOP;
		}
		if (cdrom_pause() != -1) {
			cdi.state &= ~CDROM_STATE_STOP;
		}
		cdrom_timer_off();
		return;
	}

	if ((cdi.state & CDROM_STATE_PLAY) == 0) {
		debug_printf(1, "cb_cdrom_next: stopped on track %d\n",
			    cdi.curtrack);
		return;
	}

	if (cdi.state & CDROM_STATE_SHUFFLE)
		cdi.curtrack = shuffle_next_track();

	debug_printf(1, "cb_cdrom_next: playing track %d\n",
		    cdi.curtrack);

	/* restart playing if not paused and currently playing */
	(void) cdrom_play();
}


/*ARGSUSED*/
void
cb_cdrom_eject(widget, client_data, call_data)
	Widget		widget;
	XtPointer	client_data;
	XtPointer	call_data;
{
	Arg	args[1];
	Boolean	state;

	if (cdrom_open() == -1) {
		debug_printf(1, "cb_cdrom_eject: error from cdrom_open\n");
		return;
	}

#ifdef sgi
	if (cdrom_status() == CDROM_NO_STATUS) {
		cdi.state |= CDROM_STATE_EJECTED;
		buttons_reset();
		return;
	}
#endif

	/* Check if this is just a toggle event.  If so, do nothing */
	XtSetArg(args[0], XtNstate, &state);
	XtGetValues(widget, args, 1);
	if (state == False) {
		return;
	}

	/* toggle button weirdness; see comment in cb_cdrom_play for details */
	if (cdi.state & CDROM_STATE_EJECTED) {
		debug_printf(1, "cb_cdrom_eject: already ejected\n");
		return;
	}

	debug_printf(1, "cb_cdrom_eject: ejecting on track %d\n",
		    cdi.curtrack);

	cdrom_reset();
	cdi.maxtrack = 0;

	program_cancel();
	cdrom_eject();
	cdi.state |= CDROM_STATE_EJECTED;
	cdrom_close();

	disc_title = NODISCSTR;
	update_title();
}

#ifdef sgi
/*ARGSUSED*/
void
cb_cdrom_audio(widget, client_data, call_data)
	Widget		widget;
	XtPointer	client_data;
	XtPointer	call_data;
{
	if (cdrom_open() == -1) {
		debug_printf(1, "cb_cdrom_audio: error from cdrom_open\n");
		return;
	}

	if (cdi.state & CDROM_STATE_PLAY) {
		XBell(XtDisplay(widget), 100);
		if (cdi.scsi_audio) {
			audio_button_set();
		} else {
			audio_button_reset();
		}
		return;
	}

	if (cdi.scsi_audio) {
		debug_printf(1, "cb_cdrom_audio: toggling audio off\n");
		cdrom_toggle_audio();
		audio_button_reset();
		return;
	}

	/* Check if it's available before turning it on */
	if ( cdrom_audio_avail() ) {
		debug_printf(1, "cb_cdrom_audio: toggling audio on\n");
		cdrom_toggle_audio();
		audio_button_set();
		return;
	}
	XBell(XtDisplay(widget), 100);
	audio_button_reset();
}
#endif /* sgi */

/*ARGSUSED*/
void
cb_cdrom_cycle(widget, client_data, call_data)
	Widget		widget;
	XtPointer	client_data;
	XtPointer	call_data;
{
	if (cdi.state & CDROM_STATE_CYCLE) {
		debug_printf(1, "cb_cdrom_cycle: cycle off\n");
		cdi.state &= ~CDROM_STATE_CYCLE;

		return;
	}

	debug_printf(1, "cb_cdrom_cycle: cycle on\n");
	cdi.state |= CDROM_STATE_CYCLE;
}

/*ARGSUSED*/
void
cb_cdrom_shuffle(widget, client_data, call_data)
	Widget		widget;
	XtPointer	client_data;
	XtPointer	call_data;
{
	if (cdi.state & CDROM_STATE_SHUFFLE) {

		debug_printf(1, "cb_cdrom_shuffle: shuffle off\n");
		cdi.state &= ~CDROM_STATE_SHUFFLE;

		return;
	}

	if (cdi.state & CDROM_STATE_PLAY) {
		XBell(XtDisplay(widget), 100);
		shuffle_button_reset();
		return;
	}

	if (cdi.state & CDROM_STATE_PROGRAM) {
		debug_printf(1, "cb_cdrom_shuffle: cancelling program\n");
		program_cancel();
	}

	debug_printf(1, "cb_cdrom_shuffle: shuffle on\n");
	cdi.state |= CDROM_STATE_SHUFFLE;
	shuffle_setup();
}

/*ARGSUSED*/
void
cb_cdrom_quit(widget, client_data, call_data)
	Widget		widget;
	XtPointer	client_data;
	XtPointer	call_data;
{
	debug_printf(1, "cb_cdrom_quit: bye\n");

	cdrom_timer_off();
	cdrom_close();

	exit(0);
}


/*ARGSUSED*/
void
cb_cdrom_rewind(widget, client_data, call_data)
	Widget		widget;
	XtPointer	client_data;
	XtPointer	call_data;
{
	if (cdrom_open() == -1) {
		debug_printf(1, "cb_cdrom_rew: error from cdrom_open\n");
		return;
	}

	if ((cdi.state & CDROM_STATE_PLAY) || (cdi.state & CDROM_STATE_PAUSE)) {
		debug_printf(1, "cb_cdrom_rew: moving backward in track %d\n",
			    cdi.curtrack);

		cdrom_rewind();
	}
}


/*ARGSUSED*/
void
cb_cdrom_ff(widget, client_data, call_data)
	Widget		widget;
	XtPointer	client_data;
	XtPointer	call_data;
{
	if (cdrom_open() == -1) {
		debug_printf(1, "cb_cdrom_ff: error from cdrom_open\n");
		return;
	}


	if ((cdi.state & CDROM_STATE_PLAY) || (cdi.state & CDROM_STATE_PAUSE)) {
		debug_printf(1, "cb_cdrom_ff: moving forward in track %d\n",
			    cdi.curtrack);

		cdrom_ff();
	}
}

static Boolean pgmButtonUp = False;

/*ARGSUSED*/
void
cb_cdrom_program(widget, topLevel, call_data)
	Widget		widget;
	Widget		topLevel;
	XtPointer	call_data;
{
	if (pgmButtonUp)
	{
	    track_button_reset();
	    pgm_button_set();

	    /*
             * have to keep track of button position internally, since toggle
             * widgets screw up when you mess with their state programatically,
             * in that if you set the state to false when notified on a button
             * press, if you also notify on button release, the state is
             * auto-reset to true prior to notification, so there is no
             * longer any way to tell if it's a button release:
             * THIS IS A HACK! (necessary, but *still* a hack.)
             */
            pgmButtonUp = False;

	    return;
	}

	if ((cdi.state & CDROM_STATE_PROGRAM) == 0)
	{
	    if (cdrom_open() == -1) {
		debug_printf(1, "cb_cdrom_play: error from cdrom_open\n");
		return;
	    }
	    if (cdi.state & CDROM_STATE_EJECTED) {
		    cdrom_new_disc(widget);
	    }
	    if (cdi.state & CDROM_STATE_SHUFFLE) {
	        debug_printf(1, "cb_cdrom_program: cancelling shuffle mode\n");
	        cdi.state &= ~CDROM_STATE_SHUFFLE;
		shuffle_button_reset();
	    }
	    cdi.state &= ~CDROM_STATE_EJECTED;
	    eject_button_reset();
	    cdi.state &= ~CDROM_STATE_STOP;

	    debug_printf(1, "cb_cdrom_program: program on\n");
	    cdi.state |= CDROM_STATE_PROGRAM;
	    timer_button_set();
	    popup_program_form (widget, topLevel, call_data);
            /*
             * have to keep track of button position internally, since command
             * widgets screw up if you mess with their state programatically:
             */
            pgmButtonUp = True;
	    return;
	}

	if (((cdi.state & CDROM_STATE_PAUSE) || 
	     ((cdi.state & CDROM_STATE_PLAY) == 0)) &&
	    (cdi.curtrack != 0))
	{
	    /* indicate to user he's programmed a track: */
	    pgm_button_reset();
	    track_button_set();
	    timer_button_update();

	    debug_printf(1, "cb_cdrom_program: adding track %d to program list\n",
		cdi.curtrack);
	    program_add_track (cdi.curtrack);
	}
        /*
         * have to keep track of button position internally, since command
         * widgets screw up if you mess with their state programatically:
         */
        pgmButtonUp = True;

}


void
cdrom_new_disc(widget)
	Widget	widget;
{
	Arg	args[1];
	Boolean	state;

	debug_printf(1, "cdrom_new_disc: resetting disc\n");

	cdrom_reset();
	
	cdi.state &= ~CDROM_STATE_EJECTED;
	eject_button_reset();

	/* toggle button weirdness; see comment in cb_cdrom_play for details */
	XtSetArg(args[0], XtNstate, &state);
	XtGetValues(widget, args, 1);
	if (state == False) {
		XtSetArg(args[0], XtNstate, True);
		XtSetValues(widget, args, 1);
	}

	if (cdi.state & CDROM_STATE_SHUFFLE) {
		debug_printf(1, "cb_cdrom_shuffle: shuffle on\n");
		cdi.state |= CDROM_STATE_SHUFFLE;
		shuffle_setup();
	}
}
