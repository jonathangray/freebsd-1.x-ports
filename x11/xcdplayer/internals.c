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
#ifdef sun
# include "cdrom_sun.h"
#endif
#ifdef sgi
# include "cdrom_sgi.h"
#endif


static XtIntervalId	ivid			= -1;
static XtIntervalId	scanivid		= -1;
static XtIntervalId	stativid		= -1;
static unsigned int	timer_mod		= 1000 / TIMER_PERIOD;
unsigned int		timer_fsecs;


int
cdrom_get_curtime() {

	int curtime;
	int curtrack;
	struct msf curmsf, track_start;

	if (cdrom_open() == -1) {
		debug_printf(1, "cdrom_get_curtime: error from cdrom_open\n");
		return;
	}

	switch (cdrom_status()) {
	case CDROM_PAUSED:
	case CDROM_PLAYING:
	    if (cdrom_get_curmsf(&curmsf) == -1) {
		debug_printf(1, "get_curtime: error reading location\n");
		return;
	    }

	    if (((curtrack = cdrom_get_curtrack()) == -1) ||
		(curtrack != cdi.curtrack))
	    {
		return(0);
	    }
	    else
	    {
	        track_start = cdi.addrs[cdi.curtrack - 1];
	        curtime = (curmsf.minute - track_start.minute) * 60 +
			(curmsf.second - track_start.second) + 1;

	        return (curtime);
	    }
	    break;
	default:
	    return(0);
	}
}


/*
 * we poll the cd-rom drive every TIMER_PERIOD milliseconds to see where 
 * it is and if it's on a new track, and update the label widget.
 */
void
cdrom_timer_on() {
	static void	update_track();

	if (cdi.state & CDROM_STATE_PLAY)
		ivid = XtAppAddTimeOut(appc, TIMER_PERIOD, update_track, NULL);
}


/*
 * cdrom_timer_off turns off the X timeout.
 */
void
cdrom_timer_off() {
	if (ivid != -1) {
		XtRemoveTimeOut(ivid);
		ivid = -1;
	}
}

/*
 * activates player by track/index: 
 */
int
cdrom_play() {
	int	ret;

	debug_printf(1, "cdrom_play: starting track %d\n", (int) cdi.curtrack);

	track_button_update();

	if ((cdi.state & CDROM_STATE_SHUFFLE) ||
	    (cdi.state & CDROM_STATE_PROGRAM))
		ret = cdrom_play_track(cdi.curtrack, cdi.curtrack);
	else
		ret = cdrom_play_track(cdi.curtrack, cdi.maxtrack);

	cdrom_timer_on();

	return(ret);
}


/*
 * resets player to disc origin and turns off all modes:
 */
void
cdrom_reset() {

	cdrom_timer_off();

	if (cdi.curtrack != 0)
	{
	    cdi.curtrack = 1;
	    (void) cdrom_play(); /* force disk to beginning */

	    /* 
	     * "pause disk" immediately; doesn't appear that you can truly
	     * stop the disk at origin; just stop at current location...
	     */
	    cdrom_stop();
	    cdi.state |= CDROM_STATE_STOP;
	    debug_printf(1, "cdrom_reset: forcing immediate stop\n");
	    leds_stop();
	    cdrom_timer_off();
	    cdi.curtrack = 0;
	}

	cdi.state &= ~(CDROM_STATE_PLAY | CDROM_STATE_PAUSE | 
				CDROM_STATE_EJECTED);

	update_title();

	play_button_reset();
	pause_button_reset();
	timer_button_reset();

	track_button_update();
	timer_button_update();
}

/*
 * rewinds the player through minute/second/frames:
 */
void
cdrom_rewind () {
	struct msf	track_start;
        struct msf	start_addr, end_addr;
	int		curtrack;
	extern void 	scan_update();

	if (cdrom_get_curmsf(&start_addr) == -1) {
		debug_printf(1, "rew: error reading location\n");
		return;
	}	

	curtrack = cdrom_get_curtrack();

	if (curtrack != cdi.curtrack) 
		track_button_update();

	/* find start of current track: */
	track_start = cdi.addrs[curtrack - 1];

	/* 
	 * deal with cases where we can't back up beyond the current track: 
	 */
	if ((curtrack == cdi.mintrack)  || 
	    (cdi.state & CDROM_STATE_SHUFFLE) ||
	    (cdi.state & CDROM_STATE_PROGRAM))
	{
	    /* Ugh, this is ugly... (drich) */
	    if (((cdi.state & CDROM_STATE_PLAY) && 
		 ((track_start.minute * 60) + track_start.second) >=
		 ((start_addr.minute * 60) + start_addr.second - 
		  scanSkipInterval)) ||
		((cdi.state & CDROM_STATE_PAUSE) &&
		 ((track_start.minute * 60) + track_start.second) >=
		 ((start_addr.minute * 60) + start_addr.second -
		  pauseSkipInterval)))
	    {
	        start_addr = track_start;
	        start_addr.second++; /* guarantee we never back up too far */
	    }
	    else
	    {
		if (cdi.state & CDROM_STATE_PAUSE) {
			start_addr.second -= pauseSkipInterval; 
		} else if (cdi.state & CDROM_STATE_PLAY) {
			start_addr.second -= scanSkipInterval; 
		}
	        if ((char) start_addr.second < 0)
	        {
		    start_addr.minute--;
		    start_addr.second = 60 + (char) start_addr.second; 
	        }
	    }

	}
	else /* normal case */
	{
		if (cdi.state & CDROM_STATE_PAUSE) {
			start_addr.second -= pauseSkipInterval; 
		} else if (cdi.state & CDROM_STATE_PLAY) {
			start_addr.second -= scanSkipInterval; 
		}
		if ((char) start_addr.second < 0)
		{
			start_addr.minute--;
			start_addr.second = 60 + (char) start_addr.second; 
		}
	}
	    
	if ((cdi.state & CDROM_STATE_PROGRAM) || 
	    (cdi.state & CDROM_STATE_SHUFFLE))
	{

	    /* then to end of current selection (start of next track - 1 sec) */
	    end_addr = cdi.addrs[cdi.curtrack]; 
	    end_addr.second--; 		
	    if ((char) end_addr.second < 0)
	    {
		end_addr.minute--;
		end_addr.second = 59;
	    }
	}
	else
	{

	    /*
	     * to end of last track; array 0-based, so really index for 
	     * leadout addr:
	     */
	    end_addr = cdi.addrs[cdi.maxtrack];
	    end_addr.second--; /* go to last second */
	    if ((char) end_addr.second < 0)
	    {
		end_addr.minute--;
		end_addr.second = 59;
	    }
	}

	if ((start_addr.minute == end_addr.minute) && 
	    (start_addr.second == end_addr.second))
	    end_addr.frame = start_addr.frame;

	cdrom_play_msf (&start_addr, &end_addr);

	timer_fsecs = 0;
	if (cdi.state & CDROM_STATE_PAUSE)
	{
	    if (scanivid == -1)
		scanivid = XtAppAddTimeOut(appc, 
			(int)(pausePauseInterval * 1000.0), 
			scan_update, NULL);

	    cdi.state &= ~CDROM_STATE_PAUSE;	/* allow timer to change */
	    timer_button_update();
	    cdi.state |= CDROM_STATE_PAUSE;	/* restore true state */

	    if (cdrom_pause() != -1)		/* re-pause */
	       cdi.state &= ~CDROM_STATE_STOP;

	}
	else
	{
	    if (scanivid != -1) {
		XtRemoveTimeOut(scanivid);
		ivid = -1;
	    }
	    timer_button_update();
	}

}

/*
 * fast-forwards the player through minute/second/frames:
 */
void
cdrom_ff () {
        struct msf	start_addr, end_addr,  next_start;
	char		t;
	int		curtrack;
	extern void 	scan_update();

	if (cdrom_get_curmsf(&start_addr) == -1) {
		debug_printf(1, "ff: error reading location\n");
		return;
	}	

	curtrack = cdrom_get_curtrack();

	if (curtrack != cdi.curtrack) 
		track_button_update();

	/* find start of next track */
	next_start = cdi.addrs[curtrack];
	    
	/* 
	 * deal with cases where we can't fast forward beyond the current 
	 * track: 
	 */
	if ((curtrack == cdi.maxtrack)  || 
	    (cdi.state & CDROM_STATE_SHUFFLE) ||
	    (cdi.state & CDROM_STATE_PROGRAM))
	{
	    /* see if skipping ahead will go beyond the current track: */
	    /* Ugh, this is ugly... (drich) */
	    if (((cdi.state & CDROM_STATE_PLAY) && 
		 ((next_start.minute * 60) + next_start.second) <=
		 ((start_addr.minute * 60) + start_addr.second +
		  scanSkipInterval)) ||
		((cdi.state & CDROM_STATE_PAUSE) &&
		 ((next_start.minute * 60) + next_start.second) <=
		 ((start_addr.minute * 60) + start_addr.second +
		  pauseSkipInterval)))
	    {
	    
	        /* start at end of current track */
	        start_addr = next_start;
	        start_addr.second--;
	    }
	    else
	    {
		    if (cdi.state & CDROM_STATE_PAUSE) {
			    start_addr.second += pauseSkipInterval; 
		    } else if (cdi.state & CDROM_STATE_PLAY) {
			    start_addr.second += scanSkipInterval; 
		    }
		    if (start_addr.second >= 60)
		    {
			    start_addr.minute++;
			    start_addr.second = start_addr.second - 60;
		    }
	    }
	}
	else
	{
	    if (cdi.state & CDROM_STATE_PAUSE) {
		start_addr.second += pauseSkipInterval; 
	    } else if (cdi.state & CDROM_STATE_PLAY) {
		start_addr.second += scanSkipInterval; 
	    }
	    if (start_addr.second >= 60)
	    {
		start_addr.minute++;
		start_addr.second = start_addr.second - 60;
	    }
	}

	if ((cdi.state & CDROM_STATE_PROGRAM) || 
	    (cdi.state & CDROM_STATE_SHUFFLE))
	{

	    /* then to end of current selection */
	    end_addr = next_start;	/* use start of next */
	    end_addr.second--; 		/* and back off 1 second */
	    if ((char) end_addr.second < 0)
	    {
		end_addr.minute--;
		end_addr.second = 59;
	    }
	}
	else
	{
	    /* 
	     * "to end of last track"; array 0-based, so really index for 
	     * leadout addr 
	     */
	    end_addr = cdi.addrs[cdi.maxtrack];

	    end_addr.second--; /* (you can't play the leadout) */
	    if ((char) end_addr.second < 0)
	    {
		end_addr.minute--;
		end_addr.second = 59;
	    }
	}

	if ((start_addr.minute == end_addr.minute) && 
	    (start_addr.second == end_addr.second))
	{
	    start_addr.frame = end_addr.frame = 0;
	}

	cdrom_play_msf (&start_addr, &end_addr);

	timer_fsecs = 0;
	if (cdi.state & CDROM_STATE_PAUSE)
	{
	    if (scanivid == -1)
		scanivid = XtAppAddTimeOut(appc, 
			(int)(pausePauseInterval * 1000.0), 
			scan_update, NULL);

	    cdi.state &= ~CDROM_STATE_PAUSE;	/* allow timer to change */
	    timer_button_update();
	    cdi.state |= CDROM_STATE_PAUSE;	/* restore true state */

	    if (cdrom_pause() != -1)		/* re-pause */
	       cdi.state &= ~CDROM_STATE_STOP;
	}
	else
	{
	    if (scanivid != -1) {
		XtRemoveTimeOut(scanivid);
		ivid = -1;
	    }
	    timer_button_update();
	}
}


/*
 * called by update_track when the cd has hit
 * the end of the track or the disc.
 */
static void
cdrom_atend() {
	cdrom_timer_off();
	leds_stop();
	debug_printf(1, "cdrom_atend: at end\n");

#ifdef sgi
	/* Force a stop to kill the child (if any).
	 * This is due to some sort of weirdness when the SGI runs off the 
	 * disc during a CDreadda().
	 */
	if (cdi.scsi_audio) {
		cdrom_stop();
	}
#endif
	if (cdi.state & CDROM_STATE_SHUFFLE) {

		if (cdi.currand == cdi.ntracks) {
			if ((cdi.state & CDROM_STATE_CYCLE) == 0) {
				debug_printf(1, "cdrom_atend: shuffle done\n");

				cdrom_reset();
				return;
			}

			debug_printf(1, "cdrom_atend: shuffle cycling\n");
			shuffle_setup();
		}

		cdi.curtrack = shuffle_next_track();
	}
	else if (cdi.state & CDROM_STATE_PROGRAM) {
		if ((cdi.curtrack = program_goto_next_track()) == 0)
		{
		    if (cdi.state & CDROM_STATE_CYCLE)
		    {
		        debug_printf(1, "cdrom_atend: cycling program\n");
			cdi.curtrack = program_resume();
			timer_fsecs = 0;
			cdi.duration = 0;
			timer_button_update();

			(void) cdrom_play();
		    }
		    else
		    {
		        debug_printf(1, "cdrom_atend: all done\n");
		    	cdrom_reset();
		    }
		    return;
		}
	}
			
	else if ((cdi.curtrack < cdi.maxtrack) && (cdi.curtrack != 0)) {
		debug_printf(1, "cdrom_atend: continuing\n");
		cdi.curtrack++;
	}
	else if (cdi.state & CDROM_STATE_CYCLE) {
		debug_printf(1, "cdrom_atend: cycling\n");
		cdi.curtrack = cdi.mintrack;
	}
	else {
		debug_printf(1, "cdrom_atend: all done\n");
		buttons_reset();
		cdrom_reset();
		return;
	}

	timer_fsecs = 0;
	cdi.duration = 0;
	timer_button_update();

	(void) cdrom_play();
}

/*
 * scan_update is called when the scan timeout fires; it updates the timer 
 * and calls the label update routine.
 */
/*ARGSUSED*/
static void
scan_update(data, id)
	XtPointer	*data;
	XtIntervalId	*id;
{
	unsigned int	curtrack;
	Arg	args[1];
	Boolean	state;

	extern Widget	rew_button_widget;
	extern Widget	ff_button_widget;

	if ((curtrack = cdrom_get_curtrack()) != cdi.curtrack) {
		if (curtrack == 0) {
			cdrom_atend();

			return;
		}

		timer_fsecs = 0;
		cdi.duration = 0;
		timer_button_update();

		cdi.curtrack = curtrack;
		track_button_update();
	}


	XtSetArg(args[0], XtNstate, &state);
	XtGetValues(rew_button_widget, args, 1);
	if (state == True)
	{
		cdrom_rewind();
		leds_update(BACKWARDS);
		if (cdi.state & CDROM_STATE_PAUSE) {
			if (cdrom_pause() != -1)
				 cdi.state &= ~CDROM_STATE_STOP;
		}
	        ivid = XtAppAddTimeOut(appc, 
			(int)(scanPauseInterval * 1000.0), 
			scan_update, NULL);
	}
	else
	{
		leds_update(FORWARDS);
		XtSetArg(args[0], XtNstate, &state);
		XtGetValues(ff_button_widget, args, 1);
		if (state == True)
		{
			cdrom_ff();
			if (cdi.state & CDROM_STATE_PAUSE) {
				if (cdrom_pause() != -1)
					 cdi.state &= ~CDROM_STATE_STOP;
			}

			ivid = XtAppAddTimeOut(appc,
				(int)(scanPauseInterval * 1000.0),
				scan_update, NULL);
		}
		else if (scanivid != -1) {
			XtRemoveTimeOut(scanivid);
			scanivid = -1;
		}
	}
}


/*
 * update_status is called when the status timeout fires;  it maintains
 * the disc status, and will detect when a new disc has been inserted.
 */
/*ARGSUSED*/
void
update_status(data, id)
	XtPointer	*data;
	XtIntervalId	*id;
{
	if (cdi.state & CDROM_STATE_EJECTED) {
		if (cdrom_open() == -1) {
			debug_printf(1, "cdrom_open: cdrom not ready\n");
		} else {
			cdi.state &= ~CDROM_STATE_EJECTED;
			buttons_reset();
		}
	}
	
	/* Reset timer */
	stativid = XtAppAddTimeOut(appc, 1000, update_status, NULL);
}

/*
 * update_track is called when the timeout fires; it updates curtrack and 
 * calls the label update routine.
 */
/*ARGSUSED*/
static void
update_track(data, id)
	XtPointer	*data;
	XtIntervalId	*id;
{
	unsigned int	curtrack;
	Arg	args[1];
	Boolean	state;

	extern Widget	rew_button_widget;
	extern Widget	ff_button_widget;
#ifdef sgi

	int	vol;
#endif

	if ((curtrack = cdrom_get_curtrack()) != cdi.curtrack) {
		if (curtrack == 0) {
			cdrom_atend();

			return;
		}

		timer_fsecs = 0;
		cdi.duration = 0;
		timer_button_update();

		cdi.curtrack = curtrack;
		track_button_update();
	}

#ifdef sgi
	/* Update the volume control */
	if ((vol = cdrom_get_volume()) != 0) {
		set_volume(vol);
	}
#endif

	XtSetArg(args[0], XtNstate, &state);
	XtGetValues(rew_button_widget, args, 1);
	if (state == True)
	{
		cdrom_rewind();
		leds_update(BACKWARDS);
		if (cdi.state & CDROM_STATE_PAUSE) {
			if (cdrom_pause() != -1)
				 cdi.state &= ~CDROM_STATE_STOP;
		}
	        ivid = XtAppAddTimeOut(appc, 
			(int)(scanPauseInterval * 1000.0), 
			update_track, NULL);
	}
	else
	{
		leds_update(FORWARDS);
		XtSetArg(args[0], XtNstate, &state);
		XtGetValues(ff_button_widget, args, 1);
		if (state == True)
		{
			cdrom_ff();
			if (cdi.state & CDROM_STATE_PAUSE) {
				if (cdrom_pause() != -1)
					 cdi.state &= ~CDROM_STATE_STOP;
			}

			ivid = XtAppAddTimeOut(appc,
				(int)(scanPauseInterval * 1000.0),
				update_track, NULL);
		}
		else
		{
			if ((timer_fsecs++ % timer_mod) == 0) {
				timer_button_update();
			}
			ivid = XtAppAddTimeOut(appc, TIMER_PERIOD, 
				update_track, NULL);
		}
	}
}

