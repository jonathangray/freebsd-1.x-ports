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
# include <X11/Shell.h>
# include <X11/Xaw/Form.h>
# include <X11/Xaw/Label.h>
# include <X11/Xaw/Command.h>
# include <X11/Xaw/Toggle.h>
# include <X11/Xaw/Scrollbar.h>
# include <X11/Xaw/Dialog.h>
# include <X11/Xaw/Viewport.h>

# include <math.h>
# include <stdio.h>

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

# include "logo.xbm"
# include "thumb.xbm"
# include <X11/bitmaps/gray>

# define MAXVOL		0xff

void	Done();

static Widget	track_button_widget;
static Widget	timer_button_widget;
static Widget	volume_scroll_widget;

Widget		title_viewport;
Widget		title_form;
Widget		title_button;
Widget		title_dialog_shell;
Widget		title_dialog_widget;
Widget		title_done_widget;

static int	vol;
extern void     leds_label_setup();
static void     cb_track_button();
static void     cb_timer_button();
static void     volume_jump_proc();
static void     volume_scroll_proc();
static void     popup_title_dialog();
static void     popdown_title_dialog();


void
logo_setup(parent_widget)
	Widget		parent_widget;
{
	Widget		version_label_widget;
	char		version_string[80];

	Widget		logo_form_widget;
	Widget		logo_label_widget;
	Pixmap		logo_label_pixmap;
	Pixmap		thumb_pixmap;
	Pixmap		gray;
	int			depth;
	int		length;
	

	Arg		args[4];
	static XtActionsRec	done[] = {
		{"done", Done},
	};


	logo_form_widget = XtCreateManagedWidget("logoForm", formWidgetClass,
						 parent_widget,
						 (ArgList) NULL, 0);

	logo_label_widget = XtCreateManagedWidget("logoLabel",
						  labelWidgetClass,
						  logo_form_widget,
						  (ArgList) NULL, 0);

	logo_label_pixmap = XCreateBitmapFromData(XtDisplay(logo_label_widget),
						  rootwin(logo_label_widget),
						  logo_bits,
						  logo_width, logo_height);

	sprintf(version_string,"XCdplayer %s%d",VERSION,PATCHLEVEL);
	if (BETA != 0) {
		sprintf(version_string,"%s beta %d",version_string,BETA);
	}
	XtSetArg(args[0], XtNjustify, XtJustifyRight);
	XtSetArg(args[1], XtNlabel, version_string);
	version_label_widget = XtCreateManagedWidget("versionLabel",
						     labelWidgetClass,
						     logo_form_widget,
						     (ArgList) args, 2);

	XtSetArg(args[0], XtNbitmap, logo_label_pixmap);
	XtSetValues(logo_label_widget, args, 1);

	leds_label_setup(logo_form_widget);


	track_button_widget = XtCreateManagedWidget("trackButton",
						    toggleWidgetClass,
						    logo_form_widget,
						    (ArgList) NULL, 0);

	XtAddCallback(track_button_widget, XtNcallback, cb_track_button, 0);

	track_button_update();

	if (display_timer == True)
		timer_button_widget = XtCreateManagedWidget("timerButton",
						    	   toggleWidgetClass,
							   logo_form_widget,
							   (ArgList) NULL, 0);

	XtAddCallback(timer_button_widget, XtNcallback, cb_timer_button, 0);

	timer_button_update();
	thumb_pixmap = XCreateBitmapFromData(XtDisplay(logo_form_widget),
						  rootwin(logo_form_widget),
						  thumb_bits,
						  thumb_width, thumb_height);

    XtSetArg (args[0], XtNdepth, &depth);
    XtGetValues (logo_form_widget, args, 1);

	if (depth == 1) {
	    gray = XCreateBitmapFromData(XtDisplay(logo_form_widget),
						  rootwin(logo_form_widget),
						  gray_bits,
						  gray_width, gray_height);

	    XtSetArg(args[0], XtNbackgroundPixmap, gray);
	    XtSetArg(args[1], XtNthumb, thumb_pixmap);
	    volume_scroll_widget = XtCreateManagedWidget("volumeScrollbar",
						     scrollbarWidgetClass,
						     logo_form_widget,
						     (ArgList) args, 2);
	}
	else
	{
	     /*XtSetArg(args[0], XtNthumb, thumb_pixmap);*/

	     volume_scroll_widget = XtCreateManagedWidget("volumeScrollbar",
						     scrollbarWidgetClass,
						     logo_form_widget,
						     (ArgList) args, 0);
	}

	XtAddCallback(volume_scroll_widget, XtNjumpProc, volume_jump_proc, 0);
	XtAddCallback(volume_scroll_widget, XtNscrollProc, volume_scroll_proc, 0);

#ifdef sgi
	if ((vol = cdrom_get_volume()) == 0) {
		vol = (int) ((MAXVOL - volbase) * 0.75) + volbase;
	}
#else
	vol = (int) ((MAXVOL - volbase) * 0.75) + volbase;
#endif
	cdrom_volume(vol, vol); 
#ifdef sgi
	XawScrollbarSetThumb(volume_scroll_widget, 
			     (float) (VAL2PCT(vol)),
			     (float) 1.0);
#else
	XawScrollbarSetThumb(volume_scroll_widget, (float) 0.75, (float) 1.0);
#endif

	title_viewport = XtCreateManagedWidget ("titleBarViewport",
				viewportWidgetClass,
				logo_form_widget,
				(ArgList) NULL, 0);

	title_form = XtCreateManagedWidget ("titleForm",
				formWidgetClass,
				title_viewport,
				(ArgList) NULL, 0);

	if (disc_title != NULL) 
        	XtSetArg(args[0], XtNlabel, disc_title);
	else
#ifdef sgi
		if (cdrom_status() == CDROM_NO_STATUS) {
			XtSetArg(args[0], XtNlabel, NODISCSTR);
		} else {
			XtSetArg(args[0], XtNlabel, NOTITLESTR);
		}
#else
		XtSetArg(args[0], XtNlabel, NOTITLESTR);
#endif

	title_button = XtCreateManagedWidget ("titleBar",
				commandWidgetClass,
				title_form,
				(ArgList) args, 1);

	title_dialog_shell = XtCreatePopupShell("titleShell", 
				transientShellWidgetClass,
				title_button,
				(ArgList) NULL, 0);

        XtSetArg(args[0], XtNlabel, "Title: ");
        title_dialog_widget = XtCreateManagedWidget("titleDialog",
                                	dialogWidgetClass,
                                	title_dialog_shell,
                                	(ArgList) args, 1);

	title_done_widget = XtCreateManagedWidget("Done",
					commandWidgetClass,
					title_dialog_widget,
					(ArgList) NULL, 0);
	
	XtAppAddActions(appc, done, XtNumber(done));

	XtAddCallback(title_button, XtNcallback, popup_title_dialog,
			(XtPointer) logo_form_widget);
	XtAddCallback(title_done_widget, XtNcallback, popdown_title_dialog,
			(XtPointer) title_dialog_widget);


}

/*ARGSUSED*/
static void
cb_track_button(widget, client_data, call_data)
	Widget		widget;
	XtPointer	client_data;
	XtPointer	call_data;
{
	char		track_buf[40];
	Arg		args[1];
	Boolean		state;

	XtSetArg(args[0], XtNstate, &state);
	XtGetValues(widget, args, 1);

#ifdef sgi
	if (cdrom_status() == CDROM_NO_STATUS) {
		cdi.state |= CDROM_STATE_EJECTED;
		buttons_reset();
		return;
	}
#endif

	if (cdi.state & CDROM_STATE_EJECTED) {
		cdrom_new_disc();
	}

	if (state == True)
		sprintf(track_buf, "%d\n", cdi.maxtrack);
	else
		sprintf(track_buf, "%d\n", cdi.curtrack);

	XtSetArg(args[0], XtNlabel, track_buf);
	XtSetValues(widget, args, 1);
}

void
track_button_update() {
	char		track_buf[40];
	Arg		args[1];

	sprintf(track_buf, "%d\n", cdi.curtrack);

	XtSetArg(args[0], XtNlabel, track_buf);
	XtSetValues(track_button_widget, args, 1);
}

static void
cb_timer_button(widget, client_data, call_data)
	Widget		widget;
	XtPointer	client_data;
	XtPointer	call_data;
{
	char		timer_buf[40];
	int		time_remaining;
	Arg		args[1];
	Boolean		state;

	if (cdi.state & CDROM_STATE_EJECTED) {
		cdrom_new_disc();
	}

#ifdef sgi
	if (cdrom_status() == CDROM_NO_STATUS) {
		cdi.state |= CDROM_STATE_EJECTED;
		buttons_reset();
		return;
	}
#endif

	cdi.duration = cdrom_get_curtime();

	XtSetArg(args[0], XtNstate, &state);
	XtGetValues(widget, args, 1);

	if ((state == True) && (cdi.curtrack != 0))
	{
	    if (((cdi.state & CDROM_STATE_PAUSE) == 0) &&
	        (cdi.state & CDROM_STATE_PROGRAM))
	    {
	        time_remaining = program_time_remaining();
	    }
	    else
	        time_remaining = cdi.times[cdi.curtrack - 1] - cdi.duration;

	    if (time_remaining < 0)
	        sprintf(timer_buf, "-%02u:%02u\n",abs(time_remaining) / 60, 
		    abs(time_remaining) % 60);
	    else
	        sprintf(timer_buf, "-%02d:%02d\n", time_remaining / 60, 
		    time_remaining % 60);
	} else if (cdi.curtrack != 0) {
		if (cdi.duration < 0) 
			sprintf(timer_buf, "-%02u:%02u\n", cdi.duration / 60, 
				abs(cdi.duration) % 60);
		else
			sprintf(timer_buf, "%02u:%02u\n", cdi.duration / 60, 
				cdi.duration % 60);
	} else {
		if ((state == True) && (cdrom_status() != CDROM_NO_STATUS))
			sprintf(timer_buf, "-%02u:%02u\n", 
				cdi.addrs[cdi.maxtrack].minute, 
				cdi.addrs[cdi.maxtrack].second);
		else
			sprintf(timer_buf, "--:--\n");
	}

	XtSetArg(args[0], XtNlabel, timer_buf);
	XtSetValues(widget, args, 1);
}

void
timer_button_update() {
	char		timer_buf[40];
	int		time_remaining;
	Arg		args[1];
	Boolean		state;

	if (display_timer == False)
		return;

	    if ((cdi.state & CDROM_STATE_PAUSE) == 0)
	        cdi.duration = cdrom_get_curtime();

	    XtSetArg(args[0], XtNstate, &state);
	    XtGetValues(timer_button_widget, args, 1);

	    if ((state == True) && (cdi.curtrack != 0))
	    {
		if (((cdi.state & CDROM_STATE_PAUSE) == 0) &&
		    (cdi.state & CDROM_STATE_PROGRAM))
		{
	            time_remaining = program_time_remaining();
		}
		else
	            time_remaining = cdi.times[cdi.curtrack - 1] - cdi.duration;
	        if (time_remaining < 0)
	            sprintf(timer_buf, "-%02u:%02u\n",abs(time_remaining) / 60, 
		        abs(time_remaining) % 60);
	        else
	            sprintf(timer_buf, "-%02d:%02d\n", time_remaining / 60, 
		        time_remaining % 60);
	    }
	    else if (cdi.state & CDROM_STATE_PLAY)
	    {
	        if (cdi.duration < 0)
	            sprintf(timer_buf, "-%02u:%02u\n", cdi.duration / 60, 
		        abs(cdi.duration) % 60);
	        else
	            sprintf(timer_buf, "%02u:%02u\n", cdi.duration / 60, 
		        cdi.duration % 60);
	    }
	    else
		if ((state == True) && (cdrom_status() != CDROM_NO_STATUS))
			sprintf(timer_buf, "-%02u:%02u\n", 
				cdi.addrs[cdi.maxtrack].minute, 
				cdi.addrs[cdi.maxtrack].second);
		else
			sprintf(timer_buf, "--:--\n");

	XtSetArg(args[0], XtNlabel, timer_buf);
	XtSetValues(timer_button_widget, args, 1);
}

static void
volume_jump_proc(scroll_widget, client_data, percent)
	Widget		scroll_widget;
	XtPointer	client_data;
	XtPointer	percent;
{
#ifdef sgi
	/* The volume control on SGI is not linear, but is exponential */
	vol = PCT2VAL(*(float *)percent);
#else
	vol = (*(float *) percent) * MAXVOL;
	vol = (vol * volpcent) + volbase;
#endif

	if (vol > MAXVOL)
		vol = MAXVOL;
	if (vol <= 0)
		vol = 1;

	debug_printf(1, "volume=%u\n", (unsigned int) vol);

	cdrom_volume(vol, vol);
}

static void
volume_scroll_proc(scroll_widget, client_data, position)
	Widget		scroll_widget;
	XtPointer	client_data;
	XtPointer	position;
{
	Arg		args[1];
	Dimension	length;
	float		top;
	double		abspos;
	double		percent;
	double		vdelta;

	if ((abspos = (int) position) < 0)
		abspos = -abspos;

	XtSetArg(args[0], XtNlength, &length);
	XtGetValues(scroll_widget, args, 1);

	if (length <= 0)
		length = 1;

	percent = abspos / (float) length;

#ifdef sgi
	/* The volume control on SGI is not linear, but is exponential */
	if ((vdelta = ((VAL2PCT(vol)) * percent)) < (VAL2PCT(1))) {
		vdelta = (VAL2PCT(1));
	}
	if ((int) position < 0)
		vol = (PCT2VAL((VAL2PCT(vol)) - vdelta));
	else
		vol = (PCT2VAL((VAL2PCT(vol)) + vdelta));
#else
	if ((vdelta = (vol * percent)) < 1.0)
		vdelta = 1.0;

	if ((int) position < 0)
		vol += vdelta;
	else
		vol -= vdelta;
#endif

	if (vol > MAXVOL)
		vol = MAXVOL;
	if (vol <= 0)
		vol = 1;

	debug_printf(1, "volume=%u\n", (unsigned int) vol);

	cdrom_volume(vol, vol);

#ifdef sgi
	XtSetArg (args[0], XtNlength, &length);
	XtGetValues (volume_scroll_widget, args, 1);
	XawScrollbarSetThumb(volume_scroll_widget, 
			     (float) (VAL2PCT(vol)),
			     (float) -1.0);
#endif
	top = (double) vol / (double) MAXVOL;

	XawScrollbarSetThumb(volume_scroll_widget, (float) top, (float) -1.0);
}

void
track_button_set() {
	Arg             args[1];

	XtSetArg(args[0], XtNstate, True);
	XtSetValues(track_button_widget, args, 1);
}

void
track_button_reset() {
	Arg             args[1];

	XtSetArg(args[0], XtNstate, False);
	XtSetValues(track_button_widget, args, 1);
}

void
timer_button_set() {
	Arg             args[1];

	XtSetArg(args[0], XtNstate, True);
	XtSetValues(timer_button_widget, args, 1);
}

void
timer_button_reset() {
	Arg             args[1];

	XtSetArg(args[0], XtNstate, False);
	XtSetValues(timer_button_widget, args, 1);
}


/*ARGSUSED*/
void
popup_title_dialog(widget, parent, call_data)
Widget		widget;
Widget		parent;
XtPointer	call_data;
{
	Position x, y;
	Dimension width, height;
	Arg	args[2];
	Widget	value;
	char	non_null[256];



	if (disc_title != NULL) {
		sscanf(disc_title, "%s", non_null);

		if (strcmp ("", non_null) != 0)
        		XtSetArg(args[0], XtNvalue, disc_title);
		else
        	    XtSetArg(args[0], XtNvalue, "");
	}
	else
        	XtSetArg(args[0], XtNvalue, "");

	XtSetValues(title_dialog_widget, args, 1);	

	XtTranslateCoords (widget, 
		(Position) 14,
		(Position) 14,
		&x, &y);
	
	XtSetArg(args[0], XtNx, x);
	XtSetArg(args[1], XtNy, y);
	XtSetValues(title_dialog_shell, args, 2);

	if ((value = XtNameToWidget(title_dialog_widget, "value")) != 0) {
	    XtSetArg(args[0], XtNwidth, &width);
	    XtGetValues(title_button, args, 1);
	    XtSetArg(args[0], XtNwidth, width);
	    XtSetValues(value, args, 1);
	}

	/*XtSetSensitive (widget, FALSE); */
	XtPopup (title_dialog_shell, XtGrabNone);

}

void
Done(widget, event, params, num_params)
	Widget		widget;
	XButtonEvent	*event;
	String		*params;
	Cardinal	*num_params;
{
	Arg	args[2];

	if (disc_title != NULL)
	    free(disc_title);
	disc_title = XawDialogGetValueString(title_dialog_widget);
	XtSetArg(args[0], XtNlabel, disc_title);
	XtSetValues(title_button, args, 1);

	XtPopdown(title_dialog_shell);
	XtSetSensitive(title_button, TRUE);
}

void
popdown_title_dialog(widget, dialog, call_data)
Widget	widget;
Widget	dialog;
XtPointer call_data;
{
	Arg	args[2];

	if (disc_title != NULL)
	    free(disc_title);
	disc_title = XawDialogGetValueString(dialog);
	XtSetArg(args[0], XtNlabel, disc_title);
	XtSetValues(title_button, args, 1);

	XtPopdown(title_dialog_shell);
	XtSetSensitive(title_button, TRUE);
}

void
update_title () {
	Arg		args[1];
	char	non_null[256];

	if (title_button != NULL) {
#if 0
		if (disc_title != NULL) {
			sscanf(disc_title, "%s", non_null);
			if (strcmp ("", non_null) != 0)
				XtSetArg(args[0], XtNvalue, disc_title);
			else
				XtSetArg(args[0], XtNvalue, "");
		else
			XtSetArg(args[0], XtNvalue, "");
		}

		XtSetArg(args[0], XtNvalue, disc_title);
		XtSetValues(title_button, args, 1);
		XtSetArg(args[0], XtNvalue, disc_title);
		XtSetValues(title_dialog_widget, args, 1);
#endif
		XtSetArg(args[0], XtNvalue, disc_title);
		XtSetValues(title_dialog_widget, args, 1);
		XtSetArg(args[0], XtNlabel, disc_title);
		XtSetValues(title_button, args, 1);

	}
}

#ifdef sgi
set_volume(vol) 
	int	vol;
{
	XawScrollbarSetThumb(volume_scroll_widget, 
			     (float) (VAL2PCT(vol)),
			     (float) 1.0);
}
#endif
