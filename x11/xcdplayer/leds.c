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
# include <X11/Xaw/Label.h>

# include "cdrom_globs.h"

# include "circle0a.xbm"
# include "circle0b.xbm"
# include "circle0c.xbm"
# include "circle0d.xbm"
# include "circle0e.xbm"
# include "circle0f.xbm"
# include "circle0g.xbm"
# include "circle0h.xbm"
# include "circle00.xbm"

static Widget	leds_label_widget;

# define NUM_LEDS	8

static Pixmap	leds_pixmaps[NUM_LEDS];
static Pixmap	leds_pixmap;

void
leds_update(Direction) 
int	Direction;
{
	static int	ctr = 1;
	Arg		args[1];
	Pixmap		leds;

		
	if (Direction == BACKWARDS)
	    leds = leds_pixmaps[(ctr += NUM_LEDS - 1) % NUM_LEDS];
	else
	    leds = leds_pixmaps[ctr++ % NUM_LEDS];

	XtSetArg(args[0], XtNbitmap, (XtArgVal) leds);
	XtSetValues(leds_label_widget, args, 1);
}

void
leds_stop() {
	Arg		args[1];

	XtSetArg(args[0], XtNbitmap, (XtArgVal) leds_pixmap);
	XtSetValues(leds_label_widget, args, 1);
}

void
leds_label_setup(parent_widget)
	Widget		parent_widget;
{
	Arg		args[1];

	leds_label_widget = XtCreateManagedWidget("ledsLabel",
						  labelWidgetClass,
						  parent_widget,
						  (ArgList) NULL, 0);

	leds_pixmaps[0] = XCreateBitmapFromData(XtDisplay(leds_label_widget),
						rootwin(leds_label_widget),
						circle0a_bits,
						circle0a_width, circle0a_height);
	leds_pixmaps[1] = XCreateBitmapFromData(XtDisplay(leds_label_widget),
						rootwin(leds_label_widget),
						circle0b_bits,
						circle0b_width, circle0b_height);
	leds_pixmaps[2] = XCreateBitmapFromData(XtDisplay(leds_label_widget),
						rootwin(leds_label_widget),
						circle0c_bits,
						circle0c_width, circle0c_height);
	leds_pixmaps[3] = XCreateBitmapFromData(XtDisplay(leds_label_widget),
						rootwin(leds_label_widget),
						circle0d_bits,
						circle0d_width, circle0d_height);
	leds_pixmaps[4] = XCreateBitmapFromData(XtDisplay(leds_label_widget),
						rootwin(leds_label_widget),
						circle0e_bits,
						circle0e_width, circle0e_height);
	leds_pixmaps[5] = XCreateBitmapFromData(XtDisplay(leds_label_widget),
						rootwin(leds_label_widget),
						circle0f_bits,
						circle0f_width, circle0f_height);
	leds_pixmaps[6] = XCreateBitmapFromData(XtDisplay(leds_label_widget),
						rootwin(leds_label_widget),
						circle0g_bits,
						circle0g_width, circle0g_height);
	leds_pixmaps[7] = XCreateBitmapFromData(XtDisplay(leds_label_widget),
						rootwin(leds_label_widget),
						circle0h_bits,
						circle0h_width, circle0h_height);

	leds_pixmap = XCreateBitmapFromData(XtDisplay(leds_label_widget),
					     rootwin(leds_label_widget),
					     circle00_bits,
					     circle00_width, circle00_height);

	XtSetArg(args[0], XtNbitmap, (XtArgVal) leds_pixmap);
	XtSetValues(leds_label_widget, args, 1);
}
