/* $Id: kconvert.c,v 1.1 1994/06/27 17:17:44 asami Exp $ */
/*
 * Copyright (c) 1989  Software Research Associates, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Software Research Associates not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Software Research
 * Associates makes no representations about the suitability of this software
 * for any purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * Author:  Makoto Ishisone, Software Research Associates, Inc., Japan
 *		ishisone@sra.co.jp
 */

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xmu/Xmu.h>
#include <X11/Xproto.h>
#include <stdio.h>
#include <setjmp.h>
#include <ctype.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include "ptyx.h"
#include "data.h"

#ifdef KTERM_KCONV

extern int	_beginConversionWithAttributes();
extern void	_changeConversionAttributes();
extern int	convCTtoCS();
extern int	convCStoEUC();
extern int	convCStoSJIS();
extern int	convCStoJIS();

static Atom	japanese_conversion_atom;	/* JAPANESE_CONVERSION */
static Atom	compound_text_atom;		/* COMPOUND_TEXT */
static int	atominitialized;
static int	(*originalerrorhandler)();

static Atom	converting = None;
static Window	forwardwin = None;

static XEvent	*event_queue;
static int	queue_event;
static int	queue_end;

static struct kcres {
    Boolean forwardevent;
    Boolean keypressonly;
    int queuesize;
} kcr;

static XtResource resources[] = {
    { "forwardKeyEvent", "ForwardKeyEvent", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(struct kcres, forwardevent), XtRImmediate, (XtPointer)True },
    { "forwardKeyPressOnly", "ForwardKeyPressOnly", XtRBoolean, sizeof(Boolean),
      XtOffsetOf(struct kcres, keypressonly), XtRImmediate, (XtPointer)True },
    { "forwardQueueSize", "ForwardQueueSize", XtRInt, sizeof(int),
      XtOffsetOf(struct kcres, queuesize), XtRImmediate, (XtPointer)32 },
};

static void
flushEventQueue(dpy, win)
Display *dpy;
Window win;
{
    int i;

    for (i = 0; i < queue_end; i++) {
	XEvent *ev = &event_queue[i];
	ev->xkey.window = forwardwin;
	XSendEvent(dpy, win, False, 0L, ev);
    }
    queue_end = 0;
}

static void
clearEventQueue()
{
    queue_end = 0;
}

/* ARGSUSED */
static void
forwardKeyEvent(w, cldata, ev, continuep)
Widget w;
XtPointer cldata;
XEvent *ev;
Boolean *continuep;
{
    if (ev->xany.send_event) return;

    if (forwardwin != None) {
	/* send the event to IMS */
	*continuep = False;
	if (ev->type == KeyPress || !kcr.keypressonly) {
	    ev->xkey.window = forwardwin;
	    XSendEvent(XtDisplay(w), forwardwin, False, 0L, ev);
	}
    } else if (queue_event) {
	*continuep = False;
	if (ev->type == KeyPress || !kcr.keypressonly) {
	    event_queue[queue_end++] = *ev;
	    if (queue_end >= kcr.queuesize) queue_event = 0;
	}
    }
}

static int
forwardErrorHandler(dpy, error)
Display *dpy;
XErrorEvent *error;
{
    if (error->request_code == X_SendEvent &&
	error->error_code == BadWindow) {
	/* IMS dead */
	forwardwin = None;
	queue_event = 0;
    } else {
	(*originalerrorhandler)(dpy, error);
    }
}

static void
initializeForwarding(w)
Widget w;
{
    XtGetApplicationResources(w, (XtPointer)&kcr, resources,
			      XtNumber(resources), (ArgList)NULL, 0);
    if (!kcr.forwardevent) return;

    XtInsertEventHandler(w, KeyPressMask|KeyReleaseMask, False,
			 forwardKeyEvent, NULL, XtListHead);
    originalerrorhandler = XSetErrorHandler(forwardErrorHandler);

    if (kcr.queuesize < 0) kcr.queuesize = 0;
    if (kcr.queuesize > 0) {
	event_queue = (XEvent *)XtMalloc(kcr.queuesize * sizeof(XEvent));
    }
}

/* ARGSUSED */
static void
inputString(w, selection, type, format, size, str, client_data)
Widget		w;
Atom		selection;
Atom		type;
int		format;
unsigned long	size;
unsigned char	*str;
caddr_t		client_data;
{
	unsigned char	lbuf[256 + 1];
	unsigned char	*lstr, *p, *lp;
	int		n;
	int		(*convfunc)();
	int		pty = ((XtermWidget)w)->screen.respond;

	if (str == NULL)
		return;

	if (type == compound_text_atom && format == 8) {
		Ichr		cbuf[256 + 1];
		Ichr		*cs;
		n = convCTtoCS(str, size, NULL);
		cs = (n > 256) ? (Ichr *)XtMalloc((n+1) * sizeof(Ichr))
			       : cbuf;
		(void)convCTtoCS(str, size, cs);
		if (term->flags & EUC_KANJI)
		    convfunc = convCStoEUC;
		else if (term->flags & SJIS_KANJI)
		    convfunc = convCStoSJIS;
		else
		    convfunc = convCStoJIS;

		n = (*convfunc)(cs, NULL);
		lstr = (n > 256) ? (unsigned char *)XtMalloc(n + 1) : lbuf;
		(void)(*convfunc)(cs, lstr);
		if (cs != cbuf) XtFree((char *)cs);
	} else {
		/* ignore unknown type */
		XtFree((char *)str);
		return;
	}

	/*
	 * Hack: (since Compound Text can't send carriage return)
	 * change each newline into carriage return.
	 */
	for (p = lp = lstr; *p; p++)	{
		if (*p != '\n' && *p != '\r')
			continue;
		*p = '\r';
		v_write(pty, lp, p - lp + 1);
	    lp = p + 1;
	}
	if (lp != p)
		v_write(pty, lp, p - lp);

	XtFree((char *)str);
	if (lstr != lbuf) XtFree((char *)lstr);
}

/* ARGSUSED */
static void
startendProc(w, selection, state, client_data, forward)
Widget w;
Atom selection;
int state;
caddr_t client_data;
Window forward;
{
    if (state == 0) {	/* start */
	converting = selection;
	forwardwin = forward;
	if (queue_event) flushEventQueue(XtDisplay(w), forwardwin);
    } else {		/* end or fail */
	converting = None;
	forwardwin = None;
    }
    queue_event = 0;
    clearEventQueue();
}

void
SendSpot()
{
    TScreen *screen = &term->screen;
    Arg arg[2];
    Position x, y;

    if (converting == None) return;

    x = CursorX(screen, screen->cur_col);
    y = CursorY(screen, screen->cur_row) + screen->max_ascent;
    XtSetArg(arg[0], "spotX", x);
    XtSetArg(arg[1], "spotY", y);
    _changeConversionAttributes(term, converting, arg, 2);
}

void
SendColor()
{
    TScreen *screen = &term->screen;
    Arg arg[2];

    if (converting == None) return;

    XtSetArg(arg[0], "foreground", screen->foreground);
    XtSetArg(arg[1], "background", term->core.background_pixel);
    _changeConversionAttributes(term, converting, arg, 2);
}

void
SendSize()
{
    TScreen *screen = &term->screen;
    Arg arg[1];
    XRectangle area;

    if (converting == None) return;

    area.x = screen->border + screen->scrollbar;
    area.y = screen->border;
    area.width = Width(screen);
    area.height = Height(screen);
    XtSetArg(arg[0], "clientArea", &area);
    _changeConversionAttributes(term, converting, arg, 1);
}

void
SendFonts()
{
    TScreen *screen = &term->screen;
    Arg arg[1];
    XFontStruct *fonts[FCNT + 1];
    int i, nfonts;

    if (converting == None) return;

    for (nfonts = 0, i = 0; i < FCNT; i++) {
	if (screen->_fnt_norm[i] != NULL) {
	    fonts[nfonts++] = screen->_fnt_norm[i];
	}
    }
    fonts[nfonts] = NULL;
    if (nfonts > 0) {
	XtSetArg(arg[0], "fonts", fonts);
	_changeConversionAttributes(term, converting, arg, 1);
    }
}

/* ARGSUSED */
void
BeginConversion(w, event, params, nparams)
Widget	w;
XEvent	*event;
String	*params;
Cardinal	*nparams;
{
	Atom	catom;
	Arg	arg[10];
	Cardinal	i;
	Position	x, y;
	int spacing;
	XRectangle	area;
	TScreen	*screen = &term->screen;
	Cardinal nfonts, j;
	XFontStruct *fonts[FCNT + 1];

	if (!atominitialized) {
		japanese_conversion_atom =
		    XInternAtom(XtDisplay(w), "_JAPANESE_CONVERSION", False);
		compound_text_atom =
		    XInternAtom(XtDisplay(w), "COMPOUND_TEXT", False);
		atominitialized = 1;
		initializeForwarding(w);
	}

	if (*nparams != 1) {
		catom = japanese_conversion_atom;
	} else {
		XmuInternStrings(XtDisplay(w), params, 1, &catom);
	}

	i = 0;

	XtSetArg(arg[i], "inputStyle", "over"); i++;

	x = CursorX(screen, screen->cur_col);
	y = CursorY(screen, screen->cur_row) + screen->max_ascent;
	XtSetArg(arg[i], "spotX", x); i++;
	XtSetArg(arg[i], "spotY", y); i++;

	XtSetArg(arg[i], "foreground", screen->foreground); i++;
	XtSetArg(arg[i], "background", term->core.background_pixel); i++;

	XtSetArg(arg[i], "lineSpacing", FontHeight(screen)); i++;

	for (nfonts = 0, j = 0; j < FCNT; j++) {
	    if (screen->_fnt_norm[j] != NULL) {
		fonts[nfonts++] = screen->_fnt_norm[j];
	    }
	}
	fonts[nfonts] = NULL;
	if (nfonts > 0) {
	    XtSetArg(arg[i], "fonts", fonts); i++;
	}

	area.x = screen->border + screen->scrollbar;
	area.y = screen->border;
	area.width = Width(screen);
	area.height = Height(screen);
	XtSetArg(arg[i], "clientArea", &area); i++;

	if (kcr.forwardevent) {
	    queue_event = (kcr.queuesize > 0);
	    clearEventQueue();
	    XtSetArg(arg[i], "eventCaptureMethod", "none"); i++;
	}

	if (_beginConversionWithAttributes(w, catom, compound_text_atom,
					   inputString, startendProc,
					   (XtPointer)NULL, arg, i) < 0) {
	    queue_event = 0;
	}
}
#endif	/* KTERM_KCONV */
