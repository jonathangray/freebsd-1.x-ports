/*
 * Copyright 1993 The University of Newcastle upon Tyne
 * 
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose other than its commercial exploitation
 * is hereby granted without fee, provided that the above copyright
 * notice appear in all copies and that both that copyright notice and
 * this permission notice appear in supporting documentation, and that
 * the name of The University of Newcastle upon Tyne not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission. The University of
 * Newcastle upon Tyne makes no representations about the suitability of
 * this software for any purpose. It is provided "as is" without express
 * or implied warranty.
 * 
 * THE UNIVERSITY OF NEWCASTLE UPON TYNE DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF
 * NEWCASTLE UPON TYNE BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 * 
 * Author:  Jim Wight (j.k.wight@newcastle.ac.uk)
 *          Department of Computing Science
 *          University of Newcastle upon Tyne, UK
 */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include "AxeIntrinsic.h"
#include "util.h"

extern char *getenv();

unsigned char xerror;
int (*DefaultErrorHandler)();

static int
TrapError(dpy, error)
     Display *dpy;
     XErrorEvent *error;
{
    xerror = error->error_code;
    if (xerror != BadWindow)
    {
        DefaultErrorHandler(dpy, error);
    }
    return 0;
}

void
TrapErrors(dpy)
     Display *dpy;
{
    /*
     * XSync is called to ensure that any outstanding
     * errors are dealt with by the default handler. 
     */
    XSync(dpy, False);
    DefaultErrorHandler = XSetErrorHandler(TrapError);
    xerror = Success;
}

void
DontTrapErrors(dpy)
     Display *dpy;
{
    /*
     * XSync is called to ensure that any error that occurs
     * while we are trapping errors is dealt with before the
     * default error handler is restored.
     */
    XSync(dpy, False);
    (void) XSetErrorHandler(NULL);
}

void
PopupCentred(popup, grabState)
     Widget popup;
     Boolean grabState;
{
    Window root, child;
    unsigned int mask;
    XEvent event;
    Dimension width, height, b_width;
    Position x, y, max_x, max_y;

    XtRealizeWidget(popup);

    XQueryPointer(XtDisplay(popup), XtWindow(popup), &root, &child,
		  &event.xbutton.x_root, &event.xbutton.y_root,
		  &event.xbutton.x, &event.xbutton.y, &mask);

    x = event.xbutton.x_root;
    y = event.xbutton.y_root;

    XtVaGetValues(popup,
                  XtNwidth, &width,
                  XtNheight, &height,
                  XtNborderWidth, &b_width,
                  NULL);

    width += 2 * b_width;
    height += 2 * b_width;

    x -= ( (Position) width/2 );
    if (x < 0) x = 0;
    if (x > (max_x = (Position) (XtScreen(popup)->width - width))) x = max_x;
    
    y -= ( (Position) height/2 );
    if (y < 0) y = 0;
    if (y > (max_y = (Position) (XtScreen(popup)->height - height))) y = max_y;

    XtVaSetValues(popup,
                  XtNx, x,
                  XtNy, y,
                  NULL);

    XtPopup(popup, grabState);
}

int
GetHostName(name, namelen)
     char *name;
     int namelen;
{
#if defined(SYSV) || defined(SVR4)
#include <sys/utsname.h>
    struct utsname info;
    int code;

    if ( (code = uname(&info)) == 0)
    {
	strcpy(name, info.nodename);
    }
    return code;
#else
    return gethostname(name, namelen);
#endif
}

Atom
HostUserServerAtom(display)
     Display *display;
{
    char host[MAXHOSTNAMELEN], *user, propName[MAXPATHLEN];

    strcpy(propName, AXE_SERVER);
    if (GetHostName(host, MAXHOSTNAMELEN - strlen(AXE_SERVER)) == 0)
    {
        strcat(propName, "_");
        strcat(propName, host);
    }

    if ( (user = getenv("USER")) )
    {
	strcat(propName, "_");
	strcat(propName, user);
    }

    return XInternAtom(display, propName, False);
}
