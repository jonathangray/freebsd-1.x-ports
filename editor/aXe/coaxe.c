/*
 * Copyright 1992 The University of Newcastle upon Tyne
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

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <sys/param.h>
#include <sys/errno.h>
extern int errno;
#ifndef __BSD_4_4__
extern char *sys_errlist[];
#endif
#include <stdio.h>
extern char *getenv();
extern char *malloc(), *realloc();

#include "util.h"

#include <AxeIntrinsic.h>

static int
ValidWindow(display, wid, atom)
     Display *display;
     Window wid;
     Atom atom;
{
    Atom gotAtom;
    int gotFormat;
    unsigned long gotItems, moreBytes; 
    unsigned char *propValue;

    TrapErrors(display);

    if (XGetWindowProperty(display,
                           wid,
                           atom,
                           0, 0,
                           False,
                           XA_STRING,
                           &gotAtom,
                           &gotFormat,
                           &gotItems,
                           &moreBytes,
                           &propValue) == Success)
    {
        if (gotAtom == None)
        {
            xerror = BadAtom;
        }
        XFree((char *) propValue);
    }

    DontTrapErrors(display);

    return xerror == Success;
}

static void
NoAxe(error)
     int error;
{
    char host[MAXHOSTNAMELEN];
    
    (void) GetHostName(host, MAXHOSTNAMELEN);
    fprintf(stderr,
	    "aXe server on %s not running: unable to start axe: %s\n",
	    host, sys_errlist[error]);
    exit(1);
}

main(argc, argv)
     int argc;
     char *argv[];
{
    int coaxe, faxe, poleaxe, arg, proplen, nchunk, newlen;
    char *r, *dpy;
    Display *display;
    Bool buffers = False, newWindow = True;
    char *token;
    Atom reqAtom = XA_WINDOW, gotAtom;
    int gotFormat;
    unsigned long gotItems, moreBytes; 
    unsigned char *propValue;
    Window propWindow;
    int serverOK;
    char wid[13], cwd[MAXPATHLEN];
    char *property, *maxp, *p;
    Window window;
    XEvent event;

#if defined(SYSV) || defined(SVR4)
    extern char *getcwd();
#define getwd(a) getcwd(a,MAXPATHLEN)
#else
    extern char *getwd();
#endif

    r = (r = rindex(argv[0], '/')) ? ++r : argv[0];
    coaxe = strncmp(r, "coaxe", 5) == 0;
    faxe = strncmp(r, "faxe", 4) == 0;
    /* poleaxe = strncmp(r, "poleaxe", 7) == 0; Not used */

    if ( !(display = XOpenDisplay(NULL)) )
    {
	if ( !(dpy = getenv("DISPLAY")) )
	{
	    fprintf(stderr, "DISPLAY variable not set\n");
	}
	else
	{
	    /* Assume XOpenDisplay puts out appropriate message */
	}

	exit(1);
    }
    
    window = XCreateSimpleWindow(display,
				 DefaultRootWindow(display),
				 0, 0,
				 1, 1,
				 0,
				 WhitePixel(display, DefaultScreen(display)),
				 WhitePixel(display, DefaultScreen(display))
				 );
    XSelectInput(display, window, PropertyChangeMask);

    sprintf(wid, "%12ld", window);
    if (XGetWindowProperty(display,
                           XDefaultRootWindow(display),
                           HostUserServerAtom(display),
                           0, 1,
                           False,
                           reqAtom,
                           &gotAtom,
                           &gotFormat,
                           &gotItems,
                           &moreBytes,
                           &propValue) == Success)
    {
	if (gotAtom == reqAtom)
	{
	    propWindow = *((Window *) propValue);
	    serverOK = ValidWindow(display,
				   propWindow, HostUserServerAtom(display));
	    XFree((char *) propValue);
	}
	else
	{
	    serverOK = False;
	}
    }

    if (!getwd(cwd))
    {
	fprintf(stderr, "Current directory unreadable\n" );
	exit(1);
    }
    
    property = malloc((unsigned) MAXPATHLEN * sizeof(char));
    maxp = property + MAXPATHLEN;
    nchunk = 1;

    strcpy(property, wid);
    p = &property[strlen(property)];
    for (arg = 1;
	 (faxe && arg < argc) || (coaxe && arg < argc && arg <= 1);
	 ++arg)
    {
	token = argv[arg];

	if (token[0] == '-' && strncmp(token, "-buffer", strlen(token)) == 0)
	{
	    buffers = True;
	    newWindow = True;
	    continue;
	}

	if (argv[arg][0] == '/')
	{
	    newlen = strlen(argv[arg]) + 1;
	}
	else
	{
	    newlen = strlen(cwd) + 1 + strlen(argv[arg]) + 1;
	}

	if (p + newlen >= maxp)
	{
	  {
            int poff = p - property;
	    if ( !(property = realloc(property,
			   (unsigned) ++nchunk * MAXPATHLEN * sizeof(char))) )
	    {
		fprintf(stderr, "Out of memory\n");
		exit(1);
	    }

	    p = property + poff;
	    maxp = property + (nchunk * MAXPATHLEN);
	  }
	}

	if (buffers && !newWindow)
	{
	    *(p - 1) = ' ';
	}

	if (argv[arg][0] == '/')
	{
	    strcpy(p, token);
	}
	else
	{
	    strcpy(p, cwd);
	    strcat(p, "/");
	    strcat(p, token);
	}

	p += strlen(p) + 1;
	if (!serverOK)
	{
	    *(p - 1) = ' ';
	}

	if (buffers && newWindow)
	{
	    newWindow = !newWindow;
	}
    }
    *p = *(p + 1) = '\0';
    proplen = p + 2 - property;

    if (!serverOK)
    {
	int err;

	if (coaxe)
	{
	    if (argc == 1)
	    {
		err = execlp("axe", "axe", "-noserver", (char *) 0);
	    }
	    else
	    {
		err = execlp("axe", "axe", "-noserver", property + 12,
			                                           (char *) 0);
	    }
	
	    if (err == -1)
	    {
		NoAxe(errno);
	    }
	}
	else if (faxe)
	{
	    char command[MAXPATHLEN];

	    if (argc == 1)
	    {
		sprintf(command, "axe -noserver &");
	    }
	    else
	    {
		sprintf(command, "axe -noserver %s &", property + 12);
	    }
	    err = system(command);

	    if ( ((err >> 8) & 0xff) != 0)
	    {
		err = (err >> 8) & 0xff;
	    }
	    else
	    {
		err = errno;
	    }

	    if (err != 0)
	    {
		NoAxe(err);
	    }
	    else
	    {
		exit(0);
	    }
	}
    }
	
    XChangeProperty(display,
		    propWindow,
		    XInternAtom(display,
				(coaxe ? AXE_COAXE : (faxe ? AXE_FAXE : AXE_POLEAXE)), False),
		    XA_STRING,
		    8,
		    PropModeReplace,
		    (unsigned char *) property,
		    proplen);

    free(property);

    XFlush(display);

    if (coaxe)
    {
	for(;;)
	{
	    XNextEvent(display, &event);
	    if (event.type == PropertyNotify &&
		event.xproperty.atom == XInternAtom(display, AXE_COAXE, False))
	    {
		break;
	    }
	}
    }

    XCloseDisplay(display);

    return 0;
}
