/*
 * Author:  Terry Weissman
 *          weissman@sgi.com
 */

#define MAIN


#include "seahaven.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <X11/Xutil.h>


void Punt(char *str) {
    fprintf(stderr, "%s: %s\n", progname, str);
    exit(1);
}

static void Usage() {
    fprintf(stderr, "Usage: %n [-d display] [-speedup speedfactor]\n",
	    progname);
    exit(1);
}


main(int argc, char **argv) {
    char *displayname = NULL;
    Bool sync = False;
    int i;

    progname = argv[0];

    speedup = 6;

    for (i=1 ; i<argc ; i++) {
	if (i < argc - 1 && (strcmp(argv[i], "-d") == 0 ||
			     strcmp(argv[i], "-display") == 0)) {
	    displayname = argv[++i];
	} else if (strcmp(argv[i], "-sync") == 0) {
	    sync = True;
	} else if (strcmp(argv[i], "-speedup") == 0) {
	    speedup = atoi(argv[++i]);
	    if (speedup <= 0) Usage();
	} else {
	    Usage();
	}
    }

    inautoplay = False;

    dpy = XOpenDisplay(displayname);
    if (!dpy) {
	Punt("Can't open display.");
    }
    screen = DefaultScreen(dpy);
    cmap = DefaultColormap(dpy, screen);
    XSynchronize(dpy, sync);

    Visual *visual = DefaultVisual(dpy, screen);

    hascolor = (visual->c_class != StaticGray &&
		visual->c_class != GrayScale && visual->bits_per_rgb > 1);

    backpixel = GetColor("forestgreen", WhitePixel(dpy, screen));

    XSetWindowAttributes attributes;
    long valuemask = CWEventMask | CWBackPixel;
    attributes.event_mask = KeyPressMask;
    attributes.background_pixel = backpixel;

    toplevel = XCreateWindow(dpy, DefaultRootWindow(dpy), 0, 0,
			     GAMEWIDTH, GAMEHEIGHT, 1, (int) CopyFromParent,
			     InputOutput, (Visual *) CopyFromParent,
			     valuemask, &attributes);
    XSetIconName(dpy, toplevel, "Seahaven");
    XStoreName(dpy, toplevel, "Seahaven Towers");

    font = XLoadQueryFont
	(dpy, "-*-helvetica-medium-r-normal--*-120-*-*-p-*-iso8859-1");
    if (font == NULL) font = XLoadQueryFont(dpy, "fixed");
    if (font == NULL) Punt("Can't find a font!");

    CardInit();
    StackInit();
    ScoreInit();

    int y = GAMEHEIGHT - font->ascent - font->descent - 10;
    int width, width2, width3;
    Window undobutton = CreateButtonWindow("Undo (U)", 10, y,
					   SouthWestGravity, &width);
    Window redobutton = CreateButtonWindow("Redo (R)", 10 + width + 5, y,
					   SouthWestGravity, &width2);
    Window autobutton = CreateButtonWindow("Autoplay",
    					   10 + width + 5 + width2 + 5, y,
					   SouthWestGravity, NULL);
    Window quitbutton = CreateButtonWindow("Quit", 0, 0,
					   SouthEastGravity, &width);
    Window newgamebutton = CreateButtonWindow("New Game", 0, 0,
					      SouthEastGravity, &width2);
    Window restartbutton = CreateButtonWindow("Restart", 0, 0,
					      SouthEastGravity, &width3);
    XMoveWindow(dpy, quitbutton, GAMEWIDTH - width - 10, y);
    XMoveWindow(dpy, newgamebutton, GAMEWIDTH - width - width2 - 20, y);
    XMoveWindow(dpy, restartbutton,
		GAMEWIDTH - width - width2 - width3 - 30, y);

    XMapWindow(dpy, toplevel);

    for (;;) {
	XEvent event;
	GetInterestingEvent(&event);
	if (!CardHandleEvent(&event)) {
	    if (event.type == KeyPress) {
		char buf[100];
		int length;
		length = XLookupString((XKeyEvent *)&event, buf, 100, NULL,
				       NULL);
		for (i=0 ; i<length ; i++) {
		    switch(buf[i]) {
		      case 'u':
		      case 'U':
		      case 'z':
		      case 'Z':
			DoUndo();
			break;
		      case 'r':
		      case 'R':
			DoRedo();
			break;
		      case '\016':
			score->NewGame(True);
			break;
		      case '\003':
			XUnmapWindow(dpy, toplevel);
			XFlush(dpy);
			exit(0);
			break;
		    }
		}
	    } else if (event.type == ButtonPress) {
		Window w = event.xbutton.window;
		if (w == undobutton) DoUndo();
		else if (w == redobutton) DoRedo();
		else if (w == restartbutton) DoRestart();
		else if (w == autobutton) DoAutoPlay();
		else if (w == newgamebutton) score->NewGame(True);
		else if (w == quitbutton) {
		    XUnmapWindow(dpy, toplevel);
		    XFlush(dpy);
		    exit(0);
		}
	    }
	}
    }
}
