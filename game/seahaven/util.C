/*
 * Author:  Terry Weissman
 *          weissman@sgi.com
 */

#include "seahaven.h"

#include <string.h>



unsigned long GetColor(char *name, unsigned long def) {
    XColor col1, col2;
    if (hascolor && XAllocNamedColor(dpy, cmap, name, &col1, &col2)) {
	return col2.pixel;
    } else {
	return def;
    }
}


void GetInterestingEvent(XEvent *event) {
    XNextEvent(dpy, event);
    while (event->type == Expose) {
	if (event->xexpose.count == 0) score->repaint();
	XNextEvent(dpy, event);
    }
}
	

Window CreateButtonWindow(char *label, int x, int y, int gravity, int *width) {
    static GC textgc = NULL, cleargc = NULL;
    int w = XTextWidth(font, label, strlen(label)) + 4;
    int h = font->ascent + font->descent + 4;
    if (width) *width = w;
    Pixmap pixmap = XCreatePixmap(dpy, toplevel, w, h,
				  DefaultDepth(dpy, screen));
    if (textgc == NULL) {
	textgc = XCreateGC(dpy, toplevel, 0, NULL);
	XSetForeground(dpy, textgc, GetColor("black",
					     BlackPixel(dpy, screen)));
	XSetFont(dpy, textgc, font->fid);
	cleargc = XCreateGC(dpy, toplevel, 0, NULL);
	XSetForeground(dpy, cleargc,  backpixel);
    }
    XFillRectangle(dpy, pixmap, cleargc, 0, 0, w, h);
    XDrawString(dpy, pixmap, textgc, 2, 2 + font->ascent,
		label, strlen(label));

    XSetWindowAttributes attributes;
    long valuemask = CWEventMask | CWBackPixmap | CWWinGravity;
    attributes.event_mask = ButtonPressMask;
    attributes.background_pixmap = pixmap;
    attributes.win_gravity = gravity;
    Window window = XCreateWindow(dpy, toplevel, x, y,
				  w, h, 1, (int) CopyFromParent,
				  InputOutput, (Visual *) CopyFromParent,
				  valuemask, &attributes);
    XMapWindow(dpy, window);
    return window;
}
    
