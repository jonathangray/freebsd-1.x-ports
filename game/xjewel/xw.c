/*
**
**	X11 Jewel By David Cooper and Jose Guterman 05/92
**
*/

#ifdef VMS
#include <decw$include/Xlib.h>
#include <decw$include/Xutil.h>
#include <decw$include/Xos.h>
#else
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#endif

#ifndef VMS
#   include <sys/param.h>
#endif
#ifndef HZ
#	define HZ 60
#endif

#include <stdlib.h>
#include <stdio.h>
#ifndef SYSV
#include <string.h>
#else
#include <strings.h>
#endif
#include <errno.h>

#include "general.h"
#include "xw.h"

/* include bitmaps */

#include "bitmaps/icon.xbm"
#include "bitmaps/smicon.xbm"

/* important stuff */

Window iw_window;
int iw_width=60;
int iw_height=60;

Display *xw_display;
int     xw_screen;
Window  xw_window;
GC      xw_gc;
int     xw_main;
int     xw_screen_width;
int     xw_screen_height;
int     xw_fg;
int     xw_bg;

/* local routines */

void xw_fatal(s,line,file)
char *s;
int line;
char *file;
	{
	fprintf(stderr,"FATAL ERROR:%s\nIn:%s, Line:%d\n",s,file,line);
	exit(1);
	}


/********************************************************/

Pixel xw_alloc_color(cname)
char *cname;
	{
	XColor dev_color, act_color;
	int res;

	/* a little BW addition from rjc@cogsci.edinburgh.ac.uk */
	if (DefaultDepth(xw_display, xw_screen)==1)
		{
		if (!strcmp(cname,"grey"))
			return BlackPixel(xw_display, xw_screen);
		if (!strcmp(cname,"black"))
			return BlackPixel(xw_display, xw_screen);

		return WhitePixel(xw_display, xw_screen);
		}

	res = XAllocNamedColor(xw_display,
			DefaultColormap(xw_display, xw_screen),
			cname, &dev_color, &act_color);
	if (!res)
		{
		char blah[128];
		sprintf(blah,"Unable to allocate color:%s.",cname);
		xw_fatal(blah,__LINE__,__FILE__);
		}
	return (dev_color.pixel);
	}


/********************************************************/

void xw_sync_sleep(ms)
unsigned long ms;
	{
	struct timeval st, et;
	long SyncTime;

	/*printf("sync ms:%ld\n",ms);*/
	
	gettimeofday(&st,NULL);
	XSync(xw_display, False);
	gettimeofday(&et,NULL);
	/*printf("et-st.sec:%ld,usec:%ld\n",et.tv_sec-st.tv_sec,
		et.tv_usec-st.tv_usec);*/
	SyncTime=( ((et.tv_sec-st.tv_sec)*1000) +
		((et.tv_usec-st.tv_usec)/1000) );
	if ((ms) > ((1000/HZ)+SyncTime))
		{
		ms_sleep(ms-SyncTime);
		}
	/* printf("slept (ms-Synctime):%ld (ms):%ld\n", (long)(ms-SyncTime), ms);
	gettimeofday(&et,NULL);
	printf("#2:et-st.sec:%ld,usec:%ld\n",et.tv_sec-st.tv_sec,
		et.tv_usec-st.tv_usec);*/
	}


BOOL timer_set=FALSE;
BOOL timer_reset=FALSE;
unsigned long timer_ms;
struct timeval timer;
struct timeval curtime;

void xw_set_timer(time_ms)
unsigned long time_ms;
	{
	timer_set=FALSE;
	timer_reset=TRUE;
	timer_ms=time_ms;
	if (time_ms <= (1000/HZ)) /* HZ should be in param.h */
		{
		timer_ms=(1000/HZ);
		}
	}


/********************************************************/

void xw_main_loop()
	{
	XEvent xev;
#ifndef VMS
#   ifdef USE_SELECT
	size_t nfds=(XConnectionNumber(xw_display) + 1);
	fd_set readfds, writefds, exceptfds;
	struct timeval timeout_BSD;

	FD_SET(XConnectionNumber(xw_display),&readfds);
	FD_SET(XConnectionNumber(xw_display),&exceptfds);
#   else
	struct pollfd fds[1];

	fds[0].fd=XConnectionNumber(xw_display);
	fds[0].events=(POLLIN|POLLPRI);
#   endif
#endif

	xw_main = 1;
	while (xw_main)
		{
		int ret, timeout, pending;
		if (timer_set)
			{
			gettimeofday(&curtime,NULL);
			timeout=( ((timer.tv_sec - curtime.tv_sec) * 1000) + 
						((timer.tv_usec - curtime.tv_usec) / 1000) );
			if (timeout <= (1000/HZ)) /* HZ should be in param.h */
				{
				xw_timeout();
				continue;
				}
			}
		else
			{ timeout=(-1); }
		if (timer_reset)
			{
			gettimeofday(&curtime,NULL);
			timer.tv_sec= curtime.tv_sec+(timer_ms/1000);
			timer.tv_usec=curtime.tv_usec+((timer_ms%1000)*1000);
			if (timer.tv_usec > 1000000)
				{
				timer.tv_usec -= 1000000;
				timer.tv_sec += 1;
				}
			timer_set=TRUE;
			timer_reset=FALSE;
			timeout=timer_ms;
			}
			
	/* ret = 0 -> timeout						    */
	/* ret < 0 -> error						    */
	/* ret > 0 -> event						    */
#if defined(VMS)
	ret=0;
	while ( (timeout > 0) && (ret == 0) )
		{
		gettimeofday(&curtime,NULL);
		timeout=( ((timer.tv_sec - curtime.tv_sec) * 1000) +
			((timer.tv_usec - curtime.tv_usec) / 1000) );
		ret=XPending(xw_display);
		}
	if (timeout <= (1000/HZ)) /* HZ should be in param.h */
		{
		ret=0;
		}
#else
#if defined(USE_SELECT)
		if (timeout > 0)
			{
			timeout_BSD.tv_usec=(timeout%1000)*1000;
			timeout_BSD.tv_sec=(timeout/1000);
			ret = select(nfds,&readfds,NULL,&exceptfds,&timeout_BSD);
			}
		else
			{ ret = select(nfds,&readfds,NULL,&exceptfds,NULL); }
		FD_SET(XConnectionNumber(xw_display),&readfds);
		FD_SET(XConnectionNumber(xw_display),&exceptfds);
#else
		while( ( (ret=poll(fds, 1L, timeout)) < 0) &&
				((errno==EINTR) || (errno=EAGAIN)) )
#endif
#endif
		
		if (ret < 0)
			{
			perror("xw poll");
			/*xw_fatal("CANNOT POLL.",__LINE__,__FILE__);*/
			}
		if (ret==0)
			{
			timer_set=FALSE;
			xw_timeout();
			}
		pending=XPending(xw_display);
/*		printf("PENDING:%d\n",pending);*/
		for (;pending > 0;pending--)
			{
			XNextEvent(xw_display, &xev);
			switch (xev.type)
				{
				case Expose:
					xw_expose_event((XExposeEvent *) &xev);
					break;
				case FocusIn:
				case FocusOut:
					xw_focus_event((XFocusChangeEvent *) &xev);
					break;
				case LeaveNotify:
					xw_leave_event((XLeaveWindowEvent *) &xev);
				case KeyRelease:
				case KeyPress:
					xw_key_event((XKeyEvent *) &xev);
					break;
				case ButtonRelease:
				case ButtonPress:
					xw_but_event((XButtonEvent *) &xev);
					break;
				case UnmapNotify:
				case MapNotify:
					xw_map_event((XMapEvent *) &xev);
					break;
				case ConfigureNotify:
					{
					XConfigureEvent *xcev=(XConfigureEvent *)&xev;
					/*printf("config:w:%d,h:%d\n", xcev->width, xcev->height);*/
					if (xcev->window == iw_window)
						{
						iw_width=xcev->width;
						iw_height=xcev->height;
						}
					}
					break;
				}
			}
		}
	}


/********************************************************/

void xw_exit_main()
	{
	xw_main = 0;
	}


/********************************************************/
Pixmap Icon;
Pixmap SmIcon;

#ifdef DECWM
#ifdef VMS
#include <decw$include/decwmhints.h>
#else
#include <X11/DECWmHints.h>
#endif

BOOL decwm_init()
	{
	/* setup for dec window manager */
	DECWmHintsRec dwmhints;
	Atom wmatom;

	/* if decwm hints atom exists -> wm is running */
	wmatom=XInternAtom(xw_display, "DEC_WM_HINTS", True);

	if (wmatom != None)
		{
		dwmhints.value_mask=DECWmIconifyPixmapMask;
		dwmhints.iconify_pixmap=SmIcon;
		XChangeProperty(xw_display, xw_window, wmatom, wmatom, 32,
			PropModeReplace, &dwmhints, sizeof(dwmhints)/4);
		return(TRUE);
		}
	else
		{ return(FALSE); }
	}
#endif

#ifndef MAX
#define MAX(a,b) ((a>b)?a:b)
#endif

void xw_init(argc, argv, w, h )
int  argc;
char **argv;
int  w, h;
	{
	XIconSize *icon_sizes;
	int size_count;
	BOOL big_icon=FALSE;
	XSizeHints hints;
	XWMHints wmhints;
	XGCValues gcv;
	unsigned long gcvm;
	char *Disp=NULL;
	char *AppName;
	int i;

	/* look for display in args */
	for (i=1;i<(argc-1);i++)
		{
		int len;
		char *dispstr="-display";
		len=MAX(strlen(dispstr),strlen(argv[i]));
		if (len<4) { continue; }
		if (strncmp(argv[i],dispstr,len)==0)
			{
			Disp=argv[i+1];
			break;
			}
		}

	xw_display = XOpenDisplay(Disp);
	if (!xw_display)
		{
		printf("Unable to connect to display:%s, exiting.\n",Disp);
		exit(-1);
		}

	if (XGetIconSizes(xw_display, RootWindow(xw_display,xw_screen),
			&icon_sizes, &size_count))
		{
		big_icon=FALSE;
		for (;size_count>0; size_count--)
			{
			if (icon_sizes->max_width >= icon_width)
			    { big_icon=TRUE; }
			/*    
			printf("minw:%d,minh:%d,maxw:%d,maxh:%d,winc:%d,hinc:%d\n",
			    icon_sizes->min_width, icon_sizes->min_height,
			    icon_sizes->max_width, icon_sizes->max_height,
			    icon_sizes->width_inc, icon_sizes->height_inc);
			*/
			icon_sizes++;
			}
		}
	else
		{ big_icon=TRUE; }

	/* look for appname in args */
#ifdef VMS
	AppName=strrchr(argv[0],']');
#else
	AppName=strrchr(argv[0],'/');
#endif
	if (!AppName)
		{ AppName=argv[0]; }
	else
		{ AppName++; /* skip trailing seperator */}
	*AppName=toupper(*AppName);

	/* lets get to it */
	xw_screen_width = w;
	xw_screen_height = h;
	hints.width = xw_screen_width;
	hints.height = xw_screen_height;
	hints.flags = PSize;

	xw_screen = DefaultScreen(xw_display);
	xw_fg = WhitePixel(xw_display,xw_screen);
	xw_bg = BlackPixel(xw_display,xw_screen);
	gcv.graphics_exposures = False;
	gcv.foreground = xw_fg;
	gcv.background = xw_bg;
/*	gcv.font = XLoadFont(xw_display, "8x13");*/
	gcvm = (GCGraphicsExposures | GCForeground | GCBackground /*| GCFont*/);

	xw_window = XCreateSimpleWindow(xw_display,
			RootWindow(xw_display, xw_screen), 0, 0, hints.width,
			hints.height, 2, xw_fg, xw_bg);

	XSetStandardProperties(xw_display, xw_window, AppName, AppName,
			None, argv, argc, &hints);

	Icon=XCreateBitmapFromData(xw_display, xw_window, icon_bits, 
		icon_width, icon_height);
	SmIcon=XCreateBitmapFromData(xw_display, xw_window, smicon_bits, 
		smicon_width, smicon_height);
	if (big_icon)
		{ wmhints.icon_pixmap=Icon; }
	else
		{ wmhints.icon_pixmap=SmIcon; }
	wmhints.flags=IconPixmapHint;

#ifdef ICON_WINDOW
	wmhints.flags|=IconWindowHint;
	iw_window=
	wmhints.icon_window=XCreateSimpleWindow(xw_display,
			RootWindow(xw_display, xw_screen), 0, 0, iw_width,
			iw_height, 2, xw_fg, xw_bg);
	XSelectInput(xw_display, iw_window, StructureNotifyMask | ExposureMask);
#endif

	wmhints.flags|=InputHint;
	wmhints.input=True;

#ifdef DECWM
	if (!decwm_init())
#endif
		{ XSetWMHints(xw_display, xw_window, &wmhints); }

	XSelectInput(xw_display, xw_window,
			 ExposureMask | KeyPressMask | ButtonPressMask |
			 LeaveWindowMask | FocusChangeMask |
			 StructureNotifyMask );

	xw_gc = XCreateGC(xw_display, xw_window, gcvm, &gcv);

	}



void xw_start()
	{
	XMapWindow(xw_display, xw_window);
	XFlush(xw_display);
	}

