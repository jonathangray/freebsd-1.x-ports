/*
**
**	X11 Jewel By David Cooper and Jose Guterman 05/92
**
*/

/*
** xw.h - Header file for xw.c (XWraper) functoins
*/

/* Some XW types */

/*typedef unsigned long Pixel;*/

/* Functions provided by xw */

extern void  xw_fatal();
extern Pixel xw_alloc_color();
extern void  xw_main_loop();
extern void  xw_exit_main();
extern void  xw_init();
extern void  xw_start();
extern void  xw_set_timer();
extern void  xw_sync_sleep();

/* Functions required by xw */
extern void xw_but_event(/*xbev*/);
extern void xw_expose_event(/*xev*/);
extern void xw_focus_event(/*xfev*/);
extern void xw_leave_event(/*xlev*/);
extern void xw_key_event(/*xkev*/);
extern void xw_map_event(/*xmev*/);
extern void xw_timeout();


#ifdef VMS
    struct timeval { long tv_sec; long tv_usec; };
#else
#	ifdef USE_SELECT
#		include <sys/types.h>
#		include <sys/time.h>
#	    ifndef FD_SET
		typedef long fd_set;
#		define FD_SET(n,p) (*(p) |= (1 << (n)) )
#		define FD_CLR(n,p) (*(p) &= ~(1 << (n)) )
#		define FD_ISSET(n,p) (*(p) & (1 << (n)) )
#		define FD_ZERO(p) bzero( (p), sizeof(*(p)) )
#	    endif
#	else
#		include <stropts.h>
#		include <poll.h>
#	endif
#endif

#if defined(VMS)
#   define ms_sleep(ms) { float tm; tm=(ms)/1000.0; LIB$WAIT(&tm); }
#else
#if defined(USE_SELECT)
#	define ms_sleep(ms) { struct timeval tv; tv.tv_sec=((ms)/1000); tv.tv_usec=(((ms)%1000)*1000); select(1,NULL,NULL,NULL,&tv); }
#else
static int __JUNK[2]={0};
#	define ms_sleep(ms) poll(__JUNK,(unsigned long)0,ms)
#endif
#endif

/* Data externs */

extern Display *xw_display;
extern int     xw_screen;
extern Window  xw_window;
extern GC      xw_gc;
