/* macstuff.c - macintosh interface routines for xlisp-stat */

#ifdef MPWC
# include <Windows.h>	/* pulls in MacTypes, QuickDraw.h */
# include <Events.h>
# include <Menus.h>
# include <Files.h>
# include <ToolUtils.h>
# include <TextEdit.h>
# include <OSUtils.h>
# include <Memory.h>
# include <OSEvents.h>
# include <Fonts.h>
# include <SegLoad.h>
# include <SysEqu.h>
# include "xmath.h"
# include "xlisp.h"
# include "version.h"
# define arrow qd.arrow
# define RETURNCHAR '\n'
# ifdef mc68881
#  ifndef _MC68881_
#   define _MC68881_
#  endif _MC68881_
# else
#  ifdef _MC68881_
#   undef _MC68881_
#  endif _MC68881_
# endif mc68881
#else
# include <WindowMgr.h>	/* pulls in MacTypes, QuickDraw.h */
# include <EventMgr.h>
# include <MenuMgr.h>
# include <ToolBoxUtil.h>
# include <OSUtil.h>
# include "xmath.h"
# include "xlisp.h"
# include <unix.h>
# include "version.h"
# define RETURNCHAR '\r'
#endif MPWC

# define TTYRETURN TtyPutc(RETURNCHAR);

# include "TransEdit.h"


/* resource numbers */
# define	ttyWindRes		1001
# define GC_CURS_RES 1800

/* external variables */
extern FILE *tfp;
extern int xldebug;
extern LVAL xlenv, xlfenv, s_input_stream;
extern int in_send;
extern long time_stamp;

/* external functions */
extern int getttyline();
extern unsigned long time();
extern char buf[];

/* forward declarations */
static background_tasks();
#ifdef DODO
static event_hook();
#endif DODO

/* local variables */
WindowPtr	ttyWind;
static LVAL input_stream;

osinit(name)
  char *name;
{
  long size;
  
  if (GetApplLimit() - GetZone() < 1200000)
	SetApplLimit(GetApplLimit() - 2048);
  else
    SetApplLimit(GetApplLimit() - 32768);
  
  MaxApplZone();

  SkelInit ();

#ifdef THINK_C
  Stdio_MacInit(true);
  Click_On(false);
#endif THINK_C

#ifdef _MC68881_
  check_MC68881();
#endif _MC68881_
  
  get_default_volume();
  
  if (ttyWind == nil) {
    Rect r;
    SetRect(&r, 10, 40, 500, 320);
    make_listener_window(r);
  }
#ifdef MPWC
  MaxMem(&size);
  unload_segments(TRUE);
  MaxMem(&size);
#endif MPWC
  /*SkelEventHook(event_hook);*/
  SkelBackground(background_tasks);
  TtyPrint(name);
  sprintf(buf, "XLISP-STAT version %s, Copyright (c) 1989, by Luke Tierney.",
          XLISPSTAT_VERSION);
  TTYRETURN
  TtyPrint(buf);
  TTYRETURN
  TtyPrint("Several files will be loaded; this may take a few minutes.");
  TTYRETURN
  TTYRETURN
  TTYRETURN
}

osfinish()
{
  while(! ClobberEWindows())	/* deal with any open edit windows */
    SysBeep(10);
  SkelClobber ();				/* throw away windows and menus */
}

oserror(msg)
{
  char line[100],*p;
  sprintf(line,"error: %s\n",msg);
  for (p = line; *p != '\0'; ++p)
  ostputc(*p);
}

FILE *osaopen(name,mode)
  char *name,*mode;
{
#ifdef MPWC
  short vref;
  
  if (mode[0] == 'w' || mode[0] == 'a') {
    /*** this may need to be cleaned up to catch errors and handle append mode ***/
	GetVol(buf, &vref);
    CtoPstr(name);
	Create(name, vref, '????', 'TEXT');
    PtoCstr(name);
  }
#endif
  return (fopen(name,mode));
}

FILE *osbopen(name,mode)
  char *name,*mode;
{
  char nmode[4];
  strcpy(nmode,mode); strcat(nmode,"b");
  return (fopen(name,nmode));
}

int osclose(fp)
  FILE *fp;
{
  return (fclose(fp));
}

int osagetc(fp)
  FILE *fp;
{
  return (getc(fp));
}

int osbgetc(fp)
  FILE *fp;
{
  return (getc(fp));
}

int osaputc(ch,fp)
  int ch; FILE *fp;
{
  return (putc(ch,fp));
}

int osbputc(ch,fp)
  int ch; FILE *fp;
{
  return (putc(ch,fp));
}

int ostgetc()
{
  int ch;

  while (! ustreamp(input_stream) || (ch = xlgetc(input_stream)) == EOF)
    waitforline();
  if (tfp) osaputc(ch, tfp);
  return (filter_char(ch));
}

filter_char(c)
	int c;
{
#ifndef MPWC
  if (c == '\r') c = '\n';
#endif MPWC
  return(c);
}

int ostputc(ch)
  int ch;
{
#ifndef MPWC
  ch = (ch == '\n') ? '\r' : ch;;
#endif MPWC
  if (tfp) osaputc(ch, tfp);
  TtyPutc(ch);
  return (1);
}

/* oscheck - check for control characters during execution */
/* command-period is the interrupt for the mac             */
oscheck()
{
  EventRecord theEvent;
  char c;
  static EventRecord lastEvent;
  
  if (EventAvail(autoKeyMask, &theEvent))
    if (theEvent.when == lastEvent.when) {
      GetNextEvent(autoKeyMask, &theEvent);
      c = theEvent.message & charCodeMask;
      if ((theEvent.modifiers & cmdKey) && (c == '.')) {
	    FlushEvents (everyEvent - diskMask, 0 );      
        xltoplevel();
      }
    }
  else lastEvent = theEvent;
}

/* osflush - flush the input line buffer */
osflush()
{
  ostputc(RETURNCHAR);
  while (ustreamp(input_stream) && xlgetc(input_stream) != EOF)
    ;
}

ossymbols()
{
  statsymbols();
}

osfinit()
{
  statfinit();
  warn_low_memory(250000);
}

osreset()
{
  in_send = FALSE;
  StGWResetBuffer();
}

/*
  Show a window if it's not visible.  Select the window FIRST, then
  show it, so that it comes up in front.  Otherwise it will be drawn
  in back then brought to the front, which is ugly.
*/

MyShowWindow (wind)
  WindowPeek wind;
{
  SelectWindow ((WindowPtr) wind);
  ShowWindow ((WindowPtr) wind);
}

waitforline()
{
  SkelMain ();
  unload_segments(FALSE);
}
	
getttyline(s)
	LVAL s;
{
  input_stream = getvalue(s_input_stream);
  SkelWhoa();
}

static background_tasks()
{
  LVAL task, queue, oldenv, oldfenv;
    
  queue = getvalue(xlenter("*EVENT-QUEUE*"));
  if (consp(queue)) {
  
    /* set the lexical environment to null */
    xlstkcheck(2);
    xlsave(oldenv);
    xlsave(oldfenv);
    oldenv = xlenv; xlenv = NIL;
    oldfenv = xlfenv; xlfenv = NIL;

    task = car(queue);
  	setvalue(xlenter("*EVENT-QUEUE*"), cdr(queue));
  	xleval(task);

    /* reset the environment */
    xlenv = oldenv;
    xlfenv = oldfenv;
    xlpopn(2);
  }
  unload_segments(FALSE);
}

set_gc_cursor(on)
	int on;
{
  static Cursor;
  CursHandle c;
  
  if (on && (c = GetCursor(GC_CURS_RES)) != NIL) SetCursor(*c);
  else {
    SetCursor(&arrow);
	oscheck();
    warn_low_memory(75000);
  }
}

#ifdef OPTIMIZE
extern xsbracket_search();                /* optimize.c */
#endif OPTIMIZE
extern init_objects();                    /* objectinit.c */
extern ivector();                         /* linalg.c */
extern crludcmp();                        /* ludecomp.c */
extern choldecomp();                      /* cholesky.c */
extern qrdecomp();                        /* qrdecomp.c */
extern svdcmp();                          /* svdecomp.c */
extern iview_hist_allocate();             /* xshistogram.c */
extern iview_list_allocate();             /* xsnamelist.c */
extern iview_scatmat_allocate();          /* xsscatmat.c */
extern iview_spin_allocate();             /* xsspin.c */
extern GetDialogItemData();               /* dialogs1.c */
extern doDialog();                        /* dialogs2.c */
extern check_point_list();                /* dialogs3.c */
extern xsmenu_isnew();                    /* menus.c */
extern matrixp();                         /* matrices1.c */
extern xsmakesweepmatrix();               /* matrices2.c */
extern xsrbinomialcdf();                  /* ddistributions.c */
extern xsrnormalcdf();                    /* distributions.c */
extern betabase();                        /* betabase.c */
extern xlinit();                          /* xlinit.c */
extern iview_plot2d_add_points();         /* xsscatterplot.c */
/*extern atof(); */                           /* unix library */
extern numergrad();                       /* derivatives.c */
extern evalfront();                       /* functions.c */
extern lowess();                          /* lowess.c */
extern xscall_cfun();                     /* macdynload.c */
extern cfft();                            /* cfft.c */

static unload_segments(initial)
	int initial;
{
  if (xldebug != 0) return;
#ifdef MPWC
  if (! (initial && is_small_machine()) && (FreeMem() > 200000)) return;
#else
  if (FreeMem() > 200000) return;
#endif MPWC
  if (in_send) return;

#ifdef THINK_C
#ifdef _MC68881_
  UnloadSeg(_exp);
#else
  UnloadSeg(exp);
#endif
#ifdef OPTIMIZE
  UnloadSeg(xsbracket_search);
#endif OPTIMIZE
  UnloadSeg(init_objects);
  UnloadSeg(ivector);
  UnloadSeg(crludcmp);
  UnloadSeg(choldecomp);
  UnloadSeg(qrdecomp);
  UnloadSeg(svdcmp);
  UnloadSeg(iview_hist_allocate);
  UnloadSeg(iview_list_allocate);
  UnloadSeg(iview_scatmat_allocate);
  UnloadSeg(iview_spin_allocate);
  UnloadSeg(GetDialogItemData);
  UnloadSeg(doDialog);
  UnloadSeg(check_point_list);
  UnloadSeg(xsmenu_isnew);
  UnloadSeg(matrixp);
  UnloadSeg(xsmakesweepmatrix);
  UnloadSeg(xsrbinomialcdf);
  UnloadSeg(xsrnormalcdf);
  UnloadSeg(betabase);
  UnloadSeg(xlinit);
  UnloadSeg(iview_plot2d_add_points);
  UnloadSeg(atof);
  UnloadSeg(Stdio_MacInit);
  UnloadSeg(numergrad);
  UnloadSeg(evalfront);
  UnloadSeg(xscall_cfun);
  UnloadSeg(cfft);
#endif THINK_C
#ifdef MPWC
  UnloadSeg(init_objects);
  UnloadSeg(ivector);
  UnloadSeg(crludcmp);
  UnloadSeg(choldecomp);
  UnloadSeg(qrdecomp);
  UnloadSeg(svdcmp);
  UnloadSeg(iview_hist_allocate);
  UnloadSeg(iview_list_allocate);
  UnloadSeg(iview_scatmat_allocate);
  UnloadSeg(iview_spin_allocate);
  UnloadSeg(GetDialogItemData);
  UnloadSeg(doDialog);
  UnloadSeg(check_point_list);
  UnloadSeg(xsmenu_isnew);
  UnloadSeg(matrixp);
  UnloadSeg(xsmakesweepmatrix);
  UnloadSeg(xsrbinomialcdf);
  UnloadSeg(xsrnormalcdf);
  UnloadSeg(betabase);
  if (! initial) UnloadSeg(xlinit);
  UnloadSeg(iview_plot2d_add_points);
  UnloadSeg(numergrad);
  UnloadSeg(evalfront);
  UnloadSeg(lowess);
  UnloadSeg(xscall_cfun);
  UnloadSeg(cfft);
#endif MPWC
}

#ifdef DODO
static event_hook(theEvent)
	EventRecord	*theEvent;
{
  GrafPtr port;
  
  switch(theEvent->what) {
  case mouseDown: 
    if (FindWindow (theEvent->where, &port) == inMenuBar) unload_segments(FALSE);
    break;
  case keyDown:
  case autoKey:
    if (theEvent->modifiers & cmdKey) unload_segments(FALSE);
    break;
  }
  return(FALSE);
}
#endif DODO

max(x, y)
	int x, y;
{
  return((x > y) ? x : y);
}

min(x, y)
	int x, y;
{
  return((x < y) ? x : y);
}

is_small_machine() { return(FreeMem() < 600000); }

#define TIMEGAP 120L
#ifdef MPWC
#define WARNSTRING "\pMemory is down to %ldK. \nClose plots and \"undef\" variables."
#else
#define WARNSTRING "\pMemory is down to %ldK. \rClose plots and \"undef\" variables."
#endif MPWC
unsigned long time();

static warn_low_memory(space)
	long space;
{
  char s[100];
  unsigned long tm;
  static unsigned long old_tm = 0L;
  static int inited = FALSE;
  long free;
  
  if (FreeMem() < space) {
    MaxMem(&free);
    free = FreeMem();
    if (free < space) {
      tm = time((unsigned long *) NULL);
      if (! inited || tm > old_tm + TIMEGAP) {
        old_tm = tm;
        sprintf(s, WARNSTRING, free / 1000);
        (void) FakeAlert (s, "\p", "\p", "\p", 1, 1, "\pOK", "\p", "\p");
      }
      inited = TRUE;
    }
  }
}

maximum_memory()
{
  long size;
  MaxMem(&size);
}

#ifdef _MC68881_
static check_MC68881()
{
  SysEnvRec r;

  SysEnvirons(1, &r);
  if (! r.hasFPU) {
    FakeAlert ("\pThis version requres the MC68881", "\p", "\p", "\p",
               1, 1, "\pOK", "\p", "\p");
    exit();
  }
}
#endif _MC68881_

unsigned long ticks_per_second() { return((unsigned long) 60); }

unsigned long run_tick_count()
{
  return((unsigned long) TickCount());
}

unsigned long real_tick_count() 
{
  return((unsigned long) (60 * (time((unsigned long *) NULL) - time_stamp)));
}

/* thee ought to be a sensible way to do this, but I can't figure it out yet */
get_directory(s)
	char *s;
{
  strcpy(s, "");
}

#ifdef MPWC
#undef SysBeep
SYSBEEPMPW(x)
	int x;
{
  SysBeep(x);
}
#endif MPWC
