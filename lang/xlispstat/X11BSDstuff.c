/* X11BSDstuff.c - X11 BSD interface routines for xlisp */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

# include <signal.h>
# include <sys/types.h>
# include <sys/times.h>
# include <sys/time.h>
# include "xlisp.h"
# include "version.h"
# define nil 0L

/* externals */
extern FILE *tfp;
extern long time_stamp;

/* symbols */
LVAL s_true, s_ivalue, s_sublist, s_select, s_elt, s_vector;
LVAL s_list, s_ielement, s_icontents, s_displacedto, s_arrayident;
LVAL s_subarray, a_float, s_pi;

/* static variables to protect gc from interrupt */
static int in_gc = FALSE, gc_interrupt = FALSE;

osinit(name)
  char *name;
{
  extern intercatch(), fpecatch();

  if (signal(SIGINT, SIG_IGN) != SIG_IGN)
    signal(SIGINT, intercatch);
  signal(SIGFPE, fpecatch);
  printf(name);
  printf("\nXLISP-STAT version %s, Copyright (c) 1989, by Luke Tierney.\n",
	 XLISPSTAT_VERSION);
  printf("Several files will be loaded; this may take a few minutes.\n\n\n");
}

osfinish()
{
  StX11Finish();
}

osreset()
{
  if (StHasWindows()) {
    StGWResetBuffer();
    StX11DialogReset();
    StX11ReleaseButton();
  }
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
  ch = x11_get_char();
  if (tfp) osaputc(ch,tfp);
  return(ch);
}

int ostputc(ch)
  int ch;
{
    putchar(ch);
    if (tfp)
	osaputc(ch,tfp);
    return (1);
}

osflush()
{
}

oscheck()
{
}

ossymbols()
{
  statsymbols();
}

osfinit()
{
  statfinit();
}

intercatch()
{
  signal(SIGINT, intercatch);
  if (in_gc) gc_interrupt = TRUE;
  else xltoplevel();
}

fpecatch()
{
  signal(SIGFPE, fpecatch);
  xlfail("floating point error");
}

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

set_gc_cursor(on) 
     int on;
{
  if (on) in_gc = TRUE;
  else {
    if (gc_interrupt) {
      gc_interrupt = FALSE;
      in_gc = FALSE;
      xltoplevel();
    }
    else in_gc = FALSE;
  }
}

SysBeep(n)
     int n;
{
  n = n / 10 - 1;
  do {
    printf("\007");
  } while (n-- > 0);
  fflush(stdout);
}

#ifndef HZ
#define HZ 60
#endif

unsigned long ticks_per_second() { return((unsigned long) HZ); }

unsigned long run_tick_count()
{
  struct tms tm;

  times(&tm);
  
  return((unsigned long) tm.tms_utime + tm.tms_stime);
}

unsigned long real_tick_count() 
{
  return((unsigned long) (60 * (time((unsigned long *) NULL) - time_stamp)));
}

/* The following routines will have to be changed for non-BSD systems. At
 * present they use a BSD-specific ioctl call as well as file structure
 * information to determine when terminal input characters are available.
 * While no characters are available events are processed. 
 *
 * An alternative polling system can be constructed using alarm or
 * setitimer. The alarm aproach is used in the S X11 and NeWS drivers.
 */

#ifdef DODO
static x11_get_char()
{
  int ch;

  fflush(stdout);
  do {
    StPollEvent();
  } while (! char_available());
  ch =  getchar();
  return(ch);
}

static char_available()
{
  FILE *file = stdin;
  int c = 0;
  
  if (file->_cnt > 0) return(TRUE);
  ioctl(fileno(file), FIONREAD, &c);
  if (c > 0) return(TRUE);
  else return(StBlockForInput());
}
#endif /* DODO */

static int in_a_line = FALSE;

LOCAL void reset_input() { in_a_line = FALSE; }

LOCAL int x11_get_char()
{
  int ch;

  fflush(stdout);
  if (! in_a_line) {
    do {
      StPollEvent();
    } while (! line_available());
  }
  ch =  getchar();
  in_a_line = (ch == '\n') ? FALSE : TRUE;
  return(ch);
}

LOCAL int line_available()
{
  FILE *file = stdin;
  int result;
  fd_set readmask;
  static struct timeval tv = {0, 0};
  static int ndfs = 0;

  if (ndfs == 0) ndfs = fileno(stdin) + 1; /*** is this right? ***/

  FD_ZERO(&readmask);
  FD_SET(fileno(stdin), &readmask);

  result = select(ndfs, &readmask, nil, nil, &tv);
  if (result > 0) return(TRUE);
  /* *** should merge the select here with the one for blocking ***/
  else return(StBlockForInput());
}

/* 
 * This routine is used to flush input to the tty during modal dialogs.
 * The ioctl call seems to do the job on BDS systems but may not exist
 * (in this form) on other systems. The ifdef is a hack to see if the
 * call is available.
 */
StX11FlushStdin()
{
#ifdef TIOCFLUSH
  int c = 0;
  ioctl(fileno(stdin), TIOCFLUSH, &c);
#else
  fflush(stdin);
#endif
}

#undef getenv
extern char *getenv();

char *getunixenv(s) { return(getenv(s)); }
