/* sunstuff.c - suntools interface routines for xlisp                  */
/* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney                  */
/* Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz    */
/* You may give out copies of this software; for conditions see the    */
/* file COPYING included with this distribution.                       */

# include <signal.h>
# include "xlisp.h"
# include "version.h"
# include <sys/types.h>
# include <sys/times.h>
# include <sunwindow/notify.h>

/* externals */
extern FILE *tfp;
extern long time_stamp;

/* symbols */
LVAL s_true, s_ielement, s_ivalue, s_sublist, s_select, s_elt, s_vector;
LVAL s_list, s_ielement, s_icontents, s_displacedto, s_arrayident;
LVAL s_subarray, a_float, s_pi;

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

osfinish() {}
osreset()
{
  StGWResetBuffer();
  DialogReset();
  MouseReset();
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

int jump_to_top = FALSE;

int ostgetc()
{
  char ch;

  if (jump_to_top) {
    notify_do_dispatch();
    jump_to_top = FALSE;
    xltoplevel();
  }

  ch = getchar();
  if (tfp)
    osaputc(ch,tfp);
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
  if (jump_to_top) {
    notify_do_dispatch();
    jump_to_top = FALSE;
    xltoplevel();
  }
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
  notify_no_dispatch();
  jump_to_top = TRUE;
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

set_gc_cursor() {}

SysBeep(n)
     int n;
{
  n = n % 10;
  do {
    printf("\007");
  } while (n-- > 0);
  fflush(stdout);
}

#ifdef SUN3X
int (*signal(sig, fn))()
     int sig, (*fn)();
{
  return((int (*)()) notify_set_signal_func(sig, fn, sig, NOTIFY_ASYNC));
}
#else
void (*signal(sig, fn))()
     int sig, (*fn)();
{
  return((void (*)()) notify_set_signal_func(sig, fn, sig, NOTIFY_ASYNC));
}
#endif SUN3X

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

#undef getenv
extern char *getenv();

char *getunixenv(s) { return(getenv(s)); }
