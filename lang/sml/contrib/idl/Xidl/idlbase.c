/*

	IDL:    Bernard Sufrin, Oxford
		Generic IDL base
		@(#)idlbase.c	2.1 93/03/07 00:58:09
*/

#include <sys/types.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include "idlbase.h"

extern  int idlcontinue;
	int idlcontinue=1;

#define SBUFSIZE 50000
static  char  stringbuffer[SBUFSIZE];
static  char  *stringbufp = stringbuffer;

extern  void  idlreset()
{ stringbufp = stringbuffer;
  fflush(stdout);
}

FILE    *idlin, *idlout;

extern  int idlcaninput()
{       int count[2];
	if (ioctl(fileno(idlin), FIONREAD, count)>=0) return count[0];
	return 0;
}

extern  int     idlidle(ms)
	long    ms;
{       int     rfds=1 << fileno(idlin),
		wfds=0,
		xfds=0;
	struct  timeval timeout;
	timeout.tv_sec =  ms / 1000;
	timeout.tv_usec=  10 * (ms % 1000);
	return  select(32, &rfds, &wfds, &xfds, &timeout);
}

extern int  idlinputchannel()   { return fileno(idlin); }

extern void idlflush()
	    { if (idlout->_cnt!=idlout->_bufsiz) fflush(idlout);}

extern idlinit(stdio) string stdio;
{  int c0 = fileno(stdin);
   int c1 = fileno(stdout);
   int C0 = dup(c0);
   int C1 = dup(c1);
   idlin  = fdopen(C0, "r"); setbuf(idlin, NULL);
   idlout = fdopen(C1, "w");
   fclose(stdin);
   fclose(stdout);
   if (stdio!=NULL)
      { freopen(stdio, "r", stdin);
	freopen(stdio, "w", stdout);
      }
   idlcontinue=1;
}

/*
	This is safer than the use of getc(idlin)
	if the X library makes idlin non-blocking.
*/
int     getch(idlin) FILE *idlin;
	{       int c=getc(idlin);

		if (c == EOF)
		{  fprintf(stderr, "[[[GETC=EOF]]]"); fflush(stderr);
		   while (c == EOF) c=getc(idlin);
		}

		return (c&0377);
	}

#       define GETCH    (getch(idlin))
#       define PUTCH(c) putc(c, idlout)

short   read_short()
{       fflush(idlout);
	{ int i = 0;
	  i = GETCH;
	  i = (i << 8) | GETCH;
	  return i;
	};
}

void    write_short(n) short n;
{
	PUTCH((n>> 8)&255);
	PUTCH(n&255);
}

int     read_int()
{       fflush(idlout);
	{ int i = 0;
	  i = GETCH;
	  i = (i << 8) | GETCH;
	  i = (i << 8) | GETCH;
	  i = (i << 8) | GETCH;
	  return i;
	};
}

void    write_int(n) int n;
{
	PUTCH((n>>24)&255);
	PUTCH((n>>16)&255);
	PUTCH((n>> 8)&255);
	PUTCH(n&255);
}

address read_address()
{       fflush(idlout);
	{ long i = 0;
	  i = (long) GETCH;
	  i = (long)(((long)i) << 8) | (long) GETCH;
	  i = (long)(((long)i) << 8) | (long) GETCH;
	  i = (long)(((long)i) << 8) | (long) GETCH;
	  return (address) i;
	};
}

void    write_address(n) address n;
{
	PUTCH(((long)n>>24)&255);
	PUTCH(((long)n>>16)&255);
	PUTCH(((long)n>> 8)&255);
	PUTCH((long)n&255);
}

unit    read_unit()
{       fflush(idlout);
	(unit) GETCH;
}

void    write_unit()
{
	PUTCH(0);
}
string  read_string()
{       int l    = read_int();
	string r = stringbufp;
	if (stringbufp-stringbuffer+l>SBUFSIZE)
	{       sprintf("[String buffer overflow]\n");
		exit(2);
	};
	while (l>0) { *stringbufp = GETCH; stringbufp++; l--; };
	*stringbufp++ = '\000';
	return r;
}

void    write_string(s) string s;
{
	write_int(strlen(s));
	while (*s) { PUTCH(*s); s++; };
}

bool    read_bool()
{       return ((bool) (0!=GETCH));
}

void    write_bool(b) bool b;
{       PUTCH(b?1:0);
}

char    read_byte()
{       return (GETCH);
}

void    write_byte(b) char b;
{
	PUTCH(b);
}
