/* exp_win.c - window support

Written by: Don Libes, NIST, 10/25/93

This file is in the public domain.  However, the author and NIST
would appreciate credit if you use this file or parts of it.

*/

#include "expect_cf.h"
#include "tcl.h"

/* sure hope this gets everyone's window size definitions */
#ifdef HAVE_TERMIOS
#include <termios.h>
#else
#include <sys/ioctl.h>
#endif

#include "exp_tty.h"
#include "exp_win.h"

#ifdef TIOCGWINSZ
typedef struct winsize exp_winsize;
#define columns ws_col
#define rows ws_row
#define EXP_WIN
#endif

#if !defined(EXP_WIN) && defined(TIOCGSIZE)
typedef struct ttysize exp_winsize;
#define columns ts_cols
#define rows ts_lines
#define EXP_WIN
#endif

#if !defined(EXP_WIN)
typedef struct {
	int columns;
	int rows;
} exp_winsize;
#endif

static exp_winsize winsize = {0, 0};
static exp_winsize win2size = {0, 0};

int exp_window_size_set(fd)
int fd;
{
#ifdef TIOCSWINSZ
	ioctl(fd,TIOCSWINSZ,&winsize);
#endif
#if defined(TIOCSSIZE) && !defined(TIOCSWINSZ)
	ioctl(fd,TIOCSSIZE,&winsize);
#endif
}

int exp_window_size_get(fd)
int fd;
{
#ifdef TIOCGWINSZ
	ioctl(fd,TIOCGWINSZ,&winsize);
#endif
#if defined(TIOCGSIZE) && !defined(TIOCGWINSZ)
	ioctl(fd,TIOCGSIZE,&winsize);
#endif
#if !defined(EXP_WIN)
	winsize.rows = 0;
	winsize.columns = 0;
#endif
}

void
exp_win_rows_set(rows)
char *rows;
{
	winsize.rows = atoi(rows);
	exp_window_size_set(exp_dev_tty);
}

void
exp_win_rows_get(rows)
char *rows;
{
	exp_window_size_get(exp_dev_tty);
	sprintf(rows,"%d",winsize.rows);
}

void
exp_win_columns_set(columns)
char *columns;
{
	winsize.columns = atoi(columns);
	exp_window_size_set(exp_dev_tty);
}

void
exp_win_columns_get(columns)
char *columns;
{
	exp_window_size_get(exp_dev_tty);
	sprintf(columns,"%d",winsize.columns);
}

/*
 * separate copy of everything above - used for handling user stty requests
 */

int exp_win2_size_set(fd)
int fd;
{
#ifdef TIOCSWINSZ
			ioctl(fd,TIOCSWINSZ,&win2size);
#endif
#if defined(TIOCSSIZE) && !defined(TIOCSWINSZ)
			ioctl(fd,TIOCSSIZE,&win2size);
#endif
}

int exp_win2_size_get(fd)
int fd;
{
#ifdef TIOCGWINSZ
	ioctl(fd,TIOCGWINSZ,&win2size);
#endif
#if defined(TIOCGSIZE) && !defined(TIOCGWINSZ)
	ioctl(fd,TIOCGSIZE,&win2size);
#endif
}

void
exp_win2_rows_set(fd,rows)
int fd;
char *rows;
{
	exp_win2_size_get(fd);
	win2size.rows = atoi(rows);
	exp_win2_size_set(fd);
}

void
exp_win2_rows_get(fd,rows)
int fd;
char *rows;
{
	exp_win2_size_get(fd);
	sprintf(rows,"%d",win2size.rows);
#if !defined(EXP_WIN)
	win2size.rows = 0;
	win2size.columns = 0;
#endif
}

void
exp_win2_columns_set(fd,columns)
int fd;
char *columns;
{
	exp_win2_size_get(fd);
	win2size.columns = atoi(columns);
	exp_win2_size_set(fd);
}

void
exp_win2_columns_get(fd,columns)
int fd;
char *columns;
{
	exp_win2_size_get(fd);
	sprintf(columns,"%d",win2size.columns);
}
