/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Michael Fischbein.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)print.c	5.22 (Berkeley) 5/10/90";
#endif /* not lint */

#include "client.h"
#include <grp.h>
#include <pwd.h>
#include <utmp.h>
#include "ls.h"

#ifndef ANSI_PROTOTYPES
static void printtime();
static int printtype();
static int printaname();

#else /* ANSI_PROTOTYPES */

static void printtime(time_t ftime);
static int printtype(u_long mode);
static int printaname(LS *lp);
#endif /* ANSI_PROTOTYPES */

#define BLK(A) (((A)+1023)/1024)

void
#ifndef ANSI_PROTOTYPES
printscol(stats, num)
	LS *stats;
	int num;
#else /* ANSI_PROTOTYPES */
printscol(LS *stats, int num)
#endif /* ANSI_PROTOTYPES */
{
	for (; num--; ++stats) {
		(void)printaname(stats);
		(void)putc('\n', STDOUT);
	}
}

void
#ifndef ANSI_PROTOTYPES
printlong(stats, num)
	LS *stats;
	int num;
#else /* ANSI_PROTOTYPES */
printlong(LS *stats, int num)
#endif /* ANSI_PROTOTYPES */
{
	char *modep;

	if (f_total)
		ffprintf(STDOUT, "total %lu\n", stats[0].lst_btotal);
	for (; num--; ++stats) {
		if (f_inode) ffprintf(STDOUT, "%6lu ", stats->lstat.st_ino);
		if (f_size ) ffprintf(STDOUT, "%4ld ", BLK(stats->lstat.st_size));
		modep = ((S_IFDIR & stats->lstat.st_mode)) ? "drwxrwxrwx"
							: "-rw-rw-rw-" ;

		ffprintf(STDOUT, "%s %3u %-*s ",
			modep, stats->lstat.st_nlink, 8, "nobody");

		if (f_group)
			ffprintf(STDOUT, "%-*s ", 8, "nogroup");
		else
			ffprintf(STDOUT, "%8ld ", stats->lstat.st_size);

		if (f_accesstime)
			printtime(stats->lstat.st_atime);
		else if (f_statustime)
			printtime(stats->lstat.st_ctime);
		else
			printtime(stats->lstat.st_mtime);
		ffprintf(STDOUT, "%s", stats->name);
		if (f_type)
			(void)printtype(stats->lstat.st_mode);
		(void)putc('\n', STDOUT);
	}
}

#define	TAB	8

void
#ifndef ANSI_PROTOTYPES
printcol(stats, num)
	LS *stats;
	int num;
#else /* ANSI_PROTOTYPES */
printcol(LS *stats, int num)
#endif /* ANSI_PROTOTYPES */
{
	extern int termwidth;
	int base, chcnt, cnt, col, colwidth;
	int endcol, numcols, numrows, row;

	colwidth = stats[0].lst_maxlen;
	if (f_inode)
		colwidth += 6;
	if (f_size)
		colwidth += 5;
	if (f_type)
		colwidth += 1;

	colwidth = (colwidth + TAB) & ~(TAB - 1);
	if (termwidth < 2 * colwidth) {
		printscol(stats, num);
		return;
	}

	numcols = termwidth / colwidth;
	numrows = num / numcols;
	if (num % numcols)
		++numrows;

	if (f_size && f_total)
		ffprintf(STDOUT, "total %lu\n", stats[0].lst_btotal);
	for (row = 0; row < numrows; ++row) {
		endcol = colwidth;
		for (base = row, chcnt = col = 0; col < numcols; ++col) {
			chcnt += printaname(stats + base);
			if ((base += numrows) >= num)
				break;
			while ((cnt = ((chcnt + TAB) & ~(TAB - 1))) <= endcol) {
				(void)putc('\t', STDOUT);
				chcnt = cnt;
			}
			endcol += colwidth;
		}
		(void)putc('\n', STDOUT);
	}
}

/*
 * print [inode] [size] name
 * return # of characters printed, no trailing characters
 */
static int
#ifndef ANSI_PROTOTYPES
printaname(lp)
	LS *lp;
#else /* ANSI_PROTOTYPES */
printaname(LS *lp)
#endif /* ANSI_PROTOTYPES */
{
	int chcnt;

	chcnt = 0;

	if (f_inode)
	{
	    ffprintf(STDOUT, "%5lu ", lp->lstat.st_ino);
	    chcnt += 6;
	}

	if (f_size)
	{
	    ffprintf(STDOUT, "%4ld ", BLK(lp->lstat.st_size));
	    chcnt += 5;
	}

	/* looks like some printf's in this world are dumb... --- pgr*/
	/* chcnt += printf("%s", lp->name); */
	ffprintf(STDOUT, "%s", lp->name); chcnt += strlen(lp->name);

	if (f_type)
	{
	    chcnt += printtype(lp->lstat.st_mode);
	}

	return(chcnt);
}

static void
#ifndef ANSI_PROTOTYPES
printtime(ftime)
	time_t ftime;
#else /* ANSI_PROTOTYPES */
printtime(time_t ftime)
#endif /* ANSI_PROTOTYPES */
{
	int i;
	char *longstring;

	longstring = ctime((long *)&ftime);
	for (i = 4; i < 11; ++i)
		(void)putc(longstring[i], STDOUT);

#define	SIXMONTHS	((365 / 2) * 24 * 60 * 60)
	if (ftime + SIXMONTHS > time((time_t *)NULL))
		for (i = 11; i < 16; ++i)
			(void)putc(longstring[i], STDOUT);
	else {
		(void)putc(' ', STDOUT);
		for (i = 20; i < 24; ++i)
			(void)putc(longstring[i], STDOUT);
	}
	(void)putc(' ', STDOUT);
}

static int
#ifndef ANSI_PROTOTYPES
printtype(mode)
	u_long mode;
#else /* ANSI_PROTOTYPES */
printtype(u_long mode)
#endif /* ANSI_PROTOTYPES */
{
	switch(mode & S_IFMT) {
	case S_IFDIR:
		(void)putc('/', STDOUT);
		return(1);
	}
	return(0);
}
