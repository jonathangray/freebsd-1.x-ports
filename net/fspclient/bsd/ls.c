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
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)ls.c	5.42 (Berkeley) 5/17/90";
#endif /* not lint */

#include "client.h"
#include <sys/ioctl.h>
#include "ls.h"

#ifndef ANSI_PROTOTYPES
static int (*sortfcn)();
static void (*printfcn)();

static void doargs();
static void displaydir();
static void subdir();
static int tabdir();

#else /* ANSI_PROTOTYPES */

static int (*sortfcn)(LS *, LS *);
static void (*printfcn)(LS *stats, int num);

static void doargs(int argc, char **argv);
static void displaydir(LS *stats, int num);
static void subdir(LS *lp);
static int tabdir(LS *lp, LS **s_stats);
#endif /* ANSI_PROTOTYPES */

int termwidth;			/* default terminal width */
static int ls_retval;

/* flags */
int f_accesstime;		/* use time of last access */
static int f_column;		/* columnated format */
static int f_dirname;		/* if precede with directory name */
int f_group;			/* show group ownership of a file */
static int f_ignorelink;	/* indirect through symbolic link operands */
int f_inode;			/* print inode */
static int f_kblocks;		/* print size in kilobytes */
static int f_listalldot;	/* list . and .. as well */
static int f_listdir;		/* list actual directory, not contents */
static int f_listdot;		/* list files beginning with . */
static int f_longform;		/* long listing format */
static int f_needstat;		/* if need to stat files */
static int f_newline;		/* if precede with newline */
static int f_nonprint;		/* show unprintables as ? */
static int f_nosort;		/* don't sort output */
static int f_recursive;		/* ls subdirectories also */
static int f_reversesort;	/* reverse whatever sort is used */
static int f_singlecol;		/* use single column output */
int f_size;			/* list size in short listing */
int f_statustime;		/* use time of last mode change */
static int f_timesort;		/* sort by time vice name */
int f_total;			/* if precede with "total" line */
int f_type;			/* add type character for non-regular files */

extern time_t sincetime;

int
#ifndef ANSI_PROTOTYPES
fls_main(argc, argv)
	int argc;
	char **argv;
#else /* ANSI_PROTOTYPES */
fls_main(int argc, char **argv)
#endif /* ANSI_PROTOTYPES */
{
#ifndef ANSI_PROTOTYPES
	extern int getopt();
#else /* ANSI_PROTOTYPES */
	extern int getopt(int argc, char **argv, char *opts);
#endif /* ANSI_PROTOTYPES */
	extern int optind, opterr;
	int ch, exit_after_options;
	char *p;

	ls_retval = 0;

	f_accesstime = f_column = f_group = f_ignorelink = f_inode =
		f_kblocks = f_listalldot = f_listdir = f_listdot =
		f_longform = f_needstat = f_newline = f_nonprint =
		f_nosort = f_recursive = f_reversesort = f_singlecol =
		f_size = f_statustime = f_dirname = f_timesort =
		f_total = f_type = 0;

	/* terminal defaults to -Cq, non-terminal defaults to -1 */
	termwidth = 80;
	if (isatty(1)) {
		f_nonprint = 1;
#ifdef TIOCGWINSZ
		{   struct winsize win;
		    if (ioctl(1, TIOCGWINSZ, &win) == -1 || !win.ws_col) {
			    if ((p = (char *)getenv("COLUMNS")))
				    termwidth = atoi(p);
		    }
		    else
			    termwidth = win.ws_col;
		}
#endif
		f_column = 1;
	} else
		f_singlecol = 1;

	/* root is -A automatically */
#if 0
	/* within FSP, if there are dot files to be seen, then the person
	** *must* be root
	*/
	if (!getuid())
		f_listdot = 1;
#endif
	/* within my mechanism, there are dot files iff you are root */
	f_listdot = 1;

	optind = 1;
	opterr = 1;
	exit_after_options = 0;

	while ((ch = getopt(argc, argv, "1ACFLRacdfgiklqrstu")) != EOF) {
		switch (ch) {
		/*
		 * -1, -C and -l all override each other
		 * so shell aliasing works right
		 */
		case '1':
			f_singlecol = 1;
			f_column = f_longform = 0;
			break;
		case 'C':
			f_column = 1;
			f_longform = f_singlecol = 0;
			break;
		case 'l':
			f_longform = 1;
			f_column = f_singlecol = 0;
			break;
		/* -c and -u override each other */
		case 'c':
			f_statustime = 1;
			f_accesstime = 0;
			break;
		case 'u':
			f_accesstime = 1;
			f_statustime = 0;
			break;
		case 'F':
			f_type = 1;
			break;
		case 'L':
			f_ignorelink = 1;
			break;
		case 'R':
			f_recursive = 1;
			break;
		case 'a':
			f_listalldot = 1;
			/* FALLTHROUGH */
		case 'A':
			f_listdot = 1;
			break;
		case 'd':
			f_listdir = 1;
			break;
		case 'f':
			f_nosort = 1;
			break;
		case 'g':
			f_group = 1;
			break;
		case 'i':
			f_inode = 1;
			break;
		case 'k':
			f_kblocks = 1;
			break;
		case 'q':
			f_nonprint = 1;
			break;
		case 'r':
			f_reversesort = 1;
			break;
		case 's':
			f_size = 1;
			break;
		case 't':
			f_timesort = 1;
			break;
		default:
		case '?':
			exit_after_options = 1;
		}
	}

	if (exit_after_options)
		usage();

	argc -= optind;
	argv += optind;

	/* -d turns off -R */
	if (f_listdir)
		f_recursive = 0;

	/* if need to stat files */
	f_needstat = f_longform || f_recursive || f_timesort ||
	    f_size || f_type || (sincetime > 0);

	/* select a sort function */
	if (f_reversesort) {
		if (!f_timesort)
			sortfcn = revnamecmp;
		else if (f_accesstime)
			sortfcn = revacccmp;
		else if (f_statustime)
			sortfcn = revstatcmp;
		else /* use modification time */
			sortfcn = revmodcmp;
	} else {
		if (!f_timesort)
			sortfcn = namecmp;
		else if (f_accesstime)
			sortfcn = acccmp;
		else if (f_statustime)
			sortfcn = statcmp;
		else /* use modification time */
			sortfcn = modcmp;
	}

	/* select a print function */
	if (f_singlecol)
		printfcn = printscol;
	else if (f_longform)
		printfcn = printlong;
	else
		printfcn = printcol;

	if (!argc) {
		char *myargv[2];
		argc = 1;
		myargv[0] = ".";
		myargv[1] = NULL;
		doargs(argc, myargv);
	}
	else
		doargs(argc, argv);

	return ls_retval;
}

static char path[2*1024 + 1];
static char *endofpath;

static void
#ifndef ANSI_PROTOTYPES
tidyup_mem(gavi, gav, dstats, rstats)
	int gavi;
	char ***gav;
	LS *dstats;
	LS *rstats;
#else /* ANSI_PROTOTYPES */
tidyup_mem(int gavi, char ***gav, LS *dstats, LS *rstats)
#endif /* ANSI_PROTOTYPES */
{
	int i;

	for (i = 0; i < gavi; i++)
		free_glob(gav[i]);

	(void)free(gav);

	if (dstats) (void)free((char*)dstats);
	if (rstats) (void)free((char*)rstats);
}

static void
#ifndef ANSI_PROTOTYPES
doargs(argc, argv)
	int argc;
	char **argv;
#else /* ANSI_PROTOTYPES */
doargs(int argc, char **argv)
#endif /* ANSI_PROTOTYPES */
{
	register LS *dstatp, *rstatp;
	register int cnt, dircnt, dirmax, maxlen, regcnt, regmax;
	LS *dstats, *rstats;
	struct stat sb;
	char top[2*1024 + 1], **av, *av2[2];
	unsigned long blocks;
#ifndef ANSI_PROTOTYPES
	extern char **glob();
#else /* ANSI_PROTOTYPES */
	extern char **glob(char *);
#endif /* ANSI_PROTOTYPES */
	char ***gav;
	int gavi = 0;

	gav = (char***) malloc(sizeof(char**) * argc);

	endofpath = path;

	/*
	 * walk through the operands, building separate arrays of LS
	 * structures for directory and non-directory files.
	 */
	dstats = rstats = NULL;
	dirmax = regmax = 0;

	for (dircnt = regcnt = 0; *argv; ++argv)
	{
	    if(!(gav[gavi] = av = glob(*argv)))
		{ av = av2; av2[0] = *argv; av2[1] = 0; }
	    else gavi++;

	    for( ; *av; av++)
	    {
		if (util_stat(*av, &sb)) {
			if (client_intr_state < 2)
			{
			    ls_retval = 1;
			    perror(*av);
			    if (errno == ENOENT)
				    continue;
			}
			tidyup_mem(gavi, gav, dstats, rstats);
			ls_bad(1);
		}
		if ((S_IFDIR & sb.st_mode) && !f_listdir) {
			if(dirmax == dircnt)
			{
			    dirmax += 10;
			    if (!dstats)
			    {
				dstatp = dstats = (LS *)emalloc(dirmax *
								(sizeof(LS)));
			    } else 
			    {
				dstats = (LS *)realloc((char*)dstats,
							dirmax * (sizeof(LS)));
				dstatp = dstats + dircnt;
			    }
			}
			dstatp->name = *av;
			dstatp->lstat = sb;
			++dstatp;
			++dircnt;
		}
		else {
			if (sb.st_mtime < sincetime) continue;
			if(regmax == regcnt)
			{
			    regmax += 10;

			    if (!rstats)
			    {
				blocks = 0;
				maxlen = -1;
				rstatp = rstats = (LS *)emalloc(regmax *
						    (sizeof(LS)));
			    } else
			    {
				rstats = (LS *)realloc(rstats,
							regmax * (sizeof(LS)));
				rstatp = rstats + regcnt;
			    }
			}
			rstatp->name = *av;
			rstatp->lstat = sb;

			/* save name length for -C format */
			rstatp->len = strlen(*av);

			if (f_nonprint)
				prcopy(*av, *av, rstatp->len);

			/* calculate number of blocks if -l/-s formats */
			if (f_longform || f_size)
				blocks += (sb.st_size + 1023)/1024;

			/* save max length if -C format */
			if (f_column && maxlen < rstatp->len)
				maxlen = rstatp->len;

			++rstatp;
			++regcnt;
		}
	    }
	}
	/* display regular files */
	if (regcnt) {
		rstats[0].lst_btotal = blocks;
		rstats[0].lst_maxlen = maxlen;
		displaydir(rstats, regcnt);
		f_newline = f_dirname = 1;
	}
	/* display directories */
	if (dircnt) {
		register char *p;

		f_total = 1;
		if (dircnt > 1) {
			(void)util_getwd(top);
			qsort((char *)dstats, dircnt, sizeof(LS), sortfcn);
			f_dirname = 1;
		}
		for (cnt = 0, dstatp = dstats; cnt < dircnt; ++dstatp) {
			for (endofpath = path, p = dstatp->name;
			    (*endofpath = *p++); ++endofpath);
			subdir(dstatp);
			f_newline = 1;
			if (++cnt < dircnt && util_cd(top)) {
				if (client_intr_state < 2) perror(top);
				tidyup_mem(gavi, gav, dstats, rstats);
				ls_bad(1);
			}
		}
	}
	tidyup_mem(gavi, gav, dstats, rstats);
}

static void
#ifndef ANSI_PROTOTYPES
displaydir(stats, num)
	LS *stats;
	int num;
#else /* ANSI_PROTOTYPES */
displaydir(LS *stats, int num)
#endif /* ANSI_PROTOTYPES */
{
	register char *p, *savedpath;
	LS *lp;

	if (num > 1 && !f_nosort) {
		unsigned long save1, save2;

		save1 = stats[0].lst_btotal;
		save2 = stats[0].lst_maxlen;
		qsort((char *)stats, num, sizeof(LS), sortfcn);
		stats[0].lst_btotal = save1;
		stats[0].lst_maxlen = save2;
	}

	printfcn(stats, num);

	if (f_recursive) {
		savedpath = endofpath;
		for (lp = stats; num--; ++lp) {
			if (!(S_IFDIR & lp->lstat.st_mode))
				continue;
			p = lp->name;
			if (p[0] == '.' && (!p[1] || (p[1] == '.' && !p[2])))
				continue;
			if (endofpath != path && endofpath[-1] != '/')
				*endofpath++ = '/';
			for (; (*endofpath = *p++); ++endofpath);
			f_newline = f_dirname = f_total = 1;
			subdir(lp);
			*(endofpath = savedpath) = '\0';
			if (client_intr_state > 1)
			    break;
		}
	}
}

static void
#ifndef ANSI_PROTOTYPES
subdir(lp)
	LS *lp;
#else /* ANSI_PROTOTYPES */
subdir(LS *lp)
#endif /* ANSI_PROTOTYPES */
{
	LS *stats;
	int num;

	if (f_newline)
		(void)putc('\n', STDOUT);
	if (f_dirname)
		ffprintf(STDOUT, "%s:\n", path);

	if (util_cd(lp->name)) {
		ls_retval = 1;
		if (client_intr_state < 2) perror(lp->name);
		return;
	}
	if ((num = tabdir(lp, &stats))) {
		displaydir(stats, num);
		(void)free((char *)stats);
	}
	if (!((lp->name)[0] == '.' && (lp->name)[1] == '\0'))
		if (util_cd("..")) {
			if (client_intr_state < 2) perror("..");
			ls_bad(1);
		}
}

static int
#ifndef ANSI_PROTOTYPES
tabdir(lp, s_stats)
	LS *lp, **s_stats;
#else /* ANSI_PROTOTYPES */
tabdir(LS *lp, LS **s_stats)
#endif /* ANSI_PROTOTYPES */
{
	register RDIR *dirp;
	register int cnt, maxentry, maxlen;
	register char *p;
	struct rdirent *dp;
	unsigned long blocks;
	LS *stats;

	if (!(dirp = util_opendir("."))) {
		ls_retval = 1;
		if (client_intr_state < 2) perror(lp->name);
		return(0);
	}
	blocks = maxentry = maxlen = 0;
	stats = NULL;
	for (cnt = 0; (dp = util_readdir(dirp));) {
		/* this does -A and -a */
		p = dp->rd_name;
		if (p[0] == '.') {
			if (!f_listdot)
				continue;
			if (!f_listalldot && (!p[1] || (p[1] == '.' && !p[2])))
				continue;
		}
		if (cnt == maxentry) {
#define	DEFNUM	256
			maxentry += DEFNUM;
			if(stats)
			{
			  if (!(*s_stats = stats = (LS *)realloc((char *)stats,
			    (unsigned int)maxentry * sizeof(LS))))
				nomem();
			} else
			{
			  if (!(*s_stats = stats = (LS *)malloc(
			    (unsigned int)maxentry * sizeof(LS))))
				nomem();
			}
		}
		if (f_needstat && util_stat(dp->rd_name, &stats[cnt].lstat)) {
			/*
			 * don't exit -- this could be an NFS mount that has
			 * gone away.  Flush STDOUT so the messages line up.
			 */
			(void)fflush(STDOUT);
			if (client_intr_state < 2)
			{
			    ls_retval = 1;
			    perror(dp->rd_name);
			    continue;
			}
			else
			    break;
		}
		if (f_needstat &&
		    (S_IFDIR & stats[cnt].lstat.st_mode) == 0 &&
		    stats[cnt].lstat.st_mtime < sincetime)
		    continue;
		stats[cnt].name = dp->rd_name;

		/*
		 * get the inode from the directory, so the -f flag
		 * works right.
		 */
		stats[cnt].lstat.st_ino = dp->rd_fileno;

		/* save name length for -C format */
		stats[cnt].len = dp->rd_namlen;

		/* calculate number of blocks if -l/-s formats */
		if (f_longform || f_size)
			blocks += (stats[cnt].lstat.st_size + 1023)/1024;

		/* save max length if -C format */
		if (f_column && maxlen < (int)dp->rd_namlen)
			maxlen = dp->rd_namlen;
		++cnt;
	}
	(void)util_closedir(dirp);

	if (cnt) {
		stats[0].lst_btotal = blocks;
		stats[0].lst_maxlen = maxlen;
	} else if (stats) {
		(void)free((char *)stats);
	}
	return(cnt);
}
