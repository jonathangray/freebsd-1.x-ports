/*	EPATH:	This file contains certain info needed to locate the
		MicroEMACS files on a system dependent basis.

									*/

/*	possible names and paths of help files under different OSs	*/

/*
 * $Log: epath.h,v $
 * Revision 1.1  1994/02/01 03:29:17  jkh
 * Initial revision
 *
 * Revision 1.10  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.9  1993/08/10  10:54:28  pgf
 * added "sys$login:" to VMS search path
 *
 * Revision 1.8  1993/05/11  16:22:22  pgf
 * see tom's CHANGES, 3.46
 *
 * Revision 1.7  1992/12/14  09:03:25  foxharp
 * lint cleanup, mostly malloc
 *
 * Revision 1.6  1992/11/19  08:58:06  foxharp
 * added HELP_LOC support -- set in makefile, as alternate place to put vile.hlp
 *
 * Revision 1.5  1992/08/20  23:40:48  foxharp
 * typo fixes -- thanks, eric
 *
 * Revision 1.4  1992/03/19  23:16:33  pgf
 * ifdef fix
 *
 * Revision 1.3  1991/08/07  11:51:32  pgf
 * added RCS log entries
 *
 * revision 1.2
 * date: 1991/06/26 09:43:03;
 * added trailing slash on some file search paths
 * ----------------------------
 * revision 1.1
 * date: 1990/09/21 10:25:08;
 * initial vile RCS revision
 */

/* first two entries are default startup and help files, the rest are
	possible places to look for them */

char *pathname[] =

#if	AMIGA
{
	".vilerc",
	"vile.hlp",
	"",
	"sys:c/",
	"sys:t/",
	"sys:s/",
	":c/",
	":t/",
	":s/"
};
#endif

#if	ST520
{
	"vile.rc",
	"vile.hlp",
	"\\",
	"\\bin\\",
	"\\util\\",
	""
};
#endif

#if	FINDER
{
	"vile.rc",
	"vile.hlp",
	"/bin/",
	"/sys/public/",
	""
};
#endif

#if	MSDOS
{
	"vile.rc",
	"vile.hlp",
	"\\sys\\public\\",
	"\\usr\\bin\\",
	"\\bin\\",
	"\\",
	""
};
#endif

#if	UNIX
{
	".vilerc",
	"vile.hlp",
	".",		/* replaced at runtime with path-head of argv[0] */
	"/usr/local/",
	"/usr/lib/",
	"/usr/local/bin/",
	"/usr/local/lib/",
#ifdef HELP_LOC
#ifndef lint	/* makefile gives inconsistent quoting for lint, compiler */
	HELP_LOC,
#endif	/* lint */
#endif
	""
};
#endif

#if	VMS
{
	"vile.rc",
	"vile.hlp",
	".",		/* replaced at runtime with path-head of argv[0] */
	"sys$login:",
	"",
	"sys$sysdevice:[vmstools]"
};
#endif

#define	NPNAMES	(sizeof(pathname)/sizeof(char *))
