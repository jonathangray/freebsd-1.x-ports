/*
 * version & usage-messages for vile
 *
 * $Log: version.c,v $
 * Revision 1.1  1994/02/01 03:29:41  jkh
 * Initial revision
 *
 * Revision 1.2  1993/09/06  16:35:36  pgf
 * clarified wording in version message (i.e. what the date means)
 *
 * Revision 1.1  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 */

#include	"estruct.h"	/* global structures and defines */
#include	"edef.h"	/* global definitions */

#if UNIX || VMS
#include <sys/stat.h>		/* ...for 'struct stat' */
#include <time.h>		/* ...for 'ctime()' */
#endif

extern char *pathname[];	/* startup file path/name array */

#if UNIX || VMS
static	char	built_at[40];
#endif

void
print_usage P((void))
{
	static	char	*options[] = {
	"-h             to get help on startup",
	"-gNNN          or simply +NNN to go to line NNN",
	"-sstring       or +/string to search for \"string\"",
#if TAGS
	"-ttagname      to look up a tag",
#endif
	"-v             to view files as read-only",
#if CRYPT
	"-kcryptkey     for encrypted files",
#endif
#if X11
	"-name name     to change program name for X resources",
	"-fg color      to change foreground color",
	"-bg color      to change background color",
	"-f fontname    to change font",
	"-d displayname to change the default display",
	"-r             for reverse video",
	"=geometry      to set window size (like '=80x50')",
#endif
#if IBMPC
	"-2             25-line mode",
	"-4             43-line mode",
	"-5             50-line mode",
#endif
	"-V             for version info",
	"use @filename to run filename as commands",
	" (this will suppress .vilerc)" };
	register int	j;

	(void)fprintf(stderr, "usage: %s [-flags] [@cmdfile] files...\n", prog_arg);
	for (j = 0; j < SIZEOF(options); j++)
		(void)fprintf(stderr, "\t%s\n", options[j]);
	ExitProgram(BAD(1));
}

#if UNIX || VMS
void
makeversion()
{
	/*
	 * Remember the directory from which we were run, to use in finding the
	 * help-file.
	 */
	char	temp[NFILEN];
	char	*s = strmalloc(lengthen_path(strcpy(temp, prog_arg))),
		*t = pathleaf(s);
	if (t != s) {
#if UNIX	/* 't' points past slash */
		t[-1] = EOS;
#else		/* 't' points to ']' */
		*t = EOS;
#endif
		pathname[2] = s;
	}

	/*
	 * We really would like to have the date at which this program was
	 * linked, but a.out doesn't have that in general.  COFF files do. 
	 * Getting the executable's modification-time is a reasonable
	 * compromise.
	 */
	*built_at = EOS;
	if ((s = flook(prog_arg, FL_ANYWHERE)) != NULL) {
		struct	stat	sb;
		if (stat(s, &sb) >= 0) {
			(void)lsprintf(built_at, ", installed %s",
				ctime(&sb.st_mtime));
			built_at[strlen(built_at)-1] = EOS;
		}
	}
}
#endif

/* ARGSUSED */
int
showversion(f,n)
int f,n;
{
#if UNIX || VMS
	mlforce("%s%s", version, built_at);
#else
# if defined(__DATE__) && !SMALLER
	mlforce("%s, built %s %s", version, __DATE__, __TIME__);
# else
	mlforce(version);
# endif
#endif
	return TRUE;
}
