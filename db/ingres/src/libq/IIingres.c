#include <stdio.h>
#include <stdarg.h>
#include <signal.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <ingres.h>
#include <symbol.h>
#include <aux.h>
#include "IIglobals.h"
#include <pipes.h>
#include "sccs.h"

#include "protos.h"

SCCSID(@(#)IIingres.c	8.1	12/31/84)

#ifdef CLOSED
#undef CLOSED
#endif

#define	CLOSED	'?'

#ifndef MAX_INGRES_ARGS
#define MAX_INGRES_ARGS	512
#endif

char	*IImainpr =	"ingres";

char	IIPathname[41];

/*
**	IIingres opens the needed pipes and
**	forks an ingres process.
**
**	ingres recognizes the EQUEL flag followed by
**	three control characters as being an equel processes
**
**	parameters to ingres are passed directly. only
**	the first 9 are counted.
*/

void
IIingres(char *p1, ...)
{
	int		pipes[4];		/* pipe vector buffer */
	char		eoption[15];	/* dummy variable to hold -EQUEL option */
	register char	*cp;
	va_list		vp;
	char		*argv[MAX_INGRES_ARGS];
	pb_t		pb;
	int		i;

#ifdef xETR1
	if (IIdebug)
		printf("IIingres\n");
#endif
	/* test if ingres is already invoked */
	if (IIingpid)
		IIsyserr("Attempt to invoke INGRES twice");

	IIgetpath();
	/* open INGRES pipes */
	if (pipe(&pipes[0]) || pipe(&pipes[2]))
		IIsyserr("pipe error in IIingres");

	IIw_down = pipes[1];	/* file desc for equel->parser */
	IIr_down = pipes[2];	/* file desc for parser->equel */

	/* catch interupts if they are not being ignored */
	if (signal(SIGINT, SIG_IGN) != SIG_IGN) {
		signal(SIGINT, IIresync);
	}

	/* set up equel option flag */
	cp = eoption;
	*cp++ = '-';
	*cp++ = EQUEL;
	*cp++ = pipes[0] | 0100;
	*cp++ = pipes[1] | 0100;
	*cp++ = CLOSED;
	*cp++ = pipes[3] | 0100;
	*cp++ = CLOSED;	/* this will be the equel->ovqp pipe in the future */
	*cp++ = CLOSED;	/* old ovqp->equel pipe */
	/* put "6.3" at end of flag for OVQP to not do flush after
	 * every tuple
	 */
	*cp++ = '6';
	*cp++ = '.';
	*cp++ = '3';
	*cp = '\0';
	if (cp - eoption >= sizeof(eoption))
		IIsyserr("IIingres: too big an eoption");

	if ((IIingpid = fork()) < 0)
		IIsyserr("IIingres:cant fork %d", IIingpid);
	/* if parent, close the unneeded files and return */
	if (IIingpid) {
		if (close(pipes[0]) || close(pipes[3]))
			IIsyserr("close error 1 in IIingres");
		IIinput = IIr_down;
		IIpb_prime(&pb, PB_NOTYPE);
#ifdef xETR1
		if (IIdebug)
			printf("calling ingres with '%s' database %s\n", eoption, p1);
#endif
		return;
	}
	/* the child overlays /usr/bin/ingres */
	argv[0] = "ingres";
	argv[1] = eoption;
	va_start(vp, p1);
	for (i = 2 ; i < MAX_INGRES_ARGS ; i++) {
		if ((argv[i] = va_arg(vp, char *)) == (char *) NULL) {
			break;
		}
	}
	va_end(vp);
	execvp(IImainpr, argv);
	IIsyserr("cannot exec INGRES in IIingres");
}

/*
**  IIGETPATH -- initialize the IIPathname variable
**
**	Parameters:
**		none
**
**	Returns:
**		none
**
**	Side Effects:
**		Sets IIPathname to the home directory of the USERINGRES
**		[unix.h] user.
**
**	Called By:
**		IIingres.c
**
**	History:
**		3/26/79 -- (marc) written
*/
void
IIgetpath(void)
{
	char			line[MAX_LINE_SIZE + 1];
	static char		reenter;
	register int		i;
	register char		*lp;
	iob_t		iobuf;
	char			*field[UF_NFIELDS];

	if (reenter)
		return;
	else
		reenter++;

	if ((i = IIfopen("/etc/passwd", &iobuf)) < 0)
		IIsyserr("IIgetpath: no /etc/passwd");

	do {
		/* get a line from the passwd file */
		i = 0;
		for (lp = line; (*lp = IIgetc(&iobuf)) != '\n'; lp++) {
			if (*lp == -1)
				IIsyserr("IIgetpath: no user 'ingres' in /etc/passwd");
			if (++i > sizeof(line) - 1) {
				*lp = '\0';
				IIsyserr("IIgetpath: line overflow: \"%s\"",
					line);
			}
		}
		*lp = '\0';
		for (i = 0, lp = line; *lp != '\0'; lp++) {
			if (*lp == ':') {
				*lp = '\0';
				if (i > UF_NFIELDS)
					IIsyserr("IIgetpath: too many fields in passwd \"%s\"",
					line);
				field[i++] = lp + 1;
			}
		}
		/* check for enough fields for valid entry */
		if (i < 3)
			IIsyserr("IIgetpath: too few fields \"%s\"",
			line);
	}  while (strcmp(line, USERINGRES) != 0);
	IIclose(&iobuf);

	/* check that pathname won't overflow static buffer */
	if (field[i - 1] - field[i - 2] > sizeof(IIPathname))
		IIsyserr("IIgetpath: path too long \"%s\"", field[i - 2]);

	/* move pathname into buffer */
	IIbmove(field[i - 2], IIPathname, field[i - 1] - field[i - 2]);
}
