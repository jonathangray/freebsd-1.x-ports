/*
 *	vmspipe.c
 *		adapted from elvis, in turn from
 *		Chris Janton's (chj) VMS Icon port.
 *
 * $Log: vmspipe.c,v $
 * Revision 1.1  1994/02/01 03:29:43  jkh
 * Initial revision
 *
 * Revision 1.3  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.2  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.1  1993/04/01  12:54:44  pgf
 * Initial revision
 *
 */

#include "estruct.h"

#include <iodef>
#include <ssdef>
#include <dvidef.h>
#include <file.h>

#define	OK(f)	((f) & STS$M_SUCCESS)
#define	MAXBFR	256

/*
 *	Here we fudge to help the "elvis" program implement rpipe.  The
 *	routine essentially does an popen using fd as stdin--except that
 *	few VMS utilities use the 'C' library.	So we pass in the standard
 *	input file name and use it if fd is non-zero.
 */

typedef struct _descr {
		int	length;
		char *	ptr;
	} descriptor;

typedef struct _pipe {
	long	pid;		/* process id of child */
	long	status;		/* exit status of child */
	long	flags;		/* LIB$SPAWN flags */
	int	ichan;		/* MBX channel number */
	int	ochan;
	int	efn;
	unsigned running : 1;	/* 1 if child is running */
} Pipe;

Pipe _pipes[_NFILE];		/* one for every open file */

#define NOWAIT		1
#define NOCLISYM	2
#define NOLOGNAM	4
#define NOKEYPAD	8
#define NOTIFY		16
#define NOCONTROL	32
#define SFLAGS	(NOWAIT|NOKEYPAD|NOCONTROL)


/*
 * Taken from pclose - close a pipe
 * Last modified 2-Apr-86/chj
 */
static int
vms_rpclose(int pfile)
{
	Pipe *pd = (pfile >= 0 && pfile < SIZEOF(_pipes)) ? &_pipes[pfile] : 0;
	int status;
	int fstatus;

	if (pd == NULL)
		return (-1);
	fstatus = close(pfile);
	SYS$DASSGN(pd->ichan);
	SYS$DASSGN(pd->ochan);
	LIB$FREE_EF(&pd->efn);
	pd->running = 0;
	return (fstatus);
}

static int
vms_pread (int pfile, char *buffer, int size)
/* Be compatible when we read data in (handle newlines). */
{
	Pipe *pd = (pfile >= 0 && pfile < SIZEOF(_pipes)) ? &_pipes[pfile] : 0;
	struct {
		short	status,
			count;
		int:16;
	} iosb;

	/*
	 * This is sort of nasty.  The default mailbox size is 256 maxmsg and
	 * 1056 bufquo if your sysgen parameters are standard.  Asking for more
	 * on the CREMBX command (in rpipe) might be a bad idea as that could
	 * cause an "exceeded quota" error.  Since we only return -1 on error,
	 * there's no hope that the poor user would ever know what went wrong.
	 */
	if (pd != 0) {
		int	request = size > MAXBFR ? MAXBFR : size - 1;
		register int	got;

		if (OK(sys$qiow(0, pd->ochan, IO$_READVBLK, &iosb, 0, 0,
				buffer, request, 0, 0, 0, 0))) {

			if (iosb.status == SS$_ENDOFFILE)
				return 0;
			got = iosb.count;
			if (got == 0 || buffer[got-1] != '\n')
				buffer[got++] = '\n';
			buffer[got] = EOS;
			return got;
		}
	}
	return -1;
}

int
vms_rpipe (char *cmd, int fd, char *input_file)
{
	int	pfile;		/* the Pfile */
	Pipe	*pd;		/* _pipe database */
	descriptor inmbxname;	/* name of input mailbox */
	descriptor outmbxname;	/* name of mailbox */
	char	inmname[65];
	char	outmname[65];	/* mailbox name string */
	int	ochan;		/* mailbox channel number */
	int	ichan;		/* Input mailbox channel number */
	int	efn;

	struct {
		short	len;
		short	code;
		char *	address;
		char *	retlen;
		int	last;
	} itmlst;

	if (!cmd || cmd[0] == EOS)
		return (0);
	LIB$GET_EF(&efn);
	if (efn == -1)
		return (0);

	/* create and open the input mailbox */
	if (!OK(SYS$CREMBX(0, &ichan, 0, 0, 0, 0, 0))) {
		LIB$FREE_EF(&efn);
		return (0);
	}

	itmlst.last = inmbxname.length = 0;
	itmlst.address = inmbxname.ptr = inmname;
	itmlst.retlen = &inmbxname.length;
	itmlst.code = DVI$_DEVNAM;
	itmlst.len = 64;
	if (!OK(SYS$GETDVIW(0, ichan, 0, &itmlst, 0, 0, 0, 0))) {
		LIB$FREE_EF(&efn);
		return (0);
	}
	inmname[inmbxname.length] = EOS;

	/* create and open the output mailbox */
	if (!OK(SYS$CREMBX(0, &ochan, 0, 0, 0, 0, 0))) {
		LIB$FREE_EF(&efn);
		return (0);
	}

	itmlst.last = outmbxname.length = 0;
	itmlst.address = outmbxname.ptr = outmname;
	itmlst.retlen = &outmbxname.length;
	if (!OK(SYS$GETDVIW(0, ochan, 0, &itmlst, 0, 0, 0, 0))) {
		LIB$FREE_EF(&efn);
		return (0);
	}

	outmname[outmbxname.length] = EOS;
	pfile = open(outmname, O_RDONLY);
	if (pfile < 0) {
		LIB$FREE_EF(&efn);
		SYS$DASSGN(ichan);
		SYS$DASSGN(ochan);
		return (0);
	}

	/* Save file information now */
	pd = &_pipes[pfile]; /* get Pipe pointer */
	pd->pid   = pd->status = pd->running = 0;
	pd->flags = SFLAGS;
	pd->ichan = ichan;
	pd->ochan = ochan;
	pd->efn   = efn;

	/* Initiate the command by writing down the input mailbox (SYS$INPUT). */

	if (fd > 0) {
		char	pre_command[132+12];

		strcpy (pre_command, "DEFINE/USER SYS$INPUT ");
		strcat (pre_command, input_file);
		if (!OK(
			sys$qiow(0,
				ichan,
				IO$_WRITEVBLK | IO$M_NOW,
				0, 0, 0,
				pre_command, strlen(pre_command),
				0, 0, 0, 0))) {
			LIB$FREE_EF(&efn);
			SYS$DASSGN(ichan);
			SYS$DASSGN(ochan);
			return (0);
		}
	}

	if (!OK(
		sys$qiow(0,
		ichan,
		IO$_WRITEVBLK | IO$M_NOW,
		0, 0, 0, cmd, strlen(cmd), 0, 0, 0, 0))) {
		;
	} else if (!OK(
		sys$qiow(0,
			ichan,
			IO$_WRITEOF | IO$M_NOW,
			0, 0, 0, 0, 0, 0, 0, 0, 0))) {
		;
	} else if (!OK(
		LIB$SPAWN(0,
			&inmbxname,		/* input file */
			&outmbxname,		/* output file */
			&pd->flags,
			0,
			&pd->pid,
			&pd->status,
			&pd->efn,
			0, 0, 0, 0))) {
		;
	} else {
		FILE	*pp;
		int	len;
		int	count	= 0;
		char	buffer[MAXBFR];

		pd->running = 1;

		/*
		 * For our application, we need a file-pointer.  Empty the
		 * pipe into a temporary file.  Unfortunately, this means that
		 * the application will use 'slowreadf()' to write the data.
		 */
		if (pp = tmpfile()) {
			for (;;) {
				len = vms_pread(pfile, buffer, sizeof(buffer));
				if (len <= 0)
					break;
				fwrite(buffer, sizeof(*buffer), len, pp);
				count++;
			}
			mlforce("[Read %d lines from %s ]", count, cmd);
			vms_rpclose(pfile);
			(void)fflush(pp);
			rewind(pp);
			return pp;
		}
		return (0);
	}

	LIB$FREE_EF(&efn);
	SYS$DASSGN(ichan);
	SYS$DASSGN(ochan);
	return (0);
}
