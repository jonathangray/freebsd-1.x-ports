/*
 * The routines in this file read and write ASCII files from the disk. All of
 * the knowledge about files are here.
 *
 * $Log: fileio.c,v $
 * Revision 1.1  1994/02/01 03:29:23  jkh
 * Initial revision
 *
 * Revision 1.57  1993/09/10  16:06:49  pgf
 * tom's 3.61 changes
 *
 * Revision 1.56  1993/09/03  09:11:54  pgf
 * tom's 3.60 changes
 *
 * Revision 1.55  1993/08/05  14:29:12  pgf
 * tom's 3.57 changes
 *
 * Revision 1.54  1993/07/27  18:06:20  pgf
 * see tom's 3.56 CHANGES entry
 *
 * Revision 1.53  1993/07/06  16:39:04  pgf
 * integrated Tuan DANG's changes for the djgpp compiler under DOS
 *
 * Revision 1.52  1993/07/06  12:32:00  pgf
 * only check visible buffers' modtimes after shell escape or pipe
 *
 * Revision 1.51  1993/07/01  16:15:54  pgf
 * tom's 3.51 changes
 *
 * Revision 1.50  1993/06/25  11:25:55  pgf
 * patches for Watcom C/386, from Tuan DANG
 *
 * Revision 1.49  1993/06/18  15:57:06  pgf
 * tom's 3.49 changes
 *
 * Revision 1.48  1993/06/02  14:28:47  pgf
 * see tom's 3.48 CHANGES
 *
 * Revision 1.47  1993/05/24  15:21:37  pgf
 * tom's 3.47 changes, part a
 *
 * Revision 1.46  1993/05/11  16:22:22  pgf
 * see tom's CHANGES, 3.46
 *
 * Revision 1.45  1993/05/06  16:34:27  pgf
 * oops -- seek to current plus len, not to eof, in ffread
 *
 * Revision 1.44  1993/05/06  11:02:47  pgf
 * keep stdio in sync, by seeking to end after reading whole file.  this
 * keeps things working if we have to fseek back to the beginning later
 *
 * Revision 1.43  1993/05/04  17:05:14  pgf
 * see tom's CHANGES, 3.45
 *
 * Revision 1.42  1993/04/28  14:34:11  pgf
 * see CHANGES, 3.44 (tom)
 *
 * Revision 1.41  1993/04/20  12:18:32  pgf
 * see tom's 3.43 CHANGES
 *
 * Revision 1.40  1993/04/02  11:01:07  pgf
 * use < > instead of " " on system includes.
 *
 * Revision 1.39  1993/04/01  13:07:50  pgf
 * see tom's 3.40 CHANGES
 *
 * Revision 1.38  1993/03/25  19:50:58  pgf
 * see 3.39 section of CHANGES
 *
 * Revision 1.37  1993/03/18  17:42:20  pgf
 * see 3.38 section of CHANGES
 *
 * Revision 1.36  1993/03/17  10:00:29  pgf
 * initial changes to make VMS work again
 *
 * Revision 1.35  1993/03/16  10:53:21  pgf
 * see 3.36 section of CHANGES file
 *
 * Revision 1.34  1993/03/05  17:50:54  pgf
 * see CHANGES, 3.35 section
 *
 * Revision 1.33  1993/02/23  12:04:46  pgf
 * added support for appending to file (from alistair crooks)
 *
 * Revision 1.32  1993/01/16  10:33:59  foxharp
 * macro-ization of checks for shell-buffers
 *
 * Revision 1.31  1992/12/14  09:03:25  foxharp
 * lint cleanup, mostly malloc
 *
 * Revision 1.30  1992/08/20  23:40:48  foxharp
 * typo fixes -- thanks, eric
 *
 * Revision 1.29  1992/07/28  21:42:36  foxharp
 * linux switched to GNU stdio... okay, so maybe isready_c() _wasn't_
 * such a good idea...
 *
 * Revision 1.28  1992/07/24  18:21:17  foxharp
 * cleanup and better comments re: isready_c, and linux support
 *
 * Revision 1.27  1992/06/25  23:00:50  foxharp
 * changes for dos/ibmpc
 *
 * Revision 1.26  1992/05/19  08:55:44  foxharp
 * more prototype and shadowed decl fixups
 *
 * Revision 1.25  1992/05/16  14:02:55  pgf
 * header/typedef fixups
 *
 * Revision 1.24  1992/05/16  12:00:31  pgf
 * prototypes/ansi/void-int stuff/microsoftC
 *
 * Revision 1.23  1992/04/14  08:51:44  pgf
 * ifdef fixups for pjr and DOS
 *
 * Revision 1.22  1992/03/25  19:13:17  pgf
 * BSD portability changes
 *
 * Revision 1.21  1992/03/19  23:21:12  pgf
 * moved includes to top
 *
 * Revision 1.20  1992/01/05  00:06:13  pgf
 * split mlwrite into mlwrite/mlprompt/mlforce to make errors visible more
 * often.  also normalized message appearance somewhat.
 *
 * Revision 1.19  1992/01/01  16:17:46  pgf
 * improve behavior of long line reads (from Eric Krohn)
 *
 * Revision 1.18  1991/11/27  10:09:09  pgf
 * move some ifdef, so as not to leave empty if body
 *
 * Revision 1.17  1991/11/16  18:31:39  pgf
 * ifdef with FIONREAD instead of BSD in typahead()
 *
 * Revision 1.16  1991/11/08  13:22:56  pgf
 * moved dosfiles processing out to file.c, and
 * made aborts of file reads report FIOABRT, and
 * hopefully, editing binary files should work better (don't
 * use strncpy on lines)
 *
 * Revision 1.15  1991/11/07  02:00:32  pgf
 * lint cleanup
 *
 * Revision 1.14  1991/11/03  17:38:38  pgf
 * fixed some slop in the cr/nl checking
 *
 * Revision 1.13  1991/11/01  14:38:00  pgf
 * saber cleanup
 *
 * Revision 1.12  1991/10/23  12:05:37  pgf
 * changed filio.h to ioctl.h as the source of FIONREAD
 *
 * Revision 1.11  1991/10/22  14:08:23  pgf
 * took out old ifdef BEFORE code
 *
 * Revision 1.10  1991/10/10  12:31:53  pgf
 * added more "ff" utilities:  ffread, ffseek, ffrewind, ffsize
 *
 * Revision 1.9  1991/09/25  00:24:27  pgf
 * ffhasdata now works most of the time for system V, by simply checking
 * the stdio buffer -- no system call necessary
 *
 * Revision 1.8  1991/08/08  13:20:23  pgf
 * set "dosfile" global after reading each line
 *
 * Revision 1.7  1991/08/07  12:35:07  pgf
 * added RCS log messages
 *
 * revision 1.6
 * date: 1991/06/18 20:08:09;
 * added decl for FILE *npopen()
 * 
 * revision 1.5
 * date: 1991/04/22 09:02:03;
 * portability
 * 
 * revision 1.4
 * date: 1991/04/08 15:49:44;
 * added ffhasdata routine
 * 
 * revision 1.3
 * date: 1991/04/04 09:37:25;
 * minor fixes
 * 
 * revision 1.2
 * date: 1990/10/12 19:30:46;
 * added beeps on non-writeable
 * 
 * revision 1.1
 * date: 1990/09/21 10:25:18;
 * initial vile RCS revision
 */

#include	"estruct.h"
#include        "edef.h"
#if UNIX || VMS || MSDOS
#include	<sys/stat.h>
#endif

#if VMS
#include	<file.h>
#else
#include        <fcntl.h>
#endif

#if	BERK
#include	<sys/ioctl.h>
#endif

#if MSDOS && NEWDOSCC
#include	<io.h>
#endif

/*--------------------------------------------------------------------------*/

static	void	free_fline P(( void ));
#if MSDOS
static	int	make_backup P(( char * ));
#endif
static	int	count_fline;	/* # of lines read with 'ffgetline()' */
  

/*--------------------------------------------------------------------------*/
static void
free_fline()
{
	FreeAndNull(fline);
	flen = 0;
}

#if MSDOS
/*
 * Before overwriting a file, rename any existing version as a backup
 */
static int
make_backup (fname)
char	*fname;
{
	struct	stat	sb;
	if (stat(fname, &sb) >= 0) {
		char	tname[NFILEN];
		char	*s = pathleaf(strcpy(tname, fname)),
			*t = strrchr(s, '.');
		if (t == 0)
			t = s + strlen(s);
		(void)strcpy(t, ".bak");
		(void)unlink(tname);
		return (rename(fname, tname) >= 0);
	}
	return TRUE;
}
#endif

/*
 * Open a file for reading.
 */
int
ffropen(fn)
char    *fn;
{
	fileispipe = FALSE;
	eofflag = FALSE;

	if (isShellOrPipe(fn)) {
		ffp = 0;
#if UNIX || MSDOS
	        ffp = npopen(fn+1, FOPEN_READ);
#endif
#if VMS
	        ffp = vms_rpipe(fn+1, 0, (char *)0);
		/* really a temp-file, but we cannot fstat it to get size */
#endif
		if (ffp == 0)
			return (FIOERR);

		fileispipe = TRUE;
		count_fline = 0;

	} else if (is_directory(fn)) {
#if WATCOM
		set_errno(S_IFDIR);  /* not the same, but... */
#endif
#if TURBO
		set_errno(EISDIR);
#endif
#if GO32
 		set_errno(ENOENT);
#endif
		return (FIOERR);

	} else if ((ffp=fopen(fn, FOPEN_READ)) == NULL) {
		if (errno != ENOENT)
			return (FIOERR);
		return (FIOFNF);
	}

        return (FIOSUC);
}

/*
 * Open a file for writing. Return TRUE if all is well, and FALSE on error
 * (cannot create).
 */
int
ffwopen(fn)
char    *fn;
{
#if UNIX || MSDOS
	char	*name;
	char	*what = "file";
	char	*mode = FOPEN_WRITE;
	char	*action = "writ";

	if (isShellOrPipe(fn)) {
		if ((ffp=npopen(fn+1, mode)) == NULL) {
	                mlforce("[Cannot open pipe for writing]");
			TTbeep();
	                return (FIOERR);
		}
		fileispipe = TRUE;
	} else {
		if ((name = is_appendname(fn)) != NULL) {
			fn = name;
			mode = FOPEN_APPEND;
			action = "append";
		}
		if (is_directory(fn)) {
#if WATCOM
		        set_errno(S_IFDIR);
#endif
#if TURBO
		        set_errno(EISDIR);
#endif
#if GO32
			set_errno(ENOENT);
#endif
			what = "directory";
		}
#if MSDOS	/* patch: should make this a mode */
		if (!make_backup(fn))
			return (FIOERR);
#endif
		if (*what != 'f'
		 || (ffp = fopen(fn, mode)) == NULL) {
			mlforce("[Cannot open %s for %sing]", what, action);
			TTbeep();
			return (FIOERR);
		}
		fileispipe = FALSE;
	}
#else
#if     VMS
	char	temp[NFILEN];
        register int    fd;
	register char	*s;

	if ((s = strchr(fn = strcpy(temp, fn), ';')))	/* strip version */
		*s = EOS;

	if (is_appendname(fn)
	||  is_directory(fn)
	|| (fd=creat(fn, 0666, "rfm=var", "rat=cr")) < 0
        || (ffp=fdopen(fd, FOPEN_WRITE)) == NULL) {
                mlforce("[Cannot open file for writing]");
                return (FIOERR);
        }
#else
        if ((ffp=fopen(fn, FOPEN_WRITE)) == NULL) {
                mlforce("[Cannot open file for writing]");
                return (FIOERR);
        }
#endif
#endif
        return (FIOSUC);
}

/* is the file read-only?  true or false */
int
ffronly(fn)
char    *fn;
{
	if (isShellOrPipe(fn)) {
		return TRUE;
	} else {
#if UNIX || VMS
		return (access(fn, 2) != 0);	/* W_OK==2 */
#else
		int fd;
	        if ((fd=open(fn, O_WRONLY)) < 0) {
	                return TRUE;
		}
		(void)close(fd);
		return FALSE;
#endif
	}
}

#if !OPT_MAP_MEMORY
#if UNIX || VMS
long
ffsize()
{
	struct stat statbuf;
	if (fstat(fileno(ffp), &statbuf) == 0) {
		return (long)statbuf.st_size;
	}
        mlforce("[File sizing error]");
        return -1;
}
#endif

#if MSDOS

#if GO32

long
ffsize(void)
{
	int flen, prev;
	prev = ftell(ffp); fseek(ffp, 0, 2);
	flen = ftell(ffp); fseek(ffp, prev, 0);
	return flen;
}

#else

long
ffsize(void)
{
	int fd = fileno(ffp);
	return  filelength(fd);
}

#endif
#endif
#endif	/* !OPT_MAP_MEMORY */

#if !MSDOS && !OPT_MAP_MEMORY
int
ffread(buf,len)
char *buf;
long len;
{
#if VMS
	/*
	 * If the input file is record-formatted (as opposed to stream-lf, a
	 * single read won't get the whole file.
	 */
	int	total = 0;

	while (len > 0) {
		int	this = read(fileno(ffp), buf+total, len-total);
		if (this <= 0)
			break;
		total += this;
	}
	fseek (ffp, len, 1);	/* resynchronize stdio */
	return total;
#else
	int got = read(fileno(ffp), buf, (int)len);
	fseek (ffp, len, 1);	/* resynchronize stdio */
	return got;
#endif
}

void
ffseek(n)
long n;
{
#if VMS
	ffrewind();	/* see below */
#endif
	fseek (ffp,n,0);
}

void
ffrewind()
{
#if VMS
	/* VAX/VMS V5.4-2, VAX-C 3.2 'rewind()' does not work properly, because
	 * no end-of-file condition is returned after rewinding.  Reopening the
	 * file seems to work.  We can get away with this because we only
	 * reposition in "permanent" files that we are reading.
	 */
	char	temp[NFILEN];
	fgetname(ffp, temp);
	fclose(ffp);
	ffp = fopen(temp, FOPEN_READ);
#else
	fseek (ffp,0L,0);
#endif
}
#endif

/*
 * Close a file. Should look at the status in all systems.
 */
int
ffclose()
{
	int s = 0;

	free_fline();	/* free this since we do not need it anymore */

#if UNIX || MSDOS
	if (fileispipe) {
		npclose(ffp);
		mlforce("[Read %d lines%s]",
			count_fline,
			interrupted ? "- Interrupted" : "");
#ifdef	MDCHK_MODTIME
		(void)check_visible_modtimes();
#endif
	} else
		s = fclose(ffp);
        if (s != 0) {
                mlforce("[Error on close]");
                return(FIOERR);
        }
#else
        fclose(ffp);
#endif
        return (FIOSUC);
}

/*
 * Write a line to the already opened file. The "buf" points to the buffer,
 * and the "nbuf" is its length, less the free newline. Return the status.
 */
int
ffputline(buf, nbuf, ending)
char    buf[];
int	nbuf;
char *	ending;
{
        register int    i;
	for (i = 0; i < nbuf; ++i)
		if (ffputc(char2int(buf[i])) != FIOSUC)
			return FIOERR;

	while (*ending != EOS) {
		if (*ending != '\r' || i == 0 || buf[i-1] != '\r')
			fputc(*ending, ffp);
		ending++;
	}

        if (ferror(ffp)) {
                mlforce("[Write I/O error]");
                return (FIOERR);
        }

        return (FIOSUC);
}

/*
 * Write a char to the already opened file.
 * Return the status.
 */
int
ffputc(c)
int c;
{
	char	d = c;

#if	CRYPT
	if (cryptflag)
		ue_crypt(&d, 1);
#endif
	fputc(d, ffp);

        if (ferror(ffp)) {
                mlforce("[Write I/O error]");
                return (FIOERR);
        }

        return (FIOSUC);
}

/*
 * Read a line from a file, and store the bytes in an allocated buffer.
 * "flen" is the length of the buffer. Reallocate and copy as necessary.
 * Check for I/O errors. Return status.
 */
int
ffgetline(lenp)
int *lenp;	/* to return the final length */
{
        register int c;		/* current character read */
        register int i;		/* current index into fline */
	register char *tmpline;	/* temp storage for expanding line */

	/* if we are at the end...return it */
	if (eofflag)
		return(FIOEOF);

	/* if we don't have an fline, allocate one */
	if (fline == NULL)
		if ((fline = castalloc(char,flen = NSTRING)) == NULL)
			return(FIOMEM);

	/* read the line in */
	i = 0;
	for (;;) {
		c = fgetc(ffp);
		if ((c == '\n') || feof(ffp) || ferror(ffp))
			break;
		if (interrupted) {
			free_fline();
			*lenp = 0;
			return FIOABRT;
		}
                fline[i++] = c;
		/* if it's longer, get more room */
                if (i >= flen) {
			/* "Small" exponential growth - EJK */
			ALLOC_T growth = (flen >> 3) + NSTRING;
			if ((tmpline = castalloc(char,flen+growth)) == NULL)
                		return(FIOMEM);
                	(void)memcpy(tmpline, fline, (SIZE_T)flen);
                	flen += growth;
			free(fline);
                	fline = tmpline;
                }
		if_OPT_WORKING(cur_working++)
        }

#if !DOSFILES
# if	ST520
	if(c == '\n') {
		if(i > 0 && fline[i-1] == '\r') {
			i--;
		}
	}
# endif
#endif

	*lenp = i;	/* return the length, not including final null */
        fline[i] = EOS;

	/* test for any errors that may have occurred */
        if (c == EOF) {
		if (!feof(ffp) && ferror(ffp)) {
			mlforce("[File read error]");
			return(FIOERR);
                }

                if (i != 0)
			eofflag = TRUE;
		else
			return(FIOEOF);
        }

#if	CRYPT
	/* decrypt the line */
	if (cryptflag)
		ue_crypt(fline, i);
#endif
	count_fline++;
        return (eofflag ? FIOFUN : FIOSUC);
}

/*
 * isready_c()
 *
 * This fairly non-portable addition to the stdio set of macros is used to
 * see if stdio has data for us, without actually reading it and possibly
 * blocking.  If you have trouble building this, just define no_isready_c
 * below, so that ffhasdata() always returns FALSE.  If you want to make it
 * work, figure out how your getc in stdio.h knows whether or not to call
 * _filbuf() (or the equivalent), and write isready_c so that it returns
 * true if the buffer has chars available now.  The big win in getting it
 * to work is that reading the output of a pipe (e.g.  ":e !co -p file.c")
 * is _much_much_ faster, and I don't have to futz with non-blocking
 * reads...
 */
#if WATCOM || GO32
#define no_isready_c 1 
#endif

#ifndef no_isready_c
# ifdef __sgetc
   /* 386bsd */
#  define	isready_c(p)	( (p)->_r > 0)
# else
#  ifdef _STDIO_UCHAR_
	/* C E Chew's package */
#   define 	isready_c(p)	( (p)->__rptr < (p)->__rend)
#  else
#   ifdef _G_FOPEN_MAX
	/* GNU iostream/stdio library */
#    define 	isready_c(p)	( (p)->_gptr < (p)->_egptr)
#   else
#    if VMS
#     define	isready_c(p)	( (*p)->_cnt > 0)
#    endif
#    if TURBO
#     define    isready_c(p)	( (p)->bsize > ((p)->curp - (p)->buffer) )
#    endif
#    ifndef isready_c	/* most other stdio's (?) */
#     define	isready_c(p)	( (p)->_cnt > 0)
#    endif
#   endif
#  endif
# endif
#endif


int
ffhasdata()
{
#ifdef isready_c
	if (isready_c(ffp))
		return TRUE;
#endif
#ifdef	FIONREAD
	{
	long x;
	return(((ioctl(fileno(ffp),FIONREAD,(caddr_t)&x) < 0) || x == 0) ? FALSE : TRUE);
	}
#else
	return FALSE;
#endif
}

#if	AZTEC & MSDOS
#undef	fgetc
/*	a1getc:		Get an ascii char from the file input stream
			but DO NOT strip the high bit
*/

int a1getc(fp)

FILE *fp;

{
	int c;		/* translated character */

	c = getc(fp);	/* get the character */

	/* if its a <LF> char, throw it out  */
	while (c == '\n')
		c = getc(fp);

	/* if its a <RETURN> char, change it to a LF */
	if (c == '\r')
		c = '\n';

	/* if its a ^Z, its an EOF */
	if (c == 26)
		c = EOF;

	return(c);
}
#endif
