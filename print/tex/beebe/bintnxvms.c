/***********************************************************************
Convert a TOPS-20 file transferred in FTP "binary" mode to "tenex" mode.
In "binary" mode, we have 2 36-bit words in 9 8-bit bytes.  In "tenex"
mode, we want the top 32 bits of each 36-bit group, giving 8 8-bit
bytes.

Who knows what FTP did if the file had an odd number of 36-bit words.

[08-Oct-87]

This version is intended for VAX VMS only.  Modified by Jerry Leichter
(LEICHTER@VENUS.YCC.YALE.ARPA).

Usage:
	bintnxvms [-f[xx]] [filespec]

Filespec may be wild-carded; if it is missing, *.* is assumed.  Each
matching file is read, and a new version the same specification is
created.  (Note that you can only specify one filespec, however.)  If
-fxx is specified, the last block of the file is filled with bytes of
the given value (xx), which must be a hex number.  If -f is given with
no value, 0 is assumed.  Appropriate values seem to be:

    * DVI	DF
    * GF	DF
      PK	F6 (File spec defines no filler, F6 may be an artifact)
      PXL	?? (Unknown, probably not significant)
      TFM	00 (File spec defines no filler)

"Fill" is definitely significant, at least in some applications, for the
file types marked, with a "*"; it probably doesn't matter for others.
Note, how- ever, that there is no certainty that files filled this way
will actually be readable to all programs, since the transfer process
may insert other random junk at the end of the file.

The filler is used by programs that wish to read the files from end to
beginning - for example, by DVI file translators that need to reverse
page order.  The author has observed that "fixed up" files often follow
the pattern:

	<data> <fill-bytes> <FFFFFFFF> <fill-bytes> end-of-block

The first set of fill bytes were probably inserted by the program that
wrote the file to being with.  (There should be from 1 to 4 of them,
bringing the file to an even number of 4-byte longwords.) The
<FFFFFFFF> filler is four bytes of hex FF characters, and probably
were inserted by VMS FTP to mark the end of the file on the TOPS-20
system.  The rest of the <fill-bytes> were inserted by fixfile.  If
you run into a program that is unable to read a file produced by
fixfile, DUMP the last block of the file and see if this pattern
occurs.  If so, it should be straightforward to over-write the FF's
with more <fill-byte>'s.
***********************************************************************/

#include	<ctype.h>
#include	<stdio.h>
#include	<rms.h>

FILE *fwild();
FILE *fnext();

main(argc, argv)
int argc;
char *argv[];
{
    FILE *infile;
    FILE *outfile;
    int fill = FALSE;
    int fillchar = 0;
    char *fspec;
    char thefile[NAM$C_MAXRSS + 1];
    char *c;
    int d;

    if (argc > 1 && argv[1][0] == '-' && argv[1][1] == 'f')
    {
	fill = TRUE;
	c = &argv[1][2];
	while (isxdigit(*c))
	{
	    if (isdigit(*c))
		d = *c - '0';
	    else
		d = 10 + toupper(*c) - 'A';
	    fillchar = (fillchar << 4) | d;
	    c++;
	}
	argc--;
	argv++;
    }
    if (argc > 1)
	fspec = argv[1];
    else
	fspec = "*.*";

    if ((infile = fwild(fspec, "r")) == NULL)
    {
	perror("Bad file spec");
	exit(0);
    }

    while (fnext(infile) != NULL)
    {
	fgetname(infile, thefile);
	/* Remove version number */
	d = strlen(thefile) - 1;
	while (thefile[d] != ';')
	    d--;
	thefile[d] = '\0';
	(void)printf("[%s", thefile);
	if ((outfile = fopen(thefile, "wb", "rfm=fix", "mrs=512"))
	    == NULL)
	{
	    perror("] Can't open output file");
	    continue;
	}
	dofile(infile, outfile, fill, fillchar);
	fclose(outfile);
	(void)printf("]\n");
    }

}

dofile(infile, outfile, fill, fillchar)
FILE *infile;
FILE *outfile;
int fill;
int fillchar;
{
    int c;
    int d;
    unsigned int bytecount = 0;

#define GET()	fgetc(infile)
#define PUT(c)	(fputc((c),outfile),bytecount++)
#define ECHO()	PUT(GET())

    for (;;)
    {
	c = GET();
	if (c == EOF)
	    break;

	PUT(c);			/* 0..7 */
	ECHO();			/* 8..15 */
	ECHO();			/* 16..23 */
	ECHO();			/* 24..31 */

	d = GET();
	c = (d << 4);
	d = GET();
	c |= 0xFF & (d >> 4);
	PUT(c);			/* 4..11 */

	c = (d << 4);
	d = GET();
	c |= 0xFF & (d >> 4);
	PUT(c);			/* 12..19 */

	c = (d << 4);
	d = GET();
	c |= 0xFF & (d >> 4);
	PUT(c);			/* 20..27 */

	c = (d << 4);
	d = GET();
	c |= 0xFF & (d >> 4);
	PUT(c);			/* 28..36 */
    }

    if (fill)
    {
	while (bytecount & 511)
	    PUT(fillchar);
    }
}

/*
 * Fwild and fnext for vms.  Written by Martin Minow; taken from the
 * DECUS C VMS compatibility collection.
 */

/* #include	<stdio.h> */
/* #include	<rms.h> */
#include	<ssdef.h>
#include	<descrip.h>
#include	<ctype.h>

#define	TRUE		1
#define	FALSE		0
#define	EOS		0

typedef struct rmsstuff
{
    int flag;			/* Flag for nonwildcard calls	 */
    char *wildmode;
    struct FAB fab;
    struct NAM nam;
    char starname[NAM$C_MAXRSS + 1];
    char filename[NAM$C_MAXRSS + 1];
}   RMSSTUFF;

static RMSSTUFF *wilddata[_NFILE];	/* RMS struct for each file */

/*
 * rms->flag can take on the following values:
 *	ISWILD		A file with wild-card bytes.
 *	UNWILD		A file without wild-cards, unopened.
 *	UNWILD_OPENED	A file without wild-cards, opened.
 */
#define	ISWILD		0
#define	UNWILD		1
#define	UNWILD_OPENED	2

extern FILE *cleanup();
FILE *
fwild(filename, mode)
char *filename;
char *mode;
/*
 * Do wildcard setup
 */
{
    register FILE *fd;
    register RMSSTUFF *r;
    register int index;

    /* First get a file to work with.  In the process, make sure the
       mode we got passed is valid.  Note that every file we will open
       from here on already exists, so an open for write will fail
       (since the open will include the fully-expanded file name,
       including the version number).  Unfortunately, opening a file on
       the null device always succeeds.  So, to simplify matters, we
       just reject a "w" mode request right here.  (We could as well let
       it go and then find "no matching files" when all later open
       requests fail.  But this makes more sense.) */
    if (*mode == 'w' || (fd = fopen("_nl:", mode)) == NULL)
    {
	return (NULL);
    }
    /* Warning: the package depends on fileno(fd) remaining
       unchanged after calls to freopen(). */
    index = fileno(fd);
    /* If we've been here before, make sure buffers are released. */
    cleanup(index);
    if ((r = malloc(sizeof(RMSSTUFF))) == NULL
	|| (r->wildmode = malloc(strlen(mode) + 1)) == NULL)
	return (cleanup(index));
    strcpy(r->wildmode, mode);
    wilddata[index] = r;
    /* Setup the fab and nam blocks. */
    r->fab = cc$rms_fab;	/* Initialize fab    */
    r->nam = cc$rms_nam;	/* and nam blocks  */
    r->fab.fab$l_nam = &r->nam;	/* fab -> nam	     */
    r->fab.fab$l_fna = filename;	/* Argument filename */
    r->fab.fab$b_fns = strlen(filename);	/* filename size     */
    r->nam.nam$l_esa = r->starname;	/* Expanded file name */
    r->nam.nam$b_ess = NAM$C_MAXRSS;	/* ... size	     */
    r->nam.nam$l_rsa = r->filename;	/* Result filename   */
    r->nam.nam$b_rss = NAM$C_MAXRSS;	/* ... size	     */
    /* Parse the file name */
    if (sys$parse(&r->fab) != RMS$_NORMAL)
	return (cleanup(index));
    /* Success.  Null-terminate expanded file name and set flag to
       distinguish between "wild" and "non-wild" filenames. */
    ((char *) r->nam.nam$l_esa)[r->nam.nam$b_esl] = EOS;
    r->flag = ((r->nam.nam$l_fnb & NAM$M_WILDCARD) == 0) ?
	UNWILD : ISWILD;
    return (fd);
}


FILE *
fnext(fd)
FILE *fd;
/*
 * Open the next valid file. return fd if successful, NULL if finished.
 */
{
    register int index;
    register RMSSTUFF *r;
    register int errorcode;
    FILE *test;

    index = fileno(fd);
    if ((r = wilddata[index]) == NULL || r->flag == UNWILD_OPENED)
    {
	/* It wasn't ours, or wasn't a wildcard and has already been
	   processed. */
	fclose(fd);
	fd = NULL;
    }
    else
    if (r->flag == UNWILD)
    {
	/* Not a wildcard file, first time through */
	fd = freopen(r->starname, r->wildmode, fd);
	r->flag = UNWILD_OPENED;
    }
    else
    {
	/* Look for the next match -- who says you can't write obscure
	   structured code? */
	for (;;)
	{
	    /* Look 'em up but skip any with protection violation
	    errors. */
	    r->fab.fab$w_ifi = 0;	/* Internal file index */
	    if ((errorcode = sys$search(&r->fab)) == RMS$_NORMAL)
	    {
		/* We have a file.  Open it if we have access rights.
		   Determining this isn't as simple as it might be
		   because the access() function checks the wrong thing:
		   It ignores ACL's and any special privileges.  So we
		   just open the file twice.  (We have to check first
		   since if we fail, freopen will free up our file
		   pointer and we can't be sure of getting it back.)
		   Opening the file twice has one unfortunate side
		   effect - it bumps the file revision by 2.  That's
		   life. */
		((char *) r->nam.nam$l_rsa)[r->nam.nam$b_rsl] = EOS;
		if ((test = fopen(r->filename, r->wildmode)) == NULL)
		{
		    /* Couldn't open it; try for another. */
		    continue;
		}
		else
		{
		    /* Close up the test channel first (to avoid
		       problems with sharing; then open the file.  We
		       should normally be able to open the file.  It
		       this fails, something is wrong inside the C
		       library.  (More likely, a timing problem occured
		       - between our test and our actual open, someone
		       deleted the file, or between our close and
		       re-open, someone else opened it in an
		       incompatible mode.)  In any case, we've lost our
		       file descriptor, and there is no good way to
		       recover; so we just return NULL and let the
		       caller think that's all there is. */
		    fclose(test);
		    fd = freopen(r->filename, r->wildmode, fd);
		    break;
		}
	    }
	    else
	    if (errorcode == RMS$_PRV)
	    {
		/* sys$search() found something, but we don't have
		   privileges to open it.  Look for another. */
		continue;
	    }
	    else
	    {
		/* Can't find a file.  This should be RMS$_NMF. */
		fclose(fd);
		fd = NULL;
		break;
	    }
	}
    }
    /* Cleanup if any errors */
    if (fd == NULL)
    {
	cleanup(index);
    }
    return (fd);
}

static FILE *
cleanup(index)
register int index;
/*
 * Empty out any stored information
 */
{
    register RMSSTUFF *r;

    r = wilddata[index];
    if (r != NULL)
    {
	if (r->wildmode != NULL)
	{
	    free(r->wildmode);
	}
	free(r);
	wilddata[index] = NULL;
    }
    return (NULL);
}
