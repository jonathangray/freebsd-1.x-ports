/******************************************************************************
* This file is Copyright 1992 by Philip G. Richards.  All Rights Reserved.
* See the file README that came with this distribution for permissions on
* code usage, copying, and distribution.  It comes with absolutely no warranty.
* email: <pgr@prg.ox.ac.uk>
******************************************************************************/

/* ---INFOBEGIN--- *  DO NOT DELETE THIS COMMENT BLOCK!!!
COMMAND tar remote "collect together remote files into a local tar file"
 *  ---INFOEND---  */

#include "client.h"

#ifndef ANSI_PROTOTYPES
extern char **glob();
#else /* ANSI_PROTOTYPES */
extern char **glob(char *);
#endif /* ANSI_PROTOTYPES */

#define TAR_BLOCKSIZE	512
#define TAR_NAMESIZE	100

typedef union {
    char data[TAR_BLOCKSIZE];
    struct header {
	char name[TAR_NAMESIZE];
	char mode[8];
	char uid[8];
	char gid[8];
	char size[12];
	char mtime[12];
	char chksum[8];
	char flag;
	char linkname[TAR_NAMESIZE];
    } h;
} TarHeader;

static int tarout;
static TarHeader tarhdr;
static int baselen, cwdlen;
static char *abscwd;
static int recursive;

static void
#ifndef ANSI_PROTOTYPES
zero_tarhdr()
#else /* ANSI_PROTOTYPES */
zero_tarhdr(void)
#endif /* ANSI_PROTOTYPES */
{
    int i;

    /*
    ** how embarassing... however, I probably can't assume the existence
    ** of bzero() or memset()... (I'm disheartened by this portability
    ** lark, you know...  I wonder if I can assume the portability of
    ** `for'...)  
    */
    for (i = 0; i < sizeof(tarhdr.data); i++)
	tarhdr.data[i] = 0;
}

#define sprintf_field(F,V) \
    (void)sprintf(&tarhdr.h.F[0], "%0*o ", sizeof(tarhdr.h.F) - 1, (V))
#define sprintf_fieldn(F,V) \
    (void)sprintf(&tarhdr.h.F[0], "%0*o ", sizeof(tarhdr.h.F) - 2, (V))

static void
#ifndef ANSI_PROTOTYPES
addtar_chksum()
#else /* ANSI_PROTOTYPES */
addtar_chksum(void)
#endif /* ANSI_PROTOTYPES */
{
    int i;
    u_long chksum;

    for (i = 0; i < sizeof(tarhdr.h.chksum); i++)
	tarhdr.h.chksum[i] = ' ';

    for (chksum = 0, i = 0; i < sizeof(tarhdr); i++)
	chksum += (u_char)tarhdr.data[i];

    sprintf_fieldn(chksum, chksum);	
}

static int
#ifndef ANSI_PROTOTYPES
tar_file(name, sbufp, depth)
    char *name;
    struct stat *sbufp;
    int depth;
#else /* ANSI_PROTOTYPES */
tar_file(char *name, struct stat *sbufp, int depth)
#endif /* ANSI_PROTOTYPES */
{
    char *path, *remname;
    int bytes, retval = 0;

    if (!validate_operation(name, UTIL_DOWNLOAD))
	return -1;

    zero_tarhdr();
    (void)sprintf(&tarhdr.h.name[0], "%s", name + baselen);

    sprintf_fieldn(mode, sbufp->st_mode);	
    sprintf_fieldn(uid, (u_short)(-2));		/* nobody */
    sprintf_fieldn(gid, (u_short)(-2));		/* nogroup */

    sprintf_field(size, sbufp->st_size);
    sprintf_field(mtime, sbufp->st_mtime);

    tarhdr.h.flag = '0'; /* regular file */
    addtar_chksum();

    bytes = write(tarout, (char*)&tarhdr, sizeof(tarhdr));
    if (bytes < 0)
    {
	ffprintf(STDERR, "?tar: can't write header for file `%s': %s\n",
		 name + baselen, sys_errlist[errno]);
	return -1;
    }
    else if (bytes < sizeof(tarhdr))
    {
	ffprintf(STDERR, "?tar: only wrote partial header for `%s': %s\n",
		 name + baselen, sys_errlist[errno]);
	ffprintf(STDERR, "??    tarfile is probably corrupt!\n");
	return -1;
    }

    /* check if the cwd is a prefix of the remote filename */
    if (baselen > cwdlen && strncmp(abscwd, name, cwdlen) == 0)
	remname = name + cwdlen + 1;
    else
	remname = name;

    if (recursive)
	path = name + baselen;
    else
	path = strrchr(name, '/') + 1;

    if (client_trace >= 2)
	ffprintf(STDINFO, "downloading `%s' (%d bytes)\n",
	         remname, sbufp->st_size);

    bytes = util_download(remname, tarout, 0, sbufp->st_size);

    if (bytes < 0)
	bytes = 0;
    else if (bytes < sbufp->st_size)
	ffprintf(STDERR, "?failed to download `%d' bytes -- adding padding\n",
		 sbufp->st_size - bytes);

    retval = (bytes < sbufp->st_size);

    bytes = (sbufp->st_size - bytes)
	   + (TAR_BLOCKSIZE - (sbufp->st_size % TAR_BLOCKSIZE))
	       % TAR_BLOCKSIZE;

    zero_tarhdr();
    for ( ; bytes > 0; bytes -= TAR_BLOCKSIZE)
    {
	int towrite, written;

	towrite = (bytes < TAR_BLOCKSIZE)? bytes: TAR_BLOCKSIZE;
	written = write(tarout, (char*)&tarhdr, towrite);
	retval |= (written < towrite);
	if (written < 0)
	{
	    ffprintf(STDERR, "?tar: failed to pad tarfile for file `%s': %s\n",
		     name, sys_errlist[errno]);
	    ffprintf(STDERR, "??    tarfile is probably corrupt!\n");
	    break;
	}
    }

    return -retval;
}

static int
#ifndef ANSI_PROTOTYPES
do_tar(name)
    char *name;
#else /* ANSI_PROTOTYPES */
do_tar(char *name)
#endif /* ANSI_PROTOTYPES */
{
    char *dirname;

    if (!validate_operation(name, UTIL_PROCESS_FILE))
	return -1;

    dirname = util_abs_path(name);
    /* util_abs_path() is guaranteed to return path with a '/' */
    baselen = strrchr(dirname, '/') - dirname + 1;
    (void)free(dirname);

    return util_process_file(name, recursive, 0, tar_file, 0, 0);
}

/* ARGSUSED */
int
#ifndef ANSI_PROTOTYPES
main(argc, argv, envp)
    int argc;
    char *argv[];
    char *envp[];
#else /* ANSI_PROTOTYPES */
main(int argc, char **argv, char **envp)
#endif /* ANSI_PROTOTYPES */
{
    int retval, errcnt, ch;
    extern int optind, opterr;

    recursive = 0;

    optind = 1;
    opterr = 1;
    errcnt = 0;

    while ((ch = getopt(argc, argv, "r")) != EOF)
        switch (ch)
        {
          case 'r':
            recursive = MAXRECURSION;
            break;
          default:
            errcnt++;
            break;
        }

    if (errcnt > 0 || argc - optind < 2)
    {
	ffprintf(STDERR, "?tar usage: tar [-r] [-|tarfile] file [file...]\n");
	return 1;
    }

    if (argv[optind][0] == '-' && argv[optind][1] == '\0')
	tarout = 1;	/* write to standard out */
    else
    {
	tarout = open(argv[optind], O_WRONLY | O_CREAT | O_TRUNC, 0644);
	if (tarout < 0)
	{
	    ffprintf(STDERR, "?can't open tarfile `%s' for writing: %s\n",
		     argv[optind], sys_errlist[errno]);
	    return 1;
	}
    }

    abscwd = util_abs_path(env_dir);
    cwdlen = strlen(abscwd);
    /* special case "/" */
    if (cwdlen == 1)
	cwdlen = 0;
    retval = -util_process_arglist(argv + optind + 1, do_tar);
    (void)free(abscwd);

    /* write trailing 2 null blocks */
    zero_tarhdr();
    (void)write(tarout, (char*)&tarhdr, sizeof(tarhdr));
    (void)write(tarout, (char*)&tarhdr, sizeof(tarhdr));

    if (tarout != 1)
	(void)close(tarout);

    client_done();

    return retval;
}
