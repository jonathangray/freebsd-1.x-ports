/* clinkdata.c:
 *
 * COPYRIGHT (c) 1989 by AT&T Bell Laboratories.
 *
 * Group files into the text segment of a .o file.  This is done by creating an
 * assembly file "runtime/allmo.s" that allocates space for the code and deals
 * with the relative addressing.  This file is then assembled, and then the actual
 * code is patched into allmo.o.
 *
 * Specifically, this file produces an ML value of the type
 *
 *    datatype datalist
 *      = DATANIL
 *      | DATACONS of (string * (unit -> unit) * datalist)
 *
 * where the "unit -> unit" function is an empty closure refering to the
 * code from the mo file.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef SOLARIS
#include <fcntl.h>
#else
#include <sys/file.h>
#endif
#include "tags.h"

#define MAGICNUM	0x18273645

/* RUNTIME is defined to be the path prefix of the runtime source directory */
#ifndef RUNTIME
#define RUNTIME "runtime"
#endif

#ifdef RS6000
#define SYMBOL		"datalist"
#define TEXT(F)		fprintf(F, ".toc\n.csect [RW]\n.align 2\n")
#define GLOBAL(F,S)	fprintf(F, ".globl %s\n", S)
#define WORD(F,N)	fprintf(F, ".long %d\n", N)
#define ADDR(F,A)	fprintf(F, ".long %s\n", A)
#define ADDR1(F,A,N)	fprintf(F, ".long %s+%d\n", A,N)
#define SPACE(F,N)	fprintf(F, ".space %d\n", N)
#else /* !RS6000 */
#ifdef MIPS
#define SYMBOL		"datalist"
#define TEXT(F)         fprintf(F, ".text\n.set noreorder\n")
#define GLOBAL(F,S)	fprintf(F, ".globl %s\n", S)
#define WORD(F,N)	fprintf(F, ".word %d\n", N)
#define ADDR(F,A)	fprintf(F, ".word %s\n", A)
#define ADDR1(F,A,N)	fprintf(F, ".word %s+%d\n", A,N)
#define SPACE(F,N)	fprintf(F, ".space %d\n", N)
#else /* !RS6000 && !MIPS */
#ifdef SPARC
#ifdef SOLARIS
#define SYMBOL		"datalist"
#else
#define SYMBOL		"_datalist"
#endif
#define TEXT(F)		fprintf(F, ".seg \"text\"\n")
#define GLOBAL(F,S)	fprintf(F, ".global %s\n", S)
#define WORD(F,N)	fprintf(F, ".word %d\n", N)
#define ADDR(F,A)	fprintf(F, ".word %s\n", A)
#define ADDR1(F,A,N)	fprintf(F, ".word %s+%d\n", A,N)
#define SPACE(F,N)	fprintf(F, ".skip %d\n", N)
#else /* !RS6000 && !MIPS && !SPARC */
#ifdef sun3
#define SYMBOL		"_datalist"
#define TEXT(F)		fprintf(F, ".text\n")
#define GLOBAL(F,S)	fprintf(F, ".globl %s\n", S)
#define WORD(F,N)	fprintf(F, ".long %d\n", N)
#define ADDR(F,A)	fprintf(F, ".long %s\n", A)
#define ADDR1(F,A,N)	fprintf(F, ".long %s+%d\n", A, N)
#define SPACE(F,N)	fprintf(F, ".skip %d\n", N)
#else /* !RS6000 && !MIPS && !SPARC && !sun3 */
#if defined(HPUX) && !defined(HPPA)
#define SYMBOL		"_datalist"
#define TEXT(F)		fprintf(F, "text\n")
#define GLOBAL(F,S)	fprintf(F, "global %s\n", S)
#define WORD(F,N)	fprintf(F, "long %d\n", N)
#define ADDR(F,A)	fprintf(F, "long %s\n", A)
#define ADDR1(F,A,N)	fprintf(F, "long %s+%d\n", A, N)
#define SPACE(F,N)	fprintf(F, "space %d\n", N)
#else /* !RS6000 && !MIPS && !SPARC && !sun3 && !HPUX */
#ifdef AUX
#define SYMBOL		"datalist"
#define TEXT(F)		fprintf(F, ".file \"allmo.s\"\n.text\n")
#define GLOBAL(F,S)	fprintf(F, ".globl %s\n", S)
#define WORD(F,N)	fprintf(F, ".long %d\n", N)
#define ADDR(F,A)	fprintf(F, ".long %s\n", A)
#define ADDR1(F,A,N)	fprintf(F, ".long %s+%d\n", A, N)
#define SPACE(F,N)	fprintf(F, ".space %d\n", N)
#else
#ifdef HPPA
#define SYMBOL		"datalist"
#define TEXT(F)		fprintf(F, "\t.space $PRIVATE$,SORT=16\n\t.subspa $DATA$,QUAD=0,ALIGN=4,ACCESS=63,SORT=16\n");
#define GLOBAL(F,S)	fprintf(F, "\t.export %s,DATA\n", S)
#define WORD(F,N)	fprintf(F, "\t.word %d\n", N)
#define ADDR(F,A)	fprintf(F, "\t.word %s\n", A)
#define ADDR1(F,A,N)	fprintf(F, "\t.word %s+%d\n", A, N)
#define SPACE(F,N)	fprintf(F, "\t.blockz %d\n", N)
#else /* !RS6000 && !MIPS && !SPARC && !sun3 && !HPUX && !AUX && !HPPA*/
#define SYMBOL		"_datalist"
#define TEXT(F)		fprintf(F, ".text\n")
#define GLOBAL(F,S)	fprintf(F, ".globl %s\n", S)
#define WORD(F,N)	fprintf(F, ".long %d\n", N)
#define ADDR(F,A)	fprintf(F, ".long %s\n", A)
#define ADDR1(F,A,N)	fprintf(F, ".long %s+%d\n", A, N)
#define SPACE(F,N)	fprintf(F, ".space %d\n", N)
#endif /* HPPA */
#endif /* AUX */
#endif /* HPUX */
#endif /* sun3 */
#endif /* SPARC */
#endif /* MIPS */
#endif /* RS6000 */
#ifdef HPPA
#  define LABEL(F,L)	fprintf(F, "\t.export %s,DATA\n%s\n", L,L)
#else
#  define LABEL(F,L)	fprintf(F, "%s:\n", L)
#endif

#define LONG_SZ	    (sizeof(long))
#define align(x)    (((x) + (LONG_SZ - 1)) & ~(LONG_SZ - 1))

void die(s, arg)
    char *s;
    int arg;
{
    char buf[64];
    sprintf(buf, s, arg);
    perror(buf);
    exit(1);
}

extern char *malloc();

/* make_sfile:
 * generate the assembly skeleton file
 */
void make_sfile (asfile, nfiles, mofiles)
    FILE	*asfile;
    int		nfiles;
    char	**mofiles;
{
    struct stat st;
    int n, len, str_sz, code_sz, i;
    char buf[64], buf2[64], lab_buf[8];
    static int lab_cnt = 0;

    TEXT(asfile);
    WORD(asfile, MAGICNUM);
    GLOBAL(asfile, SYMBOL);

    for (i = 0;  i < nfiles;  i++) {
	char *fname = mofiles[i];
      /* compute the space required for the file */
	if (stat(fname, &st) == -1)
	    die("stat(\"%s\")", fname);
	n = st.st_size;
	if (n == 0)
	    die("mo file is empty");
	len = strlen(fname);
	str_sz = align(len) + 2*LONG_SZ;  /* includes filename and code descriptors */
	code_sz = align(n);

      /* extract the structure name from the file name */
	{
	    register char *p, *begin;

	  /* find the beginning of the file name. */
	    for (p = begin = fname;  *p;  p++) {
		if (*p == '/')
		    begin = p+1;
	    }
	  /* find the ".mo" */
	    for (p = begin, len = 0;  *p && (*p != '.');  p++, len++)
		continue;
	    strncpy (buf, begin, len);
	    buf[len] = '\0';
	}

      /* add the datalist entry for this structure */
	if (i == 0)
	    LABEL(asfile, SYMBOL);
	WORD(asfile, MAKE_DESC(3, TAG_record));
	sprintf (lab_buf, "L%d", lab_cnt++);
	LABEL(asfile, lab_buf);
	ADDR1(asfile, lab_buf, 24);	/* mo filename string */
	ADDR1(asfile, lab_buf, 16);	/* closure address */
	if (i == (nfiles-1))
	    WORD(asfile, 1);		/* DATANIL */
	else
	    ADDR1(asfile, buf, code_sz+4);

      /* create the closure record */
	WORD(asfile, MAKE_DESC(1, TAG_record));
	ADDR1(asfile, buf, 4);		/* code string address */

      /* add a label and space for the structure to the "allmo.s" file. */
	SPACE(asfile, str_sz);
	LABEL(asfile, buf);
	SPACE(asfile, code_sz);		/* space for code (not including tag) */
    }

} /* end of make_sfile */


/* copy:
 * append the code from file to output, preceded by name.
 */
int copy (file, ofd)
    char *file; int ofd;
{
    int fd;
    long i, x, len, n, nbytes;
    char *s;
    struct stat st;
    static int bufsize = 0;
    static long *buf;

    if ((fd = open(file, 0)) == -1) {
	char	buf[64];
	sprintf(buf, "open: %s", file);
	die(buf);
    }
    if (fstat(fd, &st) == -1)
	die("stat");

    n = st.st_size;
    if (n == 0)
	die("mo file is empty");
    len = strlen(file);
    nbytes = LONG_SZ + align(n) + LONG_SZ + align(len);

    if (nbytes > bufsize) {
	bufsize = nbytes + 4096;
	if (buf != NULL)
	    free(buf);
	buf = (long *)malloc(bufsize);
	if (buf == NULL)
	    die("malloc(buf)");
    }

    buf[0] = MAKE_DESC(len, TAG_string);	   /* tag for file name */
    strcpy((char *) &buf[1], file);
    s = ((char *) &buf[1]) + len;
    for (i = align(len); i > len; i--)
	*s++ = 0;

    *((long *) s) = MAKE_DESC(n, TAG_string);  /* tag for code */
    s += LONG_SZ;

    for (x = n;  x > 0; ) {
	i = read(fd, s, x);
	if (i <= 0) die("read");
	s += i; x -= i;
     }

    for (i = align(n); i > n; i--) *s++ = 0;

    if (close(fd) == -1) die("close");

  /* advance the file pointer past the mo_list object and closure */
    if (lseek(ofd, 6*LONG_SZ, 1 /* L_INCR */) == -1) die("lseek");

    s = (char*)buf;
    for (x = nbytes;  x > 0; ) {
	i = write(ofd, s, x);
	if (i <= 0) die ("write");
	x -= i; s += i;
    }
}


int main(argc, argv)
    int argc;
    char *argv[];
{
    int i, pos, sts;
    FILE *asfile, *ofile; int ofd;
    char *as_buf, *o_buf, *cmd_buf;

    sprintf(as_buf = malloc(sizeof(RUNTIME)+16), "%s/allmo.s", RUNTIME);
    sprintf(o_buf = malloc(sizeof(RUNTIME)+16), "%s/allmo.o", RUNTIME);
    sprintf(cmd_buf = malloc(2*sizeof(RUNTIME)+sizeof(AS)+32), "%s %s -o %s",
	AS, as_buf, o_buf);

    asfile = fopen(as_buf,"w");
    if (asfile == NULL)
	die("fopen %s", as_buf);

    make_sfile (asfile, argc-1, argv+1);

    fclose (asfile);

    fprintf (stderr, "%s> %s\n", argv[0], cmd_buf);
    if ((sts = system (cmd_buf)) != 0) {
	fprintf (stderr, "cannot assemble %s, status = %d\n", as_buf, sts);
	exit (1);
    }

    ofile = fopen(o_buf,"r");
    if (!ofile)
	die("fopen %s", o_buf);
    for (i = 0, pos = 0;  i != MAGICNUM;  pos += LONG_SZ) {
	if (fread (&i, LONG_SZ, 1, ofile) == 0)
	    die("can't find magic num");
    }
    fclose (ofile);

    ofd = open (o_buf, O_RDWR, 0);
    if (ofd < 0)
	die("open %s", o_buf);

  /* set the file pointer to the tag word of the first datalist item */
    lseek (ofd, pos, 0 /* L_SET */);

    for (i = 1;  i < argc;  i++)
	copy (argv[i], ofd);

    close (ofd);

    exit (0);
}


