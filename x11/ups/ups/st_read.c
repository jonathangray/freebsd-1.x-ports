/* st_read.c - low level code for opening and reading the symbol table */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_st_read_c_sccsid[] = "@(#)st_read.c	1.25 15/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <sys/types.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <a.out.h>
#include <errno.h>

#ifdef __STDC__
#include <unistd.h>
#endif

extern int errno;	/* some versions of errno.h don't declare this */

#include <local/ukcprog.h>
#include <mtrprog/strcache.h>
#include <mtrprog/utils.h>

#include "ups.h"
#include "symtab.h"
#ifdef ARCH_MIPS
#include "mips_frame.h"
#endif
#include "st_priv.h"

#define STRPAGESIZE	256

typedef const char *strpage_t[STRPAGESIZE];

#ifdef COFF_SUN386
#define DBX_TYPE(n) (((n)->n_leading_zero == 0 && (n)->n_dbx_type != 0) \
					? (n)->n_dbx_type : fake_dbx_type(n))

#define ST_COFF_MAGIC	I386MAGIC
#endif

#ifdef ST_TE
#ifdef MIPSEB
#define ST_COFF_MAGIC	MIPSEBMAGIC
#else
#define ST_COFF_MAGIC	MIPSELMAGIC
#endif
#endif

/*  Fields needed for reading symbols from the text file.
 */
typedef struct symiost {
	int si_fd;		/* File descriptor of the a.out file */
	off_t si_addr_to_fpos_offset;	/* Offset in file of start of text */
	strcache_id_t si_strcache_id;	/* Symbol strings cache handle */
	strpage_t **si_strpage_tab;	/* Table of cached sym string values */
	off_t si_syms_offset;	/* File offset of start of symbols */
	off_t si_strings_offset;/* File offset of symtab strings */
	int si_nsyms;		/* Number of symbols in this a.out file */
	int si_symbuf_nsyms;	/* Size of symbol buffer in symbols */
	nlist_t *si_symbuf;	/* Symbol buffer */
	int si_symbase;		/* Symno of first symbol in buffer */
	int si_symlim;		/* Symno of one after the last sym in the buffer */
	alloc_id_t si_alloc_id;
} symio_t;

static void fill_symbuf PROTO((symio_t *si, int symno));
#ifdef COFF_SUN386
static void get_sec_lno_offsets PROTO((int fd, off_t fpos,
							int count, fil_t *sfiles));
static int fake_dbx_type PROTO((nlist_t *n));
#endif

/*  Size of the buffer used by getsym, in units of SYMSIZE.
 *
 *  There are two buffer sizes - a large one for sequential
 *  reading and a small one for random reading.  The sequential scan
 *  routine (findsym) reduces the buffer size when it reads the last
 *  symbol.
 *
 *  Unfortunately SYMSIZE (12) is not an even divisor of 512 or 1024, so
 *  the read() buffer size is almost but not quite entirely unlike 1024.
 */
#define S_SYMBUF_NSYMS	256
#define R_SYMBUF_NSYMS	42

/*  A name for this machine, used in the error message in open_textfile().
 */
#ifdef ARCH_VAX
#define MNAME	"a VAX"
#else
#ifdef ARCH_SUN386
#define MNAME	"a Sun 386i"
#else
#if defined(ARCH_SUN2) || defined(ARCH_SUN3)
#define MNAME	"a Sun 2 or Sun 3"
#else
#ifdef ARCH_SUN4
#define MNAME	"a Sun 4"
#else
#ifdef ARCH_MIPS
#define MNAME	"a MIPS"
#else
#define MNAME	"an"
#endif /* !ARCH_MIPS */
#endif /* !ARCH_SUN4 */
#endif /* !ARCH_SUN{2,3} */
#endif /* !ARCH_SUN386 */
#endif /* !ARCH_VAX */

#ifdef COFF_SUN386
/*  Do a prescan of the line number information of an COFF file.
 *  The aim of this function is to set fu_lno_index and fu_num_lnos
 *  for each function with line number entries.  This prepares for
 *  future calls to read_func_coff_lnos().
 */
void
get_func_coff_lno_offsets(symio_id, sfiles)
symio_id_t symio_id;
fil_t *sfiles;
{
	symio_t *si;
	struct scnhdr *sectab, *st;
	int sectab_nbytes;
	struct filehdr filehdr;

	si = (symio_t *)symio_id;

	if (lseek(si->si_fd, (long)0, L_SET) == -1)
		panic("lseek failed in gfclo");
	if (read(si->si_fd, (char *)&filehdr, sizeof(filehdr)) != sizeof(filehdr))
		panic("read error in gfclo");

	sectab_nbytes = filehdr.f_nscns * sizeof(struct scnhdr);
	sectab = (struct scnhdr *)e_malloc(sectab_nbytes);

	if (lseek(si->si_fd, sizeof(struct aouthdr), L_INCR) == -1)
		panic("lseek failed in gfclo");
	if (read(si->si_fd, (char *)sectab, sectab_nbytes) != sectab_nbytes)
		panic("read failed in gfclo");
	
	for (st = sectab; st < &sectab[filehdr.f_nscns]; ++st) {
		if (st->s_nlnno != 0)
			get_sec_lno_offsets(si->si_fd, st->s_lnnoptr,
							      st->s_nlnno, sfiles);
	}

	free((char *)sectab);
}

static void
get_sec_lno_offsets(fd, fpos, count, sfiles)
int fd;
off_t fpos;
int count;
fil_t *sfiles;
{
	int lnnum;
	char *buf;
	const int bufcount = 1024;	/* # entries in buf */
	register char *iptr, *ilim;
	func_t *prev_f;
	taddr_t addr;
	fil_t *fil;
	funclist_t *fl;

	if (lseek(fd, fpos, L_SET) == -1)
		panic("lseek failed in gslo");

	/*  Note that LINESZ != sizeof(struct lineno) - the 386i C compiler
	 *  pads the 6 byte structure to 8 bytes.
	 */
	buf = e_malloc(bufcount * LINESZ);
	iptr = ilim = buf;

	addr = 0;
	fl = NULL;
	prev_f = NULL;
	fil = NULL;
	for (lnnum = 0; lnnum < count; ++lnnum) {
		struct lineno *l;

		if (iptr == ilim) {
			int to_read, nbytes;

			to_read = count - lnnum;
			if (to_read > bufcount)
				to_read = bufcount;
			nbytes = to_read * LINESZ;

			if (read(fd, buf, nbytes) != nbytes)
				panic("read error in gslo");
			iptr = buf;
			ilim = buf + nbytes;
		}
		l = (struct lineno *)iptr;
		iptr += LINESZ;

		/*  If l_lnno is zero, l_symndx points at the symbol table
		 *  entry for the first function in the source file, so we
		 *  must find which source file we are in.
		 */
		if (l->l_lnno == 0) {
			if (prev_f != NULL)
				FU_COFF_LNO_LIM(prev_f) = fpos + lnnum * LINESZ;
			for (fil = sfiles; fil != NULL; fil = fil->fi_next) {
				stf_t *stf;

				stf = (stf_t *)fil->fi_stf;
				if (l->l_addr.l_symndx >= stf->stf_symno &&
				    l->l_addr.l_symndx < stf->stf_symlim)
					break;
			}
			if (fil == NULL)
				panic("bad lnum in gslo");
			
			fl = fil->fi_funclist_head;
			if (fl == NULL)
				panic("fl botch in gslo");
			addr = fl->fl_func->fu_addr;
			prev_f = NULL;
		}
		else if (addr != 0 && l->l_addr.l_paddr > addr) {
			/*  FRAGILE CODE
			 *
			 *  We use ">" in the test above because the second
			 *  lineno entry for a function seems to correspond
			 *  to the first real line of the function.
			 *  Also, for the first function in the file, there
			 *  is no lineno with l_addr.l_paddr exactly equal
			 *  to the function address.
			 */
			if (prev_f != NULL)
				FU_COFF_LNO_LIM(prev_f) = fpos + lnnum * LINESZ;
			prev_f = fl->fl_func;
			FU_COFF_LNO_START(prev_f) = fpos + lnnum * LINESZ;
			fl = fl->fl_next;
			addr = (fl != NULL) ? fl->fl_func->fu_addr : 0;
		}
	}

	if (prev_f != NULL)
		FU_COFF_LNO_LIM(prev_f) = fpos + lnnum * LINESZ;
	
	free(buf);
}

lno_t *
read_func_coff_lnos(symio_id, f, p_last)
symio_id_t symio_id;
func_t *f;
lno_t **p_last;
{
	static char *buf = NULL;

	register char *iptr;
	lno_t *lnos, *ln;
	symio_t *si;
	static long buf_nbytes = 0;
	long nbytes;
	int count;

	nbytes = FU_COFF_LNO_LIM(f) - FU_COFF_LNO_START(f);
	if (nbytes == 0) {
		*p_last = NULL;
		return NULL;
	}
	if (nbytes % LINESZ != 0)
		panic("nbytes botch in rfcl");
	count = nbytes / LINESZ;

	si = (symio_t *)symio_id;

	if (nbytes > buf_nbytes) {
		if (buf != NULL)
			free(buf);
		buf_nbytes = nbytes + 128;
		buf = e_malloc(buf_nbytes);
	}

	if (lseek(si->si_fd, FU_COFF_LNO_START(f), L_SET) == -1)
		panic("lseek failed in rfcl");
	if (read(si->si_fd, buf, nbytes) != nbytes)
		panic("read failed in rfcl");
	
	
	lnos = (lno_t *)alloc(si->si_alloc_id, count * sizeof(lno_t));

	for (ln = lnos, iptr = buf; iptr < buf + nbytes; ++ln, iptr += LINESZ) {
		struct lineno *l;

		l = (struct lineno *)iptr;

		/*  From looking at 386i symbol tables, there seem to
		 *  be a lot of junk lineno entries after the last function
		 *  in a file.  The first of these seems to start with
		 *  l_lnno == 1, so we stop if we see this on other than
		 *  the first entry for a function.
		 */
		if (ln != lnos && l->l_lnno == 1)
			break;

		ln->ln_addr = l->l_addr.l_paddr;
		ln->ln_num = l->l_lnno;
		ln->ln_next = ln + 1;
	}
	ln[-1].ln_next = NULL;
	*p_last = ln - 1;
	return lnos;
}
#endif

/*  Open the a.out file execfile and check that it's reasonable.
 *
 *  If the file is OK, return a file descriptor referring to execfile
 *  open for reading and positioned at the start of the file.
 *
 *  On some machines we set *p_text_addr and *p_data_addr to the
 *  addresses of the start of the text and data.  We do this on
 *  machines where this information can't be got from the core file.
 *
 *  Otherwise give an error message and return -1.
 */
int
open_textfile(execfile, p_data_addr)
const char *execfile;
taddr_t *p_data_addr;
{
#ifdef ST_COFF
	struct filehdr hdr;
#else
	struct exec hdr;
#endif
	int fd, n_read;
	struct stat stbuf;

	if ((fd = open(execfile, 0)) < 0)
		errf("Can't open %s (%m)", execfile);
	else if (fstat(fd, &stbuf) == 0 && ((stbuf.st_mode & S_IFMT) != S_IFREG)) {
		errf("%s is a %s (expected a regular file)",
					execfile,
					filetype_to_string((int)stbuf.st_mode));
	}
	else if ((n_read = read(fd, (char *)&hdr, sizeof(hdr))) != sizeof(hdr)) {
		if (n_read == -1)
			errf("Error reading in %s (%m)", execfile);
		else
			errf("EOF in a.out header of %s", execfile);
	}
#ifdef ST_COFF
	else if (hdr.f_magic != ST_COFF_MAGIC)
		errf("%s is not %s a.out file", execfile, MNAME);
	else if (hdr.f_nsyms == 0)
		errf("%s has been stripped", execfile);
#else
	else if (N_BADMAG(hdr))
		errf("%s is not %s a.out file", execfile, MNAME);
	else if (hdr.a_syms == 0)
		errf("%s has been stripped", execfile);
	else if (hdr.a_syms % SYMSIZE != 0)
		errf("Bad symbol table size %d (not a multiple of %d)",
						hdr.a_syms, SYMSIZE);
#endif /* !ST_COFF */
	else if (lseek(fd, L_SET, (long)0) == -1)
		errf("Can't lseek in %s (%m)", execfile);
	else {
#ifdef ARCH_CLIPPER
		*p_data_addr = (taddr_t)N_DATADDR(hdr);
#else
		*p_data_addr = 0;
#endif
		return fd;
	}
	
	(void) close(fd);
	return -1;
}

#undef MNAME

/*  Given an executable file name and some other information, make a
 *  symio_t structure which can be use to read symbols and their strings
 *  from the text file.  Return an opaque handle on the structure.
 */
symio_id_t
make_symio(execfile, fd, syms_offset, nsyms, strings_offset,
						addr_to_fpos_offset, alloc_id)
const char *execfile;
int fd;
off_t syms_offset;
int nsyms;
off_t strings_offset;
long addr_to_fpos_offset;
alloc_id_t alloc_id;
{
	symio_t *si;
	strcache_id_t strcache_id;
	strpage_t **pagetab;
	int npages, pagenum;

	strcache_id = sc_make_fd_strcache(fd);

	npages = nsyms / STRPAGESIZE + 1;
	pagetab = (strpage_t **)alloc(alloc_id, sizeof(strpage_t *) * npages);
	for (pagenum = 0; pagenum < npages; ++pagenum)
		pagetab[pagenum] = NULL;

	si = (symio_t *) alloc(alloc_id, sizeof(symio_t));

	si->si_fd = fd;
	si->si_strcache_id = strcache_id;
	si->si_strings_offset = strings_offset;
	si->si_strpage_tab = pagetab;

	si->si_syms_offset = syms_offset;
	si->si_nsyms = nsyms;
	si->si_symbuf_nsyms = S_SYMBUF_NSYMS;
	si->si_symbuf = (nlist_t *) alloc(alloc_id, si->si_symbuf_nsyms * SYMSIZE);
	si->si_symbase = si->si_nsyms + 1;
	si->si_symlim = 0;

	si->si_addr_to_fpos_offset = addr_to_fpos_offset;

	si->si_alloc_id = alloc_id;

	return (symio_id_t)si;
}

/*  Change the address offset of symio_id by delta.  See change_text_addr_offset()
 *  We need this offset for textfile_tread().
 */
void
adjust_symio_addr_offset(symio_id, delta)
symio_id_t symio_id;
long delta;
{
	((symio_t *)symio_id)->si_addr_to_fpos_offset += delta;
}

/*  Return the number of symbols in the text file referred to by symio_id.
 *  Used by skim_symtab() to detect the end of the symbol table.
 */
int
get_symio_nsyms(symio_id)
symio_id_t symio_id;
{
	return ((symio_t *)symio_id)->si_nsyms;
}

/*  Close (and conceptually destroy) symio_id.  We don't actually free
 *  the structure here because space for it was allocated via alloc().
 */
void
close_symio(symio_id)
symio_id_t symio_id;
{
	sc_close_strcache(((symio_t *)symio_id)->si_strcache_id);
	close(((symio_t *)symio_id)->si_fd);
}

/*  Fill the symbols buffer of si so that symbol symno is at the start of
 *  the buffer.
 */
static void
fill_symbuf(si, symno)
symio_t *si;
int symno;
{
	int n_read, nsyms_to_read;

	if (symno < 0 || symno >= si->si_nsyms)
		panic("symno botch in fs");
	if (lseek(si->si_fd, si->si_syms_offset + symno * SYMSIZE, L_SET) == -1)
		panic("lseek failed in fs");
	nsyms_to_read = si->si_symbuf_nsyms;
	if (si->si_nsyms - symno < nsyms_to_read)
		nsyms_to_read = si->si_nsyms - symno;
	n_read = read(si->si_fd, (char *)si->si_symbuf, nsyms_to_read * SYMSIZE);
	if (n_read == -1)
		panic("read error in symbol table");
	if (n_read == 0)
		panic("unexpected EOF in symbol table");
	if (n_read % SYMSIZE != 0)
		panic("n_read botch in fs");
	si->si_symbase = symno;
	si->si_symlim = si->si_symbase + n_read / SYMSIZE;
}

/*  Get symbol symnum from the symbol table, and copy to *p_nm.
 */
void
getsym(symio_id, symno, p_nm)
symio_id_t symio_id;
int symno;
nlist_t *p_nm;
{
	register symio_t *si;
	char *src;

	si = (symio_t *)symio_id;

	if (symno < si->si_symbase || symno >= si->si_symlim)
		fill_symbuf(si, symno);

	/*  On the Sun 386, sizeof(nlist_t) is not the same as the
	 *  size of an nlist entry in the file.  Thus we have to
	 *  play silly games to get to the right entry.
	 */
	src = (char *)si->si_symbuf + (symno - si->si_symbase) * SYMSIZE;
	*p_nm = *(nlist_t *)src;
#ifdef COFF_SUN386
	p_nm->n_type = DBX_TYPE(p_nm);
#endif
}

#ifdef COFF_SUN386
/*  Construct an dbx symbol table format type from the various
 *  mangy bits of COFF info.
 */
static int
fake_dbx_type(n)
nlist_t *n;
{
	/*  Map from COFF section numbers to nm types.
	 *
	 *  The sections seem to be numbered from one on the nm.n_scnum stuff,
	 *  so we have a filler at zero.
	 *
	 *  BUG: should probably look at the section names and set this
	 *       array from them.
	 */
	static int sctypes[] = { N_UNDF, N_TEXT, N_DATA, N_BSS };

	if ((unsigned)n->n_scnum >= sizeof sctypes / sizeof *sctypes)
		return N_UNDF;
		
	return sctypes[n->n_scnum] | ((n->n_sclass == C_EXT) ? N_EXT : 0);
}
#endif

/*  Return a copy of the NUL terminated string at the given offset
 *  from the start of the strings for symio_id.
 */
const char *
si_get_string(symio_id, offset)
symio_id_t symio_id;
off_t offset;
{
	symio_t *si;
	long len;
	const char *s;

	si = (symio_t *)symio_id;
	s = sc_get_string(si->si_strcache_id, si->si_strings_offset + offset,
									'\0', &len);
	if (s == NULL)
		panic("str botch in sgs");
	return strcpy(alloc(si->si_alloc_id, (int)len + 1), s);
}


#ifndef ST_TE
/*  Search the symbol table starting from symbol number symno for a
 *  symbol whose type is in symset.  A symbol is wanted if symset[nm.n_type]
 *  is non zero.  If we find a wanted symbol, copy it to *p_nm and return
 *  its symbol number.  Otherwise return a symbol number one larger than
 *  that of the last symbol in the table.
 *
 *  This is used by the sequential prescan in skim_symtab().
 *
 *  See the comment in getsym for why p_nm below is "char *" rather
 *  than "nlist_t *".
 *
 *  Not used on the DECstation 3100.
 */
int
findsym(symio_id, symno, p_res, symset)
symio_id_t symio_id;
int symno;
nlist_t *p_res;
register const char *symset;
{
	register char *p_nm;
	register symio_t *si;
#ifdef COFF_SUN386
	int numaux;
#endif

	si = (symio_t *)symio_id;
	if (symno < si->si_symbase)
		fill_symbuf(si, symno);
	p_nm = (char *)si->si_symbuf + (symno - si->si_symbase) * SYMSIZE;
#ifdef COFF_SUN386
	numaux = 0;
#endif
	for (; symno < si->si_nsyms; p_nm += SYMSIZE, ++symno) {
		if (symno >= si->si_symlim) {
			fill_symbuf(si, symno);
			p_nm = (char *)si->si_symbuf;
		}
#ifdef COFF_SUN386
		if (numaux > 0) {
			--numaux;
			continue;
		}
		numaux = ((nlist_t *)p_nm)->n_numaux;
		((nlist_t *)p_nm)->n_type = DBX_TYPE((nlist_t *)p_nm);
#endif
		if (symset[((nlist_t *)p_nm)->n_type] != 0) {
			*p_res = *(nlist_t *)p_nm;
			return symno;
		}
	}

	/*  End of sequential scan, so shrink the buffer.
	 *
	 *  The buffer was allocated via alloc(), so we don't actually
	 *  change the size - we just drop si_symbuf_nsyms.  The next
	 *  fill of the buffer will use the reduced size.
	 */
	si->si_symbuf_nsyms = R_SYMBUF_NSYMS;

	return symno;
}

/*  Symbol table perversity #473: gcc on BSD/386 puts N_DATA symbols
 *  in the middle of a symbol whose definition is split over multiple
 *  lines.  So when we see a backslash continuation, we have to keep
 *  reading until we see a symbol of the same type as the first.
 */
const char *
get_cont_symstring(symio_id, p_symno)
symio_id_t symio_id;
int *p_symno;
{
#ifndef ARCH_BSDI386
	return symstring(symio_id, ++*p_symno);
#else
	int symtype;
	nlist_t nm;

	getsym(symio_id, *p_symno, &nm);
	symtype = nm.n_type;

	do {
		getsym(symio_id, ++*p_symno, &nm);
	} while (nm.n_type != symtype);

	return symstring(symio_id, *p_symno);
#endif
}

/*  Return the string for symbol symno.
 *
 *  Not used on the DECstation 3100.
 */
const char *
symstring(symio_id, symno)
symio_id_t symio_id;
int symno;
{
	symio_t *si;
	nlist_t nm;
	int pagenum, slot;
	strpage_t *page;
	
	si = (symio_t *)symio_id;

	if (symno < 0 || symno >= si->si_nsyms)
		panic("symno botch in ss");
	pagenum = symno / STRPAGESIZE;
	page = si->si_strpage_tab[pagenum];
	if (page == NULL) {
		int i;

		page = (strpage_t *)alloc(si->si_alloc_id, sizeof(strpage_t));
		for (i = 0; i < STRPAGESIZE; ++i)
			(*page)[i] = NULL;
		si->si_strpage_tab[pagenum] = page;
	}
	slot = symno % STRPAGESIZE;
	if ((*page)[slot] == NULL) {
		const char *line;

		getsym(symio_id, symno, &nm);
#ifdef COFF_SUN386
		if (nm.n_leading_zero != 0) {
			char *buf;

			buf = alloc(si->si_alloc_id, SYMNMLEN + 1);
			(void) memcpy(buf, nm.n_name, SYMNMLEN);
			buf[SYMNMLEN] = '\0';
			line = buf;
		}
		else /* revolting, but I can't think of a neater way */
#endif
		      if (nm.n_offset == 0) 
			line = NULL;
		else {
			long len;

			line = sc_get_string(si->si_strcache_id,
					     si->si_strings_offset + nm.n_offset,
					     '\0', &len);
			line = strcpy(alloc(si->si_alloc_id, (int)len + 1), line);
		}
		(*page)[slot] = line;
	}
	return (*page)[slot];
}
#endif /* !ST_TE */

/*  Read n bytes of text starting at virtual address addr from the
 *  a.out file.
 *
 *  BUG: this function refers to both symtab_t and symio_t structure
 *       members.
 */
int
textfile_tread(symtab_id, addr, buf, nbytes)
symtab_id_t symtab_id;
taddr_t addr;
char *buf;
int nbytes;
{
	symtab_t *st;
	symio_t *si;
	off_t pos;

	st = (symtab_t *)symtab_id;
	si = (symio_t *)st->st_symio_id;
	pos = addr - si->si_addr_to_fpos_offset;
	if (lseek(si->si_fd, pos, 0) != pos)
		return -1;
	return (read(si->si_fd, buf, nbytes) == nbytes) ? 0 : -1;
}
