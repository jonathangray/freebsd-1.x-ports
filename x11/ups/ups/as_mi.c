/* as_mi.c - ups machine independent assembler interface */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_as_mi_c_sccsid[] = "@(#)as_mi.c	1.18 12/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <sys/types.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include <local/ukcprog.h>
#include <mtrprog/utils.h>

#include "ups.h"
#include "symtab.h"
#include "trun.h"
#include "as.h"
#include "text.h"
#include "debug.h"

#ifdef KNOW_ASM
static void disassemble PROTO((FILE *fp, func_t *f, taddr_t addr,
							const char *text, int len));
static void disassemble_text_range PROTO((FILE *fp, func_t *f,
					       taddr_t firstaddr, taddr_t lastaddr));
static int disassemble_func PROTO((func_t *f, taddr_t addrlim,
						char *cfp, char *unused_arg2));
static int dump_func_as_assembler PROTO((func_t *f, const char *filename,
					 bool overwrite, bool want_source));

static void
disassemble(fp, f, addr, text, len)
FILE *fp;
func_t *f;
taddr_t addr;
const char *text;
int len;
{
	bool want_offset_addrs, flush_each_line;
	const char *name, *next, *lim, *asmline;

	want_offset_addrs = (Debug_flags & DBFLAG_ASM_OFFSET_ADDRS) != 0;
	flush_each_line = (Debug_flags & DBFLAG_ASM_LINEBUF) != 0;

	name = f->fu_name;
	if (want_offset_addrs && *name == '[' && (Debug_flags & DBFLAG_DBXASM)) {
		static char namebuf[50];

		name = strcpy(namebuf, name + 1);
		namebuf[strlen(name) - 1] = '\0';
	}

	lim = text + len;
	while (text < lim) {
		next = disassemble_one_instruction(addr, text, &asmline);
		if (want_offset_addrs) {
			int offset;

			offset = addr - f->fu_addr;
			if (offset == 0)
				fprintf(fp, "%s:\t%s\n", name, asmline);
			else if (offset < 10)
				fprintf(fp, "%s+%d:\t%s\n", name, offset, asmline);
			else
				fprintf(fp, "%s+0x%x:\t%s\n", name, offset, asmline);
		}
		else
			fprintf(fp, "\t%08x  %s\n", addr, asmline);
		if (flush_each_line)
			fflush(fp);
		addr += next - text;
		text = next;
	}
}

static void
disassemble_text_range(fp, f, firstaddr, lastaddr)
FILE *fp;
func_t *f;
taddr_t firstaddr, lastaddr;
{
	char *buf;
	int nbytes;

	nbytes = lastaddr + 1 - firstaddr;

	if (nbytes <= 0) {
		fputs("\t<No text>\n", fp);
		return;
	}

	buf = e_malloc(nbytes);
	if (textfile_tread(f->fu_symtab_id, firstaddr, buf, nbytes) == 0)
		disassemble(fp, f, firstaddr, buf, nbytes);
	else
		errf("Tread failed in dtr");
	free(buf);
}

/* ARGSUSED */
static int
disassemble_func(f, addrlim, cfp, unused_arg2)
func_t *f;
taddr_t addrlim;
char *cfp;
char *unused_arg2;
{
	FILE *fp;
	taddr_t base, lim;

	fp = (FILE *)cfp;
	base = f->fu_addr;
#ifdef ARCH_VAX
	base += 2;		/* VAXen have reg save mask at func addr */
#endif
	lim = addrlim - 1;
	if ((Debug_flags & DBFLAG_DBXASM) == 0)
		fprintf(fp, "\n%s: 0x%x - 0x%x\n", f->fu_name, base, lim);
	disassemble_text_range(fp, f, base, lim);
	return 0;
}

static int
dump_func_as_assembler(f, filename, overwrite, want_source)
func_t *f;
const char *filename;
bool overwrite, want_source;
{
	char *buf;
	const char *srcline;
	taddr_t addr, nextaddr;
	int buflen, linelen;
	lno_t *l;
	FILE *fp;
	symtab_id_t symtab_id;
	extern func_t *Main_func;

	fp = overwrite ? fopen(filename, "w") : fopen_new_file(filename);
	if (fp == NULL)
		return -1;
	
	symtab_id = (f != NULL) ? f->fu_symtab_id : Main_func->fu_symtab_id;
	if (f == NULL) {
		iterate_over_functions(symtab_id, disassemble_func,
						(char *)fp, (char *)symtab_id);
	}
	else if (FU_LNOS(f) == NULL || !want_source)
		disassemble_func(f, get_addr_lim(f), (char *)fp, (char *)symtab_id);
	else {
		if (open_source_file(f->fu_fil, TRUE) != 0)
			return -1;
		buflen = 128;
		buf = e_malloc(buflen);
		for (l = FU_LNOS(f); l != NULL; l = l->ln_next) {
			addr = l->ln_addr;
			if (l->ln_next != NULL)
				nextaddr = l->ln_next->ln_addr;
			else
				nextaddr = get_addr_lim(f);
			if (nextaddr == addr)
				continue;
			linelen = nextaddr - addr;
			if (linelen > buflen) {
				buflen = linelen;
				buf = e_realloc(buf, buflen);
			}
			if (textfile_tread(symtab_id, addr, buf, linelen) != 0) {
				errf("Tread failed in dfaa");
				free(buf);
				return -1;
			}
			if (f->fu_fil->fi_so != 0)
				srcline = so_getline(f->fu_fil->fi_so, l->ln_num-1);
			else
				srcline = "";
			fprintf(fp, "\n%d: %s\n", l->ln_num, srcline);
			disassemble(fp, f, addr, buf, linelen);
		}
		free(buf);
	}

	return fclose_new_file(fp, filename);
}
#endif /* KNOW_ASM */

void
dump_as_assembler(funcname, want_source)
const char *funcname;
int want_source;
{
	func_t *f;
	const char *filename;
	char *newname, *what;
	bool overwrite;

	if ((filename = strchr(funcname, '>')) == NULL) {
		filename = "asm.s";
		newname = NULL;
		overwrite = TRUE;
	}
	else {
		const char *pos;

		for (pos = filename; pos > funcname && pos[-1] == ' '; --pos)
			;

		if (overwrite = filename[1] == '!')	/* note '=' */
			++filename;

		for (++filename; isspace(*filename); ++filename)
			;
		if (*filename == '\0') {
			errf("Redirection to zero length output file");
			return;
		}

		newname = strf("%.*s", pos - funcname, funcname);
		funcname = newname;
	}

	if (*funcname == '\0') {
		f = NULL;
		what = strsave("Entire text");
	}
	else {
		if (find_func_by_name(funcname, &f) != 0) {
			if (newname != NULL)
				free(newname);
			return;
		}

		if (f->fu_fil == NULL)
			what = strsave(f->fu_name);
		else
			what = strf("%s:%s", f->fu_fil->fi_name, f->fu_name);
	}

#ifdef KNOW_ASM
	if (dump_func_as_assembler(f, filename, overwrite, want_source) == 0)
		errf("\b%s dumped as assembler to %s", what, filename);
#else
	errf("Can't disassemble %s - don't know assembler for this machine",
								    what);
#endif /* !KNOW_ASM */

	free(what);
	if (newname != NULL)
		free(newname);
}

const char *
hex(n)
int n;
{
	static char buf[1 + 2 + 8 + 1];	/* - 0x ffffffff NUL */
#if ARCH_SUN3
	char *cptr;

	cptr = buf;
	if (n < 0) {
		n = -n;
		*cptr++ = '-';
	}
	(void) sprintf(cptr, (n < 10) ? "%d" : "0x%x", n);
#else
	sprintf(buf, "0x%x", n);
#endif
	return buf;
}

const char *
addr_to_func_and_offset(addr, allow_offset)
taddr_t addr;
int allow_offset;
{
	static char buf[50], namebuf[30];
	func_t *f;

	if ((f = addr_to_func(addr)) != NULL) {
		const char *name;

		name = f->fu_name;
		if (*name == '[' && (Debug_flags & DBFLAG_DBXASM)) {
			name = strcpy(namebuf, name + 1);
			namebuf[strlen(name) - 1] = '\0';
		}

		if (f->fu_addr == addr)
			(void) strcpy(buf, name);
		else if (allow_offset)
			(void) sprintf(buf, "%s+%s", name,
						hex((int)addr - (int)f->fu_addr));
		else
			(void) strcpy(buf, hex((int)addr));
	}
	else
		(void) strcpy(buf, hex((int)addr));
	return buf;
}
