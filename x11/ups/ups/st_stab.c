/* st_stab.c - interface to the symbol table routines */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_st_stab_c_sccsid[] = "@(#)st_stab.c	1.31 18/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <a.out.h>

#ifdef __STDC__
#include <unistd.h>
#endif

#ifdef ARCH_VAX
#define N_TXTADDR(x)	0
#endif

#ifdef ARCH_CLIPPER
#ifndef N_TXTADDR
#define N_TXTADDR(x)	0x8000
#endif
#endif

#include <local/ukcprog.h>
#include <local/arg.h>
#include <mtrprog/genmergesort.h>
#include <mtrprog/strcache.h>
#include <mtrprog/alloc.h>

#include "ups.h"
#include "symtab.h"
#include "ci.h"
#ifdef ARCH_MIPS
#include "mips_frame.h"
#endif
#include "st_priv.h"
#include "text.h"
#include "obj_bpt.h"
#include "src.h"
#include "proc.h"

static symtab_t *get_symtab PROTO((const char *execfile, symtab_type_t symtab_type,
				   int fd, taddr_t text_addr_offset,
				   target_info_t *target_info,
				   int *p_have_common_blocks));

static int load_func_info PROTO((func_t *f, taddr_t unused_addr,
					    char *verbose, char *unused_arg2));

#ifdef OS_SUNOS_4
static void adjust_addrlist_addr_offset PROTO((addrlist_t *addrlist, long delta));
static int load_shlib_symtabs PROTO((shlib_t *shlibs, target_info_t *target_info,
							     symtab_t **p_stlist));
static void close_symtab PROTO((symtab_t *st));
static void free_symtab_cache PROTO((void));
static symtab_t *get_symtab_from_cache PROTO((const char *name));
static void adjust_fil_vars_addr_base PROTO((fil_t *flist, long delta));
static void change_text_addr_offset PROTO((symtab_t *st, taddr_t new_addr));
#endif
static block_t *get_rootblock PROTO((void));
static block_t *make_rootblock PROTO((void));

static int Verbose;

/*  List of symbol tables.  The list only has one entry unless we are using
 *  SunOS 4.0 shared libraries.
 */
static symtab_t *Main_st = NULL;

/*  Cached list of free symbol tables.  We keep this only between one run
 *  of the target and the next.
 */
static symtab_t *Symtab_cache_list = NULL;

/*  Insert a new entry at the front of the globals address map
 *  for symbol table st.
 */
addrlist_t *
insert_global_addr(alloc_id, addrlist, name, addr)
alloc_id_t alloc_id;
addrlist_t *addrlist;
const char *name;
taddr_t addr;
{
	addrlist_t *al;

	al = (addrlist_t *)alloc(alloc_id, sizeof(addrlist_t));
	al->al_name = name;
	al->al_addr = addr;
	al->al_var = NULL;
	al->al_next = addrlist;
	return al;
}

#ifdef OS_SUNOS_4
/*  Look up the address of global variable name.  Return the
 *  address, or 0 if name is not found.
 */
static void
adjust_addrlist_addr_offset(addrlist, delta)
addrlist_t *addrlist;
long delta;
{
	addrlist_t *al;

	for (al = addrlist; al != NULL; al = al->al_next)
		al->al_addr += delta;
}
#endif /* OS_SUNOS_4 */

/*  Look up the address of global variable name.  Return the
 *  address, or 0 if name is not found.
 */
taddr_t
find_addr_in_addrlist(addrlist, name)
addrlist_t *addrlist;
const char *name;
{
	addrlist_t *al;

	for (al = addrlist; al != NULL; al = al->al_next)
		if (strcmp(al->al_name, name) == 0)
			return al->al_addr;
	return 0;
}

/*   Find the address of a global.  Return 0 if not found (this will happen
 *   for an extern with no definition).
 *
 *   We look first in the symbol table st, then in Main_st if st isn't
 *   itself Main_st.
 */
taddr_t
lookup_global_addr(st, name)
symtab_t *st;
const char *name;
{
	taddr_t addr;

	addr = find_addr_in_addrlist(st->st_addrlist, name);
	if (addr == 0 && st != Main_st)
		addr = find_addr_in_addrlist(Main_st->st_addrlist, name);
	return addr;
}

static int
load_func_info(f, unused_addrlim, verbose, unused_arg2)
func_t *f;
taddr_t unused_addrlim;
char *verbose, *unused_arg2;
{
	if ((bool)verbose)
		errf("\bloading symbols of function %s", f->fu_name);
	(void) min_bpt_addr(f);
	(void) get_fu_lnos(f);
	(void) get_fu_blocks(f);
	return 0;
}

/*  Load all symbol table information.  This is used for debugging.
 */
void
debug_load_symbols(name)
const char *name;
{
	symtab_t *st;
	fil_t *fil;

	if (*name != '\0') {
		func_t *f;

		if (strchr(name, '.') != NULL || strchr(name, ':') == NULL) {
			if ((fil = name_to_fil(name)) != NULL)
				get_fi_vars(fil);
			else
				errf("Unknown source file name %s", name);
		}
		else if (find_func_by_name(name, &f) == 0) {
			load_func_info(f, (taddr_t)0, (char *)FALSE, (char *)0);
		}
	}
	else {
		for (st = Main_st; st != NULL; st = st->st_next) {
			errf("Loading entire symbol table");
			iterate_over_functions((symtab_id_t)st, load_func_info,
							(char *)TRUE, (char *)0);
			for (fil = st->st_sfiles; fil != NULL; fil = fil->fi_next)
				(void) get_fi_vars(fil);
			errf("Entire symbol table loaded");
		}
	}
}

#ifdef OS_SUNOS_4
/*  Close down a symbol table.  This is called if we rerun a
 *  target and find that it uses a different shared library
 *  from the last run (rare, but could happen if the
 *  environment variable LD_LIBRARY_PATH is changed).
 *  If we extended ups to allow multiple binaries in the
 *  same run this routine would also get called.
 *
 *  Free all the resources and memory associated with this
 *  symbol table (the memory part is made easy by alloc()).
 */
static void
close_symtab(st)
symtab_t *st;
{
	fil_t *fil;

	/*  Delete any breakpoints from this symbol table
	 */
	remove_symtab_breakpoints((symtab_id_t)st);

	/*  Close any source files we have open.  Also delete edit blocks
	 *  belonging to the source files.
	 */
	for (fil = st->st_sfiles; fil != NULL; fil = fil->fi_next) {
		remove_all_editblocks(fil, DO_CALL_CALLBACK);
		if (fil->fi_so != 0)
			so_close(fil->fi_so);
	}

	free_cblist_strings(st->st_cblist_id);

	close_symio(st->st_symio_id);

	/*  Free all storage associated with this symtab.
	 *  Vars, functions, common blocks, the lot.
	 */
	alloc_free_pool(st->st_alloc_id);
}

/*  Free any symbol tables in the cache.  This is called just
 *  after startup of the target to flush any cached symbol
 *  tables that weren't in fact used.
 */
static void
free_symtab_cache()
{
	symtab_t *st, *next;

	for (st = Symtab_cache_list; st != NULL; st = next) {
		next = st->st_next;
		close_symtab(st);
	}
}

/*  Look up the symbol table for name in the cache.  Remove
 *  it from the cache and return it if found, otherwise
 *  return NULL.
 *
 *  Name is the name of the executable or shared library file
 *  that the symbol table came from.
 */
static symtab_t *
get_symtab_from_cache(name)
const char *name;
{
	symtab_t *st, *prev;

	prev = NULL;
	for (st = Symtab_cache_list; st != NULL; st = st->st_next) {
		if (strcmp(name, st->st_name) == 0) {
			if (prev != NULL)
				prev->st_next = st->st_next;
			else
				Symtab_cache_list = st->st_next;
			st->st_next = NULL;		/* for safety */
			return st;
		}
		prev = st;
	}
	return NULL;
}

/*  Adjust the addresses of all the global variables associated
 *  with source files in flist.  Called when a shared library
 *  mapping address changes across runs of the target.
 */
static void
adjust_fil_vars_addr_base(flist, delta)
fil_t *flist;
long delta;
{
	fil_t *fil;
	var_t *v;

	for (fil = flist; fil != NULL; fil = fil->fi_next) {
		((stf_t *)fil->fi_stf)->stf_addr += delta;
		if (fil->fi_flags & FI_DONE_VARS)
			for (v = fil->fi_block->bl_vars; v != NULL; v = v->va_next)
				v->va_addr += delta;
	}
}

/*  Deal with a change in the text offset of a symbol table.  This may
 *  be necessary when re-running the target as shared libraries may be
 *  mapped at different addresses.  It's also necessary when we have
 *  preloaded symbol tables with a nominal offset of zero.
 *
 *  We adjust the following:
 *
 *	function and line number addresses
 *	symbol table address to text file offset
 *	addresses of global variables
 *
 *  We don't change breakpoint addresses here - we do that by removing
 *  and recreating all breakpoints just after starting the target (see
 *  start_target() in exec.c.
 */
static void
change_text_addr_offset(st, new_addr)
symtab_t *st;
taddr_t new_addr;
{
	long delta;

	delta = new_addr - st->st_text_addr_offset;
	if (delta != 0) {
		adjust_addrlist_addr_offset(st->st_addrlist, delta);
		adjust_symio_addr_offset(st->st_symio_id, delta);
		adjust_functab_text_addr_base(st->st_functab_id,
						st->st_funclist, delta);
		adjust_fil_vars_addr_base(st->st_sfiles, delta);
		st->st_text_addr_offset = new_addr;
	}
}

/*  Load the symbol tables for the shared libraries of an object.
 *  This is called just after the target starts, because only
 *  then have the shared libraries been loaded and mapped.
 */
int
load_shared_library_symtabs()
{
	static char dynamic[] = "__DYNAMIC";
	const char *sym0_name;
	nlist_t nm;
	taddr_t addr;
	shlib_t *shlibs;
	symtab_t *stlist;
	alloc_id_t alloc_id;

	if (Main_st == NULL || Main_st->st_next != NULL)
		panic("shared lib botch");

	if (Main_st->st_dynamic) {
		sym0_name = symstring(Main_st->st_symio_id, 0);
		if (strcmp(sym0_name, dynamic) != 0) {
			errf("First symbol in %s is %s (expected %s)",
					Main_st->st_name, sym0_name, dynamic);
			return -1;
		}
		getsym(Main_st->st_symio_id, 0, &nm);
		addr = nm.n_value;

		if (Verbose)
			fputs("Get shlib names and global addresses...", stderr);
		alloc_id = alloc_create_pool();

		if (get_shlibs_and_global_addrs(alloc_id, Main_st, addr,
								&shlibs) != 0) {
			alloc_free_pool(alloc_id);
			return -1;
		}
		if (Verbose)
			fputs("done\n", stderr);
		if (load_shlib_symtabs(shlibs, Main_st->st_target_info,
								&stlist) != 0) {
			alloc_free_pool(alloc_id);
			return -1;
		}
		alloc_free_pool(alloc_id);
		free_symtab_cache();
		Main_st->st_next = stlist;
	}
	return 0;
}

/*  Load the shared libraries desribed in shlibs, and point *p_stlist
 *  at the loaded list.  Return 0 for success, -1 for failure.
 *  In the normal case all the shared libraries will be in the
 *  cache, either from the preload or from a previous run of the
 *  target.
 */
static int
load_shlib_symtabs(shlibs, target_info, p_stlist)
shlib_t *shlibs;
target_info_t *target_info;
symtab_t **p_stlist;
{
	symtab_t *stlist, *st;
	shlib_t *sh;
	int shlib_fd, have_cblocks;
	taddr_t junk;

	stlist = NULL;
	for (sh = shlibs; sh != NULL; sh = sh->sh_next) {
		if ((st = get_symtab_from_cache(sh->sh_name)) != NULL)
			change_text_addr_offset(st, sh->sh_addr);
		else {
			if ((shlib_fd = open_textfile(sh->sh_name, &junk)) == -1)
				return -1;
			st = get_symtab(sh->sh_name, STT_SHLIB, shlib_fd,
						sh->sh_addr, target_info,
						&have_cblocks);
			if (st == NULL)
				return -1;
		}
		st->st_next = stlist;
		stlist = st;
	}
	*p_stlist = stlist;
	return 0;
}

/*  Unload shared library symbol tables.  This is called when the
 *  target dies.  We don't free the symbol tables at this point - we
 *  just put them in the cache in anticipation of using them again
 *  when the target is re-run.
 */
void
unload_shared_library_symtabs()
{
	symtab_t *st, *next, *prev;

	prev = NULL;
	for (st = Main_st; st != NULL; st = next) {
		next = st->st_next;
		if (st->st_type == STT_SHLIB) {
			if (prev != NULL)
				prev->st_next = st->st_next;
			else
				Main_st = st->st_next;
			st->st_next = Symtab_cache_list;
			Symtab_cache_list = st;
		}
		else
			prev = st;
	}
}
#endif /* OS_SUNOS_4 */

const char *
get_target_name(fil)
fil_t *fil;
{
	return ((stf_t *)fil->fi_stf)->stf_symtab->st_target_info->ti_name;
}

long
get_target_mod_time(fil)
fil_t *fil;
{
	return ((stf_t *)fil->fi_stf)->stf_symtab->st_target_info->ti_mod_time;
}

void
set_highlighted_line(fil, lnum)
fil_t *fil;
int lnum;
{
	target_info_t *ti;

	ti = ((stf_t *)fil->fi_stf)->stf_symtab->st_target_info;
	ti->ti_highlighted_fil = fil;
	ti->ti_highlighted_lnum = lnum;
}

bool
lnum_is_highlighted(fil, lnum)
fil_t *fil;
int lnum;
{
	target_info_t *ti;

	ti = ((stf_t *)fil->fi_stf)->stf_symtab->st_target_info;
	return fil == ti->ti_highlighted_fil && lnum == ti->ti_highlighted_lnum;
}

/*  Get the main symbol table for execfile, via fd which should
 *  be a file descriptor referring to execfile.  The file pointer
 *  is assumed to be at the start of the file.  We set p_have_common_blocks
 *  depending on whether any FORTRAN common blocks were found.
 * 
 *  If execfile is a shared library, we try to guess which
 *  shared libraries it will use and preload these into the
 *  shared library cache.  This is to allow the user to set
 *  breakpoints in shared library functions before the target
 *  is started, and to avoid a long pause when the target is
 *  started for the first time.
 */
int
get_and_install_symtabs(execfile, fd, p_have_common_blocks)
const char *execfile;
int fd;
int *p_have_common_blocks;
{
#ifdef OS_SUNOS_4
	shlib_t *shlibs;
	symtab_t *stlist;
#endif
	symtab_t *st;
	struct stat stbuf;
	target_info_t *target_info;

	Verbose = getenv("VERBOSE") != NULL;

	if (fstat(fd, &stbuf) != 0)
		panic("fstat failed");

	target_info = (target_info_t *)e_malloc(sizeof(target_info_t));
	target_info->ti_name = execfile;
	target_info->ti_mod_time = stbuf.st_mtime;
	target_info->ti_highlighted_fil = NULL;

	st = get_symtab(execfile, STT_MAIN, fd, (taddr_t)0, target_info,
							p_have_common_blocks);
	if (st == NULL)
		return -1;
	st->st_next = NULL;
	Main_st = st;
#ifdef OS_SUNOS_4
	if (st->st_dynamic) {
		alloc_id_t alloc_id;

		if (Verbose)
			fputs("Get preload list...", stderr);
		alloc_id = alloc_create_pool();
		get_preload_shlib_list(alloc_id, execfile, &shlibs);
		if (Verbose)
			fputs("done\n", stderr);
		if (load_shlib_symtabs(shlibs, target_info, &stlist) != 0) {
			alloc_free_pool(alloc_id);
			return -1;
		}
		alloc_free_pool(alloc_id);
		Symtab_cache_list = stlist;
	}
#endif /* OS_SUNOS_4 */
	return 0;
}

static block_t *
get_rootblock()
{
	static block_t *block = NULL;

	if (block == NULL)
		block = make_rootblock();
	
	return block;
}

static block_t *
make_rootblock()
{
	static struct {
		const char *regname;
		taddr_t regno;
	} regtab[] = {
		"$pc",	REG_PC,
		"$fp",	REG_FP,
		"$sp",	REG_SP
	};
#ifdef PURIFY
	static alloc_id_t save_alloc_id;
#endif
	alloc_id_t alloc_id;
	type_t *type;
	block_t *block;
	int i;

	alloc_id = alloc_create_pool();
#ifdef PURIFY
	save_alloc_id = alloc_id;
#endif

	block = ci_make_block(alloc_id, (block_t *)NULL);

	type = ci_code_to_type(TY_INT);

	for (i = 0; i < sizeof regtab / sizeof *regtab; ++i) {
		var_t *v;

		v = ci_make_var(alloc_id, regtab[i].regname, CL_REG, type,
							regtab[i].regno);

		/*  This is pretty bogus.  The problem is that the register
		 *  values for these registers are stored in a data
		 *  structure in our address space, and we don't have an
		 *  address in the target to hand.  Thus we pass back
		 *  (in get_reg_addr) the address in our address space,
		 *  and set the flag below to make it look there.
		 */
		v->va_flags |= VA_IS_CI_VAR;

		v->va_next = block->bl_vars;
		block->bl_vars = v;
	}

	return block;
}

/*  Do a prescan of the symbol table of execfile, which is assumed to
 *  be an a.out file which has been checked by check_execfile.
 *
 *  Fd is a read only file descriptor referring to execfile, and a is
 *  a pointer to its exec header.
 *
 *  If successful, we set *p_have_common_blocks to TRUE iff the symbol
 *  table of execfile contains common blocks, and return 0.
 *
 *  Otherwise we give an error message and return -1.
 */
static symtab_t *
get_symtab(execfile, symtab_type, fd, text_addr_offset, ti, p_have_common_blocks)
const char *execfile;
symtab_type_t symtab_type;
int fd;
taddr_t text_addr_offset;
target_info_t *ti;
int *p_have_common_blocks;
{
	long addr_to_fpos_offset;
#ifdef ST_COFF
	struct filehdr hdr;
	struct aouthdr aouthdr;
#else
	struct exec hdr;
#endif
	symtab_t *st;
	fil_t *sfiles;
	alloc_id_t alloc_id;
	functab_id_t functab_id;
	cblist_id_t cblist_id;
	off_t file_text_offset, file_syms_offset;
	taddr_t mem_text_offset, text_size;
#ifndef ST_TE
	int nsyms;
	off_t file_strings_offset;
#endif

	if (Verbose)
		fprintf(stderr, "Loading symtab %s...", execfile);

	if (read(fd, (char *)&hdr, sizeof(hdr)) != sizeof(hdr)) {
		errf("Read error in %s (%m)", execfile);
		return NULL;
	}
#ifdef ST_COFF
	if (read(fd, (char *)&aouthdr, sizeof(aouthdr)) != sizeof(aouthdr)) {
		errf("Read error in %s (%m)", execfile);
		return NULL;
	}
#endif

	alloc_id = alloc_create_pool();

	st = (symtab_t *) alloc(alloc_id, sizeof(symtab_t));
	st->st_alloc_id = alloc_id;

	st->st_name = strcpy(alloc(alloc_id, strlen(execfile) + 1), execfile);
	st->st_type = symtab_type;
	st->st_target_info = ti;

#ifdef OS_SUNOS_4
#ifdef COFF_SUN386
	st->st_dynamic = aouthdr.vstamp == SUNVERSION_DYNAMIC;
#else
	st->st_dynamic = hdr.a_dynamic;
#endif
#else
	st->st_dynamic = FALSE;
#endif

#ifdef COFF_SUN386
	mem_text_offset = aouthdr.text_start;

	/*  The STT_MAIN case is documented in the SunOS coff(5) manual page.
	 *  The shared library case is not documented as far as I know.
	 *  I decided on zero from looking at an example of a shared library.
	 */
	if (symtab_type == STT_MAIN)
		file_text_offset = mem_text_offset - 0x1000;
	else
		file_text_offset = 0;

	text_size = aouthdr.tsize;
	file_syms_offset = hdr.f_symptr;
	nsyms = hdr.f_nsyms;
	file_strings_offset = hdr.f_symptr + hdr.f_nsyms * SYMSIZE;
#else
#ifdef ST_TE
	mem_text_offset = aouthdr.text_start;
	file_text_offset = N_TXTOFF(hdr, aouthdr);
	text_size = aouthdr.tsize;
	file_syms_offset = hdr.f_symptr;
#else
	mem_text_offset = (taddr_t)N_TXTADDR(hdr); /* TXTADDR is sometimes caddr_t */
	file_text_offset = N_TXTOFF(hdr);
	text_size = hdr.a_text;
	file_syms_offset = N_SYMOFF(hdr);
	nsyms = hdr.a_syms / SYMSIZE;
	file_strings_offset = N_STROFF(hdr);
#endif /* !ST_TE */
#endif

	addr_to_fpos_offset = text_addr_offset - file_text_offset;
	if (symtab_type == STT_MAIN)
		addr_to_fpos_offset += mem_text_offset;

	st->st_text_addr_offset = text_addr_offset;
	st->st_addrlist = NULL;

#ifdef ST_TE
	if (skim_te_symtab(st, fd, file_syms_offset, addr_to_fpos_offset,
				text_addr_offset,
				text_addr_offset + mem_text_offset + text_size,
				get_rootblock(),
				p_have_common_blocks,
				&sfiles, &functab_id, &cblist_id) != 0) {
		alloc_free_pool(alloc_id);
		return NULL;
	}
#else
	st->st_symio_id = make_symio(execfile, fd, file_syms_offset, nsyms,
				     file_strings_offset, addr_to_fpos_offset,
				     alloc_id);
	skim_symtab(st, text_addr_offset,
			text_addr_offset + mem_text_offset + text_size,
			get_rootblock(),
			p_have_common_blocks,
			&sfiles,
			&functab_id,
			&cblist_id);
#endif
	
	st->st_sfiles = sfiles;
	st->st_functab_id = functab_id;
	st->st_cblist_id = cblist_id;

	if (Verbose)
		fputs("done\n", stderr);

	return st;
}

symtab_t *
get_main_st()
{
	return Main_st;
}

symtab_t *
get_symtab_cache_list()
{
	return Symtab_cache_list;
}
