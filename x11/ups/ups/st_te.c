/* st_te.c - routines to read a DECstation (Third Eye) symbol table */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_st_te_c_sccsid[] = "@(#)st_te.c	1.15 15/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <sys/types.h>
#include <stdlib.h>
#include <a.out.h>
#include <sys/file.h>

#include <local/ukcprog.h>
#include <mtrprog/strcache.h>

#include "ups.h"
#include "symtab.h"
#include "text.h"
#include "ci.h"
#include "preamble.h"
#ifdef ARCH_MIPS
#include "mips_frame.h"
#endif
#include "st_priv.h"

/*  Ultrix 4.1 has btVoid, which isn't defined under earlier releases.
 */
#ifndef btVoid
#define btVoid 26
#endif

#ifdef ST_TE
#define GET_TAB(fd, name, offset, count, type) \
			((type *)get_bytes(fd, name, offset, sizeof(type) * count))
static char *get_bytes PROTO((int fd, const char *name, off_t offset, int size));
static language_t lang_to_srctype PROTO((int te_lang));
static type_t *get_tir_type PROTO((symtab_t *st, stf_t *stf, block_t *bl,
						int auxno, const char *tag));
static void get_aux PROTO((symtab_t *st, stf_t *stf, int auxno, AUXU *p_aux));
static type_t *get_aggr PROTO((symtab_t *st, stf_t *stf, block_t *bl,
					int symno, typecode_t typecode));
static void parse_rndx PROTO((symtab_t *st, stf_t *stf, int *p_auxno,
						stf_t **p_stf, int *p_symno));
static const char *get_name PROTO((symtab_t *st, stf_t *stf, int iss));
static void push_typedef PROTO((symtab_t *st, stf_t *stf, block_t *bl,
						int symindex, const char *name));
static void get_fi_types PROTO((fil_t *fil));
static int skip_proc PROTO((symtab_t *st, stf_t *stf, int symno, int symlim));
static typecode_t guess_aggr_typecode PROTO((symtab_t *st, stf_t *stf, int symno, int symlim));

static language_t
lang_to_srctype(te_lang)
int te_lang;
{
	switch (te_lang) {
	case langC:
		return LANG_C;
	case langFortran:
		return LANG_FORTRAN;
	default:
		return LANG_UNKNOWN;
	}
}

lno_t *
get_fu_lnos(f)
func_t *f;
{
	fsyminfo_t *fs;
	symtab_t *st;
	stf_t *stf;
	int lnum;
	taddr_t addr;
	char *lbuf;
	lno_t *last_lno;

	if (f->fu_flags & FU_DONE_LNOS)
		return f->fu__lnos;

	f->fu_flags |= FU_DONE_LNOS;

	stf = (stf_t *)f->fu_fil->fi_stf;
	st = (symtab_t *)f->fu_symtab_id;
	fs = (fsyminfo_t *)f->fu_fsyminfo_id;

	/*  Don't return line number information for functions in
	 *  GLEVEL_0 (-->STF_HIDE) source files - the numbers seem
	 *  to be bogus.
	 */
	if ((stf->stf_flags & STF_HIDE) != 0)
		return NULL;

	lbuf = st->st_lnotab + stf->stf_lno_base + fs->fs_lno_base;

	lnum = fs->fs_first_lnum;
	addr = f->fu_addr;

	if (lbuf < st->st_lnotab || lbuf >= st->st_lnotab + st->st_lnotab_size)
		panic("lbuf botch in gfl");

	last_lno = NULL;
	while (lnum < fs->fs_last_lnum)	{	/* BUG: need proper test */
		int code, delta, count;
		lno_t *lno;

		static int signtab[16] = {
			 0,  1,  2,  3,  4,  5,  6,  7,
			-8, -7, -6, -5, -4, -3, -2, -1
		};

		code = *lbuf++;
		delta = signtab[(code >> 4) & 0xf]; 
		count = (code & 0xf) + 1;

		if (delta == -8) {
			delta = (*lbuf << 8) | lbuf[1];
			lbuf += 2;
		}

		/*  Add following entries with a delta of 0 to the count.
		 */
		while ((*lbuf & 0xf0) == 0)
			count += (*lbuf++ & 0xf) + 1;

		/*  Bump lnum here, as the count is the source line
		 *  delta from the last line to this.
		 */
		lnum += delta;

		lno = (lno_t *)alloc(st->st_alloc_id, sizeof(lno_t));
		lno->ln_num = lnum;
		lno->ln_addr = addr;

		if (last_lno != NULL)
			last_lno->ln_next = lno;
		else
			f->fu__lnos = lno;
		last_lno = lno;

		addr += count * 4;	/* each MIPS instruction is 4 bytes */
	}

	if (last_lno == NULL)
		f->fu__lnos = NULL;
	else
		last_lno->ln_next = NULL;

	/*  The first line number put out by the MIPS compiler seems to
	 *  always point at the opening '{' of a function definition, so
	 *  skip it (so long as we have other symbols beyond it).
	 *
	 *  One exception: if a function has no stack frame, then the
	 *  first lno seems to point at the first real source line.
	 */
	if (fs->fs_frbuf.fr_frame_size != 0 &&
				f->fu__lnos != NULL && f->fu__lnos->ln_next != NULL)
		f->fu__lnos = f->fu__lnos->ln_next;

	if (f->fu__lnos != NULL && last_lno != NULL)
		f->fu_max_lnum = last_lno->ln_num;
	return f->fu__lnos;
}

static const char *
get_name(st, stf, iss)
symtab_t *st;
stf_t *stf;
int iss;
{
	if (iss == 0)
		return NULL;
	return si_get_string(st->st_symio_id, stf->stf_strings_base + iss);
}

static void
get_fi_types(fil)
fil_t *fil;
{
	symtab_t *st;
	stf_t *stf;
	int symno, symlim;

	if (fil->fi_flags & FI_DOING_TYPES)
		panic("dup fil in gft");
	if (fil->fi_flags & FI_DONE_TYPES)
		return;
	fil->fi_flags |= FI_DOING_TYPES;

	stf = (stf_t *)fil->fi_stf;
	st = stf->stf_symtab;
	symlim = stf->stf_symlim - stf->stf_symno;

	for (symno = 1; symno < symlim; ++symno) {
		SYMR sym;

		getsym(st->st_symio_id,  stf->stf_symno + symno, &sym);
		switch (sym.st) {
		case stTypedef:
			push_typedef(st, stf, fil->fi_block, sym.index,
							get_name(st, stf, sym.iss));
			break;
		case stStatic:
			break;
		case stBlock:
			get_aggr(st, stf, fil->fi_block, symno, TY_NOTYPE);
			symno = sym.index - 1;
			break;
		case stProc:
		case stStaticProc:
			symno = skip_proc(st, stf, symno, symlim);
			break;
		case stEnd:
			if (sym.index != 0)
				panic("end sym botch in gfv");
			if (symno != symlim - 1)
				panic("symno end botch in gfv");
			break;
		default:
			panic("unknown st in gfv");
		}
	}

	fil->fi_flags &= ~FI_DOING_TYPES;
	fil->fi_flags |= FI_DONE_TYPES;
}

var_t *
get_fi_vars(fil)
fil_t *fil;
{
	symtab_t *st;
	stf_t *stf;
	var_t *varlist;
	extsym_t *es, *eslim;
	int symno, symlim;

	if (fil->fi_language != LANG_C) {
		if (fil->fi_language == LANG_FORTRAN)
			errf("\bSorry, can only show variables in C code");
		return NULL;
	}

	if (fil->fi_flags & FI_DONE_VARS)
		return fil->fi_block->bl_vars;
	
	stf = (stf_t *)fil->fi_stf;
	st = stf->stf_symtab;
	varlist = NULL;

	if (stf->stf_symlim < stf->stf_symno)
		panic("stf symno botch in gfv");
	symlim = stf->stf_symlim - stf->stf_symno;

	/*  First add any globals that belong to this file.
	 */
	eslim = st->st_exttab + st->st_exttab_size;
	for (es = st->st_exttab; es < eslim; ++es) {
		if (es->es_stf == stf) {
			type_t *type;
			var_t *v;

			type = get_tir_type(st, stf, fil->fi_block,
							es->es_symno, es->es_name);
			v = ci_make_var(st->st_alloc_id, es->es_name,
							CL_EXT, type, es->es_addr);
			v->va_next = varlist;
			varlist = v;
		}
	}
			
	for (symno = 1; symno < symlim; ++symno) {
		SYMR sym;

		getsym(st->st_symio_id,  stf->stf_symno + symno, &sym);
		switch (sym.st) {
		case stTypedef:
			/*  This has been handled by get_fi_types above.
			 */
			break;
		case stStatic:
			{
				const char *name;
				type_t *type;
				var_t *v;

				name = alloc_strdup(st->st_alloc_id,
						    get_name(st, stf, sym.iss));
				type = get_tir_type(st, stf, fil->fi_block,
								sym.index, name);
				v = ci_make_var(st->st_alloc_id, name, CL_STAT,
								type, sym.value);
				v->va_next = varlist;
				varlist = v;
			}
			break;
		case stBlock:
			if (sym.sc != scInfo)
				panic("unknown block sc in gfv");
			symno = sym.index - 1;
			break;
		case stProc:
		case stStaticProc:
			symno = skip_proc(st, stf, symno, symlim);
			break;
		case stEnd:
			if (sym.index != 0)
				panic("end sym botch in gfv");
			if (symno != symlim - 1)
				panic("symno end botch in gfv");
			break;
		default:
			panic("unknown st in gfv");
		}
	}

	get_fi_types(fil);

	fil->fi_block->bl_vars = varlist;
	fil->fi_flags |= FI_DONE_VARS;
	return varlist;
}

/*  Skip to the stEnd symbol corresponding to an stProc symbol.
 */
static int
skip_proc(st, stf, symno, symlim)
symtab_t *st;
stf_t *stf;
int symno, symlim;
{
	SYMR sym;
	int start_symno;

	start_symno = symno++;
	for (;;) {
		getsym(st->st_symio_id, stf->stf_symno + symno, &sym);
		if (sym.st == stEnd && sym.index == start_symno)
			break;
		if (sym.st == stBlock)
			symno = sym.index;
		else
			++symno;
		if (symno > symlim)
			panic("symno botch in gfb");
	}
	if (sym.sc != scText)
		panic("end sym botch in gfb");
	return symno;
}

static void
push_typedef(st, stf, bl, symindex, name)
symtab_t *st;
stf_t *stf;
block_t *bl;
int symindex;
const char *name;
{
	typedef_t *td;
	type_t *type;

	type = get_tir_type(st, stf, bl, symindex, name);

	td = (typedef_t *)alloc(st->st_alloc_id, sizeof(typedef_t));
	td->td_name = alloc_strdup(st->st_alloc_id, name);
	td->td_type = type;
	td->td_lexinfo = NULL;
	type->ty_typedef = td;

	td->td_next = bl->bl_typedefs;
	bl->bl_typedefs = td;
}

block_t *
get_fu_blocks(f)
func_t *f;
{
	int symno, symlim;
	bool first_block;
	symtab_t *st;
	stf_t *stf;
	fsyminfo_t *fs;
	alloc_id_t alloc_id;
	block_t *bl;

	if (f->fu_language != LANG_C) {
		if (f->fu_language == LANG_FORTRAN)
			errf("\bSorry, can only show variables in C code");
		return NULL;
	}

	if (f->fu_flags & FU_DONE_BLOCKS)
		return f->fu__blocks;

	st = (symtab_t *)f->fu_symtab_id;
	alloc_id = st->st_alloc_id;
	stf = (stf_t *)f->fu_fil->fi_stf;
	fs = (fsyminfo_t *)f->fu_fsyminfo_id;

	bl = ci_make_block(alloc_id, f->fu_fil->fi_block);
	bl->bl_start_lnum = fs->fs_first_lnum;
	bl->bl_end_lnum = fs->fs_last_lnum;

	f->fu__blocks = bl;

	first_block = TRUE;
	symlim = fs->fs_symlim;
	for (symno = fs->fs_symno + 1; symno < symlim; ++symno) {
		var_t *v;
		taddr_t addr;
		int stype;
		const char *name;
		class_t class;
		type_t *type;
		SYMR sym;

		getsym(st->st_symio_id, stf->stf_symno + symno, &sym);
		name = get_name(st, stf, sym.iss);
		addr = sym.value;

		stype = sym.st;
		switch (stype) {
		case stLabel:
			/*  We are not interested in goto targets.
			 */
			break;
		case stFile:
		case stGlobal:
			panic("unexpected sym type in gfb");
			break;
		case stTypedef:
			push_typedef(st, stf, bl, sym.index, name);
			break;
		case stStatic:
			type = get_tir_type(st, stf, bl, sym.index, name);
			name = alloc_strdup(alloc_id, name);
			v = ci_make_var(alloc_id, name, CL_LSTAT, type, addr);
			v->va_next = bl->bl_vars;
			bl->bl_vars = v;
			break;
		case stParam:
		case stLocal:
			switch (sym.sc) {
			case scRegister:
				class = CL_REG;
				break;
			case scVar:
				class = CL_REF;
				break;
			case scAbs:
				class = (stype == stParam) ? CL_AUTO : CL_ARG;
				break;
			default:
				panic("bad st for param/local in gfb");
				class = CL_NOCLASS;	/* to stisfy gcc */
				break;
			}
			type = get_tir_type(st, stf, bl, sym.index, name);
			name = alloc_strdup(alloc_id, name);
			v = ci_make_var(alloc_id, name, class, type, addr);
			v->va_next = bl->bl_vars;
			bl->bl_vars = v;
			break;
		case stBlock:
			if (sym.sc == scInfo) {
				symno = sym.index - 1;
				break;
			}
			if (sym.sc != scText)
				panic("unknown block sym in gfb");
			
			bl = ci_make_block(alloc_id, bl);
			bl->bl_start_lnum = addr_to_lnum(f, f->fu_addr + addr);
			bl->bl_end_lnum = sym.index - 1;	/* see stEnd */

			if (first_block) {
				bl->bl_vars = bl->bl_parent->bl_vars;
				bl->bl_parent->bl_vars = NULL;
				first_block = FALSE;
			}
			break;
		case stEnd:
			if (sym.sc == scInfo)
				break;
			if (sym.sc != scText)
				panic("unknown block end sym in gfb");
			if (name != NULL) {
				/*  If this stEnd matches the stProc that
				 *  starts the function, force an edit from
				 *  the loop.  We do this because symlim
				 *  may be too high (wrongly including 
				 *  globals declared between this function
				 *  and the next.
				 */
				if (sym.index == fs->fs_symno)
					symno = symlim - 1;
				break;
			}
			if (symno != bl->bl_end_lnum)
				panic("block mismatch");
			bl->bl_end_lnum = addr_to_lnum(f, f->fu_addr + addr);
			bl->bl_next = bl->bl_parent->bl_blocks;
			bl->bl_parent->bl_blocks = bl;
			bl = bl->bl_parent;
			break;
		default:
			panic("unknown st in gfv");
		}
	}

	if (bl != f->fu__blocks || bl->bl_next != NULL)
		panic("outer block botch in gfb");
	if (bl->bl_blocks != NULL) {
		if (bl->bl_vars != NULL)
			panic("outer block vars botch in gfb");
		if (bl->bl_blocks->bl_next != NULL)
			panic("multiple block botch in gfb");
		f->fu__blocks = bl->bl_blocks;
	}
	
	/*  We want to use typedef names if possible for structures and
	 *  enums (see ci_basetype_name), so we need any type information
	 *  in the file.  Thus we load the file information.
	 */
	get_fi_vars(f->fu_fil);

	f->fu_flags |= FU_DONE_BLOCKS;
	return f->fu__blocks;
}

static void
get_aux(st, stf, auxno, p_aux)
symtab_t *st;
stf_t *stf;
int auxno;
AUXU *p_aux;
{
	off_t fpos;
	char *bytes;
	long len;

	fpos = (stf->stf_aux_base + auxno) * sizeof(AUXU);
	bytes = sc_get_bytes(st->st_aux_strcache, fpos, sizeof(AUXU), &len);
	if (len != sizeof(AUXU))
		panic("aux botch in gtt");
	*p_aux = *(AUXU *)bytes;
}

static typecode_t
guess_aggr_typecode(st, stf, symno, symlim)
symtab_t *st;
stf_t *stf;
int symno, symlim;
{
	int orig_symno, expected_index;

	orig_symno = symno;
	expected_index = 0;

	for (; symno < symlim; ++symno) {
		SYMR sym;

		getsym(st->st_symio_id, stf->stf_symno + symno, &sym);

		if (sym.st != stMember)
			return TY_STRUCT;

		if (expected_index == 0)
			expected_index = sym.index;
		else {
			++expected_index;
			if (sym.index != expected_index)
				return TY_STRUCT;
		}
	}

	/*  The sym.index fields for enums seem to always be greater than
	 *  than the symno of the start of the enum.
	 */
	if (symno == orig_symno + 1 && expected_index < orig_symno)
		return TY_STRUCT;

	return TY_ENUM;
}

static type_t *
get_aggr(st, stf, bl, symno, typecode)
symtab_t *st;
stf_t *stf;
block_t *bl;
int symno;
typecode_t typecode;
{
	alloc_id_t alloc_id;
	aggr_or_enum_def_t *ae;
	const char *name;
	class_t class;
	SYMR sym;
	symio_id_t symio_id;
	int start_symno, symlim;
	bool unsure_about_typecode, all_addrs_zero;
	var_t *v;
	enum_member_t *em;
	aggrlist_t *al;

	for (al = stf->stf_aggrlist; al != NULL; al = al->al_next) {
		if (al->al_symno == symno)
			return al->al_type;
	}

	alloc_id = st->st_alloc_id;
	symio_id = st->st_symio_id;

	getsym(st->st_symio_id, stf->stf_symno + symno, &sym);

	if (sym.sc != scInfo)
		panic("aggr botch in ga");
	if (sym.st == stTypedef) {
		name = get_name(st, stf, sym.iss);
		al = (aggrlist_t *)alloc(alloc_id, sizeof(aggrlist_t));
		al->al_symno = symno;
		al->al_type = get_tir_type(st, stf, bl, sym.index, name);
		al->al_next = stf->stf_aggrlist;
		stf->stf_aggrlist = al;
		return al->al_type;
	}
	if (sym.st != stBlock)
		panic("aggr botch in ga");

	start_symno = symno;
	symlim = sym.index - 1;

	name = get_name(st, stf, sym.iss);

	/*  The MIPS compiler invents names like ".F34" for unnamed objects.
	 */
	if (name != NULL && *name == '.')
		name = NULL;

	if (name != NULL)
		name = alloc_strdup(alloc_id, name);
	
	/*  The <censored> DECstation compiler ties the typecode (struct/union/enum)
	 *  information to variables of the type.  When we are scanning a file
	 *  for types (in get_fi_types) we don't have that information.
	 *
	 *  We guess (and we can guess wrong).
	 */
	unsure_about_typecode = typecode == TY_NOTYPE;
	if (unsure_about_typecode) {
		if (sym.value != 4)
			typecode = TY_STRUCT;	/* fix later if TY_UNION */
		else
			typecode = guess_aggr_typecode(st, stf, symno + 1, symlim);
	}

	ae = ci_make_aggr_or_enum_def(alloc_id, name, typecode, (type_t *)NULL);
	ae->ae_is_complete = AE_COMPLETE;

	ae->ae_next = bl->bl_aggr_or_enum_defs;
	bl->bl_aggr_or_enum_defs = ae;

	al = (aggrlist_t *)alloc(alloc_id, sizeof(aggrlist_t));
	al->al_symno = symno;
	al->al_type = ae->ae_type;
	al->al_next = stf->stf_aggrlist;
	stf->stf_aggrlist = al;

	switch (typecode) {
	case TY_STRUCT:
	case TY_UNION:
		class = (typecode == TY_STRUCT) ? CL_MOS : CL_MOU;
		ae->ae_aggr_members = NULL;
		ae->ae_size = sym.value;
		break;
	case TY_ENUM:
		class = CL_MOE;
		ae->ae_enum_members = NULL;
		ae->ae_size = sizeof(class_t);	/* or any enum var */
		break;
	default:
		panic("unknown typecode in ga");
		class = CL_NOCLASS;	/* to satisfy gcc */
		break;
	}

	v = NULL;
	all_addrs_zero = TRUE;
	for (++symno; symno < symlim; ++symno) {
		getsym(st->st_symio_id, stf->stf_symno + symno, &sym);
		if (sym.sc != scInfo)
			panic("member sc botch in ga");

		switch (sym.st) {
		case stTypedef:
			break;
		case stBlock:
			symno = sym.index - 1;
			continue;
		case stMember:
			name = alloc_strdup(alloc_id, get_name(st, stf, sym.iss));
			if (typecode == TY_ENUM) {
				em = ci_make_enum_member(alloc_id, name, sym.value);
				em->em_next = ae->ae_enum_members;
				em->em_enum = ae;
				ae->ae_enum_members = em;
			}
			else {
				type_t *type;
				taddr_t addr;

				type = get_tir_type(st, stf, bl, sym.index, name);

				if (type->ty_code == TY_BITFIELD) {
					addr = (sym.value / 32) * 4;
					type->ty_bitfield->bf_offset =
								sym.value % 32;
				}
				else {
					addr = sym.value / 8;
					if (addr != 0)
						all_addrs_zero = FALSE;
				}

				v = ci_make_var(alloc_id, name, class, type, addr);
				v->va_next = ae->ae_aggr_members;
				ae->ae_aggr_members = v;
			}
			break;
		default:
			panic("member st botch in ga");
		}
	}

	getsym(st->st_symio_id, stf->stf_symno + symno, &sym);
	if (sym.st != stEnd || sym.sc != scInfo || sym.index != start_symno)
		panic("end aggr botch in ga");
	
	/*  If we tentatively think that this is a struct, and there
	 *  are multiple elements all with offset zero, then it's a union.
	 */
	if (unsure_about_typecode && typecode == TY_STRUCT && all_addrs_zero
						&& v != NULL && v->va_next != NULL)
		ae->ae_type->ty_code = TY_UNION;

	return ae->ae_type;
}

static void
parse_rndx(st, stf, p_auxno, p_stf, p_symno)
symtab_t *st;
stf_t *stf;
int *p_auxno;
stf_t **p_stf;
int *p_symno;
{
	int auxno, rfd, symno;
	stf_t *nstf;
	AUXU aux;

	auxno = *p_auxno;

	get_aux(st, stf, auxno++, &aux);
	rfd = aux.rndx.rfd;
	symno = aux.rndx.index;

	if (rfd == ST_RFDESCAPE) {
		get_aux(st, stf, auxno++, &aux);
		rfd = aux.isym;
	}

	if (rfd == -1)
		nstf = NULL;
	else {
		int fnum;

		if (rfd < 0 || rfd >= stf->stf_rfdtab_size)
			panic("rfd out of range in gtt");
		fnum = stf->stf_rfdtab[rfd];

		if (fnum < 0 || fnum >= st->st_stftab_size)
			panic("fnum out of range in gtt");
		nstf = st->st_stftab[fnum];

		if (symno < 0 || symno >= nstf->stf_symlim)
			panic("symno out of range in gtt");
	}
	
	*p_auxno = auxno;
	*p_stf = nstf;
	*p_symno = symno;
}

static type_t *
get_tir_type(st, stf, bl, auxno, tag)
symtab_t *st;
stf_t *stf;
block_t *bl;
int auxno;
const char *tag;
{
	AUXU aux;
	TIR tir;
	alloc_id_t alloc_id;
	int i;
	typecode_t typecode;
	type_t *type;

	if (auxno == ST_ANONINDEX)
		return ci_code_to_type(TY_INT_ASSUMED);

	get_aux(st, stf, auxno++, &aux);
	tir = aux.ti;

	switch (tir.bt) {
	case btNil:	typecode = TY_VOID;	break;
	case btChar:	typecode = TY_CHAR;	break;
	case btUChar:	typecode = TY_UCHAR;	break;
	case btShort:	typecode = TY_SHORT;	break;
	case btUShort:	typecode = TY_USHORT;	break;
	case btInt:	typecode = TY_INT;	break;
	case btUInt:	typecode = TY_UINT;	break;
	case btLong:	typecode = TY_LONG;	break;
	case btULong:	typecode = TY_ULONG;	break;
	case btFloat:	typecode = TY_FLOAT;	break;
	case btDouble:	typecode = TY_DOUBLE;	break;
	case btStruct:	typecode = TY_STRUCT;	break;
	case btUnion:	typecode = TY_UNION;	break;
	case btEnum:	typecode = TY_ENUM;	break;

	case btAdr:	typecode = TY_ULONG;	break;
	case btVoid:	typecode = TY_VOID;	break;
	  
	default:
		panic("unknown typecode in gtt");
		typecode = TY_NOTYPE;	/* to satisfy gcc */
		break;
	}

	alloc_id = st->st_alloc_id;

	if (typecode == TY_ENUM || typecode == TY_STRUCT || typecode == TY_UNION) {
		stf_t *nstf;
		int symno;

		parse_rndx(st, stf, &auxno, &nstf, &symno);
		if (nstf != NULL) {
			if (nstf != stf &&
			    (nstf->stf_fil->fi_flags &
					(FI_DONE_TYPES | FI_DOING_TYPES)) == 0) {
				get_fi_types(nstf->stf_fil);
				push_typedefs_and_aggrs(alloc_id,
							stf->stf_fil->fi_block,
							nstf->stf_fil->fi_block);
			}
			type = get_aggr(st, nstf, bl, symno, typecode);
		}
		else {
			typecode_t utypecode;

			switch (typecode) {
				case TY_ENUM:	utypecode = TY_U_ENUM;	break;
				case TY_STRUCT:	utypecode = TY_U_STRUCT;break;
				case TY_UNION:	utypecode = TY_U_UNION;	break;
				default:	utypecode = TY_NOTYPE;	break;
			}
			type = ci_make_undef_type(alloc_id, tag, utypecode,
								(type_t *)NULL);
		}
	}
	else if (tir.fBitfield) {
		get_aux(st, stf, auxno++, &aux);
		type = ci_make_bitfield_type(alloc_id, typecode, 0, aux.isym % 32);
	}
	else
		type = ci_code_to_type(typecode);

	for (i = 0; i < itqMax; ++i) {
		type_t *dtype;
		int deriv;

		switch (i) {
		case 0:		deriv = tir.tq0;	break;
		case 1:		deriv = tir.tq1;	break;
		case 2:		deriv = tir.tq2;	break;
		case 3:		deriv = tir.tq3;	break;
		case 4:		deriv = tir.tq4;	break;
		case 5:		deriv = tir.tq5;	break;
		default:
			panic("bad tq# in gtt");
			deriv = 0;	/* to satisfy gcc */
			break;
		}

		switch (deriv & 0xf) {
		case tqNil:
			dtype = NULL;
			break;
		case tqVol:
			/*  We ignore volatile for the moment.
			 */
			continue;
		case tqPtr:
			dtype = ci_make_type(alloc_id, DT_PTR_TO);
			dtype->ty_qualifiers = 0;
			break;
		case tqProc:
			dtype = ci_make_type(alloc_id, DT_FUNC_RETURNING);
			break;
		case tqArray:
			{
				dim_t *dim;
				int low, high;

				get_aux(st, stf, auxno + 2, &aux);
				low = aux.dnLow;
				get_aux(st, stf, auxno + 3, &aux);
				high = aux.dnHigh + 1;
				auxno += 5;

				dim = (dim_t *)alloc(alloc_id, sizeof(dim_t));
				dim->di_low = low;
				dim->di_ldynamic = FALSE;
				dim->di_high = high;
				dim->di_hdynamic = FALSE;
				dim->di_high_expr_id = NULL;
				dim->di_type = ci_code_to_type(TY_INT);
				
				dtype = ci_make_type(alloc_id, DT_ARRAY_OF);
				dtype->ty_dim = dim;
			}
			break;
		default:
			panic("unknown deriv in gtt");
			dtype = NULL;	/* to satisfy gcc */
			break;
		}
		if (dtype == NULL)
			break;
		dtype->ty_base = type;
		type = dtype;
	}

	if (tir.continued)
		 panic("cont tir NYI in gtt");
	
	return type;
}

int
skim_te_symtab(st, fd, syms_offset, addr_to_fpos_offset, first_addr, last_addr,
	       rootblock, p_have_common_blocks, p_sfiles, p_functab_id, p_cblist_id)
symtab_t *st;
int fd;
off_t syms_offset, addr_to_fpos_offset;
taddr_t first_addr, last_addr;
block_t *rootblock;
bool *p_have_common_blocks;
fil_t **p_sfiles;
functab_id_t *p_functab_id;
cblist_id_t *p_cblist_id;
{
	fil_t *sfiles;
	func_t *flist, *f;
	funclist_t *fl;
	FDR *fdrs, *fdr, *fdrlim;
	RFDT *rfdtab;
	int rfdtab_size;
	HDRR *sh;
	SYMR sym;
	EXTR *extrs, *extlim, *er;
	PDR *pdrs, *pdrlim;
	extsym_t *es;
	alloc_id_t alloc_id;
	symio_id_t symio_id;
	int flist_len, exttab_size;
	char *estrings;

	if ((sh = GET_TAB(fd, st->st_name, syms_offset, 1, HDRR)) == NULL)
		return -1;

	fdrs = GET_TAB(fd, st->st_name, sh->cbFdOffset, sh->ifdMax, FDR);
	pdrs = GET_TAB(fd, st->st_name, sh->cbPdOffset, sh->ipdMax, PDR);

	extrs = GET_TAB(fd, st->st_name, sh->cbExtOffset, sh->iextMax, EXTR);
	extlim = extrs + sh->iextMax;
	estrings = get_bytes(fd, st->st_name, sh->cbSsExtOffset, sh->issExtMax);

	st->st_lnotab = get_bytes(fd, st->st_name, sh->cbLineOffset, sh->cbLine);
	st->st_lnotab_size = sh->cbLine;
	
	rfdtab = GET_TAB(fd, st->st_name, sh->cbRfdOffset, sh->crfd, RFDT);
	rfdtab_size = sh->crfd;

	if (fdrs == NULL || pdrs == NULL || st->st_lnotab == NULL ||
	    rfdtab == NULL || extrs == NULL || estrings == NULL) {
		free((char *)sh);
		if (fdrs != NULL)
			free((char *)fdrs);
		if (pdrs != NULL)
			free((char *)pdrs);
		if (st->st_lnotab != NULL)
			free((char *)st->st_lnotab);
		if (rfdtab != NULL)
			free((char *)rfdtab);
		if (extrs != NULL)	
			free((char *)extrs);
		if (estrings != NULL)
			free(estrings);
		return -1;
	}

	alloc_id = st->st_alloc_id;
	symio_id = make_symio(st->st_name, fd, sh->cbSymOffset, sh->isymMax,
				     sh->cbSsOffset, addr_to_fpos_offset,
				     alloc_id);
	st->st_symio_id = symio_id;

	st->st_aux_strcache = sc_make_fd_strcache(fd);
	sc_set_offset(st->st_aux_strcache, sh->cbAuxOffset);

	st->st_stftab = (stf_t **)alloc(alloc_id, sh->ifdMax * sizeof(stf_t *));
	st->st_stftab_size = sh->ifdMax;

	flist_len = 0;
	flist = NULL;
	fdrlim = fdrs + sh->ifdMax;
	sfiles = NULL;

	for (fdr = fdrs; fdr < fdrlim; ++fdr) {
		const char *name;
		stf_t *stf;
		funclist_t *nextfl;
		PDR *pdr, *nextpdr;

		getsym(symio_id, fdr->isymBase, &sym);
		name = si_get_string(symio_id, fdr->issBase + sym.iss);
		stf = make_stf(name, st, fdr->isymBase,
						lang_to_srctype(fdr->lang), 0);

		if (fdr->rfdBase < 0 || fdr->crfd < 0 ||
					fdr->rfdBase + fdr->crfd > rfdtab_size)
			panic("fdr rfd botch in sts");

		if (fdr->glevel == GLEVEL_0 || fdr->cpd == 0)
			stf->stf_flags |= STF_HIDE;
		stf->stf_aggrlist = NULL;
		stf->stf_rfdtab = rfdtab + fdr->rfdBase;
		stf->stf_rfdtab_size = fdr->crfd;
		stf->stf_strings_base = fdr->issBase;
		stf->stf_aux_base = fdr->iauxBase;
		stf->stf_symlim = fdr->isymBase + fdr->csym;
		stf->stf_lno_base = fdr->cbLineOffset;
		stf->stf_lno_lim = fdr->cbLineOffset + fdr->cbLine;

		stf->stf_fil = make_fil(stf, rootblock, (char *)NULL, sfiles);
		sfiles = stf->stf_fil;

		st->st_stftab[fdr - fdrs] = stf;

		pdr = pdrs + fdr->ipdFirst;
		pdrlim = pdr + fdr->cpd;

		/*  Skip pdrs with isym==-1 - see comment in loop below.
		 */
		while (pdr < pdrlim && pdr->isym == -1)
			++pdr;

		sfiles->fi_funclist_tail = NULL;
		for (; pdr < pdrlim; pdr = nextpdr) {
			int symno;
			preamble_t *pr;
			fsyminfo_t *fs;

			/*  In Ultrix 4.1 there are a few pdrs with isym
			 *  set to -1 helpfully scattered about.  Skip
			 *  them.  Thanks to rcb@shaman.cc.ncsu.edu for
			 *  finding this.
			 */
			nextpdr = pdr + 1;
			while (nextpdr < pdrlim && nextpdr->isym == -1)
				++nextpdr;

			fl = (funclist_t *)alloc(alloc_id, sizeof(funclist_t));

			symno = pdr->isym;

			getsym(symio_id, stf->stf_symno + symno, &sym);

			name = si_get_string(symio_id, fdr->issBase + sym.iss);
			flist = ci_make_func(alloc_id, name, sym.value,
					    (symtab_id_t)st, sfiles, flist, fl);
			if (sym.st == stStaticProc)
				flist->fu_flags |= FU_STATIC;

			fs = make_fsyminfo(alloc_id, symno);
			fs->fs_symlim = (nextpdr < pdrlim) ? nextpdr->isym
					 		   : fdr->csym;
			++flist_len;

			pr = (preamble_t *)alloc(alloc_id, sizeof(preamble_t));
			pr->pr_bpt_offset = 0;
			pr->pr_rsave_mask = pdr->regmask;
			pr->pr_rsave_offset = pdr->regoffset;
			pr->pr_fpreg_rsave_mask = pdr->fregmask;
			pr->pr_fpreg_rsave_offset = pdr->fregoffset;

			fs->fs_lno_base = pdr->cbLineOffset;
			fs->fs_first_lnum = pdr->lnLow;
			fs->fs_last_lnum = pdr->lnHigh;

			fs->fs_frbuf.fr_frame_size = pdr->frameoffset;
			fs->fs_frbuf.fr_reg_offset = pdr->regoffset;
			fs->fs_frbuf.fr_reg_mask = pdr->regmask;
			fs->fs_frbuf.fr_pcreg = pdr->pcreg;
			fs->fs_frbuf.fr_spreg = pdr->framereg;

			flist->fu_fsyminfo_id = (fsyminfo_id_t)fs;
			flist->fu_preamble_id = (preamble_id_t)pr;

			fl->fl_func = flist;
			fl->fl_prev = sfiles->fi_funclist_tail;
			sfiles->fi_funclist_tail = fl;

			f = flist;
		}

		/*  Build the forwards links in the function list.
		 *  See the comment on the flist_t structure in symtab.h.
		 */
		nextfl = NULL;
		for (fl = sfiles->fi_funclist_tail; fl != NULL; fl = fl->fl_prev) {
			fl->fl_next = nextfl;
			nextfl = fl;
		}
		sfiles->fi_funclist_head = nextfl;
	}

	exttab_size = 0;
	for (er = extrs; er < extlim; ++er) {
		if (er->asym.st == stGlobal)
			++exttab_size;
	}

	st->st_exttab = (extsym_t *)e_malloc(exttab_size * sizeof(extsym_t));
	st->st_exttab_size = exttab_size;

	es = st->st_exttab;
	for (er = extrs; er < extlim; ++er) {
		switch (er->asym.st) {
		case stGlobal:
			if (es - st->st_exttab >= exttab_size)
				panic("ext count botch in sts");
			if (er->asym.iss < 0 || er->asym.iss >= sh->issExtMax)
				panic("estring botch in sts");
			es->es_name = estrings + er->asym.iss;
			es->es_addr = er->asym.value;
			es->es_stf = st->st_stftab[er->ifd];
			es->es_symno = er->asym.index;
			++es;
			break;
		case stLabel:
		case stProc:
			break;
		default:
			panic("ext sym st botch in sts");
		}
	}
	if (es - st->st_exttab != exttab_size)
		panic("esym count botch in sts");

	free((char *)sh);
	free((char *)fdrs);
	free((char *)pdrs);

	*p_have_common_blocks = FALSE;
	*p_sfiles = sfiles;
	*p_functab_id = make_functab(st, flist, flist_len, first_addr, last_addr);
	*p_cblist_id = NULL;

	return 0;
}

int
get_frame_size(addr)
taddr_t addr;
{
	func_t *f;

	if ((f = addr_to_func(addr)) == NULL)
		return 0;
	return ((fsyminfo_t *)f->fu_fsyminfo_id)->fs_frbuf.fr_frame_size;
}

frame_t *
get_frame_info(f)
func_t *f;
{
	return &((fsyminfo_t *)f->fu_fsyminfo_id)->fs_frbuf;
}

static char *
get_bytes(fd, name, offset, size)
int fd;
const char *name;
off_t offset;
int size;
{
	char *buf;
	int n_read;

	if ((buf = malloc(size)) == NULL) {
		errf("Can't malloc %d bytes (%s)", size);
		return NULL;
	}
							
	if (lseek(fd, offset, L_SET) == -1) {
		errf("Lseek to %l in %s failed (%s)", offset, name);
		free(buf);
		return NULL;
	}
	if ((n_read = read(fd, buf, size)) != size) {
		if (n_read == 0)
			errf("Unexpected EOF in %s", name);
		else
			errf("Error reading %s (%s)", name);
		free(buf);
		return NULL;
	}
	return buf;
}
#endif /* ST_TE */
