/* st_parse.c - code to parse a type string */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_st_parse_c_sccsid[] = "@(#)st_parse.c	1.32 15/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <sys/types.h>
#include <ctype.h>
#include <string.h>
#include <a.out.h>
#ifndef OS_RISCOS
#include <stab.h>
#endif

#include <local/ukcprog.h>
#include <mtrprog/strcache.h>

#include "ups.h"
#include "symtab.h"
#include "ci.h"
#ifdef ARCH_MIPS
#include "mips_frame.h"
#endif
#include "st_priv.h"

/*  This file parses the symbol table grammar given in the 4.3bsd manual
 *  page dbx(5).
 *
 *  The routine names are the same as the non terminal names in the grammer.
 *  Above each routine is a description of the part of the grammar that
 *  the routine parses.  "Comments" in these grammar fragments are
 *  bracketed with '{' and '}'.
 *
 *  We don't parse the whole grammar - constructs that we don't parse
 *  are marked with the comment NI (not implemented).  We deviate from
 *  the grammar in some cases - these are marked with DEV.
 *  Partially implemented constructs are marked with PI.
 *
 *  The Sun C compiler generates type numbers (given as INTEGER in the grammar)
 *  of the form " '(' Filenum ',' Typenum ')' ".
 */


/*  Function prototypes.
 */

#ifndef ST_TE
static void notimp PROTO((const char *s));
static int parse_signed_num PROTO((const char **p_s));
static type_t *TypeDef PROTO((stf_t *stf, int *p_symno, const char **p_s,
							int tnum, bool eval));
static aggr_or_enum_def_t *EnumList PROTO((symtab_t *st, int *p_symno,
						const char **p_s, type_t *type));
static void Enum PROTO((symtab_t *st, const char **p_s, aggr_or_enum_def_t *ae));
static aggr_or_enum_def_t *Record PROTO((stf_t *stf, int *p_symno, const char **p_s,
					 bool is_struct, bool eval, type_t *type));
static var_t *Field PROTO((stf_t *stf, int *p_symno, const char **p_s, int is_struct,
							var_t *next, bool eval));
static dim_t *Subrange PROTO((stf_t *stf, int *p_symno, const char **p_s,
					bool eval, bool want_subrange_type));

static void get_basic_type PROTO((language_t language, symtab_t *st,
					int symno, dim_t *dim, type_t *type));

/*  The address of this variable is used as a special value for ty_typedef
 *  to signal that it is a basic type not a user defined typedef.
 */
static typedef_t Is_basic_type;

/*  If **p_s is ch, move *p_s on, otherwise abort.  Used for checking
 *  and swallowing required terminals in the grammar.
 */
void
scheck(p_s, ch)
const char **p_s;
int ch;
{
	if (**p_s != ch)	
		panic("syntax error in symtab");
	++*p_s;
}

/*  Oops - we encountered a construct which we haven't implemented in the
 *  grammar.
 */
static void
notimp(s)
const char *s;
{
	char *mesg;

	mesg = strf("%s not implemented", s);
	panic(mesg);
}

/*  Class:
 *	'c' = Constant ';'	{ NI }
 *	Variable
 *	Procedure		{ PI }
 *	Parameter
 *	NamedType
 *	'X' ExportInfo		{ NI }
 *
 *   All the ones that we implement resolve to a type letter giving the
 *   class of the variable followed by a TypeId.
 */
type_t *
Class(stf, p_symno, p_s, p_class)
stf_t *stf;
int *p_symno;
const char **p_s;
class_t *p_class;
{
	const char *s;
	class_t class;
	type_t *rtype;

	s = *p_s;
	switch(*s) {

	/* 'c' '=' Constant */
	case 'c':
		notimp("Constants");
		class = CL_NOCLASS;	/* to satisfy gcc */
		break;
	
	/* Variable */
	case '(':
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		class = CL_AUTO;
		--s;
		break;
	case 'r':
		class = CL_REG;
		break;
	case 'S':
		class = CL_STAT;
		break;
	case 'V':
		class = CL_LSTAT;
		break;
	case 'G':
		class = CL_EXT;
		break;
	
	/*  Procedure
	 *
	 *  DEV: 'P' should be in this group according to the grammar
	 *       see the comment later in this function.
	 */
	case 'Q':
	case 'I':
		notimp("Q/I functions");
		class = CL_NOCLASS;	/* to satisfy gcc */
		break;
	case 'F':
		class = CL_FUNC;
		break;
	case 'f':
		class = CL_LFUNC;
		break;
	case 'J':
		notimp("Internal functions");
		class = CL_NOCLASS;	/* to satisfy gcc */
		break;
	
	/* Parameter
	 *
	 *  DEV: the grammar says 'P' introduces a global procedure,
	 *       but gcc uses it for parameters, so we just silently
	 *       take it to mean a parameter.
	 */
	case 'P':
	case 'p':
		class = CL_ARG;
		break;
	case 'v':
		class = CL_REF;
		break;
	
	/* Namedtype */
	case 't':
		class = CL_TYPEDEF;
		break;
	case 'T':
		/*  Gcc now emits 'Tt' for structure definitions with
		 *  no tag of the form `typedef struct { ... } foo_t'.
		 */
		if (s[1] == 't') {
			++s;
			class = CL_TYPEDEF;
			break;
		}

		class = CL_TAGNAME;
		break;
	
	/* 'X' ExportInfo */
	case 'X':
		/*  This has been seen in a SunOS 4.0 f77 a.out file.
		 *  I don't think we need to worry about it, so just
		 *  ignore the symbol (setting class to NOCLASS
		 *  indicates to our caller that this symbol is junk).
		 */
		class = CL_NOCLASS; 
		break;
	
	default:
		panic("unknown type class letter in symbol table");
		class = CL_NOCLASS; /* to satisfy gcc */
	}
	
	s++;
	rtype = TypeId(stf, p_symno, &s, TRUE);
	*p_s = s;

	/*  The basic types (int, char etc) get entries in the symbol
	 *  table that look like user typedefs.  We don't want to
	 *  clutter up the typedef lists with these entries.
	 */
	if (class == CL_TYPEDEF && rtype->ty_typedef == NULL)
		*p_class = CL_NOCLASS;
	else
		*p_class = class;

	return rtype;
}

/*  Parse a number (INTEGER in the grammar).  No sign allowed.
 */
int
parse_num(p_s)
const char **p_s;
{
	const char *s;
	int res;

	res = 0;
	s = *p_s;
	if (!isdigit(*s))
		panic("bad number in parse_num");
	while (*s != '\0' && isdigit(*s))
		res = res * 10 + *s++ - '0';
	*p_s = s;
	return res;
}

/*  Parse a possibly signed number.
 */
static int
parse_signed_num(p_s)
const char **p_s;
{
	const char *s;
	int neg, num;

	s = *p_s;
	if (neg = *s == '-')
		s++;
	num = parse_num(&s);
	*p_s = s;
	return neg ? -num : num;
}

/*  Parse a name (NAME in the grammar).
 *  If save is non zero, return a pointer to a saved copy of the name,
 *  otherwise return NULL.  Names are saved via an alloc() on alloc_id.
 */
const char *
parse_name(p_s, alloc_id)
const char **p_s;
alloc_id_t alloc_id;
{
	const char *s;
	char *name;
	int len;

	s = *p_s;
	if (!isalpha(*s) && *s != '_' && *s != '$')
		panic("bad name in parse_name");
	while (isalnum(*s) || *s == '_' || *s == '$')
		s++;
	if (alloc_id != 0) {
		len = s - *p_s;
		name = alloc(alloc_id, len + 1);
		(void) strncpy(name, *p_s, len);
		name[len] = '\0';
	}
	else
		name = NULL;
	*p_s = s;
	return name;
}

/* Typenum:
 *	'(' INTEGER ':' INTEGER ')'	{ DEV, Sun C symbol tables }
 *	INTEGER				{ f77 and all VAX symbol tables }
 */
void
Typenum(p_s, p_fnum, p_tnum)
const char **p_s;
int *p_fnum, *p_tnum;
{
	const char *s;

	s = *p_s;
	if (*s == '(') {
		++s;
		*p_fnum = parse_num(&s);
		scheck(&s, ',');
		*p_tnum = parse_num(&s);
		scheck(&s, ')');
	}
	else {
		*p_fnum = 0;
		*p_tnum = parse_num(&s);
	}
	*p_s = s;
}

static void
get_basic_type(language, st, symno, dim, type)
language_t language;
symtab_t *st;
int symno;
dim_t *dim;
type_t *type;
{
	typedef struct {
		const char *a_name;
		const char *a_alias;
	} alias_t;
	typedef struct {
		long ct_low;
		long ct_high;
		typecode_t ct_code;
		int ct_size;
	} c_typetab_t;
	typedef struct {
		const char *ft_name;
		typecode_t ft_code;
		int ft_size;
	} f77_typetab_t;
		
	static c_typetab_t c_typetab[] = {
		-2147483648,	2147483647,	TY_INT,		sizeof(long),
		-128,		127,		TY_CHAR,	sizeof(char),
		0,		127,		TY_CHAR,	sizeof(char),
		-32768,		32767,		TY_SHORT,	sizeof(short),
		0,		255,		TY_UCHAR,	sizeof(char),
		0,		65535,		TY_USHORT,	sizeof(short),
		0,		-1,		TY_UINT,	sizeof(long),
		4,		0,		TY_FLOAT,	sizeof(float),
		8,		0,		TY_DOUBLE,	sizeof(double),
	};
#define N_C_TYPES	(sizeof c_typetab / sizeof *c_typetab)
	static f77_typetab_t f77_typetab[] = {
		"integer*2",		TY_INTEGER_2,	sizeof(short),
		"integer*4",		TY_INTEGER_4,	sizeof(int),
		"real",			TY_REAL,	sizeof(float),
		"double precision",	TY_DBLPRES,	sizeof(double),
		"complex",		TY_COMPLEX,	2 * sizeof(float),
		"double complex",	TY_DBLCOMP,	2 * sizeof(double),

		"logical",		TY_LOGICAL,	sizeof(int),

		"logical*1",		TY_LOGICAL,	sizeof(char),
		"logical*2",		TY_LOGICAL,	sizeof(short),
		"logical*4",		TY_LOGICAL,	sizeof(int),

		"char",			TY_CHARACTER,	sizeof(char),
		"void",			TY_FVOID,	0,
	};
#define N_F77_TYPES	(sizeof f77_typetab / sizeof *f77_typetab)

	static alias_t aliastab[] = {
		"long int",		"long",
		"unsigned int",		"unsigned",
		"long unsigned int",	"unsigned long",
		"short int",		"short",
		"short unsigned int",	"unsigned short",
	};
	c_typetab_t *ct;
	alias_t *al;
	const char *line, *cptr;
	int len;

	if ((line = symstring(st->st_symio_id, symno)) == NULL)
		panic("NULL line in gbt");
	if ((cptr = strchr(line, ':')) == NULL)
		panic("missing ':' in gbt");
	len = cptr - line;

	type->ty_base = NULL;
	type->ty_typedef = NULL;
	
	if (language == LANG_FORTRAN) {
		f77_typetab_t *ft;

		for (ft = f77_typetab; ft < f77_typetab + N_F77_TYPES; ++ft) {
			if (strncmp(ft->ft_name, line, len) == 0 &&
						ft->ft_name[len] == '\0') {
				type->ty_code = ft->ft_code;
				type->ty_size = ft->ft_size;
				if (ft->ft_code == TY_CHARACTER) 
					type->ty_name = "character";
				else
					type->ty_name = ft->ft_name;
				return;
			}
		}
		panic("unknown f77 basic type line in symtab");
	}

	if (language != LANG_C)
		panic("unknown lang in gbt");

	/*  Gcc puts rather cumbersome names for some C types in the symbol
	 *  table.  If the name is one of these, rewrite it in the more
	 *  usual C idiom, otherwise just take a copy of the name.
	 */
	for (al = aliastab; ; ++al) {
		if (al == aliastab + sizeof aliastab / sizeof *aliastab) {
			char *name;

			name = alloc(st->st_alloc_id, len + 1);
			(void) strncpy(name, line, len);
			name[len] = '\0';
			type->ty_name = name;
			break;
		}
		if (strncmp(al->a_name, line, len) == 0 && al->a_name[len] == '\0') {
			type->ty_name = al->a_alias;
			break;
		}
	}

	if (dim == NULL) {
		if (strcmp(type->ty_name, "void") != 0)
			panic("unknown special type in gbt");
		type->ty_code = TY_VOID;
		type->ty_size = 0;
		return;
	}

	for (ct = c_typetab; ct < c_typetab + N_C_TYPES; ++ct) {
		if (dim->di_low == ct->ct_low && dim->di_high - 1 == ct->ct_high) {
			type->ty_code = ct->ct_code;
			type->ty_size = ct->ct_size;
			return;
		}
	}

	panic("unknown size range");
}

/*  TypeId:
 *	Typenum
 *	Typenum '=' TypeDef
 *	Typenum '=' TypeAttrs TypeDef 	{ NI }
 *
 */
type_t *
TypeId(stf, p_symno, p_s, eval)
stf_t *stf;
int *p_symno;
const char **p_s;
bool eval;
{
	static char utypechars[] = "sue";
	static typecode_t utypes[] = { TY_U_STRUCT, TY_U_UNION, TY_U_ENUM };
	char *pos;
	stf_t *nstf;
	typecode_t utypecode;
	int fnum, tnum, save_symno;
	bool have_typenum, is_named_type, is_typedefed;
	const char *s, *tag;
	alloc_id_t alloc_id;
	type_t *rtype;

	s = *p_s;
	alloc_id = stf->stf_symtab->st_alloc_id;

	is_named_type = s[-1] == 'T';
	is_typedefed = s[-1] == 't';
	have_typenum = isdigit(*s) || *s == '(';

	if (have_typenum) {
		Typenum(&s, &fnum, &tnum);
		if (fnum >= stf->stf_mapsize)
			panic("fnum out of range in TypeId");
		nstf = stf->stf_fmap[fnum]->hf_stf;

		if (*s == '=' && s[1] == 'x' &&
					(pos = strchr(utypechars, s[2])) != NULL) {
			utypecode = utypes[pos - utypechars];
			s += 3;
			tag = parse_name(&s, alloc_id);
			scheck(&s, ':');
		}
		else {
			utypecode = TY_INT_ASSUMED;
			tag = NULL;
		}
		rtype = lookup_tnum(nstf, tnum);
	}
	else {
		rtype = NULL;
		nstf = stf;
		tnum = -1;
		utypecode = TY_NOTYPE;		/* to satisfy gcc */
		tag = 0;			/* to satisfy gcc */
	}

	if (is_named_type && rtype != NULL)
		return rtype;
	save_symno = *p_symno;

	if (have_typenum) {
		if (*s == '=') {
			++s;
			if (*s == '@')
				notimp("Type attributes");
			if (rtype == NULL)
				rtype = TypeDef(nstf, p_symno, &s, tnum, eval);
			else if (!is_typedefed)
				(void) TypeDef(nstf, p_symno, &s, tnum, FALSE);
		}
		else if (eval) {
			rtype = tnum_to_type(nstf, tnum);
			if (rtype == NULL) {
				ftype_t *ft;

				ft = insert_ftype(nstf, tnum);
				rtype = ci_make_undef_type(alloc_id, tag, utypecode,
								    &ft->ft_type);
				
				if (utypecode == TY_U_STRUCT ||
				    utypecode == TY_U_UNION ||
				    utypecode == TY_U_ENUM) {
					aggr_or_enum_def_t *ae;
					block_t *bl;

					/*  BUG: struct might not be in
					 *       outermost block.
					 */
					ae = rtype->ty_aggr_or_enum;
					bl = nstf->stf_fil->fi_block;

					ae->ae_next = bl->bl_aggr_or_enum_defs;
					bl->bl_aggr_or_enum_defs = ae;
				}
			}
		}
	}
	else
		rtype = TypeDef(nstf, p_symno, &s, -1, eval);
	*p_s = s;

	if (rtype->ty_typedef == &Is_basic_type) {
		is_typedefed = FALSE;
		rtype->ty_typedef = NULL;
	}

	if (eval && (is_named_type || is_typedefed)) {
		const char *name;

		name = symstring(nstf->stf_symtab->st_symio_id, save_symno);

		if (is_typedefed) {
			typedef_t *td;

			td = (typedef_t *)alloc(alloc_id, sizeof(typedef_t));
			td->td_name = parse_name(&name, alloc_id);
			td->td_lexinfo = NULL;
			td->td_type = rtype;
			rtype->ty_typedef = td;
		}
		else {
			switch(rtype->ty_code) {
			case TY_U_STRUCT:
			case TY_U_UNION:
			case TY_U_ENUM:
			case TY_STRUCT:
			case TY_UNION:
			case TY_ENUM:
				break;
			default:
				panic("bad type code in TypeId");
			}
			rtype->ty_aggr_or_enum->ae_tag = parse_name(&name, alloc_id);
		}
	}

	return rtype;
}

/*  Typedef:
 *	TYPENUM				{ NI }
 *	Subrange
 *	Array				{ PI, DEV }
 *	Record
 *	'e' Enumlist
 *	'*' TypeId
 *	'S' Typeid			{ Set of TypeId (NI) }
 *	'd' Typeid			{ File of TypeId (NI) }
 *	ProcedureType			{ PI }
 *	'i' NAME ':' NAME ';'		{ NI }
 *	'o' NAME ';'			{ NI }
 *	'i' NAME ':' NAME ',' TypeId ';'{ NI }
 *	'o' NAME ',' TypeId		{ NI }
 */
static type_t *
TypeDef(stf, p_symno, p_s, tnum, eval)
stf_t *stf;
int *p_symno;
const char **p_s;
int tnum, eval;
{
	ftype_t *ft, dummy_ftype;
	aggr_or_enum_def_t *ae;
	dim_t *dim;
	int is_struct, junk;
	const char *s;

	if (tnum == -1) {
		ft = (ftype_t *)alloc(stf->stf_symtab->st_alloc_id, sizeof(ftype_t));
		ft->ft_tnum = -1;
		ft->ft_type.ty_size = -1;
		ft->ft_type.ty_typedef = NULL;
		ft->ft_next = NULL;
	}
	else
		ft = eval ? insert_ftype(stf, tnum) : &dummy_ftype;

	s = *p_s;
	ae = NULL;
	switch(*s++) {

	/*  '*' TypeId */
	case '*':
		ft->ft_type.ty_code = DT_PTR_TO;
		ft->ft_type.ty_qualifiers = 0;
		ft->ft_type.ty_base = TypeId(stf, p_symno, &s, eval);
		break;

	/*  Typenum.  We assume this is a special case basic type (e.g. void).
	 */
	case '(':
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
		--s;
		Typenum(&s, &junk, &junk);
		get_basic_type(stf->stf_language, stf->stf_symtab,
					*p_symno, (dim_t *)NULL, &ft->ft_type);
		break;

	/*  Subrange.  We assume this means a basic type.
	 */
	case 'r':
		--s;
		dim = Subrange(stf, p_symno, &s, eval, FALSE);
		get_basic_type(stf->stf_language, stf->stf_symtab,
					*p_symno, dim, &ft->ft_type);

		/*  Signal to our caller (TypeId()) that this is not
		 *  a user typedef.
		 */
		ft->ft_type.ty_typedef = &Is_basic_type;

		break;

	/*  Array.
	 *  The grammar gives < 'a' TypeId ';' TypeId >, but we currently
	 *  only cope with < 'a' Subrange ';' TypeId >.
	 */
	case 'a':
		ft->ft_type.ty_code = DT_ARRAY_OF;
		ft->ft_type.ty_dim = Subrange(stf, p_symno, &s, eval, TRUE);
		scheck(&s, ';');
		ft->ft_type.ty_base = TypeId(stf, p_symno, &s, eval);
		break;
	
	/*  ProcedureType.
	 *  We only implement the < 'f' TypeId ';' > form.
	 *  Sun symbol tables seem to omit the ';'.
	 */
	case 'f':
		ft->ft_type.ty_code = DT_FUNC_RETURNING;
		ft->ft_type.ty_base = TypeId(stf, p_symno, &s, eval);
		break;
	
	/*  Record.
	 */
	case 's':
	case 'u':
		if (!eval)
			errf("Found aggregate with eval FALSE");
		is_struct = s[-1] == 's';
		ft->ft_type.ty_code = is_struct ? TY_STRUCT : TY_UNION;
		ae = Record(stf, p_symno, &s, is_struct, eval, &ft->ft_type);
		break;
	
	/*  'e' EnumList
	 */
	case 'e':
		if (!eval)
			errf("Found enum with eval FALSE");
		ae = EnumList(stf->stf_symtab, p_symno, &s, &ft->ft_type);
		break;

	default:
		panic("unknown character in TypeDef");
	}
	*p_s = s;
	return &ft->ft_type;
}

/*  Enum:
 *	'e' EnumList ';'
 *
 *  EnumList:
 *	Enum
 *	EnumList Enum
 *
 *  Some machines omit the ';' so we don't insist on it.
 *
 *  TypeDef above has already skipped the 'e'.
 */
static aggr_or_enum_def_t *
EnumList(st, p_symno, p_s, type)
symtab_t *st;
int *p_symno;
const char **p_s;
type_t *type;
{
	aggr_or_enum_def_t *ae;
	const char *s;

	s = *p_s;
	ae = ci_make_aggr_or_enum_def(st->st_alloc_id, (const char *)NULL,
								TY_ENUM, type);
	ae->ae_is_complete = AE_COMPLETE;
	ae->ae_enum_members = NULL;
	for (;;) {
		if (*s == '\\')
			s = get_cont_symstring(st->st_symio_id, p_symno);
		if (*s == ';' || *s == '\0')
			break;
		Enum(st, &s, ae);
	}
	if (*s != '\0')
		scheck(&s, ';');
	*p_s = s;
	return ae;
}

/*  Enum:
 *	NAME ':' OrdValue ','
 *
 *  OrdValue:
 *	INTEGER
 */
static void
Enum(st, p_s, ae)
symtab_t *st;
const char **p_s;
aggr_or_enum_def_t *ae;
{
	const char *s, *name;
	int val;
	enum_member_t *em;

	s = *p_s;

	name = parse_name(&s, st->st_alloc_id);
	scheck(&s, ':');
	val = parse_signed_num(&s);
	scheck(&s, ',');

	em = ci_make_enum_member(st->st_alloc_id, name, val);
	em->em_enum = ae;

	em->em_next = ae->ae_enum_members;
	ae->ae_enum_members = em;

	*p_s = s;
}

/*  Record:
 *	's' ByteSize FieldList ';'
 *	'u' ByteSize FieldList ';'
 *
 *  TypeDef above has already skipped the 's' or 'u'.
 */
static aggr_or_enum_def_t *
Record(stf, p_symno, p_s, is_struct, eval, type)
stf_t *stf;
int *p_symno;
const char **p_s;
int is_struct;
bool eval;
type_t *type;
{
	const char *s;
	aggr_or_enum_def_t *ae;
	typecode_t typecode;
	var_t *members;

	s = *p_s;
	typecode = is_struct ? TY_STRUCT : TY_UNION;
	ae = ci_make_aggr_or_enum_def(stf->stf_symtab->st_alloc_id,
				      (const char *)NULL, typecode, type);
	ae->ae_is_complete = AE_COMPLETE;
	ae->ae_size = parse_num(&s);
	members = NULL;
	for (;;) {
		if (*s == '\\') {
			s = get_cont_symstring(stf->stf_symtab->st_symio_id,
								     p_symno);
		}
		if (*s == ';')
			break;
		members = Field(stf, p_symno, &s, is_struct, members, eval);
	}
	ae->ae_aggr_members = members;

	scheck(&s, ';');
	*p_s = s;
	return ae;
}

/* Field:
 *	NAME ':' TypeId ',' BitOffset ',' BitSize ';'
 */
static var_t *
Field(stf, p_symno, p_s, is_struct, next, eval)
stf_t *stf;
int *p_symno;
const char **p_s;
int is_struct;
var_t *next;
bool eval;
{
	const char *s;
	type_t *type;
	int width, offset;
	var_t *v;

	s = *p_s;
	v = (var_t *) alloc(stf->stf_symtab->st_alloc_id, sizeof(var_t));
	v->va_name = parse_name(&s, stf->stf_symtab->st_alloc_id);
	v->va_language = stf->stf_language;
	v->va_flags = 0;
	v->va_class = is_struct ? CL_MOS : CL_MOU;
	scheck(&s, ':');
	type = TypeId(stf, p_symno, &s, eval);
	scheck(&s, ',');
	offset = parse_num(&s);
	scheck(&s, ',');
#ifdef ARCH_CLIPPER
	/*  The Clipper puts this in the symbol table for varargs entries:
	 *
	 *	va_arg0:p65=s4_nameless:1,0,-1;;
	 *
	 *  so allow -1 as a possible width.
	 */
	width = parse_signed_num(&s);
	if (width < 0) {
		if (width != -1)
			panic("width botch in Field");
		width = typesize(type) * 8;
	}
#else
	width = parse_num(&s);
#endif
	scheck(&s, ';');
	*p_s = s;
	if (!eval)
		return NULL;

	v->va_addr = offset / 8;

	if (IS_BASIC_TYPE(type->ty_code) && width != typesize(type) * 8)
		v->va_type = ci_make_bitfield_type(stf->stf_symtab->st_alloc_id,
						   type->ty_code,
						   offset - v->va_addr * 8,
						   width);
	else
		v->va_type = type;
	v->va_next = next;
	return v;
}

/*  Subrange:
 *	'r' TypeId ';' INTEGER ';' INTEGER
 *
 *  FRAGILE CODE:
 *
 *  We take the letters 'T', 'A' and 'J' as indicating a dynamic array.
 *  This is not documented in the dbx(5) manual page - it was determined
 *  by examining symbol tables produced by the SunOS 3.5 f77 compiler.
 *  Your mileage may vary.
 */
static dim_t *
Subrange(stf, p_symno, p_s, eval, want_subrange_type)
stf_t *stf;
int *p_symno;
const char **p_s;
bool eval, want_subrange_type;
{
	const char *s;
	dim_t *dim;

	s = *p_s;
	scheck(&s, 'r');
	dim = (dim_t *) alloc(stf->stf_symtab->st_alloc_id, sizeof(dim_t));

	if (want_subrange_type)
		dim->di_type = TypeId(stf, p_symno, &s, eval);
	else {
		int junk;

		Typenum(&s, &junk, &junk);
	}

	scheck(&s, ';');
	if (dim->di_ldynamic = *s == 'T' || *s == 'A' || *s == 'J')
		++s;
	dim->di_low = parse_signed_num(&s);
	scheck(&s, ';');
	if (dim->di_hdynamic = *s == 'T' || *s == 'A' || *s == 'J')
		++s;
	dim->di_high = parse_signed_num(&s) + 1;
	*p_s = s;
	return dim;
}

#endif /* !ST_TE */
