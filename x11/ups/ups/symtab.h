/* symtab.h - internal ups symbol table format */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)symtab.h	1.19 12/9/92 (UKC) */

/*  Define a symbol to indicate that we've been #included.
 *  This is so other prototype files like src.h can include
 *  prototypes involving fil_t * etc only if they've been
 *  defined.
 */
#define SYMTAB_H_INCLUDED

#include <mtrprog/so.h>		/* needed for fi_so */

/*  Languages
 */
typedef enum languageen {
	LANG_C,		/* C */
	LANG_FORTRAN,	/* FORTRAN */
	LANG_UNKNOWN	/* Unknown language */
} language_t;

/*  Variable code types.
 */
typedef enum typecodeen {

/* derivations */
	DT_PTR_TO,		/* pointer to type ty_base */
	DT_FUNC_RETURNING,	/* function returning type ty_base */
	DT_ARRAY_OF,	 	/* array [ty_dim] of ty_base */

/* seperator between derivations and base types */
	TY_NOTYPE,

/*  Types for the C interpreter */
	TY_IDENTIFIER,	/* "type" used for stashing varnames when parsing */
	TY_ELLIPSIS, 	/* temporary indicator that a function is variadic */
	TY_UNSIGNED,
	TY_SIGNED,

/* C aggregate types etc */
	TY_STRUCT,	/* structure */
	TY_UNION,	/* union */
	TY_ENUM,		/* enumeration */
	TY_U_STRUCT,	/* undefined structure */
	TY_U_UNION,	/* undefined union */
	TY_U_ENUM,	/* undefined enum */
	TY_BITFIELD,	/* bitfield */

/* seperator between aggregate and basic types */
	TY_BASIC_TYPES,

/* C base types */
	TY_VOID,		/* void */
	TY_CHAR,		/* character */
	TY_UCHAR,	/* unsigned character */
	TY_SHORT,	/* short integer */
	TY_USHORT,	/* unsigned short */
	TY_INT,		/* integer */
	TY_UINT,		/* unsigned int */
	TY_LONG,		/* long integer */
	TY_ULONG,	/* unsigned long */
	TY_FLOAT,	/* floating point */
	TY_DOUBLE,	/* double word */
	TY_INT_ASSUMED,	/* unknown - int assumed */

/* FORTRAN base types */
	TY_INTEGER_2,	/* integer*2 */
	TY_INTEGER_4,	/* integer*4 */
	TY_REAL,		/* real */
	TY_DBLPRES,	/* double precision */
	TY_COMPLEX,	/* complex */
	TY_DBLCOMP,	/* double complex */
	TY_LOGICAL,	/* logical */
	TY_CHARACTER,	/* character */
	TY_FVOID, 	/* FORTRAN void */

/* Maximum type number */
	TY_MAXTYPE

} typecode_t;

#define ISDERIV(code)		((int)(code) < (int)TY_NOTYPE)

#define IS_BASIC_TYPE(code)	((int)(code) > (int)TY_BASIC_TYPES)

/*  Lexical information - currently a filename and line number.
 *
 *  Used for recording where things are in the source for better
 *  error reporting.
 */
typedef struct lexinfost {
	const char *lx_filename;
	int lx_lnum;
	int lx_cnum;
} lexinfo_t;

/*  Opaque handle on a FORTRAN common block.  Non-opaque only in st_cb.c
 *
 *  We use an anonymous structure pointer to ensure that we do get a unique
 *  type.
 */
typedef struct { int common_block_dummy; } *common_block_id_t;

/*  Opaque handles on C interpreter objects
 */
typedef struct { int ei_dummy; } *expr_id_t;
typedef struct { int si_dummy; } *statement_id_t;
typedef struct { int ii_dummy; } *initialiser_id_t;

/*  Member of enum.
 */
typedef struct enum_memberst {
	expr_id_t em_expr_id;
	const char *em_name;
	long em_val;
	struct aggr_or_enum_defst *em_enum;	/* ptr back to parent enum struct */
	struct enum_memberst *em_next;
	lexinfo_t *em_lexinfo;
} enum_member_t;

typedef enum { AE_COMPLETE, AE_INCOMPLETE } ae_is_complete_t;

/*  Aggregate (struct, union) or enum definition.
 *  Whether the aggregate is a struct or a union is recorded in the type_t
 *  structure that points to this, which is pointed back to by ag_type.
 *
 *  We have a nasty special case due to the SunOS header file scheme:
 *  if ae_type is NULL then ae_sublist is a pointer to a sublist that
 *  should be interpolated at this point in the list.  This will recurse
 *  if there are nested #includes.
 */
typedef struct aggr_or_enum_defst {
	const char *ae_tag;
	ae_is_complete_t ae_is_complete;
	int ae_size;
	int ae_alignment;
	struct typest *ae_type;
	struct aggr_or_enum_defst *ae_next;
	union {
		struct varst *aeu_aggr_members;
		enum_member_t *aeu_enum_members;
		struct aggr_or_enum_defst *aeu_sublist;
	} ae_u;
	lexinfo_t *ae_lexinfo;
} aggr_or_enum_def_t;

#define ae_aggr_members		ae_u.aeu_aggr_members
#define ae_enum_members		ae_u.aeu_enum_members
#define ae_sublist		ae_u.aeu_sublist

/*  Array size description.  For C, the type of the range must be int
 *  and di_low must be zero. 
 *
 *  If di_ldynamic or di_hdynamic is TRUE, then the corresponding range
 *  field (di_low or di_high) is dynamic.  This is the case with FORTRAN
 *  parameter arrays with subranges defined by expressions.
 */
typedef struct dimst {
	short di_ldynamic;	/* TRUE if lower bound is dynamic */
	short di_hdynamic;	/* TRUE if upper bound is dynamic */
	long di_low;		/* lower bound, or where same can be found */
	long di_high;		/* upper bound, or where same can be found */
	expr_id_t di_high_expr_id;	/* see ci_parse.y */
	struct typest *di_type;	/* type of an element */
} dim_t;

/*  Bitfield description.
 */
typedef struct bitfieldst {
	typecode_t bf_code;
	expr_id_t bf_expr_id;
	short bf_offset;	/* within a word, hence 0..31 */
	short bf_width;
} bitfield_t;

typedef struct identifierst {
	const char *id_name;
	lexinfo_t *id_lexinfo;
	bool id_lparen_follows;
} identifier_t;

typedef struct identifier_listst {
	identifier_t *idl_id;
	struct identifier_listst *idl_next;
} identifier_list_t;

typedef enum { FDT_IDLIST, FDT_TYPELIST } params_type_t;

/*  Structure representing a "function returning" derivation.
 */
typedef struct funcretst {
	params_type_t fr_params_type;
	int fr_nparams;
	bool fr_is_variadic;
	bool fr_is_old_style;
	struct varst *fr_params;
	identifier_list_t *fr_idlist;
} funcret_t;

/*  Type qualifiers.  At present just a bit mask.
 */
typedef unsigned qualifiers_t;

#define QU_VOLATILE	01
#define QU_CONST	02

/*  Structure defining a type.  The type representation here is internal
 *  to ups and is not based on any standard.
 */
typedef struct typest {
	typecode_t ty_code;		/* derivation or base type */
	int ty_size;			/* size in bytes of this type (or -1) */
	struct typest *ty_base;		/* type that this type is derived from */
	union {
		qualifiers_t tyu_qualifiers;		/* DT_PTR_TO */
		dim_t *tyu_dim;				/* DT_ARRAY_OF */
		aggr_or_enum_def_t *tyu_aggr_or_enum;	/* TY_STRUCT/TY_UNION/TY_ENUM */
		bitfield_t *tyu_bitfield;		/* TY_BITFIELD */
		identifier_t *tyu_identifier;		/* TY_IDENTIFIER (pseudo) */
		const char *tyu_name;			/* a base type */
		funcret_t *tyu_funcret;			/* DT_FUNC_RETURNING */
	} ty_u;
	struct typedefst *ty_typedef;	/* non NULL if this type is defed */
} type_t;

#define ty_aggr_or_enum	ty_u.tyu_aggr_or_enum
#define ty_dim		ty_u.tyu_dim
#define ty_bitfield	ty_u.tyu_bitfield
#define ty_name		ty_u.tyu_name
#define ty_identifier	ty_u.tyu_identifier
#define ty_funcret	ty_u.tyu_funcret
#define ty_qualifiers	ty_u.tyu_qualifiers

/*  Class types
 */
typedef enum classen {
	CL_NOCLASS,	/* out of band value */
	CL_DECL,	/* declaration only */
	CL_AUTO,	/* automatic variable */
	CL_EXT,		/* external symbol */
	CL_STAT,	/* static */
	CL_REG,		/* register variable */
	CL_MOS,		/* member of structure */
	CL_ARG,		/* function argument passed by value */
	CL_REF,		/* function argument passed by reference */
	CL_MOU,		/* member of union */
	CL_MOE,		/* member of enumeration */
	CL_LSTAT,	/* local static */
	CL_FUNC,	/* function */
	CL_LFUNC,	/* static function */
	CL_TYPEDEF,	/* typedef */
	CL_TAGNAME	/* struct/union tag */
} class_t;

/*  Variable
 */
typedef struct varst {
	const char *va_name;	/* variable name */
	class_t va_class;	/* class of var (see C_ defines below) */
	language_t va_language;	/* language (C, FORTRAN etc) */
	short va_flags;		/* flags - see below */
	type_t *va_type;	/* variable type */
	int va_addr;		/* variable address */
	struct varst *va_next;	/* next variable */
	lexinfo_t *va_lexinfo;
} var_t;

/*  Flags describing a variable.
 */
#define VA_SET			0x0001	/* var has been assigned to */
#define VA_IS_CI_VAR		0x0002	/* var is private to the C interpreter */
#define VA_HAS_INITIALISER	0x0004	/* var is initialised */
#define VA_HIDE_PTR		0x0008	/* hide outer ptr type (see st_skim.c) */

/*  Element in the list of functions attached to a source file.
 *
 *  This list is doubly linked for historical reasons.  It is built
 *  in reverse order in st_skim.c because that's easier.  Thus the
 *  functions that use it were written to handle the reversed list.
 *  Then I needed a forwards list (in get_fi_types()) and it was too
 *  much hassle to go and rewrite the existing functions, so I just
 *  added forwards links.
 *
 *  TODO: build the list forwards, and drop the fl_prev link.
 */
typedef struct funclistst {
	struct funcst *fl_func;
	struct funclistst *fl_next;
	struct funclistst *fl_prev;
} funclist_t;

/*  Opaque handle on a symbol table.
 */
typedef struct symtab_idst { int dummy_member; } *symtab_id_t;

/*  Opaque handle on the syminfo structure of a function.
 *  The field of this structure are used only in st_*.c.
 */
typedef struct fsyminfo_idst { int dummy_member; } *fsyminfo_id_t;

/*  Opaque handle on the preamble function structure used by ups
 *  in stack.c and text.c
 */
typedef struct preamble_idst { int dummy_member; } *preamble_id_t;

/*  source file structure
 */
typedef struct filst {
	const char *fi_name;		/* file name */
	const char *fi_path_hint;	/* possible directory of source file */
	language_t fi_language;		/* programming language of the file */
	short fi_flags;
	long fi_stf;			/* used in st_skim.c only */
	so_id_t fi_so;			/* handle on file for displaying source */
	long fi_editblocks_id;		/* handle on added editable lines */
	struct blockst *fi_block;	/* vars and defs with file scope */
	funclist_t *fi_funclist_head;	/* head of list of functions in this file */
	funclist_t *fi_funclist_tail;	/* tail of list of functions in this file */
	struct filst *fi_next;		/* next file */
} fil_t;

#define FI_DONE_VARS		0x1
#define FI_DONE_TYPES		0x2
#define FI_DOING_TYPES		0x4

var_t *get_fi_vars PROTO((fil_t *fil));

#define FI_VARS(fil)	(((fil)->fi_flags & FI_DONE_VARS) \
						? (fil)->fi_block->bl_vars \
					        : get_fi_vars(fil))

/* line number structure.
 */
typedef struct lnost {
	taddr_t ln_addr;	/* text offset */
	int ln_num;		/* line number */
	struct lnost *ln_next;	/* next line */
} lno_t;

/*  We have the same kind of special case here as for aggr_or_enum_def_t:
 *  SunOS header files mean we have to handle sublists.
 */
typedef struct typedefst {
	const char *td_name;
	type_t *td_type;	/* the type_t structure that points to this */
	struct typedefst *td_next;
	union {
		lexinfo_t *tdu_lexinfo;
		struct typedefst *tdu_sublist;
	} td_u;
} typedef_t;

#define td_lexinfo	td_u.tdu_lexinfo
#define td_sublist	td_u.tdu_sublist

typedef struct initlistst {
	var_t *il_var;
	fil_t *il_fil;
	initialiser_id_t il_initialiser_id;
	struct initlistst *il_next;
} initlist_t;

/*  Structure describing a block.
 */
typedef struct blockst {
	int bl_start_lnum;		/* first line # */
	int bl_end_lnum;		/* last line # */
	var_t *bl_vars;			/* vars declared at in this block */
	typedef_t *bl_typedefs;		/* typedefs declared in this block */
	aggr_or_enum_def_t *bl_aggr_or_enum_defs;
	initlist_t *bl_initlist;	/* used by the C interpreter */
	struct blockst *bl_next;	/* next block at this level */
	struct blockst *bl_blocks;	/* sub blocks */
	struct blockst *bl_parent;	/* parent block */
} block_t;

/*  Maximum block nesting level.
 */
#define MAX_BLOCK_LEVEL	32

/*  Structure defining a function.  As we only read the symbol table
 *  on demand, many of the fields are initially unset.
 *
 *  The fields with a double underscore (e.g. fu__blocks) should not
 *  be accessed directly.  Instead the corresponding macro (e.g. FU_BLOCKS(f))
 *  should be used.
 */
typedef struct funcst {
	/*  Always valid.
	 */
	short fu_flags;			/* flags, see below */
	const char *fu_name;		/* function name */
	type_t *fu_type;
	lexinfo_t *fu_lexinfo;
	taddr_t fu_addr;		/* function text area address */
	fil_t *fu_fil;			/* source file */
	language_t fu_language;		/* source language */
	symtab_id_t fu_symtab_id; /* symbol table of this function */
	funclist_t *fu_fl;		/* pointer to funclist entry */
	struct funcst *fu_next;

	fsyminfo_id_t fu_fsyminfo_id;	/* file symtab stuff for st_*.c in ups */
	statement_id_t fu_statement_id;	/* used only for C interpreter functions */
	preamble_id_t fu_preamble_id;	/* used by ups for preamble info */

	/*  Valid if FU_DONE_LNOS set
	 */
	lno_t *fu__lnos;		/* list of line numbers, if any */
	short fu_max_lnum;

	/*  Valid if FU_DONE_BLOCKS set
	 */
	block_t *fu__blocks;		/* block list */
} func_t;

/*  Bits in fu_flags
 */
#define FU_NOSYM	  0x1	/* No symbol table info */
#define FU_DONE_LNOS	  0x2	/* Have processed line number info */
#define FU_DONE_BLOCKS	  0x4	/* Have processed block info */
#define FU_NO_FP	  0x8	/* Function doesn't set up frame pointer */
#define FU_STATIC	 0x10	/* Function is declared static (C interpreter) */

/*  Macros for accessing fields which may need to be read in.
 */
#define FU_BLOCKS(f)	(((f)->fu_flags & FU_DONE_BLOCKS) ? (f)->fu__blocks \
							  : get_fu_blocks(f))

#define FU_LNOS(f)	(((f)->fu_flags & FU_DONE_LNOS) ? (f)->fu__lnos \
							: get_fu_lnos(f))

/*  Function prototypes.  BUG: I'm not sure they belong here.
 *  Also, typesize is defined in va_*.c and used in st_*.c - dubious.
 */
block_t *get_fu_blocks PROTO((func_t *f));
lno_t *get_fu_lnos PROTO((func_t *f));
int typesize PROTO((type_t *type));
func_t *addr_to_func PROTO((taddr_t addr));
int find_global_by_name PROTO((const char *name, fil_t *fil, func_t *f,
			       bool exact, func_t **p_f, var_t **p_v,
			       common_block_id_t *p_cblock, fil_t **p_fil));
int resolve_untyped_name PROTO((const char *name, var_t **p_v));
int find_func_by_name PROTO((const char *name, func_t **p_f));

func_t *name_and_fil_to_func PROTO((const char *name, fil_t *fil));
fil_t *name_to_fil PROTO((const char *name));

const char *get_cblock_name PROTO((common_block_id_t cblock));
const char *get_cblock_funcname PROTO((common_block_id_t cblock));
var_t *get_cblock_vars PROTO((common_block_id_t cblock));
void iterate_over_common_blocks PROTO((void (*func)PROTO((common_block_id_t cblock))));

typedef int (*iof_func_t)PROTO((func_t *f, taddr_t addr,
						char *arg1, char *arg2));
int iterate_over_functions PROTO((symtab_id_t symtab_id, iof_func_t func,
						char *arg1, char *arg2));
void iterate_over_vars_of_block PROTO((block_t *block,
				       void (*func)(var_t *v, char *args),
				       char *args));

void set_use_srcpath_only_flag PROTO((void));
int open_source_file PROTO((fil_t *fil, bool want_error_messages));
void iterate_over_source_files PROTO((void (*func)PROTO((fil_t *fil))));
int textfile_tread PROTO((symtab_id_t symtab_id,
			  taddr_t addr, char *buf, int nbytes));
taddr_t get_addr_lim PROTO((func_t *f));
int open_textfile PROTO((const char *name, taddr_t *p_data_addr));
int get_and_install_symtabs PROTO((const char *execfile, int fd,
				   int *p_have_common_blocks));
int load_shared_library_symtabs PROTO((void));
void unload_shared_library_symtabs PROTO((void));
void debug_load_symbols PROTO((const char *name));

const char *get_target_name PROTO((fil_t *fil));
long get_target_mod_time PROTO((fil_t *fil));

bool lnum_is_highlighted PROTO((fil_t *fil, int lnum));
void set_highlighted_line PROTO((fil_t *fil, int lnum));
