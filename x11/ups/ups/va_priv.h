/* va_priv.h - private header file for the va_*.c files */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)va_priv.h	1.8 25/4/92 (UKC) */

/*  Constants describing the variable display formats
 */
typedef enum vformaten {
	DF_NONE,	/* No format */
	DF_SDEC,	/* Signed decimal */
	DF_UDEC,	/* Unsigned decimal */
	DF_SOCT,	/* Signed octal */
	DF_UOCT,	/* Unsigned octal */
	DF_SHEX,	/* Signed hex */
	DF_UHEX,	/* Unsigned hex */
	DF_ASCII,	/* Ascii character */
	DF_STRING,	/* String */
	DF_UBIN		/* Unsigned binary */
} vformat_t;

/*  The brackets that bound the editable parts of declarations in
 *  the various languages.
 */

#define C_AOPEN		'<'
#define C_ACLOSE	'>'

#define FORTRAN_AOPEN	'['
#define FORTRAN_ACLOSE	']'

/*  Macros which test for a character being the opening or closing
 *  delimiters of a subscript.
 *
 *  We test for a union of the characters used in FORTRAN and C.
 *  This scheme will probably break down if we add more languages.
 */
#define IS_LBRAC(c)	((c) == '[' || (c) == '{' || (c) == ',')
#define IS_RBRAC(c)	((c) == ']' || (c) == '}' || (c) == ',')

/*  Which way the level is going in change_level().
 */
enum leveldir { CL_UP, CL_DOWN };

/*  Size returned by typesize for an object whose size cannot be determined.
 *  Currently the only objects of this sort are FORTRAN dynamic arrays.
 */
#define UNKNOWN_SIZE	(-1)

/*  Impossible addresses returned by dvar_addr() in case of error
 */
#define BAD_ADDR	((taddr_t)0x80000000)

typedef struct ilistst {
	char il_low_known;
	char il_high_known;
	int il_low;
	int il_high;
	int il_index;
	struct ilistst *il_next;
} ilist_t;

typedef struct dvarst {
	var_t *dv_var;			/* symbol table info for var */
	taddr_t dv_addr;		/* address of variable */
	short dv_ul_start;		/* char where underlining starts */
	short dv_ilevel;		/* indirection level */
	ilist_t *dv_ilist;		/* list of array indexes */
	vformat_t dv_format;		/* display format */
	unsigned dv_flags;		/* flags - see below */
	struct dvarst *dv_nextfree;	/* for free list management only */
} dvar_t;

#define DVF_VAL_CHANGED		0x001	/* value changed since last time */
#define DVF_NO_TYPEDEFS		0x002	/* don't use typedefs in decls */

/*  Field numbers in a variable object.
 */
#define FN_VAR_DECL	0	/* the declaration */
#define FN_VAR_VALUE	1	/* the value */
#define FN_VAR_LAST	2

/*  Function prototypes.
 */

/*  va_type.c
 */
int dynamic_type_size PROTO((type_t *type, ilist_t *il));
type_t *get_basetype PROTO((type_t *type));
int fix_if_fortran_dynamic_char PROTO((type_t *type, taddr_t addr,
							ilist_t *ilist));
type_t *get_type_at_level PROTO((var_t *v, int level));

/*  va_decl.c
 */
const char *mkdecl PROTO((dvar_t *dv));
taddr_t dvar_addr PROTO((dvar_t *dv));
int default_level PROTO((type_t *type));
vformat_t default_format PROTO((type_t *vtype, type_t *type));
ilist_t *make_ilist PROTO((type_t *orig_type, int level));
ilist_t *dup_ilist PROTO((ilist_t *old));
void free_ilist_list PROTO((ilist_t *ilist));

/*  va_val.c
 */
const char *mkval PROTO((dvar_t *dv));
void int_to_string PROTO((char *buf, int buflen, long n, vformat_t format));
void enumval_to_string PROTO((char *buf, int buflen, long val, type_t *type));
void addr_to_string PROTO((char *buf, int buflen, taddr_t addr,
						type_t *type, vformat_t format));
int get_decl_len PROTO((int len));
void get_value_colors PROTO((bool val_changed, short *p_fg, short *p_bg));
const char *deriv_to_string PROTO((typecode_t typecode));
vformat_t mval_to_vformat PROTO((int cmd));
void read_and_show_C_string PROTO((taddr_t addr, char *rbuf, int rbufsize,
							char *obuf, int obufsize));
#ifdef OBJ_H_INCLUDED
int var_or_expr_getwidth PROTO((objid_t obj, int fnum, fval_t fval));
#endif

/*  va_menu.c
 */
#ifdef OBJ_H_INCLUDED
void update_var PROTO((objid_t obj, int change_caused_by_edit));
void update_struct PROTO((objid_t par, int change_caused_by_edit));
#endif
type_t *deref_aggr PROTO((dvar_t *dv));
void change_dv_level PROTO((dvar_t *dv, enum leveldir level));
void redo_decl PROTO((dvar_t *dv));

/*  va_expr.c
 */
#ifdef OBJ_H_INCLUDED
void update_expr PROTO((objid_t obj, int change_caused_by_edit));
taddr_t var_or_expr_addr PROTO((objid_t obj));
#endif
