/* ci_types.h - header file for ci_types.c */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)ci_types.h	1.3 4/7/91 (UKC) */

void report_redecl PROTO((const char *name,
				 nametype_t nametype, lexinfo_t *this_lx,
				 nametype_t prev_nametype, lexinfo_t *prev_lx));
bool ci_types_same PROTO((type_t *t1, type_t *t2));
type_t *ci_resolve_typedefs PROTO((type_t *type));
void ci_complain_if_types_differ PROTO((const char *name,
					type_t *prev_type, lexinfo_t *prev_lexinfo,
					type_t *type, lexinfo_t *lexinfo));
bool ci_complain_about_any_void_types PROTO((var_t *vlist));
bool ci_is_integral PROTO((typecode_t typecode));
bool ci_is_signed_type PROTO((typecode_t typecode));
void ci_show_type PROTO((type_t *type, const char *what));
taddr_t ci_align_addr PROTO((taddr_t addr, int alignment));
taddr_t ci_align_addr_for_type PROTO((taddr_t addr, type_t *type));
int ci_type_alignment PROTO((type_t *type));

void ci_add_type_specifier PROTO((declaration_t *declaration, type_t *type));
void ci_fix_signed_and_unsigned PROTO((declaration_t *declaration));

#define IS_ARITHMETIC_TYPE(code) \
	     (ci_is_integral(code) || ((code) == TY_FLOAT) || ((code) == TY_DOUBLE))

#define IS_ARITHMETIC_OR_PTR_TYPE(code) \
				(IS_ARITHMETIC_TYPE(code) || ((code) == DT_PTR_TO))
