/* ci_util.h - utility routines for the C interpreter */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

/* @(#)ci_util.h	1.6 26/4/92 (UKC) */

const char *ci_lexinfo_to_string PROTO((lexinfo_t *lx, lexinfo_t *cur_lx));
type_t *ci_make_pointer PROTO((type_t *base, qualifiers_t qualifiers));
declaration_t *ci_make_declaration PROTO((class_t class, type_t *basetype,
					  qualifiers_t qualifiers,
					  declarator_t *declarators));
declarator_t *ci_make_declarator PROTO((type_t *type));
initexpr_t *ci_make_initexpr PROTO((bool is_list,
					  expr_t *expr, initexpr_t *list));
type_t *ci_make_array_type PROTO((type_t *type, expr_t *dim_expr));
enum_member_t *ci_build_enum_member PROTO((identifier_t *id, expr_t *expr));
type_t *ci_make_expr_bitfield_type PROTO((type_t *base, expr_t *expr));

initlist_t *ci_push_initlist PROTO((initlist_t *newlist, initlist_t *list));
expr_list_t *ci_reverse_expr_list PROTO((expr_list_t *el));
type_t *ci_push_types PROTO((type_t *typelist, type_t *types));


typedef enum { ET_ERROR, ET_WARNING, ET_MORE, ET_IGNORE } errtype_t;

void diagf PROTO((errtype_t errtype, lexinfo_t *lx, const char *fmt, ...))
							    FORMF_ARGS(3, 4);

typedef void (*diag_handler_func_t) PROTO((char *arg, errtype_t errtype,
					   lexinfo_t *lx, const char *mesg));
void ci_set_diag_handler PROTO((diag_handler_func_t func, char *arg));

void ci_panic PROTO((const char *s));


identifier_list_t *ci_reverse_idlist PROTO((identifier_list_t *idlist));

extern alloc_id_t Parse_alloc_id;

#define NEW(type)	((type *)alloc(Parse_alloc_id, sizeof(type)))
