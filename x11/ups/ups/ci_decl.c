/* ci_decl.c - routines used by ci_parse.y to build the parse tree */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_ci_decl_c_sccsid[] = "@(#)ci_decl.c	1.19 12/9/92 (UKC)";

#include <string.h>

#include <stdlib.h>

#include <local/ukcprog.h>

#include "ups.h"
#include "symtab.h"
#include "ci.h"
#include "ci_parse.h"
#include "ci_decl.h"
#include "ci_types.h"
#include "ci_util.h"
#include "ci_lex.h"
#include "ci_init.h"

static namedesc_t *lookup_name_in_block PROTO((block_t *block, const char *name));
static namedesc_t *lookup_name_in_varlist PROTO((var_t *vlist, const char *name));
static void check_redeclaration PROTO((namedesc_t *nd, var_t *v));
static statement_t *reverse_statements PROTO((statement_t *st));
static bool decl_wanted PROTO((namedesc_t *nd, var_t *v, initlist_t *il));
static taddr_t set_bitfield_addr PROTO((var_t *v, var_t *prev, taddr_t addr));
static type_t *check_bitfield_type PROTO((type_t *type, lexinfo_t *lx));
static bool is_incomplete_type PROTO((type_t *type));
static var_t *push_var_definitions PROTO((var_t *v, var_t *list));
static aggr_or_enum_def_t *find_tag PROTO((aggr_or_enum_def_t *ae, const char *tag));
static aggr_or_enum_def_t *lookup_enum_member PROTO((aggr_or_enum_def_t *ae,
								const char *tag));
static typedef_t *name_to_typedef PROTO((typedef_t *tdlist, const char *name));
static namedesc_t *make_namedesc PROTO((var_t *v));

/*  These globals are active during a given run of yyparse() only.
 *
 *  BUG: Current_block and Funclist not static, as used in ci_func.c
 */
block_t *Current_block;
func_t *Funclist;
static func_t *Old_funclist;
static initlist_t *Old_initlist;
static var_t *Varlist;
ci_resolve_name_func_t Resolve_name_func;

static bool Make_externs_toplevel;

/*  The outermost block created by the C interpreter.  The parent of this
 *  block is not necessarily NULL - see ci_start_parse_tree().
 */
static block_t *Outer_ci_block;

/*  Return TRUE if a redeclaration is wanted.
 *
 *  This function returns FALSE is the new declaration is an old style
 *  function declaration and we already have a new style declaration.
 *
 *  It also returns FALSE if the declaration is not a definition and there
 *  is a previous declaration or definition.
 *
 *  In other words, we don't want an old style declaration to hide an
 *  existing prototype and thus lose type checking.
 */
static bool
decl_wanted(nd, v, il)
namedesc_t *nd;
var_t *v;
initlist_t *il;
{
	type_t *oldtype, *newtype;

	if (nd->nd_nametype != NT_FUNCNAME && nd->nd_nametype != NT_VARNAME)
		return TRUE;

	if (nd->nd_nametype == NT_VARNAME && v->va_class == CL_DECL)
		return FALSE;
	
	oldtype = nd->nd_var->va_type;
	newtype = v->va_type;

	/*  If we are seeing a definition of a variable for which we
	 *  already have a declaration, turn the declaration into a
	 *  definition by copying the relevant bits from the definition,
	 *  and forget about the new definition (i.e. return FALSE).
	 */
	if (nd->nd_nametype == NT_VARNAME &&
	    nd->nd_var->va_class == CL_DECL && v->va_class != CL_DECL) {
		if (!ci_types_same(oldtype, newtype))
			ci_panic("type botch in dw");
		nd->nd_var->va_class = v->va_class;

		if ((nd->nd_var->va_flags & ~VA_IS_CI_VAR) != 0)
			ci_panic("unexpected va_flags in dw");
		nd->nd_var->va_flags = v->va_flags;

		/*  It is important to copy the type here, as the definition
		 *  of the variable may include information like the size of
		 *  arrays which is absent in the declaration.
		 */
		nd->nd_var->va_type = v->va_type;

		/*  If there is an initialiser for the variable, make it
		 *  point at what is now the definition.
		 */
		if (il != NULL && il->il_var == v)
			il->il_var = nd->nd_var;

		return FALSE;
	}

	if (nd->nd_var->va_class != CL_DECL ||
	    v->va_class != CL_DECL ||
	    oldtype->ty_code != DT_FUNC_RETURNING ||
	    newtype->ty_code != DT_FUNC_RETURNING)
		return TRUE;
	
	return oldtype->ty_funcret->fr_is_old_style ||
					!newtype->ty_funcret->fr_is_old_style;
}

void
ci_declarations_to_varlist(declarations, default_class, p_vlist, p_initlist)
declaration_t *declarations;
class_t default_class;
var_t **p_vlist;
initlist_t **p_initlist;
{
	var_t *vlist;
	initlist_t *initlist;
	declaration_t *dn;

	initlist = NULL;
	vlist = NULL;
	for (dn = declarations; dn != NULL; dn = dn->dn_next) {
		declarator_t *dr;
		class_t class;
		block_t *block;

		if ((class = dn->dn_class) == CL_NOCLASS)
			class = default_class;
		
		/*  The `auto' keyword in a formal parameter declaration
		 *  means class CL_ARG, not CL_AUTO.
		 */
		if (class == CL_AUTO && default_class == CL_ARG)
			class = CL_ARG;
		
		if (Make_externs_toplevel && class == CL_DECL &&
							default_class == CL_EXT)
			block = Outer_ci_block;
		else
			block = Current_block;

		for (dr = dn->dn_declarators; dr != NULL; dr = dr->dr_next) {
			const char *name;
			bool is_bitfield;
			lexinfo_t *lx;
			type_t *type;
			var_t *v;
			bool want_decl;

			is_bitfield = dr->dr_type != NULL &&
						dr->dr_type->ty_code == TY_BITFIELD;

			type = ci_push_types(dn->dn_basetype, dr->dr_type);

			if (type->ty_code == TY_IDENTIFIER) {
				name = type->ty_identifier->id_name;
				lx = type->ty_identifier->id_lexinfo;
				type = type->ty_base;
			}
			else {
				name = NULL;
				lx = NULL;
			}

			if (type == NULL) {
				if (name != NULL)
					diagf(ET_WARNING, lx,
					     "Type defaults to int for `%s'",
									name);
				type = ci_code_to_type(TY_INT);
			}

			if (is_bitfield)
				type = check_bitfield_type(type, lx);
			
			/*  We don't object to void types here if the name
			 *  is null.  This is so we don't bomb on the
			 *  special case prototype syntax f(void).
			 */
			if (type->ty_code == TY_VOID && name != NULL) {
				diagf(ET_ERROR, lx,
					"Illegal type void for `%s'", name);
				type = ci_code_to_type(TY_INT);
			}
								
			v = ci_make_var(Parse_alloc_id, name, class, type, 0);
			v->va_lexinfo = lx;

			/*  If this is just a declaration (class CL_DECL)
			 *  we may be declaring the type of a non CI variable,
			 *  so be conservative and only set the CI flag for
			 *  definitions.
			 */
			if (class != CL_DECL)
				v->va_flags = VA_IS_CI_VAR;

			if (type->ty_code == DT_FUNC_RETURNING &&
					    (class == CL_EXT || class == CL_AUTO))
				v->va_class = CL_DECL;

			if (dr->dr_initialiser != NULL) {
				initlist_t *il;

				il = ci_make_var_initialiser(v, dr->dr_initialiser);
				if (il != NULL) {
					il->il_next = initlist;
					initlist = il;
				}
			}

			want_decl = TRUE;

			if (name != NULL) {
				namedesc_t *nd;

				name = v->va_name;
				nd = lookup_name_in_varlist(vlist, name);
				if (nd != NULL) {
					check_redeclaration(nd, v);
					want_decl = decl_wanted(nd, v, initlist);
				}
				else if (default_class == CL_EXT ||
					 default_class == CL_AUTO ||
					 default_class == CL_TYPEDEF) {
					nd = lookup_name_in_block(block, name);
					if (nd != NULL) {
						check_redeclaration(nd, v);
						want_decl = decl_wanted(nd, v,
									initlist);
					}
				}
			}

			if (want_decl) {
				v->va_next = vlist;
				vlist = v;
			}
		}
	}
	*p_vlist = vlist;
	*p_initlist = initlist;
}

/*  BUG: Should build list of storage classes and complain later, then
 *       we'd be able to give the variable name and lexinfo.  We could
 *	 also look at the total result and give a better message.
 */
void
ci_set_storage_class(dn, class)
declaration_t *dn;
class_t class;
{
	if (class == CL_NOCLASS)
		return;

	if (dn->dn_class != CL_NOCLASS) {
		if (dn->dn_class == class)
			diagf(ET_WARNING, (lexinfo_t *)NULL,
					"Repeated storage class specifiers");
		else {
			diagf(ET_WARNING, (lexinfo_t *)NULL,
				 "Multiple different storage class specifiers");
			class = CL_NOCLASS;	/* avoid furthur messages */
		}
	}

	dn->dn_class = class;
}

void
ci_complain_about_any_initialisers(initlist, what)
initlist_t *initlist;
const char *what;
{
	initlist_t *il;

	for (il = initlist; il != NULL; il = il->il_next) {
		const char *name;

		name = il->il_var->va_name;
		if (name == NULL)
			name = "";
		diagf(ET_ERROR, il->il_var->va_lexinfo,
			"Illegal initialisation of %s `%s'", what, name);
	}
}

void
ci_complain_about_incomplete_types(vlist, what)
var_t *vlist;
const char *what;
{
	var_t *v;
	type_t *type;

	for (v = vlist; v != NULL; v = v->va_next) {
		type = v->va_type;

		if (type->ty_code == DT_FUNC_RETURNING)
			type = type->ty_base;
		else if (v->va_class == CL_DECL)
			continue;

		if (is_incomplete_type(type)) {
			diagf(ET_ERROR, v->va_lexinfo,
			      "%s `%s' is of incomplete type", what,v->va_name);
			ci_show_type(type, "type");
		}
	}
}

static aggr_or_enum_def_t *
find_tag(ae, tag)
aggr_or_enum_def_t *ae;
const char *tag;
{
	if (ae->ae_tag != NULL && strcmp(ae->ae_tag, tag) == 0)
		return ae;
	
	return NULL;
}

type_t *
ci_build_aggr_or_enum_def(typecode, id, new_is_complete, aggr_decls, enum_members)
typecode_t typecode;
identifier_t *id;
ae_is_complete_t new_is_complete;
declaration_t *aggr_decls;
enum_member_t *enum_members;
{
	const char *tag;
	block_t *bl;
	aggr_or_enum_def_t *ae = NEW(aggr_or_enum_def_t);

	tag = (id == NULL) ? NULL : id->id_name;
	if (tag != NULL) {
		for (bl = Current_block; bl != NULL; bl = bl->bl_parent) {
			ae = ci_apply_to_aelist(bl->bl_aggr_or_enum_defs,
						find_tag, tag);
			if (ae != NULL)
				break;
		}
	}
	else {
		ae = NULL;
		bl = NULL;	/* to satisfy gcc */
	}

	/*  Give error messages for any enum members which are
	 *  redeclarations of identifiers already declared at
	 *  this level.
	 */
	if (typecode == TY_ENUM) {
		enum_member_t *em1, *em2;

		for (em1 = enum_members; em1 != NULL; em1 = em1->em_next)
			for (em2 = enum_members; em2 != em1; em2 = em2->em_next)
				if (strcmp(em1->em_name, em2->em_name) == 0)
					report_redecl(em1->em_name,
						      NT_ENUM_CONST,em1->em_lexinfo,
						      NT_ENUM_CONST,em2->em_lexinfo);
	}

	if (ae != NULL && ae->ae_is_complete == AE_COMPLETE) {
		if (new_is_complete == AE_INCOMPLETE)
			return ae->ae_type;

		/*  Redeclaration.  Print an error message if it's at the same
		 *  level as the original declaration, then forget the old
		 *  declaration so we can insert a new one.
		 */
		if (bl == Current_block) {
			report_redecl(tag, NT_TAG, (id == NULL) ? NULL
								: id->id_lexinfo,
					   NT_TAG, ae->ae_lexinfo);
		}
		ae = NULL;
	}
			
	if (ae == NULL) {
		/*  This aggregate or enum definition has not been seen
		 *  before at this level.
		 *
		 *  Create a new aggregate entry, and assign the fields of
		 *  the type that don't change when an incomplete type is
		 *  completed.
		 */
		ae = ci_make_aggr_or_enum_def(Parse_alloc_id, tag, typecode,
								(type_t *)NULL);

		ae->ae_next = Current_block->bl_aggr_or_enum_defs;
		Current_block->bl_aggr_or_enum_defs = ae;
	}

	/*  At this point, there was either no previous definition of this
	 *  type, or the previous definition did not give the members.
	 */

	/*  Assign the fields that change when an incomplete type is completed.
	 */
	ae->ae_is_complete = new_is_complete;
	ae->ae_type->ty_code = typecode;
	if (id != NULL)
		ae->ae_lexinfo = id->id_lexinfo;

	if (typecode == TY_ENUM) {
		enum_member_t *em;

		for (em = enum_members; em != NULL; em = em->em_next)
			em->em_enum = ae;

		ae->ae_enum_members = enum_members;
	}
	else {
		var_t *v, *prev, *aggr_members;
		initlist_t *initlist;
		class_t class;
		taddr_t addr, max_addr;
		int alignment, max_alignment;

		class = (typecode == TY_STRUCT) ? CL_MOS : CL_MOU;
		ci_declarations_to_varlist(aggr_decls, class,
							&aggr_members, &initlist);
		ci_complain_about_any_initialisers(initlist, "Struct/union member");

		max_alignment = 1;
		max_addr = 0;
		prev = NULL;
		for (v = aggr_members; v != NULL; prev = v, v = v->va_next) {
			typecode_t vcode;

			vcode = v->va_type->ty_code;
			if (v->va_type->ty_code == TY_BITFIELD) {
				if (typecode != TY_STRUCT)
					diagf(ET_ERROR, v->va_lexinfo,
					      "Bitfield illegal in union");
				addr = set_bitfield_addr(v, prev, max_addr);
			}
			else if (is_incomplete_type(v->va_type)) {
				diagf(ET_ERROR, v->va_lexinfo,
				      "Incomplete type for %s member %s",
				      (typecode == TY_STRUCT) ? "struct"
							      : "union",
							v->va_name);
				ci_show_type(v->va_type, "type");
				continue;
			}
			else {
				if (typecode == TY_STRUCT)
					addr = ci_align_addr_for_type(max_addr,
								      v->va_type);
				else
					addr = 0;
				v->va_addr = addr;
				addr += ci_typesize(v->va_lexinfo, v->va_type);
			}

			alignment = ci_type_alignment(v->va_type);
			if (alignment > max_alignment)
				max_alignment = alignment;
			
			if (addr > max_addr)
				max_addr = addr;
		}
		ae->ae_size = ci_align_addr(max_addr, max_alignment);
		ae->ae_alignment = max_alignment;
		ae->ae_aggr_members = aggr_members;
	}

	return ae->ae_type;
}

static bool
is_incomplete_type(type)
type_t *type;
{
	if (type->ty_code == TY_U_STRUCT || type->ty_code == TY_U_UNION)
		return TRUE;
	if (type->ty_code == DT_ARRAY_OF && type->ty_dim->di_hdynamic)
		return TRUE;
	return FALSE;
}

static type_t *
check_bitfield_type(type, lx)
type_t *type;
lexinfo_t *lx;
{
	if (type->ty_code != TY_BITFIELD) {
		diagf(ET_ERROR, lx, "Bitfields must have type int or unsigned");
		return type;
	}

	switch (type->ty_base->ty_code) {
	case TY_INT:
	case TY_UINT:
		type->ty_bitfield->bf_code = type->ty_base->ty_code;
		break;
	case TY_ULONG:
	case TY_USHORT:
	case TY_UCHAR:
		diagf(ET_WARNING, lx, "Illegal bitfield type %s",
					ci_basetype_name(type->ty_base));
		type->ty_bitfield->bf_code = TY_UINT;
		break;
	case TY_LONG:
	case TY_SHORT:
	case TY_CHAR:
		diagf(ET_WARNING, lx, "Illegal bitfield type %s",
					ci_basetype_name(type->ty_base));
		type->ty_bitfield->bf_code = TY_INT;
		break;
	default:
		diagf(ET_ERROR, lx, "Illegal bitfield type %s",
					ci_basetype_name(type->ty_base));
		type->ty_bitfield->bf_code = TY_NOTYPE;
		break;
	}
	type->ty_base = NULL;
	return type;
}

static taddr_t
set_bitfield_addr(v, prev, addr)
var_t *v, *prev;
taddr_t addr;
{
	bitfield_t *bf;
	int offset, bits_per_word;

	if (v->va_type->ty_code != TY_BITFIELD)
		ci_panic("sba called on non bitfield");
	bf = v->va_type->ty_bitfield;

	bits_per_word = ci_typesize((lexinfo_t *)NULL, ci_code_to_type(TY_INT)) * 8;

	if (bf->bf_width < 0 || bf->bf_width > bits_per_word) {
		diagf(ET_ERROR, v->va_lexinfo,
				"Illegal bitfield width %d", bf->bf_width);
		return addr;
	}

	if (prev == NULL || prev->va_type->ty_code != TY_BITFIELD)
		offset = bits_per_word;
	else
		offset = prev->va_type->ty_bitfield->bf_width +
					prev->va_type->ty_bitfield->bf_offset;

	if (bf->bf_width == 0 || offset + bf->bf_width > bits_per_word) {
		v->va_addr = ci_align_addr_for_type(addr, v->va_type);
		bf->bf_offset = 0;
	}
	else {
		v->va_addr = prev->va_addr;
		bf->bf_offset = offset;
	}
	return v->va_addr + ci_typesize(v->va_lexinfo, v->va_type);
}

void
ci_handle_declaration(declaration)
declaration_t *declaration;
{
	class_t default_class;
	var_t *vlist;
	initlist_t *initlist;

	if (declaration->dn_class == CL_TYPEDEF) {
		ci_add_typedefs_to_current_block(declaration);
		return;
	}

	if (Current_block->bl_parent == Outer_ci_block)
		default_class = CL_ARG;
	else
		default_class = (Current_block == Outer_ci_block) ? CL_EXT : CL_AUTO;
	ci_declarations_to_varlist(declaration, default_class, &vlist, &initlist);

	/*  If the parent of the current block is the outer block,
	 *  vlist refers to old-style formal parameters.  We check
	 *  these in ci_check_func_decls() so don't check them here.
	 *  This is partly because we want the incomplete type check
	 *  to come after the rewrite of `int x[]' to `int *x' that
	 *  happens for formal parameters.
	 */
	if (Current_block->bl_parent != Outer_ci_block)
		ci_complain_about_incomplete_types(vlist, "Variable");

	Current_block->bl_initlist = ci_push_initlist(initlist,
						 Current_block->bl_initlist);
	if (Make_externs_toplevel) {
		var_t *v, *next;

		for (v = vlist; v != NULL; v = next) {
			next = v->va_next;
			if (v->va_class == CL_DECL && default_class != CL_ARG) {
				v->va_next = Outer_ci_block->bl_vars;
				Outer_ci_block->bl_vars = v;
			}
			else {
				v->va_next = Current_block->bl_vars;
				Current_block->bl_vars = v;
			}
		}
	}
	else {
		Current_block->bl_vars = ci_push_vars(vlist, Current_block->bl_vars);
	}
}

void
ci_insert_var_declaration(v)
var_t *v;
{
	v->va_next = Current_block->bl_vars;
	Current_block->bl_vars = v;
}

static statement_t *
reverse_statements(st)
statement_t *st;
{
	statement_t *list, *next;

	list = NULL;
	for (; st != NULL; st = next) {
		next = st->st_next;
		st->st_next = list;
		list = st;
	}
	return list;
}

statement_t *
ci_end_compound_statement(statements)
statement_t *statements;
{
	statement_t *st = NEW(statement_t);
	
	st->st_type = STT_COMPOUND;
	st->st_lexinfo = NULL;
	st->st_compound = NEW(compound_stm_t);
	st->st_compound->co_block = Current_block;
	st->st_compound->co_statements = reverse_statements(statements);
	ci_end_block();
	return st;
}

void
ci_add_var_decl_for_func_if_necessary(f)
func_t *f;
{
	namedesc_t *nd;
	var_t *v;
	block_t *bl;

	bl = Outer_ci_block;
	if ((nd = lookup_name_in_varlist(bl->bl_vars, f->fu_name)) == NULL) {
		v = ci_make_var(Parse_alloc_id, f->fu_name, CL_DECL, f->fu_type, 0);
		v->va_lexinfo = f->fu_lexinfo;
		v->va_next = bl->bl_vars;
		bl->bl_vars = v;
	}
}

static namedesc_t *
make_namedesc(v)
var_t *v;
{
	static namedesc_t ndbuf;

	ndbuf.nd_nametype = (v->va_type->ty_code == DT_FUNC_RETURNING)
						? NT_FUNCNAME : NT_VARNAME;
	ndbuf.nd_lexinfo = v->va_lexinfo;
	ndbuf.nd_var = v;
	return &ndbuf;
}

static namedesc_t *
lookup_name_in_varlist(vlist, name)
var_t *vlist;
const char *name;
{
	var_t *v;

	for (v = vlist; v != NULL; v = v->va_next) {
		if (v->va_name != NULL && strcmp(v->va_name, name) == 0)
			return make_namedesc(v);
	}
	return NULL;
}

static aggr_or_enum_def_t *
lookup_enum_member(ae, tag)
aggr_or_enum_def_t *ae;
const char *tag;
{
	enum_member_t *em;

	if (ae->ae_type->ty_code != TY_ENUM)
		return NULL;

	for (em = ae->ae_enum_members; em != NULL; em = em->em_next) {
		if (strcmp(em->em_name, tag) == 0)
			return (aggr_or_enum_def_t *)em;
	}
	
	return NULL;
}

/*  Search the names defined in block block for name name.
 *
 *  We search for name as a object name, a typedef name or an enum constant.
 *
 *  The returned pointer points to a static object, which is overwritten
 *  on each call.
 */
static namedesc_t *
lookup_name_in_block(block, name)
block_t *block;
const char *name;
{
	static namedesc_t ndbuf;
	namedesc_t *nd;
	enum_member_t *em;
	typedef_t *td;

	/*  Variable name?
	 */
	if ((nd = lookup_name_in_varlist(block->bl_vars, name)) != NULL)
		return nd;

	/*  Enum constant?
	 */
	em = (enum_member_t *)ci_apply_to_aelist(block->bl_aggr_or_enum_defs,
						 lookup_enum_member, name);
	if (em != NULL) {
		ndbuf.nd_nametype = NT_ENUM_CONST;
		ndbuf.nd_lexinfo = em->em_lexinfo;
		ndbuf.nd_enum_member = em;
		return &ndbuf;
	}

	/*  Typedef name?
	 */
	if ((td = name_to_typedef(block->bl_typedefs, name)) != NULL) {
		ndbuf.nd_nametype = NT_TYPEDEF_NAME;
		ndbuf.nd_lexinfo = td->td_lexinfo;
		ndbuf.nd_typedef = td;
		return &ndbuf;
	}

	return NULL;
}

namedesc_t *
ci_lookup_name(name)
const char *name;
{
	block_t *bl;
	namedesc_t *nd;
	var_t *v;

	for (bl = Current_block; bl != NULL; bl = bl->bl_parent)
		if ((nd = lookup_name_in_block(bl, name)) != NULL)
			return nd;

	if (Resolve_name_func != NULL && (*Resolve_name_func)(name, &v) == 0)
		return make_namedesc(v);

	return NULL;
}

static typedef_t *
name_to_typedef(tdlist, name)
typedef_t *tdlist;
const char *name;
{
	typedef_t *td, *rtd;

	for (td = tdlist; td != NULL; td = td->td_next) {
		if (td->td_type == NULL) {
			if ((rtd = name_to_typedef(td->td_sublist, name)) != NULL)
				return rtd;
		}
		else if (strcmp(td->td_name, name) == 0)
			return td;
	}

	return NULL;
}

type_t *
ci_lookup_typedef(name)
const char *name;
{
	block_t *bl;
	typedef_t *td;

	for (bl = Current_block; bl != NULL; bl = bl->bl_parent) {
		if ((td = name_to_typedef(bl->bl_typedefs, name)) != NULL)
			return td->td_type;
	}

	return NULL;
}

void
ci_start_block(add_to_parent)
bool add_to_parent;
{
	block_t *bl, *lastchild, *child;

	child = ci_make_block(Parse_alloc_id, Current_block);
	child->bl_start_lnum = Lex_env->le_lnum;

	if (add_to_parent) {
		lastchild = NULL;
		for (bl = Current_block->bl_blocks; bl != NULL; bl = bl->bl_next)
			lastchild = bl;
		if (lastchild != NULL)
			lastchild->bl_next = child;
		else
			Current_block->bl_blocks = child;
	}
	
	Current_block = child;
}

void
ci_end_block()
{
	Current_block->bl_end_lnum = Lex_env->le_lnum;
	Current_block = Current_block->bl_parent;
}

void
ci_free_parse_id(parse_id)
parse_id_t parse_id;
{
	alloc_free_pool(((parse_res_t *)parse_id)->pr_alloc_id);
}

void
ci_start_parse_tree(pr, resolve_name_func, block, flags)
parse_res_t *pr;
ci_resolve_name_func_t resolve_name_func;
block_t *block;
unsigned long flags;
{

	if (pr == NULL) {
		Parse_alloc_id = alloc_create_pool();
		Old_funclist = Funclist = NULL;
		Current_block = ci_make_block(Parse_alloc_id, block);
		Varlist = NULL;
	}
	else {
		Parse_alloc_id = pr->pr_alloc_id;

		Old_funclist = Funclist;
		Funclist = pr->pr_funcs;

		Current_block = ci_make_block(Parse_alloc_id, block);

		Current_block->bl_initlist = pr->pr_block->bl_initlist;
		Varlist = pr->pr_block->bl_vars;
	}

	Old_initlist = Current_block->bl_initlist;

	Outer_ci_block = Current_block;
	Make_externs_toplevel = (flags & CI_MAKE_EXTERNS_TOPLEVEL) != 0;

	Resolve_name_func = resolve_name_func;
}

static var_t *
push_var_definitions(v, list)
var_t *v, *list;
{
	var_t *next;

	for (; v != NULL; v = next) {
		next = v->va_next;
		if (v->va_class != CL_DECL) {
			v->va_next = list;
			list = v;
		}
	}
	return list;
}

parse_res_t *
ci_end_parse_tree(parse_succeeded)
bool parse_succeeded;
{
	parse_res_t *pr;

	if (parse_succeeded) {
		fil_t *fil;
		funclist_t *funclist;
		initlist_t *il;
		func_t *f;

		if (Current_block != Outer_ci_block)
			ci_panic("block level botch in ept");

		pr = (parse_res_t *)alloc(Parse_alloc_id, sizeof(parse_res_t));
		pr->pr_alloc_id = Parse_alloc_id;
		pr->pr_block = Current_block;
		pr->pr_funcs = Funclist;
		pr->pr_block->bl_vars = push_var_definitions(pr->pr_block->bl_vars,
									Varlist);

		/*  Build a fil structure and point all the functions' fu_fil
		 *  pointer at it.
		 */

		fil = NEW(fil_t);

		funclist = NULL;
		for (f = pr->pr_funcs; f != Old_funclist; f = f->fu_next) {
			funclist_t *fl;

			f->fu_fil = fil;
			fl = NEW(funclist_t);
			fl->fl_func = f;
			fl->fl_prev = funclist;
			funclist = fl;
		}

		for (il = pr->pr_block->bl_initlist; il != Old_initlist;
								il = il->il_next) {
			il->il_fil = fil;
		}

		if (pr->pr_funcs != NULL)
			fil->fi_name = pr->pr_funcs->fu_lexinfo->lx_filename;
		else if (pr->pr_block->bl_vars != NULL)
			fil->fi_name = pr->pr_block->bl_vars->va_lexinfo->lx_filename;
		else
			fil->fi_name = "Unknown name";

		fil->fi_path_hint = NULL;
		fil->fi_language = LANG_C;
		fil->fi_flags = FI_DONE_VARS;
		fil->fi_stf = 0;
		fil->fi_so = NULL;
		fil->fi_editblocks_id = 0;
		fil->fi_block = Current_block;
		fil->fi_funclist_head = NULL;	/* for safety */
		fil->fi_funclist_tail = funclist;
		fil->fi_next = NULL;
	}
	else {
		alloc_free_pool(Parse_alloc_id);
		pr = NULL;
	}
	return pr;
}

static void
check_redeclaration(nd, v)
namedesc_t *nd;
var_t *v;
{
	nametype_t var_nametype;

	var_nametype = (v->va_type->ty_code == DT_FUNC_RETURNING) ? NT_FUNCNAME
								  : NT_VARNAME;

	if ((nd->nd_nametype == NT_VARNAME || nd->nd_nametype == NT_FUNCNAME) &&
	    (var_nametype == NT_VARNAME || var_nametype == NT_FUNCNAME) &&
	    (nd->nd_var->va_class == CL_DECL || v->va_class == CL_DECL)) {
		ci_complain_if_types_differ(v->va_name,
				    nd->nd_var->va_type, nd->nd_var->va_lexinfo,
				    v->va_type, v->va_lexinfo);
	}
	else {
		report_redecl(v->va_name,
			      NT_VARNAME, v->va_lexinfo,
			      nd->nd_nametype, nd->nd_lexinfo);
	}
}

void
ci_add_typedefs_to_current_block(declarations)
declaration_t *declarations;
{
	var_t *vlist, *v;
	initlist_t *initlist;

	ci_declarations_to_varlist(declarations, CL_TYPEDEF, &vlist, &initlist);
	ci_complain_about_any_initialisers(initlist, "typedef");

	for (v = vlist; v != NULL; v = v->va_next) {
		typedef_t *td;
		type_t *type;

		/*  We make a copy of the type because we are about to change
		 *  one of its fields (ty_typedef).
		 */
		type = ci_make_type(Parse_alloc_id, TY_NOTYPE);
		*type = *v->va_type;

		td = NEW(typedef_t);
		td->td_name = v->va_name;
		td->td_lexinfo = v->va_lexinfo;
		td->td_type = type;
		td->td_type->ty_typedef = td;
		td->td_next = Current_block->bl_typedefs;

		Current_block->bl_typedefs = td;
	}
}
