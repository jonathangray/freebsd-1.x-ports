/* ci_types.c - type checking etc for the C parser */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_ci_types_c_sccsid[] = "@(#)ci_types.c	1.13 20/5/92 (UKC)";

#include <string.h>

#include <stdlib.h>
#include <stdio.h>

#include <local/ukcprog.h>

#include "ups.h"
#include "symtab.h"
#include "ci.h"
#include "ci_parse.h"
#include "ci_util.h"
#include "ci_types.h"

static const char *nametype_to_string PROTO((nametype_t nametype));
static bool params_same PROTO((funcret_t *fr1, funcret_t *fr2));
static char *param_types PROTO((type_t *type));
static const char *qualifiers_to_string PROTO((qualifiers_t qualifiers));

#define CHAR_SIZE	sizeof(char)
#define SHORT_SIZE	sizeof(short)
#define INT_SIZE	sizeof(int)
#define LONG_SIZE	sizeof(long)
#define FLOAT_SIZE	sizeof(float)
#define DOUBLE_SIZE	sizeof(double)
#define ENUM_SIZE	sizeof(typecode_t)	/* or any enum */
#define POINTER_SIZE	sizeof(int *)

#define CHAR_ALIGN	CHAR_SIZE
#define SHORT_ALIGN	SHORT_SIZE
#define INT_ALIGN	INT_SIZE
#define LONG_ALIGN	LONG_SIZE
#define POINTER_ALIGN	POINTER_SIZE
#define FLOAT_ALIGN	FLOAT_SIZE
#define DOUBLE_ALIGN	FLOAT_SIZE	/* BUG: dubious */

#define WORD_ALIGN	LONG_ALIGN
#define MAX_ALIGN	DOUBLE_ALIGN

int
ci_type_alignment(type)
type_t *type;
{
	switch(type->ty_code) {
	case TY_U_ENUM:
	case TY_ENUM:
	case DT_PTR_TO:
		return WORD_ALIGN;
	case TY_STRUCT:
	case TY_UNION:
		return type->ty_aggr_or_enum->ae_alignment;
	case DT_ARRAY_OF:
		return ci_type_alignment(type->ty_base);
	case TY_LONG:
	case TY_ULONG:
		return LONG_ALIGN;
	case TY_INT:
	case TY_UINT:
	case TY_BITFIELD:
		return INT_ALIGN;
	case TY_SHORT:
	case TY_USHORT:
		return SHORT_ALIGN;
	case TY_CHAR:
	case TY_UCHAR:
		return CHAR_ALIGN;
	case TY_FLOAT:
		return FLOAT_ALIGN;
	case TY_DOUBLE:
		return DOUBLE_ALIGN;
	default:
		ci_panic("bad type in ta");
		return 0; /* to satisfy gcc */
	}
}

taddr_t
ci_align_addr(addr, alignment)
taddr_t addr;
int alignment;
{
	int pad;

	pad = alignment - addr % alignment;
	return (pad == alignment) ? addr : addr + pad;
}
	
taddr_t
ci_align_addr_for_type(addr, type)
taddr_t addr;
type_t *type;
{
	int alignment;

	alignment = (type != NULL) ? ci_type_alignment(type) : MAX_ALIGN;
	return ci_align_addr(addr, alignment);
}

void
ci_fix_signed_and_unsigned(declaration)
declaration_t *declaration;
{
	if (declaration->dn_basetype != NULL) {
		if (declaration->dn_basetype->ty_code == TY_UNSIGNED)
			declaration->dn_basetype = ci_code_to_type(TY_UINT);
		else if (declaration->dn_basetype->ty_code == TY_SIGNED)
			declaration->dn_basetype = ci_code_to_type(TY_INT);
	}
}

void
ci_add_type_specifier(declaration, oldtype)
declaration_t *declaration;
type_t *oldtype;
{
	typecode_t oldcode, newcode;
	type_t *newtype, *type;

	newtype = declaration->dn_basetype;
	oldcode = oldtype->ty_code;
	newcode = newtype->ty_code;

	if ((oldcode == TY_SHORT || oldcode == TY_LONG) && newcode == TY_INT)
		type = oldtype;
	else if (oldcode == TY_LONG && newcode == TY_DOUBLE)
		type = newtype;
	else if (oldcode == TY_UNSIGNED || oldcode == TY_SIGNED) {
		typecode_t ucode;

		switch (newcode) {
			case TY_CHAR:	ucode = TY_UCHAR;	break;
			case TY_SHORT:	ucode = TY_USHORT;	break;
			case TY_INT:	ucode = TY_UINT;	break;
			case TY_LONG:	ucode = TY_ULONG;	break;
			default:	ucode = TY_NOTYPE;	break;
		}
		if (ucode != TY_NOTYPE) {
			if (oldcode == TY_UNSIGNED)
				type = ci_code_to_type(ucode);
			else
				type = newtype;
		}
		else
			type = NULL;
	}
	else
		type = NULL;

	if (type != NULL)
		declaration->dn_basetype = type;
	else {
		char *oldtypestr, *newtypestr;

		oldtypestr = ci_type_to_english(oldtype, FALSE);
		newtypestr = ci_type_to_english(newtype, FALSE);
		diagf(ET_ERROR, (lexinfo_t *)NULL,
		      "`%s' and `%s' in the same declaration",
							oldtypestr, newtypestr);
		free(oldtypestr);
		free(newtypestr);
	}
}

/*  Return the size in bytes of type.
 */
long
ci_typesize(lx, type)
lexinfo_t *lx;
type_t *type;
{
	if (type->ty_size == -1) {
		taddr_t size;

		switch(type->ty_code) {
		case DT_PTR_TO:
			size = POINTER_SIZE;
			break;
		case DT_ARRAY_OF:
			size = type->ty_dim->di_high *
						ci_typesize(lx, type->ty_base);
			break;
		case DT_FUNC_RETURNING:
			diagf(ET_ERROR, lx, "Sizeof <function> undefined");
			size = -1;
			break;
		case TY_VOID:
			diagf(ET_ERROR, lx, "Sizeof void undefined");
			size = -1;
			break;
		case TY_U_STRUCT:
		case TY_U_UNION:
			diagf(ET_ERROR, lx,
			      "Sizeof <%s> undefined", ci_basetype_name(type));
			size = -1;
			break;
		case TY_STRUCT:
		case TY_UNION:
			size = type->ty_aggr_or_enum->ae_size;
			break;
		case TY_U_ENUM:
		case TY_ENUM:
			size = ENUM_SIZE;
			break;
		case TY_LONG:
		case TY_ULONG:
			size = LONG_SIZE;
			break;
		case TY_INT:
		case TY_UINT:
		case TY_BITFIELD:
			size = INT_SIZE;
			break;
		case TY_SHORT:
		case TY_USHORT:
			size = SHORT_SIZE;
			break;
		case TY_CHAR:
		case TY_UCHAR:
			size = CHAR_SIZE;
			break;
		case TY_FLOAT:
			size = FLOAT_SIZE;
			break;
		case TY_DOUBLE:
			size = DOUBLE_SIZE;
			break;
		default:
			ci_panic("unknown typecode in ts");
			size = -1; /* to satisfy gcc */
		}
		type->ty_size = size;
	}
	return type->ty_size;
}

static const char *
nametype_to_string(nametype)
nametype_t nametype;
{
	switch(nametype) {
	case NT_VARNAME:
		return "variable";
	case NT_TAG:
		return "tag";
	case NT_ENUM_CONST:
		return "enum constant";
	case NT_TYPEDEF_NAME:
		return "typedef name";
	case NT_FUNCNAME:
		return "function name";
	default:
		return "<internal error - unknown type>";
	}
}

void
report_redecl(name, nametype, lx, prev_nametype, prev_lx)
const char *name;
nametype_t nametype;
lexinfo_t *lx;
nametype_t prev_nametype;
lexinfo_t *prev_lx;
{
	const char *nametype_str, *prev_nametype_str;
	char buf[50];

	nametype_str = nametype_to_string(nametype);
	if (nametype == prev_nametype)
		prev_nametype_str = "";
	else {
		(void) sprintf(buf, " as a %s", nametype_to_string(prev_nametype));
		prev_nametype_str = buf;
	}
	
	diagf(ET_ERROR, lx, "%s %s already declared%s at %s",
					nametype_str, name,
			     		prev_nametype_str,
					ci_lexinfo_to_string(prev_lx, lx));
}

static bool
params_same(fr1, fr2)
funcret_t *fr1, *fr2;
{
	var_t *v1, *v2;

	if (fr1->fr_is_old_style || fr2->fr_is_old_style)
		return TRUE;
	
	if (fr1->fr_nparams != fr2->fr_nparams)
		return FALSE;
	
	for (v1 = fr1->fr_params, v2 = fr2->fr_params; v1 != NULL;
					v1 = v1->va_next, v2 = v2->va_next)
		if (!ci_types_same(v1->va_type, v2->va_type))
			return FALSE;
	return TRUE;
}

bool
ci_types_same(t1, t2)
type_t *t1, *t2;
{
	for (; t1 != NULL && t2 != NULL; t1 = t1->ty_base, t2 = t2->ty_base) {

		if (t1->ty_code != t2->ty_code)
			return FALSE;
		switch (t1->ty_code) {
		case DT_PTR_TO:
			if (t1->ty_qualifiers != t2->ty_qualifiers)
				return FALSE;
			break;
		case DT_FUNC_RETURNING:
			if (!params_same(t1->ty_funcret, t2->ty_funcret))
				return FALSE;
			break;
		case DT_ARRAY_OF:
			{
				dim_t *d1 = t1->ty_dim;
				dim_t *d2 = t2->ty_dim;

				if (!ci_types_same(d1->di_type, d2->di_type))
					return FALSE;

				if (!d1->di_ldynamic && !d2->di_ldynamic &&
							d1->di_low != d2->di_low)
					return FALSE;
				
				if (!d1->di_hdynamic && !d2->di_hdynamic &&
						    d1->di_high != d2->di_high)
					return FALSE;
				
				return TRUE;
			}
			break;
		case TY_STRUCT: case TY_U_STRUCT:
		case TY_UNION:	case TY_U_UNION:
		case TY_ENUM:	case TY_U_ENUM:
			if (t1->ty_aggr_or_enum != t2->ty_aggr_or_enum)
				return FALSE;
			break;
		case TY_VOID:
		case TY_CHAR:	case TY_UCHAR:
		case TY_SHORT:	case TY_USHORT:
		case TY_INT:	case TY_UINT:
		case TY_LONG:	case TY_ULONG:
		case TY_FLOAT:	case TY_DOUBLE:
			break;
		default:
			ci_panic("bad type in ts");
		}
	}

	return t1 == NULL && t2 == NULL;
}

void
ci_show_type(type, what)
type_t *type;
const char *what;
{
	char *typestr;

	typestr = ci_type_to_english(type, FALSE);
	diagf(ET_MORE, (lexinfo_t *)NULL, "%s: %s", what, typestr);
	free(typestr);
}

bool
ci_is_signed_type(typecode)
typecode_t typecode;
{
	switch (typecode) {
	case TY_CHAR:
	case TY_SHORT:
	case TY_INT:
	case TY_LONG:
	case TY_ENUM:
	case TY_FLOAT:
	case TY_DOUBLE:
		return TRUE;
	case TY_UCHAR:
	case TY_USHORT:
	case TY_UINT:
	case TY_ULONG:
	case DT_PTR_TO:
	case DT_ARRAY_OF:
		return FALSE;
	default:
		ci_panic("bad type in ist");
		return FALSE;	/* to satisfy gcc */
	}
}

bool
ci_is_integral(typecode)
typecode_t typecode;
{
	switch(typecode) {
	case TY_CHAR:
	case TY_UCHAR:
	case TY_SHORT:
	case TY_USHORT:
	case TY_INT:
	case TY_UINT:
	case TY_LONG:
	case TY_ULONG:
	case TY_BITFIELD:
		return TRUE;
	default:
		return FALSE;
	}
}

const char *
ci_basetype_name(type)
type_t *type;
{
	static char buf[128];
	const char *tagtype, *tag;

	if (type->ty_base != NULL)
		ci_panic("ci_basetype_name called on derived type");

	switch(type->ty_code) {
	case TY_U_STRUCT:
		tagtype = "undefined struct";
		break;
	case TY_STRUCT:
		tagtype = "struct";	
		break;

	case TY_UNION:
		tagtype = "union";	
		break;
	case TY_U_UNION:
		tagtype = "undefined union";
		break;

	case TY_ENUM:
		tagtype = "enum";
		break;
	case TY_U_ENUM:
		tagtype = "undefined enum";
		break;
	
	case TY_BITFIELD:
		return "bitfield";

	default:
		return type->ty_name;	
	}

	if ((tag = type->ty_aggr_or_enum->ae_tag) != NULL)
		(void) sprintf(buf, "%s %s", tagtype, tag);
	else
		(void) sprintf(buf, "(unnamed) %s", tagtype);
	return buf;
}

static char *
param_types(type)
type_t *type;
{
	funcret_t *fr;
	var_t *v;
	char *str, *new, *cptr;

	if (type->ty_code != DT_FUNC_RETURNING)
		ci_panic("bad type in pt");
	fr = type->ty_funcret;
	
	if (fr->fr_is_old_style)
		return strsave("()");

	if (fr->fr_nparams == 0)
		return strsave("(void)");

	str = strsave("");
	for (v = fr->fr_params; v != NULL; v = v->va_next) {
		cptr = ci_type_to_decl(v->va_type, FALSE);
		new = strf("%s, %s", str, cptr);
		free(cptr);
		free(str);
		str = new;
	}
	new = strf("(%s)", str + 2);
	free(str);
	return new;
}

static const char *
qualifiers_to_string(qualifiers)
qualifiers_t qualifiers;
{
	static char buf[50];

	strnf(buf, sizeof(buf), "%s%s",
			 (qualifiers & QU_CONST) ?    "const "    : "",
			 (qualifiers & QU_VOLATILE) ? "volatile " : "");
	return buf;
}

char *
ci_type_to_decl(type, resolve_typedefs)
type_t *type;
bool resolve_typedefs;
{
	char *str, *new, *cptr;
	bool quit, last_was_ptr;
	
	str = strsave("");
	quit = last_was_ptr = FALSE;
	for (;;) {
		const char *asizestr;

		if (!resolve_typedefs && type->ty_typedef != NULL)
			break;

		switch(type->ty_code) {
		case DT_PTR_TO:
			new = strf("*%s%s",
					qualifiers_to_string(type->ty_qualifiers),
					str);
			break;
		case DT_FUNC_RETURNING:
			cptr = param_types(type);
			new = strf(last_was_ptr ? "(%s)%s" : "%s%s", str, cptr);
			free(cptr);
			break;
		case DT_ARRAY_OF:
			if (type->ty_dim->di_hdynamic)
				asizestr = "";
			else{
				static char buf[30];

				strnf(buf, sizeof(buf), "%d", 
							type->ty_dim->di_high);
				asizestr = buf;
			}
			new = strf(last_was_ptr ? "(%s)[%s]" : "%s[%s]",
							        str, asizestr);
			break;
		default:
			quit = TRUE;
			new = NULL;	/* to satisfy gcc */
			break;
		}
		if (quit)
			break;

		free(str);
		str = new;

		last_was_ptr = type->ty_code == DT_PTR_TO;
		type = type->ty_base;
	}

	if (type->ty_code == TY_BITFIELD)
		new = strf("int %s:%d", str, type->ty_bitfield->bf_width);
	else {
		const char *name;

		if (!resolve_typedefs && type->ty_typedef != NULL)
			name = type->ty_typedef->td_name;
		else
			name = ci_basetype_name(type);
		new = strf("%s%s%s", name, *str == '\0' ? "" : " ", str);
	}
	free(str);
	return new;
}

char *
ci_type_to_english(type, resolve_typedefs)
type_t *type;
bool resolve_typedefs;
{
	bool quitloop;
	const char *space;
	char *str, *cptr, *params_str;

	str = strsave("");
	space = "";		/* no space the first time round the loop */

	for (quitloop = FALSE; !quitloop && type != NULL; type = type->ty_base){
		char *new;

		if (!resolve_typedefs && type->ty_typedef != NULL) {
			new = strf("%s%s%s", str, space,
						type->ty_typedef->td_name);
			quitloop = TRUE;
		}
		else {
			switch (type->ty_code) {
			case DT_PTR_TO:
				new = strf("%s%s%spointer to", str, space,
					 qualifiers_to_string(type->ty_qualifiers));
				break;
			case DT_FUNC_RETURNING:
				cptr = param_types(type);
				params_str = (strcmp(cptr, "()") == 0) ? "" : cptr;
				new = strf("%s%sfunction%s returning",
							str, space, params_str);
				free(cptr);
				break;
			case DT_ARRAY_OF:
				if (type->ty_dim->di_hdynamic)
					new = strf("%s%sarray[(unspecified size)] of",
							str, space);
				else
					new = strf("%s%sarray[%d] of",
							str, space,
							type->ty_dim->di_high);
				break;
			default:
				new = strf("%s%s%s", str, space,
							ci_basetype_name(type));
				break;
			}
		}
		free(str);
		str = new;
		space = " ";
	}
	return str;
}

void
ci_complain_if_types_differ(name, prev_type, prev_lexinfo, type, lexinfo)
const char *name;
type_t *prev_type;
lexinfo_t *prev_lexinfo;
type_t *type;
lexinfo_t *lexinfo;
{
	if (ci_types_same(prev_type, type))
		return;

	diagf(ET_ERROR, lexinfo,
	      "Declaration of `%s' conflicts with previous declaration at %s",
	      name, ci_lexinfo_to_string(prev_lexinfo, lexinfo));
	diagf(ET_MORE, (lexinfo_t *)NULL,
				"Old: %s", ci_type_to_english(prev_type, TRUE));
	diagf(ET_MORE, (lexinfo_t *)NULL,
				"New: %s", ci_type_to_english(type, TRUE));
}

bool
ci_complain_about_any_void_types(vlist)
var_t *vlist;
{
	bool complained;
	var_t *v;

	complained = FALSE;
	for (v = vlist; v != NULL; v = v->va_next) {
		typecode_t prevcode;
		type_t *type;

		prevcode = TY_NOTYPE;
		for (type = v->va_type; type->ty_base != NULL; type = type->ty_base)
			prevcode = type->ty_code;
		if (type->ty_code == TY_VOID && prevcode != DT_PTR_TO &&
						prevcode != DT_FUNC_RETURNING) {
			if (v->va_name != NULL)
				diagf(ET_ERROR, v->va_lexinfo,
					"Illegal type \"%s\" for `%s'",
					ci_type_to_english(v->va_type, TRUE),
					v->va_name);
			else
				diagf(ET_ERROR, v->va_lexinfo,
					"Illegal type \"%s\"",
					ci_type_to_english(v->va_type, TRUE));
			complained = TRUE;
		}
	}
	return complained;
}
