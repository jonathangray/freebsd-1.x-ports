/* va_edit.c - editing of variable declarations */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_va_edit_c_sccsid[] = "@(#)va_edit.c	1.19 15/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include <local/ukcprog.h>
#include <local/wn.h>
#include <local/obj/obj.h>
#include <local/obj/fed.h>

#include "ups.h"
#include "symtab.h"
#include "objtypes.h"
#include "obj_buildf.h"
#include "obj_target.h"
#include "state.h"
#include "va.h"
#include "ci.h"
#include "data.h"
#include "va_priv.h"
#include "ui.h"
#include "tdr.h"

char Var_format[] = "%50cv%[-]30cV\n";

fnamemap_t Var_fnamemap[] = {
	"subscript",	FN_VAR_DECL,
	"value",	FN_VAR_VALUE,
	NULL,		0,
};

static void var_draw PROTO((draw_t *p_dets));
static void var_puckfunc PROTO((edesc_t *edesc));
static void var_keyfunc PROTO((edesc_t *edesc));
static int var_quitfunc PROTO((edesc_t *edesc, int n_tries));
static int value_quitfunc PROTO((edesc_t *edesc, int n_tries));
static void var_edit PROTO((draw_t dets));
static void value_edit PROTO((draw_t dets));
static const char *start_editable PROTO((dvar_t *dv, const char *s));
static void update_ilist PROTO((dvar_t *dv, const char *decl));
static int get_value PROTO((type_t *type, const char *vstr,
					value_t *vl, vformat_t *p_vformat));
static int get_integer PROTO((const char *vstr, unsigned long *p_val,
			      vformat_t *p_vformat));
static enum_member_t *find_enum_member PROTO((aggr_or_enum_def_t *ae, const char *name));
static int get_float PROTO((const char *vstr, double *p_dval));
static void set_fieldval PROTO((int *p_word, int value, bitfield_t *bf));
static int get_c_string PROTO((const char *vstr, char **p_s, int *p_len));
static int get_new_string PROTO((const char *orig, const char *vstr,
				 dvar_t *dv, char **p_s, int *p_len));
static int write_new_value PROTO((taddr_t addr, type_t *type, value_t value,
				  const char *vstr, bool *p_done_mesg));
static int check_addr_for_edit PROTO((dvar_t *dv));
static int index_offset PROTO((dvar_t *dv));
static int get_hex_float PROTO((const char *vstr, bool is_float, int *p_ints));

fdef_t Var_fdefs[] = {
	'v', var_draw, var_edit, var_or_expr_getwidth,
	'V', s_draw, value_edit, NULL,
	'\0', NULL, NULL, NULL,
};

#define MAX_DECL_LEN	256

int
get_decl_len(len)
int len;
{
	static int lentab[] = { 35, 50, 65 };
	int i;

	for (i = 0; i < sizeof lentab / sizeof *lentab; ++i)
		if (lentab[i] > len)
			return lentab[i];
	return len + 1;
}

static void
var_draw(dets)
draw_t *dets;
{
	static int want_ul = -1;
	static char uldef[] = "UnderlineVars";
	const char *def, *s;
	int x1, x2, y;
	dvar_t *dv;

	s = (const char *)dets->dr_fval;
	wn_ttext(dets->dr_wn, s, dets->dr_x, dets->dr_y, dets->dr_fg, dets->dr_bg);

	if (want_ul == -1)
		want_ul = (def = wn_get_default(uldef)) != NULL &&
							strcmp(def, "on") == 0;
	if (want_ul) {
		dv = (dvar_t *) dets->dr_code;
		x1 = dets->dr_x + wn_strnwidth(s, dv->dv_ul_start, (font_t *)NULL);
		x2 = x1 + wn_strwidth(dv->dv_var->va_name, (font_t *)NULL);
		y = dets->dr_y + wn_get_sysfont()->ft_height - 2;
		wn_draw_line(dets->dr_wn, x1, y, x2, y, dets->dr_fg);
	}

	dets->dr_width = get_decl_len(strlen(s)) * wn_get_sysfont()->ft_width;
}

/*  Return the width of the declaration field.  This is the getwidth function
 *  for the %v format character.
 */
/* ARGSUSED */
int
var_or_expr_getwidth(obj, fnum, fval)
objid_t obj;
int fnum;
fval_t fval;
{
	return get_decl_len(strlen((const char *)fval)) * wn_get_sysfont()->ft_width;
}

/*  Mouse input handler for var declarations.
 *  If the user has pointed at a character with the mouse, force
 *  the marker bar to be within an index number.
 */
static void
var_puckfunc(edesc)
edesc_t *edesc;
{
	language_t language;
	char *pos, *min, *max, *lo, *hi;

	if (edesc->ed_meaning != EDM_SETCURSOR)
		return;
	
	language = ((dvar_t *)edesc->ed_user)->dv_var->va_language;

	/*  First force the marker bar to be in the editable part of the
	 *  declaration.
	 */
	switch (language) {
	case LANG_C:
		min = strchr(edesc->ed_copy, C_AOPEN);
		max = (min == NULL) ? NULL : strchr(min, C_ACLOSE);
		break;
	case LANG_FORTRAN:
		min = strrchr(edesc->ed_copy, FORTRAN_AOPEN);
		max = strrchr(edesc->ed_copy, FORTRAN_ACLOSE);
		break;
	default:
		panic("bad language in var_puckfunc");
		min = max = 0; /* to satisfy gcc */
	}
	if (min == NULL || max == NULL)
		panic("missing character in declaration");

	pos = edesc->ed_copy + edesc->ed_newpos;
	if (pos <= min)
		pos = min + 1;
	if (pos > max)
		pos = max;

	if (language == LANG_C) {
		/*  Find the nearest LBRAC or RBRAC in either direction.
		 */
		for (lo = pos - 1; lo > min; --lo)
			if (IS_LBRAC(*lo) || IS_RBRAC(*lo))
				break;
		for (hi = pos; hi < max; hi++)
			if (IS_LBRAC(*hi) || IS_RBRAC(*hi))
				break;
		
		/*  Force pos to lie between a LBRAC and a RBRAC
		 */
		if (lo <= min && IS_LBRAC(*hi))
			pos = hi + 1;
		else if (IS_RBRAC(*lo) && hi >= max)
			pos = lo;
		else if (IS_RBRAC(*lo) && IS_LBRAC(*hi))
			pos = (hi - pos > pos - lo) ? lo -1 : hi + 1;
		else if (!(IS_LBRAC(*lo) && IS_RBRAC(*hi)))
			panic("bad declaration syntax");
	}
	
	edesc->ed_newpos = pos - edesc->ed_copy;
}

static const char *
start_editable(dv, s)
dvar_t *dv;
const char *s;
{
	const char *pos;

	switch(dv->dv_var->va_language) {
	case LANG_C:
		pos = strchr(s, C_AOPEN);
		break;
	case LANG_FORTRAN:
		pos = strchr(s, FORTRAN_AOPEN);
		break;
	default:
		panic("bad language in var_quitfunc");
		pos = 0; /* to satisfy gcc */
	}
	return pos;
}

void
adjust_index(edesc, up)
edesc_t *edesc;
bool up;
{
	char buf[MAX_DECL_LEN];
	int val, level, inc, dig_offset;
	const char *pos, *minpos, *maxpos, *cptr;
	dvar_t *dv;
	ilist_t *ilist;

	td_record_adjust_index("subscript", edesc->ed_curpos, up);

	pos = edesc->ed_copy + edesc->ed_curpos;

	/*  What power of ten should we adjust by?
	 */
	inc = 1;
	for (cptr = pos; isdigit(*cptr); ++cptr)
		inc *= 10;
	dig_offset = cptr - pos;

	/*  Find the start of the number.
	 */
	for (--pos; isdigit(*pos) || *pos == '-'; --pos)
		;
	if (!IS_LBRAC(*pos))
		panic("bad decl in ai");

	if (!up)
		inc = -inc;

	val = atoi(pos + 1) + inc;
	dv = (dvar_t *) edesc->ed_user;
	ilist = dv->dv_ilist;
	level = 0;

	/*  First update the ilist so we don't lose existing edits
	 *  to other indices.
	 */
	update_ilist(dv, edesc->ed_copy);

	switch (dv->dv_var->va_language) {
	case LANG_C:
		minpos = start_editable(dv, edesc->ed_copy);
		for (--pos; pos > minpos; --pos) {
			if (IS_LBRAC(*pos)) {	
				level++;
				ilist = ilist->il_next;
			}
		}
		ilist->il_index = val;
		(void) strcpy(buf, mkdecl(dv));

		pos = start_editable(dv, buf);
		for (;;) {
			if (IS_LBRAC(*pos))
				if (--level < 0)
					break;
			pos++;
		}
		while (!IS_RBRAC(*pos))
			pos++;
		break;

	case LANG_FORTRAN:
		maxpos = edesc->ed_copy + strlen(edesc->ed_copy) - 1;
		for (cptr = maxpos; cptr > pos; --cptr) {
			if (*cptr == ',') {
				++level;
				ilist = ilist->il_next;
			}
		}
		ilist->il_index = val;
		(void) strcpy(buf, mkdecl(dv));

		for (pos = buf + strlen(buf) - 1; ; --pos)
			if (*pos == ',' || *pos == FORTRAN_ACLOSE)
				if (--level < 0)
					break;
		break;

	default:
		panic("bad language in adjust_index");
	}

	while (pos > buf && isdigit(pos[-1]) && dig_offset > 0) {
		--dig_offset;
		--pos;
	}

	edesc->ed_curpos = pos - buf;
	ed_newstring(edesc, buf);

	if (edesc->ed_wn != -1) {
		wn_updating_off(edesc->ed_wn);
		ed_cursor(edesc, EDC_CURSOR_OFF);
	}

	change_field((objid_t)edesc->ed_user, FN_VAR_DECL, edesc->ed_copy);
	ed_new_orig(edesc, get_field_value((objid_t)edesc->ed_user,
								FN_VAR_DECL));
	update_var((objid_t)edesc->ed_user, TRUE);

	if (edesc->ed_wn != -1) {
		ed_cursor(edesc, EDC_CURSOR_ON);
		wn_updating_on(edesc->ed_wn);
	}
}

static void
var_keyfunc(edesc)
edesc_t *edesc;
{
	char *s;

	if (edesc->ed_copy == edesc->ed_orig)
		panic("edesc botch");
	s = edesc->ed_copy + edesc->ed_curpos;
	switch(edesc->ed_meaning) {
	case EDM_DELETE_LINE:
		wn_updating_off(edesc->ed_wn);
		while(!IS_LBRAC(edesc->ed_copy[edesc->ed_curpos - 1]))
			do_edit(edesc, EDM_DELETE_CHAR);
		wn_updating_on(edesc->ed_wn);
		edesc->ed_meaning = EDM_CONT;
	case EDM_DELETE_CHAR:
	case EDM_BACK_SPACE:
		if (IS_LBRAC(s[-1])) {
			wn_bell(edesc->ed_wn);
			edesc->ed_meaning = EDM_CONT;
		}
		break;
	case EDM_FORWARD_SPACE:
		if (IS_RBRAC(*s)) {
			wn_bell(edesc->ed_wn);
			edesc->ed_meaning = EDM_CONT;
		}
		break;
	case EDM_INSERT_CHAR:
	case EDM_ILLEGAL_CHAR:
		switch(edesc->ed_char) {
		case '>':
		case 'j':
		case CONTROL('J'):
			adjust_index(edesc, TRUE);
			edesc->ed_meaning = EDM_CONT;
			break;
		case '<':
		case 'k':
		case CONTROL('K'):
			adjust_index(edesc, FALSE);
			edesc->ed_meaning = EDM_CONT;
			break;
		case '-':
			if (!IS_LBRAC(s[-1]) || (!isdigit(*s) && !IS_RBRAC(*s)))
				edesc->ed_meaning = EDM_ILLEGAL_CHAR;
			break;
		default:
			if (!isdigit(edesc->ed_char) || *s == '-')
				edesc->ed_meaning = EDM_ILLEGAL_CHAR;
			break;
		}
		break;
	}
	if (edesc->ed_copy == edesc->ed_orig)
		panic("edesc botch");
}

static void
update_ilist(dv, decl)
dvar_t *dv;
const char *decl;
{
	const char *cptr;
	ilist_t *ilist;
	int res, neg, ch, level;

	ilist = dv->dv_ilist;
	switch(dv->dv_var->va_language) {
	case LANG_C:
		level = 0;
		cptr = decl;
		while (*cptr != '\0') {
			ch = *cptr++;
			if (IS_LBRAC(ch)) {
				res = 0;
				neg = *cptr == '-';
				if (neg)
					cptr++;
				while (isdigit(*cptr))
					res = res * 10 + *cptr++ - '0';
				if (neg)
					res = -res;
				if (!IS_RBRAC(*cptr))
					panic("bad decl syntax in ui");
				ilist->il_index = res;
				ilist = ilist->il_next;
				level++;
			}
		}
		if (level != dv->dv_ilevel)
			panic("wrong # indices in ui");
		break;
	case LANG_FORTRAN:
		cptr = decl + strlen(decl) - 1;
		do {
			--cptr;
			while (*cptr != ',' && *cptr != FORTRAN_AOPEN)
				--cptr;
			ilist->il_index = atoi(cptr + 1);
			ilist = ilist->il_next;
		} while (*cptr != FORTRAN_AOPEN);
		break;
	default:
		panic("bad language in ui");
	}
}

static int
var_quitfunc(edesc, n_tries)
edesc_t *edesc;
int n_tries;
{
	dvar_t *dv;
	const char *old, *new;

	if (edesc->ed_copy == edesc->ed_orig)
		panic("edesc botch");

	if (strcmp(edesc->ed_orig, edesc->ed_copy) == 0)
		return EDR_CONFIRM_NO_CHANGE;

	dv = (dvar_t *) edesc->ed_user;

	update_ilist(dv, edesc->ed_copy);

	if (edesc->ed_wn != -1)
		wn_updating_off(edesc->ed_wn);

	old = (const char *)get_field_value((objid_t)dv, FN_VAR_DECL);
	new = mkdecl(dv);
	if (strcmp(old, new) != 0) {
		set_field_value((objid_t)dv, FN_VAR_DECL, (fval_t)strsave(new));
		free((char *)old);
	}
	else if (strcmp(edesc->ed_copy, new) != 0)
		obj_has_changed((objid_t)dv);
	update_var((objid_t)dv, TRUE);

	if (edesc->ed_wn != -1)
		wn_updating_on(edesc->ed_wn);

	return EDR_CONFIRM_CHANGE;
}

void
get_value_colors(val_changed, p_fg, p_bg)
bool val_changed;
short *p_fg, *p_bg;
{
	static color_t colors[] = {
		-1, 0, 0, 0,
		-1, 0xffff, 0xffff, 0
	};
	int pixel, planes;

	if (colors[0].co_pixel == -1) {
		const char *colorname;

		if ((colorname = wn_get_default("HighlightColor")) != NULL)
			wn_parse_color(colorname, &colors[1]);

		colors[0].co_pixel = WN_BG;
		wn_get_pixel_colors(colors, 1);

		if (wn_get_nplanes() > 1 &&
		    wn_get_pixels_and_planes(1, 1, TRUE, &pixel, &planes) == 0) {
			colors[0].co_pixel = pixel;
			colors[1].co_pixel = pixel | planes;
			wn_set_pixel_colors(colors, 2);
		}
		else {
			colors[0].co_pixel = WN_BG;
			colors[1].co_pixel = WN_FG;
		}
	}

	if (val_changed) {
		*p_bg = colors[0].co_pixel;
		*p_fg = colors[1].co_pixel;
	}
	else {
		*p_bg = WN_BG;
		*p_fg = WN_FG;
	}
}

void
var_getcolor(obj, p_fg, p_bg)
objid_t obj;
short *p_fg, *p_bg;
{
	bool val_changed;

	val_changed = (((dvar_t *)obj)->dv_flags & DVF_VAL_CHANGED) != 0;
	get_value_colors(val_changed, p_fg, p_bg);
}

static void
set_fieldval(p_word, value, bf)
int *p_word, value;
bitfield_t *bf;
{
	int shift;
	unsigned int mask;

#ifdef IS_BIG_ENDIAN
	shift = 32 - (bf->bf_offset + bf->bf_width);
#else
	shift = bf->bf_offset;
#endif

	mask = (~(unsigned)0 >> (32 - bf->bf_width)) << shift;
	value <<= shift;
	
	*p_word = (*p_word & ~mask) | (value & mask);
}

static int
index_offset(dv)
dvar_t *dv;
{
	int orig_index, level;
	taddr_t addr, base_addr;
	ilist_t *il;

	addr = dvar_addr(dv);

	il = dv->dv_ilist;
	for (level = 0; level < dv->dv_ilevel - 1; ++level) {
		il = il->il_next;
		if (il == NULL)
			panic("level botch in io");
	}
	
	if (!il->il_low_known)
		 panic("il botch in io");
		
	if (il->il_index == il->il_low)
		return 0;
	
	orig_index = il->il_index;

	il->il_index = il->il_low;
	base_addr = dvar_addr(dv);
	il->il_index = orig_index;

	return addr - base_addr;
}

static int
get_new_string(orig, vstr, dv, p_s, p_len)
const char *orig, *vstr;
dvar_t *dv;
char **p_s;
int *p_len;
{
	char *s;
	type_t *type, *btype;
	var_t *v;
	int len, oldlen;
	bool longer_ok;

	v = dv->dv_var;
	type = get_type_at_level(v, dv->dv_ilevel);

	oldlen = UNKNOWN_SIZE;

	longer_ok = vstr[0] == '>' && vstr[1] == '>' && vstr[2] == '"';

	if (longer_ok) {
		vstr += 2;
	}
	else {
		/*  If the variable is an array then we can find out how
		 *  many bytes are safe to store.
		 */
		if (dv->dv_ilevel > 0) {
			btype = get_type_at_level(v, dv->dv_ilevel - 1);

			if (btype->ty_code == DT_ARRAY_OF) {
				oldlen = typesize(btype) - index_offset(dv);
				if (oldlen < 0)
					oldlen = UNKNOWN_SIZE;
			}
		}
	
		if (oldlen == UNKNOWN_SIZE) {
			char *oldval;

			if (get_c_string(orig, &oldval, &oldlen) != 0)
				panic("old string botch");
			free(oldval);
		}
	}

	if (get_c_string(vstr, &s, &len) != 0)
		return -1;
	
	/*  If the `string' is contained in a register (i.e. an
	 *  integer being shown as a string) then we can't write
	 *  more that a register's worth back.
	 */
	if (v->va_class == CL_REG && type == v->va_type && len > sizeof(int)) {
		errf("Can't store more than %d bytes a register", sizeof(int));
		return -1;
	}

	if (len > oldlen && !longer_ok) {
		int xs;

		xs = len - oldlen;
		errf("String %d byte%s too long (use >>\"xxx\" to override)",
						     xs, (xs == 1) ? "" : "s");

		free(s);
		return -1;
	}

	*p_s = s;
	*p_len = len;
	return 0;
}

static int
write_new_value(addr, type, value, vstr, p_done_mesg)
taddr_t addr;
type_t *type;
value_t value;
const char *vstr;
bool *p_done_mesg;
{
#define DWRITE_VAR(addr, val)	dwrite(addr, (const char *)&val, sizeof(val))
	int res, word;

	*p_done_mesg = FALSE;

	switch (type->ty_code) {
	case TY_BITFIELD:
		if (dread(addr, (char *)&word, sizeof(int)) != 0) {
			errf("Error reading old bitfield value (%m)");
			*p_done_mesg = TRUE;
			res = -1;
			break;
		}
		set_fieldval(&word, value.vl_int, type->ty_bitfield);
		res = DWRITE_VAR(addr, word);
		break;
	case TY_ENUM:
	case TY_U_ENUM:
	case TY_INT:
	case TY_UINT:
		res = DWRITE_VAR(addr, value.vl_int);
		break;
	case TY_FLOAT:
		res = DWRITE_VAR(addr, value.vl_float);
		break;
	case TY_DOUBLE:
		res = DWRITE_VAR(addr, value.vl_double);
		break;
	case TY_SHORT:
	case TY_USHORT:
		res = DWRITE_VAR(addr, value.vl_short);
		break;
	case TY_CHAR:
	case TY_UCHAR:
		res = DWRITE_VAR(addr, value.vl_char);
		break;
	case DT_PTR_TO:
		res = DWRITE_VAR(addr, value.vl_addr);
		break;
	default:
		errf("Ignoring new value %s", vstr);
		*p_done_mesg = TRUE;
		res = -1;
	}

	return res;
}

static int
value_quitfunc(edesc, n_tries)
edesc_t *edesc;
int n_tries;
{
	dvar_t *dv;
	taddr_t addr;
	type_t *type;
	const char *vstr;
	value_t value;
	int res;
	bool done_mesg;
#ifdef IS_BIG_ENDIAN
	int size;
#endif

	if (strcmp(edesc->ed_orig, edesc->ed_copy) == 0)
		return EDR_CONFIRM_NO_CHANGE;

	dv = (dvar_t *) edesc->ed_user;
	type = get_type_at_level(dv->dv_var, dv->dv_ilevel);

	addr = dvar_addr(dv);
	if (addr == BAD_ADDR)
		panic("bad addr in vq");
	
#ifdef IS_BIG_ENDIAN
	size = typesize(type);
	if (dv->dv_var->va_class == CL_ARG && dv->dv_ilevel == 0 &&
							size < sizeof(int))
		addr += sizeof(int) - size;
#endif
	
	for (vstr = edesc->ed_copy; isspace(*vstr); ++vstr)
		;


	if (dv->dv_format == DF_STRING) {
		char *s;
		int len;

		if (get_new_string(edesc->ed_orig, vstr, dv, &s, &len) != 0)
			return force_quit(n_tries);

		res = dwrite(addr, s, len) != 0;
		done_mesg = FALSE;
		free(s);
	}
	else {
		if (get_value(type, vstr, &value, &dv->dv_format) != 0)
			return force_quit(n_tries);
		
		res = write_new_value(addr, type, value, vstr, &done_mesg);
	}

	if (res != 0) {
		if (!done_mesg)
			errf("Error writing new value to target address space (%m)");
		return EDR_CANCEL_AND_QUIT;
	}

	/*  This change may potentially affect any other variable
	 *  in the display (e.g. duplicates displays of this variable,
	 *  expressions using the value of this variable).
	 *  Thus we update all variable values.
	 */
	change_field((objid_t)dv, FN_VAR_VALUE, edesc->ed_copy);
	update_variable_values();

	return EDR_CONFIRM_CHANGE;
}

static enum_member_t *
find_enum_member(ae, name)
aggr_or_enum_def_t *ae;
const char *name;
{
	enum_member_t *em;

	for (em = ae->ae_enum_members; em != NULL; em = em->em_next) {
		if (strcmp(em->em_name, name) == 0)
			return em;
	}

	return NULL;
}

static int
get_c_string(vstr, p_s, p_len)
const char *vstr;
char **p_s;
int *p_len;
{
	char *obuf, *optr;

	if (*vstr++ != '"') {
		errf("Missing `\"' at start of string");
		return -1;
	}
		
	/*  We rely on the fact that all C escape sequences are longer
	 *  than the character they represent, so the real string is
	 *  never longer than the C representation.
	 */
	optr = obuf = e_malloc(strlen(vstr) + 1);

	for (; *vstr != '\0' && *vstr != '"'; ++vstr) {
		int ch;

		if (*vstr == '\\')
			vstr = ci_translate_escape(vstr + 1, &ch);
		else
			ch = *vstr;

		*optr++ = ch;
	}

	if (*vstr == '"') {
		*optr++ = '\0';

		if (vstr[1] != '\0') {
			errf("Extra characters after final `\"' in string");
			free(obuf);
			return -1;
		}
	}

	*p_s = obuf;
	*p_len = optr - obuf;
	return 0;
}

static int
get_value(type, vstr, vl, p_vformat)
type_t *type;
const char *vstr;
value_t *vl;
vformat_t *p_vformat;
{
	unsigned long val;
	double d;
	int res;

	res = 0;

	if (*vstr == '\0') {
		errf("Illegal zero length value");
		return -1;
	}

	switch (type->ty_code) {
	case TY_FLOAT:
		if (*vstr == '<')
			res = get_hex_float(vstr, TRUE, &vl->vl_int);
		else if (get_float(vstr, &d) == 0)
			vl->vl_float = d;
		else
			res = -1;
		break;
	case TY_DOUBLE:
		if (*vstr == '<')
			res = get_hex_float(vstr, FALSE, vl->vl_ints);
		else if (get_float(vstr, &d) == 0)
			vl->vl_double = d;
		else
			res = -1;
		break;

	case TY_INT:
	case TY_UINT:
	case TY_BITFIELD:
	case TY_U_ENUM:
		if (get_integer(vstr, &val, p_vformat) == 0)
			vl->vl_int = val;
		else
			res = -1;
		break;

	case TY_SHORT:
	case TY_USHORT:
		if (get_integer(vstr, &val, p_vformat) == 0)
			vl->vl_short = val;
		else
			res = -1;
		break;

	case TY_CHAR:
	case TY_UCHAR:
		if (get_integer(vstr, &val, p_vformat) == 0)
			vl->vl_char = val;
		else
			res = -1;
		break;
	case TY_ENUM:
		if (isalpha(*vstr) || *vstr == '_') {
			enum_member_t *em;

			em = find_enum_member(type->ty_aggr_or_enum, vstr);
			if (em != NULL)
				vl->vl_int = em->em_val;
			else {
				errf("`%s' is not a member of %s",
						vstr, ci_basetype_name(type));
				res = -1;
			}
		}
		else {
			if (get_integer(vstr, &val, p_vformat) == 0)
				vl->vl_int = val;
			else
				res = -1;
		}
		break;
	case DT_PTR_TO:
		if (type->ty_base->ty_code == DT_FUNC_RETURNING &&
					(isalpha(*vstr) || *vstr == '_')) {
			func_t *f;
			const char *pos;
			char *new;

			if ((pos = strchr(vstr, '(')) == NULL)
				new = NULL;
			else {
				new = strf("%.*s", pos - vstr, vstr);
				vstr = new;
			}

			if (find_func_by_name(vstr, &f) != 0)
				res = -1;
			else
				vl->vl_addr = f->fu_addr;

			if (new != NULL)
				free(new);
		}
		else {
			if (get_integer(vstr, &val, p_vformat) == 0)
				vl->vl_addr = val;
			else
				res = -1;
		}
		break;
	default:
		errf("Unknown type");
		res = -1;
	}

	return res;
}

static int
get_float(vstr, p_dval)
const char *vstr;
double *p_dval;
{
	char *ends;

	*p_dval = strtod(vstr, &ends);

	if (*ends != '\0' || ends == vstr) {
		errf("`%s' is not a floating point number", vstr);
		return -1;
	}

	return 0;
}

static int
get_hex_float(vstr, is_float, p_ints)
const char *vstr;
bool is_float;
int *p_ints;
{
#define HEX_FLOAT_NDIG		(sizeof(float) * 2)
	/*  See get_real().
	 */
	static const char badf[] = "illegal float ";
	static const char badd[] = "illegal double ";
	int ndig, maxdig, lsw, msw;
	char hexbuf[HEX_FLOAT_NDIG + 1];
	const char *vptr, *s, *what;

	vptr = vstr + 1;

	if (strncmp(vptr, badf, sizeof(badf) - 1) == 0)
		vptr += sizeof(badf) - 1;
	else if (strncmp(vptr, badd, sizeof(badd) - 1) == 0)
		vptr += sizeof(badd) - 1;
	
	if (strncmp(vptr, "0x", 2) == 0)
		vptr += 2;
	
	/*  Don't count leading zeroes.
	 */
	while (*vptr == '0' && isxdigit(vptr[1]))
		++vptr;

	for (s = vptr; isxdigit(*s); ++s)
		;
	ndig = s - vptr;
		
	if (*s != '>' || s[1] != '\0' || ndig < 1) {
		errf("`%s' is not a legal hex format floating point number",
									vstr);
		return -1;
	}

	if (is_float) {
		what = "float";
		maxdig = HEX_FLOAT_NDIG;
	}
	else {
		what = "double";
		maxdig = HEX_FLOAT_NDIG * 2;
	}

	if (ndig > maxdig) {
		errf("`%.*s' has %d digits - max for a %s is %d",
						ndig, vptr, ndig, what, maxdig);
		return -1;
	}

	if (ndig < HEX_FLOAT_NDIG) {
		strncpy(hexbuf, vptr, ndig);
		hexbuf[ndig] = '\0';
		lsw = strtol(hexbuf, (char **)NULL, 16);
		msw = 0;
	}
	else {
		memcpy(hexbuf, vptr + (ndig - HEX_FLOAT_NDIG), HEX_FLOAT_NDIG);
		hexbuf[HEX_FLOAT_NDIG] = '\0';
		lsw = strtol(hexbuf, (char **)NULL, 16);

		memcpy(hexbuf, vptr, ndig - HEX_FLOAT_NDIG);
		hexbuf[ndig - HEX_FLOAT_NDIG] = '\0';
		msw = strtol(hexbuf, (char **)NULL, 16);
	}

	if (is_float)
		p_ints[0] = lsw;
	else {
		p_ints[DOUBLE_MSW] = msw;
		p_ints[DOUBLE_LSW] = lsw;
	}

	return 0;
}

static int
get_integer(vstr, p_val, p_vformat)
const char *vstr;
unsigned long *p_val;
vformat_t *p_vformat;
{
	const char *s, *endpos;
	int radix;
	unsigned long val;
	vformat_t vformat;
	bool negative;

	s = vstr;

	negative = *s == '-';
	if (negative)
		++s;

	if (*s == '\'') {
		int res;

		++s;
		if (*s == '\\')
			s = ci_translate_escape(s + 1, &res) + 1;
		else
			res = *s++;

		if (*s != '\'') {
			errf("\"%s\" is not a valid character constant", vstr);
			return -1;
		}
		*p_val = negative ? -res : res;
		*p_vformat = DF_ASCII;
		return 0;
	}

	endpos = NULL;
	radix = 0;		/* to satisfy gcc */
	vformat = DF_SDEC;	/* to satisfy gcc */

	if (*s == '0' && s[1] != '\0') {
		++s;
		if (*s == 'x' || *s == 'X') {
			radix = 16;
			vformat = negative ? DF_SHEX : DF_UHEX;
			++s;
		}
		else if (*s == 'b' || *s == 'B') {
			radix = 2;
			vformat = DF_UBIN;
			++s;
		}
		else if (isdigit(*s)) {
			radix = 8;
			vformat = negative ? DF_SOCT : DF_UOCT;
			++s;
		}
		else
			endpos = s;
	}
	else if (isdigit(*s)) {
		radix = 10;
		vformat = negative ? DF_SDEC : DF_UDEC;
	}
	else
		endpos = s;

	val = 0;
	if (endpos == NULL) {
		for (; *s != '\0'; ++s) {
			int digit;

			/*  We allow spaces in binary format because we
			 *  use them on output, and we want the user to
			 *  be able to submit the output value as input.
			 */
			if (radix == 2 && isspace(*s))
				continue;
			if (!isalnum(*s))
				break;

			digit = *s - '0';

			if (strchr("abcdef", *s) != NULL)
				digit -= 'a' - ('9' + 1);
			if (strchr("ABCDEF", *s) != NULL)
				digit -= 'A' - ('9' + 1);
			
			if (digit < 0 || digit >= radix)
				break;
				
			val = val * radix + digit;
		}
		endpos = s;
	}
	
	if (*endpos != '\0') {
		errf("`%s' is not an integral value", vstr);
		return -1;
	}

	*p_vformat = vformat;
	*p_val = negative ? -val : val;
	
	return 0;
}

static int
check_addr_for_edit(dv)
dvar_t *dv;
{
	taddr_t addr;
	char c;

	addr = dvar_addr(dv);

	if (addr == 0) {
		errf("Can't edit target of NULL pointer");
		return -1;
	}

	if (addr == BAD_ADDR || dread(addr, &c, 1) != 0) {
		errf("Can't edit target of illegal address");
		return -1;
	}

	if (dwrite(addr, &c, 1) != 0) {
		errf("Can't edit value as it is in read-only memory");
		return -1;
	}

	return 0;
}

static void
value_edit(fdets)
draw_t fdets;
{
	dvar_t *dv;
	edesc_t edescbuf;
	type_t *type;
	bool can_edit;

	dv = (dvar_t *)fdets.dr_code;

	if (get_target_state() == TS_CORE) {
		errf("Can't edit variable values in a core file");
		wn_wait_for_release_of(fdets.dr_wn, B_ANY);
		return;
	}

	type = get_type_at_level(dv->dv_var, dv->dv_ilevel);
	switch(type->ty_code) {
	case TY_CHAR:
	case TY_UCHAR:
	case TY_SHORT:
	case TY_INT:
	case TY_LONG:
 	case TY_BITFIELD:
	case TY_USHORT:
	case TY_UINT:
	case TY_ULONG:
	case TY_ENUM:
	case TY_U_ENUM:
	case DT_PTR_TO:
	case TY_FLOAT:
	case TY_DOUBLE:
		can_edit = TRUE;
		break;
	default:
		errf("Can't edit value of variable %s", dv->dv_var->va_name);
		can_edit = FALSE;
		break;
	}

	if (!can_edit || check_addr_for_edit(dv) != 0) {
		wn_wait_for_release_of(fdets.dr_wn, B_ANY);
	}
	else {
		clear_selection();
		make_edesc(&edescbuf, fdets.dr_wn, (const char *)fdets.dr_fval,
				        MAX_DECL_LEN, fdets.dr_fg, fdets.dr_bg);
		edescbuf.ed_user = (int)dv;
		edescbuf.ed_quitfunc = (ed_quitfunc_t)value_quitfunc;
		suppress_ta_cursor_then_edit_field(&edescbuf, "value");
	}
}

static void
var_edit(fdets)
draw_t fdets;
{
	dvar_t *dv;
	edesc_t edescbuf;

	if (strlen((const char *)fdets.dr_fval) > MAX_DECL_LEN) {
		errf("Declaration is too long to edit");
		wn_wait_for_release_of(fdets.dr_wn, B_ANY);
		return;
	}

	dv = (dvar_t *) fdets.dr_code;
	if (dv->dv_ilevel == 0) {
		errf("No editable subscripts in the declaration you pointed at");
		wn_wait_for_release_of(fdets.dr_wn, B_ANY);
		return;
	}
	clear_selection();
	make_edesc(&edescbuf, fdets.dr_wn, (const char *)fdets.dr_fval,
							MAX_DECL_LEN,
							fdets.dr_fg, fdets.dr_bg);
	edescbuf.ed_user = (int)dv;
	edescbuf.ed_quitfunc = (ed_quitfunc_t)var_quitfunc;
	edescbuf.ed_puckfunc = (ed_puckfunc_t)var_puckfunc;
	edescbuf.ed_keyfunc = (ed_keyfunc_t)var_keyfunc;
	suppress_ta_cursor_then_edit_field(&edescbuf, "subscript");
}

objid_t
find_var(par, v)
objid_t par;
var_t *v;
{
	dvar_t *dv;

	dv = (dvar_t *) get_code(par, OBJ_CHILD);
	for (; dv != NULL; dv = (dvar_t *) get_code((objid_t)dv, OBJ_NEXT))
		if (dv->dv_var == v)
			return (objid_t)dv;
	return NULL;
}
