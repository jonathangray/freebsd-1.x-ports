/* printf.c - implementation of the ups $printf function */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_printf_c_sccsid[] = "@(#)printf.c	1.17 15/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include <local/ukcprog.h>

#include "ups.h"
#include "symtab.h"
#include "ci.h"
#include "proc.h"
#include "printf.h"
#include "output.h"
#include "exec.h"
#include "data.h"
#include "ui.h"
#include "state.h"
#include "tdr.h"

/*  Structures for get_arg
 */
typedef struct {
	type_t *v_type;
	value_t v_value;
} val_t;

typedef struct {
	taddr_t *a_vals;
	taddr_t *a_types;
	int a_type_index;
} args_t;

static int get_arg PROTO((args_t *args, val_t *val));
static const char *fmt_int PROTO((long val, int base,	
					bool is_signed, bool upper_case_hex));
static const char *get_string_from_proc PROTO((proc_t proc,
						taddr_t addr, int max_chars));
static const char *fmt_var PROTO((proc_t proc, code_id_t code_id,
				  outwin_id_t outwin_id, val_t *val,
				  int max_chars, bool show_aggrptr_members));
static const char *fmt_string PROTO((proc_t proc, code_id_t code_id,
						taddr_t arg, int max_chars));
static const char *enum_val_to_name PROTO((aggr_or_enum_def_t *ae, long val));
static bool is_string_type PROTO((type_t *type));
static int get_value PROTO((proc_t proc, code_id_t code_id,
					type_t *type, taddr_t addr,
					val_t *val, const char **p_mesg));
static void show_aggr PROTO((proc_t proc, code_id_t code_id, taddr_t addr,
					type_t *type, outwin_id_t outwin_id));

static int
get_arg(args, val)
args_t *args;
val_t *val;
{
	int nslots;

	if (args->a_vals >= args->a_types) {
		errf("Not enough arguments to $printf");
		return -1;
	}

	val->v_type = (type_t *)args->a_types[args->a_type_index];
	switch (val->v_type->ty_code) {
	case TY_DOUBLE:
		val->v_value.vl_double = *(double *)args->a_vals;
		nslots = sizeof(double) / sizeof(taddr_t);
		break;
	case TY_FLOAT:
		val->v_value.vl_double = *(float *)args->a_vals;
		nslots = sizeof(float) / sizeof(taddr_t);
		break;
	case TY_STRUCT:
	case TY_UNION:
		errf("Can't pass aggregate type to $printf (use `&')");
		return -1;
	default:
		val->v_value.vl_addr = *(taddr_t *)args->a_vals;
		nslots = 1;
	}

	args->a_vals += nslots;
	++args->a_type_index;

	return 0;
}

/*  Convert val to ascii format in base base. Base should be in the
 *  range 2 to 16 inclusive.
 */
static const char *
fmt_int(val, base, is_signed, upper_case_hex)
long val;
int base;
bool is_signed, upper_case_hex;
{
	static char nbuf[sizeof(long) * 8 + 1];
	char *s;
	const char *digits;
	unsigned uval;
	bool is_negative;
	
	digits = upper_case_hex ? "0123456789ABCDEF" : "0123456789abcdef";

	s = nbuf + sizeof(nbuf);

	is_negative = is_signed && val < 0;
	uval = is_negative ? -val : val;

	do {
		*--s = digits[uval % base];
		uval /= base;
	} while (uval != 0);

	if (is_negative)
		*--s = '-';

	return(s);
}

static const char *
get_string_from_proc(proc, addr, max_chars)
proc_t proc;
taddr_t addr;
int max_chars;
{
	char rbuf[16];	/* size of this determines proc_dread chunk size */
	static char *buf;
	static int bufsize = 0;
	int pos;

	if (bufsize == 0) {
		bufsize = 64;
		buf = e_malloc(bufsize);
	}

	if (addr == 0)
		return "<NULL>";

	pos = 0;

	/*  We set an arbitary limit on the length of a string that
	 *  will be read to avoid long waits if the address doesn't
	 *  point at a string.
	 */
	if (max_chars == -1)
		max_chars = 1024;

	for (;;) {
		if (proc_read_data(proc, addr + pos, rbuf, sizeof(rbuf)) != 0) {
			sprintf(buf, "<bad address 0x%x>", addr);
			break;
		}

		if (pos + sizeof(rbuf) > bufsize) {
			bufsize *= 2;
			buf = e_realloc(buf, bufsize);
		}
		memcpy(buf + pos, rbuf, sizeof(rbuf));

		pos += sizeof(rbuf);
		if (pos >= max_chars) {
			buf[max_chars] = '\0';
			break;
		}

		if (memchr(rbuf, '\0', sizeof(rbuf)) != NULL)
			break;
	}

	return buf;
}

static const char *
enum_val_to_name(ae, val)
aggr_or_enum_def_t *ae;
long val;
{
	static char *buf = NULL;

	if (ae != NULL) {
		enum_member_t *em;

		for (em = ae->ae_enum_members; em != NULL; em = em->em_next)
			if (em->em_val == val)
				return em->em_name;
	}

	if (buf != NULL)
		free(buf);
	buf = strf("(%s)%d", ci_basetype_name(ae->ae_type), val);
	return buf;
}

static const char *
fmt_var(proc, code_id, outwin_id, val, max_chars, show_aggrptr_members)
proc_t proc;
code_id_t code_id;
outwin_id_t outwin_id;
val_t *val;
int max_chars;
bool show_aggrptr_members;
{
	static char buf[50];
	const char *s;
	type_t *type;
	func_t *f;

	type = val->v_type;
	switch (type->ty_code) {
	case TY_ENUM:
		return enum_val_to_name(type->ty_aggr_or_enum,
						(long)val->v_value.vl_addr);

	case TY_CHAR:
		return fmt_int((long)val->v_value.vl_char, 10, TRUE, FALSE);
	case TY_UCHAR:
		return fmt_int((long)val->v_value.vl_uchar, 10, TRUE, FALSE);

	case TY_SHORT:
		return fmt_int((long)val->v_value.vl_short, 10, TRUE, FALSE);
	case TY_USHORT:
		return fmt_int((long)val->v_value.vl_ushort, 10, TRUE, FALSE);

	case TY_INT:
		return fmt_int((long)val->v_value.vl_int, 10, TRUE, FALSE);
	case TY_LONG:
		return fmt_int(val->v_value.vl_long, 10, TRUE, FALSE);

	case TY_UINT:
		return fmt_int((long)val->v_value.vl_uint, 10, FALSE, FALSE);
	case TY_ULONG:
		return fmt_int((long)val->v_value.vl_ulong, 10, FALSE, FALSE);
	
	case TY_FLOAT:
		return get_real(val->v_value, FALSE, TRUE);
	case TY_DOUBLE:
		return get_real(val->v_value, FALSE, FALSE);

	case DT_PTR_TO:
	case DT_ARRAY_OF:
		switch (type->ty_base->ty_code) {
		case TY_CHAR:
			s = fmt_string(proc, code_id, val->v_value.vl_addr, max_chars);

			if (show_aggrptr_members)
				return s;
			else {
				ow_putc(outwin_id, '"');
				ow_write(outwin_id, s, strlen(s));
				ow_putc(outwin_id, '"');
				return NULL;
			}

		case DT_FUNC_RETURNING:
			if ((f = addr_to_func(val->v_value.vl_addr)) != NULL)
				return f->fu_name;
			sprintf(buf, "(0x%x)()", val->v_value.vl_addr);
			return buf;

		case TY_STRUCT:
		case TY_UNION:
			if (show_aggrptr_members) {
				if (code_id != NULL &&
				     !ci_is_ci_addr(code_id,
						      val->v_value.vl_addr)) {
					code_id = NULL;
				}
				show_aggr(proc, code_id, val->v_value.vl_addr,
						type->ty_base, outwin_id);
				return NULL;
			}
			/* fall through */

		default:
			sprintf(buf, "0x%x", val->v_value.vl_addr);
			return buf;
		}
	
	default:
		{
			char *typestr;
			static char *str = NULL;

			if (str != NULL)
				free(str);
			typestr = ci_type_to_decl(type, FALSE);
			str = strf("(%s)0x%x", typestr,
							val->v_value.vl_addr);
			free(typestr);
			return str;
		}
	}
}

static int
get_value(proc, code_id, type, addr, val, p_mesg)
proc_t proc;
code_id_t code_id;
type_t *type;
taddr_t addr;
val_t *val;
const char **p_mesg;
{
	int nbytes;
	char *buf;

	nbytes = typesize(type);

	*p_mesg = NULL;
	buf = NULL;	/* to satisfy gcc */

	switch (type->ty_code) {
		case TY_CHAR: buf = (char *)&val->v_value.vl_char;	break;
		case TY_UCHAR: buf = (char *)&val->v_value.vl_uchar;	break;
		case TY_SHORT: buf = (char *)&val->v_value.vl_short;	break;
		case TY_USHORT: buf = (char *)&val->v_value.vl_ushort;	break;
		case TY_INT: buf = (char *)&val->v_value.vl_int;		break;
		case TY_UINT: buf = (char *)&val->v_value.vl_uint;	break;
		case TY_LONG: buf = (char *)&val->v_value.vl_long;	break;
		case TY_ULONG: buf = (char *)&val->v_value.vl_ulong;	break;
		case TY_FLOAT: buf = (char *)&val->v_value.vl_float;	break;
		case TY_DOUBLE: buf = (char *)&val->v_value.vl_double;	break;
		case DT_PTR_TO: buf = (char *)&val->v_value.vl_addr;	break;

		case TY_U_STRUCT: *p_mesg = "<undef struct>";	break;
		case TY_U_UNION: *p_mesg = "<undef union>";	break;
		case DT_ARRAY_OF: *p_mesg = "<array>";		break;
		default: *p_mesg = "<unknown type>";		break;
	}

	if (*p_mesg != NULL)
		return 0;
	
	if (code_id != NULL) {
		memcpy(buf, (char *)addr, nbytes);
		return 0;
	}

	return proc_read_data(proc, addr, buf, nbytes);
}

static void
show_aggr(proc, code_id, addr, type, outwin_id)
proc_t proc;
code_id_t code_id;
taddr_t addr;
type_t *type;
outwin_id_t outwin_id;
{
	aggr_or_enum_def_t *ae;
	var_t *v;

	ae = type->ty_aggr_or_enum;

	/*  The vars are in reverse order, so reverse the order
	 *  temporarily
	 */
	ae->ae_aggr_members = ci_push_vars(ae->ae_aggr_members, (var_t *)NULL);

	ow_putc(outwin_id, '{');
	for (v = ae->ae_aggr_members; v != NULL; v = v->va_next) {
		val_t value;
		const char *valstr, *mesg;

		ow_write(outwin_id, v->va_name, strlen(v->va_name));
		ow_putc(outwin_id, '=');

		value.v_type = v->va_type;

		if (v->va_type->ty_code == TY_STRUCT ||
					    v->va_type->ty_code == TY_UNION) {
			show_aggr(proc, code_id, addr + v->va_addr,
							v->va_type, outwin_id);
			valstr = NULL;
		}
		else if (get_value(proc, code_id, v->va_type, addr + v->va_addr,
							&value, &mesg) == 0) {
			if (mesg != NULL)
				valstr = mesg;
			else 
				valstr = fmt_var(proc, code_id, outwin_id,
							    &value, -1, FALSE);
		}
		else {
			char buf[50];

			sprintf(buf, "<bad address 0x%x>", addr + v->va_addr);
			valstr = buf;
		}

		if (valstr != NULL)
			ow_write(outwin_id, valstr, strlen(valstr));
		if (v->va_next != NULL)
			ow_write(outwin_id, " ", 1);
	}
	ow_putc(outwin_id, '}');

	ae->ae_aggr_members = ci_push_vars(ae->ae_aggr_members, (var_t *)NULL);
}

static const char *
fmt_string(proc, code_id, arg, max_chars)
proc_t proc;
code_id_t code_id;
taddr_t arg;
int max_chars;
{
	const char *s;

	if (code_id != NULL && ci_is_ci_addr(code_id, arg)) {
		s = (const char *)arg;
		if (max_chars == -1)
			s = (const char *)arg;
		else {
			static char *buf;
			static int buflen = 0;

			if (max_chars >= buflen) {
				if (buf != NULL)
					free(buf);
				buflen = max_chars;
				buf = e_malloc(buflen);
			}
			strncpy(buf, (char *)arg, max_chars);
			buf[max_chars] = '\0';
			s = buf;
		}
	}
	else
		s = get_string_from_proc(proc, arg, max_chars);
	return s;
}

static bool 
is_string_type(type)
type_t *type;
{
	return (type->ty_code == DT_PTR_TO || type->ty_code == DT_ARRAY_OF) &&
	       (type->ty_base->ty_code == TY_CHAR ||
					type->ty_base->ty_code == TY_UCHAR);
}

ci_exec_result_t
ups_printf(proc, code_id, args, nargs)
proc_t proc;
code_id_t code_id;
taddr_t *args;
int nargs;
{
	int argno;
	char cbuf[2];
	val_t val;
	args_t argdesc;
	const char *fmt, *save_fmt, *argstr;
	outwin_id_t outwin_id;

	if ((outwin_id = get_current_outwin()) == NULL) {
		td_add_outwin();
		outwin_id = get_current_outwin();
	}

	/*  Set up the structure for extracting arguments.
	 */
	argdesc.a_vals = args;
	argdesc.a_types = args + nargs;
	argdesc.a_type_index = 0;

	if (get_arg(&argdesc, &val) != 0)
		return STOP;
	if (!is_string_type(val.v_type)) {
		errf("First argument of $printf is not a string");
		return STOP;
	}
	save_fmt = fmt = (char *)val.v_value.vl_addr;

	while (*fmt != '\0') {
		int padchar, padding, min_fwidth, max_fwidth;
		bool pad_right;

		if (*fmt != '%') {
			ow_putc(outwin_id, *fmt++);
			continue;
		}
		if (*++fmt == '%') {
			ow_putc(outwin_id, *fmt++);
			continue;
		}

		if (pad_right = *fmt == '-')	/* note '=' */
			++fmt;

		if (*fmt == '0') {
			padchar = '0';
			++fmt;
		}
		else
			padchar = ' ';

		min_fwidth = 0;
		if (*fmt == '*') {
			++fmt;
			if (get_arg(&argdesc, &val) != 0)
				goto fail;
			min_fwidth = val.v_value.vl_int;
			++argno;
		}
		else {
			while (isdigit(*fmt))
				min_fwidth = min_fwidth * 10 + *fmt++ - '0';
		}

		max_fwidth = -1;
		if (*fmt == '.') {
			if (*++fmt == '*') {
				++fmt;
				if (get_arg(&argdesc, &val) != 0)
					goto fail;
				max_fwidth = val.v_value.vl_int;
				++argno;
			}
			else {
				max_fwidth = 0;
				while (isdigit(*fmt))
					max_fwidth = max_fwidth * 10 + *fmt++ - '0';
			}
		}

		if (get_arg(&argdesc, &val) != 0)
			goto fail;

		/*  Check that the type matches the format.
		 */
		switch (*fmt) {
		case 'c':
		case 'b':
		case 'o':
		case 'u':
		case 'd':
		case 'x':
		case 'X':
			if (val.v_type->ty_code == TY_DOUBLE) {
				errf("Double passed to %%%c format character in $printf", *fmt);
				goto fail;
			}
			break;
		case 's':
			if (!is_string_type(val.v_type)) {
				errf("Non char * passed to %%s format character in $printf");
				goto fail;
			}
			break;
		case 'e':
		case 'f':
		case 'g':
			if (val.v_type->ty_code != TY_DOUBLE) {
				errf("Non double passed to %%%c format character in $printf", *fmt);
				goto fail;
			}
			break;
		case 'v':
			break;
		default:
			errf("Unknown format character %c", *fmt);
			goto fail;
		}

		switch (*fmt++) {
		case 'c':
			cbuf[0] = val.v_value.vl_int;
			cbuf[1] = '\0';
			argstr = cbuf;
			break;
		case 'b':
			argstr = fmt_int(val.v_value.vl_int, 2, FALSE, FALSE);
			break;
		case 'o':
			argstr = fmt_int(val.v_value.vl_int, 8, FALSE, FALSE);
			break;
		case 'u':
			argstr = fmt_int(val.v_value.vl_int, 10, FALSE, FALSE);
			break;
		case 'd':
			argstr = fmt_int(val.v_value.vl_int, 10, TRUE, FALSE);
			break;
		case 'x':
			argstr = fmt_int(val.v_value.vl_int, 16, FALSE, FALSE);
			break;
		case 'X':
			argstr = fmt_int(val.v_value.vl_int, 16, FALSE, TRUE);
			break;
		case 's':
			argstr = fmt_string(proc, code_id, val.v_value.vl_addr, max_fwidth);
			break;
		case 'v':
			argstr = fmt_var(proc, code_id, outwin_id,
							&val, max_fwidth, TRUE);
			break;
		case 'f':
		case 'e':
		case 'g':
			{
				char fmtbuf[20];
				static char buf[200];

				if (max_fwidth == -1)
					max_fwidth = 6;
				(void) sprintf(fmtbuf, "%%.%d%c",
							max_fwidth, fmt[-1]);

				/*  Note: we know at this point that val.v_value.vl_double
				 *  is a valid fp number - if it wasn't the
				 *  C interpreter would have caught it.
				 */
				(void) sprintf(buf, fmtbuf, val.v_value.vl_double);

				argstr = buf;
			}
			break;
		default:
			errf("Unknown format character %c", fmt[-1]);
			goto fail;
		}

		if (argstr != NULL) {
			padding = min_fwidth - strlen(argstr);
			if (!pad_right)
				while (--padding >= 0)
					ow_putc(outwin_id, padchar);

			ow_write(outwin_id, argstr, strlen(argstr));

			while (--padding >= 0)
				ow_putc(outwin_id, padchar);
		}
	}

	ow_refresh(outwin_id);
	return CI_ER_CONTINUE;

fail:
	/*  We want any partial output to appear
	 */
	ow_refresh(outwin_id);
	return STOP;
}
