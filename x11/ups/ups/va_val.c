/* va_val.c - display of variable values */

/*  Copyright 1991 Mark Russell, University of Kent at Canterbury.
 *
 *  You can do what you like with this source code as long as
 *  you don't try to make money out of it and you include an
 *  unaltered copy of this message (including the copyright).
 */

char ups_va_val_c_sccsid[] = "@(#)va_val.c	1.29 15/9/92 (UKC)";

#include <mtrprog/ifdefs.h>

#include <sys/types.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include <local/ukcprog.h>
#include "ups.h"
#include "symtab.h"
#include "data.h"
#include "va.h"
#include "va_priv.h"

static void show_string PROTO((const char *iptr, int ilen,
			       char *optr, int olen,
			       int delim, language_t language));
static int get_fieldval PROTO((int word, bitfield_t *bf, int is_signed));
static int is_signed_format PROTO((vformat_t format));

/*  Read and display a C NUL terminated string.
 *
 *  This routine first tries to read the string in a big chunk.
 *  If that fails it tries to read it a character at a time.  If
 *  we don't do this we might wrongly say bad address for a string
 *  that starts inside the address space, but extends beyond it.
 */
void
read_and_show_C_string(addr, rbuf, rbufsize, obuf, obufsize) 
taddr_t addr;
char *rbuf;
int rbufsize;
char *obuf;
int obufsize;
{

	int len;

	if (dread(addr, rbuf, rbufsize) == 0)
		len = rbufsize;
	else {
		int i;

		for (i = 0; i < rbufsize; ++i) {
			if (i > 0 && rbuf[i - 1] == '\0')
				break;
			if (dread(addr + i, rbuf + i, 1) != 0)
				break;
		}
		len = i;
	}
			
	if (len > 0)
		show_string(rbuf, len, obuf, obufsize, '"', LANG_C);
	else
		strnf(obuf, obufsize, "Bad address 0x%x", addr);
		
}

static void
show_string(iptr, ilen, optr, olen, delim, language)
const char *iptr;
int ilen;
char *optr;
int olen, delim;
language_t language;
{
	static char escapes[] = "\nn\rr\tt\bb\ff\\\\''\"\"";
	const char *ilim, *olim, *cptr;
	int ch;

	olim = optr + olen - 2;		/* one for delim, one for NUL */
	ilim = iptr + ilen;
	*optr++ = delim;
	while (iptr < ilim) {
		ch = *iptr++ & 0xff;
		if (ch >= ' ' && ch != '\\' && ch != delim && ch < 127) {
			if (optr >= olim)
				break;
			*optr++ = ch;
		}
		else if (ch == 0) {
			if (delim == '"' || optr >= olim - 1) {
				*optr++ = delim;
				break;
			}
			*optr++ = '\\';
			*optr++ = '0';
		}
		else if ((cptr = strchr(escapes, ch)) != NULL) {
			if (optr >= olim - 1)
				break;
			*optr++ = '\\';
			*optr++ = cptr[1];
		}
		else {
			if (optr >= olim - 4)
				break;
			sprintf(optr, "\\%03o", ch);
			optr += 4;
		}
	}
	if (iptr == ilim && optr < olim && (delim != '"' || language != LANG_C))
		*optr++ = delim;
	*optr++ = '\0';
}

void
enumval_to_string(buf, bufsize, val, type)
char *buf;
int bufsize;
long val;
type_t *type;
{
	enum_member_t *em;

	if (type->ty_aggr_or_enum == NULL)
		em = NULL;
	else
		em = type->ty_aggr_or_enum->ae_enum_members;

	for (; em != NULL; em = em->em_next) {
		if (val == em->em_val) {
			strncpy(buf, em->em_name, bufsize - 1);
			buf[bufsize - 1] = '\0';
			break;
		}
	}
	if (em == NULL)
		strnf(buf, bufsize - 1, "<%d>", val);
}

void
addr_to_string(buf, bufsize, addr, type, format)
char *buf;
int bufsize;
taddr_t addr;
type_t *type;
vformat_t format;
{
	func_t *f;
	char rbuf[128];

	if (addr == 0)
		strcpy(buf, "NULL");
	if (format == DF_STRING)
		read_and_show_C_string(addr, rbuf, sizeof(rbuf), buf, bufsize);
	else if (type->ty_base->ty_code == DT_FUNC_RETURNING) {
		/* pointer to a function
		 */
		if ((f = addr_to_func(addr)) == NULL)
			strnf(buf, bufsize - 1, "(%x)()", (int)addr);
		else if (f->fu_addr == addr)
			strnf(buf, bufsize - 1, "%s()", f->fu_name);
		else
			strnf(buf, bufsize - 1, "(%s+%x)()", f->fu_name,
							   addr - f->fu_addr);
	}
	else
		int_to_string(buf, bufsize, (long)addr, format);
}

/*  Construct a value string for an integer type variable using the format.
 */
void
int_to_string(buf, bufsize, n, format)
char *buf;
int bufsize;
long n;
vformat_t format;
{
	switch(format) {
	case DF_SDEC:
		sprintf(buf, "%ld", n);
		break;
	case DF_UDEC:
		sprintf(buf, "%lu", n);
		break;
	case DF_SOCT:
		if (n >= 0)
			sprintf(buf, "0%lo", n);
		else
			sprintf(buf, "-0%lo", -n);
		break;
	case DF_UOCT:
		sprintf(buf, "0%lo", n);
		break;
	case DF_SHEX:
		if (n >= 0)
			sprintf(buf, "0x%lx", n);
		else
			sprintf(buf, "-0x%lx", -n);
		break;
	case DF_UHEX:
	case DF_STRING:
		sprintf(buf, "0x%lx", n);
		break;
	case DF_UBIN:
	    {
		int i, start_i;
		char *cptr;

		for (i = 0; n > 0; ++i)
			n <<= 1;
		if (n == 0) {
			strcpy(buf, "0b0");
			break;
		}
		strcpy(buf, "0b");
		start_i = i;
		for (cptr = buf + 2; i < 32; n <<= 1, ++i) {
			if (i > start_i && i % 4 == 0)
				*cptr++ = ' ';
			*cptr++ = (n < 0) ? '1' : '0';
		}
		*cptr = '\0';
		break;
	    }
	case DF_ASCII:
		{
			int count;
			char obuf[sizeof(n) + 1], *optr;

			optr = obuf;
			if ((unsigned)n <= 0xff) {
				*optr = n;
				count = 1;
			}
			else {
				memcpy(optr, (char *)&n, sizeof(n));
				count = sizeof(n);
				while (*optr == '\0') {
					++optr;
					--count;
				}
			}
			optr[count] = '\0';
			show_string(optr, count, buf, bufsize - 1, '\'', LANG_C);
		}
		break;
	default:
		panic("unknown format in df");
	}
}

static int
get_fieldval(word, bf, is_signed)
int word;
bitfield_t *bf;
int is_signed;
{
	int shift;
	unsigned int mask, val;

#ifdef IS_BIG_ENDIAN
	shift = 32 - (bf->bf_offset + bf->bf_width);
#else
	shift = bf->bf_offset;
#endif
	mask = ~(unsigned)0 >> (32 - bf->bf_width);
	val = (word >> shift) & mask;

	/*  If the bitfield is signed we must sign extend it ourself
	 *  if the most significant bit of the field is set.
	 */
	if (is_signed && bf->bf_width > 0 && (val & (1 << (bf->bf_width - 1))))
		val |= ~mask;
	
	return val;
}

static int
is_signed_format(format)
vformat_t format;
{
	switch(format) {
	case DF_UDEC:
	case DF_UOCT:
	case DF_UHEX:
	case DF_UBIN:
		return FALSE;
	default:
		return TRUE;
	}
}

#define DREAD_VAR(addr, var)	(dread(addr, (char *)&var, sizeof(var)) == 0)

/*  Construct a value string for the variable.
 */
const char *
mkval(dv)
dvar_t *dv;
{
	var_t *v;
	value_t vl;
	taddr_t addr;
	type_t *type, *type2;
	ilist_t *il;
	static char buf[128], rbuf[128];
	bool want_hex;
	int len;

	v = dv->dv_var;
	addr = dvar_addr(dv);
	if (addr == BAD_ADDR)
		return "Address out of range";

	type = get_type_at_level(v, dv->dv_ilevel);

	if (type->ty_code == DT_PTR_TO) {
		if (!DREAD_VAR(addr, vl.vl_addr)) {
			sprintf(buf, "<bad address 0x%x>", addr);
			return buf;
		}
		addr_to_string(buf, sizeof(buf), vl.vl_addr,
							type, dv->dv_format);
		return buf;
	}

	if (ISDERIV(type))
		panic("unexpected type in mkval");

	if (addr == 0 && v->va_type->ty_code == DT_PTR_TO &&
		    dv->dv_ilevel == 1 && dv->dv_ilist->il_index == 0)
		return "*NULL";

	if (addr != 0 && dv->dv_format == DF_STRING) {
		int ilen;

		if (v->va_class == CL_REG && type == v->va_type)
			ilen = sizeof(int);
		else
			ilen = sizeof(rbuf);

		read_and_show_C_string(addr, rbuf, ilen, buf, sizeof(buf));
		return buf;
	}
	
	*buf = '\0';
	want_hex = dv->dv_format == DF_SHEX || dv->dv_format == DF_UHEX;

	switch (type->ty_code) {
	case TY_CHARACTER:
		il = dv->dv_ilist;
		type2 = v->va_type->ty_base;
		for (; ISDERIV(type2->ty_code); type2 = type2->ty_base)
			il = il->il_next;
		len = il->il_high - 1;
		if (len > sizeof(buf))
			len = sizeof(buf);

		if (dread(addr, rbuf, len) == 0)
			show_string(rbuf, len, buf, sizeof(buf),
							'"', LANG_FORTRAN);
		break;

	case TY_INTEGER_2:
		if (DREAD_VAR(addr, vl.vl_short))
			int_to_string(buf, sizeof(buf), vl.vl_short,
								dv->dv_format);
		break;
	
	case TY_INTEGER_4:
		if (DREAD_VAR(addr, vl.vl_int))
			int_to_string(buf, sizeof(buf), vl.vl_int,
								dv->dv_format);
		break;

	case TY_LOGICAL:
		if (DREAD_VAR(addr, vl.vl_logical))
			strcpy(buf, vl.vl_logical ? ".true." : ".false.");
		break;

	case TY_REAL:
		if (DREAD_VAR(addr, vl.vl_float))
			strcpy(buf, get_real(vl, want_hex, TRUE));
		break;
	case TY_DBLPRES:
		if (DREAD_VAR(addr, vl.vl_double))
			strcpy(buf, get_real(vl, want_hex, FALSE));
		break;
	case TY_COMPLEX:
		if (DREAD_VAR(addr, vl.vl_float))
			sprintf(buf, "(%s,", get_real(vl, want_hex, TRUE));
		if (DREAD_VAR(addr + sizeof(float), vl.vl_float))
			sprintf(buf + strlen(buf), "%s)",
						  get_real(vl, want_hex, TRUE));
		else
			sprintf(buf, "Bad address 0x%x", addr);
		break;
	case TY_DBLCOMP:
		if (DREAD_VAR(addr, vl.vl_double))
			sprintf(buf, "(%s,", get_real(vl, want_hex, FALSE));
		if (DREAD_VAR(addr + sizeof(double), vl.vl_double))
			sprintf(buf + strlen(buf), "%s)",
					       get_real(vl, want_hex, FALSE));
		else
			sprintf(buf, "Bad address 0x%x", addr);
		break;
	
	case TY_CHAR:
	case TY_UCHAR:
		if (DREAD_VAR(addr, vl.vl_char)) {
			if (is_signed_format(dv->dv_format))
				int_to_string(buf, sizeof(buf), vl.vl_char,
							dv->dv_format);
			else
				int_to_string(buf, sizeof(buf), vl.vl_char & 0xff,
							dv->dv_format);
		}
		break;
	case TY_USHORT:
	case TY_SHORT:
		if (DREAD_VAR(addr, vl.vl_short)) {
			if (is_signed_format(dv->dv_format))
				int_to_string(buf, sizeof(buf), vl.vl_short,
							dv->dv_format);
			else
				int_to_string(buf, sizeof(buf), vl.vl_short & 0xffff,
							dv->dv_format);
		}
		break;
	case TY_BITFIELD:
		if (DREAD_VAR(addr, vl.vl_int))
			int_to_string(buf, sizeof(buf),
			       get_fieldval(vl.vl_int, type->ty_bitfield,
					is_signed_format(dv->dv_format)),
			       dv->dv_format);
		break;
	case TY_INT:
	case TY_LONG:
	case TY_UINT:
	case TY_ULONG:
		if (DREAD_VAR(addr, vl.vl_int))
			int_to_string(buf, sizeof(buf), vl.vl_int, dv->dv_format);
		break;
	case TY_UNION:
	case TY_STRUCT:
	case TY_U_STRUCT:
	case TY_U_UNION:
		sprintf(buf, "*0x%x", addr);
		break;
	case TY_FLOAT:
		if (dread_fpval(addr, v->va_class == CL_REG, FALSE,
						    (char *)&vl.vl_float) == 0)
			strcpy(buf, get_real(vl, want_hex, TRUE));
		break;
	case TY_DOUBLE:
		if (dread_fpval(addr, v->va_class == CL_REG, TRUE,
						   (char *)&vl.vl_double) == 0)
			strcpy(buf, get_real(vl, want_hex, FALSE));
		break;
	case TY_ENUM:
	case TY_U_ENUM:
		if (DREAD_VAR(addr, vl.vl_int))
			enumval_to_string(buf, sizeof(buf), vl.vl_int, type);
		break;
	default:
		strcpy(buf, "<unknown type - tell mtr@ukc.ac.uk>");
	}

	if (*buf == '\0')
		sprintf(buf, "<bad address 0x%x>", addr);

	return buf;
}
