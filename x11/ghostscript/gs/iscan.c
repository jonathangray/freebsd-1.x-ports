/* Copyright (C) 1989, 1992, 1993 Aladdin Enterprises.  All rights reserved.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* iscan.c */
/* Token scanner for Ghostscript interpreter */
#include "ghost.h"
#include "ctype_.h"
#include "memory_.h"
#include "stream.h"
#include "alloc.h"
#include "dict.h"			/* for //name lookup */
#include "dstack.h"			/* ditto */
#include "errors.h"
#include "ilevel.h"
#include "iname.h"
#include "iscan.h"			/* defines interface */
#include "iutil.h"
#include "ivmspace.h"
#include "ostack.h"			/* for accumulating proc bodies */
#include "packed.h"
#include "store.h"
#include "scanchar.h"

/* Array packing flag */
ref ref_array_packing;			/* t_boolean */
/* Binary object format flag. This will never be set non-zero */
/* unless the binary token feature is enabled. */
ref ref_binary_object_format;		/* t_integer */
#define recognize_btokens()\
  (ref_binary_object_format.value.intval != 0 && level2_enabled)

/* Procedure for binary tokens.  Set at initialization if Level 2 */
/* features are included; only called if recognize_btokens() is true. */
/* Returns 0 or scan_BOS on success, <0 on failure. */
int (*scan_btoken_proc)(P3(stream *, ref *, int)) = NULL;

/* Setup procedure for ASCII85 literals.  Set at initialization if Level 2 */
/* features are included. */
void (*scan_ascii85_setup_proc)(P4(stream *, stream *, byte *, uint)) = NULL;

/*
 * Level 2 includes some changes in the scanner:
 *	- \ is always recognized in strings, regardless of the data source;
 *	- << and >> are legal tokens;
 *	- <~ introduces an ASCII85 encoded string (terminated by ~>);
 *	- Character codes above 127 introduce binary objects.
 * We explicitly enable or disable these changes here.
 */
#define scan_enable_level2 level2_enabled	/* from ilevel.h */

/* Forward references */
private	int	scan_ascii85_string(P2(stream *, ref *)),
		scan_hex_string(P2(stream *, ref *)),
		scan_int(P6(const byte **, const byte *, int, int,
			    long *, double *)),
		scan_number(P3(const byte *, const byte *, ref *)),
		scan_string(P3(stream *, int, ref *));

/* Define the character scanning table (see scanchar.h). */
byte scan_char_array[258];

/* A structure for dynamically growable objects */
typedef struct dynamic_area_s {
	byte *base;
	byte *next;
	uint num_elts;
	uint elt_size;
	int is_dynamic;			/* false if using fixed buffer */
	byte *limit;
} dynamic_area;
typedef dynamic_area _ss *da_ptr;

/* Begin a dynamic object. */
/* dynamic_begin returns the value of alloc, which may be 0: */
/* the invoker of dynamic_begin must test the value against 0. */
#define dynamic_begin(pda, dnum, desize)\
	((pda)->base = (byte *)alloc((pda)->num_elts = (dnum),\
				     (pda)->elt_size = (desize), "scanner"),\
	 (pda)->limit = (pda)->base + (dnum) * (desize),\
	 (pda)->is_dynamic = 1,\
	 (pda)->next = (pda)->base)
/* Begin a dynamically allocated string in a static buffer. */
#define static_begin_string(pda, count, buf)\
	((pda)->num_elts = (count),\
	 (pda)->elt_size = 1,\
	 (pda)->is_dynamic = 0,\
	 (pda)->limit = (buf) + (count),\
	 (pda)->next = (pda)->base = (buf))

/* Free a dynamic object. */
private void
dynamic_free(da_ptr pda)
{	if ( pda->is_dynamic )
		alloc_free((char *)(pda->base), pda->num_elts, pda->elt_size,
			   "scanner");
}

/* Grow a dynamic object. */
/* If the allocation fails, free the old contents, and return NULL; */
/* otherwise, return the new `next' pointer. */
private byte *
dynamic_grow(register da_ptr pda, byte *next)
{	if ( next != pda->limit ) return next;
	pda->next = next;
	   {	uint num = pda->num_elts;
		uint old_size = num * pda->elt_size;
		uint pos = pda->next - pda->base;
		uint new_size = (old_size < 10 ? 20 :
				 old_size >= (max_uint >> 1) ? max_uint :
				 old_size << 1);
		uint new_num = new_size / pda->elt_size;
		if ( pda->is_dynamic )
		   {	byte *base = alloc_grow(pda->base, num, new_num, pda->elt_size, "scanner");
			if ( base == 0 )
			   {	dynamic_free(pda);
				return NULL;
			   }
			pda->base = base;
			pda->num_elts = new_num;
			pda->limit = pda->base + new_size;
		   }
		else
		   {	byte *base = pda->base;
			if ( !dynamic_begin(pda, new_num, pda->elt_size) ) return NULL;
			memcpy(pda->base, base, old_size);
			pda->is_dynamic = 1;
		   }
		pda->next = pda->base + pos;
	   }
	return pda->next;
}

/* Initialize the scanner. */
void
scan_init(void)
{	/* Initialize decoder array */
	register byte _ds *decoder = scan_char_decoder;
	static const char _ds *stop_chars = "()<>[]{}/%";
	static const char _ds *space_chars = " \f\t\n\r";
	decoder[ERRC] = ctype_eof;	/* ****** FIX THIS? ****** */
	decoder[EOFC] = ctype_eof;
	memset(decoder, ctype_name, 256);
	memset(decoder + 128, ctype_btoken, 32);
	   {	register const char _ds *p;
		for ( p = space_chars; *p; p++ )
		  decoder[*p] = ctype_space;
		decoder[char_NULL] = decoder[char_VT] =
		  decoder[char_DOS_EOF] = ctype_space;
		for ( p = stop_chars; *p; p++ )
		  decoder[*p] = ctype_other;
	   }
	   {	register int i;
		for ( i = 0; i < 10; i++ )
		  decoder['0' + i] = i;
		for ( i = 0; i < max_radix - 10; i++ )
		  decoder['A' + i] = decoder['a' + i] = i + 10;
	   }
	/* Other initialization */
	make_false(&ref_array_packing);
	make_int(&ref_binary_object_format, 0);
}

/*
 * Read a token from a stream.
 * Return 1 for end-of-stream, 0 if a token was read,
 * or a (negative) error code.
 * If the token required a terminating character (i.e., was a name or
 * number) and the next character was whitespace, read and discard
 * that character: see the description of the 'token' operator on
 * p. 232 of the Red Book (First Edition).
 * from_string indicates reading from a string vs. a file,
 * because Level 1 interpreters ignore \ escapes in the former case.
 * (See the footnote on p. 23 of the Red Book.)
 */
int
scan_token(register stream *s, int from_string, ref *pref)
{	ref *myref = pref;
	dynamic_area proc_da;	/* (not actually dynamic) */
	int pstack = 0;		/* offset from proc_da.base */
	int retcode = 0;
	register int c;
	s_declare_inline(s, sptr, endptr);
#define sreturn(code)\
  { s_end_inline(s, sptr, endptr); return_error(code); }
#define sreturn_no_error(code)\
  { s_end_inline(s, sptr, endptr); return(code); }
	int name_type;		/* number of /'s preceding */
	int max_name_ctype =
		(recognize_btokens() ? ctype_name : ctype_btoken);
	int try_number;
	byte s1[2];
	register byte _ds *decoder = scan_char_decoder;
	s_begin_inline(s, sptr, endptr);
top:	c = sgetc_inline(s, sptr, endptr);
#ifdef DEBUG
if ( gs_debug['s'] )
	fprintf(gs_debug_out, (c >= 32 && c <= 126 ? "`%c'" : "`%03o'"), c);
#endif
	switch ( c )
	   {
	case ' ': case '\f': case '\t': case '\n': case '\r':
	case char_NULL: case char_VT: case char_DOS_EOF:
		goto top;
	case '[':
	case ']':
		s1[0] = (byte)c;
		name_ref(s1, 1, myref, 1);
		r_set_attrs(myref, a_executable);
		break;
	case '<':
		if ( scan_enable_level2 )
		   {	c = sgetc_inline(s, sptr, endptr);
			switch ( c )
			   {
			case '<':
				sputback_inline(s, sptr, endptr);
				name_type = try_number = 0;
				goto try_funny_name;
			case '~':
				s_end_inline(s, sptr, endptr);
				retcode = scan_ascii85_string(s, myref);
				goto sx;
			   }
			if ( char_is_data(c) )
				sputback_inline(s, sptr, endptr);
		   }
		s_end_inline(s, sptr, endptr);
		retcode = scan_hex_string(s, myref);
sx:		s_begin_inline(s, sptr, endptr);
		break;
	case '(':
		s_end_inline(s, sptr, endptr);
		retcode = scan_string(s, from_string, myref);
		s_begin_inline(s, sptr, endptr);
		break;
	case '{':
		if ( pstack == 0 )
		   {	/* Use the operand stack to accumulate procedures. */
			myref = osp + 1;
			proc_da.base = (byte *)myref;
			proc_da.limit = (byte *)(ostop + 1);
			proc_da.is_dynamic = 0;
			proc_da.elt_size = sizeof(ref);
			proc_da.num_elts = ostop - osp;
		   }
		if ( proc_da.limit - (byte *)myref < 2 * sizeof(ref) )
		  sreturn(e_limitcheck); /* ****** SHOULD GROW OSTACK ****** */
		r_set_size(myref, pstack);
		myref++;
		pstack = (byte *)myref - proc_da.base;
		goto top;
	case '>':
		if ( scan_enable_level2 )
		   {	name_type = try_number = 0;
			goto try_funny_name;
		   }
		/* falls through */
	case ')':
		retcode = e_syntaxerror;
		break;
	case '}':
		if ( pstack == 0 )
		   {	retcode = e_syntaxerror;
			break;	
		   }
		   {	ref *ref0 = (ref *)(proc_da.base + pstack);
			uint size = myref - ref0;
			myref = ref0 - 1;
			pstack = r_size(myref);
			if ( pstack == 0 ) myref = pref;
			if ( ref_array_packing.value.index )
			   {	retcode = make_packed_array(ref0, size, myref,
							    "scanner(packed)");
				if ( retcode < 0 )
					sreturn(retcode);
				r_set_attrs(myref, a_executable);
			   }
			else
			{	retcode = alloc_array(myref, a_executable + a_all, size, "scanner(proc)");
				if ( retcode < 0 )
					sreturn(retcode);
				refcpy_to_new(myref->value.refs, ref0, size);
			  }
		   }
		break;
	case '/':
		c = sgetc_inline(s, sptr, endptr);
		if ( c == '/' )
		   {	name_type = 2;
			c = sgetc_inline(s, sptr, endptr);
		   }
		else
			name_type = 1;
		try_number = 0;
		switch ( decoder[c] )
		   {
		case ctype_name:
		default:
			goto do_name;
		case ctype_btoken:
			if ( !recognize_btokens() ) goto do_name;
			/* otherwise, an empty name */
			sputback_inline(s, sptr, endptr);
		case ctype_eof:
			/* Empty name: bizarre but legitimate. */
			name_ref((byte *)0, 0, myref, 1);
			goto have_name;
		case ctype_other:
			switch ( c )
			   {
			case '[':	/* only special as first character */
			case ']':	/* ditto */
				s1[0] = (byte)c;
				name_ref(s1, 1, myref, 1);
				goto have_name;
			case '<':	/* legal in Level 2 */
			case '>':
				if ( scan_enable_level2 ) goto try_funny_name;
			default:
				/* Empty name: bizarre but legitimate. */
				name_ref((byte *)0, 0, myref, 1);
				sputback_inline(s, sptr, endptr);
				goto have_name;
			   }
		case ctype_space:
			/* Empty name: bizarre but legitimate. */
			name_ref((byte *)0, 0, myref, 1);
			/* Check for \r\n */
			if ( c == '\r' && (c = sgetc_inline(s, sptr, endptr)) != '\n' && char_is_data(c) )
				sputback_inline(s, sptr, endptr);
			goto have_name;
		   }
		/* NOTREACHED */
	case '%':
	   {	for ( ; ; )
		  switch ( sgetc_inline(s, sptr, endptr) )
		   {
		case '\r':
			if ( (c = sgetc_inline(s, sptr, endptr)) != '\n' && char_is_data(c) )
				sputback_inline(s, sptr, endptr);
			/* falls through */
		case '\n': case '\f':
			goto top;
		case EOFC:
			goto ceof;
		case ERRC:
			goto cerr;
		   }
ceof:		;
	   }	/* falls through */
	case EOFC:
		retcode = (pstack != 0 ? e_syntaxerror : scan_EOF);
		break;
	case ERRC:
cerr:		retcode = e_ioerror;
		break;

	/* Check for a Level 2 funny name (<< or >>). */
	/* c is '<' or '>'. */
try_funny_name:
	   {	int c1 = sgetc_inline(s, sptr, endptr);
		if ( c1 == c )
		   {	s1[0] = s1[1] = c;
			retcode = name_ref(s1, 2, myref, 1);
			goto have_name;
		   }
		if ( char_is_data(c1) ) sputback_inline(s, sptr, endptr);
	   }	retcode = e_syntaxerror;
		break;

	/* Handle separately the names that might be a number. */
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	/* We have special fast code for unsigned integers up to 4 digits, */
	/* because these account for a very large proportion of */
	/* all numbers found in PostScript files. */
		if ( sbufavailable_inline(s, sptr, endptr) >= 6 )
		{	/* Worst case: 4 digits, \r, \n */
			int i = decoder[c];
			int d;
#define char_is_digit(ch)\
 ((d = decoder[ch]) < 10)
			if ( !char_is_digit(sptr[1]) )
			{	if ( d != ctype_space )
					goto nn;
				sptr++;
				goto endi;
			}
			i = i * 10 + d;
			if ( !char_is_digit(sptr[2]) )
			{	if ( d != ctype_space )
					goto nn;
				sptr += 2;
				goto endi;
			}
			i = i * 10 + d;
			if ( !char_is_digit(sptr[3]) )
			{	if ( d != ctype_space )
					goto nn;
				sptr += 3;
				goto endi;
			}
			if ( decoder[sptr[4]] != ctype_space )
				goto nn;
			i = i * 10 + d;
			sptr += 4;
endi:			/* Check for \r\n */
			if ( *sptr == '\r' && sptr[1] == '\n' )
				sptr++;
			make_int_new(myref, i);
			break;
#undef char_is_digit
		}
	/* Handle general possible-numbers. */
	case '.': case '+': case '-':
nn:		name_type = 0;
		try_number = 1;
		goto do_name;

	/* Check for a binary object */
#define case4(c) case c: case c+1: case c+2: case c+3
	case4(128): case4(132): case4(136): case4(140):
	case4(144): case4(148): case4(152): case4(156):
#undef case4
		if ( recognize_btokens() )
		   {	s_end_inline(s, sptr, endptr);
			retcode = (*scan_btoken_proc)(s, myref, c);
			s_begin_inline(s, sptr, endptr);
			break;
		   }
	/* Not a binary object, fall through. */

	/* The default is a name. */
	default:
	/* Populate the switch with enough cases to force */
	/* simple compilers to use a dispatch rather than tests. */
	case '!': case '"': case '#': case '$': case '&': case '\'':
	case '*': case ',': case '=': case ':': case ';': case '?': case '@':
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
	case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M':
	case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S':
	case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
	case '\\': case '^': case '_': case '`':
	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
	case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm':
	case 'n': case 'o': case 'p': case 'q': case 'r': case 's':
	case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
	case '|': case '~':
		/* Common code for scanning a name. */
		/* try_number and name_type are already set. */
		/* We know c has ctype_name (or maybe ctype_btoken) */
		/* or is a digit. */
		name_type = 0;
		try_number = 0;
do_name:
	   {	dynamic_area da;
		/* Try to scan entirely within the stream buffer. */
		/* We stop 1 character early, so we don't switch buffers */
		/* looking ahead if the name is terminated by \r\n. */
		byte *ptr;
		byte *endp1 = endptr - 1;
		da.base = sptr;
		da.is_dynamic = 0;
		do
		{	if ( sptr >= endp1 )	/* stop 1 early! */
				goto dyn_name;
		}
		while ( decoder[*++sptr] <= max_name_ctype );	/* digit or name */
		/* Name ended within the buffer. */
		ptr = sptr;
		c = *sptr;
		goto nx;
dyn_name:	/* Name extended past end of buffer. */
		s_end_inline(s, sptr, endptr);
		/* Initialize the dynamic area. */
		/* We have to do this before the next */
		/* sgetc, which will overwrite the buffer. */
		da.limit = ++sptr;
		da.num_elts = sptr - da.base;
		da.elt_size = 1;
		ptr = dynamic_grow(&da, da.limit);
		if ( !ptr )
			sreturn(e_VMerror);
		s_begin_inline(s, sptr, endptr);
		while ( decoder[c = sgetc_inline(s, sptr, endptr)] <= max_name_ctype )
		  {	if ( ptr == da.limit )
			   {	ptr = dynamic_grow(&da, ptr);
				if ( !ptr )
					sreturn(e_VMerror);
			   }
			*ptr++ = c;
		   }
nx:		switch ( decoder[c] )
		  {
		  case ctype_btoken:
		  case ctype_other:
			sputback_inline(s, sptr, endptr);
			break;
		  case ctype_space:
			/* Check for \r\n */
			if ( c == '\r' && (c = sgetc_inline(s, sptr, endptr)) != '\n' && char_is_data(c) )
				sputback_inline(s, sptr, endptr);
		  case ctype_eof: ;
		  }
		/* Check for a number */
		if ( try_number )
		   {	retcode = scan_number(da.base, ptr, myref);
			if ( retcode != e_syntaxerror )
			   {	dynamic_free(&da);
				if ( name_type == 2 )
					sreturn(e_syntaxerror);
				break;	/* might be e_limitcheck */
			   }
		   }
		retcode = name_ref(da.base, (uint)(ptr - da.base), myref, 1);
		dynamic_free(&da);
	   }
		/* Done scanning.  Check for preceding /'s. */
have_name:	if ( retcode < 0 )
			sreturn(retcode);
		switch ( name_type )
		   {
		case 0:			/* ordinary executable name */
			if ( r_has_type(myref, t_name) )	/* i.e., not a number */
			  r_set_attrs(myref, a_executable);
		case 1:			/* quoted name */
			break;
		case 2:			/* immediate lookup */
		   {	ref *pvalue;
			if ( !r_has_type(myref, t_name) )
				sreturn(e_undefined);
			if ( (pvalue = dict_find_name(myref)) == 0 )
				sreturn(e_undefined);
			if ( !r_is_global(pvalue) && pstack != 0 &&
			     !alloc_current_local()
			   )
				sreturn(e_invalidaccess);
			ref_assign_new(myref, pvalue);
		   }
		   }
	   }
	if ( retcode < 0 )
		sreturn(retcode);
	/* If we are at the top level, return the object, */
	/* otherwise keep going. */
	if ( pstack == 0 )
		sreturn_no_error(retcode);
	if ( proc_da.limit - (byte *)myref < 2 * sizeof(ref) )
		sreturn(e_limitcheck); /* ****** SHOULD GROW OSTACK ****** */
	myref++;
	goto top;
}

/* The internal scanning procedures return 0 on success, */
/* or a (negative) error code on failure. */

/* Scan a number for cvi or cvr. */
/* The first argument is a t_string.  This is just like scan_number, */
/* but allows leading or trailing whitespace. */
int
scan_number_only(const ref *psref, ref *pnref)
{	const byte *str = psref->value.const_bytes;
	const byte *end = str + r_size(psref);
	if ( !r_has_attr(psref, a_read) )
		return_error(e_invalidaccess);
	while ( str < end && scan_char_decoder[*str] == ctype_space )
	  str++;
	while ( str < end && scan_char_decoder[end[-1]] == ctype_space )
	  end--;
	return scan_number(str, end, pnref);
}

/* Note that the number scanning procedures use a byte ** and a byte * */
/* rather than a stream.  (It makes quite a difference in performance.) */
#define neof(sp) (sp >= end)
#define ngetc(cvar, sp, exit) if ( neof(sp) ) { exit; } else cvar = *sp++
#define nputback(sp) (--sp)
#define nreturn(v) return (*pstr = sp, v)

/* Procedure to scan a number. */
private int
scan_number(const byte *str, const byte *end, ref *pref)
{	/* Powers of 10 up to 6 can be represented accurately as */
	/* a single-precision float. */
#define num_powers_10 6
	static const float powers_10[num_powers_10+1] =
	   {	1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6	};
	static const double neg_powers_10[num_powers_10+1] =
	   {	1e0, 1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6	};
	const byte *sp = str;		/* can't be register because of & */
	int sign;
	long ival;
	double dval;
	int exp10;
	int code;
	register int c;
	if ( neof(sp) )
		return_error(e_syntaxerror);
	switch ( *sp )
	{
	case '+': sign = 1; sp++; break;
	case '-': sign = -1; sp++; break;
	default: sign = 0;
	}
	if ( (code = scan_int(&sp, end, 10, 0, &ival, &dval)) != 0 )
	{	if ( code < 0 )
		{	/* We can't actually get a limitcheck yet, */
			/* because we don't check for floating overflow. */
			if ( code != e_syntaxerror )	/* e_limitcheck */
				return code;
			/* Might be a number starting with '.'. */
			/* We don't pre-check for this, because it's rare. */
			ngetc(c, sp, return_error(e_syntaxerror));
			if ( c != '.' )
				return_error(e_syntaxerror);
			ngetc(c, sp, return_error(e_syntaxerror));
			if ( !isdigit(c) )
				return_error(e_syntaxerror);
			ival = 0;
			goto fi;
		}
		/* Code == 1, i.e., the integer overflowed. */
#define retreal()		/* return a float */\
  make_real_new(pref, (float)(sign < 0 ? -dval : dval));\
  return 0
		ngetc(c, sp, retreal());
		switch ( c )
		   {
		default:
			return_error(e_syntaxerror); /* not terminated properly */
		case '.':
			ngetc(c, sp, c = EOFC);
			exp10 = 0;
			goto fd;
		case 'e': case 'E':
			exp10 = 0;
			goto fsd;
		case ERRC:
			return_error(e_ioerror);
		   }
	}
	ngetc(c, sp, goto ri);
	switch ( c )
	{
	case ERRC:
		return_error(e_ioerror);
	case '.':
		ngetc(c, sp, c = EOFC);
		goto fi;
	default:
		return_error(e_syntaxerror);	/* not terminated properly */
	case 'e': case 'E':
		dval = (sign < 0 ? -ival : ival);
		exp10 = 0;
		goto fe;
	case '#':
		if ( sign || ival < min_radix || ival > max_radix )
			return_error(e_syntaxerror);
		code = scan_int(&sp, end, (int)ival, 1, &ival, NULL);
		if ( code )
			return code;
		ngetc(c, sp, goto ri);
		switch ( c )
		   {
		case ERRC:
			return_error(e_ioerror);
		default:
			return_error(e_syntaxerror);
		   }
	}
ri:	/* Return an integer */
	make_int_new(pref, (sign < 0 ? -ival : ival));
	return 0;
	/* Handle a real.  We just saw the decimal point. */
	/* Enter here if we are still accumulating an integer in ival. */
fi:	exp10 = 0;
	while ( isdigit(c) )
	   {	/* Check for overflowing ival */
		if ( ival >= (max_ulong >> 1) / 10 - 1 )
		   {	dval = ival;
			goto fd;
		   }
		ival = ival * 10 + (c - '0');
		exp10--;
		ngetc(c, sp, c = EOFC);
	   }
	if ( sign < 0 ) ival = -ival;
	/* Take a shortcut for the common case */
	if ( !(c == 'e' || c == 'E' || exp10 < -num_powers_10) )
	   {	/* Check for trailing garbage */
		if ( c != EOFC ) return_error(e_syntaxerror);
		make_real_new(pref, (float)(ival * neg_powers_10[-exp10]));
		return 0;
	   }
	dval = ival;
	goto fe;
	/* Now we are accumulating a double in dval. */
fd:	while ( isdigit(c) )
	   {	dval = dval * 10 + (c - '0');
		exp10--;
		ngetc(c, sp, c = EOFC);
	   }
fsd:	if ( sign < 0 ) dval = -dval;
fe:	/* dval contains the value, negated if necessary */
	if ( c == 'e' || c == 'E' )
	   {	/* Check for a following exponent. */
		int esign = 0;
		long eexp;
		ngetc(c, sp, c = EOFC);
		switch ( c )
		   {
		case '+':
			/* The following unnecessary assignment */
			/* works around a bug in the bundled Sun compiler. */
			esign = 0;
			break;
		case '-':
			esign = 1;
			break;
		default:
			nputback(sp);
		   }
		code = scan_int(&sp, end, 10, 0, &eexp, NULL);
		if ( code < 0 ) 
			return code;
		if ( code > 0 || eexp > 999 )
			return_error(e_limitcheck);	/* semi-arbitrary */
		if ( esign )
			exp10 -= (int)eexp;
		else
			exp10 += (int)eexp;
		ngetc(c, sp, c = EOFC);
	   }
	if ( c != EOFC )
		return_error((c == ERRC ? e_ioerror : e_syntaxerror));
	/* Compute dval * 10^exp10. */
	if ( exp10 > 0 )
	   {	while ( exp10 > num_powers_10 )
			dval *= powers_10[num_powers_10],
			exp10 -= num_powers_10;
		if ( exp10 > 0 )
			dval *= powers_10[exp10];
	   }
	else if ( exp10 < 0 )
	   {	while ( exp10 < -num_powers_10 )
			dval /= powers_10[num_powers_10],
			exp10 += num_powers_10;
		if ( exp10 < 0 )
			dval /= powers_10[-exp10];
	   }
	make_real_new(pref, (float)dval);
	return 0;
}
/* Internal subroutine to scan an integer. */
/* Return 0, e_limitcheck, or e_syntaxerror. */
/* (The only syntax error is no digits encountered.) */
/* Put back the terminating character. */
/* If nosign is true, the integer is scanned as unsigned; */
/* overflowing a ulong returns e_limitcheck.  If nosign is false, */
/* the integer is scanned as signed; if the integer won't fit in a long, */
/* then: */
/*   if pdval == NULL, return e_limitcheck; */
/*   if pdval != NULL, return 1 and store a double value in *pdval. */
private int
scan_int(const byte **pstr, const byte *end, int radix, int nosign,
  long *pval, double *pdval)
{	register const byte *sp = *pstr;
	uint ival;
	ulong lval, lmax;
	uint lrem;
	double dval;
	register int c, d;
	register byte _ds *decoder = scan_char_decoder;
#define convert_digit_fails(c, d)\
  (d = decoder[c]) >= radix
	ngetc(c, sp, return_error(e_syntaxerror));
	if ( convert_digit_fails(c, d) )
		return_error(e_syntaxerror);
	ival = d;
	/* Pick up numbers up to 4 digits quickly. */
	if ( radix <= 16 )
	{	ngetc(c, sp, goto out);
		if ( convert_digit_fails(c, d) )
			goto pb;
		ival = ival * radix + d;
		ngetc(c, sp, goto out);
		if ( convert_digit_fails(c, d) )
			goto pb;
		ival = ival * radix + d;
		ngetc(c, sp, goto out);
		if ( convert_digit_fails(c, d) )
			goto pb;
		ival = ival * radix + d;
		if ( neof(sp) )
		{
#if arch_ints_are_short
			/* 4 digits might overflow into the sign bit */
			/* if radix == 16.  Check for this now. */
			if ( (int)ival < 0 && !nosign )
			{	/* Value overflowed into the sign bit, */
				/* but is still OK.  The VMS compiler */
				/* doesn't widen unsigneds to longs */
				/* correctly, so we do an extra assignment. */
				/* We know that the value fits into a long. */
				lval = ival;
				*pval = (long)lval;
				nreturn(0);
			}
#endif
			goto out;
		}
	}

	/* More than 4 digits, or very large radix. */
	/* Accumulate the value in a long. */
	/* Avoid the long divisions when radix = 10. */
	lval = ival;
	if ( radix == 10 )		/* Avoid the divides */
		lmax = max_ulong / 10, lrem = max_ulong % 10;
	else
		lmax = max_ulong / radix, lrem = max_ulong % radix;
	while ( 1 )
	   {	ngetc(c, sp, goto l_end);
		if ( convert_digit_fails(c, d) )
			goto l_pb;
		if ( lval >= lmax && (lval > lmax || d > lrem) )
			goto l_over;		/* overflow */
		lval = lval * radix + d;
	   }

	/* End of short integer. */
pb:	if ( char_is_data(c) ) nputback(sp);
out:	*pval = ival;
	nreturn(0);

	/* End of long integer */
l_pb:	if ( char_is_data(c) ) nputback(sp);
l_end:	if ( (long)lval < 0 && !nosign )
	   {	d = lval % radix;
		lval /= radix;
		goto l_over;
	   }
	*pval = lval;
	nreturn(0);

l_over:	/* Integer overflowed.  Accumulate the result as a double. */
	if ( pdval == NULL )
		nreturn(e_limitcheck);
	dval = (double)lval * radix + d;
	while ( 1 )
	{	ngetc(c, sp, break);
		if ( convert_digit_fails(c, d) )
		{	if ( char_is_data(c) ) nputback(sp);
			break;
		}
		dval = dval * radix + d;
	}

	/* End of very large integer. */
	*pdval = dval;
	nreturn(1);
}

/* Make a string.  If the allocation fails, release any dynamic storage. */
private int
mk_string(ref *pref, da_ptr pda, byte *next)
{	uint size = (pda->next = next) - pda->base;
	byte *body;
	if ( pda->is_dynamic )
	{	body = alloc_shrink(pda->base, pda->num_elts, size, 1,
				    "scanner(string)");
		if ( body == 0 )
		{	dynamic_free(pda);
			return_error(e_VMerror);
		}
	}
	else
	{	body = (byte *)alloc(size, 1, "scanner(string)");
		if ( body == 0 )
			return_error(e_VMerror);
		memcpy(body, pda->base, size);
	}
	make_tasv_new(pref, t_string, a_all, size, bytes, body);
	return 0;
}

/* Internal procedure to scan a string. */
private int
scan_string(register stream *s, int from_string, ref *pref)
{	dynamic_area da;
	register int c;
#define str_count 50
	byte str[str_count];		/* faster buffer for short strings */
	register byte *ptr = static_begin_string(&da, str_count, str);
#undef str_count
	int plevel = 0;
top:	while ( 1 )
	   {	switch ( (c = sgetc(s)) )
		   {
		case EOFC:
			dynamic_free(&da);
			return_error(e_syntaxerror);
		case ERRC:
			dynamic_free(&da);
			return_error(e_ioerror);
		case '\\':
			/* Only old P*stScr*pt interpreters use from_string.... */
			if ( from_string && !scan_enable_level2 ) break;
			switch ( (c = sgetc(s)) )
			   {
			case 'n': c = '\n'; break;
			case 'r': c = '\r'; break;
			case 't': c = '\t'; break;
			case 'b': c = '\b'; break;
			case 'f': c = '\f'; break;
			case '\r':	/* ignore, check for following \n */
				c = sgetc(s);
				if ( c != '\n' && char_is_data(c) )
					sputback(s);
				goto top;
			case '\n': goto top;	/* ignore */
			case '0': case '1': case '2': case '3':
			case '4': case '5': case '6': case '7':
			   {	int d = sgetc(s);
				c -= '0';
				if ( d >= '0' && d <= '7' )
				   {	c = (c << 3) + d - '0';
					d = sgetc(s);
					if ( d >= '0' && d <= '7' )
					   {	c = (c << 3) + d - '0';
						break;
					   }
				   }
				if ( char_is_signal(d) )
				   {	dynamic_free(&da);
					return (d == ERRC ? e_ioerror : e_syntaxerror);
				   }
				sputback(s);
			   }
				break;
			default: ;	/* ignore the \ */
			   }
			break;
		case '(':
			plevel++; break;
		case ')':
			if ( --plevel < 0 ) goto out; break;
		case '\r':		/* convert to \n */
			c = sgetc(s);
			if ( c != '\n' && char_is_data(c) )
				sputback(s);
			c = '\n';
		   }
		if ( ptr == da.limit )
		   {	ptr = dynamic_grow(&da, ptr);
			if ( !ptr )
				return_error(e_VMerror);
		   }
		*ptr++ = c;
	   }
out:	return mk_string(pref, &da, ptr);
}

/* Internal procedure to scan an ASCII85 string. */
private int
scan_ascii85_string(stream *s, ref *pref)
{	dynamic_area da;
	stream ss;
#define buf85_size 128
	byte buf85[buf85_size];
	int c;
#define str_count 50
	byte str[str_count];		/* faster buffer for short strings */
	byte *ptr = static_begin_string(&da, str_count, str);
#undef str_count
	if ( ptr == 0 )
		return_error(e_VMerror);
	(*scan_ascii85_setup_proc)(&ss, s, &buf85[0], buf85_size);
	while ( (c = sgetc(&ss)) >= 0 )
	{	if ( ptr == da.limit )
		   {	ptr = dynamic_grow(&da, ptr);
			if ( !ptr )
				return_error(e_VMerror);
		   }
		*ptr++ = c;
	}
	if ( c != EOFC )
	   {	dynamic_free(&da);
		return_error(e_syntaxerror);
	   }
	return mk_string(pref, &da, ptr);
#undef buf85_size
}

/* Internal procedure to scan a hex string. */
private int
scan_hex_string(stream *s, ref *pref)
{	dynamic_area da;
	int c1, c2, val1, val2;
#define str_count 50
	byte str[str_count];		/* faster buffer for short strings */
	byte *ptr = static_begin_string(&da, str_count, str);
#undef str_count
	register const byte _ds *decoder = scan_char_decoder;
	if ( ptr == 0 )
		return_error(e_VMerror);
l1:	do
	   {	c1 = sgetc(s);
		if ( (val1 = decoder[c1]) < 0x10 )
		   {	do
			   {	c2 = sgetc(s);
				if ( (val2 = decoder[c2]) < 0x10 )
				   {	if ( ptr == da.limit )
					   {	ptr = dynamic_grow(&da, ptr);
						if ( !ptr )
							return_error(e_VMerror);
					   }
					*ptr++ = (val1 << 4) + val2;
					goto l1;
				   }
			   }
			while ( val2 == ctype_space );
			if ( c2 != '>' )
			   {	dynamic_free(&da);
				return_error(e_syntaxerror);
			   }
			if ( ptr == da.limit )
			   {	ptr = dynamic_grow(&da, ptr);
				if ( !ptr )
					return_error(e_VMerror);
			   }
			*ptr++ = val1 << 4;	/* no 2nd char */
			goto lx;
		   }
	   }
	while ( val1 == ctype_space );
	if ( c1 != '>' )
	   {	dynamic_free(&da);
		return_error(e_syntaxerror);
	   }
lx:	return mk_string(pref, &da, ptr);
}
