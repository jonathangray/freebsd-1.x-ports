/*  formf.c -- formatted strings and error messages  */

char ukcprog_formf_sccsid[] = "@(#)formf.c	1.16 26/4/92 UKC";


#include <stdio.h>
#include <ctype.h>
#include <errno.h>

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include <ukcstdlib.h>
#include <ukcstring.h>

#include "ukcprog.h"

#ifndef MSDOS
#ifdef VMS

extern volatile int noshare sys_nerr;
extern const char noshare *sys_errlist[];

#else /* !VMS */

extern int sys_nerr, errno;
extern char *sys_errlist[];

#endif /* !VMS */
#endif /* !MSDOS */


/* forward declarations */
static void new_buffer PROTO((char ** p_buf, int *p_lim, bool *p_from_malloc));
static void concat PROTO((char **p_buf, int *p_pos, int* p_lim,
				bool *p_from_malloc, const char *str, int len));
static char *long_to_ascii PROTO((unsigned long unum, int base,
						bool want_uppercase));
static char *float_to_ascii PROTO((double d, int precision));
static char *efloat_to_ascii PROTO((double d, int precision,
						bool want_uppercase));
static char *gfloat_to_ascii PROTO((double d, int precision,
						bool want_uppercase));

#define MAX_NUM_LEN	40	/* buffer sizes used in numeric conversions */
#define MAX_FLOAT_LEN	128


/*
 *  new_buffer()
 *  Reallocate the given buffer to twice its current size.  *p_lim is the
 *  last usable character in the buffer.  The buffer passed in might
 *  not have been allocated by malloc(); this is indicated by *p_from_malloc.
 */
static void
new_buffer(p_buf, p_lim, p_from_malloc)
char **p_buf;
int *p_lim;
bool *p_from_malloc;
{
	char *new;
	size_t size;

	size = *p_lim + 1;

	if (*p_from_malloc)
		new = realloc(*p_buf, size * 2);

	else {
		if ((new = malloc(size * 2)) != NULL);
			memcpy(new, *p_buf, (int) size);
		*p_from_malloc = TRUE;
	}

	if (new == NULL)
		panic("malloc returned NULL in new_buffer");

	*p_buf = new;
	*p_lim = (size * 2) - 1;
}


/*
 *  concat()
 *  Add exactly len bytes of str to the buffer, expanding it with
 *  new_buffer() if we need extra space.  Str must be at least len
 *  bytes long.
 */
static void
concat(p_buf, p_pos, p_lim, p_from_malloc, str, len)
char **p_buf;
int *p_pos, *p_lim;
bool *p_from_malloc;
const char *str;
int len;
{
	while (*p_lim - *p_pos < len)
		new_buffer(p_buf, p_lim, p_from_malloc);

	memcpy(&(*p_buf)[*p_pos], str, len);
	*p_pos += len;
}


static char *
long_to_ascii(unum, base, want_uppercase)
unsigned long unum;
int base;
bool want_uppercase;
{
	static char nbuf[MAX_NUM_LEN + 1];
	char *s;
	const char *digs;

	s = nbuf + MAX_NUM_LEN;

	digs = want_uppercase ? "0123456789ABCDEF" : "0123456789abcdef";
	do {
		*--s = digs[unum % base];
		unum /= base;
	} while (unum != 0);


	return s;
}


static char *
float_to_ascii(d, precision)
double d;
int precision;
{
	static char buf[MAX_FLOAT_LEN];

	sprintf(buf, "%.*f", precision, d);
	return buf;
}


static char *
efloat_to_ascii(d, precision, want_uppercase)
double d;
int precision;
bool want_uppercase;
{
	static char buf[MAX_FLOAT_LEN];
	const char *format;

	if (want_uppercase)
		format = "%.*E";
	else
		format = "%.*e";

	sprintf(buf, format, precision, d);
	return buf;
}



static char *
gfloat_to_ascii(d, precision, want_uppercase)
double d;
int precision;
bool want_uppercase;
{
	static char buf[MAX_FLOAT_LEN];
	const char *format;

	if (want_uppercase)
		format = "%.*G";
	else
		format = "%.*g";

	sprintf(buf, format, precision, d);
	return buf;
}

/*
 *  formf()
 *  Returns a pointer to a string formatted from the arguments given.
 *  If the string has been obtained from malloc(), the return value will
 *  be different to the buffer pased in; it is the caller's responsibility
 *  to free() this when no longer required.
 */
char *
formf(buffer_space, buffer_size, format, args)
char *buffer_space;
int buffer_size;
const char *format;
va_list args;
{
	bool left_justify, print_sign, prefix_space, zero_pad, alternate_form;
	bool is_negative, precision_given, want_uppercase, from_malloc;
	int min_field_width, precision, modifier, base, pos, lim, len;
	const char *fmt, *alternate_prefix, *s;
	char *buf, str[2], errno_buffer[42];
	int alternate_size, saved_errno;
	unsigned long u;
	long i;
	int *ip;
	double d;

	saved_errno = errno;	/* for use later in %m format */

	/*
	 *  The formatted string is prepared in a buffer pointed to by buf.
	 *  This starts out pointing to a buffer passed in as an argument,
	 *  but if this fills, a new one is obtained from malloc().
	 *  Pos is the index of the position for the *next* character
	 *  in the output string, and lim is the index of the last
	 *  usable character.
	 */
	from_malloc = FALSE;
	buf = buffer_space;
	lim = buffer_size - 1;
	pos = 0;
	fmt = format;

	for (;;) {
		min_field_width = 0;
		precision_given = FALSE;
		precision = 0;
		modifier = 0;
		left_justify = FALSE;
		print_sign = FALSE;
		prefix_space = FALSE;
		zero_pad = FALSE;
		alternate_form = FALSE;

		/*  Find next argument for conversion */
		while (*fmt != '\0' && *fmt != '%') {
			if (pos == lim)
				new_buffer(&buf, &lim, &from_malloc);

			buf[pos++] = *fmt++;
		}

		if (*fmt == '\0') {
			buf[pos] = '\0';
			return buf;	/* end of format string */
		}

		/* flags, in any order */
		for (;;) {
			if (*++fmt == '\0')
				panic("confused format flags in formf");

			if (*fmt == '-')
				left_justify = TRUE;

			else if (*fmt == '+')
				print_sign = TRUE;

			else if (*fmt == ' ')
				prefix_space = TRUE;

			else if (*fmt == '0')
				zero_pad = TRUE;

			else if (*fmt == '#')
				alternate_form = TRUE;
			
			else
				break;	/* end of flags */

		}

		/* minimum field width */
		if (*fmt == '*') {
			min_field_width = va_arg(args, int);
			++fmt;
		} else
			while (isdigit(*fmt)) {
				min_field_width *= 10;
				min_field_width += *fmt - '0';
				++fmt;
			}
		
		if (*fmt == '.') {

			/* precision */
			if (*++fmt == '*') {
				precision_given = TRUE;
				precision = va_arg(args, int);
				++fmt;
			} else if (isdigit(*fmt)) {
				precision_given = TRUE;
				precision = 0;
				do {
					precision *= 10;
					precision += *fmt - '0';
					++fmt;
				} while (isdigit(*fmt));
			}
		}

		if (strchr("hlL", *fmt) != NULL)
			modifier = *fmt++;

		want_uppercase = TRUE;
		alternate_prefix = "0X";
		alternate_size = 2;
		is_negative = FALSE;

		switch (*fmt) {
			case 'd':
			case 'i':
				if (modifier == 'h')
					i = va_arg(args, short);
				else if (modifier == 'l')
					i = va_arg(args, long);
				else
					i = va_arg(args, int);
				is_negative = i < 0;
				u = is_negative ? -i : i;
				s = long_to_ascii(u, 10, want_uppercase);
				break;

			case 'o':
				base = 8;
				alternate_prefix = "0";
				alternate_size = 1;
				goto gencase;
			case 'p':
				alternate_prefix = "0x";
				alternate_size = 2;
				want_uppercase = FALSE;
				base = 16;
				u = (unsigned long)va_arg(args, char *);
				s = long_to_ascii(u, base, want_uppercase);
#ifdef MSDOS
				/*  The traditional format for pointers on
				 *  MSDOS is segment:offset.  NOTE: we depend
				 *  on long_to_ascii() returning a buffer with
				 *  space before the beginning.
				 */
				if ((len = strlen(s)) <= 8) {
					s -= 8 - len;
					memset(s, '0', 8 - len);

					--s;
					memmove(s, s + 1, 4);
					s[4] = ':';
				}
#endif
				break;
			case 'x':
				alternate_prefix = "0x";
				alternate_size = 2;
				want_uppercase = FALSE;	/* fall through */
			case 'X':
				base = 16;
				goto gencase;
			case 'u':
				base = 0;
gencase:			if (modifier == 'h')
					u = va_arg(args, short);
				else if (modifier == 'l')
					u = va_arg(args, long);
				else
					u = va_arg(args, int);
				s = long_to_ascii(u, base, want_uppercase);
				break;

			case 'c':
				str[0] = (char)va_arg(args, int);  /* promoted char */
				str[1] = '\0';
				s = str;
				break;

			case '%':
				str[0] = '%';
				str[1] = '\0';
				s = str;
				break;

			case 's':
				s = va_arg(args, char *);
				if (s == NULL)
					panic("null pointer for %s in formf");
				break;
			case 'm':
				if (saved_errno < 0 || saved_errno > sys_nerr ||
					    *sys_errlist[saved_errno] == '\0') {
					sprintf(errno_buffer,
						"errno %d out of range",
								saved_errno);
					s = errno_buffer;
				} else
					s = sys_errlist[saved_errno];
				break;

			case 'f':
				d = va_arg(args, double);
				is_negative = d < 0.0;
				if (is_negative)
					d = -d;
				if (!precision_given)
					precision = 6;	/* from K&R */
				s = float_to_ascii(d, precision);
				break;
			case 'e':
				want_uppercase = FALSE;	/* fall through */
			case 'E':
				d = va_arg(args, double);
				is_negative = d < 0.0;
				if (is_negative)
					d = -d;
				if (!precision_given)
					precision = 6;	/* from K&R */
				s = efloat_to_ascii(d, precision,
							want_uppercase);
				break;
			case 'g':
				want_uppercase = FALSE;	/* fall through */
			case 'G':
				d = va_arg(args, double);
				is_negative = d < 0.0;
				if (is_negative)
					d = -d;
				if (!precision_given)
					precision = 6;	/* from K&R */
				s = gfloat_to_ascii(d, precision,
							want_uppercase);
				break;
			case 'n':
				ip = va_arg(args, int *);
				*ip = pos;

				++fmt;	/* step over format character */
				continue;
				break;

			default:
				panic("illegal format in formf");
				s = NULL;	/* to satisfy gcc */
				break;
		}

		len = strlen(s);

		/* truncate strings if requested */
		if ((*fmt == 's' || *fmt == 'm')
				&& precision_given && precision < len)
			len = precision;

		if (!left_justify) {
			const char *fillch_str = zero_pad ? "0" : " ";

			if (is_negative && zero_pad) {
				concat(&buf, &pos, &lim, &from_malloc, "-", 1);
				is_negative = FALSE;
				--min_field_width;
			}

			if (alternate_form && alternate_prefix != NULL) {
				concat(&buf, &pos, &lim, &from_malloc,
					alternate_prefix, alternate_size);
				min_field_width -= alternate_size;
				alternate_form = FALSE;
			}

			while (len < min_field_width) {
				concat(&buf, &pos, &lim, &from_malloc,
								fillch_str, 1);
				--min_field_width;
			}
		}

		if (is_negative) {
			concat(&buf, &pos, &lim, &from_malloc, "-", 1);
			--min_field_width;
		}

		if (alternate_form && alternate_prefix != NULL) {
			concat(&buf, &pos, &lim, &from_malloc,
					alternate_prefix, alternate_size);
			min_field_width -= alternate_size;
		}

		
		concat(&buf, &pos, &lim, &from_malloc, s, len);
		min_field_width -= len;

		if (left_justify) {
			while (min_field_width > 0) {
				concat(&buf, &pos, &lim, &from_malloc, " ", 1);
				--min_field_width;
			}
		}

		++fmt;	/* step over format character */
	}
}
