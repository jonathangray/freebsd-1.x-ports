/* Copyright (C) 1992, 1993 Aladdin Enterprises.  All rights reserved.

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

/* sbits.h */
/* Definitions for Ghostscript bit-oriented streams */
/* Requires stream.h */

/*
 * Several filters read or write non-byte-aligned data
 * (primarily, but not exclusively, Huffman codes).
 * The macros and procedures declared in this file support this.
 * Note that they distinguish `s', the bit-oriented filter stream,
 * from `strm', the underlying stream supplying or receiving bytes.
 */

/* Define the size of the buffer for reading bits. */
#define sbits_size (arch_sizeof_int == 2 ? 16 : 32)

/* ------ Reading bits ------ */

/*
 * Invariants when reading bits:
 *	0 <= bits_left <= bits_size;
 *	bits[bits_left-1..0] contain valid (unread) data.
 */

/* Declare a table for reversing the bit order of each byte. */
extern const byte sbits_reverse_bits[256];

/* Initialize for reading bits. */
#define sgetbits_init(s, reverse)\
  ((s)->bits = 0, (s)->bits_left = 0, (s)->reverse_bits = reverse)

/* Declare variables for reading bits. */
/* Note that the following names are used, always: */
/*	strm, bits, bits_left, bits_cp, bits_ep	*/
#define sbits_declare_inline\
  stream *strm = s->strm;\
  s_declare_inline(strm, bits_cp, bits_ep);\
  uint bits;\
  int bits_left
/* Begin and end inline bit reading. */
#define sbits_begin_inline(s)\
  s_begin_inline(strm, bits_cp, bits_ep),\
  bits = (s)->bits, bits_left = (s)->bits_left
#define sbits_end_inline(s)\
  s_end_inline(strm, bits_cp, bits_ep),\
  (s)->bits = bits, (s)->bits_left = bits_left

/* Ensure at least n valid bits in the buffer. */
/* n must not be greater than 9. */
#define sbits_ensure_inline(n)\
  if ( bits_left < n ) sbits_more()
#define sbits_more_1()\
  { int c = sgetc_inline(strm, bits_cp, bits_ep);\
    if ( c < 0 ) { s->end_status = c; goto out; }\
    if ( s->reverse_bits ) c = sbits_reverse_bits[c];\
    bits = (bits << 8) + c, bits_left += 8;\
  }
#if sbits_size == 16
#  define sbits_more() sbits_more_1()
#else				/* sbits_size >= 32 */
#  define sbits_more()\
  { if ( bits_ep - bits_cp < 3 ) sbits_more_1()\
    else\
    { if ( s->reverse_bits )\
	bits = (bits << 24) + ((uint)sbits_reverse_bits[bits_cp[0]] << 16) + ((uint)sbits_reverse_bits[bits_cp[1]] << 8) + sbits_reverse_bits[bits_cp[2]];\
      else\
	bits = (bits << 24) + ((uint)bits_cp[0] << 16) + ((uint)bits_cp[1] << 8) + bits_cp[2];\
      bits_left += 24, bits_cp += 3;\
    }\
  }
#endif

/* Peek at the next n bits (known to be available). */
/* Use peek_bits if n is constant, peek_var_bits if n is variable. */
extern const byte sbits_peek_masks[9];
#define sbits_peek_inline(n)\
  ((bits >> (bits_left - (n))) & ((1 << (n)) - 1))
#define sbits_peek_var_inline(n)\
  ((bits >> (bits_left - (n))) & sbits_peek_masks[n])

/* Skip over n bits (known to be available). */
#define sbits_skip_inline(n) bits_left -= (n)

/* ------ Writing bits ------ */

/* Initialize for reading or writing bits. */
#define sputbits_init(s, reverse)\
  ((s)->bits = 0, (s)->bits_left = sbits_size, (s)->reverse_bits = reverse)
