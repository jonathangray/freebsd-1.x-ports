/* Copyright (C) 1992 Aladdin Enterprises.  All rights reserved.

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

/* shc.h */
/* Common definitions for filters using Huffman coding */

/*
 * These definitions are valid for code lengths up to 16 bits
 * and non-negative decoded values up to 15 bits.
 */

/* ------ Encoding tables ------ */

/* Define the structure for the encoding tables. */
typedef struct hce_code_s {
	ushort code;
	ushort code_length;
} hce_code;
#define hce_entry(c, len) { c, len }

typedef struct hce_table_s {
	uint count;
	hce_code *codes;
} hce_table;

/* ------ Decoding tables ------ */

/*
 * Define the structure for the decoding tables.
 * First-level nodes are either leaves, which have
 *	value = decoded value
 *	code_length <= initial_bits
 * or non-leaves, which have
 *	value = the index of a sub-table
 *	code_length = initial_bits + the number of additional dispatch bits
 * Second-level nodes are always leaves, with
 *	code_length = the actual number of bits in the code - initial_bits.
 */
#define hcd_value_error (-1)

typedef struct hcd_code_s {
	short value;
	ushort code_length;
} hcd_code;

typedef struct hcd_table_s {
	uint count;
	uint initial_bits;
	hcd_code *codes;
} hcd_table;
