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

/* sdct.h */
/* Common definitions for JPEG DCT encoding and decoding filters */
#include "shc.h"

/* Per-color information for DCT filters */
typedef struct dct_color_params_s {
	int HVSamples[2];
	byte *QuantTable;
	union {
		hce_table encode;
		hcd_table decode;
	} HuffTables[2];
} dct_color_params;
#define HSamples HVSamples[0]
#define VSamples HVSamples[1]
