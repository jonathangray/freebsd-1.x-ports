/* Copyright (C) 1989, 1991, 1993 Aladdin Enterprises.  All rights reserved.

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

/* font.h */
/* Internal font representation for Ghostscript */

/* The external definition of fonts is given in the PostScript manual, */
/* pp. 91-93. */

/* The structure given below is 'client data' from the viewpoint */
/* of the library.  t_fontID objects point directly to a gs_font.  */

typedef struct font_data_s {
	ref dict;			/* font dictionary object */
	ref BuildChar;
	ref BuildGlyph;
	ref Encoding;
	ref CharStrings;
	ref Subrs;			/* from Private dictionary */
} font_data;

/* Registered encodings, for the benefit of platform fonts, `seac', */
/* and compiled font initialization. */
#define registered_Encodings_countof 4
extern ref registered_Encodings[registered_Encodings_countof];
#define StandardEncoding (registered_Encodings[0])
