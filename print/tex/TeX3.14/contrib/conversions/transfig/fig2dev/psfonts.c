/* 
 *	psfont.c : PostScript font mappings
 *
*/
#include <stdio.h>
#include "object.h"

char			*PSfontnames[] = {
			"Times-Roman", "Times-Roman",	/* default */
			"Times-Italic",			/* italic */
			"Times-Bold",			/* bold */
			"Times-BoldItalic",
			"AvantGarde",
			"AvantGarde-BookOblique",
			"AvantGarde-Demi",
			"AvantGarde-DemiOblique",
			"Bookman-Light",
			"Bookman-LightItalic",
			"Bookman-Demi",
			"Bookman-DemiItalic",
			"Courier",	
			"Courier-Oblique",
			"Courier-Bold",
			"Courier-BoldItalic",
			"Helvetica",
			"Helvetica-Oblique",
			"Helvetica-Bold",
			"Helvetica-BoldOblique",
			"Helvetica-Narrow",
			"Helvetica-Narrow-Oblique",
			"Helvetica-Narrow-Bold",
			"Helvetica-Narrow-BoldOblique",
			"NewCenturySchlbk-Roman",
			"NewCenturySchlbk-Italic",
			"NewCenturySchlbk-Bold",
			"NewCenturySchlbk-BoldItalic",
			"Palatino-Roman",
			"Palatino-Italic",
			"Palatino-Bold",
			"Palatino-BoldItalic",
			"Symbol",
			"ZapfChancery-MediumItalic",
			"ZapfDingbats"
		};

static int	PSfontmap[] = {
		ROMAN_FONT, ROMAN_FONT,		/* Times-Roman */
		ITALIC_FONT,			/* Times-Italic */
		BOLD_FONT,			/* Times-Bold */
		BOLD_FONT,			/* Times-BoldItalic */
		ROMAN_FONT,			/* AvantGarde */
		ROMAN_FONT,			/* AvantGarde-BookOblique */
		ROMAN_FONT,			/* AvantGarde-Demi */
		ROMAN_FONT,			/* AvantGarde-DemiOblique */
		ROMAN_FONT,			/* Bookman-Light */
		ITALIC_FONT,			/* Bookman-LightItalic */
		ROMAN_FONT,			/* Bookman-Demi */
		ITALIC_FONT,			/* Bookman-DemiItalic */
		TYPEWRITER_FONT,		/* Courier */
		TYPEWRITER_FONT,		/* Courier-Oblique */
		BOLD_FONT,			/* Courier-Bold */
		BOLD_FONT,			/* Courier-BoldItalic */
		MODERN_FONT,			/* Helvetica */
		MODERN_FONT,			/* Helvetica-Oblique */
		BOLD_FONT,			/* Helvetica-Bold */
		BOLD_FONT,			/* Helvetica-BoldOblique */
		MODERN_FONT,			/* Helvetica-Narrow */
		MODERN_FONT,			/* Helvetica-Narrow-Oblique */
		BOLD_FONT,			/* Helvetica-Narrow-Bold */
		BOLD_FONT,		/* Helvetica-Narrow-BoldOblique */
		ROMAN_FONT,			/* NewCenturySchlbk-Roman */
		ITALIC_FONT,			/* NewCenturySchlbk-Italic */
		BOLD_FONT,			/* NewCenturySchlbk-Bold */
		BOLD_FONT,		/* NewCenturySchlbk-BoldItalic */
		ROMAN_FONT,			/* Palatino-Roman */
		ITALIC_FONT,			/* Palatino-Italic */
		BOLD_FONT,			/* Palatino-Bold */
		BOLD_FONT,			/* Palatino-BoldItalic */
		ROMAN_FONT,			/* Symbol */
		ROMAN_FONT,			/* ZapfChancery-MediumItalic */
		ROMAN_FONT			/* ZapfDingbats */
		};

static char *figfontnames[] = {
		"Roman", "Roman",
		"Roman", 
		"Bold",
		"Italic",
		"Modern",
		"Typewriter"
		};

void unpsfont(t)
F_text	*t;
{
	if (!psfont_text(t)) return;
	fprintf(stderr, "PS fonts not supported; substituting %s for %s\n",
		figfontnames[PSfontmap[t->font]+1], PSfontnames[t->font]);
	t->font = PSfontmap[t->font+1];
}

