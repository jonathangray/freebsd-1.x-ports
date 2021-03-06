/***
 ***	PXL font reading routines.
 ***	Public routines are read_PXL_index and read_PXL_char.
 ***/

#ifndef	MSBITFIRST
static	unsigned char	_reverse_byte[0x100] = {
	0x00, 0x80, 0x40, 0xc0, 0x20, 0xa0, 0x60, 0xe0,
	0x10, 0x90, 0x50, 0xd0, 0x30, 0xb0, 0x70, 0xf0,
	0x08, 0x88, 0x48, 0xc8, 0x28, 0xa8, 0x68, 0xe8,
	0x18, 0x98, 0x58, 0xd8, 0x38, 0xb8, 0x78, 0xf8,
	0x04, 0x84, 0x44, 0xc4, 0x24, 0xa4, 0x64, 0xe4,
	0x14, 0x94, 0x54, 0xd4, 0x34, 0xb4, 0x74, 0xf4,
	0x0c, 0x8c, 0x4c, 0xcc, 0x2c, 0xac, 0x6c, 0xec,
	0x1c, 0x9c, 0x5c, 0xdc, 0x3c, 0xbc, 0x7c, 0xfc,
	0x02, 0x82, 0x42, 0xc2, 0x22, 0xa2, 0x62, 0xe2,
	0x12, 0x92, 0x52, 0xd2, 0x32, 0xb2, 0x72, 0xf2,
	0x0a, 0x8a, 0x4a, 0xca, 0x2a, 0xaa, 0x6a, 0xea,
	0x1a, 0x9a, 0x5a, 0xda, 0x3a, 0xba, 0x7a, 0xfa,
	0x06, 0x86, 0x46, 0xc6, 0x26, 0xa6, 0x66, 0xe6,
	0x16, 0x96, 0x56, 0xd6, 0x36, 0xb6, 0x76, 0xf6,
	0x0e, 0x8e, 0x4e, 0xce, 0x2e, 0xae, 0x6e, 0xee,
	0x1e, 0x9e, 0x5e, 0xde, 0x3e, 0xbe, 0x7e, 0xfe,
	0x01, 0x81, 0x41, 0xc1, 0x21, 0xa1, 0x61, 0xe1,
	0x11, 0x91, 0x51, 0xd1, 0x31, 0xb1, 0x71, 0xf1,
	0x09, 0x89, 0x49, 0xc9, 0x29, 0xa9, 0x69, 0xe9,
	0x19, 0x99, 0x59, 0xd9, 0x39, 0xb9, 0x79, 0xf9,
	0x05, 0x85, 0x45, 0xc5, 0x25, 0xa5, 0x65, 0xe5,
	0x15, 0x95, 0x55, 0xd5, 0x35, 0xb5, 0x75, 0xf5,
	0x0d, 0x8d, 0x4d, 0xcd, 0x2d, 0xad, 0x6d, 0xed,
	0x1d, 0x9d, 0x5d, 0xdd, 0x3d, 0xbd, 0x7d, 0xfd,
	0x03, 0x83, 0x43, 0xc3, 0x23, 0xa3, 0x63, 0xe3,
	0x13, 0x93, 0x53, 0xd3, 0x33, 0xb3, 0x73, 0xf3,
	0x0b, 0x8b, 0x4b, 0xcb, 0x2b, 0xab, 0x6b, 0xeb,
	0x1b, 0x9b, 0x5b, 0xdb, 0x3b, 0xbb, 0x7b, 0xfb,
	0x07, 0x87, 0x47, 0xc7, 0x27, 0xa7, 0x67, 0xe7,
	0x17, 0x97, 0x57, 0xd7, 0x37, 0xb7, 0x77, 0xf7,
	0x0f, 0x8f, 0x4f, 0xcf, 0x2f, 0xaf, 0x6f, 0xef,
	0x1f, 0x9f, 0x5f, 0xdf, 0x3f, 0xbf, 0x7f, 0xff
};
#endif	/* MSBITFIRST */

static	void
read_PXL_char(fontp, ch)
	register struct font *fontp;
	ubyte	ch;
{
	register struct bitmap *bitmap;
	register BMUNIT *ptr;
	register FILE *fp = fontp->file;
	register int i, j;
#ifndef	BMLONG
	register int padding_length;
#endif

	bitmap = &fontp->glyph[ch].bitmap;
		/* in file, bitmap rows are multiples of 32 bits wide */
	alloc_bitmap(bitmap);
	ptr = (BMUNIT *) bitmap->bits;
#ifndef	BMLONG
	padding_length = 3 - (bitmap->bytes_wide + 3) % 4;
#endif
	for (i = bitmap->h; i > 0; --i) {
	    for (j = bitmap->bytes_wide; j > 0; j -= BYTES_PER_BMUNIT) {
#ifndef	MSBITFIRST
		*ptr = _reverse_byte[one(fp)];
#if	BYTES_PER_BMUNIT > 1
		*ptr |= (BMUNIT) _reverse_byte[one(fp)] << 8;
#endif
#ifdef	BMLONG
		*ptr |= (BMUNIT) _reverse_byte[one(fp)] << 16;
		*ptr |= (BMUNIT) _reverse_byte[one(fp)] << 24;
#endif
#else	/* MSBITFIRST */
		*ptr = 0;
#ifdef	BMLONG
		*ptr |= (BMUNIT) one(fp) << 24;
		*ptr |= (BMUNIT) one(fp) << 16;
#endif
#if	BYTES_PER_BMUNIT > 1
		*ptr |= (BMUNIT) one(fp) << 8;
#endif
		*ptr |= (BMUNIT) one(fp);
#endif	/* MSBITFIRST */
		++ptr;
	    }
#ifndef	BMLONG
	    for (j = padding_length; j > 0; --j) (void) one(fp);
#endif
	}
}

void
read_PXL_index(fontp)
	register struct font *fontp;
{
	register FILE *fp	= fontp->file;
	register struct glyph *g;
	long	font_dir_ptr;
	long	trailer_ptr;

	if (debug & DBG_PK)
	    Printf("Reading PXL file %s\n", fontp->filename);
	fontp->read_char = read_PXL_char;
	/* seek to trailer info */
	Fseek(fp, (long) -4, 2);
	while (four(fp) != 1001)
	    Fseek(fp, (long) -5, 1);
	Fseek(fp, (long) -5 * 4, 1);
	trailer_ptr = ftell(fp);
	(void) four(fp);	/* checksum */
	(void) four(fp);	/* magnify */
	(void) four(fp);	/* design size */
	font_dir_ptr = sfour(fp) * 4;
	(void) four(fp);	/* pxl id word */
	/* seek to font directory */
	Fseek(fp, font_dir_ptr, 0);
	maxchar = (trailer_ptr - font_dir_ptr) / 16 - 1;
	fontp->glyph = (struct glyph *)
		xmalloc((maxchar + 1) * sizeof(struct glyph), "glyph array");
	for (g = fontp->glyph; g <= fontp->glyph + maxchar; ++g) {
	    g->bitmap.bits = NULL;
	    g->bitmap2.bits = NULL;
#ifdef	GREY
	    g->pixmap2 = NULL;
#endif
	    g->bitmap.w = two(fp);
	    g->bitmap.h = two(fp);
	    g->x = stwo(fp);
	    g->y = stwo(fp);
	    g->addr = four(fp) * 4;
		/*
		**  The TFM-width word is kind of funny in the units
		**  it is expressed in.  It works this way:
		**
		**  If a glyph has width 'w' in a font with design-size
		**  'd' (both in same units), the TFM-width word is
		**
		**                t = (w/d) * 2^20
		**
		**  Therefore, in order to find the glyph width in
		**  DVI units (1 / 2^16 points), we take the design-size
		**  'd' (in DVI's), the magnification 'm' of the PXL file
		**  and the TFM-width word 't' to the width (in DVI's)
		**  as follows:
		**
		**                     dmt
		**                w = -----
		**                    2^20
		**
		**  But the magnification of the PXL file is just the
		**  scaled size 's' over the design size, so the final
		**  expression for the width is
		**
		**                     st
		**                w = ----
		**                    2^20
		**
		*/

	    g->dvi_adv = fontp->dimconv * four(fp);
	    if (debug & DBG_PK)
		Printf("size=%dx%d, dvi_adv=%d\n", g->bitmap.w, g->bitmap.h,
		    g->dvi_adv);
	}
}
