
#include <stdio.h>
#include "rst.h"

write_filemark(fp)
FILE *fp;
{
	fseek(fp, 0L, 0);
	outstr("Rast", fp);
	out4byt(0, fp);
}

write_preamble(rp, fp)
struct rst_preamble *rp;
FILE *fp;
{
	fseek(fp, 8L, 0);
	out2byt(rp->rp_bytes, fp);
	out1byt(rp->rp_version, fp);
	out3byt(rp->rp_glyphptr, fp);
	out2byt(rp->rp_firstglyph, fp);
	out2byt(rp->rp_lastglyph, fp);
	out4byt(rp->rp_fontmag, fp);
	out4byt(rp->rp_designsize, fp);
	out4byt(rp->rp_interline, fp);
	out4byt(rp->rp_interword, fp);
	out2byt(rp->rp_rotation, fp);
	out1byt(rp->rp_charadvance, fp);
	out1byt(rp->rp_lineadvance, fp);
	out4byt(rp->rp_checksum, fp);
	out2byt(rp->rp_fontres, fp);
	out1byt(rp->rp_fontident.s_l, fp);
	outstr(rp->rp_fontident.s_p, fp);
	out1byt(rp->rp_facetype.s_l, fp);
	outstr(rp->rp_facetype.s_p, fp);
	out1byt(rp->rp_outdevice.s_l, fp);
	outstr(rp->rp_outdevice.s_p, fp);
	out1byt(rp->rp_creator.s_l, fp);
	outstr(rp->rp_creator.s_p, fp);
}

write_glyphdir(rp, gd, fp)
struct rst_preamble *rp;
struct rst_glyph_entry *gd;
FILE *fp;
{
	register int i;

	fseek(fp, (long)rp->rp_glyphptr, 0);
	for (i = rp->rp_firstglyph; i <= rp->rp_lastglyph; i++) {
		out2byt(gd[i].rg_h, fp);
		out2byt(gd[i].rg_w, fp);
		out2byt(gd[i].rg_y, fp);
		out2byt(gd[i].rg_x, fp);
		out4byt(gd[i].rg_width, fp);
		out3byt(gd[i].rg_offset, fp);
	}
}

write_rasters(rp, gd, poff, ifp, ofp)
struct rst_preamble *rp;
struct rst_glyph_entry *gd;
long *poff;
FILE *ifp, *ofp;
{
	register int i, j, k;
	int c;

        for (i = rp->rp_firstglyph; i <= rp->rp_lastglyph; i++) {
		fseek(ifp, poff[i], 0);
		fseek(ofp, (long)gd[i].rg_offset, 0);
		for (j = 0; j < gd[i].rg_h; j++) {
			for (k = 0; k < gd[i].rg_w; k++) {
				c = getc(ifp);
				putc(c, ofp);
			}
		}
	}
}
