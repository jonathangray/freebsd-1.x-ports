#include <stdio.h>
#include "rst.h"

#define in1byt(fp)	inbytes(fp, 1)
#define in2byt(fp)	inbytes(fp, 2)
#define	in3byt(fp)	inbytes(fp, 3)
#define in4byt(fp)	inbytes(fp, 4)

static long inbytes();

read_filemark(s, fp)
char *s;
FILE *fp;
{
	int i = 8;

	fseek(fp, 0L, 0);
	while (i-- > 0)
		*s++ = in1byt(fp);
}

read_preamble(rp, fp)
struct rst_preamble *rp;
FILE *fp;
{
	fseek(fp, 8L, 0);	/* past file mark */
	rp->rp_bytes = in2byt(fp);
	rp->rp_version = in1byt(fp);
	rp->rp_glyphptr = in3byt(fp);
	rp->rp_firstglyph = in2byt(fp);
	rp->rp_lastglyph = in2byt(fp);
	rp->rp_fontmag = in4byt(fp);
	rp->rp_designsize = in4byt(fp);
	rp->rp_interline = in4byt(fp);
	rp->rp_interword = in4byt(fp);
	rp->rp_rotation = in2byt(fp);
	rp->rp_charadvance = in1byt(fp);
	rp->rp_lineadvance = in1byt(fp);
	rp->rp_checksum = in4byt(fp);
	rp->rp_fontres = in2byt(fp);
	indesc(&rp->rp_fontident, fp);
	indesc(&rp->rp_facetype, fp);
	indesc(&rp->rp_outdevice, fp);
	indesc(&rp->rp_creator, fp);
}

read_glyphdir(rp, gd, fp)
struct rst_preamble *rp;
struct rst_glyph_entry *gd;
FILE *fp;
{
	register int i;

	fseek(fp, (long)rp->rp_glyphptr, 0);
	for (i = rp->rp_firstglyph; i <= rp->rp_lastglyph; i++) {
		gd[i].rg_h = in2byt(fp);
		gd[i].rg_w = in2byt(fp);
		gd[i].rg_y = in2byt(fp);
		gd[i].rg_x = in2byt(fp);
		gd[i].rg_width = in4byt(fp);
		gd[i].rg_offset = in3byt(fp);
	}
}

/*
 * Read size bytes from the FILE fp, constructing
 * them into an integer
 *
 * The routine is for reading unsigned numbers, but it will
 * also work for two and four byte signed numbers when assigning
 * into a two and four byte field (respectively).  This
 * covers everything that occurs in an RST file.
 */
static long
inbytes(fp, size)
FILE *fp;
int size;
{
        register int i, c;
	long x;
	
	x = 0;
	for (i = 0; i < size; i++) {
		if ((c = getc(fp)) == EOF) {
			fprintf(stderr, "Unexpected EOF in raster\n");
			exit(1);
		}
		x <<= 8;
		x += c & 0377;
	}
	return(x);
}

/* read a string descriptor */
static
indesc(dp, fp)
struct rst_dsc *dp;
FILE *fp;
{
	int i;
	char *p;

	dp->s_l = in1byt(fp);
	i = dp->s_l;
	p = dp->s_p;
	while (i-- > 0)
		*p++ = in1byt(fp);
	*p = '\0';
}
