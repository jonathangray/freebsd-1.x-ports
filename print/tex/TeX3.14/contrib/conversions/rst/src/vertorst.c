/*
 * Convert Versatec or ditroff rasters to RST format
 * The default output resolution is 240 dots per inch.
 *
 * The Versatec format is documented in vfont(5).
 *
 * by not copying glyphs into memory, we have a hope
 * of making this program work on a 16 bit machine
 */
#include <stdio.h>
#include <sys/types.h>
#include <vfont.h>
#include "rst.h"

struct rst_glyph_entry gd[128];
struct rst_preamble rpre;
long ver_offset[128];     	/* Seek locations of each pxl glyph */
int dflag;
int spos;
FILE *verfp, *rstfp;
int ver_resolution, ver_pointsize;

#define	RST_DEVICE	"Imprint-10"
#define	RST_RESOLUTION	240

main(argc, argv)
char **argv;
{
	int slen, i;
	char *verfile, *rstfile;
	char *p, *getlogin(), *rindex();
	int target_resolution;
	char *target_device;

	ver_resolution = 200;
	ver_pointsize = 0;
	target_resolution = RST_RESOLUTION;
	target_device = RST_DEVICE;
	for (i = 1; i < argc && argv[i][0] == '-'; i++) {
		switch (argv[i][1]) {
		case 'r':
			i++;
			ver_resolution = atoi(argv[i]);
			break;
		case 'p':
			i++;
			ver_pointsize = atoi(argv[i]);
			break;
		case 'd':
			dflag++;
			ver_resolution = 240;
			break;
		case 's':
			i++;
			spos = atoi(argv[i]);
			break;
		case 't':
			i++;
			target_resolution = atoi(argv[i]);
			break;
		case 'o':
			i++;
			target_device = argv[i];
			break;
		default:
		usage:
			fprintf(stderr,
"Usage: %s [-p size] [-r res] [-t res] [-o dev] [-d] ver-font rst-font\n", argv[0]);
			exit(1);
		}
	}
	if (argc-2 != i)
		goto usage;
	verfile = argv[i++];
	rstfile = argv[i];
	if ((verfp = fopen(verfile, "r")) == NULL) {
		perror(verfile);
		exit(1);
	}
	if ((rstfp = fopen(rstfile, "w")) == NULL) {
		perror(rstfile);
		exit(1);
	}
	if (ver_pointsize == 0) {
		/* guess point size by using raster name extension */
		p = rindex(verfile, '.');
		if (p == NULL || (ver_pointsize = atoi(p+1)) < 0) {
			fprintf(stderr, "Assuming 10 point\n");
			ver_pointsize = 10;
		}
	}

	/* initialize RST preamble variables which are not found in VER file */
	rpre.rp_version = 0;
	rpre.rp_interline = 0;
	rpre.rp_interword = 0;
	rpre.rp_rotation = 0;
	rpre.rp_charadvance = 0;
	rpre.rp_lineadvance = 1;
	rpre.rp_checksum = 0;
	rpre.rp_fontres = target_resolution;
	/* initialize rp_fontident, rp_fonttype */
	strcpy(rpre.rp_outdevice.s_p, target_device);
	rpre.rp_outdevice.s_l = strlen(rpre.rp_outdevice.s_p);
	if (p = getlogin()) {
		strcpy(rpre.rp_creator.s_p, p);
		rpre.rp_creator.s_l = strlen(rpre.rp_creator.s_p);
	}
	slen = rpre.rp_fontident.s_l + rpre.rp_facetype.s_l +
		rpre.rp_outdevice.s_l + rpre.rp_creator.s_l + 4;
	rpre.rp_bytes = PREAMBLE_FIXED_PART + slen - 2;
	rpre.rp_glyphptr = PREAMBLE_FIXED_PART + slen + FILEMARK_SIZE;
	/*
	 * troff raster files really don't have the
	 * concept of design size.  Especially since troff
	 * determines character widths by scaling up or down
	 * from a well known reference size.
	 *
	 * Therefore, we arbitrarily set the design
	 * size to 10 point.  This will simplify things later on.
	 * The magnification depends on the original raster resolution
	 * and pointsize, as well as the target resolution.
	 */
	rpre.rp_fontmag = (1000*ver_resolution*ver_pointsize) /
		(10*target_resolution);
	rpre.rp_designsize = 10 * (1L << 20);

	read_ver_header();
	write_rst_header();
	write_gd();
	copy_rasters();

#ifdef	DEBUG
	freopen(argv[2], "r", rstfp);
	dump_font();
#endif
	exit(0);
}

read_ver_header()
{
	int i;
	int rowbytes;
	struct header h;
	struct dispatch d;
	long rst_offset, ver_base;
	
	rheader(verfp, &h);
	if (h.magic != 0436) {
		fprintf(stderr, "Bad magic number in versatec font\n");
		exit(1);
	}
	rpre.rp_firstglyph = 0;
	rpre.rp_lastglyph = 127;
	rst_offset = rpre.rp_glyphptr +
		(rpre.rp_lastglyph-rpre.rp_firstglyph+1) * GLYPH_ENTRY_SIZE;
	/*
	 * the following is really (on a pdp11 or vax):
	 *
	 * ver_base = 256 * sizeof (struct dispatch) + sizeof (struct header);
	 *
	 * HOWEVER, WE MUST HARDWIRE THESE NUMBERS,
	 * SINCE STRUCTURES ARE NOT PORTABLE!!!
	 */
	ver_base = 256 * 10 + 10;
	if (spos > 0)
		fseek(verfp, (long)(spos * 10 + 10), 0);
	for (i = 0; i < 128; i++) {
		rdispatch(verfp, &d);
		if (d.nbytes == 0) {
			gd[i].rg_w = gd[i].rg_h = gd[i].rg_x = gd[i].rg_y = 0;
			gd[i].rg_offset = gd[i].rg_width = 0;
			continue;
		}
		/*
		 * Compensate for a rounding bug in the ditroff font format
		 */
		if (dflag) {
			gd[i].rg_w = d.right + d.left + 1;
			rowbytes = (gd[i].rg_w + 7) / 8;
			gd[i].rg_h = d.nbytes / rowbytes;
			gd[i].rg_x = d.left;
			gd[i].rg_y = gd[i].rg_h - d.down;
		} else {
			gd[i].rg_w = d.right + d.left;
			gd[i].rg_h = d.up + d.down;
			gd[i].rg_x = d.left;
			gd[i].rg_y = d.up;
		}
		ver_offset[i] = d.addr + ver_base;
		gd[i].rg_offset = rst_offset;
		/*
		 * fixes/inch = (fixes/point) * (points/inch)
		 *		= 2**20 * 72.27;
		 *
		 * gd[i].rg_width
		 *	= pixel_width * (inch/design_pixels) * (fixes/inch)
		 *	= d.width * 1. / (target_resolution * magnification) *
		 *		(2**20 * 72.27)
		 *
		 * where
		 *	magnification = (ver_pointsize / 10) *
		 *		(ver_resolution / target_resolution)
		 */
		gd[i].rg_width =
			d.width * (1L << 20) * 72.27 * 10 /
			(ver_resolution * ver_pointsize);
		rst_offset += d.nbytes;
	}
        
}

write_rst_header()
{
	/* this is the filemark */
	outstr("Rast", rstfp);
	out4byt(0, rstfp);		/* rest of name is nulls */

	/* now the preamble */
	out2byt(rpre.rp_bytes, rstfp);
	out1byt(rpre.rp_version, rstfp);
	out3byt(rpre.rp_glyphptr, rstfp);
	out2byt(rpre.rp_firstglyph, rstfp);
	out2byt(rpre.rp_lastglyph, rstfp);
	out4byt(rpre.rp_fontmag, rstfp);
	out4byt(rpre.rp_designsize, rstfp);
	out4byt(rpre.rp_interline, rstfp);
	out4byt(rpre.rp_interword, rstfp);
	out2byt(rpre.rp_rotation, rstfp);
	out1byt(rpre.rp_charadvance, rstfp);
	out1byt(rpre.rp_lineadvance, rstfp);
	out4byt(rpre.rp_checksum, rstfp);
	out2byt(rpre.rp_fontres, rstfp);
	out1byt(rpre.rp_fontident.s_l, rstfp);
	outstr(rpre.rp_fontident.s_p, rstfp);
	out1byt(rpre.rp_facetype.s_l, rstfp);
	outstr(rpre.rp_facetype.s_p, rstfp);
	out1byt(rpre.rp_outdevice.s_l, rstfp);
	outstr(rpre.rp_outdevice.s_p, rstfp);
	out1byt(rpre.rp_creator.s_l, rstfp);
	outstr(rpre.rp_creator.s_p, rstfp);
}

write_gd()
{
	register int i;

/*
	fseek(rstfp, (long)rpre.rp_glyphptr, 0);
*/
	for (i = rpre.rp_firstglyph; i <= rpre.rp_lastglyph; i++) {
		out2byt(gd[i].rg_h, rstfp);
		out2byt(gd[i].rg_w, rstfp);
		out2byt(gd[i].rg_y, rstfp);
		out2byt(gd[i].rg_x, rstfp);
		out4byt(gd[i].rg_width, rstfp);
		out3byt(gd[i].rg_offset, rstfp);
	}
}

copy_rasters()
{
	register int i, j, k;
	int nbytes;

        for (i = rpre.rp_firstglyph; i <= rpre.rp_lastglyph; i++) {
		nbytes = (gd[i].rg_w + 7) / 8;
		fseek(verfp, ver_offset[i], 0);
	/*
		fseek(rstfp, (long)gd[i].rg_offset, 0);
	*/
		for (j = 0; j < gd[i].rg_h; j++) {
			for (k = 0; k < nbytes; k++)
				putc(getc(verfp), rstfp);
		}
	}
}

/*
 * read a versatec header
 * some munging around since structures are not portable
 */
rheader(fp, hp)
	FILE *fp;
	struct header *hp;
{
#if !(defined(vax) || defined(pdp11))
	char buf[10];

	fread(buf, sizeof buf, 1, fp);
	hp->magic = makeshort(buf[1], buf[0]);
	hp->size = makeshort(buf[3], buf[2]);
	hp->maxx = makeshort(buf[5], buf[4]);
	hp->maxy = makeshort(buf[7], buf[6]);
	hp->xtend = makeshort(buf[9], buf[8]);
#else
	fread((char *)hp, sizeof (struct header), 1, fp);
#endif
}

/*
 * read a versatec dispatch entry
 * some munging around since structures are not portable
 */
rdispatch(fp, dp)
	FILE *fp;
	struct dispatch *dp;
{
#if !(defined(vax) || defined(pdp11))
	char buf[10];

	fread(buf, sizeof buf, 1, fp);
	dp->addr = makeshort(buf[1], buf[0]);
	dp->nbytes = makeshort(buf[3], buf[2]);
	dp->up = buf[4];
	dp->down = buf[5];
	dp->left = buf[6];
	dp->right = buf[7];
	dp->width = makeshort(buf[9], buf[8]);
#else
	fread((char *)dp, sizeof (struct dispatch), 1, fp);
#endif
}

makeshort(high, low)
{
	return(((high & 0377) << 8) + (low & 0377));
}
#ifdef	DEBUG

dump_font()
{
	int i, j, k, l, c;

	printf("Dump of font:\n");
	for (i = 0; i < 128; i++) {
		printf("\nCharacter %d ", i);
		if (i < 040)
			printf("(^%c)", i + 0100);
		else if (i != 0177)
			printf("(%c)", i);
		else
			printf("(^?)");
		printf(":  ");
		printf("H, W, X, Y = %d, %d, %d, %d\n",
			gd[i].rg_h, gd[i].rg_w, gd[i].rg_x, gd[i].rg_y);
		printf("offset = %ld, voffset = %ld, width = %ld\n",
			gd[i].rg_offset, ver_offset[i], gd[i].rg_width);
		printf("===RST===\n");
		fseek(rstfp, (long)gd[i].rg_offset, 0);
		for (j = 0; j < gd[i].rg_h; j++) {
			for (k = 0; k < (gd[i].rg_w + 7) / 8; k++) {
				c = getc(rstfp);
				for (l = 7; l >= 0; l--) {
					if (c & (1 << l))
	    					putchar('@');
					else
	    					putchar(' ');
				}
			}
			putchar('\n');
		}
	}
}
#endif
