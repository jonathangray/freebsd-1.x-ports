/*
 * Convert PXL to RST format
 * The output device resolution defaults to 240 dots per inch.
 *
 * PXL rasters are the standard format for TeX and MetaFont.
 * They are documented in the TUGBoat article by Dave Fuchs.
 *
 * By not copying glyphs into memory, we have a hope
 * of making this program work on a 16 bit machine
 */
#include <stdio.h>
#include <sys/types.h>
#include "rst.h"

#define in1byt(fp)	inbytes(fp, 1)
#define in2byt(fp)	inbytes(fp, 2)
#define in4byt(fp)	inbytes(fp, 4)

struct rst_glyph_entry gd[128];
struct rst_preamble rpre;
long pxl_offset[128];     	/* Seek locations of each pxl glyph */
FILE *pxlfp, *rstfp;
unsigned long inbytes();
int target_resolution;

#define	PXL_RESOLUTION	200
#define	RST_DEVICE	"Imprint-10"
#define	RST_RESOLUTION	240

main(argc, argv)
char **argv;
{
	int i;
	int slen;
	char *p, *getlogin();
	char *target_device;
	char *pxlfile, *rstfile;

	target_device = RST_DEVICE;
	target_resolution = RST_RESOLUTION;
	for (i = 1; i < argc && argv[i][0] == '-'; i++) {
		switch (argv[i][1]) {
		case 'o':			/* change output device */
			i++;
			target_device = argv[i];
			break;
		case 't':			/* change target resolution */
			i++;
			target_resolution = atoi(argv[i]);
			break;
		default:
		usage:
fprintf(stderr, "Usage: %s [-t res] [-o dev] pxl-font rst-font\n", argv[0]);
			exit(1);
		}
	}
	if (argc-2 != i)
		goto usage;
	pxlfile = argv[i++];
	rstfile = argv[i];
	if ((pxlfp = fopen(pxlfile, "r")) == NULL) {
		perror(pxlfile);
		exit(1);
	}
	if ((rstfp = fopen(rstfile, "w")) == NULL) {
		perror(rstfile);
		exit(1);
	}

	/* initialize RST preamble variables which are not found in PXL file */
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

	read_pxl_header();
	write_rst_header();
	write_gd();
	copy_rasters();

#ifdef	DEBUG
	freopen(argv[2], "r", rstfp);
	dump_font();
#endif
	exit(0);
}

read_pxl_header()
{
	int i;
        long magnification, design_size, font_dir_ptr, rst_offset;
	
	fseek(pxlfp, -(5 * 4L), 2);		/* Seek to trailer info */
        (void)in4byt(pxlfp);			/* checksum */
        magnification = in4byt(pxlfp);
        design_size = in4byt(pxlfp);
        font_dir_ptr = in4byt(pxlfp) * 4;
	if (in4byt(pxlfp) != 1001) {
		fprintf(stderr, "Bad pxl format\n");
		exit(1);
	}
	rpre.rp_fontmag = (magnification * PXL_RESOLUTION) / target_resolution;
	rpre.rp_designsize = design_size;
        rpre.rp_firstglyph = 0;
        rpre.rp_lastglyph = 127;

	fseek(pxlfp, font_dir_ptr, 0);		/* Seek to font directory */
	rst_offset = rpre.rp_glyphptr +
		(rpre.rp_lastglyph-rpre.rp_firstglyph+1) * GLYPH_ENTRY_SIZE;
	for (i = 0; i < 128; i++) {
		gd[i].rg_w = in2byt(pxlfp);
		gd[i].rg_h = in2byt(pxlfp);
		gd[i].rg_x = in2byt(pxlfp);
		gd[i].rg_y = in2byt(pxlfp);
		pxl_offset[i] = in4byt(pxlfp) * 4;
		gd[i].rg_offset = rst_offset;
		gd[i].rg_width =
			((double)design_size / (1L << 20)) * in4byt(pxlfp);
		rst_offset += gd[i].rg_h * ((gd[i].rg_w + 7) / 8);
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
	int rst_bytes, pxl_bytes;
	int c;

        for (i = rpre.rp_firstglyph; i <= rpre.rp_lastglyph; i++) {
		rst_bytes = (gd[i].rg_w + 7) / 8;
		pxl_bytes = ((gd[i].rg_w + 31) / 32) * 4;
		fseek(pxlfp, pxl_offset[i], 0);
	/*
		fseek(rstfp, (long)gd[i].rg_offset, 0);
	*/
		for (j = 0; j < gd[i].rg_h; j++) {
			for (k = 0; k < pxl_bytes; k++) {
				c = getc(pxlfp);
				if (k < rst_bytes)
					putc(c, rstfp);
			}
		}
	}
}

/*
 * read size bytes from the FILE fp, constructing
 * them into an integer
 */
unsigned long
inbytes(fp, size)
FILE *fp;
int size;
{
        int i;
	long x;
	
	x = 0;
	for (i = 0; i < size; i++)
		x = x * 256 + (unsigned)getc(fp);
	return(x);
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
		printf("offset = %ld, poffset = %ld, width = %ld\n",
			gd[i].rg_offset, pxl_offset[i], gd[i].rg_width);
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
