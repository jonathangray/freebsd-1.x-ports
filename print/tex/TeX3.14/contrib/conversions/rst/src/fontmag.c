/*
 * Magnify an RST font
 *
 * If the magnfication is not specified, we try to
 * guess what the target point size is, and adjust the
 * magnfication appropriately.
 *
 * By not reading the glyphs into memory, we have a
 * hope of making this program work on a 16bit machine.
 */
#include <stdio.h>
#include <sys/types.h>
#include "rst.h"

struct rst_glyph_entry o_gd[128];
struct rst_glyph_entry n_gd[128];
struct rst_preamble o_rpre;
struct rst_preamble n_rpre;
char bit[8] = { 0200, 0100, 040, 020, 010, 4, 2, 1 };

double atof();
char *rindex();
char *malloc();
char *strsave();

main(argc, argv)
char **argv;
{
	register struct rst_glyph_entry *ogp, *ngp;
	int i, j, k;
	char *p, *q;
	int pointsize;
	double fontmag, magh, magw;
	FILE *oldfp, *newfp;
	char *oldbase, *newbase;
	char *oldp, *newp;
	int obytes, nbytes;
	int oldbpl, newbpl, oldj, oldk;
	long noffset;
	char *cmd;

	cmd = argv[0];
	fontmag = -1;
	pointsize = -1;
	while (--argc > 0) {
		argv++;
		if (**argv != '-')
			break;
		switch (argv[0][1]) {
		case 'm':
			argc--, argv++;
			fontmag = atof(*argv);
			break;
		case 'p':
			argc--, argv++;
			pointsize = atoi(*argv);
			break;
		default:
		usage:
	fprintf(stderr, "Usage: %s [-m mag] [-p psize] ofile nfile\n", cmd);
			exit(1);
		}
	}
	if (argc != 2)
		goto usage;
	if ((oldfp = fopen(argv[0], "r")) == NULL) {
		perror(argv[0]);
		exit(1);
	}
	if ((newfp = fopen(argv[1], "w")) == NULL) {
		perror(argv[1]);
		exit(1);
	}

	read_preamble(&o_rpre, oldfp);
	read_glyphdir(&o_rpre, o_gd, oldfp);
	if (fontmag <= 0.0) {
		double oldpsize;

		/* intuit the magnification size from output file name */
		if (pointsize <= 0) {
			q = argv[1];
			if (p = rindex(q, '/'))
				q = ++p;
			if (p = rindex(q, '.'))
				q = ++p;
			pointsize = atoi(q);
			if (pointsize <= 0) {
	fprintf(stderr, "Must specify magnfication or point size\n");
				exit(1);
			}
		}
		oldpsize = ((double)o_rpre.rp_designsize / (1L << 20)) *
			o_rpre.rp_fontmag / 1000.0;
		fontmag = pointsize / oldpsize;
	}

	fprintf(stderr, "font magnification %g\n", fontmag);

	/*
	 * first pass:
	 *
	 * calculate new offsets, sizes
	 */
	n_rpre = o_rpre;
	n_rpre.rp_fontmag = o_rpre.rp_fontmag * fontmag;
	obytes = 0;
	nbytes = 0;
	noffset = (n_rpre.rp_lastglyph - n_rpre.rp_firstglyph + 1) * 15 +
		n_rpre.rp_glyphptr;
	for (i = o_rpre.rp_firstglyph; i <= o_rpre.rp_lastglyph; i++) {
		ogp = o_gd + i; ngp = n_gd + i;
		if (ogp->rg_width == 0) {
			*ngp = *ogp;
			continue;
		}
		if (ogp->rg_x > 0)
			ngp->rg_x = ogp->rg_x * fontmag + .5;
		else
			ngp->rg_x = ogp->rg_x * fontmag - .5;
		ngp->rg_y = ogp->rg_y * fontmag + .5;
		ngp->rg_w = ogp->rg_w * fontmag + .5;
		ngp->rg_h = ogp->rg_h * fontmag + .5;
		ngp->rg_width = ogp->rg_width;	/* based on design size */
		ngp->rg_offset = noffset;
		j = (ogp->rg_w + 7) / 8;
		j *= ogp->rg_h;
		k = (ngp->rg_w + 7) / 8;
		k *= ngp->rg_h;
		if (j > obytes)
			obytes = j;
		if (k > nbytes)
			nbytes = k;
		noffset += k;
	}

	/*
	 * second pass:
	 *
	 * copy the rasters
	 */
	oldbase = malloc(obytes);
	newbase = malloc(nbytes);

	write_filemark(newfp);
	write_preamble(&n_rpre, newfp);
	write_glyphdir(&n_rpre, n_gd, newfp);

	for (i = o_rpre.rp_firstglyph; i <= o_rpre.rp_lastglyph; i++) {
		ogp = o_gd + i; ngp = n_gd + i;
		if (ogp->rg_width == 0)
			continue;
		oldbpl = (ogp->rg_w + 7) / 8;
		newbpl = (ngp->rg_w + 7) / 8;
		fseek(oldfp, (long)ogp->rg_offset, 0);
		fread(oldbase, oldbpl * ogp->rg_h, 1, oldfp);

		/* zero out new bit map */
		nbytes = newbpl * ngp->rg_h;
		p = newbase;
		j = nbytes;
		while (--j >= 0)
			*p++ = '\0';

		/*
		 * Construct new bit map - use nearest neighbor.
		 * Recompute the magnification factors because we
		 * are dealing with discrete values.
		 */
		if (ngp->rg_h > 1)
			magh = (double)(ogp->rg_h - 1.0) / (ngp->rg_h - 1.0);
		else
			magh = 1.0;
		if (ngp->rg_w > 1)
			magw = (double)(ogp->rg_w - 1.0) / (ngp->rg_w - 1.0);
		else
			magw = 1.0;
		newp = newbase;

		for (j = 0; j < ngp->rg_h; j++) {
			oldj = j * magh + .5;
			oldp = oldbase + (oldj * oldbpl);
			for (k = 0; k < ngp->rg_w; k++) {
				/* use nearest neighbor */
				oldk = k * magw + .5;
#ifdef SLOW
				if (isset(oldj, oldk, oldbase, oldbpl))
					setbit(j, k, newbase, newbpl);
#else
				p = oldp + (oldk >> 3);
				if (*p & bit[oldk & 07]) {
					p = newp + (k >> 3);
					*p |= bit[k & 07];
				}
#endif
			}
			newp += newbpl;
		}
		/* fseek(newfp, (long)ngp->rg_offset, 0); */
		fwrite(newbase, nbytes, 1, newfp);
	}
	exit(0);
}

#ifdef	SLOW
isset(i, j, cp, bpl)
int i, j, bpl;
char *cp;
{
	cp += (i * bpl) + (j / 8);
	return (*cp & (1 << (7 - (j%8))));
}

setbit(i, j, cp, bpl)
int i, j, bpl;
char *cp;
{
	cp += (i * bpl) + (j / 8);
	*cp |= 1 << (7 - (j%8));
}

clearbit(i, j, cp, bpl)
int i, j, bpl;
char *cp;
{
	cp += (i * bpl) + (j / 8);
	*cp &= ~(1 << (7 - (j%8)));
}
#endif
