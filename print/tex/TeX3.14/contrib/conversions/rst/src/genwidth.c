/*
 * Generate a width table for ditroff from an RST font
 *
 *	Lou Salkind
 *	New York University
 *
 * We have to specify both a character layout file and a
 * RST font file as input.
 *
 * The character layout file consists of three entries
 * separated by white space:
 *
 *	name	kern	position
 *
 * Lines beginning with # and blank lines are ignored.
 */
#include <stdio.h>
#include <sys/types.h>
#include "rst.h"

#define	MAXNAMES	4	/* maximum number of aliases per character */

struct rst_glyph_entry gd[128];
struct rst_preamble rpre;
FILE *widfp, *chmfp, *rstfp;
char *rindex();
long inbytes();
char *strsave();

/* information from the character map and the RST widths */
struct widtab {
	int	w_count;		/* number of names */
	char	*w_name[MAXNAMES];	/* the names */
	int	w_kern;			/* descender/ascender */
	int	w_width;		/* width in device units */
} w[128];

struct ligtab {
	char	*l_cname;		/* troff character */
	char	*l_hname;		/* width table header entry */
} ligtab[] = {
	"ff", "ff",
	"fi", "fi",
	"fl", "fl",
	"Fi", "ffi", 
	"Fl", "ffl",
	0, 0
};

main(argc, argv)
char **argv;
{
	char buf[BUFSIZ];
	int pos, kern, i, j;
	int lig = 0;
	char charstr[20];
	char flag[20];
	char *fontname = NULL;
	char *special = NULL;
	double frac;
	int resolution, unitwidth;
	double designsize;

	unitwidth = resolution = 0;
	for (i = 1; i < argc && argv[i][0] == '-'; i++) {
		switch (argv[i][1]) {
		case 'r':
			i++;
			resolution = atoi(argv[i]);
			break;
		case 'u':
			i++;
			unitwidth = atoi(argv[i]);
			break;
		case 'n':
			i++;
			fontname = argv[i];
			break;
		case 's':
			i++;
			special = argv[i];
			break;
		default:
		usage:
			fprintf(stderr,
"Usage: %s [-u unitsize] [-r res] [-n name] charmap rstfont\n", argv[0]);
			exit(1);
		}
	}
	if (argc-2 != i)
		goto usage;
	if ((chmfp = fopen(argv[i], "r")) == NULL) {
		perror(argv[i]);
		exit(1);
	}
	if ((rstfp = fopen(argv[i+1], "r")) == NULL) {
		perror(argv[i+1]);
		exit(1);
	}

	read_preamble(&rpre, rstfp);
	read_glyphdir(&rpre, gd, rstfp);

	/*
	 * To convert fixes to ditroff width units, we assume
	 * the following reasonable defaults:
	 *
	 *	1) the number of device internal units per inch
	 *	(resolution in the ditroff DESC file) is
	 *	the same as the font resolution specified in the
	 *	RST raster (rp_fontres).
	 *
	 *	2) the reference size (unitwidth in the DESC file)
	 *	is 16 points.  This number gives reasonable width values
	 *	for a 240 dot/inch printer.
	 */
	if (!fontname) {
		if (fontname = rindex(argv[i+1], '/'))
			fontname++;
		else
			fontname = argv[i+1];
	}
	if (resolution <= 0)
		resolution = rpre.rp_fontres;
	if (unitwidth <= 0)
		unitwidth = 16;
	designsize = (double)rpre.rp_designsize / (1L << 20);
	fprintf(stderr, "font %.2s res %d unitwidth %d designsize %g\n",
		fontname, resolution, unitwidth, designsize);

	while (fgets(buf, sizeof buf, chmfp)) {
		if (buf[0] == '#' || buf[0] == '\n')
			continue;
		if (sscanf(buf, "%s %d %d", charstr, &kern, &pos) != 3) {
			fprintf(stderr, "Invalid format: %s\n", buf);
			continue;
		}
		if (pos < 0 || pos >= 128) {
			fprintf(stderr, "Bad font pos: %d\n", pos);
			continue;
		}
		if (gd[pos].rg_width == 0) {
			fprintf(stderr, "Missing char: %d\n", pos);
			continue;
		}
		lig |= checklig(charstr);
		if ((i = w[pos].w_count++) >= MAXNAMES) {
			fprintf(stderr, "Too many aliases (pos %d)\n", i);
			break;
		}
		w[pos].w_name[i] = strsave(charstr);
		w[pos].w_kern = kern;
		/*
		 * The RST fonts give us the width of each character
		 * in fixes (at the designsize).
		 *
		 * The ditroff character width is specified as the number
		 * of device internal units at a standard reference size
		 * (unitwidth).
		 */
		frac = (double)gd[pos].rg_width / ((1L << 20) * 72.27);
		w[pos].w_width = frac * resolution *
				unitwidth / designsize + .5;
	}
	printf("name	%.2s\n", fontname);
	if (rpre.rp_fontident.s_l)
		printf("internalname	%s\n", rpre.rp_fontident.s_p);
	if (special)
		printf("%s\n", special);
	if (lig) {
		printf("ligatures");
		for (i = 0; ligtab[i].l_cname; i++)
			if (lig & (1 << i))
				printf(" %s", ligtab[i].l_hname);
		printf(" 0\n");
	}
	printf("charset\n");
	for (i = 0; i < 128; i++) {
		if (w[i].w_width == 0)
			continue;
		for (j = 0; j < w[i].w_count; j++)
			printf("%s\t%d\t%d\t%d\n", w[i].w_name[j], w[i].w_width,
			w[i].w_kern, i);
	}
	exit(0);
}

checklig(s)
char *s;
{
	int i;

	if (*s != 'f' && *s != 'F')
		return(0);
	for (i = 0; ligtab[i].l_cname; i++)
		if (strcmp(ligtab[i].l_cname, s) == 0)
			return(1 << i);
	return(0);
}

char *
strsave(s)
char *s;
{
	char *p, *malloc();

	p = malloc(strlen(s)+1);
	if (p == NULL)
		return(NULL);
	strcpy(p, s);
	return(p);
}
