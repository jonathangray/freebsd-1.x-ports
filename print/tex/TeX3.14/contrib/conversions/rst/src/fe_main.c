/*
 * fe - a font editor for RST files
 *
 * NOTE:
 *	This program is about 1/3 done right now.
 *
 *	About the only thing useful it does at the moment is print out
 *	the RST files (good for debugging the other programs).
 *		p, P, and g are the print out commands.
 */

#include <signal.h>
#include <stdio.h>
#include <ctype.h>
#include <setjmp.h>
#include "rst.h"

#define	FixToPxl(w,r)	(r*(w/(72.27*(1L<<20))))

jmp_buf jenv;
struct rst_glyph_entry gd[128];
struct rst_preamble rpre;
int curchar;
char curfile[128];
FILE *rastfp;
int sigcatch();

main(argc, argv)
char **argv;
{
	/*
	 * Commands:
	 *	read specific fonts, glyphs
	 *	magnify a font
	 *	modify preamble fields
	 *	modify a glyph
	 *	print a glyph, raster, preamble
	 *	delete glyphs
	 *	move glyphs around
	 */
	init_edit();
	if (argc > 1) {
		read_font(argv[1]);
		strcpy(curfile, argv[1]);
	}
	docmd();
}

docmd()
{
	char buf[BUFSIZ];
	char str1[100], str2[100];
	char *p;
	int i, j;

	setjmp(jenv);
	signal(SIGINT, sigcatch);
	printf(": "); fflush(stdout);
	while (fgets(buf, sizeof buf, stdin)) {
		p = buf;
		getrange(&p, &i, &j);
		if (i < 0 && j >= 128 && j < i) {
			fprintf(stderr, "Invalid range\n");
			goto pprompt;
		}
		switch (*p) {
		case 'P':		/* list preamble */
			list_preamble(gd);
			break;
		case '\0':
			/* should increment dot by 1 */
		case 'p':		/* list character widths */
			list_glyph(i, j, gd);
			break;
		case 'g':		/* print glyph */
			list_rast(i, j, gd, rastfp);
			break;
		case 'e':		/* edit glyph file */
			getfile(++p, str1);
			read_font(str1);
			strcpy(curfile, str1);
			break;
		case 'r':		/* read glyph file */
			getfile(++p, str1);
			read_font(str1);
			break;
		case 'f':		/* set raster name */
			getfile(++p, str1);
			if (str1[0])
				strcpy(curfile, str1);
			else {
printf("%s: %s\n", curfile[0] ? curfile : "[Buffer]", "[editing]");
			}
			break;
		case 'c':		/* change a value */
			if (sscanf(++p, "%s %s", str1, str2) < 2)
				fprintf(stderr, "change {hwxypa} value\n");
			else
				modify_glyph(str1, str2);
			break;
		case 'q':		/* quit */
			return;
		case 'w':		/* write the file out */
			getfile(++p, str1);
			write_font(str1[0] ? str1 : curfile);
			break;
		case 'd':		/* delete some glyphs */
			printf("delete glyphs\n");
			break;
		case 'D':		/* change design size */
			printf("change design size\n");
			break;
		case 'R':		/* change resolution (?) */
			printf("change resolution\n");
			break;
		case 'x':		/* magnify font */
			printf("magnify\n");
			break;
		default:
			fprintf(stderr, "Unrecognized command\n");
			break;
		}	
	pprompt:
		printf(": "); fflush(stdout);
	}
}

sigcatch()
{
	putchar('\n');
	longjmp(jenv, 1);
}

init_edit()
{
}

getfile(s, p)
char *s, *p;
{
	while (isspace(*s))
		s++;
	while (*s && !isspace(*s))
		*p++ = *s++;
	*p = '\0';
	while (isspace(*s))
		s++;
	if (*s)
		fprintf(stderr, "Invalid file specifier\n");
}

getrange(s, i, j)
char **s;
int *i, *j;
{
	char *p;

	if (getnum(s, i) == 0) {
		*i = *j = curchar;
		return(0);
	}
	*j = *i;
	if (**s == ',') {
		(*s)++;
		getnum(s, j);
		return(2);
	}
	return(1);
}

getnum(s, i)
char **s;
int *i;
{
	int gotnum = 1;
	char *p = *s;

	while (isspace(*p))
		p++;
	if (isdigit(*p)) {
		*i = atoi(p);
		while (isdigit(*++p))
			;
	} else if (*p == '.') {
		*i = curchar;
		p++;
	} else if (*p == '$') {
		*i = 127;
		p++;
	} else
		gotnum = 0;
	while (isspace(*p))
		p++;
	*s = p;
	return(gotnum);
}	

write_font(s)
char *s;
{
	printf("Writing font %s\n", s);
}

read_font(s)
char *s;
{
	char filem[8];
	FILE *fp;

	if ((fp = fopen(s, "r")) == NULL) {
		perror(s);
		return;
	}
	read_filemark(filem, fp);
	if (strcmp(filem, "Rast")) {
		fprintf(stderr, "Invalid font format\n");
		return;
	}
	read_preamble(&rpre, fp);
	read_glyphdir(&rpre, gd, fp);
	printf("%s: glyphs: %d/%d\n", s, rpre.rp_firstglyph, rpre.rp_lastglyph);
	if (rastfp) 		/* XXX */
		fclose(rastfp);
	rastfp = fp;
}

list_preamble(rp)
struct rst_preamble *rp;
{
	printf("font magnification: %d  ", rpre.rp_fontmag);
	printf("design size: %ld\n", rpre.rp_designsize);
	printf("font resolution: %d\n", rpre.rp_fontres);
}

list_glyph(first, last, gd)
struct rst_glyph_entry *gd;
{
	int i;

	printf("	H	W	X	Y	ADV\n");
	for (i = first; i <= last; i++)
		printf("%d:\t%d\t%d\t%d\t%d\t%ld (%d)\n", i, gd[i].rg_h,
		gd[i].rg_w, gd[i].rg_x, gd[i].rg_y, gd[i].rg_width,
(int)(FixToPxl(gd[i].rg_width, rpre.rp_fontres)*((double)rpre.rp_fontmag/1000)));
	curchar = last;
}

list_rast(first, last, gd, fp)
struct rst_glyph_entry *gd;
FILE *fp;
{
	int c, i, j, k, l;

	for (i = first; i <= last; i++) {
		printf("%d:\n", i);
		fseek(fp, (long)gd[i].rg_offset, 0);
		for (j = 0; j < gd[i].rg_h; j++) {
			for (k = 0; k < (gd[i].rg_w + 7) / 8; k++) {
				c = getc(fp);
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
	curchar = last;
}

modify_glyph(s1, s2)
char *s1, *s2;
{
	switch (*s1) {
	case 'x':
	case 'y':
	case 'h':
	case 'w':
		break;
	case 'a':
		break;
	default:
		fprintf(stderr, "possible fields are x,y,h,w,a\n");
		break;
	}
	printf("Field %s = %d\n", s1, atoi(s2));
}
