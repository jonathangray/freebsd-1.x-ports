/* 
 *	genpstex.c : psTeX and psTeX_t drivers for fig2dev
 *
 *	Author: Jose Alberto Fernandez R /Maryland CP 9/90
 * 	It uses the LaTeX and PostScript drivers to generate 
 *      LaTeX processed text for a Postscript figure.
 *
 * The pstex_t driver is like a latex driver that only translates 
 * text defined in the defaul font.
 *
 * The pstex driver is like a PostScript driver that translates 
 * everything except for text in the default font.
 *
 * The option '-p file' added to the pstex_t translator specifies
 * the name of the PostScript file to be called in the psfig macro.
 * If not set or its value is null then no PS file will be inserted.
 *
 * Jose Alberto.
 */

#if defined(hpux) || defined(SYSV)
#include <sys/types.h>
#endif
#include <sys/file.h>
#include <stdio.h>
#include <math.h>
#include "object.h"
#include "fig2dev.h"
#include "texfonts.h"

extern char *strchr();
extern double rad2deg, sin(), cos(), acos(), fabs(), atan();

#ifdef hpux
#define rint(a) floor((a)+0.5)     /* close enough? */
#endif

#ifdef gould
#define rint(a) floor((a)+0.5)     /* close enough? */
#endif

extern void genlatex_start (),
	gendev_null (),
	genlatex_end (),
     	genps_option (),
	genps_start (),
	genps_arc (),
	genps_ellipse (),
	genps_line (),
	genps_spline (),
	genps_end (),
        genlatex_option (),
        genlatex_text (),
        genps_text ();

static char pstex_file[40] = "";

void genpstex_t_option(opt, optarg)
char opt, *optarg;
{
       if (opt == 'p') strcpy(pstex_file, optarg);
       else genlatex_option(opt, optarg);
}


void genpstex_t_start(objects)
F_compound	*objects;
{
        genlatex_start(objects);
	/* Put PostScript Image if any*/
        if (pstex_file[0] != '\0')
	  fprintf(tfp, "\\put(%d,%d){\\strut\\psfig{figure=%s}}\n", 
		  llx, lly, pstex_file);

}

void genpstex_t_text(t)
F_text	*t;
{

	if (!special_text(t))
	  gendev_null(t);
	else genlatex_text(t);
}

void genpstex_text(t)
F_text	*t;
{

	if (!special_text(t))
	  genps_text(t);
	else gendev_null(t);
}

void genpstex_option(opt, optarg)
char opt, *optarg;
{
       if (opt != 'p') genlatex_option(opt, optarg);
}

struct driver dev_pstex_t = {
     	genpstex_t_option,
	genpstex_t_start,
	gendev_null,
	gendev_null,
	gendev_null,
	gendev_null,
	genpstex_t_text,
	genlatex_end,
	EXCLUDE_TEXT
};

struct driver dev_pstex = {
     	genpstex_option,
	genps_start,
	genps_arc,
	genps_ellipse,
	genps_line,
	genps_spline,
	genpstex_text,
	genps_end,
	INCLUDE_TEXT
};


