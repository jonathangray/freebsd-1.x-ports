#include <stdio.h>
#include "transfig.h"

/*
 * create an appropriate makefile
 */
makefile(mk, arg_list)
FILE *mk;
argument *arg_list;
{
  argument *a;
  char *i;
  enum language to;
  int needps, needpic, needfig;
  

  fprintf(mk, "#\n# TransFig makefile\n#\n");

  fprintf(mk, "\nall: ");
  for (a = arglist; a; a = a->next)
    	fprintf(mk, "%s.tex ", a->name);
  fprintf(mk, "\n");

  for (a = arglist; a; a = a->next)
  {
	i = a->name;
	to = a->tolang;

	needps = needpic = needfig = 0;

	fprintf(mk, "\n# translation into %s\n\n", lname[(int)to]);

	switch (to)
	{
	case box:
		putfig(mk, box, NULL, NULL, a->m, i, "tex");
		needfig = 1;
		break;

	case eepicemu:
	case epic:
	case eepic:
		putfig(mk, to, a->f, a->s, a->m, i, "tex");
		needfig = 1;
		break;

	case latex:
		putfig(mk, latex, a->f, a->s, a->m, i, "tex");
		needfig = 1;
		break;

	case pictex:
		putfig(mk, pictex, a->f, a->s, a->m, i, "tex");
		needfig = 1;
		break;

	case postscript:
                puttarget(mk, i, "tex", "ps");
                fprintf(mk, "\tfig2ps2tex %s.ps >%s.tex\n", i, i);
		needps = 1;
                break;

        case psfig:
                puttarget(mk, i, "tex", "ps");
                fprintf(mk,"\techo \'\\strut\\psfig\{figure=%s.ps\}\' >%s.tex\n",
                          i, i);
		needps = 1;
                break;

	case pstex:

		/*
		 * The makefile for the pstex need to update two files.
		 * file.ps with is created using fig2dev -L texps file.fig
		 * and
		 * file.tex with fig2dev -L pstex -p file.ps file.fig
		 * 
		 */
		puttarget(mk, i, "tex", "ps");
		fprintf(mk, "\tfig2dev -L pstex_t -p %s.ps ", i);
		putoptions(mk, a->f, a->s, a->m, i, "tex");
		needps = 1;
		break;

	case textyl:
		putfig(mk, textyl, a->f, a->s, a->m, i, "tex");
		needfig = 1;
		break;

	case tpic:
		puttarget(mk, i, "tex", "pic");
		/* fprintf(mk, "\ttpic %s.pic\n", i); */
		fprintf(mk, "\tpic2tpic %s.pic | tpic > %s.tex\n", i, i);
		fprintf(mk, "\techo \'\\strut\\box\\graph\' >>%s.tex\n", i);
		needpic = 1;
		break;

	}

	putclean(mk, i, "tex" );

	/* conversion to postscript */
	if (needps && a->type != ps) {
		if ( a->tops ) {
		    puttarget(mk, i, "ps", iname[(int)a->type]);
		    fprintf(mk, "\t%s %s.%s > %s.ps\n", a->tops, i, iname[(int)a->type], i);
		}
		else {
                    putfig(mk, (to == pstex ? pstex : postscript), 
			   a->f, a->s, a->m, i, "ps");
                    a->interm = mksuff(i, ".ps");
		    needfig = 1;
		}
		putclean(mk, i, "ps" );
	}

	/* conversion to pic */
	if (needpic && a->type != pic) {
		if ( a->topic ) {
		    puttarget(mk, i, "pic", iname[(int)a->type]);
		    fprintf(mk, "\t%s %s.%s > %s.pic\n", a->topic, i, iname[(int)a->type],i);
		}
		else {
		    putfig(mk, tpic, a->f, a->s, a->m, i, "pic");

		    needfig = 1;
		}
		putclean(mk, i, "pic" );
	}

	/* conversion to fig */
	if (needfig && a->type != fig) {
		if ( a->tofig ) {
		    puttarget(mk, i, "fig", iname[(int)a->type]);
		    fprintf(mk, "\t%s %s.%s > %s.fig\n", a->tofig, i, iname[(int)a->type],i);
		    a->interm = mksuff(i, ".fig");
		}
		else {
		    fprintf(stderr, "transfig: warning: don't now how to make %s\n", mksuff(i, ".fig") );
		}
		putclean(mk, i, "fig" );
	}
  }
}

puttarget(mk, i, suf1, suf2)
FILE *mk;
char *i, *suf1, *suf2;
{
    fprintf(mk, "%s.%s: %s.%s %s\n", i, suf1, i, suf2, mkfile);
}

putfig(mk, to, f, s, m, i, suf)
FILE *mk;
enum language to;
char *f, *s, *m, *i, *suf;
{
  fprintf(mk, "%s%s%s: %s.fig %s\n",
	       i, (suf ? "." : ""), (suf ? suf : ""), i, mkfile);

  if ( to == tpic )
	  fprintf(mk, "\tfig2dev -L pic ");
  else
	  fprintf(mk, "\tfig2dev -L %s ", lname[(int)to]);

  putoptions(mk, f, s, m, i, suf);
}

putoptions(mk, f, s, m, i, suf)
FILE *mk;
char *f, *s, *m, *i, *suf;
{
  if (f && *f) fprintf(mk, "-f %s ", f);
  if (s && *s) fprintf(mk, "-s %s ", s);
  if (m && *m) fprintf(mk, "-m %s ", m);

  fprintf(mk, "%s.fig > %s%s%s\n", i, i, (suf ? "." : ""), (suf ? suf : ""));
}

putclean(mk, i, suf)
FILE *mk;
char *i, *suf;
{
   fprintf(mk, "clean::\n");
   fprintf(mk, "\trm -f %s.%s\n", i, suf);
   fprintf(mk, "\n");
}
