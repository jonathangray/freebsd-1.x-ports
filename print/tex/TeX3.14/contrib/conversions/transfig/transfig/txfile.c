#include <stdio.h>
#include "transfig.h"

/*
 * create appropriate .tex file
 */
texfile(tx, arg_list)
FILE *tx;
argument *arg_list;
{
  enum language to;
  argument *a, *arg_l;

  for (a = arglist; a; a = a->next) {
     to = a->tolang;

     /* see if we already have this language */
     for (arg_l = arglist; arg_l != a; arg_l = arg_l->next)
	if ( arg_l->tolang == to ) break;
	
     if ( arg_l == a )
	switch (to)
	{
	case box:
		fprintf(tx, "\\typeout{TransFig: null figures.}\n");
		break;

	case eepicemu:
		to = eepicemu;

	case eepic:
#ifdef eemulation
		to = eepicemu;
#endif

	case epic:
		fprintf(tx, "\\typeout{TransFig: figures in %s.}\n",
							lname[(int)to]);
		if (to == eepicemu || to == eepic)
			fprintf(tx, "\\documentstyle{epic}");
		fprintf(tx, "\\documentstyle{%s}\n", lname[(int)to]);
		break;

	case latex:
		fprintf(tx, "\\typeout{TransFig: figures in LaTeX.}\n");
		break;

	case pictex:
		fprintf(tx, "\\typeout{TransFig: figures in PiCTeX.}\n");
		fprintf(tx, "\\input{prepictex}\n");
		fprintf(tx, "\\input{pictex}\n");
		fprintf(tx, "\\input{postpictex}\n");
		break;

	case postscript:
		fprintf(tx, "\\typeout{TransFig: figures in PostScript.}\n");
		break;

	case pstex: 
		fprintf(tx, "\\typeout{TransFig: figure text in LaTeX.}\n");

	case psfig:
		fprintf(tx, "\\typeout{TransFig: figures in PostScript w/psfig.}\n");
		fprintf(tx, "\\documentstyle{psfig}\n");
		break;

	case textyl:
		fprintf(tx, "\\typeout{TransFig: figures in TeXtyl.}\n");
		break;

	case tpic:
		fprintf(tx, "\\typeout{TransFig: figures in tpic.}\n");
		break;

	default:
		fprintf(tx, "Unknown graphics language %s\n", lname[(int)to]);
		exit(1);
		break;

	}
  }
}
