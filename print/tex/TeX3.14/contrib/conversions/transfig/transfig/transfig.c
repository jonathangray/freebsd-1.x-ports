/*
 * transfig: 	figure translation setup program
 *		creates TeX macro file and makefile
 *
 * usage: transfig <option> ... [[<flag> ... ] [<figure>] ... ] ...
 *
 * where:	<option> = -L <language> | -M <makefile> | -T <texfile>
 *		<flag>	 = -f <font> | -s <size> | -m <scale>
 */

#include <stdio.h>
#include "transfig.h"

argument *parse_arg(), *arglist = NULL, *lastarg = NULL;
char *strip();

char *mkfile = "Makefile";
char *txfile = "transfig.tex";

char *lname[] = {
	"box",
	"epic",
	"eepic",
	"eepicemu",
	"latex",
	"pictex",
	"ps",
	"psfig",
	"pstex",
	"textyl",
	"tpic"};

char *iname[] = {
	"fig",
  	"pic",
	"x",
	"ps"};
 
main(argc, argv)
int argc;
char *argv[];
{
  FILE *mk, *tx;
  enum language tolang = epic;
  argument *a;
  char c, *cp, *arg_f = NULL, *arg_s = NULL, *arg_m = NULL, *argbuf;

  for ( optind = 1; optind < argc; optind++ ) {
    cp = argv[optind];
    if (*cp == '-')
    {
  	if (!cp[1])
	{
		fprintf(stderr, "transfig: bad option format '-'\n");
		exit(1);
	}
	if (cp[2])
	{
		optarg = &cp[2];
	} else
	{
		optind += 1;
		if (optind == argc)
		{
			fprintf(stderr,
				"transfig: no value for '%c' arg\n", cp[1]);
			exit(1);
		}
		optarg = argv[optind];
	}
 	switch (cp[1]) {
 
  	case 'L':
		tolang = str2lang(optarg);
		break;
  	case 'M':
		mkfile = optarg;
		break;
  	case 'T':
		txfile = optarg;
		break;
	case 'f':
		arg_f = optarg;
		break;
	case 's':
		arg_s = optarg;	
		break;
	case 'm':
		arg_m = optarg;	
		break;

  	default:
		fprintf(stderr, "transfig: illegal option -- '%c'\n", *cp);
		exit(1);
  	}
    } else
    {
	a = parse_arg(tolang, arg_f, arg_s, arg_m, argv[optind]);

	if ( !lastarg )
		arglist = a;
	else
		lastarg->next = a; 
	lastarg = a;
    }
  }

  /* no files specified -> all files */
  if (!arglist)
  {
	argbuf = sysls();
	while (cp = strchr(argbuf, '\n'))
	{
		*cp = '\0';
		a = parse_arg(tolang, arg_f, arg_s, arg_m, argbuf);
		if ( !lastarg )
			arglist = a;
		else
			lastarg->next = a; 
		lastarg = a;
		argbuf = cp+1;
	}
  }

  sysmv(txfile);
  tx = fopen(txfile, "w");
  texfile(tx, arglist);

  sysmv(mkfile);
  mk = fopen(mkfile, "w");
  makefile(mk, arglist);
  exit(0);
}

enum language str2lang(s)
char *s;
{
  int i;

  /* aliases */
  if (!strcmp(s, "pic")) return tpic;
  if (!strcmp(s, "postscript")) return postscript;
  if (!strcmp(s, "null")) return box;

  /* real names*/
  for (i = 0; i <= (int)MAXLANG; i++)
	if (!strcmp(lname[i], s)) return (enum language)i;

  /* other strings */
  fprintf(stderr, "Unknown output language \"%s\"\n", s);
  exit(1);
}

argument *parse_arg(tolang, arg_f, arg_s, arg_m, arg)
enum language tolang;
char *arg_f, *arg_s, *arg_m, *arg;
{
  argument *a;

  a = (argument *)malloc(sizeof(argument));
  a->f = arg_f;
  a->s = arg_s;
  a->m = arg_m;
  a->next = NULL;
  a->tofig = NULL;
  a->topic = NULL;
  a->tops = NULL;
  a->tolang = tolang;
  
  /* PIC */
  if (strip(arg, ".pic"))
  {
  	a->name = mksuff(arg, "");
  	a->type = pic;
	a->tofig = PIC2FIG;
	return a;
  }

  /* PS format */
  if (strip(arg, ".ps"))
  {
  	a->name = mksuff(arg, "");
  	a->type = ps;
 	return a;
  }

  /* ApGraph format */
  if (strip(arg, ".apg"))
  {
  	a->name = mksuff(arg, "");
  	a->type = apg;
	a->tofig = APG2FIG;
 	return a;
  }

  /* Fig format */
  strip(arg, ".fig");
  a->name = mksuff(arg, "");
  a->type = fig;
  return a;
}
