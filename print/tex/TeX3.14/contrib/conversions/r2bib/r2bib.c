# include <ctype.h>
# include <stdio.h>

struct rb {
	char	rb_kl;		/* refer key letter		*/
	char *	rb_kw;		/* bibtex string		*/
	char	rb_emit;	/* don't print data if 0	*/
	char *	rb_data;	/* refer data			*/
};

struct rb rb[] = {
	{ 'A',	"author",	1,	NULL	},
	{ 'B',	"booktitle",	1,	NULL	},
	{ 'C',	"address",	1,	NULL	},
	{ 'D',	"year",		1,	NULL	},	/* mismatch */
	{ 'E',	"editor",	1,	NULL	},
/*	{ 'H',	"commentary1",	1,	NULL	},*/
	{ 'I',	"publisher",	1,	NULL	},
	{ 'J',	"journal",	1,	NULL	},
	{ 'K',	"note",		1,	NULL	},	/* mismatch */
	{ 'L',	"label",	0,	NULL	},	/* use as bibtex key */
	{ 'N',	"number",	1,	NULL	},
/*	{ 'O',	"commentary2",	1,	NULL	},*/
	{ 'P',	"pages",	1,	NULL	},
	{ 'Q',	"institution",	1,	NULL	},
	{ 'R',	"report",	0,	NULL	},
	{ 'S',	"series",	1,	NULL	},
	{ 'T',	"title",	1,	NULL	},
	{ 'V',	"volume",	1,	NULL	},
/*	{ 'X',	"abstract",	1,	NULL	},*/
	{ 0,	0,		0,	0	}
};

struct bmap {
	char	bm_kl;
	char	*bm_entry;
};

/*
 * entries are in order of precedence.
 * any entry with a 'J' field must be
 * an article, but anthing with an 'I'
 * field doesn't have to be a book (if
 * an entry has both 'J' and 'I' it is
 * considered to be an article).
 */
struct bmap	bmap[] = {
	{ 'J',	"article"	},
	{ 'R',	"techreport"	},
	{ 'I',	"book"		},
	{ 0,	0		}
};

main(argc, argv)
	char		**argv;
{
	register FILE	*fid;
	register int	i;
	int		err;

	err = 0;

	if (argc > 1) {
		for (i = 1; i < argc; i++) {
			if ((fid = fopen(argv[i], "r")) == NULL) {
				fprintf(stderr, "fopen: ");
				perror(argv[i]);
				continue;
			}
			err += r2bib(argv[i], fid);
		}
	}
	else
		err += r2bib("stdin", stdin);

	if (err)
		exit(1);

	exit(0);
}

r2bib(file, fid)
	char		*file;
	FILE		*fid;
{
	extern char	*sanz();
	register char	*cp;
	struct rb	*lrb;		/* last rb stored into */
	int		line;
	char		buf[BUFSIZ];
	int		err;

	lrb = NULL;
	err = 0;
	line = 0;

	while (fgets(buf, sizeof(buf), fid) != NULL) {
		line++;

		if ((cp = sanz(buf)) == NULL) {
			if (lrb != NULL) {
				dumprb();
				lrb = NULL;
			}
			continue;
		}

		/*
		 * if the first letter is a % then it's the
		 * a new record, otherwise it's a continuation
		 * of the previous one.
		 */
		if (cp[0] == '%') {
			for (lrb = &rb[0]; lrb->rb_kl != 0; lrb++) {
				if (lrb->rb_kl == cp[1]) {
					stuffrb(lrb, &cp[2]);
					break;
				}
			}
			if (lrb->rb_kl == 0) {
				fprintf(stderr, "r2b: %s: line %d: unknown key letter %c, ignoring\n", file, line, cp[1]);
				err = 1;
			}
		}
		else {
			if (lrb == NULL) {
				fprintf(stderr, "r2b: %s: line %d: bad format, ignoring\n", file, line);
				err = 1;
				continue;
			}

			stuffrb(lrb, &cp[0]);
		}
	}

	if (lrb != NULL)
		dumprb();

	return(err);
}

dumprb() {
	register struct rb	*trb;
	register struct bmap	*bm;
	static int		key;
	char			*bibkey;
	char			*cp;
	int			first;

	/*
	 * first, figure out what type of entry this
	 * is.
	 */
	for (bm = &bmap[0]; bm->bm_kl != 0; bm++) {
		for (trb = &rb[0]; trb->rb_kl != 0; trb++) {
			if ((trb->rb_kl == bm->bm_kl) && (trb->rb_data != NULL)) {
				printf("@%s{", bm->bm_entry);
				goto out;
			}
		}
	}
out:
	if (bm->bm_kl == 0)
		printf("@misc{");

	/*
	 * in order of precedence; how to determine the
	 * bibtex key:
	 *	1. use keyword (%K) if only one word.
	 *	2. use refer label (%L).
	 *	3. otherwise just use the string "keyN" where N
	 *	   is the count of this bibliographic entry in
	 *	   the refer file.
	 */
	key++;
	for (trb = &rb[0]; trb->rb_kl != 0; trb++) {
		if ((trb->rb_kl == 'K') && (trb->rb_data != NULL)) {
			for (cp = trb->rb_data; *cp != NULL; cp++) {
				if (isspace(*cp))
					break;
			}

			/* ran to end of string? */
			if (*cp == NULL) {
				printf("%s,\n", trb->rb_data);

				/* if used here then free & zero it */
				(void) free(trb->rb_data);
				trb->rb_data = NULL;
				break;
			}
		}

		if ((trb->rb_kl == 'L') && (trb->rb_data != NULL)) {
			for (cp = trb->rb_data; *cp != NULL; cp++) {
				if (isspace(*cp))
					break;
			}

			/* ran to end of string? */
			if (*cp == NULL) {
				printf("%s,\n", trb->rb_data);
				break;
			}
		}
	}

	/* nothing reasonable to use, punt */
	if (trb->rb_kl == 0)
		printf("key%d,\n", key);

	first = 1;

	for (trb = &rb[0]; trb->rb_kl != 0; trb++) {
		if (trb->rb_data == NULL)
			continue;

		if (trb->rb_emit != 0) {
			/*
			 * clank,
			 * this is so that things will line up.
			 */
			if (strlen(trb->rb_kw) < 6)
				cp = "\t\t";
			else
				cp = "\t";

			if (! first)
				printf(",\n");

			printf("\t%s =%s\"%s\"", trb->rb_kw, cp, trb->rb_data);
			first = 0;
		}

		(void) free(trb->rb_data);
		trb->rb_data = NULL;
	}

	printf("\n}\n\n");
}

stuffrb(lrb, cp)
	struct rb	*lrb;
	char		*cp;
{
	extern char	*andfix();
	extern char	*malloc();
	extern char	*realloc();

	/* empty data field */
	if ((cp = sanz(cp)) == NULL)
		return;

	if (lrb->rb_kl == 'A')
		cp = andfix(cp);

	if (lrb->rb_data == NULL) {
		if ((lrb->rb_data = malloc(strlen(cp) + 1)) == NULL) {
			perror("malloc");
			exit(1);
		}

		strcpy(lrb->rb_data, cp);
	}
	else {
		char	*conj;

		if (lrb->rb_kl == 'A')
			conj = " and ";
		else
			conj = " ";

		if ((lrb->rb_data = realloc(lrb->rb_data, strlen(lrb->rb_data) + strlen(cp) + strlen(conj) + 1)) == NULL) {
			perror("realloc");
			exit(1);
		}

		strcat(lrb->rb_data, conj);
		strcat(lrb->rb_data, cp);
	}
}

/*
 */
char *
andfix(string)
	register char	*string;
{
	register char	*tmp;
	register char	*cp;

	tmp = string;

	for (cp = string; *cp != NULL; cp++) {
		if (strncmp(cp, " and ", 5) == 0) {
			/*
			 * +2 for the curly braces around "{and}",
			 * +1 for the null at the end.
			 */
			if ((tmp = malloc(strlen(string) + 2 + 1)) == NULL) {
				perror("malloc");
				exit(1);
			}

			strncpy(tmp, string, cp - string);
			tmp[cp - string] = NULL; /* strncpy doesn't */
			strcat(tmp, " {and} ");
			strcat(tmp, cp + 5);
		}
	}

	return(tmp);
}

char *
sanz(bp)
	char		*bp;
{
	register char	*cp;

	cp = &bp[strlen(bp) - 1];

	/*
	 * back up over any spaces chars
	 */
	while (isspace(*cp) && (cp >= bp))
		cp--;

	if (cp < bp)
		return(NULL);	/* empty line */

	*++cp = NULL;

	while (isspace(*bp) && (bp < cp))
		bp++;

	if (cp == bp)
		return(NULL);	/* empty line */

	return(bp);
}
