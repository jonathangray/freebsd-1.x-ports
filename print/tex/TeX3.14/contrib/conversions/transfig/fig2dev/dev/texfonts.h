static char		*texfontnames[] = {
			"rm", "rm",		/* default */
			"rm",			/* roman */
			"bf",			/* bold */
			"it",			/* italic */
			"sf", 			/* sans serif */
			"tt"			/* typewriter */
		};

/* The selection of font names may be site dependent.
 * Not all fonts are preloaded at all sizes.
 */

static char		*texfontsizes[] = {
			"elv", "elv",		/* default */
			"fiv", "fiv", "fiv", "fiv", 	/* small fonts */
			"fiv",			/* five point font */
			"six", "sev", "egt",	/* etc */
			"nin", "ten", "elv",
			"twl", "twl", "frtn",	
			"frtn", "frtn", "svtn",
			"svtn", "svtn", "twty",
			"twty", "twty", "twty", "twty", "twfv"
			};
#define MAXFONTSIZE 25

#define TEXFONT(F)	(texfontnames[((F) <= MAX_FONT) ? (F)+1 : MAX_FONT])
#define TEXFONTSIZE(S)	(texfontsizes[((S) <= MAXFONTSIZE) ? round(S)+1\
				      				: MAXFONTSIZE])
#define TEXFONTMAG(T)	TEXFONTSIZE(T->size*(rigid_text(T) ? 1.0 : mag))

static char		*texstylesizes[] = {
			"xi", "xi",		/* default */
			"v", "v", "v", "v", 	/* small fonts */
			"v",			/* five point font */
			"vi", "vii", "viii",	/* etc */
			"ix", "x", "xi",
			"xii", "xii", "xiv",	
			"xiv", "xiv", "xvii",
			"xvii", "xvii", "xx",
			"xx", "xx", "xx", "xx", "xv"
			};

#define TEXSTYLESIZE(S)	(texstylesizes[((S) <= MAXFONTSIZE) ? round(S)+1\
				      				: MAXFONTSIZE])
#define TEXSTYLEMAG(T)	TEXSTYLESIZE((int)(T->size*(rigid_text(T) ? 1.0 : mag)))
