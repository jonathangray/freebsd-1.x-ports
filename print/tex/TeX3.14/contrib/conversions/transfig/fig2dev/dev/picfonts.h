/*  The selection of font names may be site dependent */

char		*picfontnames[] = {
			"R", "R",		/* default */
			"R",			/* roman */
			"B",			/* bold */
			"I",			/* italic */
			"H",			/* sans serif */
			"C"			/* typewriter */
		};
#define MAXFONTSIZE 108

#define PICFONT(F)	(picfontnames[((F) <= MAX_FONT) ? (F)+1 : MAX_FONT])
#define PICFONTSIZE(S)  ((S) > 0 ? \
				((S) <= MAXFONTSIZE ? \
					round(S) : \
					MAXFONTSIZE) : \
				font_size)
#define PICFONTMAG(T)	PICFONTSIZE((int)(T->size*(rigid_text(T) ? 1.0 : mag)))
