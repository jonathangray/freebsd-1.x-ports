static char		*psfontnames[] = {
			"Times-Roman", "Times-Roman",	/* default */
			"Times-Roman",			/* roman */
			"Times-Bold",			/* bold */
			"Times-Italic",			/* italic */
			"Helvetica",			/* sans serif */
			"Courier"			/* typewriter */
		};

#define PS_FONTNAMES(T)	\
  	(((v2_flag&&!v21_flag) || psfont_text(T)) ? PSfontnames : psfontnames)

#define PSFONT(T) \
 ((T->font) <= MAXFONT(T) ? PS_FONTNAMES(T)[T->font+1] : PS_FONTNAMES(T)[0])

#define PSFONTMAG(T)	(((T->size) > 0 ? \
				((T->size) <= ULIMIT_FONT_SIZE ? \
			 		(T->size) : \
					ULIMIT_FONT_SIZE) : \
				font_size)/(rigid_text(T) ? mag : 1.0))
