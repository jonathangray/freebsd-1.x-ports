#include "config.h"
#include "dvi.h"

/***
 ***	VF font reading routines.
 ***	Public routine is read_index---because virtual characters are presumed
 ***	to be short, we read the whole virtual font in at once, instead of
 ***	faulting in characters as needed.
 ***/

#define	LONG_CHAR	242

/*
 *	These are parameters which determine whether macros are combined for
 *	storage allocation purposes.  Small macros ( <= VF_PARM_1 bytes) are
 *	combined into chunks of size VF_PARM_2.
 */

#ifndef	VF_PARM_1
#define	VF_PARM_1	20
#endif
#ifndef	VF_PARM_2
#define	VF_PARM_2	256
#endif

/*
 *	The main routine
 */

void
read_VF_index(fontp)
	register struct font *fontp;
{
	FILE	*VF_file = fontp->file;
	ubyte	cmnd;
	ubyte	*avail, *availend;	/* available space for macros */

	fontp->read_char = NULL;
	fontp->flags |= FONT_VIRTUAL;
	fontp->set_char_p = set_vf_char;
	if (debug & DBG_PK)
	    Printf("Reading VF pixel file %s\n", fontp->filename);
/*
 *	Read preamble.
 */
	Fseek(VF_file, (long) one(VF_file), 1);	/* skip comment */
	(void) four(VF_file);		/* skip checksum */
	(void) four(VF_file);		/* skip design size */
/*
 *	Read the fonts.
 */
	fontp->vf_chain = NULL;
	fontp->first_font = NULL;
	while ((cmnd = one(VF_file)) >= FNTDEF1 && cmnd <= FNTDEF4) {
	    define_font(VF_file, cmnd, fontp, &fontp->vf_chain);
	    if (fontp->first_font == NULL)
		fontp->first_font = fontp->vf_chain->fontp;
	}
/*
 *	Prepare macro array.
 */
	fontp->macro = (struct macro *) xmalloc(256 * sizeof(struct macro),
	    "macro array");
	bzero((char *) fontp->macro, 256 * sizeof(struct macro));
/*
 *	Read macros.
 */
	avail = availend = NULL;
	for (; cmnd <= LONG_CHAR; cmnd = one(VF_file)) {
	    register struct macro *m;
	    int len;
	    unsigned long cc;
	    long width;

	    if (cmnd == LONG_CHAR) {	/* long form packet */
		len = four(VF_file);
		cc = four(VF_file);
		width = four(VF_file);
		if (cc >= 256) {
		    Fprintf(stderr,
			"Virtual character %lu in font %s ignored.\n",
			cc, fontp->fontname);
		    Fseek(VF_file, (long) len, 1);
		    continue;
		}
	    }
	    else {	/* short form packet */
		len = cmnd;
		cc = one(VF_file);
		width = num(VF_file, 3);
	    }
	    m = &fontp->macro[cc];
	    m->dvi_adv = width * fontp->dimconv;
	    if (len > 0) {
		if (len <= availend - avail) {
		    m->pos = avail;
		    avail += len;
		}
		else {
		    m->free_me = True;
		    if (len <= VF_PARM_1) {
			m->pos = avail = (ubyte *) xmalloc(VF_PARM_2,
			    "macro array");
			availend = avail + VF_PARM_2;
			avail += len;
		    }
		    else m->pos = (ubyte *) xmalloc((unsigned) len,
			"macro array");
		}
		Fread((char *) m->pos, 1, len, VF_file);
		m->end = m->pos + len;
	    }
	    if (debug & DBG_PK)
		Printf("Read VF macro for character %lu; dy = %ld, length = %d\n",
			cc, m->dvi_adv, len);
	}
	if (cmnd != POST)
	    oops("Wrong command byte found in VF macro list:  %d", cmnd);
}
