/*
 * definitions internal to the header modules (hdr*.c)
 */

#ifndef DEFDIST
#define DEFDIST "world"		/* default Distribution: */
#endif

struct hdrdef {
	char *hdrnm;		/* ascii name */
	unsigned hdrlen;	/* STRLEN(hdrnm) */
	int hdroff;		/* offset into struct header */
};
typedef struct hdrdef *hdrlist[];

extern struct hdrdef pathhdr, xrefhdr;
extern hdrlist reqdhdrs, opthdrs, hdrvilest;

extern boolean headdebug;
