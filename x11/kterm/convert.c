/*
 *	convert.c -- code converters for kterm
 *	$Id: convert.c,v 1.1 1994/06/27 17:17:44 asami Exp $
 */

/*
 * Copyright (c) 1989  Software Research Associates, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Software Research Associates not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Software Research
 * Associates makes no representations about the suitability of this software
 * for any purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * Author:  Makoto Ishisone, Software Research Associates, Inc., Japan
 */
#include "kterm.h"

#ifdef KTERM

#define NULL	0
#define SS2	0x8E
#define CSI	0x9B

/* CS -> JIS using ESC-$-B */
int
convCStoJIS(cs, js)
Ichr		*cs;
unsigned char	*js;
{
	return convCStoANY(cs, js, NULL);
}

/* CS -> EUC */
static int
CStoEUC(cs_p, es_p)
Ichr		**cs_p;
unsigned char	**es_p;
{
	int		c1, c2;
	Ichr		*cs = *cs_p;
	unsigned char	*es = *es_p;
	if (cs->gset == GSET_KANA) {
		c1 = cs++->code;
		if (es) {
			*es++ = SS2;
			*es++ = c1 | 0x80;
		}
		*es_p = es;
		*cs_p = cs;
		return 2;
	} else if (cs->gset == GSET_KANJI
		|| cs->gset == GSET_OLDKANJI) {
		c1 = cs++->code;
		c2 = cs++->code;
		if (es) {
			*es++ = c1 | 0x80;
			*es++ = c2 | 0x80;
		}
		*es_p = es;
		*cs_p = cs;
		return 2;
	}
	return 0;
}

int
convCStoEUC(cs, es)
Ichr		*cs;
unsigned char	*es;
{
	return convCStoANY(cs, es, CStoEUC);
}

/* CS -> SJIS */
static int
CStoSJIS(cs_p, ss_p)
Ichr		**cs_p;
unsigned char	**ss_p;
{
	int		c1, c2;
	Ichr		*cs = *cs_p;
	unsigned char	*ss = *ss_p;
	if (cs->gset == GSET_KANA) {
		c1 = cs++->code;
		if (ss) {
			*ss++ = c1 | 0x80;
		}
		*ss_p = ss;
		*cs_p = cs;
		return 1;
	} else if (cs->gset == GSET_KANJI || cs->gset == GSET_OLDKANJI) {
		c1 = cs++->code;
		c2 = cs++->code;
		if (ss) {
			*ss++ = (c1 - 0x21) / 2 + ((c1 <= 0x5e) ? 0x81 : 0xc1);
			if (c1 & 1)	/* odd */
			    *ss++ = c2 + ((c2 <= 0x5f) ? 0x1f : 0x20);
			else
			    *ss++ = c2 + 0x7e;
		}
		*ss_p = ss;
		*cs_p = cs;
		return 2;
	}
	return 0;
}

int
convCStoSJIS(cs, ss)
Ichr		*cs;
unsigned char	*ss;
{
	return convCStoANY(cs, ss, CStoSJIS);
}

/* CS -> any */
int
convCStoANY(cs, as, func)
Ichr		*cs;
unsigned char	*as;
int		(*func)();
{
	register int	c1, c2;
	register int	gset = GSET_ASCII;
	register int	n = 0, m;

	while (c1 = cs->code) {
		if (func && (m = (*func)(&cs, &as))) {
			n += m;
			continue;
		}
		if (gset != cs->gset) {
			if (gset & CS96 && !(cs->gset & CS96)) {
				if (as)
					*as++ = '\025'; /* SI */
				n++;
			} else if (!(gset & CS96) && cs->gset & CS96) {
				if (as)
					*as++ = '\024'; /* SO */
				n++;
			}
			/*
			 *	A HACK FOR JISROMAN
			 */
			if (cs->gset == GSET_ASCII
			 || cs->gset == GSET_JISROMAN) {
				if (gset != GSET_ASCII
				 && gset != GSET_JISROMAN) {
					if (as) {
						*as++ = '\033';
						*as++ = '(';
						*as++ = GSETFC(cs->gset);
					}
					n += 3;
				}
			} else if (cs->gset == GSET_KANJI
			/*
			 * if (cs->gset == GSET_KANJI
			 */
			 || cs->gset == GSET_OLDKANJI) {
				if (as) {
					*as++ = '\033';
					*as++ = '$';
					*as++ = GSETFC(cs->gset);
				}
				n += 3;
			} else {
				if (as) {
					*as++ = '\033';
					if (cs->gset & MBCS) {
						*as++ = '$';
					}
					if (cs->gset & CS96) {
						*as++ = '-';
					} else {
						*as++ = '(';
					}
					*as++ = GSETFC(cs->gset);
				}
				n += 3;
				if (cs->gset & MBCS)
					n ++;
			}
			gset = cs->gset;
		}
		cs++;
		if (gset & MBCS) {
			c2 = cs++->code;
			if (as) {
				*as++ = c1 & ~0x80;
				*as++ = c2 & ~0x80;
			}
			n += 2;
		} else {
			if (as)
				*as++ = c1 & ~0x80;
			n++;
		}
	}
	/*
	 *	A HACK FOR JISROMAN
	 * if (gset != GSET_ASCII) {
	 */
	if (gset != GSET_ASCII && gset != GSET_JISROMAN) {
		if (as) {
			*as++ = '\033';
			*as++ = '(';
			*as++ = 'B';
		}
		n += 3;
	}
	if (as)
		*as = '\0';

	return n;
}

/* CS -> ISO Latin-1 */
int
convCStoLatin1(cs, ls)
Ichr *cs;
unsigned char *ls;
{
	register int	c;
	register int	n = 0;

	if (ls) {
		while (c = cs->code) {
			if (cs++->gset == GSET_ASCII) {
				*ls++ = c & 0x7f;
				n++;
			}
		}
		*ls = '\0';
	} else {
		while (c = cs->code) {
			if (cs++->gset == GSET_ASCII) {
				n++;
			}
		}
	}
	return n;
}

/******************************************************************************
COMPOUND_TEXT Summary
  (based on Comopund Text Encoding Version 1 -- MIT X Consortium Standard)
(1) Only G0 and G1 are used. G2 and G3 are not.
(2) G0 is invoked into GL and G1 into GR. These invocation are not changed.
	(In other words, Locking Shift and Single Shift are not used)
(3) In initial state, ISO Latin-1 is designated into G0/G1.
(4) To designate MBCS into G0, ESC-$-F is not used but ESC-$-(-F.
(5) In C0, only HT, NL, and ESC are used.
(6) In C1, only CSI is used.
(7) Text direction can be indecated.
	begin left-to-right string
	begin right-to-left string
	end of string
******************************************************************************/

/* convCStoCT -- Japanese Wide Character String -> COMPOUND_TEXT */
int
convCStoCT(cs, as)
register Ichr *cs;
register unsigned char *as;
/* Convert Wide Character String cs to COMPOUND_TEXT xstr, return
 * length of xstr in bytes (not including the terminating null character).
 * If xstr is NULL, no conversion is done, but return length of xstr.
 */
{
	register int	c1, c2;
	register int	g0 = GSET_ASCII;
	register int	g1 = GSET_LATIN1R;
	register int	n = 0;

	while (c1 = cs->code) {
	    if (cs->gset & CS96
	     || cs->gset & MBCS
	     || cs->gset == GSET_KANA) {
		if (g1 != cs->gset) {
			g1 = cs->gset;
			if (as) {
				*as++ = '\033';
				if (g1 & MBCS) {
					*as++ = '$';
				}
				if (g1 & CS96) {
					*as++ = '-';
				} else {
					*as++ = ')';
				}
				*as++ = GSETFC(g1);
			}
			n += 3;
			if (g1 & MBCS)
				n ++;
		}
		cs++;
		if (g1 & MBCS) {
			c2 = cs++->code;
			if (as) {
				*as++ = c1 | 0x80;
				*as++ = c2 | 0x80;
			}
			n += 2;
		} else {
			if (as)
				*as++ = c1 | 0x80;
			n++;
		}
	    } else {
		if (g0 != cs->gset) {
			g0 = cs->gset;
			if (as) {
				*as++ = '\033';
				*as++ = '(';
				*as++ = GSETFC(g0);
			}
			n += 3;
		}
		cs++;
		if (as)
			*as++ = c1 & ~0x80;
		n++;
	    }
	}
	if (g0 != GSET_ASCII) {
		if (as) {
			*as++ = '\033';
			*as++ = '(';
			*as++ = 'B';
		}
		n += 3;
	}
	if (as)
		*as = '\0';

	return n;
}

static unsigned char *
getesc(str, len)
unsigned char *str;
int len;
{
	register int	c;

	/* Find intermediate characters and final character
	 * following the escape character in an escape sequence.
	 */
	/* The intermediate character is 02/00 to 02/15 */
	while (len > 0) {
		c = *str;
		if (c < 0x20 || 0x2f < c)
			break;
		len--, str++;
	}
	/* The final character is 03/00 to 07/14 */
	if (--len < 0 || (c = *str++) < 0x30 || 0x7e < c)
		return (unsigned char *)NULL;

	return str;
}

static unsigned char *
getcsi(str, len)
unsigned char *str;
int len;
{
	register int	c;

	/* Find parameter characters, intermediate characters
	 * and final character following the CSI character
	 * in a CSI sequence.
	 */
	/* The parameter characters is 03/00 to 03/15 */
	while (len > 0) {
		c = *str;
		if (c < 0x30 || 0x3f < c)
			break;
		len--, str++;
	}
	/* The intermediate character is 02/00 to 02/15 */
	while (len > 0) {
		c = *str;
		if (c < 0x20 || 0x2f < c)
			break;
		len--, str++;
	}
	/* The final character is 04/00 to 07/14 */
	if (--len < 0 || (c = *str++) < 0x40 || 0x7e < c)
		return (unsigned char *)NULL;

	return str;
}

/* convCTtoCS -- COMPOUND_TEXT -> Japanese Wide Character String */
int
convCTtoCS(xstr, len, cs)
register unsigned char *xstr;
int len;
Ichr *cs;
/* Convert COMPOUND_TEXT xstr to Wide Character String cs, return
 * length of cs in characters (not including the terminating null character).
 * If cs is NULL, no conversion is done, but return length of cs.
 */
{
	register int	c;
	int	nskip;
	int	n = 0;
	int	g0, g1, gs;
	unsigned char	*xstr1;

	/*
	 * Compound Text can include null octet. Therefore the length
	 * of xstr is able to be specified by parameter len.
	 * But if len is zero or negative, get length by strlen() assuming
	 * that no null octet exists.
	 */
	if (len <= 0) {
		len = strlen((char *)xstr);
	}

	/* In initial state, ISO 8859/1 is designated into G0/G1 */
	g0 = GSET_ASCII;	/* ASCII -> G0 */
	g1 = GSET_LATIN1R;	/* Latin/1 right hand part -> G1 */

	while (len-- > 0) {
		switch (c = *xstr++) {
		case '\n':	/* NEWLINE */
		case '\t':	/* TAB */
		case ' ':	/* SPACE (Note: GL is always 94 charset) */
			if (cs) {
				cs->code = c;
				cs->gset = GSET_ASCII;
				cs++;
			}
			n++;
			break;
		case 0x9b:	/* CSI */
			/*
			 * CSI sequence is generally in following form:
			 *	CSI {P} {I} F
			 *        P : 03/00 to 03/15
			 *        I : 02/00 to 02/15
			 *        F : 04/00 to 07/14
			 */
			/*
			 * Currently only directionality is definde
			 * as following:
			 *	CSI-1-]		begin left-to-right text
			 *	CSI-2-]		begin right-to-left text
			 *	CSI-]		end of string
			 * But this implementation ignores them.
			 */
			xstr1 = getcsi(xstr, len);
			if (xstr1 == NULL)
				return -1;
			len -= xstr1 - xstr;
			xstr = xstr1;
			break;
		case '\033':	/* ESC */
			/*
			 * ESC sequence is generally in following form:
			 *	ESC {I} F
			 *        I : 02/00 to 02/15
			 *        F : 03/00 to 07/14
			 */
			/*
			 * Currently, following functions are defined:
			 *   Standard character set
			 *	ESC-(-F
			 *	ESC-$-(-F
			 *	ESC-)-F
			 *	ESC---F
			 *	ESC-$-)-F
			 *   Non standard character set
			 *	ESC-%-/-[0123]
			 * Standard character set must be accepted correctly.
			 * Non standard one is ignored but must be parsed
			 * for skipping data.
			 */
			xstr1 = getesc(xstr, len);
			if (xstr1 == NULL)
				return -1;
			len -= xstr1 - xstr;
			switch (xstr1 - xstr) {
			case 2:		/* ESC - I - F */
				switch (*xstr++) {
				case '(':	/* 94chars CS -> G0 */
					g0 = GSET(*xstr);
					break;
				case ')':	/* 94chars CS -> G1 */
					g1 = GSET(*xstr);
					break;
				case '-':	/* 96chars CS -> G1 */
					g1 = GSET(*xstr) | CS96;
					break;
				default:	/* ignore */
					break;
				}
				break;
			case 3:		/* ESC - I - I - F */
				switch (*xstr++) {
				case '$':
					switch (*xstr++) {
					case '(':	/* 94chars MBCS -> G0 */
						g0 = GSET(*xstr) | MBCS;
						break;
					case ')':	/* 94chars MBCS -> G1 */
						g1 = GSET(*xstr) | MBCS;
						break;
					case '-':	/* 96chars MBCS -> G1 */
						g1 = GSET(*xstr) | CS96 | MBCS;
						break;
					default:	/* ignore */
						break;
					}
					break;
				case '%':
					if (*xstr++ != '/') {
						/* unknown sequence */
						break;
					}
					/*
					 * Private encoding is ignored.
					 * But following data must be skipped.
					 *	ESC-%-/-F-M-L
					 */
					len -= 2;
					if (len < 0)
						return -1;
					nskip = (*xstr1 & 0x7f) * 128 +
					    (*(xstr1 + 1) & 0x7f);
					if ((len -= nskip) < 0)
						return -1;
					xstr1 += nskip + 2;
					break;
				default:
					break;
				}
				break;
			default:
				break;
			}
			xstr = xstr1;
			break;
		default:
			if (!(c & 0x60)) {
				/*
				 * Non NL/TAB/ESC/CSI character in C0 or C1
				 * is an obvious error.
				 */
				return -1;
			}
			gs = (c & 0x80) ? g1 : g0;
			if (cs) {
				cs->code = c & ~0x80;
				cs->gset = gs;
				cs++;
			}
			n++;
			break;
		}
	}
	if (cs) {
		cs->code = 0;
		cs->gset = 0;
	}
	return n;
}
#endif
