/*
 *	$Id: kterm.h,v 1.1 1994/06/27 17:17:44 asami Exp $
 */

/*
 * Copyright 1988, 1989, 1990, 1991 and 1992
 * XXI working group in Japan Unix Society(XXI).
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of XXI not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.  XXI makes no representations
 * about the suitability of this software for any purpose.
 * It is provided "as is" without express or implied warranty.
 * 
 * XXI DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT
 * SHALL XXI.  BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
 * OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 * 
 * Author:
 * 	Hiroto Kagotani
 * 	Dept. of Computer Science
 *	Tokyo Institute of Technology
 * 	2-12-2 Ookayama, Meguro-ku Tokyo 152 Japan
 * 	kagotani@cs.titech.ac.jp
 */ 

#ifndef _KTERM_H_
#define _KTERM_H_
#ifdef KTERM

#define KTERM_VERSION "5.2.0"
#define KTERM_MBCS /* multi-byte character set sequence support (only Kanji in this release) */
#define KTERM_KANJI /* Kanji specific functions support */
#define KTERM_KCONV /* talk with {ROMAJI,KANA}->KANJI converter */
#define KTERM_COLOR /* accept color sequences */
#define KTERM_MBCC /* multi-byte character class support for word selection */

/* gset of Ichr,Bchr */
#define CS96		0x80 /* character sets which have 96 characters */
#define MBCS		0x40 /* multi-byte character sets */
#define MBC2		0x7f /* second byte of a mbcs character */
  /*
   * there is no character set using designating character less than ' '.
   * but who can say there is no set greater than '_' (0x3f + ' ').
   */
#define GSET(c)		((c) - ' ')
#define GSETFC(i)	(((i) & ~(MBCS|CS96)) + ' ')
		/* final character of a designation sequense for a gset */
/* code of Ichr,Bchr */
#define NEEDMAP		0x80
typedef struct {
	unsigned char gset;
	unsigned char code;
} Ichr; /* char for interchanging with other processes */

#define GSET_GRAPH	GSET('0')
#define GSET_UK		GSET('A')
#define GSET_ASCII	GSET('B')
#define GSET_JISROMAN	GSET('J')
#define GSET_KANA	GSET('I')
#define GSET_LATIN1R	(CS96|GSET('A'))

#ifdef KTERM_MBCS
# define GSET_KANJI	(MBCS|GSET('B'))
# define GSET_OLDKANJI	(MBCS|GSET('@'))
#endif /* KTERM_MBCS */

#endif /* KTERM */
#endif /* !_KTERM_H_ */
