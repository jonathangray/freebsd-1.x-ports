/* pref.h 
	vi:se ts=3 sw=3:
 */

/* $Id: pref.h,v 1.1 1994/02/19 16:03:08 ache Exp $
 * $Log: pref.h,v $
 * Revision 1.1  1994/02/19 16:03:08  ache
 * Initial revision
 *
 * Revision 3.4  1993/12/04  16:12:50  espie
 * BOOL -> boolean.
 *
 * Revision 3.3  1993/07/18  10:39:44  espie
 * suppressed show.
 *
 * Revision 3.2  1992/11/24  10:51:19  espie
 * Added show and sync.
 *
 * Revision 3.1  1992/11/19  20:44:47  espie
 * Protracker commands.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 *
 * Revision 2.7  1992/11/17  15:38:00  espie
 * Added dump_song.
 * imask, bcdvol.
 */

struct pref
    {
    int type, speed, tolerate, repeats;
    unsigned long imask;
    int bcdvol;
	int dump_song;
	boolean sync;
    };

