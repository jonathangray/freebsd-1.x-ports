/* prefs.h 
	vi:se ts=3 sw=3:
 */

/* $Id: prefs.h,v 1.1 1994/02/19 16:03:08 ache Exp $
 * $Log: prefs.h,v $
 * Revision 1.1  1994/02/19 16:03:08  ache
 * Initial revision
 *
 * Revision 4.0  1994/01/11  17:54:52  espie
 * *** empty log message ***
 *
 * Revision 1.1  1994/01/06  22:32:42  Espie
 * Initial revision
 *
 */

#define BASE_PREFS      50
#define PREF_TYPE       BASE_PREFS
#define PREF_SPEED      (BASE_PREFS+1)
#define PREF_TOLERATE   (BASE_PREFS+2)
#define PREF_REPEATS    (BASE_PREFS+3)
#define PREF_IMASK      (BASE_PREFS+4)
#define PREF_BCDVOL     (BASE_PREFS+5)
#define PREF_DUMP       (BASE_PREFS+6)
#define PREF_SYNC       (BASE_PREFS+7)
#define PREF_SHOW       (BASE_PREFS+8)

#define NUMBER_PREFS    (PREF_SHOW - BASE_PREFS + 1)

XT VALUE get_pref P((int index));
XT void set_pref P((int index, VALUE value));
XT int get_pref_scalar P((int index));
XT void set_pref_scalar P((int index, int value));
XT struct tag *get_prefs P((void));
