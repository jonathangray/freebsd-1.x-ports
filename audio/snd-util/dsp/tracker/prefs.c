/* prefs.c 
	vi:se ts=3 sw=3:
 */
/* $Id: prefs.c,v 1.1 1994/02/19 16:03:08 ache Exp $
 * $Log: prefs.c,v $
 * Revision 1.1  1994/02/19 16:03:08  ache
 * Initial revision
 *
 * Revision 4.0  1994/01/11  17:53:41  espie
 * *** empty log message ***
 *
 * Revision 1.3  1994/01/09  17:36:22  Espie
 * Generalized open.c.
 *
 * Revision 1.2  1994/01/07  15:06:26  Espie
 * VERY stupid bug.
 *
 * Revision 1.1  1994/01/06  22:32:42  Espie
 * Initial revision
 *
 */

#include <stdio.h>
#include "defs.h"
#include "extern.h"
#include "prefs.h"
#include "tags.h"

ID("$Id: prefs.c,v 1.1 1994/02/19 16:03:08 ache Exp $")
LOCAL void init_prefs P((void));

LOCAL void (*INIT)P((void)) = init_prefs;

LOCAL struct tag preferences[NUMBER_PREFS];

LOCAL void init_prefs()
   {
   int i;
   
   for (i = 0; i < NUMBER_PREFS; i++)
      preferences[i].type = BASE_PREFS + i;
   }

VALUE get_pref(index)
int index;
   {
   INIT_ONCE;

   return preferences[index-BASE_PREFS].data;
   }

void set_pref(index, value)
int index;
VALUE value;
   {
   preferences[index-BASE_PREFS].data = value;
   }

void set_pref_scalar(index, value)
int index;
int value;
   {
   VALUE temp;
   
   temp.scalar = value;
   set_pref(index, temp);
   }

int get_pref_scalar(index)
   {
   return get_pref(index).scalar;
   }

struct tag *get_prefs()
   {
   INIT_ONCE;

   return preferences;
   }
