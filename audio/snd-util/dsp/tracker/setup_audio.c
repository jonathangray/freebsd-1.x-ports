/* setup_audio.c 
	vi:se ts=3 sw=3:
 */
/* higher level interface to the raw metal */

/* $Id: setup_audio.c,v 1.1 1994/02/19 16:03:08 ache Exp $
 * $Log: setup_audio.c,v $
 * Revision 1.1  1994/02/19 16:03:08  ache
 * Initial revision
 *
 * Revision 4.0  1994/01/11  17:55:28  espie
 * Use autoinit.
 *
 * Revision 1.7  1994/01/09  17:36:22  Espie
 * Generalized open.c.
 *
 * Revision 1.6  1994/01/08  02:04:21  Espie
 * Suppressed multiple at_end.
 *
 * Revision 1.5  1994/01/06  22:32:42  Espie
 * Use new pref scheme.
 *
 * Revision 1.4  1994/01/05  14:54:09  Espie
 * *** empty log message ***
 *
 * Revision 1.3  1993/12/27  00:45:26  Espie
 * Working.
 *
 * Revision 1.2  1993/12/26  18:54:21  Espie
 * Modified in a more consistent way.
 *
 * Revision 1.1  1993/12/26  00:55:53  Espie
 * Initial revision
 *
 * Revision 3.6  1993/12/04  16:12:50  espie
 * BOOL -> boolean.
 *
 * Revision 3.5  1993/11/17  15:31:16  espie
 * New high-level functions.
 *
 * Revision 3.4  1992/11/27  10:29:00  espie
 * General cleanup
 *
 * Revision 3.3  1992/11/24  10:51:19  espie
 * Added check before closing for the sgi.
 *
 * Revision 3.1  1992/11/20  14:53:32  espie
 * Added finetune.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "defs.h"
#include "extern.h"
#include "tags.h"
#include "prefs.h"

ID("$Id: setup_audio.c,v 1.1 1994/02/19 16:03:08 ache Exp $")

LOCAL void init_audio P((void));

LOCAL void (*INIT)P((void)) = init_audio;

LOCAL boolean opened = FALSE;
LOCAL int ask_freq, real_freq, oversample;
LOCAL boolean stereo;


LOCAL void init_audio()
   {
   at_end(do_close_audio);
   }

/* setup_audio(frequency, stereo, oversample):
 * try to avoid calling open_audio and other things
 * all the time
 */
void setup_audio(f, s, o)
int f;
boolean s;
int o;
   {
   INIT_ONCE;

   if (!opened)
      {
      ask_freq = f;
      stereo = s;
      oversample = o;
      real_freq = open_audio(f, s);
      init_player(o, real_freq);
      opened = TRUE;
      }
   else
      {
      int new_freq;

      if (s != stereo || f != ask_freq)
         {
         ask_freq = f;
         stereo = s;
         close_audio();
         new_freq = open_audio(f, s);
         }
      else
         new_freq = real_freq;

      if (new_freq != real_freq || oversample != o)
         {
         real_freq = new_freq;
         oversample = o;
         init_player(o, real_freq);
         }
      }
   set_synchro(get_pref_scalar(PREF_SYNC));
   }

void do_close_audio()
   {
   if (opened)
      {
      close_audio();
      }
   opened = FALSE;
   }

