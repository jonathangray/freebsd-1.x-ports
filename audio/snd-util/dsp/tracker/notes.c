/* notes.c 
	vi:se ts=3 sw=3:
 */

/* $Id: notes.c,v 1.1 1994/02/19 16:03:08 ache Exp $
 * $Log: notes.c,v $
 * Revision 1.1  1994/02/19 16:03:08  ache
 * Initial revision
 *
 * Revision 4.0  1994/01/11  17:50:04  espie
 * Makes use of autoinit. Uses less memory, starts up faster.
 *
 * Revision 1.6  1994/01/09  17:36:22  Espie
 * Generalized open.c.
 *
 * Revision 1.5  1994/01/08  03:55:43  Espie
 * auto_init'd create_notes_table(),
 * suppressed note_name static table,
 * use name_of_note() instead (about 120 * 8 bytes gain).
 *
 * Revision 1.4  1994/01/05  16:10:49  Espie
 * *** empty log message ***
 *
 * Revision 1.3  1994/01/05  14:54:09  Espie
 * *** empty log message ***
 *
 * Revision 1.2  1994/01/05  13:50:43  Espie
 * Cosmetic change.
 *
 * Revision 1.1  1993/12/26  00:55:53  Espie
 * Initial revision
 *
 * Revision 3.6  1993/12/04  16:12:50  espie
 * New locals
 *
 * Revision 3.5  1993/11/17  15:31:16  espie
 * *** empty log message ***
 *
 * Revision 3.4  1993/11/11  20:00:03  espie
 * Amiga support.
 *
 * Revision 3.2  1992/11/20  14:53:32  espie
 * Added finetune.
 *
 * Revision 3.1  1992/11/19  20:44:47  espie
 * Protracker commands.
 *
 * Revision 3.0  1992/11/18  16:08:05  espie
 * New release.
 *
 */

#include "defs.h"

#ifdef MALLOC_NOT_IN_STDLIB
#include <malloc.h>
#else
#include <stdlib.h>
#endif
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <math.h>

#include "song.h"
#include "channel.h"
#include "extern.h"

ID("$Id: notes.c,v 1.1 1994/02/19 16:03:08 ache Exp $")


/* we can put it autoinit since find_note is ALWAYS called
 * prior to finding note values !
 */
LOCAL void create_notes_table P((void));
LOCAL void (*INIT)P((void)) = create_notes_table;


/* the musical notes correspond to some specific pitch.
 * It's useful to be able to find them back, at least for
 * arpeggii.
 */
short pitch_table[NUMBER_NOTES][NUMBER_FINETUNES];

LOCAL char *note_template = "C-C#D-D#E-F-F#G-G#A-A#B-";

/* find_note(pitch): find note corresponding to the stated pitch */
int find_note(pitch)
int pitch;
   {
   int a, b, i;
   
   INIT_ONCE;

   if (pitch == 0)
      return -1;
   a = 0;
   b = NUMBER_NOTES-1;
   while(b-a > 1)
      {
      i = (a+b)/2;
      if (pitch_table[i][0] == pitch)
         return i;
      if (pitch_table[i][0] > pitch)
         a = i;
      else
         b = i;
      }
   if (pitch_table[a][0] - FUZZ <= pitch)
      return a;
   if (pitch_table[b][0] + FUZZ >= pitch)
      return b;
   return NO_NOTE;
   }

LOCAL void create_notes_table()
   {
   double base, pitch;
   int i, j, k;

   for (j = -8; j < 8; j++)
      {
      k = j < 0 ? j + 16 : j;
      base = AMIGA_CLOCKFREQ/440.0/4.0 / pow(2.0, j/96.0);

      for (i = 0; i < NUMBER_NOTES; i++)
         {
         pitch = base / pow(2.0, i/12.0);
         pitch_table[i][k] = floor(pitch + 0.5);
         }
      }
    }

char *name_of_note(i)
int i;
   {
   static char name[4];

   if (i == NO_NOTE)
      return "   ";
   else 
      {
      name[0] = note_template[(i+9)%12 * 2];
      name[1] = note_template[(i+9)%12 * 2 +1];
      name[2] = '0' + (i-3)/12;
      name[3] = 0;
      return name;
      }
   }
   
int transpose_song(s, transpose)
struct song *s;
int transpose;
   {
   int oldt;
   int i, j, n;

   if (!s)
      return 0;
   oldt = s->info.transpose;
   for (n = 0; n < s->info.maxpat; n++)
      for (i = 0; i < BLOCK_LENGTH; i++)
         for (j = 0; j < NUMBER_TRACKS; j++)
            if (s->info.pblocks[n].e[j][i].note != NO_NOTE)
               s->info.pblocks[n].e[j][i].note += transpose - oldt;
   s->info.transpose = transpose;
   return oldt;
   }
