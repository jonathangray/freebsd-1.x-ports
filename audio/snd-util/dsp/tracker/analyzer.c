/* analyzer.c 
	vi:set ts=3 sw=3:
*/


/* read module files and output statistics on them */

/* $Id: analyzer.c,v 1.1 1994/02/19 16:03:07 ache Exp $
 * $Log: analyzer.c,v $
 * Revision 1.1  1994/02/19 16:03:07  ache
 * Initial revision
 *
 * Revision 4.1  1994/01/12  16:10:20  espie
 * Fixed up last minute problems.
 *
 * Revision 4.0  1994/01/11  17:38:49  espie
 * Lots of changes.
 *
 * Revision 1.5  1994/01/09  17:36:22  Espie
 * Generalized open.c.
 *
 * Revision 1.4  1994/01/08  03:55:43  Espie
 * removed create_note_tables(), run_in_fg().
 *
 * Revision 1.3  1994/01/06  22:32:42  Espie
 * Use new pref scheme.
 *
 * Revision 1.2  1994/01/05  14:54:09  Espie
 * *** empty log message ***
 *
 * Revision 1.1  1993/12/26  00:55:53  Espie
 * Initial revision
 *
 * Revision 3.3  1993/12/04  16:12:50  espie
 * BOOL -> boolean.
 * New open_file semantics.
 *
 * Revision 3.2  1993/05/09  14:06:03  espie
 * Added speed check.
 *
 * Revision 3.1  1993/01/16  17:00:27  espie
 * Added patch for non termio.
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
#include "song.h"
#include "tags.h"
#include "prefs.h"

ID("$Id: analyzer.c,v 1.1 1994/02/19 16:03:07 ache Exp $")

int error;

struct song *do_read_song(name, type)
char *name;
int type;
   {
   struct song *song;
   struct exfile *file;

   file = open_file(name, "r", getenv("MODPATH"));
   if (!file)
      return NULL;
   song = read_song(file, type); 
   close_file(file);
   if (song)
      puts(name);
   return song;
   }


boolean use_command[16];
boolean use_extended[16];

void analyze_block(b)
struct block *b;
   {
   int i, j;
   struct event *e;

   for (i = 0; i < BLOCK_LENGTH; i++)
      {
      int special;

      special = 0;
      for (j = 0; j < NUMBER_TRACKS; j++)
         {
         e = &b->e[j][i];
         switch(e->effect)
            {
#if 0
         case 13: /* skip */
            return;
         case 11: /* fastskip */
            return;
#endif
         case 14:
            use_extended[HI(e->parameters)] = TRUE;
            break;
         case 15:
            if (special != 0 && e->parameters != special)
               putchar('!');
            else
               special = e->parameters;
         default:
            use_command[e->effect] = TRUE;
            }
         }
      }
   }


void analyze_song(song)
struct song *song;
   {
   int i;

   for (i = 0; i < NUMBER_SAMPLES; i++)
      {
      if (song->samples[i].start)
         {
         if (song->samples[i].finetune)
            printf("Sample %d: finetune is %d\n", 
               i, song->samples[i].finetune);
         }
      }
   for (i = 0; i < 16; i++)
      {
      use_command[i] = FALSE;
      use_extended[i] = FALSE;
      }
   for (i = 0; i < song->info.maxpat; i++)
      analyze_block(song->info.pblocks+i);
   for (i = 0; i < 16; i++)
      if (use_command[i])
         printf("%3d", i);
   for (i = 0; i < 16; i++)
      if (use_extended[i])
         printf("%3dE", i);
   printf("\n");
   }

int main(argc, argv)
int argc;
char **argv;
   {
   int i;

   struct song *song;
   int default_type;

   default_type = BOTH;
   set_pref_scalar(PREF_TOLERATE, 2);

   for (i = 1; i < argc; i++)
      {
      song = do_read_song(argv[i], NEW);
      if (!song && error != NEXT_SONG)
         song = do_read_song(argv[i], OLD);
      if (song)
         {
         analyze_song(song);
         release_song(song);
         }
      }
   }



