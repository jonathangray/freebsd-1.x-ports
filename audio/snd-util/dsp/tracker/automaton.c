/* automaton.c 
	vi:se ts=3 sw=3:
 */

/* $Id: automaton.c,v 1.1 1994/02/19 16:03:07 ache Exp $
 * $Log: automaton.c,v $
 * Revision 1.1  1994/02/19 16:03:07  ache
 * Initial revision
 *
 * Revision 4.0  1994/01/11  17:42:13  espie
 * Abstracted IO calls.
 *
 * Revision 1.5  1994/01/09  23:24:37  Espie
 * Last bug fix.
 *
 * Revision 1.4  1994/01/09  17:36:22  Espie
 * Generalized open.c.
 *
 * Revision 1.3  1994/01/05  14:54:09  Espie
 * *** empty log message ***
 *
 * Revision 1.2  1993/12/28  13:54:44  Espie
 * Use display_pattern.
 *
 * Revision 1.1  1993/12/26  00:55:53  Espie
 * Initial revision
 *
 * Revision 3.16  1993/12/04  16:12:50  espie
 * *** empty log message ***
 *
 * Revision 3.15  1993/11/17  15:31:16  espie
 * *** empty log message ***
 *
 * Revision 3.13  1993/07/18  10:39:44  espie
 * Fixed up repeat code, should work better now.
 *
 * Revision 3.12  1993/07/17  22:23:41  espie
 * Fixed bug with bad loops.
 *
 * Revision 3.9  1993/05/09  14:06:03  espie
 * Modified the way set_speed works.
 *
 * Revision 3.8  1993/01/16  17:00:27  espie
 * Corrected stupid bug (run_in_fg)
 *
 * Revision 3.7  1993/01/15  14:00:28  espie
 * Added bg/fg test.
 *
 * Revision 3.6  1992/11/27  10:29:00  espie
 * General cleanup
 *
 * Revision 3.5  1992/11/24  10:51:19  espie
 * un#ifdef'ed showseq code.
 *
 * Revision 3.3  1992/11/22  17:20:01  espie
 * Simplified delay_pattern.
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
 * Revision 2.16  1992/11/17  17:15:37  espie
 * New output for new interface
 * Modified repeat logic: now works irregardless of repeat points.
 * start
 *
 * Revision 2.8  1992/07/14  14:23:41  espie
 * Changed fine speed command and comments.
 * Added two level of fault tolerancy.
 */
     

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
     
#include "defs.h"
#include "song.h"
#include "channel.h"
#include "extern.h"
     
ID("$Id: automaton.c,v 1.1 1994/02/19 16:03:07 ache Exp $")
     

LOCAL void clear_repeats(a, from, upto)
struct automaton *a;
int from, upto;
   {
   int i;

   for (i = from; i <= upto; i++)
      a->gonethrough[i] = FALSE;
   }

LOCAL void reset_repeats(a)
struct automaton *a;
   {
   clear_repeats(a, 0, a->info->length);
   a->gonethrough[a->info->length] = TRUE;
   }

/* updates the pattern to play in the automaton.
 * Checks that the pattern actually exists.
 * Checks for repetitions as well.
 */
LOCAL void set_pattern(a)
struct automaton *a;
   {
   int p;


   if (a->pattern_num >= a->info->length)
      {
      error = UNRECOVERABLE;
      return;
      }

   if (a->gonethrough[a->pattern_num])
      {
      error = ENDED;
      reset_repeats(a);
      }
   else
      a->gonethrough[a->pattern_num] = TRUE;

   display_pattern(a->pattern_num, a->info->length);

      /* there is a level of indirection in the format,
       * i.e., patterns can be repeated.
       */
   p = a->info->patnumber[a->pattern_num];
   if (p >= a->info->maxpat)
      {
      error = UNRECOVERABLE;
      return;
      }
   a->pattern = a->info->pblocks + p;
   }

/* initialize all the fields of the automaton necessary
 * to play a given song.
 */
void init_automaton(a, song, start)
struct automaton *a;
struct song *song;
int start;
   {
   a->info = &song->info;
   a->pattern_num = start;    /* first pattern */

   a->loop_note_num = 0;
   a->loop_counter = 0;

   reset_repeats(a);

   a->note_num = 0;           /* first note in pattern */
   a->counter = 0;            /* counter for the effect tempo */
   a->speed = NORMAL_SPEED;   /* this is the default effect tempo */
   a->finespeed = NORMAL_FINESPEED;    
                              /* this is the fine speed (100%=NORMAL_FINESPEED) */
   a->do_stuff = DO_NOTHING;   
                              /* some effects affect the automaton,
                               * we keep them here.
                               */

   error = NONE;              /* Maybe we should not reset errors at
                               * this point.
                               */
   set_pattern(a);
   }

/* Gets to the next pattern, and displays stuff */
LOCAL void advance_pattern(a)
struct automaton *a;
   {
   if (++a->pattern_num >= a->info->length)
      a->pattern_num = 0;
   set_pattern(a);
   a->note_num = 0;
   }

        

/* process all the stuff which we need to advance in the song,
 * including set_speed, set_skip, set_fastskip, and set_loop.
 */
void next_tick(a)
struct automaton *a;
   {
      /* there are three classes of speed changes:
       * 0 does nothing. (should stop)
       * <32 is the effect speed (resets the fine speed).
       * >=32 changes the finespeed, default 125
       */
   if ((a->do_stuff & SET_SPEED) && (a->do_stuff & SET_FINESPEED))
      {
      a->speed = a->new_speed;
      a->finespeed = a->new_finespeed;
      }
   else if (a->do_stuff & SET_FINESPEED)
      {
      a->finespeed = a->new_finespeed;
      }
   else if (a->do_stuff & SET_SPEED)
      {
      a->speed = a->new_speed;
      a->finespeed = NORMAL_FINESPEED;
      }

   if (++a->counter >= a->speed)
      {
      a->counter = 0;
         /* loop: may change note in pattern right away */
      if ((a->do_stuff & JUMP_PATTERN) && --a->loop_counter > 0)
         a->note_num = a->loop_note_num;
      else if (a->do_stuff & SET_FASTSKIP)
         {
         a->pattern_num = a->new_pattern;
         set_pattern(a);
         a->note_num = 0;
         }
      else if (a->do_stuff & SET_SKIP)
         {
         advance_pattern(a);
         a->note_num = a->new_note;
         }
      else
         {
         if (++a->note_num >= BLOCK_LENGTH)
            advance_pattern(a);
         }
      a->do_stuff = DO_NOTHING;
      }
   }


