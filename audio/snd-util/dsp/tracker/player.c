/* player.c 
	vi:se ts=3 sw=3:
 */

/* $Id: player.c,v 1.1 1994/02/19 16:03:08 ache Exp $
 * $Log: player.c,v $
 * Revision 1.1  1994/02/19 16:03:08  ache
 * Initial revision
 *
 * Revision 4.0  1994/01/11  17:51:40  espie
 * Use the new UI calls.
 * Use the new pref settings.
 *
 * Revision 1.14  1994/01/09  23:24:37  Espie
 * Last bug fix.
 *
 * Revision 1.13  1994/01/09  17:36:22  Espie
 * Generalized open.c.
 *
 * Revision 1.12  1994/01/08  03:55:43  Espie
 * Suppressed really outdated code.
 * New names: new_channel_tag_list, release_audio_channel.
 *
 * Revision 1.11  1994/01/08  02:04:21  Espie
 * Some notice to status.
 *
 * Revision 1.10  1994/01/07  15:06:26  Espie
 * *** empty log message ***
 *
 * Revision 1.9  1994/01/06  22:32:42  Espie
 * Use new pref scheme.
 *
 * Revision 1.8  1994/01/05  16:10:49  Espie
 * *** empty log message ***
 *
 * Revision 1.7  1994/01/05  14:54:09  Espie
 * *** empty log message ***
 *
 * Revision 1.6  1994/01/05  13:50:43  Espie
 * Use get_ui
 *
 * Revision 1.5  1994/01/05  01:59:14  Espie
 * Added prototypes.
 *
 * Revision 1.4  1993/12/28  13:54:44  Espie
 * Use autoinit feature of display.c
 *
 * Revision 1.3  1993/12/27  02:35:02  Espie
 * discard_buffer forgotten...
 *
 * Revision 1.2  1993/12/26  18:54:21  Espie
 * Handle errors better.
 *
 * Revision 1.1  1993/12/26  00:55:53  Espie
 * Initial revision
 *
 * Revision 3.18  1993/12/04  16:12:50  espie
 * Lots of changes.
 *
 * Revision 3.17  1993/11/19  14:27:06  espie
 * Stupid bug.
 *
 * Revision 3.16  1993/11/17  15:31:16  espie
 * New high-level functions.
 *
 * Revision 3.15  1993/11/11  20:00:03  espie
 * Amiga support.
 *
 * Revision 3.14  1993/08/04  11:34:33  espie
 * *** empty log message ***
 *
 * Revision 3.13  1993/07/18  11:49:29  espie
 * Bug with delay_pattern: can't factorize the check for effect thingy.
 *
 * Revision 3.12  1993/07/18  10:39:44  espie
 * Added forking under unix.
 *
 *
 * Revision 3.10  1993/05/09  14:06:03  espie
 * Reniced verbose output display.
 *
 * Revision 3.9  1993/04/25  14:08:15  espie
 * Bug fix: now use correct finetune when loading samples/starting notes.
 *
 * Revision 3.8  1993/01/15  14:00:28  espie
 * Added bg/fg test.
 *
 * Revision 3.6  1992/11/27  10:29:00  espie
 * General cleanup
 *
 * Revision 3.5  1992/11/24  10:51:19  espie
 * un#ifdef'ed showseq code.
 *
 * Revision 3.3  1992/11/22  17:20:01  espie
 * Added <> operators.
 * Added update frequency on the fly.
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
 * Revision 2.19  1992/11/17  17:15:37  espie
 * Added interface using may_getchar(). Still primitive, though.
 * imask, start.
 * Added transpose feature.
 * Added possibility to get back to MONO for the sgi.
 * Added stereo capabilities to the indigo version.
 * Added two level of fault tolerancy.
 * Added some control on the number of replays,
 * and better error recovery.
 */
     
#include <stdio.h>
     
#include "defs.h"
#include "song.h"
#include "channel.h"
#include "extern.h"
#include "tags.h"
#include "prefs.h"
     

ID("$Id: player.c,v 1.1 1994/02/19 16:03:08 ache Exp $")
     

/* setting up a given note */

void reset_note(ch, note, pitch)
struct channel *ch;
int note;
int pitch;
   {
   ch->pitch = pitch;
   ch->note = note;
   ch->viboffset = 0;
   play_note(ch->audio, ch->samp, pitch);
   }

/* changing the current pitch (value
 * may be temporary, and not stored
 * in channel pitch, for instance vibratos.
 */
void set_current_pitch(ch, pitch)
struct channel *ch;
int pitch;
   {
      /* save current pitch in case we want to change
       * the step table on the run
   ch->cpitch = pitch;
   ch->step = step_table[pitch];
      */
   set_play_pitch(ch->audio, pitch);
   }

/* changing the current volume. You HAVE to get through
 * there so that it will work on EVERY machine.
 */
void set_current_volume(ch, volume)
struct channel *ch;
int volume;
   {
   ch->volume = MAX(MIN(volume, MAX_VOLUME), MIN_VOLUME);
   set_play_volume(ch->audio, ch->volume);
   }

void set_position(ch, pos)
struct channel *ch;
int pos;
   {
   set_play_position(ch->audio, pos);
   }

/* init_channel(ch, dummy):
 * setup channel, with initially
 * a dummy sample ready to play,
 * and no note.
 */
LOCAL void init_channel(ch)
struct channel *ch;
   {
   ch->samp = NULL;
   ch->finetune = 0;
   ch->audio = new_channel_tag_list(TAG_END);
   ch->volume = 0; 
   ch->pitch = 0; 
   ch->note = NO_NOTE;

      /* we don't setup arpeggio values. */
   ch->viboffset = 0; 
   ch->vibdepth = 0;

   ch->slide = 0; 

   ch->pitchgoal = 0; 
   ch->pitchrate = 0;

   ch->volumerate = 0;

   ch->vibrate = 0;
   ch->adjust = do_nothing;
   }



LOCAL int VSYNC;          /* base number of sample to output */
LOCAL void (*eval[NUMBER_EFFECTS]) P((struct automaton *a, struct channel *ch));
                    /* the effect table */
LOCAL int oversample;     /* oversample value */
LOCAL int frequency;      /* output frequency */
LOCAL int channel;        /* channel loop counter */

LOCAL struct channel chan[NUMBER_TRACKS];
                    /* every channel */

LOCAL struct sample_info *voices;

LOCAL struct automaton a;


void init_player(o, f)
int o, f;
   {
   oversample = o;
   frequency = f;
   init_tables(o, f);
   init_effects(eval);
   }

LOCAL void setup_effect(ch, a, e)
struct channel *ch;
struct automaton *a;
struct event *e;
   {
   int samp, cmd;

      /* retrieves all the parameters */
   samp = e->sample_number;

      /* load new instrument */
   if (samp)  
      {
         /* note that we can change sample in the middle
          * of a note. This is a *feature*, not a bug (see
          * made). Precisely: the sample change will be taken
          * into account for the next note, BUT the volume change
          * takes effect immediately.
          */
      ch->samp = voices + samp;
      ch->finetune = voices[samp].finetune;
      if ((1L<<samp) & get_pref_scalar(PREF_IMASK))
         ch->samp = voices;
      set_current_volume(ch, voices[samp].volume);
      }

   a->note = e->note;
   if (a->note != NO_NOTE)
      a->pitch = pitch_table[a->note][ch->finetune];
   else
      a->pitch = e->pitch;
   cmd = e->effect;
   a->para = e->parameters;

   if (a->pitch >= REAL_MAX_PITCH)
      {
      char buffer[60];
      sprintf(buffer,"Pitch out of bounds %d", a->pitch);
      status(buffer);
      a->pitch = 0;
      error = FAULT;
      }

   dump_event(ch, e);

      /* check for a new note: portamento
       * is the special case where we do not restart
       * the note.
       */
   if (a->pitch && cmd != EFF_PORTA && cmd != EFF_PORTASLIDE)
      reset_note(ch, a->note, a->pitch);
   ch->adjust = do_nothing;
      /* do effects */
   (eval[cmd])(a, ch);
   }


LOCAL void adjust_sync(ofreq, tempo)
int ofreq, tempo;
   {
   VSYNC = ofreq * NORMAL_FINESPEED / tempo;
   }

LOCAL void play_once(a)
struct automaton *a;
   {
   int channel;

   if (a->do_stuff & DELAY_PATTERN)
      for (channel = 0; channel < NUMBER_TRACKS; channel++)
         /* do the effects */
         (chan[channel].adjust)(chan + channel);
   else
      {  
      if (a->counter == 0)
         {
         for (channel = 0; channel < NUMBER_TRACKS; channel++)
            /* setup effects */
            setup_effect(chan + channel, a, 
               &(a->pattern->e[channel][a->note_num]));
         dump_event(0, 0);
         }
      else
         for (channel = 0; channel < NUMBER_TRACKS; channel++)
            /* do the effects */
            (chan[channel].adjust)(chan + channel);
      }

      /* advance player for the next tick */
   next_tick(a);
      /* actually output samples */
   resample(oversample, VSYNC / a->finespeed);
   }

LOCAL struct tag pres[2];


struct tag *play_song(song, start)
struct song *song;
int start;
   {
   int tempo;
   int countdown;      /* keep playing the tune or not */

   song_title(song->title);
   pres[1].type = TAG_END;
   
   tempo = get_pref_scalar(PREF_SPEED);

   adjust_sync(frequency, tempo);
    /* a repeats of 0 is infinite replays */
   
   countdown = get_pref_scalar(PREF_REPEATS);
   if (countdown == 0)
      countdown = 50000;   /* ridiculously huge number */

   voices = song->samples; 

   init_automaton(&a, song, start);

   release_audio_channels();

   for (channel = 0; channel < NUMBER_TRACKS; channel++) 
      init_channel(chan + channel);

   while(countdown)
      {
      struct tag *result;
      
      play_once(&a);
      result = get_ui();
      while(result = get_tag(result))
         {
         switch(result->type)
            {  
         case UI_LOAD_SONG:
            if (!result->data.pointer)
               break;
         case UI_NEXT_SONG:
         case UI_PREVIOUS_SONG:
            discard_buffer();
            pres[0].type = result->type;
            pres[0].data = result->data;
            return pres;
         case UI_QUIT:
            discard_buffer();
            end_all(0);
            /* NOTREACHED */
         case UI_SET_BPM:
            tempo = result->data.scalar;
            adjust_sync(frequency, tempo);
            break;
         case UI_RESTART:
            discard_buffer();
            init_automaton(&a, song, start);
            release_audio_channels();
            for (channel = 0; channel < NUMBER_TRACKS; channel++) 
               init_channel(chan + channel);
            break;
         case UI_JUMP_TO_PATTERN:
            if (result->data.scalar >= 0 && result->data.scalar < a.info->length)
               {
               discard_buffer();
               init_automaton(&a, song, result->data.scalar);
               }
            break;
            /*
         case ' ':
            while (may_getchar() == EOF)
               ;
            break;
             */
         default:
            break;
            }
         result++;
         }

      {
      int new_freq;
      if (new_freq = update_frequency())
         {
         frequency = new_freq;
         adjust_sync(frequency, tempo);
         init_tables(oversample, frequency);
         }
      }

      switch(error)
         {
      case NONE:
         break;
      case ENDED:
         countdown--;
         break;
      case SAMPLE_FAULT:
      case FAULT:
      case PREVIOUS_SONG:
      case NEXT_SONG:
      case UNRECOVERABLE:
         if ( (error == SAMPLE_FAULT && get_pref_scalar(PREF_TOLERATE))
            ||(error == FAULT && get_pref_scalar(PREF_TOLERATE) > 1) )
            break;
         pres[0].type = PLAY_ERROR;
         pres[0].data.scalar = error;
         return pres;
      default:
         break;
         }
         error = NONE;
      }
   pres[0].type = TAG_IGNORE;      
   return pres;
   }

