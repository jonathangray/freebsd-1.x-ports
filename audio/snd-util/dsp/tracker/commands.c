/* commands.c 
	vi:se ts=3 sw=3:
 */

/* $Id: commands.c,v 1.1 1994/02/19 16:03:07 ache Exp $
 * $Log: commands.c,v $
 * Revision 1.1  1994/02/19 16:03:07  ache
 * Initial revision
 *
 * Revision 4.0  1994/01/11  17:44:07  espie
 * Abstracted IO.
 *
 * Revision 1.6  1994/01/09  17:36:22  Espie
 * Generalized open.c.
 *
 * Revision 1.5  1994/01/08  02:04:21  Espie
 * Some notice to status.
 *
 * Revision 1.4  1994/01/05  14:54:09  Espie
 * *** empty log message ***
 *
 * Revision 1.3  1994/01/05  13:50:43  Espie
 * Cosmetic change.
 *
 * Revision 1.2  1993/12/28  13:54:44  Espie
 * Use notice.
 *
 * Revision 1.1  1993/12/26  00:55:53  Espie
 * Initial revision
 *
 * Revision 3.15  1993/12/04  16:12:50  espie
 * Lots of LOCAL added.
 *
 * Revision 3.14  1993/11/17  15:31:16  espie
 * play_note instead of ch->mode.
 *
 * Revision 3.13  1993/08/04  11:34:33  espie
 * *** empty log message ***
 *
 * Revision 3.12  1993/07/18  10:39:44  espie
 * Cleaned up.
 *
 * Revision 3.11  1993/07/17  22:23:41  espie
 * Fixed bug with bad loops.
 *
 * Revision 3.9  1993/05/09  14:06:03  espie
 * Modified the way set_speed works.
 *
 * Revision 3.8  1993/04/28  20:13:13  espie
 * Very small bug with volume (Lawrence).
 *
 * Revision 3.7  1993/01/15  14:00:28  espie
 * Added bg/fg test.
 *
 * Revision 3.6  1992/11/27  10:29:00  espie
 * General cleanup
 *
 * Revision 3.5  1992/11/24  10:51:19  espie
 * More precise vibrato table.
 *
 * Revision 3.4  1992/11/23  10:12:23  espie
 * *** empty log message ***
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
 * Revision 2.12  1992/11/13  13:24:24  espie
 * Added some extended commands: E12AB, and some.
 * now use set_volume in audio.c. All the device-dependent operation
 * is there.
 * Defensive programming: check the range of each note
 * for arpeggio setup.
 * Structured part of the code, especially replay ``automaton''
 * and setting up of effects.
 *
 * Revision 1.9  1991/11/17  17:09:53  espie
 * Added missing prototypes.
 * Dynamic oversample and frequency.
 * Added arpeggio.
 * Fixed up vibrato depth.
 * Added vibslide and portaslide.
 * Added command 9.
 */

#include <stdio.h>

#include "defs.h"
#include "channel.h"
#include "song.h"
#include "extern.h"
     
ID("$Id: commands.c,v 1.1 1994/02/19 16:03:07 ache Exp $")

/* sine table for the vibrato effect (obtained through build.c) */

int vibrato_table[64] = 
   {
   0,50,100,149,196,241,284,325,362,396,426,452,473,490,502,510,512,
   510,502,490,473,452,426,396,362,325,284,241,196,149,100,50,0,-49,
   -99,-148,-195,-240,-283,-324,-361,-395,-425,-451,-472,-489,-501,
   -509,-511,-509,-501,-489,-472,-451,-425,-395,-361,-324,-283,-240,
   -195,-148,-99,-49
   };

/***
 *
 *
 * setting up effects/doing effects.
 * The set_xxx gets called while parsing the effect,
 * the do_xxx gets called each tick, and update the
 * sound parameters while playing it.
 *
 *
 ***/


void do_nothing(ch)
struct channel *ch;
   {
   }

LOCAL void set_nothing(a, ch)
struct automaton *a;
struct channel *ch;
   {
   }

/* slide pitch (up or down) */
LOCAL void do_slide(ch)
struct channel *ch;
   {
   ch->pitch += ch->slide;
   ch->pitch = MIN(ch->pitch, MAX_PITCH);
   ch->pitch = MAX(ch->pitch, MIN_PITCH);
   set_current_pitch(ch, ch->pitch);
   }

LOCAL void set_upslide(a, ch)
struct automaton *a;
struct channel *ch;
   {
   ch->adjust = do_slide;
   if (a->para)
      ch->slide = a->para;
   }

LOCAL void set_downslide(a, ch)
struct automaton *a;
struct channel *ch;
   {
   ch->adjust = do_slide;
   if (a->para)
      ch->slide = -a->para;
   }

/* modulating the pitch with vibrato */
LOCAL void do_vibrato(ch)
struct channel *ch;
   {
   int offset;

      /* this is no longer a literal transcription of the pt
       * code. I have rescaled the vibrato table.
       */
   ch->viboffset += ch->vibrate;
   ch->viboffset &= 63;
      /* please don't use logical shift on signed values */
   offset = (vibrato_table[ch->viboffset] * ch->vibdepth)/256;
      /* temporary update of only the step value,
       * note that we do not change the saved pitch.
       */
   set_current_pitch(ch, ch->pitch + offset);
   }

LOCAL void set_vibrato(a, ch)
struct automaton *a;
struct channel *ch;
   {
   ch->adjust = do_vibrato;
   if (HI(a->para))
      ch->vibrate = HI(a->para);
   if (LOW(a->para))
      ch->vibdepth = LOW(a->para);
   }

/* arpeggio looks a bit like chords: we alternate between two
 * or three notes very fast.
 */
LOCAL void do_arpeggio(ch)
struct channel *ch;
   {
   if (++ch->arpindex >= MAX_ARP)
      ch->arpindex =0;
   set_current_pitch(ch, ch->arp[ch->arpindex]);
   }

LOCAL void set_arpeggio(a, ch)
struct automaton *a;
struct channel *ch;
   {
      /* arpeggio can be installed relative to the
       * previous note, so we have to check that there
       * actually is a current(previous) note
       */
   if (ch->note == NO_NOTE)
      {
      status("No note present for arpeggio");
      error = FAULT;
      }
   else
      {
      int note;

      ch->arp[0] = pitch_table[ch->note][ch->finetune];
      note = ch->note + HI(a->para);
      if (note < NUMBER_NOTES)
         ch->arp[1] = pitch_table[note][ch->finetune];
      else
         {
         status("Arpeggio note out of range");
         error = FAULT;
         }
      note = ch->note + LOW(a->para);
      if (note < NUMBER_NOTES)
         ch->arp[2] = pitch_table[note][ch->finetune];
      else
         {
         status("Arpeggio note out of range");
         error = FAULT;
         }
      ch->arpindex = 0;
      ch->adjust = do_arpeggio;
      }
   }

/* volume slide. Mostly used to simulate waveform control.
 * (attack/decay/sustain).
 */
LOCAL void do_slidevol(ch)
struct channel *ch;
   {
   set_current_volume(ch, ch->volume + ch->volumerate);
   }

/* note that volumeslide does not have a ``take default''
 * behavior. If para is 0, this is truly a 0 volumeslide.
 * Issue: is the test really necessary ? Can't we do
 * a HI(para) - LOW(para). Answer: protracker does not.
 */
LOCAL void parse_slidevol(ch, para)
struct channel *ch;
int para;
   {
   if (LOW(para))
      ch->volumerate = -LOW(para);
   else
      ch->volumerate = HI(para);
   }

LOCAL void set_slidevol(a, ch)
struct automaton *a;
struct channel *ch;
   {
   ch->adjust = do_slidevol;
   parse_slidevol(ch, a->para);
   }

/* portamento: gets from a given pitch to another.
 * We can simplify the routine by cutting it in
 * a pitch up and pitch down part while setting up
 * the effect.
 */
LOCAL void do_portamento(ch)
struct channel *ch;
   {
   if (ch->pitch < ch->pitchgoal)
      {
      ch->pitch += ch->pitchrate;
      ch->pitch = MIN(ch->pitch, ch->pitchgoal);
      }
   else if (ch->pitch > ch->pitchgoal)
      {
      ch->pitch -= ch->pitchrate;
      ch->pitch = MAX(ch->pitch, ch->pitchgoal);
      }
      /* if we want to implement funk glissando, we need a change right
       * there
       */
   set_current_pitch(ch, ch->pitch);
   }

/* if para and pitch are 0, this is obviously a continuation
 * of the previous portamento.
 */
LOCAL void set_portamento(a, ch)
struct automaton *a;
struct channel *ch;
   {
   ch->adjust = do_portamento;
   if (a->para)
      ch->pitchrate = a->para;
   if (a->pitch)
      ch->pitchgoal = a->pitch;
   }

/*
 * combined commands.
 */
LOCAL void do_portaslide(ch)
struct channel *ch;
   {
   do_portamento(ch);
   do_slidevol(ch);
   }

LOCAL void set_portaslide(a, ch)
struct automaton *a;
struct channel *ch;
   {
   ch->adjust = do_portaslide;
   if (a->pitch)
      ch->pitchgoal = a->pitch;
   parse_slidevol(ch, a->para);
   }

LOCAL void do_vibratoslide(ch)
struct channel *ch;
   {
   do_vibrato(ch);
   do_slidevol(ch);
   }

LOCAL void set_vibratoslide(a, ch)
struct automaton *a;
struct channel *ch;
   {
   ch->adjust = do_vibratoslide;
   parse_slidevol(ch, a->para);
   }

/***
 *
 *  effects that just need a setup part
 *
 ***/

/* IMPORTANT: because of the special nature of
 * the player, we can't process each effect independently,
 * we have to merge effects from the four channel before
 * doing anything about it. For instance, there can be 
 * several speed change in the same note.
 */
LOCAL void set_speed(a, ch)
struct automaton *a;
struct channel *ch;
   {
   if (a->para >= 32)
      {
      a->new_finespeed = a->para;
      a->do_stuff |= SET_FINESPEED;
      }
   else if (a->para)
      {
      a->new_speed = a->para;
      a->do_stuff |= SET_SPEED;
      }
   }

LOCAL void set_skip(a, ch)
struct automaton *a;
struct channel *ch;
   {
      /* BCD decoding in read.c */
   a->new_note = a->para;
   a->do_stuff |= SET_SKIP;
   }

LOCAL void set_fastskip(a, ch)
struct automaton *a;
struct channel *ch;
   {
   a->new_pattern = a->para;
   a->do_stuff |= SET_FASTSKIP;
   }

/* immediate effect: starts the sample somewhere
 * off the start.
 */
LOCAL void set_offset(a, ch)
struct automaton *a;
struct channel *ch;
   {
   set_position(ch, a->para * 256);
   }

/* change the volume of the current channel.
 * Is effective until there is a new set_volume,
 * slide_volume, or an instrument is reloaded 
 * explicitly by giving its number. Obviously, if
 * you load an instrument and do a set_volume in the
 * same note, the set_volume will take precedence.
 */
LOCAL void set_volume(a, ch)
struct automaton *a;
struct channel *ch;
   {
   set_current_volume(ch, a->para);
   }



/***
 *
 * EXTENDED COMMANDS
 *
 ***/

/* extended command: retrig note at a fast pace
 */
LOCAL void do_retrig(ch)
struct channel *ch;
   {
   if (--ch->current <= 0)
      {
      reset_note(ch, ch->note, ch->pitch);
      ch->current = ch->retrig;
      }
   }

LOCAL void set_retrig(a, ch)
struct automaton *a;
struct channel *ch;
   {
   ch->retrig = a->para;
   ch->current = ch->retrig;
   ch->adjust = do_retrig;
   }

/* extended command: start note after a small
 * delay
 */
LOCAL void do_latestart(ch)
struct channel *ch;
   {
   if (--ch->current <= 0)
      {
      reset_note(ch, ch->note, ch->pitch);
      ch->adjust = do_nothing;
      }
   }

LOCAL void set_late_start(a, ch)
struct automaton *a;
struct channel *ch;
   {
   play_note(ch->audio, NULL, 0);
   ch->current = a->para;
   ch->adjust = do_latestart;
   }

/* extended command: cut note after some time.
 * Note we only kill the volume, as protracker does...
 */
LOCAL void do_cut(ch)
struct channel *ch;
   {
   if (ch->retrig)
      {
      if (--ch->retrig == 0)
         set_current_volume(ch, 0);
      }
   }

LOCAL void set_note_cut(a, ch)
struct automaton *a;
struct channel *ch;
   {
   ch->retrig = a->para;
   ch->adjust = do_cut;
   }


LOCAL void set_smooth_up(a, ch)
struct automaton *a;
struct channel *ch;
   {
   ch->pitch += a->para;
   ch->pitch = MIN(ch->pitch, MAX_PITCH);
   ch->pitch = MAX(ch->pitch, MIN_PITCH);
   set_current_pitch(ch, ch->pitch);
   }

LOCAL void set_smooth_down(a, ch)
struct automaton *a;
struct channel *ch;
   {
   ch->pitch -= a->para;
   ch->pitch = MIN(ch->pitch, MAX_PITCH);
   ch->pitch = MAX(ch->pitch, MIN_PITCH);
   set_current_pitch(ch, ch->pitch);
   }

LOCAL void set_change_finetune(a, ch)
struct automaton *a;
struct channel *ch;
   {
   ch->finetune = a->para;
   }


LOCAL void set_loop(a, ch)
struct automaton *a;
struct channel *ch;
   {
      /* Note: the current implementation of protracker
       * does not allow for a jump from pattern to pattern,
       * even though it looks like a logical extension to the current 
       * format.
       */
   if (a->para == 0) 
      a->loop_note_num = a->note_num;
   else
      {
      if (a->loop_counter == 0)
         a->loop_counter = a->para + 1;
      /* We have to defer the actual count-down and note jump
       * to automaton.c, because some modules include several
       * loops on the same measure, which is a bit confusing
       * (see don't you want me)
       */
      a->do_stuff |= JUMP_PATTERN;
      }
   }

LOCAL void set_smooth_upvolume(a, ch)
struct automaton *a;
struct channel *ch;
   {
   set_current_volume(ch, ch->volume + a->para);
   }

LOCAL void set_smooth_downvolume(a, ch)
struct automaton *a;
struct channel *ch;
   {
   set_current_volume(ch, ch->volume - a->para);
   }


LOCAL void set_delay_pattern(a, ch)
struct automaton *a;
struct channel *ch;
   {
   a->counter -= (a->para + 1) * a->speed;
   a->do_stuff |= DELAY_PATTERN;
   }



/* Initialize the whole effect table */

void init_effects(table)
void (*table[]) P((struct automaton *a, struct channel *ch));
   {
   int i;

   for (i = 0; i < NUMBER_EFFECTS; i++)
      table[i] = set_nothing;
   table[EFF_ARPEGGIO] = set_arpeggio;
   table[EFF_SPEED] = set_speed;
   table[EFF_SKIP] = set_skip;
   table[EFF_FF] = set_fastskip;
   table[EFF_VOLUME] = set_volume;
   table[EFF_VOLSLIDE] = set_slidevol;
   table[EFF_OFFSET] = set_offset;
   table[EFF_PORTA] = set_portamento;
   table[EFF_PORTASLIDE] = set_portaslide;
   table[EFF_UP] = set_upslide;
   table[EFF_DOWN] = set_downslide;
   table[EFF_VIBRATO] = set_vibrato;
   table[EFF_VIBSLIDE] = set_vibratoslide;
   table[EFF_SMOOTH_UP] = set_smooth_up;
   table[EFF_SMOOTH_DOWN] = set_smooth_down;
   table[EFF_CHG_FTUNE] = set_change_finetune;
   table[EFF_LOOP] = set_loop;
   table[EFF_RETRIG] = set_retrig;
   table[EFF_S_UPVOL] = set_smooth_upvolume;
   table[EFF_S_DOWNVOL] = set_smooth_downvolume;
   table[EFF_NOTECUT] = set_note_cut;
   table[EFF_LATESTART] = set_late_start;
   table[EFF_DELAY] = set_delay_pattern;
   }

