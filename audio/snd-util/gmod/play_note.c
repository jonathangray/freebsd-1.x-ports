
/*
*	gmod.c	- Module player for GUS and Linux.
*		(C) Hannu Savolainen, 1993
*
*	NOTE!	This program doesn't try to be a complete module player.
*		It's just a too I used while developing the driver. In
*		addition it can be used as an example on programming
*		the VoxWare Sound Driver with GUS.
*/

/*
* Many modifications have been done by Andrew J. Robinson.
* Refer to the ChangeLog for details.
*/


#include <sys/soundcard.h>
#include <sys/ultrasound.h>

#include "defines.h"
#include "structs.h"
#include "globals.h"
#include "protos.h"

int
do_command (int channel, int command, int parm, int note, int position, int pattern,
	    struct song_info *song_char, struct effect_info *effects)
  {
    int jump = 0;

    switch (command)
      {

      case CMD_NOP:;
	break;

      case CMD_ARPEG:
	if (parm)
	  set_arpeg (channel, parm);
	break;

      case CMD_JUMP:
	jump = MOVE_JUMP;
	effects->position = parm;
	effects->pattern = 0;
	break;

      case CMD_BREAK:
	jump = MOVE_JUMP;
	effects->position = position + 1;
	if (effects->position < song_char->songlength)
	  if (parm < pattern_len[tune[effects->position]])
	    effects->pattern = parm;
	  else
	    effects->pattern = 0;
	else
	  effects->pattern = 0;
	break;

      case CMD_SET_TICKS:
	if (parm)
	  ticks_per_division = parm;
	else
	  jump = MOVE_EXIT;
	break;

      case CMD_SET_BPM:
	tick_duration = 250.0 / parm;
	break;

      case CMD_SLIDEUP:
	if (parm > 0)
	  {
	    set_slideto (channel, parm, song_char->highest_note);
	    voices[channel].slide_pitch = SLIDE_UPDOWN;
	  }
	break;

      case CMD_SLIDEDOWN:
	if (parm > 0)
	  {
	    set_slideto (channel, parm, song_char->lowest_note);
	    voices[channel].slide_pitch = SLIDE_UPDOWN;
	  }
	break;

      case CMD_SLIDETO:
	set_slideto (channel, parm, note);
	voices[channel].slide_pitch = SLIDE_PORT;
	break;

      case CMD_SETOFFSET:
	sync_time ();
	/* if there is no note, restart current note to prevent click */
	if (!note)
	  SEQ_START_NOTE (gus_dev, channel, voices[channel].note, voices[channel].volume / 2);
	GUS_VOICE_POS (gus_dev, channel, parm * 256);
	break;

      case CMD_VOLUME:
	{
	  int vol = parm;

	  if (voices[channel].volume != vol)
	    {
	      voices[channel].volume = vol;
	      if (song_char->vol_type == VOL_LOG)
		vol = vol_log_to_lin ((unsigned char) vol) / 2;
	      else
		vol /= 2;
	      sync_time ();
	      SEQ_START_NOTE (gus_dev, channel, 255, vol);
	    }
	}
	break;

      case CMD_SET_PAN:
	if (!note)
	  set_panning (channel, (parm & 0x0f) * 17 - 128, PAN_HARDWARE);
	break;

      case CMD_VIBRA_WAVE:
	if ((parm & 0x03) == 0x03)
	  parm -= 1;
	voices[channel].vibra_wave = parm;
	break;

      case CMD_TREMOLO_WAVE:
	if ((parm & 0x03) == 0x03)
	  parm -= 1;
	voices[channel].tremolo_wave = parm;
	break;

      case CMD_GLISSANDO:
	voices[channel].glissando = parm;
	break;

      case CMD_DELAY_PAT:
	effects->delay_notes = parm;
	break;

      case CMD_CUT_NOTE:
	voices[channel].cut_count = parm + 1;
	break;

      case CMD_DELAY_NOTE:
	if (parm == 0)
	  parm = 1;
	voices[channel].delay_count = parm + 1;
	break;

      case CMD_PATTERN_LOOP:
	if (parm == 0)
	  voices[channel].pattern = pattern;
	else
	  {
	    effects->loop_chan = channel;
	    if (voices[channel].loop_times == 0)
	      {
		voices[channel].loop_times = parm;
		effects->pattern = voices[channel].pattern;
		jump = MOVE_LOOP;
	      }
	    else if (--voices[channel].loop_times > 0)
	      {
		effects->pattern = voices[channel].pattern;
		jump = MOVE_LOOP;
	      }
	  }
	break;

      case CMD_RETRIGGER:
	voices[channel].retrigger = parm;
	if (note)
	  voices[channel].retrig_count = parm + 1;
	else
	  voices[channel].retrig_count = 1;
	break;

      case CMD_FINEVOLUP:
	voices[channel].finevol = TRUE;
	voices[channel].volslide = parm * VOL_SLIDE_RATE;
	break;

      case CMD_FINEVOLDOWN:
	voices[channel].finevol = TRUE;
	voices[channel].volslide = -parm * VOL_SLIDE_RATE;
	break;

      case CMD_FINEPORTUP:
	if (parm > 0)
	  {
	    voices[channel].slide_pitch = SLIDE_ONCE;
	    voices[channel].slide_rate = parm;
	  }
	break;

      case CMD_FINEPORTDOWN:
	if (parm > 0)
	  {
	    voices[channel].slide_pitch = SLIDE_ONCE;
	    voices[channel].slide_rate = -parm;
	  }
	break;

      case CMD_VOLSLIDE:
	set_volslide (channel, parm);
	break;

      case CMD_PORTANDVOL:
	set_volslide (channel, parm);
	voices[channel].slide_pitch = 1;
	break;

      case CMD_VIBRATO:
	set_vibrato (channel, parm);
	break;

      case CMD_VIBRAANDVOL:
	set_vibrato (channel, 0);
	set_volslide (channel, parm);
	break;

      case CMD_TREMOLO:
	set_tremolo (channel, parm);
	break;

      default:
	/* printf ("Command %x %02x\n", pat->command, pat->parm1); */
      }

    return jump;
  }

int
play_note (int channel, int position, int pattern, struct note_info *pat,
	   struct song_info *song_char, struct effect_info *effects)
{
  int jump = 0;
  int sample, note, vol;
  int no_sample = FALSE;
  int old_sample, old_arpeg, old_vibra, old_tremolo;

  voices[channel].slide_pitch = 0;
  voices[channel].volslide = 0;
  voices[channel].retrigger = 0;
  old_arpeg = voices[channel].arpeg_num;
  old_vibra = voices[channel].vibra_rate;
  old_tremolo = voices[channel].tremolo;
  voices[channel].arpeg_num = 0;
  voices[channel].vibra_rate = 0;
  voices[channel].tremolo = 0;

  old_sample = voices[channel].sample - 1;
  note = pat->note;

  sample = pat->sample;

  /* A sample with no note resets the volume to its default, but should not */
  /* retrigger the note.  This really isn't possible with the current drivers */
  /* if the sample changes, so the note is retriggered on a sample change */

  if (sample && !note)
    {
      if (sample_ok[sample - 1])
	vol = sample_vol[sample - 1];
      else
	vol = 0;

      if (old_sample != (sample - 1))
	note = voices[channel].note;
      else if (voices[channel].volume != vol)
	{
	  voices[channel].volume = vol;
	  if (song_char->vol_type == VOL_LOG)
	    vol = vol_log_to_lin ((unsigned char) vol) / 2;
	  else
	    vol /= 2;
	  sync_time ();
	  SEQ_START_NOTE (gus_dev, channel, 255, vol);
	}
    }

  if (sample)
    voices[channel].sample = sample;
  else
    {
      no_sample = TRUE;
      sample = voices[channel].sample;
    }

  sample--;

  if (note && (pat->command[0] != 3) && (pat->command[1] != 3))
    {
      sync_time ();

      if ((sample >= 0) && sample_ok[sample])
	{
	  if (old_sample != sample)
	    {
	      SEQ_SET_PATCH (gus_dev, channel, sample);
	      voices[channel].sample = sample + 1;
	    }

	  if (voices[channel].pitchbender || old_arpeg || old_vibra)
	    {
	      SEQ_PITCHBEND (gus_dev, channel, 0);
	      voices[channel].pitchbender = 0;
	      old_arpeg = 0;
	      old_vibra = 0;
	    }

	  /* specifying no sample keeps the current volume */

	  if (no_sample == FALSE)
	    voices[channel].volume = sample_vol[sample];
	  if (pat->command[0] == CMD_VOLUME)
	    voices[channel].volume = pat->parm1[0];
	  if (pat->command[1] == CMD_VOLUME)
	    voices[channel].volume = pat->parm1[1];

	  vol = voices[channel].volume;
	  if (song_char->vol_type == VOL_LOG)
	    vol = vol_log_to_lin ((unsigned char) vol) / 2;
	  else
	    vol /= 2;

	  /* set panning */

	  if (pat->command[0] == CMD_SET_PAN)
	    set_panning (channel, (pat->parm1[0] & 0x0f) * 17 - 128, PAN_NO_HARDWARE);
	  if (pat->command[1] == CMD_SET_PAN)
	    set_panning (channel, (pat->parm1[1] & 0x0f) * 17 - 128, PAN_NO_HARDWARE);

	  if ((pat->command[0] != CMD_DELAY_NOTE) && (pat->command[1] != CMD_DELAY_NOTE))
	    SEQ_START_NOTE (gus_dev, channel, note, vol);

	  voices[channel].note = note;
	  voices[channel].slide_period = period_table[note - NOTE_BASE];

	  if (voices[channel].vibra_wave <= 3)
	    voices[channel].vibra_position = 0;
	  if (voices[channel].tremolo_wave <= 3)
	    voices[channel].tremolo_position = 0;
	}
      else
	SEQ_START_NOTE (gus_dev, channel, 255, 0);
    }

  jump = do_command (channel, pat->command[0], pat->parm1[0], pat->note, position,
		     pattern, song_char, effects);
  jump |= do_command (channel, pat->command[1], pat->parm1[1], pat->note, position,
		      pattern, song_char, effects);

  if (((old_arpeg && !voices[channel].arpeg_num) ||
       (old_vibra && !voices[channel].vibra_rate)) &&
      !pat->note)
    {
      sync_time ();
      SEQ_PITCHBEND (gus_dev, channel, voices[channel].pitchbender + 1);
    }

  if (old_tremolo && !voices[channel].tremolo &&
      (!pat->note || pat->command[0] == CMD_SLIDETO || pat->command[1] == CMD_SLIDETO))
    {
      sync_time ();
      SEQ_START_NOTE (gus_dev, channel, 255, voices[channel].volume / 2);
    }

  return jump;
}
