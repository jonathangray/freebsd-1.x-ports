






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

#include "defines.h"
#include "structs.h"
#include "globals.h"
#include "protos.h"

void
lets_play_voice (int channel, struct voice_info *v,
		 struct song_info *song_char)
  {
    int note, bend, vibra;

    if (v->slide_pitch && v->slide_rate)
      {
	v->slide_period -= v->slide_rate;
	period_to_note (v->slide_period, &note, &bend);
	if ((v->slide_pitch == SLIDE_PORT) && v->glissando)
	  {
	    if (bend > 0)
	      note += 1;
	    bend = 0;
	  }
	v->pitchbender = (note * 100 + bend) - (v->note * 100);
	if (v->slide_pitch == SLIDE_ONCE)
	  {			/* slide one step only */
	    v->slide_pitch = 0;
	    v->slide_rate = 0;
	  }
	else if (v->slide_rate < 0)
	  {
	    if (v->pitchbender <= v->slide_goal)
	      {
		v->pitchbender = v->slide_goal;
		v->slide_period = v->slide_period_goal;
		v->slide_pitch = 0;	/* Stop */
		v->slide_rate = 0;
	      }
	  }
	else
	  {
	    if (v->pitchbender >= v->slide_goal)
	      {
		v->pitchbender = v->slide_goal;
		v->slide_period = v->slide_period_goal;
		v->slide_pitch = 0;	/* Stop */
		v->slide_rate = 0;
	      }
	  }

	sync_time ();
	SEQ_PITCHBEND (gus_dev, channel, v->pitchbender + 1);
      }

    if (v->volslide)
      {
	vibra = v->volume + v->volslide;

	if (vibra > 255)
	  {
	    vibra = 255;
	    v->volslide = 0;
	  }
	else if (vibra < 0)
	  {
	    vibra = 0;
	    v->volslide = 0;
	  }

	if (v->finevol == TRUE)
	  {
	    v->volslide = 0;
	    v->finevol = FALSE;
	  }

	v->volume = (unsigned char) vibra;

	if (song_char->vol_type == VOL_LOG)
	  vibra = vol_log_to_lin ((unsigned char) vibra) / 2;
	else
	  vibra /= 2;

	sync_time ();
	SEQ_START_NOTE (gus_dev, channel, 255, (unsigned char) vibra);
      }

    if (v->cut_count)
      if (--(v->cut_count) == 0)
	{
	  sync_time ();
	  SEQ_START_NOTE (gus_dev, channel, 255, 0);
	  voices[channel].volume = 0;
	}

    if (v->delay_count)
      if (--(v->delay_count) == 0)
	{
	  sync_time ();
	  SEQ_START_NOTE (gus_dev, channel, v->note, v->volume);
	}

    if (v->retrigger)
      if (--(v->retrig_count) == 0)
	{
	  sync_time ();
	  SEQ_START_NOTE (gus_dev, channel, v->note, v->volume);
	  v->retrig_count = v->retrigger;
	}

    if (v->arpeg_num)
      {
	sync_time ();
	SEQ_PITCHBEND (gus_dev, channel, v->pitchbender + v->arpeg_note[v->arpeg_curr] + 1);
	v->arpeg_curr = (v->arpeg_curr + 1) % v->arpeg_num;
      }

    if (v->vibra_rate)
      {
	vibra = vibra_table[v->vibra_wave & 0x03][v->vibra_position];
	vibra = (vibra * v->vibra_depth) / 128;
	period_to_note (v->slide_period + vibra, &note, &bend);
	vibra = (note * 100 + bend) - (v->note * 100);
	sync_time ();
	SEQ_PITCHBEND (gus_dev, channel, vibra + 1);
	v->vibra_position += v->vibra_rate;
	v->vibra_position %= NUM_VIBRA;
      }

    if (v->tremolo)
      {
	vibra = vibra_table[v->tremolo_wave & 0x03][v->tremolo_position];
	vibra = (vibra * v->tremolo_depth) / 128;
	vibra = v->volume + (VOL_SLIDE_RATE * vibra);
	if (vibra > 255)
	  vibra = 255;
	else if (vibra < 0)
	  vibra = 0;
	sync_time ();
	SEQ_START_NOTE (gus_dev, channel, 255, vibra / 2);
	v->tremolo_position += v->tremolo;
	v->tremolo_position %= NUM_VIBRA;
      }

  }
