
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


#include <machine/soundcard.h>
#include <machine/ultrasound.h>
#include <stdio.h>
#include <stdlib.h>

#include "defines.h"
#include "structs.h"
#include "globals.h"
#include "protos.h"

void
play_module (char *name, struct song_info *song_char,
	     struct options_info options)
{
  int i, position, pat_start = 0, extra_ticks, jump_to_pos, pan;
  struct effect_info effects =
  {0, 0, 0, 0};

  init_voices ();

  SEQ_START_TIMER ();

  for (i = 0; i < song_char->nr_channels; i++)
    {
      SEQ_BENDER_RANGE (gus_dev, i, 8191);
      SEQ_EXPRESSION (gus_dev, i, options.main_volume);
      SEQ_MAIN_VOLUME (gus_dev, i, 100);
      pan = song_char->panning[i] * 2;
      if (pan > 127)
	pan = 127;
      else if (pan < -127)
	pan = -127;
      SEQ_PANNING (gus_dev, i, pan);
      voices[i].panning = (signed char) pan;
      SEQ_PITCHBEND (gus_dev, i, 0);
    }

  next_time = 0.0;

  set_speed (song_char->play_speed);

  this_time = 0.0;
  next_time += tick_duration;
  sync_time ();

  for (position = 0; position < song_char->songlength; position++)
    {
      int tick, pattern, channel, pos, go_to;

      pos = tune[position];

      if (pos == -1)
	{
	  position = song_char->songlength - 1;
	  continue;
	}

      if (pattern_tempo[pos])
	set_speed (pattern_tempo[pos]);

      jump_to_pos = 0;
      for (pattern = pat_start; pattern < pattern_len[pos] && jump_to_pos == 0; pattern++)
	{
	  this_time = 0.0;

	  for (channel = 0; channel < song_char->nr_channels; channel++)
	    {
	      if ((go_to = play_note (channel, position, pattern,
			&(*pattern_table[pos])[channel][pattern], song_char,
				      &effects)) != 0)
		jump_to_pos |= go_to;
	    }

	  extra_ticks = ticks_per_division * effects.delay_notes;
	  effects.delay_notes = 0;

	  /* next_time += tick_duration; */
	  for (tick = 0; tick < ticks_per_division + extra_ticks; tick++)
	    {
	      for (channel = 0; channel < song_char->nr_channels; channel++)
		{
		  lets_play_voice (channel, &voices[channel], song_char);
		}
	      next_time += tick_duration;
	    }

	}			/* pattern */

      pat_start = 0;

      if ((options.loop_breaker == TRUE) && !(jump_to_pos & MOVE_LOOP))
	tune[position] = -1;

      if (jump_to_pos & MOVE_LOOP)
	{
	  pat_start = effects.pattern;
	  effects.pattern = 0;
	  position -= 1;
	}
      if (jump_to_pos & MOVE_JUMP)
	{
	  pat_start = effects.pattern;
	  position = effects.position - 1;
	}
      if (jump_to_pos & MOVE_EXIT)
	position = song_char->songlength - 1;
    }

  sync_time ();

  for (i = 0; i < song_char->nr_channels; i++)
    SEQ_STOP_NOTE (gus_dev, i, 0, 127);

  next_time += 200;
  sync_time ();

  for (i = 0; i < song_char->nr_channels; i++)
    {
      GUS_VOICEOFF (gus_dev, i);
      GUS_RAMPOFF (gus_dev, i);
    }

  SEQ_DUMPBUF ();

  for (i = 0; i < song_char->nr_patterns; i++)
    free (pattern_table[i]);
}
