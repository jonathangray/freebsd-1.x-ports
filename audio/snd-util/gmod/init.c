
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


#include "defines.h"
#include "structs.h"
#include "globals.h"

void
init_voices ()
{
  int i;

  for (i = 0; i < MAX_TRACK; i++)
    {
      voices[i].sample = 0;
      voices[i].note = 0;
      voices[i].volume = 64;

      voices[i].slide_pitch = 0;
      voices[i].slide_goal = 0;
      voices[i].slide_rate = 0;
      voices[i].pitchbender = 0;
      voices[i].last_rate = 0;
      voices[i].slide_period = 0;
      voices[i].glissando = 0;

      voices[i].volslide = 0;
      voices[i].finevol = FALSE;

      voices[i].pattern = 0;
      voices[i].position = 0;
      voices[i].loop_times = 0;
      voices[i].cut_count = 0;

      voices[i].arpeg_num = 0;

      voices[i].retrigger = 0;

      voices[i].vibra_rate = 0;
      voices[i].vibra_old_rate = 0;
      voices[i].vibra_position = 0;
      voices[i].vibra_depth = 0;
      voices[i].vibra_wave = 0;

      voices[i].tremolo = 0;
      voices[i].tremolo_old = 0;
      voices[i].tremolo_position = 0;
      voices[i].tremolo_depth = 0;
      voices[i].tremolo_wave = 0;
    }
}
