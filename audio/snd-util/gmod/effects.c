
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

void
set_speed (int parm)
{
  if (!parm)
    parm = 1;

  if (parm < 32)
    ticks_per_division = parm;
  else
    tick_duration = (250.0 / parm);
}

void
set_volslide (int channel, int amount)
{
  int n;

  voices[channel].volslide = 0;

  if ((n = ((amount & 0xf0) >> 4)))
    voices[channel].volslide = VOL_SLIDE_RATE * n;
  else
    voices[channel].volslide = -(VOL_SLIDE_RATE * (amount & 0xf));
}

void
set_slideto (int channel, int rate, int note)
{
  int size, curr_note;

  if (rate != 0)
    voices[channel].last_rate = rate;
  else
    rate = voices[channel].last_rate;

  if (note == 0)
    {
      if (voices[channel].slide_rate > 0)
	{
	  voices[channel].slide_pitch = 1;
	  voices[channel].slide_rate = rate;
	}
      else if (voices[channel].slide_rate < 0)
	{
	  voices[channel].slide_pitch = 1;
	  voices[channel].slide_rate = -rate;
	}
      return;
    }

  curr_note = voices[channel].note * 100 + voices[channel].pitchbender;

  size = (note * 100) - curr_note;

  if (!size)
    return;

  if (size < 0)
    rate = -rate;

  voices[channel].slide_goal = voices[channel].pitchbender + size;
  voices[channel].slide_rate = rate;
  voices[channel].slide_period_goal = period_table[note - NOTE_BASE];
}

void
set_arpeg (int channel, int amount)
{
  voices[channel].arpeg_num = 3;
  voices[channel].arpeg_note[0] = 0;
  voices[channel].arpeg_curr = 0;
  voices[channel].arpeg_note[1] = (amount >> 4) * 100;
  voices[channel].arpeg_note[2] = (amount & 0x0f) * 100;
}

void
set_vibrato (int channel, int amount)
{
  int depth;

  voices[channel].vibra_rate = (amount & 0xf0) >> 4;

  if (voices[channel].vibra_rate == 0)
    voices[channel].vibra_rate = voices[channel].vibra_old_rate;
  else
    voices[channel].vibra_old_rate = voices[channel].vibra_rate;

  depth = (amount & 0x0f);

  if (depth != 0)
    voices[channel].vibra_depth = depth;
}

void
set_tremolo (int channel, int amount)
{
  int depth;

  voices[channel].tremolo = (amount & 0xf0) >> 4;

  if (voices[channel].tremolo == 0)
    voices[channel].tremolo = voices[channel].tremolo_old;
  else
    voices[channel].tremolo_old = voices[channel].tremolo;

  depth = (amount & 0x0f);

  if (depth != 0)
    voices[channel].tremolo_depth = depth;
}

void
set_panning (int channel, signed char panning, unsigned char hw_flag)
{
  int pan_val;

  pan_val = panning * 2;
  if (pan_val > 127)
    pan_val = 127;
  else if (pan_val < -127)
    pan_val = -127;
  if (voices[channel].panning != (signed char) panning)
    {
      sync_time ();
      SEQ_PANNING (gus_dev, channel, (signed char) pan_val);
      voices[channel].panning = (signed char) pan_val;
      if (hw_flag == PAN_HARDWARE)
	{
	  pan_val = (pan_val + 16) / 32 + 7;
	  GUS_VOICEBALA (gus_dev, channel, pan_val);
	}
    }
}
