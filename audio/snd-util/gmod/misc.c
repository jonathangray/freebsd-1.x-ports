
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

int
panning (int ch)
{
  static int panning_tab[] =
  {-127, 127, 127, -127};

  return panning_tab[ch % 4];
}

void
sync_time ()
{
  if (next_time > this_time)
    {
      SEQ_WAIT_TIME ((long) next_time);
      this_time = next_time;
    }
}

unsigned short
intelize (unsigned short v)
{
  return ((v & 0xff) << 8) | ((v >> 8) & 0xff);
}

unsigned long
intelize4 (unsigned long v)
{
  return
  (((v >> 16) & 0xff) << 8) | (((v >> 16) >> 8) & 0xff) |
  (((v & 0xff) << 8) | ((v >> 8) & 0xff) << 16);
}


int
gus_mem_free (int dev)
{
  ioctl (seqfd, SNDCTL_SYNTH_MEMAVL, &dev);
  return (dev);
}


/* vol_log_to_linear converts an 8 bit volume in the GUS exponent/mantissa
   format to an 8 bit linear volume.  This is not quite like real logs. */

unsigned char
vol_log_to_lin (unsigned char volume)
{
  int new_volume;
  unsigned char bits;

  bits = (volume & 0xf0) >> 4;
  new_volume = 1 << bits;
  volume &= 0x0f;
  if (bits >= 4)
    {
      new_volume |= (volume << (bits - 4));
      for (bits -= 4; bits > 0; bits--)
	new_volume |= (1 << (bits - 1));
    }
  else
    new_volume |= (volume >> (4 - bits));
  new_volume /= 256;
  return ((unsigned char) new_volume);
}
