
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


#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>

#include "defines.h"
#include "structs.h"
#include "globals.h"

void
seqbuf_dump ()
{
  int result;

  while (_seqbufptr)
    {
      if ((result = write (seqfd, _seqbuf, _seqbufptr)) == -1)
	{
	  perror ("write /dev/sequencer");
	  exit (-1);
	}
      else if (result != _seqbufptr)
	{
	  _seqbufptr -= result;
	  memmove (_seqbuf, &(_seqbuf[result]), _seqbufptr);
	}
      else
	_seqbufptr = 0;
    }
}
