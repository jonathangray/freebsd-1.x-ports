/*
* This file is part of the GMOD package.
*
* period_to_note by Andrew J. Robinson
*
* Convert a period to a midi note/pitchbend number
*/

#include <stdlib.h>

#include "defines.h"
#include "structs.h"
#include "globals.h"

void
period_to_note (int period, int *note, int *pitchbend)
{
  int low = 0, middle = 0, high = NUM_PERIODS - 1;
  int diff, diff_high, diff_low;

  *pitchbend = 0;

  if (period > period_table[0])
    period = period_table[0];
  else if (period < period_table[NUM_PERIODS - 1])
    period = period_table[NUM_PERIODS - 1];

  while (high >= low)
    {
      middle = (high + low) / 2;
      if (period == period_table[middle])
	break;
      else if (period < period_table[middle])
	low = middle + 1;
      else
	high = middle - 1;
    }

  if (period != period_table[middle])
    {
      diff_high = abs (period - period_table[high]);
      diff_low = abs (period - period_table[low]);

      if (diff_low < diff_high)
	middle = low;
      else
	middle = high;

      if (period < period_table[middle])
	diff = period_table[middle] - period_table[middle + 1];
      else
	diff = period_table[middle - 1] - period_table[middle];

      *pitchbend = (period_table[middle] - period) * 100 / diff;
    }

  *note = middle + NOTE_BASE;
}
