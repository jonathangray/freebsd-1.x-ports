/* zround.c: round R to the nearest whole number.  */

#include "config.h"

integer
zround (r)
    double r;
{
  integer i;

  /* R can be outside the range of an integer if glue is stretching or
     shrinking a lot.  We can't do any better than returning the largest
     or smallest integer possible in that case.  It doesn't seem to make
     any practical difference.  Here is a sample input file which
     demonstrates the problem, from phil@cs.arizona.edu:
     	\documentstyle{article}
	\begin{document}
	\begin{flushleft}
	$\hbox{} $\hfill 
	\filbreak
	\eject
  */
  if (r > INTEGER_MAX)
    i = INTEGER_MAX;
  else if (r < INTEGER_MIN)
    i = INTEGER_MIN;
  else if (r >= 0.0)
    i = r + 0.5;
  else
    i = r - 0.5;

  return i;
}
