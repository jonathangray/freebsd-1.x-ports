#include <stdio.h>

extern void add2 (void);

void
add1 (void)
{
  printf ("in add1\n");
  fflush (stdout);
  add2 ();
}
