/* endian -- determine whether we are running on a BigEndian or LittleEndian
   machine. */

#include <stdio.h>

void
main (argc, argv)
  int argc;
  char *argv[];
{
  union
  {
    long l;
    char c[sizeof (long)];
  } u;
  
  u.l = 1;
  
  if (u.c[0] == 0)
    printf ("#define WEB2C_BIG_ENDIAN 1\n");
  else
    printf ("/* This architecture is LittleEndian.  */\n");
  
  exit (0);
}
