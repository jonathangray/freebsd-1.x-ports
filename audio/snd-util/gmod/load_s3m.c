
#include <sys/types.h>
#include <stdio.h>

#include "defines.h"
#include "structs.h"

int
load_s3m_module (int mod_fd, char *name, struct song_info *song_char,
		 struct options_info options)
{
  fprintf (stderr, "ScreamTracker III modules not supported yet\n");
  return (0);
}
