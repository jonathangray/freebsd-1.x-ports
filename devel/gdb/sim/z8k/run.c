/* run front end support for Z8KSIM
   Copyright (C) 1987, 1992 Free Software Foundation, Inc.

This file is part of Z8KSIM

Z8KSIM is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

Z8KSIM is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Z8KSIM; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "bfd.h"
#include "tm.h"
#include "sysdep.h"
main(ac,av)
int ac;
char **av;
{
  bfd *abfd;
  bfd_vma start_address;
  asection *s;
  int i;
  int verbose = 0;
  int trace = 0;
  char *name = "";
  for (i = 1; i < ac; i++)
  {
    if (strcmp(av[i],"-v") == 0) 
    {
      verbose = 1;
    }
    else if (strcmp(av[i],"-t") == 0) 
    {
      trace = 1;
    }

    else 
    {
      name = av[i];
    }
  }
  if (verbose)
  {
    printf("run %s\n", name);
  }
  abfd = bfd_openr(name,"coff-z8k");

  if (abfd) {
      
    if (bfd_check_format(abfd, bfd_object)) 
    {
      sim_state_type info;
      if (abfd->arch_info->mach == bfd_mach_z8001)
      {
	extern int sim_z8001_mode;
	sim_z8001_mode = 1;
      }
      for (s = abfd->sections; s; s=s->next) 
      {
	char *buffer = malloc(bfd_section_size(abfd,s));
	bfd_get_section_contents(abfd, s, buffer, 0, bfd_section_size(abfd,s));
	sim_write(s->vma, buffer, bfd_section_size(abfd,s));
      }

      start_address = bfd_get_start_address(abfd);
      sim_set_pc(start_address);
      if (trace) 
      {
	int done = 0;
	while (!done) 
	{
	  done = sim_trace();
	}
      }
      else 
      { 
	sim_resume(0,0);
      }
      if (verbose) 
      {
	sim_info(&info);
	sim_info_print(&info);
      }
      return 0;
    }
  }

  return 1;
}
