/* 8 and 16 bit COFF relocation functions, for BFD.
   Copyright 1990, 1991, 1992, 1993 Free Software Foundation, Inc.
   Written by Cygnus Support.

This file is part of BFD, the Binary File Descriptor library.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* 
Most of this hacked by  Steve Chamberlain,
			sac@cygnus.com 
*/

/* These routines are used by coff-h8300 and coff-z8k to do
   relocation.  */

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"
#include "seclet.h"
#include "obstack.h"
#include "coff/internal.h"
#include "libcoff.h"

extern bfd_error_vector_type bfd_error_vector;

bfd_vma bfd_coff_reloc16_get_value(reloc, seclet)
     arelent  *reloc;
     bfd_seclet_type *seclet;
{
  bfd_vma value;
  asymbol *symbol = *(reloc->sym_ptr_ptr);
  /* A symbol holds a pointer to a section, and an offset from the
     base of the section.  To relocate, we find where the section will
     live in the output and add that in */

  if (symbol->section == &bfd_und_section)
    {
      /* Ouch, this is an undefined symbol.. */
      bfd_error_vector.undefined_symbol(reloc, seclet);
      value = symbol->value;
    }
  else 
    {
      value = symbol->value +
	symbol->section->output_offset +
	  symbol->section->output_section->vma;
    }
  
  /* Add the value contained in the relocation */
  value += reloc->addend;
  
  return value;
}

void bfd_perform_slip(s, slip, input_section, value)
     asymbol **s;
     unsigned int slip;
     asection *input_section;
     bfd_vma value;
{
  /* Find all symbols past this point, and make them know
     what's happened */
  while (*s) 
    {
      asymbol *p = *s;
      if (p->section == input_section) 
	{
	  /* This was pointing into this section, so mangle it */
	  if (p->value > value)
	    {
	      p->value -= slip;
	    }
	}
      s++;
    }    
}

boolean 
bfd_coff_reloc16_relax_section (abfd, i, symbols)
     bfd *abfd;
     asection *i;
     asymbol **symbols;
{
  /* Get enough memory to hold the stuff */
  bfd *input_bfd = i->owner;
  asection *input_section = i;
  int shrink = 0 ;
  boolean new = false;
  
  bfd_size_type reloc_size = bfd_get_reloc_upper_bound(input_bfd,
						       input_section);
  arelent **reloc_vector = (arelent **)bfd_xmalloc(reloc_size);

  /* Get the relocs and think about them */
  if (bfd_canonicalize_reloc(input_bfd, 
			     input_section,
			     reloc_vector,
			     symbols))
    {
      arelent **parent;
      for (parent = reloc_vector; *parent; parent++) 
	{
	  shrink = bfd_coff_reloc16_estimate (abfd, input_section, symbols, *parent, shrink);
	}
    }

  input_section->_cooked_size -= shrink;  
  free((char *)reloc_vector);
  return new;
}

bfd_byte *
bfd_coff_reloc16_get_relocated_section_contents(in_abfd,
						  seclet,
						  data,
						  relocateable)
     bfd *in_abfd;
     bfd_seclet_type *seclet;
     bfd_byte *data;
     boolean relocateable;
{
  /* Get enough memory to hold the stuff */
  bfd *input_bfd = seclet->u.indirect.section->owner;
  asection *input_section = seclet->u.indirect.section;
  bfd_size_type reloc_size = bfd_get_reloc_upper_bound(input_bfd,
						       input_section);
  arelent **reloc_vector = (arelent **)bfd_xmalloc(reloc_size);
  
  /* If producing relocateable output, don't bother to relax.  */
  if (relocateable)
    return bfd_generic_get_relocated_section_contents (in_abfd, seclet,
						       data, relocateable);

  /* read in the section */
  bfd_get_section_contents(input_bfd,
			   input_section,
			   data,
			   0,
			   input_section->_raw_size);
  
  
  if (bfd_canonicalize_reloc(input_bfd, 
			     input_section,
			     reloc_vector,
			     seclet->u.indirect.symbols) )
    {
      arelent **parent = reloc_vector;
      arelent *reloc ;
    


      unsigned int dst_address = 0;
      unsigned int src_address = 0;
      unsigned int run;
      unsigned int idx;
    
      /* Find how long a run we can do */
      while (dst_address < seclet->size) 
	{
      
	  reloc = *parent;
	  if (reloc) 
	    {
	      /* Note that the relaxing didn't tie up the addresses in the
		 relocation, so we use the original address to work out the
		 run of non-relocated data */
	      run = reloc->address - src_address;
	      parent++;
	
	    }
	  else 
	    {
	      run = seclet->size - dst_address;
	    }
	  /* Copy the bytes */
	  for (idx = 0; idx < run; idx++)
	    {
	      data[dst_address++] = data[src_address++];
	    }
    
	  /* Now do the relocation */
    
	  if (reloc) 
	    {
	      bfd_coff_reloc16_extra_cases (in_abfd, seclet, reloc, data,
					    &src_address, &dst_address);
	    }    
	}
    }
  free((char *)reloc_vector);
  return data;
}

