/* HP PA-RISC SOM object file format:  definitions internal to BFD.
   Copyright (C) 1990-1991 Free Software Foundation, Inc.

   Contributed by the Center for Software Science at the
   University of Utah (pa-gdb-bugs@cs.utah.edu).

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

#ifndef _SOM_H
#define _SOM_H

#include "../bfd/sysdep.h"

#ifdef HOST_HPPAHPUX

#include <sys/core.h>
#include <sys/utsname.h>

#endif /* HOST_HPPAHPUX */

#ifdef HOST_HPPABSD

#include <a.out.h>

/* BSD uses a completely different scheme for object file identification.
   so for now, define _PA_RISC_ID to accept any random value for a model
   number.  */
#undef _PA_RISC_ID
#define _PA_RISC_ID(__m_num) 1

/* Not a very swift place to put it, but that's where the BSD port
   puts them.  */
#include "/hpux/usr/include/sys/core.h"

#endif /* HOST_HPPABSD */

struct header;
struct som_exec_auxhdr;
struct subspace_dictionary;

#define FILE_HDR_SIZE sizeof(struct header)
#define AUX_HDR_SIZE sizeof(struct som_exec_auxhdr)

typedef struct hppa_symbol
{
  asymbol symbol;
  short desc;
  char other;
  unsigned char type;
} hppa_symbol_type;

struct hppadata
{
  struct header *file_hdr;
  struct som_exec_auxhdr *aux_hdr;
  hppa_symbol_type *symbols;

  /* We remember these offsets so that after check_file_format, we have
     no dependencies on the particular format of the exec_hdr.  */

  file_ptr sym_filepos;
  file_ptr str_filepos;

  unsigned stringtab_size;

  /* Size of a symbol table entry in external form */
  unsigned hp_symbol_entry_size;
};

struct hppa_data_struct {
  struct hppadata a;
};

#define padata(bfd)		((bfd)->tdata.hppa_data->a)
#define obj_file_hdr(bfd)	(padata(bfd).file_hdr)
#define obj_aux_hdr(bfd)	(padata(bfd).aux_hdr)
#define obj_pa_symbols(bfd)	(padata(bfd).symbols)
#define obj_sym_filepos(bfd)	(padata(bfd).sym_filepos)
#define obj_str_filepos(bfd)	(padata(bfd).str_filepos)
#define obj_stringtab_size(bfd)	(padata(bfd).stringtab_size)

/* We take the address of the first element of an asymbol to ensure that the
   macro is only ever applied to an asymbol */
#define hppa_symbol(asymbol) ((hppa_symbol_type *)(&(asymbol)->the_bfd))

#endif /* _SOM_H */
