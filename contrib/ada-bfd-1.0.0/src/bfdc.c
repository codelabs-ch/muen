/* Functions for BfdAda
   Copyright 2001, 2002, 2003, 2012 Free Software Foundation, Inc.
   Contributed by Stephane Carrez (Stephane.Carrez@gmail.com)

This file is part of BfdAda.

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include <bfd.h>
#include <stdlib.h>
#include <stdarg.h>

void*
_bfd_get_filename (struct bfd *abfd)
{
  return bfd_get_filename (abfd);
}

void*
bfd_get_sections (struct bfd *abfd)
{
  return abfd->sections;
}

void*
bfd_next_section (struct bfd_section *sec)
{
  return sec->next;
}

const void*
_bfd_get_section_name (struct bfd_section *sec)
{
  return bfd_section_name (0, sec);
}

unsigned long long
_bfd_get_section_vma (struct bfd_section *sec)
{
  return (unsigned long long) bfd_section_vma (0, sec);
}

unsigned long long
_bfd_get_section_lma (struct bfd_section *sec)
{
  return (unsigned long long) bfd_section_lma (0, sec);
}

unsigned long long
_bfd_get_section_size (struct bfd_section *sec)
{
  return (unsigned long long) bfd_get_section_size (sec);
}

unsigned long
_bfd_get_section_flags (struct bfd_section *sec)
{
  return (unsigned long) sec->flags;
}

void
bfd_read_symbols (bfd *abfd, int *cnt, asymbol **sy)
{
  long size;

  if (!(bfd_get_file_flags (abfd) & HAS_SYMS))
    {
      *cnt = 0;
      return;
    }
  size = bfd_get_symtab_upper_bound (abfd);
  if (size < 0)
    {
      *cnt = -1;
      return;
    }
  *cnt = bfd_canonicalize_symtab (abfd, sy);
}

void
bfd_find_nearest_line_ (bfd *abfd,
                        struct bfd_section *sec,
                        asymbol **syms,
                        unsigned long long addr,
                        const char **name,
                        const char **func,
                        unsigned *line)
{
  if (bfd_find_nearest_line (abfd, sec, syms,
                             (bfd_vma) (addr) - sec->vma, name, func, line))
    return;

  *line = -1;
}

int
ada_bfd_is_abs_section (struct bfd_section* sec)
{
   return bfd_is_abs_section (sec);
}

int
ada_bfd_is_com_section (struct bfd_section* sec)
{
   return bfd_is_com_section (sec);
}

int
ada_bfd_is_und_section (struct bfd_section* sec)
{
   return bfd_is_und_section (sec);
}

struct bfd_section*
ada_bfd_asymbol_section (asymbol* sym)
{
   return sym->section;
}

const char*
ada_bfd_asymbol_name (asymbol* sym)
{
   return bfd_asymbol_name (sym);
}


unsigned long long
ada_bfd_asymbol_value (asymbol* sym)
{
   return bfd_asymbol_value (sym);
}

unsigned long
ada_bfd_asymbol_flags (asymbol* sym)
{
   return sym->flags;
}

unsigned long
ada_bfd_get_symtab_upper_bound (bfd* abfd)
{
   return bfd_get_symtab_upper_bound (abfd);
}

int
ada_bfd_get_section_contents (bfd* abfd, asection* sec,
                              void* p, unsigned long long pos,
                              unsigned long long size)
{
   return bfd_get_section_contents (abfd, sec, p,
                                    (file_ptr) pos, (bfd_size_type) size);
}

unsigned long long
ada_bfd_get_start_address (bfd* abfd)
{
   return bfd_get_start_address (abfd);
}

unsigned long
ada_bfd_get_symbol_count (bfd* abfd)
{
   return bfd_get_symcount (abfd);
}

int
ada_bfd_get_file_flags (bfd* abfd)
{
  return bfd_get_file_flags (abfd);
}
