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
#include <dis-asm.h>

extern void ada_dis_memory_handler(int status, bfd_vma addr,
                                   struct disassemble_info* info);

void
bfdada_memory_error_handler (int status, bfd_vma memaddr,
                          struct disassemble_info* info)
{
  ada_dis_memory_handler (status, (unsigned long long) memaddr,
                          info->application_data);
}

extern int ada_dis_symbol_at_address (bfd_vma addr,
                                      struct disassemble_info* info);

extern int ada_dis_read_memory_handler (bfd_vma addr, bfd_byte* myaddr,
                                        unsigned int length,
                                        struct disassemble_info *info);

/* Pseudo FILE object for strings.  */
typedef struct
{
  char *buffer;
  void *data;
  size_t alloc;
} SFILE;

extern void ada_disassembler_output (void* data, char* buffer);
extern void ada_disassembler_output_address (bfd_vma addr,
                                             struct disassemble_info *info);

/* sprintf to a "stream".  */

static int
dis_printf (SFILE *f, const char *format, ...)
{
  size_t n;
  va_list args;

  while (1)
    {
      size_t space = f->alloc;

      va_start (args, format);
      n = vsnprintf (f->buffer, space, format, args);
      va_end (args);

      if (space > n)
	break;
      
      f->alloc = (f->alloc + n) * 2;
      f->buffer = realloc (f->buffer, f->alloc);
    }
  ada_disassembler_output (f->data, f->buffer);

  return n;
}

void*
bfd_ada_disassembler_init (void* data, bfd* abfd, char* disassembler_options)
{
  struct disassemble_info* info
    = (struct disassemble_info*) malloc (sizeof (struct disassemble_info));

  SFILE* file = (SFILE*) malloc (sizeof (SFILE));

  file->buffer = (char*) malloc (1024);
  file->alloc = 1024;
  file->data = data;

  init_disassemble_info (info, (FILE*) file, (fprintf_ftype) dis_printf);
  info->application_data = data;
  info->memory_error_func = ada_dis_memory_handler;
  info->symbol_at_address_func = ada_dis_symbol_at_address;
  info->read_memory_func = ada_dis_read_memory_handler;
  info->print_address_func = ada_disassembler_output_address;

  info->flavour = bfd_get_flavour (abfd);
  info->arch = bfd_get_arch (abfd);
  info->mach = bfd_get_mach (abfd);
  info->disassembler_options = disassembler_options;
  info->octets_per_byte = bfd_octets_per_byte (abfd);

  if (bfd_big_endian (abfd))
    info->display_endian = info->endian = BFD_ENDIAN_BIG;
  else if (bfd_little_endian (abfd))
    info->display_endian = info->endian = BFD_ENDIAN_LITTLE;
  else
    /* ??? Aborting here seems too drastic.  We could default to big or little
       instead.  */
    info->endian = BFD_ENDIAN_UNKNOWN;

  /* Allow the target to customize the info structure.  */
  disassemble_init_for_target (info);

  return info;
}

void*
bfd_ada_disassembler_get_data (struct disassemble_info* info)
{
  return info->application_data;
}

int
bfd_ada_disassembler_disassemble (bfd* abfd, struct disassemble_info* info,
                                  bfd_vma vma)
{
  disassembler_ftype handler = disassembler (abfd);
  
  return (* handler) (vma, info);
}

void
bfd_ada_disassembler_set_symbol_table (struct disassemble_info *info,
                                       asymbol** syms, int count)
{
  info->symbols = syms;
  info->num_symbols = count;
}
  
void
bfd_ada_disassembler_set_buffer (struct disassemble_info *info,
                                 void* buffer, int size,
                                 bfd_vma buffer_vma)
{
  info->buffer = buffer;
  info->buffer_vma = buffer_vma;
  info->buffer_length = size;
  info->read_memory_func = buffer_read_memory;
}
