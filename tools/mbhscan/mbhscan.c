//
//  Copyright (C) 2014  Alexander Senier <mail@senier.net>
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "multiboot.h"
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <err.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

int
main(int argc, char *argv[])
{
   int fd, rv;
   const char* file = argv[1];
   struct stat statbuf;
   void *base, *addr;
   struct multiboot_header *mbh;

   if (argc < 2)
   {
      errx (1, "invalid arguments");
   }

   fd = open (file, O_RDONLY);
   if (-1 == fd)
   {
      err (1, "open");
   }

   rv = fstat (fd, &statbuf);
   if (-1 == rv)
   {
      err (1, "stat");
   }

   printf ("Opening '%s' (%lu bytes)\n", file, statbuf.st_size);

   addr = mmap (NULL, statbuf.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
   if (addr == MAP_FAILED)
   {
      err (1, "mmap");
   }

   base = addr;
   while (base - addr < statbuf.st_size - sizeof (struct multiboot_header))
   {
      mbh = (struct multiboot_header *)base;
      if (mbh->magic == MULTIBOOT_HEADER_MAGIC)
      {
         printf ("Multiboot header found at offset %lu\n", base - addr);
         if ((mbh->magic + mbh->flags + mbh->checksum))
         {
            printf ("...checksum INVALID\n");
         } else
         {
            printf ("   checksum OK\n");
            if (mbh->flags & MULTIBOOT_PAGE_ALIGN)
            {
               printf ("   page align\n");
            }
            if (mbh->flags & MULTIBOOT_MEMORY_INFO)
            {
               printf ("   memory info\n");
            }
            if (mbh->flags & MULTIBOOT_VIDEO_MODE)
            {
               printf ("   video mode\n");
            }
            if (mbh->flags & MULTIBOOT_AOUT_KLUDGE)
            {
               printf ("   aout kludge (header=0x%x, load=0x%x, end=0x%x, bss=0x%x, entry=0x%x)\n",
                  mbh->header_addr, mbh->load_addr, mbh->load_end_addr,
                  mbh->bss_end_addr, mbh->entry_addr);
               printf ("   header base: %lx\n", base - addr + mbh->load_addr);
            }
         }
      }
      ++base;
   }

   return 0;
}
