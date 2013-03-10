-----------------------------------------------------------------------
--  BFD -- Binary File Descriptor Library (Ada Interface)
--  Copyright (C) 2001, 2002, 2003, 2004, 2012 Free Software Foundation, Inc.
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  This file is part of BfdAda.
--
--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2,
--  or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation,51 Franklin Street - Fifth Floor,
--  Boston, MA 02110-1301, USA.
-----------------------------------------------------------------------
with System;
with Interfaces;
with Interfaces.C.Strings;
with Ada.Finalization;
with Bfd.Constants;

--  The Bfd package exports the GNU Bfd library found in Binutils
--  and Gdb.  It is not intended to be as complete as the C library
--  but still provide enough methods to read any object or binary,
--  observe its sections, its symbol table.
package Bfd.Files is

   --  The bfd_format.
   type Format is (UNKNOWN, --  unknown file format
                   OBJECT,  --  linker/assembler/compiler object file
                   ARCHIVE, --  object archive file
                   CORE);   --  core dump

   --  Values that may appear in the flags field of a BFD.  These also
   --  appear in the object_flags field of the bfd_target structure, where
   --  they indicate the set of flags used by that backend (not all flags
   --  are meaningful for all object file formats) (FIXME: at the moment,
   --  the object_flags values have mostly just been copied from backend
   --  to another, and are not necessarily correct).

   BFD_NO_FLAGS : constant File_Flags := Constants.BFD_NO_FLAGS;

   --  BFD contains relocation entries.
   HAS_RELOC : constant File_Flags := Constants.HAS_RELOC;

   --  BFD is directly executable.
   EXEC_P : constant File_Flags := Constants.EXEC_P;

   --  BFD has line number information (basically used for F_LNNO in a
   --  COFF header).
   HAS_LINENO : constant File_Flags := Constants.HAS_LINENO;

   --  BFD has debugging information.
   HAS_DEBUG : constant File_Flags := Constants.HAS_DEBUG;

   --  BFD has symbols.
   HAS_SYMS : constant File_Flags := Constants.HAS_SYMS;

   --  BFD has local symbols (basically used for F_LSYMS in a COFF
   --  header).
   HAS_LOCALS : constant File_Flags := Constants.HAS_LOCALS;

   --  BFD is a dynamic object.
   DYNAMIC : constant File_Flags := Constants.DYNAMIC;

   --  Text section is write protected (if D_PAGED is not set, this is
   --  like an a.out NMAGIC file) (the linker sets this by default, but
   --  clears it for -r or -N).
   WP_TEXT : constant File_Flags := Constants.WP_TEXT;

   --  BFD is dynamically paged (this is like an a.out ZMAGIC file) (the
   --  linker sets this by default, but clears it for -r or -n or -N).
   D_PAGED : constant File_Flags := Constants.D_PAGED;

   --  BFD is relaxable (this means that bfd_relax_section may be able to
   --  do something) (sometimes bfd_relax_section can do something even if
   --  this is not set).
   BFD_IS_RELAXABLE : constant File_Flags := Constants.BFD_IS_RELAXABLE;

   --  This may be set before writing out a BFD to request using a
   --  traditional format.  For example, this is used to request that when
   --  writing out an a.out object the symbols not be hashed to eliminate
   --  duplicates.
   BFD_TRADITIONAL_FORMAT : constant File_Flags := Constants.BFD_TRADITIONAL_FORMAT;

   --  This flag indicates that the BFD contents are actually cached
   --  in memory.  If this is set, iostream points to a bfd_in_memory
   --  struct.
   BFD_IN_MEMORY : constant File_Flags := Constants.BFD_IN_MEMORY;

   --  The sections in this BFD specify a memory page.
   HAS_LOAD_PAGE : constant File_Flags := Constants.HAS_LOAD_PAGE;

   --  This BFD has been created by the linker and doesn't correspond
   --  to any input file.
   BFD_LINKER_CREATED : constant File_Flags := Constants.BFD_LINKER_CREATED;

   --  This may be set before writing out a BFD to request that it
   --  be written using values for UIDs, GIDs, timestamps, etc. that
   --  will be consistent from run to run.
   BFD_DETERMINISTIC_OUTPUT : constant File_Flags := Constants.BFD_DETERMINISTIC_OUTPUT;

   --  Compress sections in this BFD.
   BFD_COMPRESS : constant File_Flags := Constants.BFD_COMPRESS;

   --  Decompress sections in this BFD.
   BFD_DECOMPRESS : constant File_Flags := Constants.BFD_DECOMPRESS;

   --  BFD is a dummy, for plugins.
   BFD_PLUGIN : constant File_Flags := Constants.BFD_PLUGIN;

   ----------------------
   -- BFD file         --
   ----------------------
   --  This part deal with opening and closing the main BFD file.

   --  The file type representing the opened BFD file.
   type File_Type is limited private;

   --  Open the file and obtain a bfd handler.
   --  Raises OPEN_ERROR if the file cannot be opened.
   procedure Open (File   : in out File_Type;
                   Name   : in String;
                   Target : in String := "");

   --  Close the file, releasing any resource allocated for it.
   procedure Close (File : in out File_Type);

   --  Check if the file is open.
   --  Returns true if the file is open
   function Is_Open (File : in File_Type) return Boolean;

   --  Get the filename that was used to open the file.
   function Get_Filename (File : in File_Type) return String;

   --  Check if the file is of the specified format.
   --  Returns true if the file is open and of the specified format
   function Check_Format (File   : in File_Type;
                          Expect : in Format) return Boolean;

   --  Get the BFD file flags.
   function Get_File_Flags (File : in File_Type) return File_Flags;

   --  Get the start address.
   function Get_Start_Address (File : in File_Type) return Vma_Type;

   --  Return number of symbols.
   function Get_Symbol_Count (File : in File_Type) return Natural;

   --  Get the pointer to the BFD structure allocated for the file.
   function Get_Bfd_Pointer (File : in File_Type) return Ptr;

private
   subtype Pointer is System.Address;

   type Bfd_Ptr is new System.Address;

   type File_Type is new Ada.Finalization.Limited_Controlled with record
      Abfd : Ptr := System.Null_Address;
      Name : Interfaces.C.Strings.chars_ptr;
   end record;

   overriding
   procedure Finalize (File : in out File_Type);

   pragma Inline (Get_Bfd_Pointer);

end Bfd.Files;
