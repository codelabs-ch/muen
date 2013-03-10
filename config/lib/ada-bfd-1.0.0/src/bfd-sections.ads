-----------------------------------------------------------------------
--  BFD -- Binary File Descriptor Library (Ada Interface)
--  Copyright (C) 2002, 2003, 2004, 2012 Free Software Foundation, Inc.
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
--  The <tt>Bfd.Sections</tt> package gives access to the sections that
--  compose a binary program.
--
with Ada.Streams;

with Bfd.Constants;
with Bfd.Files;
package Bfd.Sections is

   ----------------------
   -- Sections         --
   ----------------------
   --  The following types and functions deal with sections and
   --  how to access/scan them.

   --  Constants below are extracted from the BFD C source file
   --  CODE FRAGMENT:  bfd/section.c:
   SEC_NO_FLAGS : constant Section_Flags := Constants.SEC_NO_FLAGS;

   SEC_ALLOC : constant Section_Flags := Constants.SEC_ALLOC;
   --  Tells the OS to allocate space for this section when loading.
   --  This is clear for a section containing debug information only.

   SEC_LOAD : constant Section_Flags := Constants.SEC_LOAD;
   --  Tells the OS to load the section from the file when loading.
   --  This is clear for a @.bss@ section.

   SEC_RELOC : constant Section_Flags := Constants.SEC_RELOC;
   --  The section contains data still to be relocated, so there is
   --  some relocation information too.

   SEC_READONLY : constant Section_Flags := Constants.SEC_READONLY;
   --  A signal to the OS that the section contains read only data.

   SEC_CODE : constant Section_Flags := Constants.SEC_CODE;
   --  The section contains code only.

   SEC_DATA : constant Section_Flags := Constants.SEC_DATA;
   --  The section contains data only.

   SEC_ROM : constant Section_Flags := Constants.SEC_ROM;
   --  The section will reside in ROM.

   SEC_CONSTRUCTOR : constant Section_Flags := Constants.SEC_CONSTRUCTOR;
   --  The section contains constructor information. This section
   --  type is used by the linker to create lists of constructors and
   --  destructors used by <<g++>>. When a back end sees a symbol
   --  which should be used in a constructor list, it creates a new
   --  section for the type of name (e.g., <<__CTOR_LIST__>>), attaches
   --  the symbol to it, and builds a relocation. To build the lists
   --  of constructors, all the linker has to do is catenate all the
   --  sections called <<__CTOR_LIST__>> and relocate the data
   --  contained within - exactly the operations it would peform on
   --  standard data.

   SEC_HAS_CONTENTS : constant Section_Flags := Constants.SEC_HAS_CONTENTS;
   --  The section has contents - a data section could be
   --  <<SEC_ALLOC>> | <<SEC_HAS_CONTENTS>>; a debug section could be
   --  <<SEC_HAS_CONTENTS>>

   SEC_NEVER_LOAD : constant Section_Flags := Constants.SEC_NEVER_LOAD;
   --  An instruction to the linker to not output the section
   --  even if it has information which would normally be written.

   SEC_THREAD_LOCAL : constant Section_Flags := Constants.SEC_THREAD_LOCAL;
   --  The Section Contains Thread Local Data.

   SEC_COFF_SHARED_LIBRARY : constant Section_Flags := Constants.SEC_COFF_SHARED_LIBRARY;
   --  The section is a COFF shared library section.  This flag is
   --  only for the linker.  If this type of section appears in
   --  the input file, the linker must copy it to the output file
   --  without changing the vma or size.  FIXME: Although this
   --  was originally intended to be general, it really is COFF
   --  specific (and the flag was renamed to indicate this).  It
   --  might be cleaner to have some more general mechanism to
   --  allow the back end to control what the linker does with
   --  sections.

   SEC_HAS_GOT_REF : constant Section_Flags := Constants.SEC_HAS_GOT_REF;
   --  The section has GOT references.  This flag is only for the
   --  linker, and is currently only used by the elf32-hppa back end.
   --  It will be set if global offset table references were detected
   --  in this section, which indicate to the linker that the section
   --  contains PIC code, and must be handled specially when doing a
   --  static link.

   SEC_IS_COMMON : constant Section_Flags := Constants.SEC_IS_COMMON;
   --  The section contains common symbols (symbols may be defined
   --  multiple times, the value of a symbol is the amount of
   --  space it requires, and the largest symbol value is the one
   --  used).  Most targets have exactly one of these (which we
   --  translate to bfd_com_section_ptr), but ECOFF has two.

   SEC_DEBUGGING : constant Section_Flags := Constants.SEC_DEBUGGING;
   --  The section contains only debugging information.  For
   --  example, this is set for ELF .debug and .stab sections.
   --  strip tests this flag to see if a section can be
   --  discarded.

   SEC_IN_MEMORY : constant Section_Flags := Constants.SEC_IN_MEMORY;
   --  The contents of this section are held in memory pointed to
   --  by the contents field.  This is checked by bfd_get_section_contents,
   --  and the data is retrieved from memory if appropriate.

   SEC_EXCLUDE : constant Section_Flags := Constants.SEC_EXCLUDE;
   --  The contents of this section are to be excluded by the
   --  linker for executable and shared objects unless those
   --  objects are to be further relocated.

   SEC_SORT_ENTRIES : constant Section_Flags := Constants.SEC_SORT_ENTRIES;
   --  The contents of this section are to be sorted based on the sum of
   --  the symbol and addend values specified by the associated relocation
   --  entries.  Entries without associated relocation entries will be
   --  appended to the end of the section in an unspecified order.

   SEC_LINK_ONCE : constant Section_Flags := Constants.SEC_LINK_ONCE;
   --  When linking, duplicate sections of the same name should be
   --  discarded, rather than being combined into a single section as
   --  is usually done.  This is similar to how common symbols are
   --  handled.  See SEC_LINK_DUPLICATES below.

   SEC_LINK_DUPLICATES : constant Section_Flags := Constants.SEC_LINK_DUPLICATES;
   --  If SEC_LINK_ONCE is set, this bitfield describes how the linker
   --  should handle duplicate sections.

   SEC_LINK_DUPLICATES_DISCARD : constant Section_Flags := Constants.SEC_LINK_DUPLICATES_DISCARD;
   --  This value for SEC_LINK_DUPLICATES means that duplicate
   --  sections with the same name should simply be discarded.

   SEC_LINK_DUPLICATES_ONE_ONLY : constant Section_Flags := Constants.SEC_LINK_DUPLICATES_ONE_ONLY;
   --  This value for SEC_LINK_DUPLICATES means that the linker
   --  should warn if there are any duplicate sections, although
   --  it should still only link one copy.

   SEC_LINK_DUPLICATES_SAME_SIZE : constant Section_Flags
     := Constants.SEC_LINK_DUPLICATES_SAME_SIZE;
   --  This value for SEC_LINK_DUPLICATES means that the linker
   --  should warn if any duplicate sections are a different size.

   SEC_LINK_DUPLICATES_SAME_CONTENTS : constant Section_Flags
     := Constants.SEC_LINK_DUPLICATES_SAME_CONTENTS;
   --  This value for SEC_LINK_DUPLICATES means that the linker
   --  should warn if any duplicate sections contain different
   --  contents.

   SEC_LINKER_CREATED : constant Section_Flags := Constants.SEC_LINKER_CREATED;
   --  This section was created by the linker as part of dynamic
   --  relocation or other arcane processing.  It is skipped when
   --  going through the first-pass output, trusting that someone
   --  else up the line will take care of it later.

   SEC_KEEP : constant Section_Flags := Constants.SEC_KEEP;
   --  This section should not be subject to garbage collection.

   SEC_SMALL_DATA : constant Section_Flags := Constants.SEC_SMALL_DATA;
   --  This section contains "short" data, and should be placed
   --  "near" the GP.

   SEC_COFF_SHARED : constant Section_Flags := Constants.SEC_COFF_SHARED;
   --  This section contains data which may be shared with other
   --  executables or shared objects.  */

   SEC_TIC54X_BLOCK : constant Section_Flags := Constants.SEC_TIC54X_BLOCK;
   --  When a section with this flag is being linked, then if the size of
   --  the input section is less than a page, it should not cross a page
   --  boundary.  If the size of the input section is one page or more, it
   --  should be aligned on a page boundary.  */

   SEC_TIC54X_CLINK : constant Section_Flags := Constants.SEC_TIC54X_CLINK;
   --  Conditionally link this section; do not link if there are no
   --  references found to any symbol in the section.  */

   SEC_MERGE : constant Section_Flags := Constants.SEC_MERGE;
   --  Attempt to merge identical entities in the section.
   --  Entity size is given in the entsize field.  */

   SEC_STRINGS : constant Section_Flags := Constants.SEC_STRINGS;
   --  If given with SEC_MERGE, entities to merge are zero terminated
   --  strings where entsize specifies character size instead of fixed
   --  size entries.

   SEC_GROUP : constant Section_Flags := Constants.SEC_GROUP;
   --  This section contains data about section groups.

   SEC_ELF_REVERSE_COPY : constant Section_Flags := Constants.SEC_ELF_REVERSE_COPY;
   --  This input section should be copied to output in reverse order
   --  as an array of pointers.  This is for ELF linker internal use
   --  only.

   --  END FRAGMENT: bfd/section.c

   ----------------------
   --  Section iterator
   ----------------------
   --  Section iterator to walk the sections of a file.
   type Section_Iterator is private;

   ----------------------
   --  Section
   ----------------------
   --  Represents a section.
   --  The opaque is really the C section pointer.
   type Section is record
      Vma    : Vma_Type;
      Lma    : Lma_Type;
      Size   : Size_Type;
      Flags  : Section_Flags;
      Opaque : Section_Iterator;
   end record;

   --  Get an iterator to scan the BFD sections.
   function Get_Sections (File : in Bfd.Files.File_Type) return Section_Iterator;

   --  Return true if the iterator contains an element.
   function Has_Element (Iter : in Section_Iterator) return Boolean;

   --  Move to the next section.
   procedure Next (Iter : in out Section_Iterator);

   --  Return the current section pointed to by the iterator.
   function Element (Iter : in Section_Iterator) return Section;

   --  Return the name of the section.
   function Get_Name (S : in Section) return String;

   --  Return true if this is the UNDEF section.
   function Is_Undefined_Section (S : in Section) return Boolean;

   --  Return true if this is the COMMON section.
   function Is_Common_Section (S : in Section) return Boolean;

   --  Return true if this is the ABS section.
   function Is_Absolute_Section (S : in Section) return Boolean;

   --  Get the content of the section starting at the given position.
   --  The result is truncated if the buffer is not large enough
   procedure Get_Section_Contents
     (File : in Bfd.Files.File_Type;
      S    : in Section;
      Pos  : in Ada.Streams.Stream_Element_Offset := 0;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   --  Find the section given its name.
   --  Raises NOT_FOUND if the section does not exist.
   function Find_Section (File : in Bfd.Files.File_Type;
                          Name : in String) return Section;

private
   type Section_Iterator is new System.Address;
end Bfd.Sections;
