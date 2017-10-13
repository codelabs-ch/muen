--
--  Copyright (C) 2017  secunet Security Networks AG
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Ada.Streams;

with Interfaces;

with Bfd.Sections;

with Bin_Split.Binary.Files;

package Bin_Split.Binary.Sections is

   type Section is private;
   type Section_Iterator is private;

   subtype Vma_Type is Interfaces.Unsigned_64;
   subtype Lma_Type is Interfaces.Unsigned_64;
   subtype Size_Type is Interfaces.Unsigned_64;

   --  Return section of binary file (Descriptor) with name Section_Name.
   --
   --  Raises Bin_Split_Error if section not found.
   function Get_Section
     (Descriptor   : Binary.Files.File_Type;
      Section_Name : String)
      return Section;

   function Get_Vma (S : Section) return Vma_Type;
   function Get_Lma (S : Section) return Lma_Type;
   function Get_Size (S : Section) return Size_Type;
   function Get_Flags (S : Section) return Section_Flags;
   function Get_Name (S : Section) return String;

   -------------------------------------------------------------------
   --  Since Sections and Section_Iterator are private, their       --
   --  primitive subprograms are not re-exported.  The following    --
   --  boilerplate subprograms reexport them.  Can I somehow avoid  --
   --  this?                                                        --
   -------------------------------------------------------------------

   --  Get an iterator to scan the BFD sections.
   function Get_Sections
     (File : Binary.Files.File_Type)
      return Section_Iterator;

   --  Return true if the iterator contains an element.
   function Has_Element (Iter : Section_Iterator) return Boolean;

   --  Move to the next section.
   procedure Next (Iter : in out Section_Iterator);

   --  Return the current section pointed to by the iterator.
   function Element (Iter : Section_Iterator) return Section;

   --  Get the content of the section starting at the given position.
   --  The result is truncated if the buffer is not large enough.
   procedure Get_Section_Contents
     (File : Binary.Files.File_Type;
      S    : Section;
      Pos  : Ada.Streams.Stream_Element_Offset := 0;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

private

   type Section is new Bfd.Sections.Section;
   type Section_Iterator is new Bfd.Sections.Section_Iterator;

end Bin_Split.Binary.Sections;
