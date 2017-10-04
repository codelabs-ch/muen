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

with Bfd.Files;

with Mutools;

with Types;

package body Binary.Sections is

   function Element (Iter : Section_Iterator) return Section
     is (Section (Bfd.Sections.Element
                    (Bfd.Sections.Section_Iterator (Iter))));

   --------------------------------------------------------------------------

   function Get_Bfd_Section
     (Descriptor   : Binary.Files.File_Type;
      Section_Name : String)
      return Section
   is
   begin
      return Section (Bfd.Sections.Find_Section
                        (File => Bfd.Files.File_Type (Descriptor),
                         Name => Section_Name));

   exception
      when Bfd.NOT_FOUND =>
         raise Types.Bin_Split_Error
           with "Section '" & Section_Name & "' not found";
   end Get_Bfd_Section;

   --------------------------------------------------------------------------

   function Get_Flags (S : Section) return Section_Flags
     is (Section_Flags (Bfd.Sections.Section (S).Flags));

   --------------------------------------------------------------------------

   function Get_Lma (S : Section) return Lma_Type
     is (Lma_Type (Bfd.Sections.Section (S).Lma));

   --------------------------------------------------------------------------

   function Get_Name (S : Section) return String
     is (Bfd.Sections.Get_Name (Bfd.Sections.Section (S)));

   --------------------------------------------------------------------------

   function Get_Sections
     (File : Binary.Files.File_Type)
      return Section_Iterator
     is (Section_Iterator
           (Bfd.Sections.Get_Sections (Bfd.Files.File_Type (File))));

   --------------------------------------------------------------------------

   procedure Get_Section_Contents
     (File : Binary.Files.File_Type;
      S    : Section;
      Pos  : Ada.Streams.Stream_Element_Offset := 0;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
   begin
      Bfd.Sections.Get_Section_Contents
        (File => Bfd.Files.File_Type (File),
         S => Bfd.Sections.Section (S),
         Pos => Pos,
         Item => Item,
         Last => Last);
   end Get_Section_Contents;

   --------------------------------------------------------------------------

   function Get_Size (S : Section) return Size_Type
     is (Size_Type (Bfd.Sections.Section (S).Size));

   --------------------------------------------------------------------------

   function Get_Vma (S : Section) return Vma_Type
     is (Vma_Type (Bfd.Sections.Section (S).Vma));

   --------------------------------------------------------------------------

   function Has_Element (Iter : Section_Iterator) return Boolean
     is (Bfd.Sections.Has_Element (Bfd.Sections.Section_Iterator (Iter)));

   --------------------------------------------------------------------------

   procedure Next (Iter : in out Section_Iterator)
   is
   begin
      Bfd.Sections.Next (Bfd.Sections.Section_Iterator (Iter));
   end Next;

end Binary.Sections;
