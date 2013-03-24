-----------------------------------------------------------------------
--  BFD -- Binary File Descriptor Library (Ada Interface)
--  Copyright (C) 2002, 2003, 2012 Free Software Foundation, Inc.
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
--  The Bfd package exports the GNU Bfd library found in Binutils
--  and Gdb.  It is not intended to be as complete as the C library
--  but still provide enough methods to read any object or binary,
--  observe its sections, its symbol table.
--
with Interfaces.C;
with Interfaces.C.Strings;
with Bfd.Thin.Sections;
package body Bfd.Sections is

   use type Interfaces.C.int;

   --  -----------------------
   --  Get an iterator to scan the BFD sections.
   --  -----------------------
   function Get_Sections (File : in Bfd.Files.File_Type) return Section_Iterator is
      Iter : Section_Iterator;
   begin
      Iter := Bfd.Thin.Sections.Get_Sections (Bfd.Files.Get_Bfd_Pointer (File));
      return Iter;
   end Get_Sections;

   --  -----------------------
   --  Return true if the iterator contains an element.
   --  -----------------------
   function Has_Element (Iter : in Section_Iterator) return Boolean is
      use type System.Address;
   begin
      return System.Address (Iter) /= System.Null_Address;
   end Has_Element;

   --  -----------------------
   --  Move to the next section.
   --  -----------------------
   procedure Next (Iter : in out Section_Iterator) is
   begin
      Iter := Bfd.Thin.Sections.Get_Next_Section (Iter);
   end Next;

   --  -----------------------
   --  Return the current section pointed to by the iterator.
   --  -----------------------
   function Element (Iter : in Section_Iterator) return Section is
      S : Section;
   begin
      S.Vma    := Bfd.Thin.Sections.Get_Section_Vma (Iter);
      S.Lma    := Bfd.Thin.Sections.Get_Section_Lma (Iter);
      S.Size   := Bfd.Thin.Sections.Get_Section_Size (Iter);
      S.Flags  := Bfd.Thin.Sections.Get_Section_Flags (Iter);
      S.Opaque := Iter;
      return S;
   end Element;

   --  -----------------------
   --  Return the name of the section.
   --  -----------------------
   function Get_Name (S : in Section) return String is
   begin
      return Interfaces.C.Strings.Value (Bfd.Thin.Sections.Get_Section_Name (S.Opaque));
   end Get_Name;

   --  -----------------------
   --  Return true if this is the UNDEF section.
   --  -----------------------
   function Is_Undefined_Section (S : in Section) return Boolean is

      function Is_Undefined (S : Section_Iterator) return Interfaces.C.int;
      pragma Import (C, Is_Undefined, "ada_bfd_is_und_section");

   begin
      return Is_Undefined (S.Opaque) /= 0;
   end Is_Undefined_Section;

   --  -----------------------
   --  Return true if this is the COMMON section.
   --  -----------------------
   function Is_Common_Section (S : in Section) return Boolean is

      function Is_Common (S : Section_Iterator) return Interfaces.C.int;
      pragma Import (C, Is_Common, "ada_bfd_is_com_section");

   begin
      return Is_Common (S.Opaque) /= 0;
   end Is_Common_Section;

   --  -----------------------
   --  Return true if this is the ABS section.
   --  -----------------------
   function Is_Absolute_Section (S : in Section) return Boolean is

      function Is_Absolute (S : Section_Iterator) return Interfaces.C.int;
      pragma Import (C, Is_Absolute, "ada_bfd_is_abs_section");

   begin
      return Is_Absolute (S.Opaque) /= 0;
   end Is_Absolute_Section;

   --  -----------------------
   --  Get the content of the section starting at the given position.
   --  The result is truncated if the buffer is not large enough
   --  -----------------------
   procedure Get_Section_Contents (File : in Bfd.Files.File_Type;
                                   S    : in Section;
                                   Pos  : in Ada.Streams.Stream_Element_Offset := 0;
                                   Item : out Ada.Streams.Stream_Element_Array;
                                   Last : out Ada.Streams.Stream_Element_Offset) is

      use type Ada.Streams.Stream_Element_Offset;

      function Get_Section_Contents (Bfd  : in Ptr;
                                     S    : in Section_Iterator;
                                     Buf  : in System.Address;
                                     Pos  : in Unsigned_64;
                                     Size : in Unsigned_64) return Interfaces.C.int;
      pragma Import (C, Get_Section_Contents, "ada_bfd_get_section_contents");

      Result : constant Boolean := Get_Section_Contents (Bfd.Files.Get_Bfd_Pointer (File),
                                                         S.Opaque,
                                                         Item (Item'First)'Address,
                                                         Unsigned_64 (Pos),
                                                         Item'Length) /= 0;
   begin
      if Result then
         Last := Item'First + Item'Length - 1;
      else
         Last := Item'First - 1;
      end if;
   end Get_Section_Contents;

   --  -----------------------
   --  Find the section given its name.
   --  Raises NOT_FOUND if the section does not exist.
   --  -----------------------
   function Find_Section (File : in Bfd.Files.File_Type;
                          Name : in String) return Section is
      Iter : Section_Iterator := Get_Sections (File);
   begin
      while Has_Element (Iter) loop
         declare
            S : constant Section := Element (Iter);
         begin
            if Get_Name (S) = Name then
               return S;
            end if;
            Next (Iter);
         end;
      end loop;
      raise NOT_FOUND with "Section '" & Name & "' not found";
   end Find_Section;

end Bfd.Sections;
