--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Muxml;

with Bfd;
with Bfd.Files;
with Bfd.Sections;

with Ada.Strings.Unbounded;

package Ext_Cspec
is

   procedure Run (Spec_File, Binary, Output_Spec : String);

   Ext_Cspec_Error : exception;

private
   type Section_Info is
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
         Write_To_File : Boolean;
         Flags : Bfd.Section_Flags;
      end record;

   type SI_Array is array (Positive range <>)
     of Section_Info;

   type Compound_Section_Info is
      record
         Infos                : access SI_Array;
         Fill                 : Boolean;
         Writable, Executable : Boolean;
      end record;

   type CSI_Array
      is array (Positive range <>) of Compound_Section_Info;

   procedure Add_Entry
     (Spec                  : Muxml.XML_Data_Type;
      Logical               : String;
      Writable, Executable  : Boolean;
      Fill                  : Boolean             := False;
      Hash, File_Name       : String              := "";
      Fill_Pattern          : Bfd.Unsigned_64     := Bfd.Unsigned_64 (0);
      Size, Virtual_Address : Bfd.Unsigned_64);

   procedure Check_Address (Section : Bfd.Sections.Section);

   procedure Check_Section_Names (Descriptor : Bfd.Files.File_Type);

   procedure Check_Flags (Sec_Info   : Section_Info;
                          Descriptor : Bfd.Files.File_Type);

   function Get_Compound_Section_Infos return CSI_Array;

   procedure Write_Compound_Section
     (Info             : Compound_Section_Info;
      Output_File_Name : String;
      Descriptor       : Bfd.Files.File_Type);

   procedure Open
     (Filename   :     String;
      Descriptor : out Bfd.Files.File_Type);

   function Get_Bfd_Section
     (Descriptor   : Bfd.Files.File_Type;
      Section_Name : String)
     return Bfd.Sections.Section;

end Ext_Cspec;
