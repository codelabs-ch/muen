--
--  Copyright (C) 2017  secunet Security Networks AG
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

with Ada.Strings.Unbounded;

with Bfd.Files;
with Bfd.Sections;
with Bfd.Constants;

package Bin_Split.Run
is

   --  Given the filename "Spec_File" of a component specification, as well as
   --  the filename "Binary_File" of a component binary, this procedure checks
   --  whether the binary corresponds to the specification.
   --
   --  If so, the binary is split into its respective sections, their
   --  definitions are amended to the specification, and the latter is written
   --  to filename "Output_Spec".
   --
   --  Otherwise, the program terminates with an error message.
   --
   --  If `Output_Dir' is not the empty string, then all output files are
   --  written to the directory `Output_Dir'.
   procedure Run
     (Spec_File   : String;
      Binary_File : String;
      Output_Spec : String;
      Output_Dir  : String := "");

   use type Bfd.Section_Flags;

   --  Section_Infos is the default SI_Array.
   --  An SI_Array (array of section infos) contains a description of the
   --  respective sections the input binary is to be split into.
   Section_Infos : constant SI_Array
     := ((Name          => Ada.Strings.Unbounded.To_Unbounded_String (".text"),
          Write_To_File => True,
          Flags         =>
            Bfd.Constants.SEC_HAS_CONTENTS or Bfd.Constants.SEC_ALLOC
              or Bfd.Constants.SEC_LOAD or Bfd.Constants.SEC_READONLY
              or Bfd.Constants.SEC_CODE,
          Fill_Pattern  => 16#00#,
          Writable      => False,
          Executable    => True,
          Optional      => False),
         (Name          =>
            Ada.Strings.Unbounded.To_Unbounded_String (".rodata"),
          Write_To_File => True,
          Flags         =>
            Bfd.Constants.SEC_HAS_CONTENTS or Bfd.Constants.SEC_ALLOC
              or Bfd.Constants.SEC_LOAD or Bfd.Constants.SEC_READONLY
              or Bfd.Constants.SEC_DATA,
          Fill_Pattern  => 16#00#,
          Writable      => False,
          Executable    => False,
          Optional      => False),
         (Name          =>
            Ada.Strings.Unbounded.To_Unbounded_String (".data"),
          Write_To_File => True,
          Flags         =>
            Bfd.Constants.SEC_HAS_CONTENTS or Bfd.Constants.SEC_ALLOC
              or Bfd.Constants.SEC_LOAD or Bfd.Constants.SEC_DATA,
          Fill_Pattern  => 16#00#,
          Writable      => True,
          Executable    => False,
          Optional      => False),
         (Name          =>
            Ada.Strings.Unbounded.To_Unbounded_String (".bss"),
          Write_To_File => False,
          Flags         => Bfd.Constants.SEC_ALLOC,
          Fill_Pattern  => 16#00#,
          Writable      => True,
          Executable    => False,
          Optional      => True),
         (Name          =>
            Ada.Strings.Unbounded.To_Unbounded_String (".stack"),
          Write_To_File => False,
          Flags         => Bfd.Constants.SEC_ALLOC,
          Fill_Pattern  => 16#00#,
          Writable      => True,
          Executable    => False,
          Optional      => False));

private

   --  Checks whether address of section "Section" is page-aligned. Moreover,
   --  the logical and virtual address of "Section" are checked that they are
   --  identical.
   --
   --  A Bin_Split_Error exception is raised if either check fails.
   procedure Check_Alignment (Section : Bfd.Sections.Section);

   --  Checks binary referred to by "Descriptor" for unknown sections.
   --
   --  A Bin_Split_Error exception is raised if an unknown section is detected.
   procedure Check_Section_Names (Descriptor : Bfd.Files.File_Type);

   --  Checks whether section flags of binary referred to by "Descriptor" are
   --  consistent with the values prescribed in "Section_Info".
   --
   --  A Bin_Split_Error exception is raised if an inconsistency is detected.
   procedure Check_Flags
     (Sec_Info   : Section_Info;
      Descriptor : Bfd.Files.File_Type);

   --  Checks whether a section with name "Section_Name" is contained in
   --  "Section_Infos".
   function Is_Valid_Section
     (Section_Name  : String;
      Section_Infos : SI_Array)
      return Boolean;

   --  Returns True and the requested binary section if present in the binary
   --  given by descriptor.
   --
   --  If the section is not found, and the section is not marked optional in
   --  the section info, an exception is raised. False is returned if the
   --  section is marked as optional in the section info record.
   function Get_Binary_Section
     (Descriptor :     Bfd.Files.File_Type;
      Sec_Info   :     Section_Info;
      Sec        : out Bfd.Sections.Section)
      return Boolean;

end Bin_Split.Run;
