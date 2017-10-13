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

with Bin_Split.Types;
with Bin_Split.Binary.Files;
with Bin_Split.Binary.Sections;

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
     (Spec_File        : String;
      Binary_File      : String;
      Output_Spec_File : String;
      Output_Dir       : String := "");

   --  Checks whether address of section "Section" is page-aligned. Moreover,
   --  the logical and virtual address of "Section" are checked that they are
   --  identical.
   --
   --  Raises Bin_Split_Error exception if either check fails.
   procedure Check_Alignment (Section : Binary.Sections.Section);

   --  Checks binary referred to by "Descriptor" for unknown sections.
   --
   --  Raises Bin_Split_Error exception if an unknown section is detected.
   procedure Check_Section_Names
     (Descriptor : Binary.Files.File_Type);

   --  Checks whether section flags of binary referred to by "Descriptor" are
   --  consistent with the values prescribed in "Section_Info".
   --
   --  Raises Bin_Split_Error exception if an inconsistency is detected.
   procedure Check_Flags (Sec_Info   : Types.Section_Info;
                          Descriptor : Binary.Files.File_Type);

   --  Get_Section_Infos returns the default SI_Array.
   --
   --  An SI_Array (array of section infos) contains a description of the
   --  respective sections the input binary is to be split into.
   function Get_Section_Infos return Types.SI_Array;

end Bin_Split.Run;
