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

with Muxml;

with Interfaces;

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
   procedure Run (Spec_File, Binary_File, Output_Spec_File : String);

private

   --  TODO: Document (after splitting up)
   procedure Add_Entry
     (Spec                      : Muxml.XML_Data_Type;
      Logical                   : String;
      Writable, Executable      : Boolean;
      Fill                      : Boolean                       := False;
      Hash, File_Name           : String                        := "";
      Fill_Pattern              : Interfaces.Unsigned_64        := 0;
      Size, Virtual_Address     : Interfaces.Unsigned_64);

   --  Checks whether address of section "Section" is page-aligned. Moreover,
   --  the logical and virtual address of "Section" are asserted to be
   --  identical.
   --
   --  Raises Bin_Split_Error exception if either check fails.
   procedure Check_Alignment (Section : Bin_Split.Binary.Sections.Section);

   --  Checks binary referred to by "Descriptor" for unknown sections.
   --
   --  Raises Bin_Split_Error exception if an unknown section is detected.
   procedure Check_Section_Names
     (Descriptor : Bin_Split.Binary.Files.File_Type);

   --  Checks whether section flags of binary referred to by "Descriptor" are
   --  consistent with the values prescribed in "Section_Info".
   --
   --  Raises Bin_Split_Error exception if an inconsistency is detected.
   procedure Check_Flags (Sec_Info   : Bin_Split.Types.Section_Info;
                          Descriptor : Bin_Split.Binary.Files.File_Type);

   --  Get_Compound_Section_Infos returns the default CSI_Array.
   --
   --  A CSI_Array (array of compound sections) contains a description of the
   --  respective sections the input binary is to be split into.  Each element
   --  of the array contains the permissions of the respective section to be
   --  written, as well as an array of sections of the input binary which are
   --  to be written to the respective section of the output binary.
   function Get_Compound_Section_Infos return Bin_Split.Types.CSI_Array;

end Bin_Split.Run;
