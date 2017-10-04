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

with Interfaces;

with Bfd.Files;
with Bfd.Sections;

with Ada.Strings.Unbounded;

package Bin_Split
is

   --  Implements the main program logic.
   --
   --  Given the filename "Spec_File" of a component specification, as well as
   --  the filename "Binary" of a component binary, the procedure checks
   --  whether the binary corresponds to the specification.
   --
   --  If so, the binary is split into its respective sections, their
   --  definitions are amended to the specification, and the latter is written
   --  to filename "Output_Spec".
   --
   --  Otherwise, the program terminates with an error message.
   procedure Run (Spec_File, Binary, Output_Spec : String);

   Bin_Split_Error : exception;

private

   type Section_Info is record
      Name              : Ada.Strings.Unbounded.Unbounded_String;
      Write_To_File     : Boolean;
      Flags             : Bfd.Section_Flags;
   end record;

   type SI_Array is array (Positive range <>) of Section_Info;

   type Compound_Section_Info is record
      Infos             : access SI_Array;
      Fill              : Boolean;
      Writable          : Boolean;
      Executable        : Boolean;
   end record;

   type CSI_Array is array (Positive range <>) of Compound_Section_Info;

   --  TODO: Document (after splitting up)
   procedure Add_Entry
     (Spec                      : Muxml.XML_Data_Type;
      Logical                   : String;
      Writable, Executable      : Boolean;
      Fill                      : Boolean                       := False;
      Hash, File_Name           : String                        := "";
      Fill_Pattern              : Interfaces.Unsigned_64        := 0;
      Size, Virtual_Address     : Interfaces.Unsigned_64);

   --  Checks whether address of section "Section" is page-aligned.  Moreover,
   --  the logical and virtual address of "Section" are asserted to be
   --  identical.
   --
   --  Raises Bin_Split_Error exception if either check fails.
   procedure Check_Alignment (Section : Bfd.Sections.Section);

   --  Checks binary referred to by "Descriptor" for unknown sections.
   --
   --  Raises Bin_Split_Error exception if an unknown section is detected.
   procedure Check_Section_Names (Descriptor : Bfd.Files.File_Type);

   --  Checks whether section flags of binary referred to by "Descriptor" are
   --  consistent with the values prescribed in "Section_Info".
   --
   --  Raises Bin_Split_Error exception if an inconsistency is detected.
   procedure Check_Flags (Sec_Info   : Section_Info;
                          Descriptor : Bfd.Files.File_Type);

   --  Get_Compound_Section_Infos returns the default CSI_Array.
   --
   --  A CSI_Array (array of compound sections) contains a description of the
   --  respective sections the input binary is to be split into.  Each element
   --  of the array contains the permissions of the respective section to be
   --  written, as well as an array of sections of the input binary which are
   --  to be written to the respective section of the output binary.
   function Get_Compound_Section_Infos return CSI_Array;

   --  Write sections of input binary which correspond to a compound section
   --  (see above) to output binary.  The input binary is given by a File_Type
   --  (Descriptor), and the output binary by a file name (Output_File_Name).
   procedure Write_Compound_Section
     (Info             : Compound_Section_Info;
      Output_File_Name : String;
      Descriptor       : Bfd.Files.File_Type);

   --  Open a binary object file referenced to by Filename.
   --
   --  Raises Bin_Split_Error if file is no binary object, and OPEN_ERROR if
   --  file could not be opened.
   procedure Open
     (Filename   :     String;
      Descriptor : out Bfd.Files.File_Type);

   --  Return section of binary file (Descriptor) with name Section_Name.
   --
   --  Raises Bin_Split_Error if section not found.
   function Get_Bfd_Section
     (Descriptor   : Bfd.Files.File_Type;
      Section_Name : String)
      return Bfd.Sections.Section;

end Bin_Split;
