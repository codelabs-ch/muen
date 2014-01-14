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

with Ada.Directories;

with Mulog;
with Mutools.Utils;

with Pack.Image;
with Pack.Parser;
with Pack.Command_Line;
with Pack.File_Transforms;

package body Pack
is

   --  Output file entry information.
   procedure Print_Layout (Files : Parser.File_Array);

   -------------------------------------------------------------------------

   procedure Print_Layout (Files : Parser.File_Array)
   is
      --  Add count whitespaces to given string and return result.
      function Indent
        (Count : Positive;
         Str   : String)
         return String;

      ----------------------------------------------------------------------

      function Indent
        (Count : Positive;
         Str   : String)
         return String
      is
         Result : String (1 .. Count + Str'Length) := (others => ' ');
      begin
         Result (Count + 1 .. Result'Length) := Str;
         return Result;
      end Indent;
   begin
      Mulog.Log (Msg => "PHYSICAL          TYPE       PATH");
      for I in Files'Range loop
         declare
            Fmt  : constant String := Files (I).Format'Img;
            Addr : constant String := Mutools.Utils.To_Hex
              (Number => Files (I).Address);
         begin
            Mulog.Log (Msg => Addr
                       & Indent
                         (Count => (16 - Addr'Length) + 2,
                          Str   => Files (I).Format'Img)
                       & Indent
                         (Count => 11 - Fmt'Length,
                          Str   => S (Files (I).Path)));
         end;
      end loop;
   end Print_Layout;

   -------------------------------------------------------------------------

   procedure Run
   is
      Out_Dir     : constant String := Command_Line.Get_Output_Dir;
      In_Dir      : constant String := Command_Line.Get_Input_Dir;
      Policy_File : constant String := Command_Line.Get_Policy;
      Kernel_File : constant String := Command_Line.Get_Kernel_Filename;
   begin
      Mulog.Log (Msg => "Looking for input files in '" & In_Dir & "'");
      Mulog.Log (Msg => "Using output directory '" & Out_Dir & "'");
      Mulog.Log (Msg => "Processing policy '" & Policy_File & "'");

      declare
         Files : Parser.File_Array := Parser.Parse (Policy => Policy_File);
      begin
         Mulog.Log (Msg => "Found" & Files'Length'Img & " file(s) to process");
         File_Transforms.Process (Files => Files);

         Pack_Image :
         declare
            Sysimg  : constant String := Out_Dir & "/" & Kernel_File;
            Knl_Elf : constant String := Sysimg & ".elf";
            Knl_Src : constant String := In_Dir & "/" & Kernel_File;
         begin
            Mulog.Log (Msg => "Creating system image '" & Sysimg & "'");
            Mulog.Log (Msg => "Using '" & Knl_Elf & "' as kernel file");

            if not Ada.Directories.Exists (Name => Knl_Src) then
               raise Pack_Error with "Kernel file '" & Kernel_File
                 & "' not found in input directory '" & In_Dir & "'";
            end if;

            Ada.Directories.Copy_File
              (Source_Name => In_Dir & "/" & Kernel_File,
               Target_Name => Knl_Elf);

            for I in Files'Range loop
               Image.Add_Section
                 (Image    => Knl_Elf,
                  Filename => S (Files (I).Path),
                  Name     => S (Files (I).Name),
                  Address  => Files (I).Address);
            end loop;

            Image.To_Binary (Src_Elf => Knl_Elf,
                             Dst_Bin => Sysimg);
            Mulog.Log (Msg => "Successfully created system image '"
                       & Sysimg & "'");
            Print_Layout (Files => Files);
         end Pack_Image;
      end;
   end Run;

end Pack;
