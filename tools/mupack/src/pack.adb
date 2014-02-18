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
with Ada.Text_IO;

with Mulog;
with Mutools.Utils;

with Pack.Image;
with Pack.Parser;
with Pack.Command_Line;
with Pack.File_Transforms;

package body Pack
is

   --  Write memory map to given file.
   procedure Print_Layout
     (File    : String;
      Entries : Parser.File_Array);

   -------------------------------------------------------------------------

   procedure Print_Layout
     (File    : String;
      Entries : Parser.File_Array)
   is
      Fd : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (File => Fd,
                          Mode => Ada.Text_IO.Out_File,
                          Name => File);
      Ada.Text_IO.Put_Line
        (File => Fd,
         Item => "[PhysicalAddress;Offset;Size;Type  Path]");
      for I in Entries'Range loop
         Ada.Text_IO.Set_Col (File => Fd,
                              To   => 1);
         Ada.Text_IO.Put (File => Fd,
                          Item => Mutools.Utils.To_Hex
                            (Number => Entries (I).Address) & ";");
         Ada.Text_IO.Put (File => Fd,
                          Item => Mutools.Utils.To_Hex
                            (Number => Entries (I).Offset) & ";");
         Ada.Text_IO.Put (File => Fd,
                          Item => Mutools.Utils.To_Hex
                            (Number => Entries (I).Size) & ";");
         Ada.Text_IO.Put (File => Fd,
                          Item => Entries (I).Format'Img);
         Ada.Text_IO.Set_Col (File => Fd,
                              To   => 46);
         Ada.Text_IO.Put (File => Fd,
                          Item => S (Entries (I).Path));
         Ada.Text_IO.New_Line (Fd);
      end loop;

      Ada.Text_IO.Close (File => Fd);
   end Print_Layout;

   -------------------------------------------------------------------------

   procedure Run
   is
      Out_Dir     : constant String := Command_Line.Get_Output_Dir;
      In_Dir      : constant String := Command_Line.Get_Input_Dir;
      Policy_File : constant String := Command_Line.Get_Policy;
      Kernel_File : constant String := Command_Line.Get_Kernel_Filename;
      Mmap        : constant String := Out_Dir & "/mmap";
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
            Print_Layout (File    => Mmap,
                          Entries => Files);
            Mulog.Log (Msg => "Memory map written to file '" & Mmap & "'");
         end Pack_Image;
      end;
   end Run;

end Pack;
