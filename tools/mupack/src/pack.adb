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

with Ada.Text_IO;
with Ada.Streams;

with Interfaces;

with Mulog;
with Mutools.Utils;

with Pack.Image;
with Pack.Parser;
with Pack.Command_Line;

package body Pack
is

   --  Write memory map to given file.
   procedure Print_Layout
     (File    : String;
      Entries : Parser.File_Array);

   --  Initialize file paths.
   procedure Set_Initial_Filepath (Files : in out Parser.File_Array);

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
         Item => "[Name;PhysicalAddress;Offset;Size;Type  Path]");
      for I in Entries'Range loop
         Ada.Text_IO.Set_Col (File => Fd,
                              To   => 1);
         Ada.Text_IO.Put (File => Fd,
                          Item => S (Entries (I).Mem_Name) & ";");
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
                              To   => 60);
         Ada.Text_IO.Put
           (File => Fd,
            Item => S (Entries (I).Path) & "/" & S (Entries (I).Filename));
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
      Mmap        : constant String := Out_Dir & "/mmap";
   begin
      Mulog.Log (Msg => "Looking for input files in '" & In_Dir & "'");
      Mulog.Log (Msg => "Using output directory '" & Out_Dir & "'");
      Mulog.Log (Msg => "Processing policy '" & Policy_File & "'");

      declare
         Files : Parser.File_Array := Parser.Parse (Policy => Policy_File);
      begin
         Set_Initial_Filepath (Files => Files);

         Mulog.Log (Msg => "Found" & Files'Length'Img & " file(s) to process");

         Pack_Image :
         declare
            Size    : constant Interfaces.Unsigned_64
              := Parser.Get_Image_Size (Files => Files);
            Img     : Pack.Image.Image_Type
              (End_Address => Ada.Streams.Stream_Element_Offset (Size));
            Sysimg  : constant String := Out_Dir & "/muen.img";
         begin
            Mulog.Log (Msg => "Creating system image '" & Sysimg & "' of size "
                       & Mutools.Utils.To_Hex (Number => Size) & " bytes");

            for I in Files'Range loop
               Image.Add_File (Image => Img,
                               File  => Files (I));
            end loop;

            Image.Write (Image    => Img,
                         Filename => Sysimg);
            Mulog.Log (Msg => "Successfully created system image '"
                       & Sysimg & "'");
            Print_Layout (File    => Mmap,
                          Entries => Files);
            Mulog.Log (Msg => "Memory map written to file '" & Mmap & "'");
         end Pack_Image;
      end;
   end Run;

   -------------------------------------------------------------------------

   procedure Set_Initial_Filepath (Files : in out Parser.File_Array)
   is
   begin
      for I in Files'Range loop
         Files (I).Path := U (Command_Line.Get_Input_Dir);
      end loop;
   end Set_Initial_Filepath;

end Pack;
