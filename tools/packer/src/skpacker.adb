--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Strings.Fixed;

with SK.Utils;

with Skp.Packer_Config;

with Pack.Image;

procedure Skpacker
is
   use Ada.Strings.Unbounded;
   use Pack;
   use Skp.Packer_Config;

   --  Print file entry.
   procedure Print_File
     (Address : SK.Word64;
      Kind    : File_Kind;
      Path    : String);

   --  Print table header.
   procedure Print_Header;

   --  Print packer usage.
   procedure Print_Usage;

   -------------------------------------------------------------------------

   procedure Print_File
     (Address : SK.Word64;
      Kind    : File_Kind;
      Path    : String)
   is
   begin
      Ada.Text_IO.Put (SK.Utils.To_Hex (Item => Address));
      Ada.Text_IO.Set_Col (To => 19);
      Ada.Text_IO.Put (Kind'Img);
      Ada.Text_IO.Set_Col (To => 31);
      Ada.Text_IO.Put_Line (Path);
   end Print_File;

   -------------------------------------------------------------------------

   procedure Print_Header
   is
   begin
      Ada.Text_IO.Put ("PHYSICAL");
      Ada.Text_IO.Set_Col (To => 19);
      Ada.Text_IO.Put ("TYPE");
      Ada.Text_IO.Set_Col (To => 31);
      Ada.Text_IO.Put_Line ("PATH");
   end Print_Header;

   -------------------------------------------------------------------------

   procedure Print_Usage
   is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Command_Line.Command_Name & " <kernel_elf> <image>");
   end Print_Usage;

   Knl_Elf : constant String := "obj/kernel.elf";
   Top_Dir : constant String := "..";
begin
   if Ada.Command_Line.Argument_Count /= 2 then
      Print_Usage;
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
      return;
   end if;

   Ada.Text_IO.Put_Line ("Packaging kernel image '"
                         & Ada.Command_Line.Argument (2) & "'");

   Ada.Directories.Copy_File
     (Source_Name => Ada.Command_Line.Argument (1),
      Target_Name => Knl_Elf);

   Print_Header;

   for F in Files'Range loop
      declare
         Path : constant String  := To_String (Files (F).Path);
         Fn   : Unbounded_String := To_Unbounded_String (Top_Dir & "/" & Path);
         Name : constant String  := Path
           (Ada.Strings.Fixed.Index
              (Source  => Path,
               Pattern => "/",
               Going   => Ada.Strings.Backward) + 1 .. Path'Last);
         Raw  : constant String  := "obj/" & Name;
      begin
         Print_File (Address => Files (F).Physical_Address,
                     Kind    => Files (F).Kind,
                     Path    => Path);

         if Files (F).Kind = Elfbin then
            Image.To_Binary (Src_Elf => To_String (Fn),
                             Dst_Bin => Raw);
            Fn := To_Unbounded_String (Raw);
         elsif Files (F).Kind = Bzimagebin then
            Image.Parse_Bzimage (Src     => To_String (Fn),
                                 Dst_Bin => Raw);
            Fn := To_Unbounded_String (Raw);
         end if;

         Image.Add_Section
           (Image    => Knl_Elf,
            Filename => To_String (Fn),
            Name     => Name,
            Address  => Files (F).Physical_Address);
      end;
   end loop;

   Image.To_Binary (Src_Elf => Knl_Elf,
                    Dst_Bin => Ada.Command_Line.Argument (2));
end Skpacker;
