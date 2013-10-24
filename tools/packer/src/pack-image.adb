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

with SK.Utils;

with Pack.OS;
with Ada.Streams.Stream_IO;
use Ada.Streams.Stream_IO;

package body Pack.Image
is

   Objcopy : constant String := "/usr/bin/objcopy";

   -------------------------------------------------------------------------

   procedure Add_Section
     (Image    : String;
      Filename : String;
      Name     : String;
      Address  : SK.Word64)
   is
   begin
      OS.Execute
        (Command => Objcopy & " --add-section ." & Name & "=" & Filename & " "
         & Image & " --change-section-address ." & Name & "=0x"
         & SK.Utils.To_Hex (Item => Address) & " --set-section-flags ."
         & Name & "=alloc 2>/dev/null");
   end Add_Section;

   -------------------------------------------------------------------------

   procedure Parse_Bzimage
     (Src     : String;
      Dst_Bin : String)
   is
      File_In  : Ada.Streams.Stream_IO.File_Type;
      File_Out : Ada.Streams.Stream_IO.File_Type;

      type Sector is array (0 .. 511) of SK.Byte;
      pragma Pack (Sector);
      S : Sector;
      Setup_Sectors : Integer := 4;
   begin
      Open (Name => Src, File => File_In, Mode => In_File);
      Create (Name => Dst_Bin, File => File_Out, Mode => Out_File);
      Sector'Read (Stream (File => File_In), S);
      if Natural (S (497)) > 0 then
         Setup_Sectors := Natural (S (497)) + 1;
      end if;
      for I in Integer range 2 .. Setup_Sectors loop
         Sector'Read (Stream (File => File_In), S);
      end loop;
      while not Ada.Streams.Stream_IO.End_Of_File (File_In) loop
         begin
            Sector'Read (Stream (File => File_In), S);
         exception
            when End_Error => null;
         end;
         Sector'Write (Stream (File => File_Out), S);
      end loop;
      Close (File => File_In);
      Close (File => File_Out);
   end Parse_Bzimage;

   -------------------------------------------------------------------------

   procedure To_Binary
     (Src_Elf : String;
      Dst_Bin : String)
   is
   begin
      OS.Execute (Command => Objcopy & " -O binary --set-section-flags "
                  & ".bss=alloc,load,contents " & Src_Elf & " " & Dst_Bin);
   end To_Binary;

end Pack.Image;
