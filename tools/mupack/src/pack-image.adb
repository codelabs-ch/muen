--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Mutools.Utils;

with Pack.OS;

package body Pack.Image
is

   Objcopy : constant String := "/usr/bin/objcopy";

   -------------------------------------------------------------------------

   procedure Add_Section
     (Image    : String;
      Filename : String;
      Name     : String;
      Address  : Interfaces.Unsigned_64)
   is
   begin
      OS.Execute
        (Command => Objcopy & " --add-section ." & Name & "=" & Filename & " "
         & Image & " --change-section-address ." & Name & "=0x"
         & Mutools.Utils.To_Hex (Number => Address,
                                 Prefix => False)
         & " --set-section-flags ."
         & Name & "=alloc 2>/dev/null");
   end Add_Section;

   -------------------------------------------------------------------------

   procedure To_Binary
     (Src_Elf : String;
      Dst_Bin : String)
   is
   begin
      OS.Execute (Command => Objcopy & " -O binary --set-section-flags "
                  & ".bss=alloc,load,contents " & Src_Elf & " " & Dst_Bin);
   end To_Binary;

   -------------------------------------------------------------------------

   procedure Write
     (Image    : Image_Type;
      Filename : String)
   is
      File : Ada.Streams.Stream_IO.File_Type;
   begin
      Mutools.Files.Open (Filename => Filename,
                          File     => File);
      Ada.Streams.Stream_IO.Write (File => File,
                                   Item => Image.Data);
      Ada.Streams.Stream_IO.Close (File => File);
   end Write;

end Pack.Image;
