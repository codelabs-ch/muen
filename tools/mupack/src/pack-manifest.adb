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
with Ada.Strings.Fixed;

with Mutools.Utils;

package body Pack.Manifest
is

   use Ada.Strings.Unbounded;

   Manifest_Header : constant String
     := "[Name;PhysicalAddress;Offset;MemorySize;ContentSize;Usage;"
     & "Type;Content]";

   -------------------------------------------------------------------------

   procedure Add_Entry
     (Manifest     : in out Manifest_Type;
      Mem_Name     :        String;
      Mem_Type     :        String;
      Content      :        String;
      Address      :        Interfaces.Unsigned_64;
      Memory_Size  :        Interfaces.Unsigned_64;
      Content_Size :        Interfaces.Unsigned_64;
      Offset       :        Interfaces.Unsigned_64)
   is

      --  Usage in percent, always rounded up.
      Usage : constant Interfaces.Unsigned_64
        := ((Content_Size * 100) + (Memory_Size - 1)) / Memory_Size;
   begin
      Manifest.Data.Insert
        (New_Item => Entry_Type'
           (Mem_Name     => To_Unbounded_String (Mem_Name),
            Mem_Type     => To_Unbounded_String (Mem_Type),
            Content      => To_Unbounded_String (Content),
            Address      => Address,
            Memory_Size  => Memory_Size,
            Content_Size => Content_Size,
            Offset       => Offset,
            Usage        => Usage));
   end Add_Entry;

   -------------------------------------------------------------------------

   procedure Write
     (Manifest : Manifest_Type;
      Filename : String)
   is
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (File => File,
                          Mode => Ada.Text_IO.Out_File,
                          Name => Filename);
      Ada.Text_IO.Put_Line (File => File,
                            Item => Manifest_Header);

      for E of Manifest.Data loop
         Ada.Text_IO.Put_Line
           (File => File,
            Item => To_String (E.Mem_Name & ";"
              & Mutools.Utils.To_Hex (Number => E.Address) & ";"
              & Mutools.Utils.To_Hex (Number => E.Offset) & ";"
              & Mutools.Utils.To_Hex (Number => E.Memory_Size) & ";"
              & Mutools.Utils.To_Hex (Number => E.Content_Size) & ";"
              & Ada.Strings.Fixed.Trim
                (Source => E.Usage'Img,
                 Side   => Ada.Strings.Left) & "%;"
              & E.Mem_Type & ";"
              & E.Content));
      end loop;

      Ada.Text_IO.Close (File => File);
   end Write;

end Pack.Manifest;
