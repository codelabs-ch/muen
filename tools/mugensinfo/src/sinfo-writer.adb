--
--  Copyright (C) 2014-2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014-2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Streams.Stream_IO;
with Ada.Unchecked_Conversion;

package body Sinfo.Writer
is

   subtype Subject_Info_Stream is Ada.Streams.Stream_Element_Array
     (1 .. Musinfo.Subject_Info_Type_Size);

   --  Subject_Info_Type'Write adds additional output so manual conversion to
   --  a Stream_Element_Array is necessary.
   function Convert is new Ada.Unchecked_Conversion
     (Source => Musinfo.Subject_Info_Type,
      Target => Subject_Info_Stream);

   -------------------------------------------------------------------------

   procedure Serialize
     (Info     : Musinfo.Subject_Info_Type;
      Filename : String)
   is
      File : Ada.Streams.Stream_IO.File_Type;
   begin
      Ada.Streams.Stream_IO.Create
        (File => File,
         Mode => Ada.Streams.Stream_IO.Out_File,
         Name => Filename);
      Ada.Streams.Stream_IO.Write (File => File,
                                   Item => Convert (S => Info));
      Ada.Streams.Stream_IO.Close (File => File);
   end Serialize;

end Sinfo.Writer;
