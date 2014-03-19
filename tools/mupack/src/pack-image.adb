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

with Ada.Streams.Stream_IO;

with Interfaces;

with Mutools.Utils;
with Mutools.Files;

package body Pack.Image
is

   -------------------------------------------------------------------------

   procedure Add_File
     (Image : in out Image_Type;
      File  :        Parser.File_Entry_Type)
   is
      use Ada.Streams;
      use type Interfaces.Unsigned_64;

      Max_Size : constant Stream_Element_Offset
        := Stream_Element_Offset (File.Size);
      Fd       : Stream_IO.File_Type;
      Last     : Stream_Element_Offset;
      Buffer   : Stream_Element_Array (0 .. Max_Size - 1);
   begin
      Stream_IO.Open (File => Fd,
                      Mode => Stream_IO.In_File,
                      Name => S (File.Path) & "/" & S (File.Filename));

      if File.Offset > 0 then

         --  Stream_IO Index starts at 1, our offset at 0 -> add one.

         Stream_IO.Set_Index (File => Fd,
                              To   => Stream_IO.Count (File.Offset + 1));
      end if;

      Stream_IO.Read (File => Fd,
                      Item => Buffer,
                      Last => Last);

      declare
         File_Start : constant Stream_Element_Offset
           := Stream_Element_Offset (File.Address);
         File_End   : constant Stream_Element_Offset
           := Stream_Element_Offset (File.Address) + Last;
      begin
         if File_End > Image.Data'Last then
            Stream_IO.Close (File => Fd);
            raise Pack_Error with "Image end address " & Mutools.Utils.To_Hex
              (Number => Interfaces.Unsigned_64 (Image.Data'Last))
              & " below file end address " & Mutools.Utils.To_Hex
              (Number => Interfaces.Unsigned_64 (File_End));
         end if;

         Image.Data (File_Start .. File_End) := Buffer (Buffer'First .. Last);
      end;

      Stream_IO.Close (File => Fd);
   end Add_File;

   -------------------------------------------------------------------------

   procedure Write
     (Image    : Image_Type;
      Filename : String)
   is
      File : Ada.Streams.Stream_IO.File_Type;

      --  Return offset of first non-zero element. The function can be used to
      --  strip leading zeros. If all elements are zero, an exception is
      --  raised.
      function Get_Non_Zero return Ada.Streams.Stream_Element_Offset;

      ----------------------------------------------------------------------

      function Get_Non_Zero return Ada.Streams.Stream_Element_Offset
      is
         use type Ada.Streams.Stream_Element;
      begin
         for I in Image.Data'Range loop
            if Image.Data (I) > 0 then
               return I;
            end if;
         end loop;

         raise Write_Error with "Unable to write image '" & Filename
           & "' - no valid image data found";
      end Get_Non_Zero;

      Start_Idx : constant Ada.Streams.Stream_Element_Offset := Get_Non_Zero;
   begin
      Mutools.Files.Open
        (Filename => Filename,
         File     => File);
      Ada.Streams.Stream_IO.Write
        (File => File,
         Item => Image.Data (Start_Idx .. Image.Data'Last));
      Ada.Streams.Stream_IO.Close
        (File => File);
   end Write;

end Pack.Image;
