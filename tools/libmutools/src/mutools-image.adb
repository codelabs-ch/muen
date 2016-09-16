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
with Ada.Unchecked_Deallocation;

with Mutools.Utils;
with Mutools.Files;

package body Mutools.Image
is

   -------------------------------------------------------------------------

   procedure Add_File
     (Image   : in out Image_Type;
      Path    :        String;
      Address :        Interfaces.Unsigned_64;
      Size    :        Interfaces.Unsigned_64;
      Offset  :        Interfaces.Unsigned_64;
      Added   :    out Interfaces.Unsigned_64)
   is
      use Ada.Streams;

      Fd   : Stream_IO.File_Type;
      Last : Stream_Element_Offset;
   begin
      Stream_IO.Open (File => Fd,
                      Mode => Stream_IO.In_File,
                      Name => Path);

      if Offset > 0 then

         --  Stream_IO Index starts at 1, our offset at 0 -> add one.

         Stream_IO.Set_Index (File => Fd,
                              To   => Stream_IO.Count (Offset + 1));
      end if;

      begin
         Stream_IO.Read
           (File => Fd,
            Item => Image.Data
              (Stream_Element_Offset (Address) .. Stream_Element_Offset
               (Address + (Size - 1))),
            Last => Last);

         Added := Interfaces.Unsigned_64 (Last) + 1 - Address;
      exception
         when Constraint_Error =>
            Stream_IO.Close (File => Fd);
            declare
               Imgsize : constant Interfaces.Unsigned_64
                 := Interfaces.Unsigned_64 (Image.Data'Length);
            begin
               raise Image_Error with "Unable to add content of file '"
                 & Path & "' with "
                 & (if Offset > 0 then "offset " & Mutools.Utils.To_Hex
                    (Number => Offset) & " and " else "")
                 & "size " & Mutools.Utils.To_Hex (Number => Size) & " bytes "
                 & "at address " & Mutools.Utils.To_Hex (Number => Address)
                 & " to system image with size "
                 & Mutools.Utils.To_Hex (Number => Imgsize);
            end;
      end;

      Stream_IO.Close (File => Fd);
   end Add_File;

   -------------------------------------------------------------------------

   procedure Add_Pattern
     (Image   : in out Image_Type;
      Pattern :        Ada.Streams.Stream_Element;
      Size    :        Interfaces.Unsigned_64;
      Address :        Interfaces.Unsigned_64)
   is
      use Ada.Streams;

      subtype Pattern_Range is Stream_Element_Offset range
        Stream_Element_Offset (Address) ..
        Stream_Element_Offset (Address + Size - 1);
   begin
      for I in Pattern_Range loop
         Image.Data (I) := Pattern;
      end loop;

   exception
      when Constraint_Error =>
         declare
            Imgsize : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64 (Image.Data'Length);
         begin
            raise Image_Error with "Unable to add pattern "
              & Mutools.Utils.To_Hex
              (Number     => Interfaces.Unsigned_64 (Pattern),
               Byte_Short => True)
              & " of "
              & Mutools.Utils.To_Hex (Number => Size) & " bytes at address "
              & Mutools.Utils.To_Hex (Number => Address)
              & " to system image with size "
              & Mutools.Utils.To_Hex (Number => Imgsize);
         end;
   end Add_Pattern;

   -------------------------------------------------------------------------

   procedure Finalize (Image : in out Image_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Ada.Streams.Stream_Element_Array,
         Name   => Stream_Access);
   begin
      if Image.Data /= null then
         Free (X => Image.Data);
      end if;
   end Finalize;

   -------------------------------------------------------------------------

   function Get_Buffer
     (Image   : Image_Type;
      Address : Interfaces.Unsigned_64;
      Size    : Interfaces.Unsigned_64)
      return Ada.Streams.Stream_Element_Array
   is
      use Ada.Streams;
   begin
      return Image.Data
        (Stream_Element_Offset
           (Address) .. Stream_Element_Offset (Address + Size - 1));

   exception
      when Constraint_Error =>
         raise Image_Error with "Unable to return image data at address "
           & Mutools.Utils.To_Hex (Number => Address) & " with size "
           & Mutools.Utils.To_Hex (Number => Size) & " (image end address is "
           & Mutools.Utils.To_Hex (Number => Interfaces.Unsigned_64
                                   (Image.End_Address)) & ")";
   end Get_Buffer;

   -------------------------------------------------------------------------

   procedure Initialize (Image : in out Image_Type)
   is
   begin
      Image.Data.all := (others => 0);
   end Initialize;

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

end Mutools.Image;
