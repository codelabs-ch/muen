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

with Ada.Streams;
with Ada.Directories;
with Ada.Exceptions;

with Test_Utils;

with Pack.Image;

package body Image_Tests
is

   use Ahven;
   use Pack;

   -------------------------------------------------------------------------

   procedure Add_Buffer_To_Image
   is
      Img   : Image.Image_Type (End_Address => 16#1d#);
      Fname : constant String := "obj/add_buffer.img";
   begin
      Image.Add_Buffer (Image   => Img,
                        Buffer  => (1 .. 10 => 16#41#),
                        Address => 0);
      Image.Add_Buffer (Image   => Img,
                        Buffer  => (1 .. 10 => 16#42#),
                        Address => 10);
      Image.Add_Buffer (Image   => Img,
                        Buffer  => (1 .. 10 => 16#43#),
                        Address => 20);
      Image.Write (Image    => Img,
                   Filename => Fname);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Fname,
               Filename2 => "data/add_data.img"),
              Message   => "Image mismatch");
      Ada.Directories.Delete_File (Name => Fname);
   end Add_Buffer_To_Image;

   -------------------------------------------------------------------------

   procedure Add_Buffer_To_Image_Small
   is
      Img : Image.Image_Type (End_Address => 16#10#);
   begin
      Image.Add_Buffer (Image   => Img,
                        Buffer  => (1 .. 3 => 0),
                        Address => 16#10#);
      Fail (Message => "Exception expected");

   exception
      when Image.Image_Error => null;
   end Add_Buffer_To_Image_Small;

   -------------------------------------------------------------------------

   procedure Add_File_To_Image
   is
      Img   : Image.Image_Type (End_Address => 16#2d#);
      Fname : constant String := "obj/add_file.img";
   begin
      Image.Add_File (Image   => Img,
                      Path    => "data/pattern",
                      Address => 16#0010#,
                      Size    => 16#0020#,
                      Offset  => 0);
      Image.Write (Image    => Img,
                   Filename => Fname);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Fname,
               Filename2 => "data/add_data.img"),
              Message   => "Image mismatch");
      Ada.Directories.Delete_File (Name => Fname);
   end Add_File_To_Image;

   -------------------------------------------------------------------------

   procedure Add_File_To_Image_Offset
   is
      Img   : Image.Image_Type (End_Address => 16#a#);
      Fname : constant String := "obj/add_file_offset.img";
   begin
      Image.Add_File (Image   => Img,
                      Path    => "data/pattern",
                      Address => 0,
                      Size    => 16#0a#,
                      Offset  => 16#02#);
      Image.Write (Image    => Img,
                   Filename => Fname);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Fname,
               Filename2 => "data/add_file_offset.img"),
              Message   => "Image mismatch");
      Ada.Directories.Delete_File (Name => Fname);
   end Add_File_To_Image_Offset;

   -------------------------------------------------------------------------

   procedure Get_Buffer_From_Image
   is
      use type Ada.Streams.Stream_Element_Array;

      Img        : Image.Image_Type (End_Address => 16#2d#);
      Ref_Buffer : constant Ada.Streams.Stream_Element_Array (1 .. 4)
        := (others => 22);
   begin
      Image.Add_Buffer (Image   => Img,
                        Buffer  => Ref_Buffer,
                        Address => 16#10#);
      Assert (Condition => Image.Get_Buffer
              (Image   => Img,
               Address => 16#10#,
               Size    => 4) = Ref_Buffer,
              Message   => "Buffer mismatch");

      begin
         declare
            Dummy : Ada.Streams.Stream_Element_Array
              := Image.Get_Buffer (Image   => Img,
                                   Address => 16#2d#,
                                   Size    => 12);
            pragma Unreferenced (Dummy);
         begin
            Fail (Message => "Exception expected");
         end;

      exception
         when E : Image.Image_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Unable to return image data at address 16#002d# with"
                    & " size 16#000c# (image end address is 16#002d#)",
                    Message   => "Exception mismatch");
      end;
   end Get_Buffer_From_Image;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Image tests");
      T.Add_Test_Routine
        (Routine => Add_Buffer_To_Image'Access,
         Name    => "Add buffer to system image");
      T.Add_Test_Routine
        (Routine => Add_Buffer_To_Image_Small'Access,
         Name    => "Add buffer to system image (too small)");
      T.Add_Test_Routine
        (Routine => Add_File_To_Image'Access,
         Name    => "Add file to system image");
      T.Add_Test_Routine
        (Routine => Add_File_To_Image_Offset'Access,
         Name    => "Add file to system image (offset)");
      T.Add_Test_Routine
        (Routine => Write_Empty_Image'Access,
         Name    => "Write empty image");
      T.Add_Test_Routine
        (Routine => Get_Buffer_From_Image'Access,
         Name    => "Get buffer from image");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Write_Empty_Image
   is
      Img : Image.Image_Type (End_Address => 12);
   begin
      Image.Write (Image    => Img,
                   Filename => "file");

   exception
      when Image.Write_Error => null;
   end Write_Empty_Image;

end Image_Tests;
