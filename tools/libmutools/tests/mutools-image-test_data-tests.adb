--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Image.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Mutools.Image.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Add_File (Gnattest_T : in out Test);
   procedure Test_Add_File_ceb1f8 (Gnattest_T : in out Test) renames Test_Add_File;
--  id:2.2/ceb1f8e14e3cc341/Add_File/1/0/
   procedure Test_Add_File (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Add_File_To_Image
      is
         use type Interfaces.Unsigned_64;

         Img   : Image_Type (End_Address => 16#2d#);
         Fname : constant String := "obj/add_file.img";
         Size  : constant Interfaces.Unsigned_64 := 16#001e#;

         Add   : Interfaces.Unsigned_64;
      begin
         Add_File (Image   => Img,
                   Path    => "data/pattern",
                   Address => 16#0010#,
                   Size    => Size,
                   Offset  => 0,
                   Added   => Add);
         Write (Image    => Img,
                Filename => Fname);
         Assert (Condition => Add = Size,
                 Message   => "Added bytes and size mismatch:"
                 & Add'Img & " /=" & Size'Img);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Fname,
                  Filename2 => "data/add_data.img"),
                 Message   => "Image mismatch");
         Ada.Directories.Delete_File (Name => Fname);
      end Add_File_To_Image;

      ----------------------------------------------------------------------

      procedure Add_File_To_Image_Error
      is
         Img   : Image_Type (End_Address => 16#2d#);
         Dummy : Interfaces.Unsigned_64;
      begin
         Add_File (Image   => Img,
                   Path    => "data/pattern",
                   Address => 16#0010#,
                   Size    => 16#0030#,
                   Offset  => 0,
                   Added   => Dummy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Image.Image_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Unable to add content of file 'data/pattern' with size "
                    & "16#0030# bytes at address 16#0010# to system image with"
                    & " size 16#002e#",
                    Message   => "Exception mismatch");
      end Add_File_To_Image_Error;

      ----------------------------------------------------------------------

      procedure Add_File_To_Image_Offset
      is
         use type Interfaces.Unsigned_64;

         Img   : Image_Type (End_Address => 16#a#);
         Fname : constant String := "obj/add_file_offset.img";
         Size  : constant Interfaces.Unsigned_64 := 16#0a#;
         Add   : Interfaces.Unsigned_64;
      begin
         Add_File (Image   => Img,
                   Path    => "data/pattern",
                   Address => 0,
                   Size    => Size,
                   Offset  => 16#02#,
                   Added   => Add);
         Write (Image    => Img,
                Filename => Fname);
         Assert (Condition => Add = Size,
                 Message   => "Added bytes and size mismatch:"
                 & Add'Img & " /=" & Size'Img);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Fname,
                  Filename2 => "data/add_file_offset.img"),
                 Message   => "Image mismatch");
         Ada.Directories.Delete_File (Name => Fname);
      end Add_File_To_Image_Offset;
   begin
      Add_File_To_Image;
      Add_File_To_Image_Offset;
      Add_File_To_Image_Error;
--  begin read only
   end Test_Add_File;
--  end read only


--  begin read only
   procedure Test_Add_Pattern (Gnattest_T : in out Test);
   procedure Test_Add_Pattern_e88c40 (Gnattest_T : in out Test) renames Test_Add_Pattern;
--  id:2.2/e88c400cccfdc2ae/Add_Pattern/1/0/
   procedure Test_Add_Pattern (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Add_Pattern_To_Image
      is
         use type Ada.Streams.Stream_Element_Array;

         Ref_Buf : constant Ada.Streams.Stream_Element_Array (0 .. 9)
           := (0 .. 2 => 16#30#,
               3 .. 8 => 16#12#,
               others => 0);
         Img : Image_Type (End_Address => Ref_Buf'Length);
      begin
         Add_Pattern (Image   => Img,
                      Pattern => 16#30#,
                      Size    => 3,
                      Address => 0);
         Add_Pattern (Image   => Img,
                      Pattern => 16#12#,
                      Size    => 6,
                      Address => 3);
         Assert (Condition => Ref_Buf = Get_Buffer
                 (Image   => Img,
                  Address => 0,
                  Size    => 10),
                 Message   => "Image mismatch");
      end Add_Pattern_To_Image;

      ----------------------------------------------------------------------

      procedure Add_Pattern_To_Image_Error
      is
         Img : Image.Image_Type (End_Address => 16#10#);
      begin
         Add_Pattern (Image   => Img,
                      Pattern => 16#30#,
                      Size    => 255,
                      Address => 0);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Image.Image_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Unable to add pattern 16#30# of 16#00ff# bytes at "
                    & "address 16#0000# to system image with size 16#0011#",
                    Message   => "Exception mismatch");
      end Add_Pattern_To_Image_Error;
   begin
      Add_Pattern_To_Image;
      Add_Pattern_To_Image_Error;
--  begin read only
   end Test_Add_Pattern;
--  end read only


--  begin read only
   procedure Test_Get_Buffer (Gnattest_T : in out Test);
   procedure Test_Get_Buffer_1ca9e7 (Gnattest_T : in out Test) renames Test_Get_Buffer;
--  id:2.2/1ca9e7ea86ae2644/Get_Buffer/1/0/
   procedure Test_Get_Buffer (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Streams.Stream_Element_Array;

      Img        : Image_Type (End_Address => 16#2d#);
      Ref_Buffer : constant Ada.Streams.Stream_Element_Array (1 .. 4)
        := (others => 22);
   begin
      Add_Pattern (Image   => Img,
                   Pattern => 16#16#,
                   Size    => 4,
                   Address => 16#10#);
      Assert (Condition => Get_Buffer
              (Image   => Img,
               Address => 16#10#,
               Size    => 4) = Ref_Buffer,
              Message   => "Buffer mismatch");

      begin
         declare
            Unreferenced : Ada.Streams.Stream_Element_Array
              := Get_Buffer (Image   => Img,
                             Address => 16#2d#,
                             Size    => 12);
         begin
            Assert (Condition => False,
                    Message   => "Exception expected");
         end;

      exception
         when E : Image.Image_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Unable to return image data at address 16#002d# with"
                    & " size 16#000c# (image end address is 16#002d#)",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Get_Buffer;
--  end read only


--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_3e3fc4 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/3e3fc46e9015a480/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Img : Image_Type (End_Address => 12);
   begin
      Write (Image    => Img,
             Filename => "file");

   exception
      when Write_Error => null;
--  begin read only
   end Test_Write;
--  end read only


--  begin read only
   procedure Test_Initialize (Gnattest_T : in out Test);
   procedure Test_Initialize_a0814a (Gnattest_T : in out Test) renames Test_Initialize;
--  id:2.2/a0814a40b9f21c19/Initialize/1/0/
   procedure Test_Initialize (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Streams.Stream_Element_Array;

      Img : Image_Type (End_Address => 12);
      Ref : constant Ada.Streams.Stream_Element_Array (0 .. 12)
        := (others => 0);
   begin
      Assert (Condition => Img.Data /= null,
              Message   => "Image data access invalid");

      Assert (Condition => Img.Data.all = Ref,
              Message   => "Image data not initialized to zero");
--  begin read only
   end Test_Initialize;
--  end read only


--  begin read only
   procedure Test_Finalize (Gnattest_T : in out Test);
   procedure Test_Finalize_bc5f62 (Gnattest_T : in out Test) renames Test_Finalize;
--  id:2.2/bc5f62846c8aef4a/Finalize/1/0/
   procedure Test_Finalize (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Img : Image_Type (End_Address => 12);
   begin
      Img.Finalize;
      Assert (Condition => Img.Data = null,
              Message   => "Image data not deallocated");
--  begin read only
   end Test_Finalize;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Mutools.Image.Test_Data.Tests;
