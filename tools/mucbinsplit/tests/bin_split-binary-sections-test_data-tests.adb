--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Bin_Split.Binary.Sections.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Bin_Split.Binary.Sections.Test_Data.Tests is


--  begin read only
   procedure Test_Get_Section (Gnattest_T : in out Test);
   procedure Test_Get_Section_f831a5 (Gnattest_T : in out Test) renames Test_Get_Section;
--  id:2.2/f831a5c3e9cfd00b/Get_Section/1/0/
   procedure Test_Get_Section (Gnattest_T : in out Test) is
   --  bin_split-binary-sections.ads:38:4:Get_Section
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Positive_Test
      is

         Fd : Bin_Split.Binary.Files.File_Type;
         S  : Section;

      begin

         Bin_Split.Binary.Files.Open (Filename   => "test_data/test_binary",
                            Descriptor => Fd);

         S := Get_Section (Descriptor => Fd, Section_Name => ".text");

         Assert (Condition => Get_Name (S) = ".text",
                 Message   => "Section not found");

      exception
         when others =>
            Bfd.Files.Close (File => Bfd.Files.File_Type (Fd));
            raise;
      end Positive_Test;

      -----------------------------------------------------------------------

      procedure Negative_Test
      is

         Fd : Bin_Split.Binary.Files.File_Type;
         S  : Section;

      begin

         Bin_Split.Binary.Files.Open (Filename   => "test_data/test_binary",
                            Descriptor => Fd);

         S := Get_Section (Descriptor => Fd, Section_Name => ".deadbeef");

         Bfd.Files.Close (File => Bfd.Files.File_Type (Fd));

         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Bin_Split_Error =>
            Bfd.Files.Close (File => Bfd.Files.File_Type (Fd));
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Section '.deadbeef' not found",
                    Message   => "Exception mismatch");
         when others =>
            Bfd.Files.Close (File => Bfd.Files.File_Type (Fd));
            raise;
      end Negative_Test;

   begin

      Positive_Test;
      Negative_Test;

--  begin read only
   end Test_Get_Section;
--  end read only


--  begin read only
   procedure Test_Get_Vma (Gnattest_T : in out Test);
   procedure Test_Get_Vma_0a69c7 (Gnattest_T : in out Test) renames Test_Get_Vma;
--  id:2.2/0a69c76be5fe21eb/Get_Vma/1/0/
   procedure Test_Get_Vma (Gnattest_T : in out Test) is
   --  bin_split-binary-sections.ads:43:4:Get_Vma
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      function Address_To_Iterator is
        new Ada.Unchecked_Conversion (Source => System.Address,
                                      Target => Bfd.Sections.Section_Iterator);


      Sec_Name : constant Interfaces.C.Strings.chars_ptr
        := Interfaces.C.Strings.New_String (".deadbeef");

      S : constant Section
        := (Vma    => 16#5000#,
            Lma    => 16#5000#,
            Size   => 16#62f1#,
            Flags  => Bfd.Sections.SEC_CODE,
            Opaque => Address_To_Iterator (Sec_Name'Address));

   begin

      Assert (Condition => Get_Vma (S) = 16#5000#,
              Message => "Wrong VMA extracted");

--  begin read only
   end Test_Get_Vma;
--  end read only


--  begin read only
   procedure Test_Get_Lma (Gnattest_T : in out Test);
   procedure Test_Get_Lma_631e53 (Gnattest_T : in out Test) renames Test_Get_Lma;
--  id:2.2/631e53f309b8659b/Get_Lma/1/0/
   procedure Test_Get_Lma (Gnattest_T : in out Test) is
   --  bin_split-binary-sections.ads:44:4:Get_Lma
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      function Address_To_Iterator is
        new Ada.Unchecked_Conversion (Source => System.Address,
                                      Target => Bfd.Sections.Section_Iterator);

      Sec_Name : constant Interfaces.C.Strings.chars_ptr
        := Interfaces.C.Strings.New_String (".deadbeef");

      S : constant Section
        := (Vma    => 16#5000#,
            Lma    => 16#5000#,
            Size   => 16#62f1#,
            Flags  => Bfd.Sections.SEC_CODE,
            Opaque => Address_To_Iterator (Sec_Name'Address));

   begin

      Assert (Condition => Get_Lma (S) = 16#5000#,
              Message => "Wrong LMA extracted");

--  begin read only
   end Test_Get_Lma;
--  end read only


--  begin read only
   procedure Test_Get_Size (Gnattest_T : in out Test);
   procedure Test_Get_Size_5437e0 (Gnattest_T : in out Test) renames Test_Get_Size;
--  id:2.2/5437e0fb50f41fb4/Get_Size/1/0/
   procedure Test_Get_Size (Gnattest_T : in out Test) is
   --  bin_split-binary-sections.ads:45:4:Get_Size
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      function Address_To_Iterator is
        new Ada.Unchecked_Conversion (Source => System.Address,
                                      Target => Bfd.Sections.Section_Iterator);

      Sec_Name : constant Interfaces.C.Strings.chars_ptr
        := Interfaces.C.Strings.New_String (".deadbeef");

      S : constant Section
        := (Vma    => 16#5000#,
            Lma    => 16#5000#,
            Size   => 16#62f1#,
            Flags  => Bfd.Sections.SEC_CODE,
            Opaque => Address_To_Iterator (Sec_Name'Address));

   begin

      Assert (Condition => Get_Size (S) = 16#62f1#,
              Message => "Wrong section size extracted");

--  begin read only
   end Test_Get_Size;
--  end read only


--  begin read only
   procedure Test_Get_Flags (Gnattest_T : in out Test);
   procedure Test_Get_Flags_d41272 (Gnattest_T : in out Test) renames Test_Get_Flags;
--  id:2.2/d41272664a3a6cdc/Get_Flags/1/0/
   procedure Test_Get_Flags (Gnattest_T : in out Test) is
   --  bin_split-binary-sections.ads:46:4:Get_Flags
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      function Address_To_Iterator is
        new Ada.Unchecked_Conversion (Source => System.Address,
                                      Target => Bfd.Sections.Section_Iterator);

      Sec_Name : constant Interfaces.C.Strings.chars_ptr
        := Interfaces.C.Strings.New_String (".deadbeef");

      S : constant Section
        := (Vma    => 16#5000#,
            Lma    => 16#5000#,
            Size   => 16#62f1#,
            Flags  => Bfd.Sections.SEC_CODE,
            Opaque => Address_To_Iterator (Sec_Name'Address));

   begin

      Assert (Condition => Get_Flags (S)
                = Bin_Split.Binary.Section_Flags (Bfd.Sections.SEC_CODE),
              Message   => "Wrong section flags extracted");

--  begin read only
   end Test_Get_Flags;
--  end read only


--  begin read only
   procedure Test_Get_Name (Gnattest_T : in out Test);
   procedure Test_Get_Name_8d97a9 (Gnattest_T : in out Test) renames Test_Get_Name;
--  id:2.2/8d97a99abc51a313/Get_Name/1/0/
   procedure Test_Get_Name (Gnattest_T : in out Test) is
   --  bin_split-binary-sections.ads:47:4:Get_Name
--  end read only

      pragma Unreferenced (Gnattest_T);

      Fd : Bin_Split.Binary.Files.File_Type;
      S  : Section;

   begin

      Bin_Split.Binary.Files.Open (Filename   => "test_data/test_binary",
                         Descriptor => Fd);

      S := Element (Get_Sections (File => Fd));

      Assert (Condition => Get_Name (S) = ".stack",
              Message   => "Wrong section name");

--  begin read only
   end Test_Get_Name;
--  end read only


--  begin read only
   procedure Test_Get_Sections (Gnattest_T : in out Test);
   procedure Test_Get_Sections_fc718f (Gnattest_T : in out Test) renames Test_Get_Sections;
--  id:2.2/fc718ff4733cbbd7/Get_Sections/1/0/
   procedure Test_Get_Sections (Gnattest_T : in out Test) is
   --  bin_split-binary-sections.ads:57:4:Get_Sections
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type System.Address;

      function Iterator_To_Address is
        new Ada.Unchecked_Conversion (Source => Section_Iterator,
                                      Target => System.Address);

      Fd : Bin_Split.Binary.Files.File_Type;
      It : Section_Iterator;

   begin

      Bin_Split.Binary.Files.Open
        (Filename   => "test_data/test_binary",
         Descriptor => Fd);

      It := Get_Sections (File => Fd);

      Assert (Condition => Iterator_To_Address (It) /= System.Null_Address,
              Message   => "Iterator is NULL");

--  begin read only
   end Test_Get_Sections;
--  end read only


--  begin read only
   procedure Test_Has_Element (Gnattest_T : in out Test);
   procedure Test_Has_Element_109a6c (Gnattest_T : in out Test) renames Test_Has_Element;
--  id:2.2/109a6cf6ff07afb8/Has_Element/1/0/
   procedure Test_Has_Element (Gnattest_T : in out Test) is
   --  bin_split-binary-sections.ads:62:4:Has_Element
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test_First
      is
         Fd : Bin_Split.Binary.Files.File_Type;
         It : Section_Iterator;

      begin

         Bin_Split.Binary.Files.Open (Filename   => "test_data/test_binary",
                            Descriptor => Fd);

         It := Get_Sections (File => Fd);

         Assert (Condition => Has_Element (It),
                 Message   => "Should have element");
      end Test_First;

      procedure Test_Last
      is
         Fd : Bin_Split.Binary.Files.File_Type;
         It : Section_Iterator;

      begin

         Bin_Split.Binary.Files.Open (Filename   => "test_data/test_binary",
                            Descriptor => Fd);

         It := Get_Sections (File => Fd);

         for I in 1 .. 10 loop
            Next (It);
         end loop;

         Assert (Condition => not Has_Element (It),
                 Message   => "Should not have element");
      end Test_Last;

   begin

      Test_First;
      Test_Last;

--  begin read only
   end Test_Has_Element;
--  end read only


--  begin read only
   procedure Test_Next (Gnattest_T : in out Test);
   procedure Test_Next_1e5023 (Gnattest_T : in out Test) renames Test_Next;
--  id:2.2/1e5023029b9af76f/Next/1/0/
   procedure Test_Next (Gnattest_T : in out Test) is
   --  bin_split-binary-sections.ads:65:4:Next
--  end read only

      pragma Unreferenced (Gnattest_T);

      Fd : Bin_Split.Binary.Files.File_Type;
      It : Section_Iterator;

   begin

      Bin_Split.Binary.Files.Open (Filename   => "test_data/test_binary",
                         Descriptor => Fd);

      It := Get_Sections (File => Fd);
      Next(It);

      Assert (Condition => Has_Element (It),
              Message   => "Should have element");

--  begin read only
   end Test_Next;
--  end read only


--  begin read only
   procedure Test_Element (Gnattest_T : in out Test);
   procedure Test_Element_8c6dd3 (Gnattest_T : in out Test) renames Test_Element;
--  id:2.2/8c6dd3c490381f8e/Element/1/0/
   procedure Test_Element (Gnattest_T : in out Test) is
   --  bin_split-binary-sections.ads:68:4:Element
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Positive
      is

         Fd : Bin_Split.Binary.Files.File_Type;
         It : Section_Iterator;

      begin

         Bin_Split.Binary.Files.Open (Filename   => "test_data/test_binary",
                                      Descriptor => Fd);

         It := Get_Sections (File => Fd);
         Next(It);

         Assert (Condition => Get_Name (Element (It)) = ".text",
                 Message   => "Wrong element");

      end Positive;

      procedure Negative
      is

         Fd : Bin_Split.Binary.Files.File_Type;
         It : Section_Iterator;
         S  : Section;

      begin

         Bin_Split.Binary.Files.Open (Filename   => "test_data/test_binary",
                                      Descriptor => Fd);

         It := Get_Sections (File => Fd);

         --  Move iterator past end of sections
         for I in 1 .. 11 loop
            Next (It);
         end loop;

         S := Element (It);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Bin_Split_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Section_Iterator has no element",
                    Message   => "Exception mismatch");
         when others =>
            raise;

      end Negative;

   begin

      Positive;
      Negative;

--  begin read only
   end Test_Element;
--  end read only


--  begin read only
   procedure Test_Get_Section_Contents (Gnattest_T : in out Test);
   procedure Test_Get_Section_Contents_c08ab3 (Gnattest_T : in out Test) renames Test_Get_Section_Contents;
--  id:2.2/c08ab3a0acbe717c/Get_Section_Contents/1/0/
   procedure Test_Get_Section_Contents (Gnattest_T : in out Test) is
   --  bin_split-binary-sections.ads:72:4:Get_Section_Contents
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Streams.Stream_Element_Array;

      procedure Positive
      is

         Fd       : Bin_Split.Binary.Files.File_Type;
         Buf      : Ada.Streams.Stream_Element_Array (1 .. 4);
         Last     : Ada.Streams.Stream_Element_Offset;
         It       : Section_Iterator;
         Sec      : Section;
         Expected : Ada.Streams.Stream_Element_Array
           := (16#e9#, 16#a1#, 16#3#, 16#0#);
      begin

         Bin_Split.Binary.Files.Open (Filename   => "test_data/test_binary",
                                      Descriptor => Fd);

         It := Get_Sections (File => Fd);
         Next (It);
         Sec := Element (It);

         Get_Section_Contents
           (File => Fd,
            S    => Sec,
            Item => Buf,
            Last => Last);

         --  Ada.Text_IO.Put_Line (Buf (1)'Img);
         --  Ada.Text_IO.Put_Line (Buf (2)'Img);
         --  Ada.Text_IO.Put_Line (Buf (3)'Img);
         --  Ada.Text_IO.Put_Line (Buf (4)'Img);

         Assert (Condition => Buf = Expected,
                 Message   => "Unexpected section beginning");

      end Positive;

   begin
      Positive;

--  begin read only
   end Test_Get_Section_Contents;
--  end read only

end Bin_Split.Binary.Sections.Test_Data.Tests;
