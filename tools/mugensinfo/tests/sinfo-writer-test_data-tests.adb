--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Sinfo.Writer.Test_Data.

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
package body Sinfo.Writer.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Serialize (Gnattest_T : in out Test);
   procedure Test_Serialize_d5673b (Gnattest_T : in out Test) renames Test_Serialize;
--  id:2.2/d5673ba8214929bb/Serialize/1/0/
   procedure Test_Serialize (Gnattest_T : in out Test) is
   --  sinfo-writer.ads:26:4:Serialize
--  end read only

      pragma Unreferenced (Gnattest_T);

      Info : Musinfo.Subject_Info_Type := Constants.Null_Subject_Info;
   begin
      Serialize
        (Info     => Info,
         Filename => "obj/null_info");

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/null_info",
               Filename2 => "obj/null_info"),
              Message   => "Null info mismatch");

      Info.Name    := Utils.Create_Name (Str => "subject1");
      Info.TSC_Khz := 2893000;

      Utils.Append_Memregion
        (Info   => Info,
         Name   => Utils.Create_Name (Str => "region1"),
         Region => Utils.Create_Memregion
           (Content    => Musinfo.Content_Uninitialized,
            Address    => 16#0020_0000#,
            Size       => 16#ffee_2000#,
            Writable   => True,
            Executable => True));
      Utils.Append_Channel
        (Info       => Info,
         Name       => Utils.Create_Name (Str => "channel1"),
         Memregion  => Utils.Create_Memregion
           (Content    => Musinfo.Content_Uninitialized,
            Address    => 0,
            Size       => 16#1000#,
            Writable   => False,
            Executable => False),
         Has_Event  => False,
         Has_Vector => False,
         Event      => 0,
         Vector     => 0);
      Utils.Append_Channel
        (Info       => Info,
         Name       => Utils.Create_Name (Str => "channel2"),
         Memregion  => Utils.Create_Memregion
           (Content    => Musinfo.Content_Uninitialized,
            Address    => Interfaces.Unsigned_64'Last,
            Size       => Interfaces.Unsigned_64'Last,
            Writable   => False,
            Executable => False),
         Has_Event  => False,
         Has_Vector => True,
         Event      => 0,
         Vector     => 255);
      Utils.Append_Memregion
        (Info   => Info,
         Name   => Utils.Create_Name (Str => "region2"),
         Region => Utils.Create_Memregion
           (Content    => Musinfo.Content_Uninitialized,
            Address    => 16#bb00_7721_f000#,
            Size       => 16#000e_0000_0000#,
            Writable   => True,
            Executable => False));
      Utils.Append_Channel
        (Info       => Info,
         Name       => Utils.Create_Name (Str => "channel3"),
         Memregion  => Utils.Create_Memregion
           (Content    => Musinfo.Content_Uninitialized,
            Address    => 16#beef_cafe_8080_1111#,
            Size       => 16#dead_beef_cafe_4321#,
            Writable   => True,
            Executable => False),
         Has_Event  => True,
         Has_Vector => False,
         Event      => 1,
         Vector     => 0);

      Serialize
        (Info     => Info,
         Filename => "obj/subject_info");

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/subject_info",
               Filename2 => "obj/subject_info"),
              Message   => "Subject info mismatch");

      Ada.Directories.Delete_File (Name => "obj/null_info");
      Ada.Directories.Delete_File (Name => "obj/subject_info");
--  begin read only
   end Test_Serialize;
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
end Sinfo.Writer.Test_Data.Tests;
