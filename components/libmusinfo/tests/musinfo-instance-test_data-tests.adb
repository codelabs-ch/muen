--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Musinfo.Instance.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Musinfo.Instance.Test_Data.Tests is


--  begin read only
   procedure Test_Check_Validity (Gnattest_T : in out Test);
   procedure Test_Check_Validity_d561cd (Gnattest_T : in out Test) renames Test_Check_Validity;
--  id:2.2/d561cd1b1a41bad5/Check_Validity/1/0/
   procedure Test_Check_Validity (Gnattest_T : in out Test) is
   --  musinfo-instance.ads:36:4:Check_Validity
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Sinfo.Object.Magic := 123;
      Check_Validity;
      Assert (Condition => not Sinfo_Valid,
              Message   => "Sinfo valid");

      Sinfo.Object.Magic := Muen_Subject_Info_Magic;
      Check_Validity;
      Assert (Condition => Sinfo_Valid,
              Message   => "Sinfo not valid");
--  begin read only
   end Test_Check_Validity;
--  end read only


--  begin read only
   procedure Test_Is_Valid (Gnattest_T : in out Test);
   procedure Test_Is_Valid_f361e9 (Gnattest_T : in out Test) renames Test_Is_Valid;
--  id:2.2/f361e911d85a7ba5/Is_Valid/1/0/
   procedure Test_Is_Valid (Gnattest_T : in out Test) is
   --  musinfo-instance.ads:39:4:Is_Valid
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Sinfo_Valid := False;
      Assert (Condition => not Is_Valid,
              Message   => "Sinfo reported valid");
      Sinfo_Valid := True;
      Assert (Condition => Is_Valid,
              Message   => "Sinfo reported invalid");
--  begin read only
   end Test_Is_Valid;
--  end read only


--  begin read only
   procedure Test_TSC_Khz (Gnattest_T : in out Test);
   procedure Test_TSC_Khz_82b138 (Gnattest_T : in out Test) renames Test_TSC_Khz;
--  id:2.2/82b138ef2388f037/TSC_Khz/1/0/
   procedure Test_TSC_Khz (Gnattest_T : in out Test) is
   --  musinfo-instance.ads:42:4:TSC_Khz
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

   begin
      Sinfo.Object.TSC_Khz := 2000;
      Assert (Condition => TSC_Khz = 2000,
              Message   => "TSC kHz value mismatch");
--  begin read only
   end Test_TSC_Khz;
--  end read only


--  begin read only
   procedure Test_TSC_Schedule_Start (Gnattest_T : in out Test);
   procedure Test_TSC_Schedule_Start_9502d2 (Gnattest_T : in out Test) renames Test_TSC_Schedule_Start;
--  id:2.2/9502d2bebd557fcb/TSC_Schedule_Start/1/0/
   procedure Test_TSC_Schedule_Start (Gnattest_T : in out Test) is
   --  musinfo-instance.ads:48:4:TSC_Schedule_Start
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

   begin
      Sinfo.Object.TSC_Schedule_Start := 12000;
      Assert (Condition => TSC_Schedule_Start = 12000,
              Message   => "TSC schedule start mismatch");
--  begin read only
   end Test_TSC_Schedule_Start;
--  end read only


--  begin read only
   procedure Test_TSC_Schedule_End (Gnattest_T : in out Test);
   procedure Test_TSC_Schedule_End_ef2d6d (Gnattest_T : in out Test) renames Test_TSC_Schedule_End;
--  id:2.2/ef2d6d4a69fbedb1/TSC_Schedule_End/1/0/
   procedure Test_TSC_Schedule_End (Gnattest_T : in out Test) is
   --  musinfo-instance.ads:54:4:TSC_Schedule_End
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

   begin
      Sinfo.Object.TSC_Schedule_End := 13500;
      Assert (Condition => TSC_Schedule_End = 13500,
              Message   => "TSC schedule end mismatch");
--  begin read only
   end Test_TSC_Schedule_End;
--  end read only


--  begin read only
   procedure Test_Memory_By_Name (Gnattest_T : in out Test);
   procedure Test_Memory_By_Name_f61cba (Gnattest_T : in out Test) renames Test_Memory_By_Name;
--  id:2.2/f61cbae1ffb0482f/Memory_By_Name/1/0/
   procedure Test_Memory_By_Name (Gnattest_T : in out Test) is
   --  musinfo-instance.ads:61:4:Memory_By_Name
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Tested in Utils package");
--  begin read only
   end Test_Memory_By_Name;
--  end read only


--  begin read only
   procedure Test_Memory_By_Hash (Gnattest_T : in out Test);
   procedure Test_Memory_By_Hash_8f3474 (Gnattest_T : in out Test) renames Test_Memory_By_Hash;
--  id:2.2/8f3474f004f5af03/Memory_By_Hash/1/0/
   procedure Test_Memory_By_Hash (Gnattest_T : in out Test) is
   --  musinfo-instance.ads:68:4:Memory_By_Hash
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Tested in Utils package");
--  begin read only
   end Test_Memory_By_Hash;
--  end read only

end Musinfo.Instance.Test_Data.Tests;
