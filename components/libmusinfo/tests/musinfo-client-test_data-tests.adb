--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Musinfo.Client.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Musinfo.Client.Test_Data.Tests is


--  begin read only
   procedure Test_Check_Validity (Gnattest_T : in out Test);
   procedure Test_Check_Validity_d561cd (Gnattest_T : in out Test) renames Test_Check_Validity;
--  id:2.2/d561cd1b1a41bad5/Check_Validity/1/0/
   procedure Test_Check_Validity (Gnattest_T : in out Test) is
   --  musinfo-client.ads:35:4:Check_Validity
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
   --  musinfo-client.ads:38:4:Is_Valid
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
   --  musinfo-client.ads:41:4:TSC_Khz
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type TSC_Tick_Rate_Khz_Type;

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
   --  musinfo-client.ads:48:4:TSC_Schedule_Start
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

end Musinfo.Client.Test_Data.Tests;