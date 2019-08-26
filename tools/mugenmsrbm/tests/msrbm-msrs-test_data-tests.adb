--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Msrbm.MSRs.Test_Data.

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
package body Msrbm.MSRs.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Allow_MSRs (Gnattest_T : in out Test);
   procedure Test_Allow_MSRs_e209a8 (Gnattest_T : in out Test) renames Test_Allow_MSRs;
--  id:2.2/e209a8771f6aa99f/Allow_MSRs/1/0/
   procedure Test_Allow_MSRs (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Streams.Stream_Element_Array;

      B        : MSR_Bitmap_Type := MSRs.Null_MSR_Bitmap;
      Null_Ref : constant MSR_Bitmap_Stream := (others => 16#ff#);
      Full_Ref : constant MSR_Bitmap_Stream := (others => 16#00#);
      RW_Ref   : constant MSR_Bitmap_Stream := (1024   => 16#00#,
                                                2064   => 16#00#,
                                                others => 16#ff#);
   begin
      Assert (Condition => To_Stream (Bitmap => B) = Null_Ref,
              Message   => "Null MSR bitmap mismatch");

      Allow_MSRs (Bitmap     => B,
                  Start_Addr => 0,
                  End_Addr   => 16#1fff#,
                  Mode       => Mutools.Types.RW);
      Allow_MSRs (Bitmap     => B,
                  Start_Addr => 16#c0000000#,
                  End_Addr   => 16#c0001fff#,
                  Mode       => Mutools.Types.RW);
      Assert (Condition => To_Stream (Bitmap => B) = Full_Ref,
              Message   => "Full MSR bitmap mismatch");

      B := Null_MSR_Bitmap;
      Allow_MSRs (Bitmap     => B,
                  Start_Addr => 16#c0000000#,
                  End_Addr   => 16#c0000007#,
                  Mode       => Mutools.Types.R);
      Allow_MSRs (Bitmap     => B,
                  Start_Addr => 16#80#,
                  End_Addr   => 16#87#,
                  Mode       => Mutools.Types.W);
      Assert (Condition => To_Stream (Bitmap => B) = RW_Ref,
              Message   => "RW MSR bitmap mismatch");
--  begin read only
   end Test_Allow_MSRs;
--  end read only


--  begin read only
   procedure Test_To_Stream (Gnattest_T : in out Test);
   procedure Test_To_Stream_a9353e (Gnattest_T : in out Test) renames Test_To_Stream;
--  id:2.2/a9353e0af3662022/To_Stream/1/0/
   procedure Test_To_Stream (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Streams.Stream_Element_Array;

      MSR_Bitmap : MSR_Bitmap_Type   := Null_MSR_Bitmap;
      All_Denied : MSR_Bitmap_Stream := (others => 16#ff#);
   begin
      Assert (Condition => To_Stream (Bitmap => MSR_Bitmap) = All_Denied,
              Message   => "Not all MSRs denied");
--  begin read only
   end Test_To_Stream;
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
end Msrbm.MSRs.Test_Data.Tests;
