--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutime.Utils.Test_Data.

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
package body Mutime.Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_To_BCD (Gnattest_T : in out Test);
   procedure Test_To_BCD_561e95 (Gnattest_T : in out Test) renames Test_To_BCD;
--  id:2.2/561e95eda3cdb2e0/To_BCD/1/0/
   procedure Test_To_BCD (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_8;
   begin
      Assert (Condition => To_BCD (Value => 0) = 0,
              Message   => "BCD mismatch (1)");
      Assert (Condition => To_BCD (Value => 26) = 16#26#,
              Message   => "BCD mismatch (2)");
      Assert (Condition => To_BCD (Value => 99) = 16#99#,
              Message   => "BCD mismatch (3)");
--  begin read only
   end Test_To_BCD;
--  end read only


--  begin read only
   procedure Test_Multiply_Divide (Gnattest_T : in out Test);
   procedure Test_Multiply_Divide_7d4c68 (Gnattest_T : in out Test) renames Test_Multiply_Divide;
--  id:2.2/7d4c68ed999118f1/Multiply_Divide/1/0/
   procedure Test_Multiply_Divide (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Q : Interfaces.Unsigned_64;
   begin
      Multiply_Divide
        (Value      => 12,
         Multiplier => 12,
         Divisor    => 2,
         Quotient   => Q);
      Assert (Condition => Q = 72,
              Message   => "Quotient invalid (1):" & Q'Img);
      Multiply_Divide
        (Value      => 1231231312,
         Multiplier => 18282822,
         Divisor    => 23442,
         Quotient   => Q);
      Assert (Condition => Q = 960258634848,
              Message   => "Quotient invalid (2):" & Q'Img);
      Multiply_Divide
        (Value      => Interfaces.Unsigned_64'Last,
         Multiplier => 2,
         Divisor    => 3,
         Quotient   => Q);
      Assert (Condition => Q = 12297829382473034410,
              Message   => "Quotient invalid (3):" & Q'Img);

      --  Corner case: Quotient = Unsigned_64'Last

      Multiply_Divide
        (Value      => 6148914691236517205,
         Multiplier => 3,
         Divisor    => 1,
         Quotient   => Q);
      Assert (Condition => Q = Interfaces.Unsigned_64'Last,
              Message   => "Quotient invalid (4):" & Q'Img);
--  begin read only
   end Test_Multiply_Divide;
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
end Mutime.Utils.Test_Data.Tests;
