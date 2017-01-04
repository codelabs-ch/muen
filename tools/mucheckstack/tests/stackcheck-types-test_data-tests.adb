--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Stackcheck.Types.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Stackcheck.Types.Test_Data.Tests is


--  begin read only
   procedure Test_Get_Name (Gnattest_T : in out Test);
   procedure Test_Get_Name_1246db (Gnattest_T : in out Test) renames Test_Get_Name;
--  id:2.2/1246db8c9e62419c/Get_Name/1/0/
   procedure Test_Get_Name (Gnattest_T : in out Test) is
   --  stackcheck-types.ads:29:4:Get_Name
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Ada.Strings.Unbounded;

      Ref_Name : constant String := "sk__cpu__sti";
      Sub      : Subprogram_Type;
   begin
      Sub.Name := To_Unbounded_String (Ref_Name);
      Assert (Condition => Get_Name (Subprogram => Sub) = Ref_Name,
              Message   => "Name mismatch");
--  begin read only
   end Test_Get_Name;
--  end read only


--  begin read only
   procedure Test_Get_Stack_Usage (Gnattest_T : in out Test);
   procedure Test_Get_Stack_Usage_ae398b (Gnattest_T : in out Test) renames Test_Get_Stack_Usage;
--  id:2.2/ae398b0456e2540d/Get_Stack_Usage/1/0/
   procedure Test_Get_Stack_Usage (Gnattest_T : in out Test) is
   --  stackcheck-types.ads:32:4:Get_Stack_Usage
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Usage : constant Natural := 42;
      Sub       : Subprogram_Type;
   begin
      Sub.Own_Stack_Usage := Ref_Usage;
      Assert (Condition => Get_Stack_Usage (Subprogram => Sub) = Ref_Usage,
              Message   => "Stack usage mismatch");
--  begin read only
   end Test_Get_Stack_Usage;
--  end read only


--  begin read only
   procedure Test_Get_Max_Stack_Usage (Gnattest_T : in out Test);
   procedure Test_Get_Max_Stack_Usage_1f4ad5 (Gnattest_T : in out Test) renames Test_Get_Max_Stack_Usage;
--  id:2.2/1f4ad51d300fa87e/Get_Max_Stack_Usage/1/0/
   procedure Test_Get_Max_Stack_Usage (Gnattest_T : in out Test) is
   --  stackcheck-types.ads:36:4:Get_Max_Stack_Usage
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Usage : constant Natural := 16#1000#;
      Sub       : Subprogram_Type;
   begin
      Sub.Max_Stack_Usage := Ref_Usage;
      Assert (Condition => Get_Max_Stack_Usage (Subprogram => Sub) = Ref_Usage,
              Message   => "Max stack usage mismatch");
--  begin read only
   end Test_Get_Max_Stack_Usage;
--  end read only


--  begin read only
   procedure Test_Add_Call (Gnattest_T : in out Test);
   procedure Test_Add_Call_a1b7c6 (Gnattest_T : in out Test) renames Test_Add_Call;
--  id:2.2/a1b7c683ae6ce6da/Add_Call/1/0/
   procedure Test_Add_Call (Gnattest_T : in out Test) is
   --  stackcheck-types.ads:39:4:Add_Call
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Containers.Count_Type;

      Ref_Name : constant String := "sk__cpu__hlt";
      Sub      : Subprogram_Type;
   begin
      Add_Call (Subprogram  => Sub,
                Callee_Name => Ref_Name);
      Assert (Condition => Sub.Calls.Length = 1,
              Message   => "Subprogram call not added");
      Assert (Condition => To_String (Sub.Calls.First_Element) = Ref_Name,
              Message   => "Added call name mismatch");
--  begin read only
   end Test_Add_Call;
--  end read only


--  begin read only
   procedure Test_Get_Call_Count (Gnattest_T : in out Test);
   procedure Test_Get_Call_Count_14636b (Gnattest_T : in out Test) renames Test_Get_Call_Count;
--  id:2.2/14636bd05cca5c71/Get_Call_Count/1/0/
   procedure Test_Get_Call_Count (Gnattest_T : in out Test) is
   --  stackcheck-types.ads:44:4:Get_Call_Count
--  end read only

      pragma Unreferenced (Gnattest_T);

      Sub : Subprogram_Type;
   begin
      Sub.Calls.Clear;
      Assert (Condition => Get_Call_Count (Subprogram => Sub) = 0,
              Message   => "Empty call count not zero");
      Sub.Calls.Append (New_Item => To_Unbounded_String ("foo"),
                        Count    => 5);
      Assert (Condition => Get_Call_Count (Subprogram => Sub) = 5,
              Message   => "Call count mismatch");
--  begin read only
   end Test_Get_Call_Count;
--  end read only

end Stackcheck.Types.Test_Data.Tests;
