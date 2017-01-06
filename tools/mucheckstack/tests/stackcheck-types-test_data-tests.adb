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
   procedure Test_Create (Gnattest_T : in out Test);
   procedure Test_Create_649a54 (Gnattest_T : in out Test) renames Test_Create;
--  id:2.2/649a54a5f4f87611/Create/1/0/
   procedure Test_Create (Gnattest_T : in out Test) is
   --  stackcheck-types.ads:32:4:Create
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type LOSC.List;

      Ref_Name  : constant String := "Foobar";
      Ref_Usage : constant Natural := 55;
      Sub       : Subprogram_Type;
   begin
      Sub := Create (Name        => Ref_Name,
                     Stack_Usage => Ref_Usage);
      Assert (Condition => To_String (Sub.Name) = Ref_Name,
              Message   => "Name mismatch");
      Assert (Condition => Sub.Own_Stack_Usage = Ref_Usage,
              Message   => "Stack usage mismatch");
      Assert (Condition => Sub.Max_Stack_Usage = Ref_Usage,
              Message   => "Max stack usage mismatch");
      Assert (Condition => Sub.Calls = LOSC.Empty_List,
              Message   => "Calls mismatch");
--  begin read only
   end Test_Create;
--  end read only


--  begin read only
   procedure Test_Get_Name (Gnattest_T : in out Test);
   procedure Test_Get_Name_1246db (Gnattest_T : in out Test) renames Test_Get_Name;
--  id:2.2/1246db8c9e62419c/Get_Name/1/0/
   procedure Test_Get_Name (Gnattest_T : in out Test) is
   --  stackcheck-types.ads:38:4:Get_Name
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
   --  stackcheck-types.ads:41:4:Get_Stack_Usage
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
   --  stackcheck-types.ads:45:4:Get_Max_Stack_Usage
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
   procedure Test_Set_Max_Stack_Usage (Gnattest_T : in out Test);
   procedure Test_Set_Max_Stack_Usage_e177c6 (Gnattest_T : in out Test) renames Test_Set_Max_Stack_Usage;
--  id:2.2/e177c61f3cc7dbf1/Set_Max_Stack_Usage/1/0/
   procedure Test_Set_Max_Stack_Usage (Gnattest_T : in out Test) is
   --  stackcheck-types.ads:48:4:Set_Max_Stack_Usage
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Usage : constant Natural := 453;
      Sub       : Subprogram_Type;
   begin
      Set_Max_Stack_Usage (Subprogram => Sub,
                           Value      => Ref_Usage);
      Assert (Condition => Sub.Max_Stack_Usage = Ref_Usage,
              Message   => "Max stack usage mismatch");
--  begin read only
   end Test_Set_Max_Stack_Usage;
--  end read only


--  begin read only
   procedure Test_Add_Call (Gnattest_T : in out Test);
   procedure Test_Add_Call_a1b7c6 (Gnattest_T : in out Test) renames Test_Add_Call;
--  id:2.2/a1b7c683ae6ce6da/Add_Call/1/0/
   procedure Test_Add_Call (Gnattest_T : in out Test) is
   --  stackcheck-types.ads:53:4:Add_Call
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
   --  stackcheck-types.ads:58:4:Get_Call_Count
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


--  begin read only
   procedure Test_Iterate_Calls (Gnattest_T : in out Test);
   procedure Test_Iterate_Calls_3d8be5 (Gnattest_T : in out Test) renames Test_Iterate_Calls;
--  id:2.2/3d8be55f76392d1e/Iterate_Calls/1/0/
   procedure Test_Iterate_Calls (Gnattest_T : in out Test) is
   --  stackcheck-types.ads:62:4:Iterate_Calls
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Calls : constant array (1 .. 3) of Unbounded_String
        := (To_Unbounded_String ("foo"),
            To_Unbounded_String ("bar"),
            To_Unbounded_String ("foobar"));

      Cur_Idx : Natural := 1;
      Sub     : Subprogram_Type;

      --  Assert that the given callee matches the expected reference name.
      procedure Check_Call (Callee : String);

      ----------------------------------------------------------------------

      procedure Check_Call (Callee : String)
      is
      begin
         Assert
           (Condition => Ref_Calls (Cur_Idx) = To_Unbounded_String (Callee),
            Message   => "Callee" & Cur_Idx'Img & " name mismatch");
         Cur_Idx := Cur_Idx + 1;
      end Check_Call;
   begin
      for C of Ref_Calls loop
         Sub.Calls.Append (New_Item => C);
      end loop;

      Iterate_Calls (Subprogram => Sub,
                     Process    => Check_Call'Access);
--  begin read only
   end Test_Iterate_Calls;
--  end read only


--  begin read only
   procedure Test_Add_Node (Gnattest_T : in out Test);
   procedure Test_Add_Node_e8151f (Gnattest_T : in out Test) renames Test_Add_Node;
--  id:2.2/e8151fee1de82d38/Add_Node/1/0/
   procedure Test_Add_Node (Gnattest_T : in out Test) is
   --  stackcheck-types.ads:71:4:Add_Node
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Containers.Count_Type;

      Cfg : Control_Flow_Graph_Type;
      Sub : constant Subprogram_Type := Create (Name        => "Foobar",
                                                Stack_Usage => 42);
   begin
      Add_Node (Graph      => Cfg,
                Subprogram => Sub);
      Assert (Condition => Cfg.Nodes.Length = 1,
              Message   => "Node not added");
      Assert (Condition => Cfg.Nodes.Element (Key => Sub.Name) = Sub,
              Message   => "Inserted node mismatch");

      begin
         Add_Node (Graph      => Cfg,
                   Subprogram => Sub);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Duplicate_Subprogram =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Node with name 'Foobar' already in graph",
                    Message   => "Exception message mismatch");
      end;
--  begin read only
   end Test_Add_Node;
--  end read only


--  begin read only
   procedure Test_Equal_Name (Gnattest_T : in out Test);
   procedure Test_Equal_Name_85b0c9 (Gnattest_T : in out Test) renames Test_Equal_Name;
--  id:2.2/85b0c9397ba7bfe7/Equal_Name/1/0/
   procedure Test_Equal_Name (Gnattest_T : in out Test) is
   --  stackcheck-types.ads:98:4:Equal_Name
--  end read only

      pragma Unreferenced (Gnattest_T);

      Sub1, Sub2 : Subprogram_Type;
   begin
      Sub1.Name := To_Unbounded_String ("foo");
      Sub2.Name := To_Unbounded_String ("bar");
      Assert (Condition => not Equal_Name (Left  => Sub1,
                                           Right => Sub2),
              Message   => "Names equal");

      Sub2.Name := Sub1.Name;
      Assert (Condition => Equal_Name (Left  => Sub1,
                                       Right => Sub2),
              Message   => "Names not equal");
--  begin read only
   end Test_Equal_Name;
--  end read only

end Stackcheck.Types.Test_Data.Tests;
