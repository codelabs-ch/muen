--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Stackcheck.Types.Test_Data.

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
package body Stackcheck.Types.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Create (Gnattest_T : in out Test);
   procedure Test_Create_649a54 (Gnattest_T : in out Test) renames Test_Create;
--  id:2.2/649a54a5f4f87611/Create/1/0/
   procedure Test_Create (Gnattest_T : in out Test) is
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
   procedure Test_Get_Own_Stack_Usage (Gnattest_T : in out Test);
   procedure Test_Get_Own_Stack_Usage_a6f81c (Gnattest_T : in out Test) renames Test_Get_Own_Stack_Usage;
--  id:2.2/a6f81cdc1ab4cbde/Get_Own_Stack_Usage/1/0/
   procedure Test_Get_Own_Stack_Usage (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Usage : constant Natural := 42;
      Sub       : Subprogram_Type;
   begin
      Sub.Own_Stack_Usage := Ref_Usage;
      Assert (Condition => Get_Own_Stack_Usage (Subprogram => Sub) = Ref_Usage,
              Message   => "Stack usage mismatch");
--  begin read only
   end Test_Get_Own_Stack_Usage;
--  end read only


--  begin read only
   procedure Test_1_Get_Max_Stack_Usage (Gnattest_T : in out Test);
   procedure Test_Get_Max_Stack_Usage_1f4ad5 (Gnattest_T : in out Test) renames Test_1_Get_Max_Stack_Usage;
--  id:2.2/1f4ad51d300fa87e/Get_Max_Stack_Usage/1/0/
   procedure Test_1_Get_Max_Stack_Usage (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Usage : constant Natural := 16#1000#;
      Sub       : Subprogram_Type;
   begin
      Sub.Max_Stack_Usage := Ref_Usage;
      Assert (Condition => Get_Max_Stack_Usage (Subprogram => Sub) = Ref_Usage,
              Message   => "Max stack usage mismatch");
--  begin read only
   end Test_1_Get_Max_Stack_Usage;
--  end read only


--  begin read only
   procedure Test_Set_Max_Stack_Usage (Gnattest_T : in out Test);
   procedure Test_Set_Max_Stack_Usage_e177c6 (Gnattest_T : in out Test) renames Test_Set_Max_Stack_Usage;
--  id:2.2/e177c61f3cc7dbf1/Set_Max_Stack_Usage/1/0/
   procedure Test_Set_Max_Stack_Usage (Gnattest_T : in out Test) is
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
   procedure Test_1_Add_Call (Gnattest_T : in out Test);
   procedure Test_Add_Call_a1b7c6 (Gnattest_T : in out Test) renames Test_1_Add_Call;
--  id:2.2/a1b7c683ae6ce6da/Add_Call/1/0/
   procedure Test_1_Add_Call (Gnattest_T : in out Test) is
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
   end Test_1_Add_Call;
--  end read only


--  begin read only
   procedure Test_Get_Call_Count (Gnattest_T : in out Test);
   procedure Test_Get_Call_Count_14636b (Gnattest_T : in out Test) renames Test_Get_Call_Count;
--  id:2.2/14636bd05cca5c71/Get_Call_Count/1/0/
   procedure Test_Get_Call_Count (Gnattest_T : in out Test) is
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
   procedure Test_Is_Active (Gnattest_T : in out Test);
   procedure Test_Is_Active_3f2f51 (Gnattest_T : in out Test) renames Test_Is_Active;
--  id:2.2/3f2f51c93bb32d4b/Is_Active/1/0/
   procedure Test_Is_Active (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Sub : Subprogram_Type;
   begin
      Sub.Active_Flag := False;
      Assert (Condition => not Is_Active (Node => Sub),
              Message   => "Node is active");

      Sub.Active_Flag := True;
      Assert (Condition => Is_Active (Node => Sub),
              Message   => "Node is not active");
--  begin read only
   end Test_Is_Active;
--  end read only


--  begin read only
   procedure Test_Set_Active (Gnattest_T : in out Test);
   procedure Test_Set_Active_31acca (Gnattest_T : in out Test) renames Test_Set_Active;
--  id:2.2/31accace91bb6116/Set_Active/1/0/
   procedure Test_Set_Active (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Sub : Subprogram_Type;
   begin
      Set_Active (Node  => Sub,
                  State => True);
      Assert (Condition => Sub.Active_Flag,
              Message   => "Active flag is False");

      Set_Active (Node  => Sub,
                  State => False);
      Assert (Condition => not Sub.Active_Flag,
              Message   => "Active flag is True");
--  begin read only
   end Test_Set_Active;
--  end read only


--  begin read only
   procedure Test_Is_Done (Gnattest_T : in out Test);
   procedure Test_Is_Done_e074a1 (Gnattest_T : in out Test) renames Test_Is_Done;
--  id:2.2/e074a165e6bec162/Is_Done/1/0/
   procedure Test_Is_Done (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Sub : Subprogram_Type;
   begin
      Sub.Done_Flag := False;
      Assert (Condition => not Is_Done (Node => Sub),
              Message   => "Node is done");

      Sub.Done_Flag := True;
      Assert (Condition => Is_Done (Node => Sub),
              Message   => "Node is not done");
--  begin read only
   end Test_Is_Done;
--  end read only


--  begin read only
   procedure Test_Set_Done (Gnattest_T : in out Test);
   procedure Test_Set_Done_969bfa (Gnattest_T : in out Test) renames Test_Set_Done;
--  id:2.2/969bfab24aa66da3/Set_Done/1/0/
   procedure Test_Set_Done (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Sub : Subprogram_Type;
   begin
      Set_Done (Node  => Sub,
                State => True);
      Assert (Condition => Sub.Done_Flag,
              Message   => "Done flag is False");

      Set_Done (Node  => Sub,
                State => False);
      Assert (Condition => not Sub.Done_Flag,
              Message   => "Done flag is True");
--  begin read only
   end Test_Set_Done;
--  end read only


--  begin read only
   procedure Test_Add_Node (Gnattest_T : in out Test);
   procedure Test_Add_Node_e8151f (Gnattest_T : in out Test) renames Test_Add_Node;
--  id:2.2/e8151fee1de82d38/Add_Node/1/0/
   procedure Test_Add_Node (Gnattest_T : in out Test) is
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
   procedure Test_Update_Node (Gnattest_T : in out Test);
   procedure Test_Update_Node_6c7c3f (Gnattest_T : in out Test) renames Test_Update_Node;
--  id:2.2/6c7c3f12242211fa/Update_Node/1/0/
   procedure Test_Update_Node (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Use : constant Natural := 153;
      Graph   : Control_Flow_Graph_Type;
      Sub     : Subprogram_Type := Create (Name        => "Foobar",
                                           Stack_Usage => 55);

      --  Set max stack usage of given node to reference value.
      procedure Set (Node : in out Subprogram_Type);

      ----------------------------------------------------------------------

      procedure Set (Node : in out Subprogram_Type)
      is
      begin
         Assert (Condition => Node = Sub,
                 Message   => "Update node parameter mismatch");
         Node.Max_Stack_Usage := Ref_Use;
      end Set;
   begin
      Graph.Nodes.Insert (Key      => Sub.Name,
                          New_Item => Sub);
      Update_Node (Graph   => Graph,
                   Name    => "Foobar",
                   Process => Set'Access);
      Assert (Condition => Graph.Nodes.Element
              (Key => Sub.Name).Max_Stack_Usage = Ref_Use,
              Message   => "Updated node mismatch");

      begin
         Update_Node (Graph   => Graph,
                      Name    => "nonexistent",
                      Process => Set'Access);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Missing_Subprogram =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "No subprogram with name 'nonexistent' in control flow"
                    & " graph",
                    Message   => "Exception message mismatch");
      end;
--  begin read only
   end Test_Update_Node;
--  end read only


--  begin read only
   procedure Test_Iterate (Gnattest_T : in out Test);
   procedure Test_Iterate_a24a0d (Gnattest_T : in out Test) renames Test_Iterate;
--  id:2.2/a24a0d6e322f61f6/Iterate/1/0/
   procedure Test_Iterate (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Nodes : array (1 .. 3) of Subprogram_Type
        := (Create (Name        => "bar",
                    Stack_Usage => 8096),
            Create (Name        => "foobar",
                    Stack_Usage => 0),
            Create (Name        => "foo",
                    Stack_Usage => 75));

      Cur_Idx : Natural := 1;
      Graph   : Control_Flow_Graph_Type;

      --  Assert that the given node matches the expected reference subprogram.
      procedure Check_Node (Node : in out Subprogram_Type);

      ----------------------------------------------------------------------

      procedure Check_Node (Node : in out Subprogram_Type)
      is
      begin
         Assert
           (Condition => Ref_Nodes (Cur_Idx) = Node,
            Message   => "Node" & Cur_Idx'Img & " mismatch");
         Cur_Idx := Cur_Idx + 1;
      end Check_Node;
   begin
      Iterate (Graph   => Graph,
               Process => Check_Node'Access);
      Assert (Condition => Cur_Idx = 1,
              Message   => "Iterated empty graph");

      for Node of Ref_Nodes loop
         Graph.Nodes.Insert (Key      => Node.Name,
                             New_Item => Node);
      end loop;

      Iterate (Graph   => Graph,
               Process => Check_Node'Access);
--  begin read only
   end Test_Iterate;
--  end read only


--  begin read only
   procedure Test_2_Add_Call (Gnattest_T : in out Test);
   procedure Test_Add_Call_32d494 (Gnattest_T : in out Test) renames Test_2_Add_Call;
--  id:2.2/32d494f093650e37/Add_Call/0/0/
   procedure Test_2_Add_Call (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Graph : Control_Flow_Graph_Type;
   begin
      Graph.Nodes.Insert (Key      => To_Unbounded_String ("foobar"),
                          New_Item => Create (Name        => "foobar",
                                              Stack_Usage => 27345));

      Add_Call (Graph       => Graph,
                Source_Name => "foobar",
                Target_Name => "foo");
      Add_Call (Graph       => Graph,
                Source_Name => "foobar",
                Target_Name => "bar");

      declare
         use type Ada.Containers.Count_Type;

         Sub : constant Subprogram_Type
           := Graph.Nodes.Element (Key => To_Unbounded_String ("foobar"));
      begin
         Assert (Condition => Sub.Calls.Length = 2,
                 Message   => "Added call count mismatch");
         Assert (Condition => Sub.Calls.First_Element
                 = To_Unbounded_String ("foo"),
                 Message   => "Added call mismatch (1)");
         Assert (Condition => Sub.Calls.Last_Element
                 = To_Unbounded_String ("bar"),
                 Message   => "Added call mismatch (2)");
      end;

      begin
         Add_Call (Graph       => Graph,
                   Source_Name => "nonexistent",
                   Target_Name => "foo");

      exception
         when E : Missing_Subprogram =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "No subprogram with name 'nonexistent' in control flow"
                    & " graph",
                    Message   => "Exception message mismatch");
      end;
--  begin read only
   end Test_2_Add_Call;
--  end read only


--  begin read only
   procedure Test_2_Get_Max_Stack_Usage (Gnattest_T : in out Test);
   procedure Test_Get_Max_Stack_Usage_4e2158 (Gnattest_T : in out Test) renames Test_2_Get_Max_Stack_Usage;
--  id:2.2/4e21586547e37cc6/Get_Max_Stack_Usage/0/0/
   procedure Test_2_Get_Max_Stack_Usage (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Graph : Control_Flow_Graph_Type;
      Sub   : Subprogram_Type;
   begin
      Sub := Create (Name        => "Foo",
                     Stack_Usage => 12);
      Sub.Max_Stack_Usage := 55;
      Graph.Nodes.Insert (Key      => Sub.Name,
                          New_Item => Sub);

      Assert (Condition => Get_Max_Stack_Usage (Graph     => Graph,
                                                Node_Name => "Foo") = 55,
              Message   => "Max stack usage mismatch");

      begin
         declare
            Dummy : constant Natural
              := Get_Max_Stack_Usage (Graph     => Graph,
                                      Node_Name => "nonexistent");
         begin
            Assert (Condition => False,
                    Message   => "Exception expected");

         end;

      exception
         when E : Missing_Subprogram =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "No subprogram with name 'nonexistent' in control flow"
                    & " graph",
                    Message   => "Exception message mismatch");
      end;
--  begin read only
   end Test_2_Get_Max_Stack_Usage;
--  end read only


--  begin read only
   procedure Test_Calculate_Stack_Usage (Gnattest_T : in out Test);
   procedure Test_Calculate_Stack_Usage_ebf2ac (Gnattest_T : in out Test) renames Test_Calculate_Stack_Usage;
--  id:2.2/ebf2ac75be8ffb9e/Calculate_Stack_Usage/1/0/
   procedure Test_Calculate_Stack_Usage (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Graph : Control_Flow_Graph_Type;
   begin

      --  Main (0 bytes) -> A (12 bytes) -> B (44 bytes)
      --                               \    |
      --                                \-> C ( 8 bytes)
      Add_Node (Graph      => Graph,
                Subprogram => Create (Name        => "Main",
                                      Stack_Usage => 0));
      Add_Call (Graph       => Graph,
                Source_Name => "Main",
                Target_Name => "A");
      Add_Node (Graph      => Graph,
                Subprogram => Create (Name        => "A",
                                      Stack_Usage => 12));
      Add_Call (Graph       => Graph,
                Source_Name => "A",
                Target_Name => "B");
      Add_Call (Graph       => Graph,
                Source_Name => "A",
                Target_Name => "C");
      Add_Node (Graph      => Graph,
                Subprogram => Create (Name        => "B",
                                      Stack_Usage => 44));
      Add_Call (Graph       => Graph,
                Source_Name => "B",
                Target_Name => "C");
      Add_Node (Graph      => Graph,
                Subprogram => Create (Name        => "C",
                                      Stack_Usage => 8));
      Calculate_Stack_Usage (Graph => Graph);

      Assert (Condition => Get_Max_Stack_Usage (Graph     => Graph,
                                                Node_Name => "Main") = 64,
              Message   => "Max stack usage mismatch (1)");
      Assert (Condition => Get_Max_Stack_Usage (Graph     => Graph,
                                                Node_Name => "A") = 64,
              Message   => "Max stack usage mismatch (2)");
      Assert (Condition => Get_Max_Stack_Usage (Graph     => Graph,
                                                Node_Name => "B") = 52,
              Message   => "Max stack usage mismatch (3)");
      Assert (Condition => Get_Max_Stack_Usage (Graph     => Graph,
                                                Node_Name => "C") = 8,
              Message   => "Max stack usage mismatch (4)");

      Recursive_Call :
      declare
         Circ_Graph : Control_Flow_Graph_Type;
      begin

         --  X (24 bytes) -> Y (0 bytes)
         --            ^     |
         --             \--- Z (4 bytes)

         Add_Node (Graph      => Graph,
                   Subprogram => Create (Name        => "X",
                                         Stack_Usage => 24));
         Add_Call (Graph       => Graph,
                   Source_Name => "X",
                   Target_Name => "Y");
         Add_Node (Graph      => Graph,
                   Subprogram => Create (Name        => "Y",
                                         Stack_Usage => 0));
         Add_Call (Graph       => Graph,
                   Source_Name => "Y",
                   Target_Name => "Z");
         Add_Node (Graph      => Graph,
                   Subprogram => Create (Name        => "Z",
                                         Stack_Usage => 4));
         Add_Call (Graph       => Graph,
                   Source_Name => "Z",
                   Target_Name => "X");
         Calculate_Stack_Usage (Graph => Graph);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Circular_Graph =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "X->Y->Z->X: Recursive call detected",
                    Message   => "Exception message mismatch");
      end Recursive_Call;
--  begin read only
   end Test_Calculate_Stack_Usage;
--  end read only


--  begin read only
   procedure Test_Equal_Name (Gnattest_T : in out Test);
   procedure Test_Equal_Name_85b0c9 (Gnattest_T : in out Test) renames Test_Equal_Name;
--  id:2.2/85b0c9397ba7bfe7/Equal_Name/1/0/
   procedure Test_Equal_Name (Gnattest_T : in out Test) is
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
end Stackcheck.Types.Test_Data.Tests;
