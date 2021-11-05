--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;
--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Mucfgcheck.Validation_Errors;
--  begin read only
--  end read only
package body Mucfgcheck.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Equals (Gnattest_T : in out Test);
   procedure Test_Equals_154a90 (Gnattest_T : in out Test) renames Test_Equals;
--  id:2.2/154a906eb6a86f76/Equals/1/0/
   procedure Test_Equals (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Equals
              (A => 12,
               B => 12),
              Message   => "Not equal");
      Assert (Condition => not Equals
              (A => 12,
               B => 13),
              Message   => "Equal");
--  begin read only
   end Test_Equals;
--  end read only


--  begin read only
   procedure Test_Not_Equals (Gnattest_T : in out Test);
   procedure Test_Not_Equals_3adfb4 (Gnattest_T : in out Test) renames Test_Not_Equals;
--  id:2.2/3adfb4e25d6ea0bf/Not_Equals/1/0/
   procedure Test_Not_Equals (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Not_Equals
              (A => 12,
               B => 13),
              Message   => "Equal");
      Assert (Condition => not Not_Equals
              (A => 12,
               B => 12),
              Message   => "Not equal");
--  begin read only
   end Test_Not_Equals;
--  end read only


--  begin read only
   procedure Test_Less_Than (Gnattest_T : in out Test);
   procedure Test_Less_Than_bdb96b (Gnattest_T : in out Test) renames Test_Less_Than;
--  id:2.2/bdb96be21ba18c9f/Less_Than/1/0/
   procedure Test_Less_Than (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Less_Than
              (A => 12,
               B => 13),
              Message   => "Not smaller");
      Assert (Condition => not Less_Than
              (A => 12,
               B => 12),
              Message   => "Smaller (1)");
      Assert (Condition => not Less_Than
              (A => 13,
               B => 12),
              Message   => "Smaller (2)");
--  begin read only
   end Test_Less_Than;
--  end read only


--  begin read only
   procedure Test_Less_Or_Equal (Gnattest_T : in out Test);
   procedure Test_Less_Or_Equal_e928aa (Gnattest_T : in out Test) renames Test_Less_Or_Equal;
--  id:2.2/e928aab62e6788f9/Less_Or_Equal/1/0/
   procedure Test_Less_Or_Equal (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Less_Or_Equal
              (A => 12,
               B => 13),
              Message   => "Not smaller (1)");
      Assert (Condition => Less_Or_Equal
              (A => 12,
               B => 12),
              Message   => "Not smaller (2)");
      Assert (Condition => not Less_Or_Equal
              (A => 13,
               B => 12),
              Message   => "Smaller");
--  begin read only
   end Test_Less_Or_Equal;
--  end read only


--  begin read only
   procedure Test_Mod_Equal_Zero (Gnattest_T : in out Test);
   procedure Test_Mod_Equal_Zero_7bedac (Gnattest_T : in out Test) renames Test_Mod_Equal_Zero;
--  id:2.2/7bedac59e660e21f/Mod_Equal_Zero/1/0/
   procedure Test_Mod_Equal_Zero (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Mod_Equal_Zero
              (A => 2048,
               B => 8),
              Message   => "Mod not zero");
      Assert (Condition => not Mod_Equal_Zero
              (A => 9,
               B => 8),
              Message   => "Mod zero");
--  begin read only
   end Test_Mod_Equal_Zero;
--  end read only


--  begin read only
   procedure Test_In_Range (Gnattest_T : in out Test);
   procedure Test_In_Range_b96b51 (Gnattest_T : in out Test) renames Test_In_Range;
--  id:2.2/b96b51a879313392/In_Range/1/0/
   procedure Test_In_Range (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => In_Range
              (A => 12,
               B => 8,
               C => 24),
              Message   => "Not in range (1)");
      Assert (Condition => In_Range
              (A => 8,
               B => 8,
               C => 24),
              Message   => "Not in range (2)");
      Assert (Condition => In_Range
              (A => 24,
               B => 8,
               C => 24),
              Message   => "Not in range (3)");

      Assert (Condition => not In_Range
              (A => 25,
               B => 8,
               C => 24),
              Message   => "In range");
--  begin read only
   end Test_In_Range;
--  end read only


--  begin read only
   procedure Test_1_Check_Attribute (Gnattest_T : in out Test);
   procedure Test_Check_Attribute_756d3d (Gnattest_T : in out Test) renames Test_1_Check_Attribute;
--  id:2.2/756d3d61c0854123/Check_Attribute/1/0/
   procedure Test_1_Check_Attribute (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl         : DOM.Core.DOM_Implementation;
      Data         : Muxml.XML_Data_Type;
      Parent, Node : DOM.Core.Node;
      Nodes        : DOM.Core.Node_List;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Parent := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "parent");
      Muxml.Utils.Append_Child
        (Node      => Data.Doc,
         New_Child => Parent);

      Node := Create_Mem_Node
        (Doc     => Data.Doc,
         Name    => "mem1",
         Address => "16#1000#",
         Size    => "16#1000#");
      Muxml.Utils.Append_Child
        (Node      => Parent,
         New_Child => Node);
      Node := Create_Mem_Node
        (Doc     => Data.Doc,
         Name    => "mem2",
         Address => "16#1000#",
         Size    => "16#1000#");
      Muxml.Utils.Append_Child
        (Node      => Parent,
         New_Child => Node);

      Nodes := DOM.Core.Documents.Get_Elements_By_Tag_Name
        (Doc      => Data.Doc,
         Tag_Name => "memory");

      --  Should not raise an exception.

      Check_Attribute
        (Nodes     => Nodes,
         Node_Type => "test",
         Attr      => "address",
         Name_Attr => "name",
         Test      => Equals'Access,
         B         => 16#1000#,
         Error_Msg => "not equal 16#1000#");

      Check_Attribute
        (Nodes     => Nodes,
         Node_Type => "test",
         Attr      => "address",
         Name_Attr => "name",
         Test      => Equals'Access,
         B         => 16#2000#,
         Error_Msg => "not equal 16#2000#");

      Assert (Condition => Validation_Errors.Contains
              (Msg => "Attribute 'address => 16#1000#' of 'mem1' test element "
               & "not equal 16#2000#"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_1_Check_Attribute;
--  end read only


--  begin read only
   procedure Test_2_Check_Attribute (Gnattest_T : in out Test);
   procedure Test_Check_Attribute_9bc01a (Gnattest_T : in out Test) renames Test_2_Check_Attribute;
--  id:2.2/9bc01a8367b39abc/Check_Attribute/0/0/
   procedure Test_2_Check_Attribute (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl         : DOM.Core.DOM_Implementation;
      Data         : Muxml.XML_Data_Type;
      Parent, Node : DOM.Core.Node;
      Nodes        : DOM.Core.Node_List;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Parent := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "parent");
      Muxml.Utils.Append_Child
        (Node      => Data.Doc,
         New_Child => Parent);

      Node := Create_Mem_Node
        (Doc     => Data.Doc,
         Name    => "mem1",
         Address => "16#1000#",
         Size    => "16#1000#");
      Muxml.Utils.Append_Child
        (Node      => Parent,
         New_Child => Node);
      Node := Create_Mem_Node
        (Doc     => Data.Doc,
         Name    => "mem2",
         Address => "16#1000#",
         Size    => "16#1000#");
      Muxml.Utils.Append_Child
        (Node      => Parent,
         New_Child => Node);

      Nodes := DOM.Core.Documents.Get_Elements_By_Tag_Name
        (Doc      => Data.Doc,
         Tag_Name => "memory");

      --  Should not raise an exception.

      Check_Attribute
        (Nodes     => Nodes,
         Node_Type => "test",
         Attr      => "address",
         Name_Attr => "name",
         Test      => In_Range'Access,
         B         => 0,
         C         => 16#1000#,
         Error_Msg => "not in range 0 .. 16#1000#");

      Check_Attribute
        (Nodes     => Nodes,
         Node_Type => "test",
         Attr      => "address",
         Name_Attr => "name",
         Test      => In_Range'Access,
         B         => 16#2000#,
         C         => 16#3000#,
         Error_Msg => "not in range 16#2000# .. 16#3000#");
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Attribute 'address => 16#1000#' of 'mem1' test element "
               & "not in range 16#2000# .. 16#3000#"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_2_Check_Attribute;
--  end read only


--  begin read only
   procedure Test_Check_Memory_Overlap (Gnattest_T : in out Test);
   procedure Test_Check_Memory_Overlap_a58328 (Gnattest_T : in out Test) renames Test_Check_Memory_Overlap;
--  id:2.2/a58328a6bb7fd357/Check_Memory_Overlap/1/0/
   procedure Test_Check_Memory_Overlap (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data         : Muxml.XML_Data_Type;
      Impl         : DOM.Core.DOM_Implementation;
      Parent, Node : DOM.Core.Node;
      Nodes        : DOM.Core.Node_List;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Parent := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "parent");
      Muxml.Utils.Append_Child
        (Node      => Data.Doc,
         New_Child => Parent);

      Node := Create_Mem_Node
        (Doc     => Data.Doc,
         Name    => "mem1",
         Address => "16#1000#",
         Size    => "16#1000#");
      Muxml.Utils.Append_Child
        (Node      => Parent,
         New_Child => Node);

      Node := Create_Mem_Node
        (Doc     => Data.Doc,
         Name    => "mem2",
         Address => "16#2000#",
         Size    => "16#1000#");
      Muxml.Utils.Append_Child
        (Node      => Parent,
         New_Child => Node);

      Nodes := DOM.Core.Documents.Get_Elements_By_Tag_Name
        (Doc      => Data.Doc,
         Tag_Name => "memory");

      --  Should not raise an exception.

      Check_Memory_Overlap
        (Nodes        => Nodes,
         Region_Type  => "physical memory region",
         Address_Attr => "address");

      Node := Create_Mem_Node
        (Doc     => Data.Doc,
         Name    => "mem3",
         Address => "16#2100#",
         Size    => "16#1000#");
      Muxml.Utils.Append_Child
        (Node      => Parent,
         New_Child => Node);

      Nodes := DOM.Core.Documents.Get_Elements_By_Tag_Name
        (Doc      => Data.Doc,
         Tag_Name => "memory");

      Check_Memory_Overlap
        (Nodes        => Nodes,
         Region_Type  => "physical memory region",
         Address_Attr => "address");
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Overlap of physical memory region 'mem2' and 'mem3'"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_Check_Memory_Overlap;
--  end read only


--  begin read only
   procedure Test_Compare_All (Gnattest_T : in out Test);
   procedure Test_Compare_All_b91afb (Gnattest_T : in out Test) renames Test_Compare_All;
--  id:2.2/b91afb969f58b770/Compare_All/1/0/
   procedure Test_Compare_All (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Addr_Not_Equal : exception;

      --  Check that address attribute is the same for Left and Right.
      procedure Check_Address_Same (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Address_Same (Left, Right : DOM.Core.Node)
      is
         Left_Addr : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "address");
         Right_Addr : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "address");
      begin
         if Left_Addr /= Right_Addr then
            raise Addr_Not_Equal with "Address differs";
         end if;
      end Check_Address_Same;

      Data         : Muxml.XML_Data_Type;
      Impl         : DOM.Core.DOM_Implementation;
      Parent, Node : DOM.Core.Node;
      Nodes        : DOM.Core.Node_List;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Parent := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "parent");
      Muxml.Utils.Append_Child
        (Node      => Data.Doc,
         New_Child => Parent);

      Node := Create_Mem_Node
        (Doc     => Data.Doc,
         Name    => "mem1",
         Address => "16#1000#",
         Size    => "16#1000#");
      Muxml.Utils.Append_Child
        (Node      => Parent,
         New_Child => Node);

      Node := Create_Mem_Node
        (Doc     => Data.Doc,
         Name    => "mem2",
         Address => "16#1000#",
         Size    => "16#1000#");
      Muxml.Utils.Append_Child
        (Node      => Parent,
         New_Child => Node);

      --  Should not raise an exception.

      Nodes := DOM.Core.Documents.Get_Elements_By_Tag_Name
        (Doc      => Data.Doc,
         Tag_Name => "memory");
      Compare_All (Nodes      => Nodes,
                   Comparator => Check_Address_Same'Access);

      Node := Create_Mem_Node
        (Doc     => Data.Doc,
         Name    => "mem3",
         Address => "16#2000#",
         Size    => "16#1000#");
      Muxml.Utils.Append_Child
        (Node      => Parent,
         New_Child => Node);

      Nodes := DOM.Core.Documents.Get_Elements_By_Tag_Name
        (Doc      => Data.Doc,
         Tag_Name => "memory");

      begin
         Compare_All (Nodes      => Nodes,
                      Comparator => Check_Address_Same'Access);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when Addr_Not_Equal => null;
      end;
--  begin read only
   end Test_Compare_All;
--  end read only


--  begin read only
   procedure Test_2_For_Each_Match (Gnattest_T : in out Test);
   procedure Test_For_Each_Match_fdf71f (Gnattest_T : in out Test) renames Test_2_For_Each_Match;
--  id:2.2/fdf71f298f86f73c/For_Each_Match/0/0/
   procedure Test_2_For_Each_Match (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Fatal_Value : Boolean := False;

      --  Return test error message.
      procedure Error_Msg
        (Node    :     DOM.Core.Node;
         Err_Str : out Ada.Strings.Unbounded.Unbounded_String;
         Fatal   : out Boolean);

      ----------------------------------------------------------------------

      procedure Error_Msg
        (Node    :     DOM.Core.Node;
         Err_Str : out Ada.Strings.Unbounded.Unbounded_String;
         Fatal   : out Boolean)
      is
      begin
         Err_Str := Ada.Strings.Unbounded.To_Unbounded_String
           ("Name mismatch");
         Fatal := Fatal_Value;
      end Error_Msg;

      Data                   : Muxml.XML_Data_Type;
      Impl                   : DOM.Core.DOM_Implementation;
      Parent, Node           : DOM.Core.Node;
      Source_Nodes, No_Nodes : DOM.Core.Node_List;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Parent := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "parent");
      Muxml.Utils.Append_Child
        (Node      => Data.Doc,
         New_Child => Parent);

      Node := Create_Mem_Node
        (Doc     => Data.Doc,
         Name    => "mem1",
         Address => "16#1000#",
         Size    => "16#1000#");
      Muxml.Utils.Append_Child
        (Node      => Parent,
         New_Child => Node);

      Node := Create_Mem_Node
        (Doc     => Data.Doc,
         Name    => "mem2",
         Address => "16#1000#",
         Size    => "16#1000#");
      Muxml.Utils.Append_Child
        (Node      => Parent,
         New_Child => Node);

      Source_Nodes := McKae.XML.XPath.XIA.XPath_Query
        (N     => Data.Doc,
         XPath => "/parent/*");

      --  Should not raise an exception.

      For_Each_Match
        (Source_Nodes => Source_Nodes,
         Ref_Nodes    => McKae.XML.XPath.XIA.XPath_Query
           (N     => Data.Doc,
            XPath => "/parent/memory"),
         Log_Message  => "attribute matches",
         Error        => Error_Msg'Access,
         Match        => Match_Name'Access);

      For_Each_Match
        (Source_Nodes => Source_Nodes,
         Ref_Nodes    => No_Nodes,
         Log_Message  => "attribute matches",
         Error        => Error_Msg'Access,
         Match        => Match_Name'Access);
      Assert (Condition => Validation_Errors.Contains
                (Msg => "Name mismatch"),
              Message   => "Exception mismatch");
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Name mismatch"),
              Message   => "Exception mismatch");

      Validation_Errors.Clear;

      Fatal_Value := True;
      begin
         For_Each_Match
           (Source_Nodes => Source_Nodes,
            Ref_Nodes    => No_Nodes,
            Log_Message  => "attribute matches",
            Error        => Error_Msg'Access,
            Match        => Match_Name'Access);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when Validation_Errors.Validation_Error =>
            Assert (Condition => Validation_Errors.Contains
                    (Msg => "Name mismatch"),
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_2_For_Each_Match;
--  end read only


--  begin read only
   procedure Test_1_For_Each_Match (Gnattest_T : in out Test);
   procedure Test_For_Each_Match_711fd6 (Gnattest_T : in out Test) renames Test_1_For_Each_Match;
--  id:2.2/711fd6b48bcba219/For_Each_Match/1/0/
   procedure Test_1_For_Each_Match (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      --  Return test error message.
      procedure Error_Msg
        (Node    :     DOM.Core.Node;
         Err_Str : out Ada.Strings.Unbounded.Unbounded_String;
         Fatal   : out Boolean);

      ----------------------------------------------------------------------

      procedure Error_Msg
        (Node    :     DOM.Core.Node;
         Err_Str : out Ada.Strings.Unbounded.Unbounded_String;
         Fatal   : out Boolean)
      is
      begin
         Err_Str := Ada.Strings.Unbounded.To_Unbounded_String
           ("Name mismatch");
         Fatal := False;
      end Error_Msg;

      Data         : Muxml.XML_Data_Type;
      Impl         : DOM.Core.DOM_Implementation;
      Parent, Node : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Parent := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "parent");
      Muxml.Utils.Append_Child
        (Node      => Data.Doc,
         New_Child => Parent);

      Node := Create_Mem_Node
        (Doc     => Data.Doc,
         Name    => "mem1",
         Address => "16#1000#",
         Size    => "16#1000#");
      Muxml.Utils.Append_Child
        (Node      => Parent,
         New_Child => Node);

      Node := Create_Mem_Node
        (Doc     => Data.Doc,
         Name    => "mem2",
         Address => "16#1000#",
         Size    => "16#1000#");
      Muxml.Utils.Append_Child
        (Node      => Parent,
         New_Child => Node);

      --  Should not raise an exception.

      For_Each_Match
        (XML_Data     => Data,
         Source_XPath => "/parent/*",
         Ref_XPath    => "/parent/memory",
         Log_Message  => "attribute matches",
         Error        => Error_Msg'Access,
         Match        => Match_Name'Access);

      For_Each_Match
        (XML_Data     => Data,
         Source_XPath => "/parent/*",
         Ref_XPath    => "/parent/nonexistent",
         Log_Message  => "attribute matches",
         Error        => Error_Msg'Access,
         Match        => Match_Name'Access);
      Assert (Condition => Validation_Errors.Contains
              (Msg => "Name mismatch"),
              Message   => "Exception mismatch");
--  begin read only
   end Test_1_For_Each_Match;
--  end read only


--  begin read only
   procedure Test_Match_Subject_Name (Gnattest_T : in out Test);
   procedure Test_Match_Subject_Name_cb4b01 (Gnattest_T : in out Test) renames Test_Match_Subject_Name;
--  id:2.2/cb4b01672b301d4b/Match_Subject_Name/1/0/
   procedure Test_Match_Subject_Name (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data        : Muxml.XML_Data_Type;
      Impl        : DOM.Core.DOM_Implementation;
      Left, Right : DOM.Core.Node;
   begin

      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Left := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "el1");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Left,
         Name  => "subject",
         Value => "refname");
      Right := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "el2");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Right,
         Name  => "name",
         Value => "refname");

      Assert (Condition => Match_Subject_Name
              (Left  => Left,
               Right => Right),
              Message   => "Name does not match");

      DOM.Core.Elements.Set_Attribute
        (Elem  => Right,
         Name  => "name",
         Value => "nonexistent");
      Assert (Condition => not Match_Subject_Name
              (Left  => Left,
               Right => Right),
              Message   => "Name matches");
--  begin read only
   end Test_Match_Subject_Name;
   --  end read only

--  begin read only
   procedure Test_Attr_Uniqueness (Gnattest_T : in out Test);
   procedure Test_Attr_Uniqueness_10895c (Gnattest_T : in out Test) renames Test_Attr_Uniqueness;
--  id:2.2/10895ce3760b8cee/Attr_Uniqueness/1/0/
   procedure Test_Attr_Uniqueness (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Nodes     : DOM.Core.Node_List;
      Data        : Muxml.XML_Data_Type;
      Impl        : DOM.Core.DOM_Implementation;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.None,
                   File => "data/test_policy_src.xml");
      Validation_Errors.Clear;

      -- Positive test: all distinct

      Nodes := McKae.XML.XPath.XIA.XPath_Query
                 (N     => Data.Doc,
                  XPath => "//car | //measurement | //boat");
      Attr_Uniqueness (Nodes => Nodes,
                       Attr_Name => "id",
                       Error_Msg => "Ids are not unique");
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");
      Validation_Errors.Clear;

      -- Positive test: empty list

      Nodes :=  McKae.XML.XPath.XIA.XPath_Query
                 (N     => Data.Doc,
                  XPath => "/doesnotexist");
      Attr_Uniqueness (Nodes => Nodes,
                       Attr_Name => "id",
                       Error_Msg => "Ids are not unique");
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");
      Validation_Errors.Clear;

      -- Positive test: list of length 1

      Nodes :=  McKae.XML.XPath.XIA.XPath_Query
                 (N     => Data.Doc,
                  XPath => "//subject[@name='uniquenesTests']");
      Attr_Uniqueness (Nodes => Nodes,
                       Attr_Name => "id",
                       Error_Msg => "Ids are not unique");
      Assert (Condition => Validation_Errors.Is_Empty,
              Message   => "Unexpected error in positive test");
      Validation_Errors.Clear;

      -- Negative test: two nodes with same "name"

      Nodes :=  McKae.XML.XPath.XIA.XPath_Query
                 (N     => Data.Doc,
                  XPath => "//subject[@name='uniquenesTests']/car "
                  & "| //subject[@name='uniquenesTests']/car/id");
      Attr_Uniqueness (Nodes => Nodes,
                       Attr_Name => "name",
                       Error_Msg => "Names are not unique.");
      Assert (Condition =>  Validation_Errors.Contains
              (Msg => "Names are not unique. "
               & "Conflicting value: 'c1'"),
              Message   => "Exception mismatch");


--  begin read only
   end Test_Attr_Uniqueness;
--  end read only


--  begin read only
   procedure Test_Match_Name (Gnattest_T : in out Test);
   procedure Test_Match_Name_c4439a (Gnattest_T : in out Test) renames Test_Match_Name;
--  id:2.2/c4439a65405d5bb8/Match_Name/1/0/
   procedure Test_Match_Name (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data        : Muxml.XML_Data_Type;
      Impl        : DOM.Core.DOM_Implementation;
      Left, Right : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Left := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "el1");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Left,
         Name  => "name",
         Value => "refname");
      Right := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "el2");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Right,
         Name  => "name",
         Value => "refname");

      Assert (Condition => Match_Name
              (Left  => Left,
               Right => Right),
              Message   => "Name does not match");

      DOM.Core.Elements.Set_Attribute
        (Elem  => Right,
         Name  => "name",
         Value => "nonexistent");
      Assert (Condition => not Match_Name
              (Left  => Left,
               Right => Right),
              Message   => "Name matches");
--  begin read only
   end Test_Match_Name;
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
end Mucfgcheck.Test_Data.Tests;
