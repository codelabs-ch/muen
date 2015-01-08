--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Mucfgcheck.Test_Data.Tests is


--  begin read only
   procedure Test_Equals (Gnattest_T : in out Test);
   procedure Test_Equals_154a90 (Gnattest_T : in out Test) renames Test_Equals;
--  id:2.2/154a906eb6a86f76/Equals/1/0/
   procedure Test_Equals (Gnattest_T : in out Test) is
   --  mucfgcheck.ads:33:4:Equals
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Equals
              (Left  => 12,
               Right => 12),
              Message   => "Not equal");
      Assert (Condition => not Equals
              (Left  => 12,
               Right => 13),
              Message   => "Equal");
--  begin read only
   end Test_Equals;
--  end read only


--  begin read only
   procedure Test_Not_Equals (Gnattest_T : in out Test);
   procedure Test_Not_Equals_3adfb4 (Gnattest_T : in out Test) renames Test_Not_Equals;
--  id:2.2/3adfb4e25d6ea0bf/Not_Equals/1/0/
   procedure Test_Not_Equals (Gnattest_T : in out Test) is
   --  mucfgcheck.ads:38:4:Not_Equals
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Not_Equals
              (Left  => 12,
               Right => 13),
              Message   => "Equal");
      Assert (Condition => not Not_Equals
              (Left  => 12,
               Right => 12),
              Message   => "Not equal");
--  begin read only
   end Test_Not_Equals;
--  end read only


--  begin read only
   procedure Test_Less_Than (Gnattest_T : in out Test);
   procedure Test_Less_Than_bdb96b (Gnattest_T : in out Test) renames Test_Less_Than;
--  id:2.2/bdb96be21ba18c9f/Less_Than/1/0/
   procedure Test_Less_Than (Gnattest_T : in out Test) is
   --  mucfgcheck.ads:43:4:Less_Than
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Less_Than
              (Left  => 12,
               Right => 13),
              Message   => "Not smaller");
      Assert (Condition => not Less_Than
              (Left  => 12,
               Right => 12),
              Message   => "Smaller (1)");
      Assert (Condition => not Less_Than
              (Left  => 13,
               Right => 12),
              Message   => "Smaller (2)");
--  begin read only
   end Test_Less_Than;
--  end read only


--  begin read only
   procedure Test_Less_Or_Equal (Gnattest_T : in out Test);
   procedure Test_Less_Or_Equal_e928aa (Gnattest_T : in out Test) renames Test_Less_Or_Equal;
--  id:2.2/e928aab62e6788f9/Less_Or_Equal/1/0/
   procedure Test_Less_Or_Equal (Gnattest_T : in out Test) is
   --  mucfgcheck.ads:48:4:Less_Or_Equal
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Less_Or_Equal
              (Left  => 12,
               Right => 13),
              Message   => "Not smaller (1)");
      Assert (Condition => Less_Or_Equal
              (Left  => 12,
               Right => 12),
              Message   => "Not smaller (2)");
      Assert (Condition => not Less_Or_Equal
              (Left  => 13,
               Right => 12),
              Message   => "Smaller");
--  begin read only
   end Test_Less_Or_Equal;
--  end read only


--  begin read only
   procedure Test_Mod_Equal_Zero (Gnattest_T : in out Test);
   procedure Test_Mod_Equal_Zero_7bedac (Gnattest_T : in out Test) renames Test_Mod_Equal_Zero;
--  id:2.2/7bedac59e660e21f/Mod_Equal_Zero/1/0/
   procedure Test_Mod_Equal_Zero (Gnattest_T : in out Test) is
   --  mucfgcheck.ads:53:4:Mod_Equal_Zero
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Mod_Equal_Zero
              (Left  => 2048,
               Right => 8),
              Message   => "Mod not zero");
      Assert (Condition => not Mod_Equal_Zero
              (Left  => 9,
               Right => 8),
              Message   => "Mod zero");
--  begin read only
   end Test_Mod_Equal_Zero;
--  end read only


--  begin read only
   procedure Test_Check_Attribute (Gnattest_T : in out Test);
   procedure Test_Check_Attribute_4b2973 (Gnattest_T : in out Test) renames Test_Check_Attribute;
--  id:2.2/4b2973ea22b182e5/Check_Attribute/1/0/
   procedure Test_Check_Attribute (Gnattest_T : in out Test) is
   --  mucfgcheck.ads:62:4:Check_Attribute
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
         Right     => 16#1000#,
         Error_Msg => "not equal 16#1000#");

      begin
         Check_Attribute
           (Nodes     => Nodes,
            Node_Type => "test",
            Attr      => "address",
            Name_Attr => "name",
            Test      => Equals'Access,
            Right     => 16#2000#,
            Error_Msg => "not equal 16#2000#");
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'address => 16#1000#' of 'mem1' test element "
                    & "not equal 16#2000#",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Check_Attribute;
--  end read only


--  begin read only
   procedure Test_Check_Memory_Overlap (Gnattest_T : in out Test);
   procedure Test_Check_Memory_Overlap_a58328 (Gnattest_T : in out Test) renames Test_Check_Memory_Overlap;
--  id:2.2/a58328a6bb7fd357/Check_Memory_Overlap/1/0/
   procedure Test_Check_Memory_Overlap (Gnattest_T : in out Test) is
   --  mucfgcheck.ads:75:4:Check_Memory_Overlap
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

      begin
         Check_Memory_Overlap
           (Nodes        => Nodes,
            Region_Type  => "physical memory region",
            Address_Attr => "address");
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Overlap of physical memory region 'mem2' and 'mem3'",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Check_Memory_Overlap;
--  end read only


--  begin read only
   procedure Test_Compare_All (Gnattest_T : in out Test);
   procedure Test_Compare_All_b91afb (Gnattest_T : in out Test) renames Test_Compare_All;
--  id:2.2/b91afb969f58b770/Compare_All/1/0/
   procedure Test_Compare_All (Gnattest_T : in out Test) is
   --  mucfgcheck.ads:84:4:Compare_All
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
   procedure Test_For_Each_Match (Gnattest_T : in out Test);
   procedure Test_For_Each_Match_86b711 (Gnattest_T : in out Test) renames Test_For_Each_Match;
--  id:2.2/86b7111f1d089f0c/For_Each_Match/1/0/
   procedure Test_For_Each_Match (Gnattest_T : in out Test) is
   --  mucfgcheck.ads:92:4:For_Each_Match
--  end read only

      pragma Unreferenced (Gnattest_T);

      --  Return test error message.
      function Error_Msg (Node : DOM.Core.Node) return String;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
      begin
         return "Name mismatch";
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

      begin
         For_Each_Match
           (XML_Data     => Data,
            Source_XPath => "/parent/*",
            Ref_XPath    => "/parent/nonexistent",
            Log_Message  => "attribute matches",
            Error        => Error_Msg'Access,
            Match        => Match_Name'Access);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Name mismatch",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_For_Each_Match;
--  end read only


--  begin read only
   procedure Test_Get_Matching (Gnattest_T : in out Test);
   procedure Test_Get_Matching_1a3e5a (Gnattest_T : in out Test) renames Test_Get_Matching;
--  id:2.2/1a3e5a4544abf41a/Get_Matching/1/0/
   procedure Test_Get_Matching (Gnattest_T : in out Test) is
   --  mucfgcheck.ads:112:4:Get_Matching
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data         : Muxml.XML_Data_Type;
      Impl         : DOM.Core.DOM_Implementation;
      Parent, Node : DOM.Core.Node;
      Result       : Matching_Pairs_Type;
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

      Result := Get_Matching
        (XML_Data    => Data,
         Left_XPath  => "/parent/memory",
         Right_XPath => "/parent/memory",
         Match       => Match_Name'Access);
      Assert (Condition => DOM.Core.Nodes.Length (List => Result.Left) = 2,
              Message   => "Left match count not 2");
      Assert (Condition => DOM.Core.Nodes.Length (List => Result.Right) = 2,
              Message   => "Right match count not 2");
--  begin read only
   end Test_Get_Matching;
--  end read only


--  begin read only
   procedure Test_Is_Valid_Reference (Gnattest_T : in out Test);
   procedure Test_Is_Valid_Reference_f00842 (Gnattest_T : in out Test) renames Test_Is_Valid_Reference;
--  id:2.2/f008425ad8c5c86b/Is_Valid_Reference/1/0/
   procedure Test_Is_Valid_Reference (Gnattest_T : in out Test) is
   --  mucfgcheck.ads:122:4:Is_Valid_Reference
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
         Name  => "physical",
         Value => "refname");
      Right := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "el2");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Right,
         Name  => "name",
         Value => "refname");

      Assert (Condition => Is_Valid_Reference
              (Left  => Left,
               Right => Right),
              Message   => "Name does not match");

      DOM.Core.Elements.Set_Attribute
        (Elem  => Right,
         Name  => "name",
         Value => "nonexistent");
      Assert (Condition => not Is_Valid_Reference
              (Left  => Left,
               Right => Right),
              Message   => "Name matches");
--  begin read only
   end Test_Is_Valid_Reference;
--  end read only


--  begin read only
   procedure Test_Match_Subject_Name (Gnattest_T : in out Test);
   procedure Test_Match_Subject_Name_cb4b01 (Gnattest_T : in out Test) renames Test_Match_Subject_Name;
--  id:2.2/cb4b01672b301d4b/Match_Subject_Name/1/0/
   procedure Test_Match_Subject_Name (Gnattest_T : in out Test) is
   --  mucfgcheck.ads:126:4:Match_Subject_Name
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
   procedure Test_Set_Size (Gnattest_T : in out Test);
   procedure Test_Set_Size_e82b63 (Gnattest_T : in out Test) renames Test_Set_Size;
--  id:2.2/e82b63c700676990/Set_Size/0/0/
   procedure Test_Set_Size (Gnattest_T : in out Test) is
   --  mucfgcheck.ads:130:4:Set_Size
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data  : Muxml.XML_Data_Type;
      Impl  : DOM.Core.DOM_Implementation;
      Nodes : DOM.Core.Node_List;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      DOM.Core.Append_Node
        (List => Nodes,
         N    => Create_Mem_Node
           (Doc     => Data.Doc,
            Name    => "mem1",
            Address => "16#1000#",
            Size    => "16#1000#"));
      DOM.Core.Append_Node
        (List => Nodes,
         N    => Create_Mem_Node
           (Doc     => Data.Doc,
            Name    => "mem2",
            Address => "16#2000#",
            Size    => "16#beef_0000#"));

      declare
         Vmem_Node : DOM.Core.Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "memory");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => Vmem_Node,
            Name  => "physical",
            Value => "mem1");
         Set_Size (Virtual_Mem_Node => Vmem_Node,
                   Ref_Nodes        => Nodes);

         Assert (Condition => DOM.Core.Elements.Get_Attribute
                 (Elem => Vmem_Node,
                  Name => "size") = "16#1000#",
                 Message   => "'mem1' size mismatch");
      end;

      declare
         Vmem_Node : DOM.Core.Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "memory");
      begin
         DOM.Core.Elements.Set_Attribute
           (Elem  => Vmem_Node,
            Name  => "physical",
            Value => "mem2");
         Set_Size (Virtual_Mem_Node => Vmem_Node,
                   Ref_Nodes        => Nodes);

         Assert (Condition => DOM.Core.Elements.Get_Attribute
                 (Elem => Vmem_Node,
                  Name => "size") = "16#beef_0000#",
                 Message   => "'mem2' size mismatch");
      end;
--  begin read only
   end Test_Set_Size;
--  end read only

end Mucfgcheck.Test_Data.Tests;
