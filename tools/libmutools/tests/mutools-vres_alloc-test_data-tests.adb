--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Vres_Alloc.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Ada.Exceptions;

with DOM.Core;
with DOM.Core.Nodes;
with DOM.Core.Documents;
with DOM.Core.Elements;

with Mutools.Intervals;

--  begin read only
--  end read only
package body Mutools.Vres_Alloc.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Allocate_And_Set_Single_Resource (Gnattest_T : in out Test);
   procedure Test_Allocate_And_Set_Single_Resource_1dada5 (Gnattest_T : in out Test) renames Test_Allocate_And_Set_Single_Resource;
--  id:2.2/1dada56e3d4e1a37/Allocate_And_Set_Single_Resource/1/0/
   procedure Test_Allocate_And_Set_Single_Resource (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Ival           : Intervals.Interval_List_Type;
      Implementation : DOM.Core.DOM_Implementation;
      Doc            : DOM.Core.Node
        := DOM.Core.Create_Document (Implementation);
      Node           : DOM.Core.Node;

      procedure Test_Event
        (Resource_Kind : Resource_Kind_Type;
         Attr_Name     : String)
      is
      begin
         Node :=  DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "node");
         Intervals.Clear (List => Ival);
         Intervals.Add_Interval (List             => Ival,
                                 Interval         => Intervals.Interval_Type'
                                   (First_Element => 10,
                                    Last_Element  => 15));
         Allocate_And_Set_Single_Resource
           (Av_Ival       => Ival,
            Node          => Node,
            Resource_Kind => Resource_Kind);
         Assert (Condition => "10"
                   = DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => Attr_Name),
                 Message   => Attr_Name  & " mismatch: "
                   & DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => Attr_Name));
         Assert (Condition => Intervals.Interval_List_To_String_Hex (List => Ival)
                   = "(16#000b#, 16#000f#)",
                 Message   => "Intervals mismatch:"
                   & Intervals.Interval_List_To_String_Hex (List => Ival)
                   & " /= "
                   & "(16#100b#, 16#000f#)");
      end Test_Event;

   begin
      --  Positive test for Virtual_Addresses
      begin
         Node :=  DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "node");
         Intervals.Add_Interval (List     => Ival,
                                 Interval => Intervals.Interval_Type'
                                   (First_Element => 16#0000#,
                                    Last_Element  => 16#2000_0000#));
         Allocate_And_Set_Single_Resource
           (Av_Ival       => Ival,
            Node          => Node,
            Resource_Kind => Virtual_Addresses,
            Size          => "16#1000#");
         Assert (Condition => "16#0000#"
                   = DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "virtualAddress"),
                 Message   => "virtualAddress mismatch: "
                   & DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "virtualAddress"));
         Assert (Condition => "(16#1000#, 16#2000_0000#)"
                   = Intervals.Interval_List_To_String_Hex (List => Ival),
                 Message   => "Intervals mismatch:"
                   & Intervals.Interval_List_To_String_Hex (List => Ival)
                   & " /= "
                   & "(16#1000#, 16#2000_0000#)");

         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "size",
            Value => "16#0004_0000#");
         Allocate_And_Set_Single_Resource
           (Av_Ival       => Ival,
            Node          => Node,
            Resource_Kind => Virtual_Addresses);
         Assert (Condition => "16#1000#"
                   = DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "virtualAddress"),
                 Message   => "virtualAddress mismatch: "
                   & DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "virtualAddress"));
         Assert (Condition => "(16#0004_1000#, 16#2000_0000#)"
                   = Intervals.Interval_List_To_String_Hex (List => Ival),
                 Message   => "Intervals mismatch:"
                   & Intervals.Interval_List_To_String_Hex (List => Ival)
                   & " /= "
                   & "(16#0004_1000#, 16#2000_0000#)");
      end;

      --  Positive test for Reader_Vectors and Writer_Events
      Test_Event (Resource_Kind => Reader_Vectors, Attr_Name => "vector");
      Test_Event (Resource_Kind => Writer_Events, Attr_Name => "event");

      --  Negative test for Virtual_Addresses: not aligned
      begin
         Node :=  DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "node");
         Intervals.Add_Interval (List     => Ival,
                                 Interval => Intervals.Interval_Type'
                                   (First_Element => 16#0000#,
                                    Last_Element  => 16#2000#));
         Allocate_And_Set_Single_Resource
           (Av_Ival       => Ival,
            Node          => Node,
            Resource_Kind => Virtual_Addresses,
            Size          => "16#0400#");
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Virtual resource not aligned",
                    Message   => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
      end;
--  begin read only
   end Test_Allocate_And_Set_Single_Resource;
--  end read only


--  begin read only
   procedure Test_Get_Target_String (Gnattest_T : in out Test);
   procedure Test_Get_Target_String_429be5 (Gnattest_T : in out Test) renames Test_Get_Target_String;
--  id:2.2/429be579202eb1dd/Get_Target_String/1/0/
   procedure Test_Get_Target_String (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Vector : Mutools.String_Vector.Vector;
      procedure Test
        (Target_List : Mutools.String_Vector.Vector;
         Prefix      : String;
         Result_Ref  : String)
      is
         Result : constant String
           :=  Get_Target_String
           (Target_List => Target_List,
            Prefix      => Prefix);
      begin
         Assert (Condition => Result = Result_Ref,
                 Message   => "String mismatch: " & Result);
      end;
   begin
      Test (Target_List => Mutools.String_Vector."&"
              ("memory/memory",
               "channels/reader"),
            Prefix      => "pre",
            Result_Ref  => "pre/memory/memory | pre/channels/reader");
      Test (Target_List => Mutools.String_Vector."&"
              ("memory/memory",
               "channels/reader"),
            Prefix      => ".",
            Result_Ref  => "./memory/memory | ./channels/reader");
      Test (Target_List => Mutools.String_Vector.To_Vector
              (New_Item => "memory/memory",
               Length   => 1),
            Prefix      => "",
            Result_Ref  => "/memory/memory");
      Test (Target_List => Mutools.String_Vector.Empty_Vector,
            Prefix      => "foo",
            Result_Ref  => "*[false()]");

--  begin read only
   end Test_Get_Target_String;
--  end read only


--  begin read only
   procedure Test_Get_Resource_Size (Gnattest_T : in out Test);
   procedure Test_Get_Resource_Size_b15b54 (Gnattest_T : in out Test) renames Test_Get_Resource_Size;
--  id:2.2/b15b543726e3b249/Get_Resource_Size/1/0/
   procedure Test_Get_Resource_Size (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      use type Interfaces.Unsigned_64;

      Doc, Single_Node, Array_Node : DOM.Core.Node;
      Result                       : Interfaces.Unsigned_64;
      Implementation               : DOM.Core.DOM_Implementation;

   begin
      Doc         := DOM.Core.Create_Document (Implementation);
      Single_Node :=  DOM.Core.Documents.Create_Element
        (Doc      => Doc,
         Tag_Name => "not_an_array");
      Array_Node  := DOM.Core.Documents.Create_Element
        (Doc      => Doc,
         Tag_Name => "array");

      --  Resource_Kind: Virtual_Addresses
      --  A single element
      DOM.Core.Elements.Set_Attribute
        (Elem  => Single_Node,
         Name  => "size",
         Value => "16#1234_0000#");
      Result := Get_Resource_Size
        (Elem          => Single_Node,
         Resource_Kind => Virtual_Addresses);
      Assert (Condition => Result = 16#1234_0000#,
              Message   => "Size mismatch: " & Result'Image);

      --  Array element
      DOM.Core.Elements.Set_Attribute
        (Elem  => Array_Node,
         Name  => "elementSize",
         Value => "16#0001#");
      Result := Get_Resource_Size
        (Elem          => Array_Node,
         Resource_Kind => Virtual_Addresses);
      Assert (Condition => Result = 16#0001#,
              Message   => "Size mismatch: " & Result'Image);

      --  Resource_Kind: Reader
      --  Size set to something (must not be read)
      Result := Get_Resource_Size
        (Elem          => Single_Node,
         Resource_Kind => Reader_Vectors);
      Assert (Condition => Result = 1,
              Message   => "Size mismatch: " & Result'Image);

      --  Resource_Kind: Writer
      --  Array
      Result := Get_Resource_Size
        (Elem          => Array_Node,
         Resource_Kind => Reader_Vectors);
      Assert (Condition => Result = 1,
              Message   => "Size mismatch: " & Result'Image);

      DOM.Core.Nodes.Free (N => Single_Node, Deep => True);
      DOM.Core.Nodes.Free (N => Array_Node, Deep => True);

      --  Negative tests
      Size_Not_Set:
      begin
         Single_Node :=  DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "not_an_array");
         Result := Get_Resource_Size
           (Elem          => Single_Node,
            Resource_Kind => Virtual_Addresses);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Could not find 'size'/'elementSize' attribute in "
                      & "node at '/not_an_array'",
                    Message   => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
            DOM.Core.Nodes.Free (N => Single_Node, Deep => True);
      end Size_Not_Set;

      --  Size set to value that cannot be converted
      False_Size:
      begin
         Single_Node :=  DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "not_an_array");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Single_Node,
            Name  => "size",
            Value => "-5");
         Result := Get_Resource_Size
           (Elem          => Single_Node,
            Resource_Kind => Virtual_Addresses);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : others =>
            DOM.Core.Nodes.Free (N => Single_Node, Deep => True);
      end False_Size;

--  begin read only
   end Test_Get_Resource_Size;
--  end read only


--  begin read only
   procedure Test_Get_Resource_Value (Gnattest_T : in out Test);
   procedure Test_Get_Resource_Value_6c4c5a (Gnattest_T : in out Test) renames Test_Get_Resource_Value;
--  id:2.2/6c4c5a783b65d3fc/Get_Resource_Value/1/0/
   procedure Test_Get_Resource_Value (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Implementation : DOM.Core.DOM_Implementation;
      Doc            : DOM.Core.Node
        := DOM.Core.Create_Document (Implementation);

      procedure Test
        (Tag_Name        : String;
         Attribute_Name  : String;
         Attribute_Value : String;
         Resource_Kind   : Resource_Kind_Type;
         Result_Ref      : String)
      is
         Node : DOM.Core.Node;
      begin
         Node :=  DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => Tag_Name);
         if Attribute_Name /= "" then
            DOM.Core.Elements.Set_Attribute
              (Elem  => Node,
               Name  => Attribute_Name,
               Value => Attribute_Value);
         end if;
         Assert (Condition => Result_Ref = Get_Resource_Value
                   (Elem          => Node,
                    Resource_Kind => Resource_Kind),
                 Message   => "Value mismatch: '"
                   & Get_Resource_Value
                   (Elem          => Node,
                    Resource_Kind => Resource_Kind)
                   & "' /= '"
                   & Result_Ref
                   & "'");
         DOM.Core.Nodes.Free (N => Node, Deep => True);
      end Test;
   begin
      --  Positive tests
      Test (Tag_Name        => "reader",
            Attribute_Name  => "virtualAddress",
            Attribute_Value => "16#1234#",
            Resource_Kind   => Virtual_Addresses,
            Result_Ref      => "16#1234#");
      Test (Tag_Name        => "writer",
            Attribute_Name  => "event",
            Attribute_Value => "15",
            Resource_Kind   => Writer_Events,
            Result_Ref      => "15");
      Test (Tag_Name        => "reader",
            Attribute_Name  => "vector",
            Attribute_Value => "16",
            Resource_Kind   => Reader_Vectors,
            Result_Ref      => "16");

      Test (Tag_Name        => "array",
            Attribute_Name  => "virtualAddressBase",
            Attribute_Value => "16#1000_0000_0000_0000#",
            Resource_Kind   => Virtual_Addresses,
            Result_Ref      => "16#1000_0000_0000_0000#");
      Test (Tag_Name        => "array",
            Attribute_Name  => "eventBase",
            Attribute_Value => "11",
            Resource_Kind   => Writer_Events,
            Result_Ref      => "11");
      Test (Tag_Name        => "array",
            Attribute_Name  => "vectorBase",
            Attribute_Value => "0",
            Resource_Kind   => Reader_Vectors,
            Result_Ref      => "0");

      Test (Tag_Name        => "event",
            Attribute_Name  => "id",
            Attribute_Value => "7",
            Resource_Kind   => Writer_Events,
            Result_Ref      => "7");

      --  Test getting 'vector'-attribute of inject_interrupt-child
      declare
         Node, Child : DOM.Core.Node;
      begin
         Node :=  DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "event");
         Child := DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "inject_interrupt");
         Child := DOM.Core.Nodes.Append_Child (N => Node, New_Child => Child);
         DOM.Core.Elements.Set_Attribute
           (Elem  => Child,
            Name  => "vector",
            Value => "200");
         Assert (Condition => "200" = Get_Resource_Value
                   (Elem          => Node,
                    Resource_Kind => Reader_Vectors),
                 Message   => "Value mismatch: "
                   & Get_Resource_Value
                   (Elem          => Node,
                    Resource_Kind => Reader_Vectors));
         DOM.Core.Nodes.Free (N => Node, Deep => True);
      end;

      --  Test case that the event does not have a child
      Test (Tag_Name        => "event",
            Attribute_Name  => "id",
            Attribute_Value => "7",
            Resource_Kind   => Reader_Vectors,
            Result_Ref      => "");

      --  Negative Test: wrong tagname
      begin
         Test (Tag_Name        => "writer",
               Attribute_Name  => "id",
               Attribute_Value => "7",
               Resource_Kind   => Reader_Vectors,
               Result_Ref      => "");
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Found unexpected node tag 'writer' "
                      & "when reading attribute value for reader event",
                    Message   => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
      end;

      --  Negative Test: wrong tagname
      begin
         Test (Tag_Name        => "foo",
               Attribute_Name  => "event",
               Attribute_Value => "7",
               Resource_Kind   => Writer_Events,
               Result_Ref      => "");
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Found unexpected node tag 'foo' "
                      & "when reading attribute value for writer event",
                    Message   => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
      end;

--  begin read only
   end Test_Get_Resource_Value;
--  end read only


--  begin read only
   procedure Test_Set_Virtual_Resource (Gnattest_T : in out Test);
   procedure Test_Set_Virtual_Resource_a05811 (Gnattest_T : in out Test) renames Test_Set_Virtual_Resource;
--  id:2.2/a05811faf89d630e/Set_Virtual_Resource/1/0/
   procedure Test_Set_Virtual_Resource (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Implementation : DOM.Core.DOM_Implementation;
      Doc            : DOM.Core.Node
        := DOM.Core.Create_Document (Implementation);

      procedure Test
        (Tag_Name        : String;
         Attribute_Name  : String;
         Attribute_Value : String;
         Resource_Kind   : Resource_Kind_Type;
         Result_Ref      : String)
      is
         Node : DOM.Core.Node;
      begin
         Node :=  DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => Tag_Name);
         if Attribute_Name /= "" then
            DOM.Core.Elements.Set_Attribute
              (Elem  => Node,
               Name  => Attribute_Name,
               Value => Attribute_Value);
         end if;
         Assert (Condition => Result_Ref = Get_Resource_Value
                   (Elem          => Node,
                    Resource_Kind => Resource_Kind),
                 Message   => "Value mismatch: '"
                   & Get_Resource_Value
                   (Elem          => Node,
                    Resource_Kind => Resource_Kind)
                   & "' /= '"
                   & Result_Ref
                   & "'");
         DOM.Core.Nodes.Free (N => Node, Deep => True);
      end Test;
   begin
      --  Positive tests
      Test (Tag_Name        => "reader",
            Attribute_Name  => "virtualAddress",
            Attribute_Value => "16#1234#",
            Resource_Kind   => Virtual_Addresses,
            Result_Ref      => "16#1234#");
      Test (Tag_Name        => "writer",
            Attribute_Name  => "event",
            Attribute_Value => "15",
            Resource_Kind   => Writer_Events,
            Result_Ref      => "15");
      Test (Tag_Name        => "reader",
            Attribute_Name  => "vector",
            Attribute_Value => "16",
            Resource_Kind   => Reader_Vectors,
            Result_Ref      => "16");

      Test (Tag_Name        => "array",
            Attribute_Name  => "virtualAddressBase",
            Attribute_Value => "16#1000_0000_0000_0000#",
            Resource_Kind   => Virtual_Addresses,
            Result_Ref      => "16#1000_0000_0000_0000#");
      Test (Tag_Name        => "array",
            Attribute_Name  => "eventBase",
            Attribute_Value => "11",
            Resource_Kind   => Writer_Events,
            Result_Ref      => "11");
      Test (Tag_Name        => "array",
            Attribute_Name  => "vectorBase",
            Attribute_Value => "0",
            Resource_Kind   => Reader_Vectors,
            Result_Ref      => "0");

      Test (Tag_Name        => "event",
            Attribute_Name  => "id",
            Attribute_Value => "7",
            Resource_Kind   => Writer_Events,
            Result_Ref      => "7");

      --  Test getting 'vector'-attribute of inject_interrupt-child
      declare
         Node, Child : DOM.Core.Node;
      begin
         Node :=  DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "event");
         Child := DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "inject_interrupt");
         Child := DOM.Core.Nodes.Append_Child (N => Node, New_Child => Child);
         DOM.Core.Elements.Set_Attribute
           (Elem  => Child,
            Name  => "vector",
            Value => "200");
         Assert (Condition => "200" = Get_Resource_Value
                   (Elem          => Node,
                    Resource_Kind => Reader_Vectors),
                 Message   => "Value mismatch: "
                   & Get_Resource_Value
                   (Elem          => Node,
                    Resource_Kind => Reader_Vectors));
         DOM.Core.Nodes.Free (N => Node, Deep => True);
      end;

      --  Test case that the event does not have a child
      Test (Tag_Name        => "event",
            Attribute_Name  => "id",
            Attribute_Value => "7",
            Resource_Kind   => Reader_Vectors,
            Result_Ref      => "");

      --  Negative Test: wrong tagname
      begin
         Test (Tag_Name        => "writer",
               Attribute_Name  => "id",
               Attribute_Value => "7",
               Resource_Kind   => Reader_Vectors,
               Result_Ref      => "");
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Found unexpected node tag 'writer' "
                      & "when reading attribute value for reader event",
                    Message   => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
      end;

--  begin read only
   end Test_Set_Virtual_Resource;
--  end read only


--  begin read only
   procedure Test_Is_Aligned (Gnattest_T : in out Test);
   procedure Test_Is_Aligned_4acf84 (Gnattest_T : in out Test) renames Test_Is_Aligned;
--  id:2.2/4acf84aae05a059b/Is_Aligned/1/0/
   procedure Test_Is_Aligned (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      Assert (Condition => True = Is_Aligned (Address => 16#1001_0000#,
                                              Size    => 16#1234_5678_1000#),
              Message   => "False negative alignment.");
      Assert (Condition => False = Is_Aligned (Address => 16#0001#,
                                               Size    => 16#1000#),
              Message   => "False positive alignment (Address not aligned)");
      Assert (Condition => False = Is_Aligned (Size => 16#0100#),
              Message   => "False positive alignment (Size not aligned)");
      Assert (Condition => False = Is_Aligned (Size => 16#0000#),
              Message   => "False positive alignment (Size is 0)");
      Assert (Condition => False = Is_Aligned (Address => 16#0fff#),
              Message   => "False positive alignment (Address not aligned)");

--  begin read only
   end Test_Is_Aligned;
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
end Mutools.Vres_Alloc.Test_Data.Tests;
