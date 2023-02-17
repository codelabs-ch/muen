--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Alloc.Map.Test_Data.

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

--  begin read only
--  end read only
package body Alloc.Map.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Get_Target_String (Gnattest_T : in out Test);
   procedure Test_Get_Target_String_1f0cfc (Gnattest_T : in out Test) renames Test_Get_Target_String;
--  id:2.2/1f0cfc866df2b790/Get_Target_String/1/0/
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
   procedure Test_To_String (Gnattest_T : in out Test);
   procedure Test_To_String_96e7f0 (Gnattest_T : in out Test) renames Test_To_String;
--  id:2.2/96e7f0344e8b8072/To_String/1/0/
   procedure Test_To_String (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => "0" = To_String (Number => 16#0000#),
              Message   => "String mismatch: " & To_String (Number => 16#0000#));
      Assert (Condition => "42" = To_String (Number => 42),
              Message   => "String mismatch: " & To_String (Number => 42));
--  begin read only
   end Test_To_String;
--  end read only


--  begin read only
   procedure Test_Get_Resource_Size (Gnattest_T : in out Test);
   procedure Test_Get_Resource_Size_dbba02 (Gnattest_T : in out Test) renames Test_Get_Resource_Size;
--  id:2.2/dbba024771ddbaae/Get_Resource_Size/1/0/
   procedure Test_Get_Resource_Size (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
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

      --  Run_Type VIRTUAL_ADDRESSES
      --  A single element
      DOM.Core.Elements.Set_Attribute
        (Elem  => Single_Node,
         Name  => "size",
         Value => "16#1234_0000#");
      Result := Get_Resource_Size
        (Elem     => Single_Node,
         Run_Type => VIRTUAL_ADDRESSES);
      Assert (Condition => Result = 16#1234_0000#,
              Message   => "Size mismatch: " & Result'Image);

      --  Array element
      DOM.Core.Elements.Set_Attribute
        (Elem  => Array_Node,
         Name  => "elementSize",
         Value => "16#0001#");
      Result := Get_Resource_Size
        (Elem     => Array_Node,
         Run_Type => VIRTUAL_ADDRESSES);
      Assert (Condition => Result = 16#0001#,
              Message   => "Size mismatch: " & Result'Image);

      --  Run_Type: Reader
      --  Size set to something (must not be read)
      Result := Get_Resource_Size
        (Elem     => Single_Node,
         Run_Type => READER_EVENTS);
      Assert (Condition => Result = 1,
              Message   => "Size mismatch: " & Result'Image);

      --  Run_Type: Writer
      --  Array
      Result := Get_Resource_Size
        (Elem     => Array_Node,
         Run_Type => READER_EVENTS);
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
           (Elem     => Single_Node,
            Run_Type => VIRTUAL_ADDRESSES);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Could not find 'size'/'elementSize' attribute in "
                      & "node at '/not_an_array'",
                    Message  => "Exception mismatch: "
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
           (Elem     => Single_Node,
            Run_Type => VIRTUAL_ADDRESSES);
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Constraint_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "bad input for 'Value: ""-5""",
                    Message  => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
            DOM.Core.Nodes.Free (N => Single_Node, Deep => True);
      end False_Size;

--  begin read only
   end Test_Get_Resource_Size;
--  end read only


--  begin read only
   procedure Test_Get_Resource_Value (Gnattest_T : in out Test);
   procedure Test_Get_Resource_Value_f982a9 (Gnattest_T : in out Test) renames Test_Get_Resource_Value;
--  id:2.2/f982a950ff707df5/Get_Resource_Value/1/0/
   procedure Test_Get_Resource_Value (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Implementation               : DOM.Core.DOM_Implementation;
      Doc : DOM.Core.Node
           := DOM.Core.Create_Document (Implementation);

      procedure Test
        (Tag_Name        : String;
         Attribute_Name  : String;
         Attribute_Value : String;
         Run_Type        : Run_Type_Type;
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
                   (Elem     => Node,
                    Run_Type => Run_Type),
                 Message   => "Value mismatch: '"
                   & Get_Resource_Value
                   (Elem     => Node,
                    Run_Type => Run_Type)
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
            Run_Type        => VIRTUAL_ADDRESSES,
            Result_Ref      => "16#1234#");
      Test (Tag_Name        => "writer",
            Attribute_Name  => "event",
            Attribute_Value => "15",
            Run_Type        => WRITER_EVENTS,
            Result_Ref      => "15");
      Test (Tag_Name        => "reader",
            Attribute_Name  => "vector",
            Attribute_Value => "16",
            Run_Type        => READER_EVENTS,
            Result_Ref      => "16");

      Test (Tag_Name        => "array",
            Attribute_Name  => "virtualAddressBase",
            Attribute_Value => "16#1000_0000_0000_0000#",
            Run_Type        => VIRTUAL_ADDRESSES,
            Result_Ref      => "16#1000_0000_0000_0000#");
      Test (Tag_Name        => "array",
            Attribute_Name  => "eventBase",
            Attribute_Value => "11",
            Run_Type        => WRITER_EVENTS,
            Result_Ref      => "11");
      Test (Tag_Name        => "array",
            Attribute_Name  => "vectorBase",
            Attribute_Value => "0",
            Run_Type        => READER_EVENTS,
            Result_Ref      => "0");

      Test (Tag_Name        => "event",
            Attribute_Name  => "id",
            Attribute_Value => "7",
            Run_Type        => WRITER_EVENTS,
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
                   (Elem     => Node,
                    Run_Type => READER_EVENTS),
                 Message   => "Value mismatch: "
                   & Get_Resource_Value
                   (Elem     => Node,
                    Run_Type => READER_EVENTS));
         DOM.Core.Nodes.Free (N => Node, Deep => True);
      end;

      --  Test case that the event does not have a child
      Test (Tag_Name        => "event",
            Attribute_Name  => "id",
            Attribute_Value => "7",
            Run_Type        => READER_EVENTS,
            Result_Ref      => "");

      --  Negative Test: wrong tagname
      begin
         Test (Tag_Name        => "writer",
               Attribute_Name  => "id",
               Attribute_Value => "7",
               Run_Type        => READER_EVENTS,
               Result_Ref      => "");
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Found unexpected node tag 'writer' "
                      & "when reading attribute value for reader event",
                    Message  => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
      end;
--  begin read only
   end Test_Get_Resource_Value;
--  end read only


--  begin read only
   procedure Test_Set_Virtual_Resource (Gnattest_T : in out Test);
   procedure Test_Set_Virtual_Resource_bf464f (Gnattest_T : in out Test) renames Test_Set_Virtual_Resource;
--  id:2.2/bf464f50b3b39b0d/Set_Virtual_Resource/1/0/
   procedure Test_Set_Virtual_Resource (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Implementation : DOM.Core.DOM_Implementation;
      Doc            : DOM.Core.Node
        := DOM.Core.Create_Document (Implementation);

      procedure Test
        (Attr_Name : String;
         Value     : String;
         Run_Type  : Run_Type_Type)
      is
         Node : DOM.Core.Node;
      begin

         Node :=  DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "node_name");
         Set_Virtual_Resource
           (Node     => Node,
            Run_Type => Run_Type,
            Value    => Interfaces.Unsigned_64'Value (Value));
         Assert (Condition => Value =
                   DOM.Core.Elements.Get_Attribute
                      (Elem  => Node,
                       Name  => Attr_Name),
                 Message   => "Value mismatch: '"
                   & DOM.Core.Elements.Get_Attribute
                   (Elem  => Node,
                    Name  => Attr_Name)
                   & "' /= '"
                   & Value
                   & "'");
         DOM.Core.Nodes.Free (N => Node, Deep => True);
      end Test;
   begin
      Test
        (Attr_Name => "virtualAddress",
         Value     => "16#1000_0000#",
         Run_Type  => VIRTUAL_ADDRESSES);
      Test
        (Attr_Name => "vector",
         Value     => "11",
         Run_Type  => READER_EVENTS);
      Test
        (Attr_Name => "event",
         Value     => "0",
         Run_Type  => WRITER_EVENTS);

--  begin read only
   end Test_Set_Virtual_Resource;
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
end Alloc.Map.Test_Data.Tests;
