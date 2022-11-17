--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Amend.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Ada.Exceptions;
with Ada.Directories;

with DOM.Core;
with DOM.Core.Nodes;
with DOM.Core.Documents;
with DOM.Core.Elements;
with Muxml.Utils;
with Muxml;
with Test_Utils;
--  begin read only
--  end read only
package body Mutools.Amend.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Expand (Gnattest_T : in out Test);
   procedure Test_Expand_4a19b8 (Gnattest_T : in out Test) renames Test_Expand;
--  id:2.2/4a19b878eb07fa84/Expand/1/0/
   procedure Test_Expand (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Data   : Muxml.XML_Data_Type;
         Output : constant String := "obj/output_amend_expand.xml";
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/amend_expand.xml");
         Expand (XML_Data => Data);

         Muxml.Write (Data => Data,
                      Kind => Muxml.None,
                      File => Output);
         Assert (Condition => Test_Utils.Equal_Files
                   (Filename1 => "data/output_amend_expand.xml",
                    Filename2 => Output),
                 Message   => "Policy mismatch: " & Output);

         Ada.Directories.Delete_File (Name => Output);

      end Positive_Test;

      ----------------------------------------------------------------------

      procedure No_Amend_Statements
      is
         Data   : Muxml.XML_Data_Type;
         Output : constant String := "obj/output_amend_expand_no_amend.xml";
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/amend_expand.xml");
         Muxml.Utils.Remove_Elements (Doc => Data.Doc, XPath => "//amend");
         Muxml.Write (Data => Data,
                      Kind => Muxml.None,
                      File => Output & ".ref");
         Expand (XML_Data => Data);
         Muxml.Write (Data => Data,
                      Kind => Muxml.None,
                      File => Output);
         Assert (Condition => Test_Utils.Equal_Files
                    (Filename1 => Output & ".ref",
                     Filename2 => Output),
                 Message   => "Policy mismatch: " & Output);
      end No_Amend_Statements;

      ----------------------------------------------------------------------

      procedure XPath_Not_Unique
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/amend_expand.xml");
         Muxml.Utils.Set_Attribute
            (Doc   => Data.Doc,
             XPath => "/system/amend[@xpath='/']",
             Name  => "xpath",
             Value => "/system/platform/mappings/classes/class/device");
         begin
            Expand (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");
         exception
            when E : Muxml.Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                          = "Found 2 matches (instead of 1) for XPath "
                          & """/system/platform/mappings/classes/class/device"" "
                          & "in amend statement.",
                       Message   => "Exception message mismatch: " &
                          Ada.Exceptions.Exception_Message (X => E) );
         end;
      end XPath_Not_Unique;

      ----------------------------------------------------------------------

      procedure XPath_Not_Found
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/amend_expand.xml");
         Muxml.Utils.Set_Attribute
            (Doc   => Data.Doc,
             XPath => "/system/amend[@xpath='/']",
             Name  => "xpath",
             Value => "/system/doesnotexit");
         begin
            Expand (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");
         exception
            when E : Muxml.Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                          = "Found 0 matches (instead of 1) for XPath "
                          & """/system/doesnotexit"" "
                          & "in amend statement.",
                       Message   => "Exception message mismatch: " &
                          Ada.Exceptions.Exception_Message (X => E));
         end;
      end XPath_Not_Found;

      ----------------------------------------------------------------------

      procedure XPath_Syntax_Error
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/amend_expand.xml");
         Muxml.Utils.Set_Attribute
            (Doc   => Data.Doc,
             XPath => "/system/amend[@xpath='/']",
             Name  => "xpath",
             Value => "/system/subjects/subject[@name='foo]");
         begin
            Expand (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");
         exception
            when E : Constraint_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                          = "XIA was evaluating XPath "
                          & """/system/subjects/subject[@name='foo]"" when reporting: "
                          & "Compare requires at most one of its operands "
                          & "to be a Node List: AS_NODE_LIST, AS_NODE_LIST",
                       Message   => "Exception message mismatch: " &
                          Ada.Exceptions.Exception_Message (X => E));
         end;
      end XPath_Syntax_Error;

      ----------------------------------------------------------------------

      procedure Amend_On_Textnode
      is
         Data           : Muxml.XML_Data_Type;
         Node, New_Node : DOM.Core.Node;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/amend_expand.xml");
         Node := Muxml.Utils.Get_Element
            (Doc   => Data.Doc,
             XPath => "/system/amend[@xpath='/']");
         New_Node := DOM.Core.Documents.Create_Text_Node
            (Doc => Data.Doc,
             Data => "Foobar");
         Muxml.Utils.Append_Child
           (Node      => Node,
            New_Child => New_Node);
         Muxml.Utils.Remove_Elements
            (Doc => Data.Doc,
             XPath => "/system/amend[@xpath='/']/system");
         begin
            Expand (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");
         exception
            when E : Muxml.Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                          = "Recursive_Merge got text-node with text 'Foobar'. "
                          & "Cannot process isolated text-nodes. "
                          & "Please add surrounding element-node.",
                       Message   => "Exception message mismatch: " &
                          Ada.Exceptions.Exception_Message (X => E));
         end;
      end Amend_On_Textnode;

      ----------------------------------------------------------------------

      procedure No_Legal_Insert_Position
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/amend_expand.xml");
         Muxml.Utils.Set_Attribute
            (Doc   => Data.Doc,
             XPath => "/system/platform/amend[@xpath='/system/hardware']",
             Name  => "xpath",
             Value => "/system/subjects/subject[@name='linux1']/component");
         begin
            Expand (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");
         exception
            when E : Muxml.Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                          = "Could not find valid place to insert 'memory' "
                          & "into node with name 'component'",
                       Message   => "Exception message mismatch: " &
                          Ada.Exceptions.Exception_Message (X => E));
         end;
      end No_Legal_Insert_Position;
   begin
      Positive_Test;
      No_Amend_Statements;
      XPath_Not_Unique;
      XPath_Not_Found;
      XPath_Syntax_Error;
      Amend_On_Textnode;
      No_Legal_Insert_Position;

--  begin read only
   end Test_Expand;
--  end read only


--  begin read only
   procedure Test_Nodes_Equal (Gnattest_T : in out Test);
   procedure Test_Nodes_Equal_02b22d (Gnattest_T : in out Test) renames Test_Nodes_Equal;
--  id:2.2/02b22dd1d511fcd1/Nodes_Equal/1/0/
   procedure Test_Nodes_Equal (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Dom_Impl    : DOM.Core.DOM_Implementation;
         Policy      : Muxml.XML_Data_Type;
         Left, Right : DOM.Core.Node;
      begin
         Policy.Doc := DOM.Core.Create_Document (Implementation => Dom_Impl);
         Left := DOM.Core.Documents.Create_Element
                    (Doc      => Policy.Doc,
                     Tag_Name => "memory");
         Right := DOM.Core.Documents.Create_Element
                    (Doc      => Policy.Doc,
                     Tag_Name => "memory");
         DOM.Core.Elements.Set_Attribute (Elem  => Left,
                                          Name  => "name",
                                          Value => "forBar_ mem");
         DOM.Core.Elements.Set_Attribute (Elem  => Right,
                                          Name  => "name",
                                          Value => "forBar_ mem");
         DOM.Core.Elements.Set_Attribute (Elem  => Left,
                                          Name  => "id",
                                          Value => "0123");
         DOM.Core.Elements.Set_Attribute (Elem  => Right,
                                          Name  => "id",
                                          Value => "0123");
         DOM.Core.Elements.Set_Attribute (Elem  => Left,
                                          Name  => "empty",
                                          Value => "");
         DOM.Core.Elements.Set_Attribute (Elem  => Right,
                                          Name  => "empty",
                                          Value => "");
         Assert (Condition => Nodes_Equal (L => Left, R => Right),
                 Message   => "Equal nodes reported as unequal");
      end Positive_Test;

      ----------------------------------------------------------------------

      procedure Distinct_Types
      is
         Dom_Impl    : DOM.Core.DOM_Implementation;
         Policy      : Muxml.XML_Data_Type;
         Left, Right : DOM.Core.Node;
      begin
         Policy.Doc := DOM.Core.Create_Document (Implementation => Dom_Impl);
         Left := DOM.Core.Documents.Create_Element
                    (Doc      => Policy.Doc,
                     Tag_Name => "memory");
         Right := DOM.Core.Documents.Create_Text_Node
                    (Doc  => Policy.Doc,
                     Data => "memory");
         Assert (Condition => not Nodes_Equal (L => Left, R => Right),
                 Message   => "Element- and text-nodes must not equal");
      end Distinct_Types;

      ----------------------------------------------------------------------

      procedure Distinct_Node_Names
      is
         Dom_Impl    : DOM.Core.DOM_Implementation;
         Policy      : Muxml.XML_Data_Type;
         Left, Right : DOM.Core.Node;
      begin
         Policy.Doc := DOM.Core.Create_Document (Implementation => Dom_Impl);
         Left := DOM.Core.Documents.Create_Element
                    (Doc      => Policy.Doc,
                     Tag_Name => "memory");
         Right := DOM.Core.Documents.Create_Element
                    (Doc      => Policy.Doc,
                     Tag_Name => "NotMemory");
         DOM.Core.Elements.Set_Attribute (Elem  => Left,
                                          Name  => "name",
                                          Value => "forBar_ mem");
         DOM.Core.Elements.Set_Attribute (Elem  => Right,
                                          Name  => "name",
                                          Value => "forBar_ mem");
         DOM.Core.Elements.Set_Attribute (Elem  => Left,
                                          Name  => "id",
                                          Value => "0123");
         DOM.Core.Elements.Set_Attribute (Elem  => Right,
                                          Name  => "id",
                                          Value => "0123");
         Assert (Condition => not Nodes_Equal (L => Left, R => Right),
                 Message   => "Nodes with different names reported equal");
      end Distinct_Node_Names;

      ----------------------------------------------------------------------

      procedure Distinct_Attributes
      is
         Dom_Impl    : DOM.Core.DOM_Implementation;
         Policy      : Muxml.XML_Data_Type;
         Left, Right : DOM.Core.Node;
         Correct     : Boolean := True;
      begin
         Policy.Doc := DOM.Core.Create_Document (Implementation => Dom_Impl);
         Left := DOM.Core.Documents.Create_Element
                    (Doc      => Policy.Doc,
                     Tag_Name => "memory");
         Right := DOM.Core.Documents.Create_Element
                    (Doc      => Policy.Doc,
                     Tag_Name => "memory");

         -- same name, L: "memory=''" vs no memory-attr but other attr. for R
         DOM.Core.Elements.Set_Attribute (Elem  => Left,
                                          Name  => "memory",
                                          Value => "");
         DOM.Core.Elements.Set_Attribute (Elem  => Right,
                                          Name  => "name",
                                          Value => "forBar");
         Correct := Correct and  not Nodes_Equal (L => Left, R => Right);

         -- one attr. differs in value
         DOM.Core.Elements.Set_Attribute (Elem  => Left,
                                          Name  => "name",
                                          Value => "0123");
         DOM.Core.Elements.Set_Attribute (Elem  => Right,
                                          Name  => "memory",
                                          Value => "");
         Correct := Correct and  not Nodes_Equal (L => Left, R => Right);

         -- R-node has additional attr.
         DOM.Core.Elements.Set_Attribute (Elem  => Left,
                                          Name  => "name",
                                          Value => "forBar");
         DOM.Core.Elements.Set_Attribute (Elem  => Right,
                                          Name  => "additionalAtrr",
                                          Value => "1");
         Correct := Correct and  not Nodes_Equal (L => Left, R => Right);

         Assert (Condition => Correct,
                 Message   => "Nodes with different attributes reported equal");
      end Distinct_Attributes;

   begin
      Positive_Test;
      Distinct_Types;
      Distinct_Node_Names;
      Distinct_Attributes;

--  begin read only
   end Test_Nodes_Equal;
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
end Mutools.Amend.Test_Data.Tests;
