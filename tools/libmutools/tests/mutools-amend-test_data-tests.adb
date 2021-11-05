--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Amend.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;
with Ada.Exceptions;
with Ada.Directories;

with DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Elements;
with Muxml.Utils;
with Muxml;
with Test_Utils;


--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

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
   procedure Test_Expand_150aa9 (Gnattest_T : in out Test) renames Test_Expand;
--  id:2.2/150aa91f5cdabaeb/Expand/1/0/
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

      procedure XPath_Not_Unique
      is
         Data   : Muxml.XML_Data_Type;
         Node   : DOM.Core.Node;
         Output : constant String := "obj/output_amend_expand.xml";
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/amend_expand.xml");
         Node := Muxml.Utils.Get_Element
                   (Doc   => Data.Doc,
                    XPath => "/system/amend[@xpath='/']");
         DOM.Core.Elements.Set_Attribute
            (Elem  => Node,
             Name  => "xpath",
             Value => "/system/platform/mappings/classes/class/device");

         begin
            Expand (XML_Data => Data);
            Assert (Condition => False,
                    Message   => "Exception expected");
         exception
            when E : Muxml.Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "No unique node for XPath "
                       & "/system/platform/mappings/classes/class/device"
                       &  " in amend statement.",
                    Message   => "Exception message mismatch");
         end;

      end XPath_Not_Unique;

   begin
      Positive_Test;
      XPath_Not_Unique;

--  begin read only
   end Test_Expand;
--  end read only


--  begin read only
   procedure Test_Recursive_Merge (Gnattest_T : in out Test);
   procedure Test_Recursive_Merge_c8dad1 (Gnattest_T : in out Test) renames Test_Recursive_Merge;
--  id:2.2/c8dad126d0ec91d5/Recursive_Merge/1/0/
   procedure Test_Recursive_Merge (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data   : Muxml.XML_Data_Type;
      Main, Child1, Child2 : DOM.Core.Node;
      Output : constant String := "obj/output_amend.xml";

   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.None,
                   File => "data/amend_recursive_merge.xml");
      Main := Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/");
      Child1 := Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/system/child1/system");
      Child2 := Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/system/child2/session");
      Recursive_Merge (Parent    => Main,
                       New_Child => Child1);

      Main := Muxml.Utils.Get_Element
                (Doc   => Data.Doc,
                 XPath => "/system/hardware");
      Recursive_Merge (Parent    => Main,
                       New_Child => Child2);
      Muxml.Write (Data => Data,
                   Kind => Muxml.None,
                   File => Output);
      Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/output_amend.xml",
                  Filename2 => Output),
              Message   => "Policy mismatch: " & Output);

      Ada.Directories.Delete_File (Name => Output);

--  begin read only
   end Test_Recursive_Merge;
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

      ---------------------------------------------------------------------

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

      ---------------------------------------------------------------------

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

      ---------------------------------------------------------------------

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
