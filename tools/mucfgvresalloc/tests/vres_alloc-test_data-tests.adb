--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Vres_Alloc.Test_Data.

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

with Test_Utils;

with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;

with Mutools.OS;
with Mutools.Vres_Alloc;

with Muxml;
with Muxml.Utils;

--  begin read only
--  end read only
package body Vres_Alloc.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_e2e013 (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/e2e013a62d62a1e1/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      --  Compare the diff between Original_File and New_File
      --  to the reference diff Ref_Diff and return true if and only if they are
      --  equal.
      function Diff_Equal
        (Original_File : String;
         New_File      : String;
         Diff_Ref      : String)
        return Boolean
      is
         Diff_Filename        : constant String
           := New_File & ".diff";
         Create_Diff_File_Cmd : constant String
           :=  "diff -w -B " & Original_File & " " & New_File
           & " > " & Diff_Filename & " || true";
      begin
         Mutools.OS.Execute (Command => Create_Diff_File_Cmd);
         return Test_Utils.Equal_Files
           (Filename1 => Diff_Filename,
            Filename2 => Diff_Ref);
      end Diff_Equal;

      ----------------------------------------------------------------------

      procedure Test
        (Input_Policy         : String;
         Input_Default_Folder : Boolean := True;
         Output_Filename      : String;
         Diff_Ref_File        : String;
         Test_Name            : String)
      is
         Output_Dir     : constant String
           := "obj/outdir";
         Output_Path    : constant String
           := Output_Dir & "/" & Output_Filename;
         Input_Path     : constant String
           := (if Input_Default_Folder then "data/" else "")
             & Input_Policy;
       Diff_File_Path : constant String
           := (if Input_Default_Folder then "data/" else "")
             & Diff_Ref_File;
      begin
         Mutools.Expressions.Name_To_String_Hashed_Map.Clear
           (Container => Memory_Sizes);
         Mutools.Expressions.Name_To_String_Hashed_Map.Clear
           (Container => Channel_Sizes);
         Component_To_Map_Package.Clear
           (Container => Components_Map);

         Run (Policy_File_Name => Input_Path,
              Output_File_Name => Output_Path);

         Assert (Condition => Ada.Directories.Exists (Name => Output_Dir),
                 Message   => "Directory not created in test '"
                   & Test_Name & "'");
         Assert (Condition => Diff_Equal
                   (Original_File => Input_Path,
                    New_File      => Output_Path,
                    Diff_Ref      => Diff_File_Path),
                 Message   => "File mismatch in test '"
                   & Test_Name & "'");

         Ada.Directories.Delete_Tree (Directory => Output_Dir);
      end Test;
   begin
      --  Fix a domain configuration for the unittests.
      --  Otherwise the tests fail if the domains in Mutools.Vres_Alloc.Config
      --  change.
      Va_Space_Native       := (First_Element => 16#0000_0000_2000_0000#,
                                Last_Element  => 16#0000_0007_FFFF_FFFF#);
      Va_Space_Vm           := (First_Element => 16#0000_0010_0000_0000#,
                                Last_Element  => 16#0000_001F_FFFF_FFFF#);
      Vector_Numbers_Domain := (First_Element => 0,
                                Last_Element  => 255);
      Event_Numbers_Domain  := (First_Element => 0,
                                Last_Element  => 63);

      --  Positive test: No resources need allocation
      Test (Input_Policy    => "policy_no_autoalloc.xml",
            Output_Filename => "output_policy_no_autoalloc.xml",
            Diff_Ref_File   => "empty_diff.diff",
            Test_Name       => "No allocation necessary");

      --  Positive test: This test intends to present a 'general case' where
      --  all features are needed, i.e., some resources are fixed in the
      --  subject, some are fixed by the component, some need to be allocated
      --  from the rest of the domain.
      Test (Input_Policy    => "policy_allocation.xml",
            Output_Filename => "output_policy_allocation.xml",
            Diff_Ref_File   => "output_policy_allocation.xml.diff",
            Test_Name       => "Allocation of all resources");

      --  Negative tests:
      --  Referenced component not found.
      declare
         Data         : Muxml.XML_Data_Type;
         Changed_Spec : constant String
           := "obj/vres_component_not_found.xml";
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/policy_allocation.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='subject2']/component",
            Name  => "ref",
            Value => "no_such_component");
         Muxml.Write
           (File => Changed_Spec,
            Kind => Muxml.None,
            Data => Data);

         Test (Input_Policy         => Changed_Spec,
               Input_Default_Folder => False,
               Output_Filename      => "output_policy_fail.xml",
               Diff_Ref_File        => "empty_diff.diff",
               Test_Name            => "Exception: component not found");
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E: Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Cannot find component with name 'no_such_component'",
                    Message   => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
            Ada.Directories.Delete_File (Name => Changed_Spec);
      end;

      --  Read-only resource with value "auto".
      declare
         Data         : Muxml.XML_Data_Type;
         Changed_Spec : constant String
           := "obj/vres_false_auto.xml";
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/policy_allocation.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='subject2']/events/source/"
              & "group[@name='vmcall']/event[@id='5']",
            Name  => "id",
            Value => "auto");
         Muxml.Write
           (File => Changed_Spec,
            Kind => Muxml.None,
            Data => Data);

         Test (Input_Policy         => Changed_Spec,
               Input_Default_Folder => False,
               Output_Filename      => "output_policy_fail.xml",
               Diff_Ref_File        => "empty_diff.diff",
               Test_Name            => "Exception: read-only with 'auto'");
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E: Muxml.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "XML validation error - obj/vres_false_auto.xml:416:14:"
                      & " Invalid integer: ""auto""",
                    Message   => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
            Ada.Directories.Delete_File (Name => Changed_Spec);
      end;

      --  Not enough space for event resource.
      declare
         Data          : Muxml.XML_Data_Type;
         Changed_Spec  : constant String
           := "obj/vres_false_auto.xml";
         Parent, Child : DOM.Core.Node;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/policy_allocation.xml");
         Parent := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='lnx']/channels");
         for I in 0 .. 63 loop
            Child := DOM.Core.Documents.Create_Element
              (Doc      => Data.Doc,
               Tag_Name => "writer");
            DOM.Core.Elements.Set_Attribute
              (Elem  => Child,
               Name  => "logical",
               Value => "automatic_node" & I'Image);
            DOM.Core.Elements.Set_Attribute
              (Elem  => Child,
               Name  => "event",
               Value => "auto");
            DOM.Core.Elements.Set_Attribute
              (Elem  => Child,
               Name  => "physical",
               Value => "data_channel");
            Child := DOM.Core.Nodes.Append_Child
              (N         => Parent,
               New_Child => Child);
         end loop;
         Muxml.Write
           (File => Changed_Spec,
            Kind => Muxml.None,
            Data => Data);

         Test (Input_Policy         => Changed_Spec,
               Input_Default_Folder => False,
               Output_Filename      => "output_policy_fail.xml",
               Diff_Ref_File        => "empty_diff.diff",
               Test_Name            => "Exception: not enough space for events");
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E: Mutools.Intervals.Out_Of_Space =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Cannot find free interval of size '16#0001#'",
                    Message   => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
            Ada.Directories.Delete_File (Name => Changed_Spec);
      end;

      --  Channel is not declared.
      declare
         Data         : Muxml.XML_Data_Type;
         Changed_Spec : constant String
           := "obj/vres_false_auto.xml";
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/policy_allocation.xml");
         Muxml.Utils.Remove_Elements
           (Doc   => Data.Doc,
            XPath => "/system/channels/channel[@name='data_channel2']");
         Muxml.Write
           (File => Changed_Spec,
            Kind => Muxml.None,
            Data => Data);

         Test (Input_Policy         => Changed_Spec,
               Input_Default_Folder => False,
               Output_Filename      => "output_policy_fail.xml",
               Diff_Ref_File        => "empty_diff.diff",
               Test_Name            => "Exception: Channel not declared");
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E: Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Cannot find size of node at "
                      & "'/#document/system/subjects/subject/channels/reader' "
                      & "with physical 'data_channel2'",
                    Message   => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
            Ada.Directories.Delete_File (Name => Changed_Spec);
      end;

      --  Memory not declared.
      declare
         Data         : Muxml.XML_Data_Type;
         Changed_Spec : constant String
           := "obj/vres_false_auto.xml";
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/policy_allocation.xml");
         Muxml.Utils.Remove_Elements
           (Doc   => Data.Doc,
            XPath => "/system/memory/memory[@name='dummy_1000_']");
         Muxml.Write
           (File => Changed_Spec,
            Kind => Muxml.None,
            Data => Data);
         Test (Input_Policy         => Changed_Spec,
               Input_Default_Folder => False,
               Output_Filename      => "output_policy_fail.xml",
               Diff_Ref_File        => "empty_diff.diff",
               Test_Name            => "Exception: Memory not declared");
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E: Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Cannot find size of node at "
                      & "'/#document/system/subjects/subject/memory/memory' "
                      & "with physical 'dummy_1000_'",
                    Message   => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
            Ada.Directories.Delete_File (Name => Changed_Spec);
      end;

      --  Same logical (not in array) in component mapped twice with different
      --  values.
      declare
         Data               : Muxml.XML_Data_Type;
         Changed_Spec       : constant String
           := "obj/vres_false_auto.xml";
         New_Node           : DOM.Core.Node;
         Parent_Xpath       : constant String
           := "/system/components/component[@name='c1']/requires/memory";

      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/policy_allocation.xml");
         New_Node := DOM.Core.Nodes.Clone_Node
           (N    =>  Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => Parent_Xpath & "/memory[@logical='ml1']"),
            Deep => True);
         DOM.Core.Elements.Set_Attribute
           (Elem  => New_Node,
            Name  => "virtualAddress",
            Value => "16#4444_0000#");
         New_Node := DOM.Core.Nodes.Append_Child
           (N         => Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => Parent_Xpath),
            New_Child => New_Node);
         Muxml.Write
           (File => Changed_Spec,
            Kind => Muxml.None,
            Data => Data);

         Test (Input_Policy         => Changed_Spec,
               Input_Default_Folder => False,
               Output_Filename      => "output_policy_fail.xml",
               Diff_Ref_File        => "empty_diff.diff",
               Test_Name            => "Exception: Same logical not array");
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E: Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Conflicting resource values for logical 'ml1': "
                      & "found '(First_Address=16#0002_0000#, Size=536870912)' "
                      & "and '(First_Address=16#4444_0000#, Size=536870912)'",
                    Message   => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
            Ada.Directories.Delete_File (Name => Changed_Spec);
      end;

      --  Same logical in array in component mapped twice with different
      --  values.
      declare
         Data               : Muxml.XML_Data_Type;
         Changed_Spec       : constant String
           := "obj/vres_false_auto.xml";
         New_Node           : DOM.Core.Node;
         Parent_Xpath       : constant String
           := "/system/components/component[@name='c1']/requires/memory";

      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/policy_allocation.xml");
         New_Node := DOM.Core.Nodes.Clone_Node
           (N    =>  Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => Parent_Xpath & "/array[@logical='mal3']"),
            Deep => True);
         DOM.Core.Elements.Set_Attribute
           (Elem  => New_Node,
            Name  => "virtualAddressBase",
            Value => "16#4444_0000#");
         New_Node := DOM.Core.Nodes.Append_Child
           (N         => Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => Parent_Xpath),
            New_Child => New_Node);
         Muxml.Write
           (File => Changed_Spec,
            Kind => Muxml.None,
            Data => Data);

         Test (Input_Policy         => Changed_Spec,
               Input_Default_Folder => False,
               Output_Filename      => "output_policy_fail.xml",
               Diff_Ref_File        => "empty_diff.diff",
               Test_Name            => "Exception: Same logical in array");
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E: Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Conflicting resource values for logical 'mem11': "
                      & "found '(First_Address=16#3087_3000#, Size=4096)' "
                      & "and '(First_Address=16#4444_0000#, Size=4096)'",
                    Message   => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
            Ada.Directories.Delete_File (Name => Changed_Spec);
      end;


      --  Alignment problems:
      declare
         Data         : Muxml.XML_Data_Type;
         Changed_Spec : constant String
           := "obj/vres_false_auto.xml";

         procedure Test_Alignment_Failure (XPath, Attr_Name, Value : String)
         is
         begin
            Muxml.Parse (Data => Data,
                         Kind => Muxml.None,
                         File => "data/policy_allocation.xml");
            Muxml.Utils.Set_Attribute
              (Doc   => Data.Doc,
               XPath => XPath,
               Name  => Attr_Name,
               Value => Value);
            Muxml.Write
              (File => Changed_Spec,
               Kind => Muxml.None,
               Data => Data);

            Test (Input_Policy         => Changed_Spec,
                  Input_Default_Folder => False,
                  Output_Filename      => "output_policy_fail.xml",
                  Diff_Ref_File        => "empty_diff.diff",
                  Test_Name            => "Exception: Alignment in component");
            Assert (Condition => False,
                    Message   => "Exception expected");
         exception
            when E: Validation_Error =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                         = "Virtual resource not aligned",
                       Message   => "Exception mismatch: "
                         & Ada.Exceptions.Exception_Message (X => E));
               Ada.Directories.Delete_File (Name => Changed_Spec);
         end Test_Alignment_Failure;
      begin
         --  Address fixed in component is not aligned.
         Test_Alignment_Failure
           (XPath     => "/system/components/component[@name='c1']/requires/"
              & "channels/reader[@logical='crl0']",
            Attr_Name => "virtualAddress",
            Value     => "16#0010_0001#");

         --  Size fixed in component is not aligned.
         Test_Alignment_Failure
           (XPath     => "/system/components/component[@name='c1']/requires/"
              & "channels/reader[@logical='crl0']",
            Attr_Name => "size",
            Value     => "16#0001_0f00#");

         --  Unused size in component is not aligned.
         Test_Alignment_Failure
           (XPath     => "/system/components/component[@name='c1']/requires/"
              & "memory/memory[@logical='ml1']",
            Attr_Name => "size",
            Value     => "16#0001#");

         --  Set address in subject not aligned.
         Test_Alignment_Failure
           (XPath     => "/system/subjects/subject[@name='subject2']/memory/"
              & "memory[@logical='ml4']",
            Attr_Name => "virtualAddress",
            Value     => "16#3003_0040#");
      end;

--  begin read only
   end Test_Run;
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
end Vres_Alloc.Test_Data.Tests;
