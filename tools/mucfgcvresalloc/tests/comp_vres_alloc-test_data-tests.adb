--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Comp_Vres_Alloc.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only


with Ada.Directories;
with Ada.Exceptions;

with Interfaces;

with Test_Utils;

with DOM.Core.Elements;
with DOM.Core.Nodes;
with DOM.Core.Documents;

with Muxml;
with Muxml.Utils;

with Mutools.Utils;
with Mutools.Vres_Alloc;
with Mutools.Vres_Alloc.Config;

--  begin read only
--  end read only
package body Comp_Vres_Alloc.Test_Data.Tests is

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
   procedure Test_Run_e5a2dd (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/e5a2dd86b12d7902/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Test
        (Input_Spec                : String;
         Input_Spec_Default_Folder : Boolean := true;
         Include_Path              : String;
         Output_File_Name          : String;
         Output_Ref_File           : String;
         Test_Name                 : String)
      is
         Output_Dir  : constant String := "obj/outdir";
         Output_Path : constant String
           := Output_Dir & "/" & Output_File_Name;
         Input_Path  : constant String
           := (if Input_Spec_Default_Folder then "data/" else "") & Input_Spec;

      begin
         Run (Input_Spec       => Input_Path,
              Include_Path     => Include_Path,
              Output_File_Name => Output_Path);

         Assert (Condition => Ada.Directories.Exists (Name => Output_Dir),
                 Message   => "Directory not created in test '"
                   & Test_Name & "'");
         Assert (Condition => Test_Utils.Equal_Files
                   (Filename1 => Output_Path,
                    Filename2 => "data/" & Output_Ref_File),
                 Message   => "File mismatch in test '"
                   & Test_Name & "'");
         Ada.Directories.Delete_Tree (Directory => Output_Dir);
      end Test;

   begin
      --  Fix domain configuration for the unittests.
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

      --  No actions necessary (output should equal input)
      Test (Input_Spec       => "component_vt.xml",
            Include_Path     => "",
            Output_File_Name => "output_component_vt.xml",
            Output_Ref_File  => "component_vt.xml",
            Test_Name        => "No_Change");

      --  Library without auto-allocation but with expressions, if,
      --  conditionals, substitutions
      Test (Input_Spec       => "library_debug.xml",
            Include_Path     => "",
            Output_File_Name => "output_library_debug.xml",
            Output_Ref_File  => "output_library_debug.xml",
            Test_Name        => "Library");

      --  Test XInclude mechanism
      Test (Input_Spec       => "component_inc.xml",
            Include_Path     => "data/incdir",
            Output_File_Name => "output_component_inc.xml",
            Output_Ref_File  => "output_component_inc.xml",
            Test_Name        => "Includes");

      --  Test evaluation of conditionals und substitutions
      Test (Input_Spec       => "component_cond.xml",
            Include_Path     => "",
            Output_File_Name => "output_component_cond.xml",
            Output_Ref_File  => "output_component_cond.xml",
            Test_Name        => "Conditionals");

      --  Case where all features regarding automatic allocation are needed.
      --  For all virtual resources, some values are given and some must be set
      --  and the free ranges have holes that get filled.
      Test (Input_Spec       => "component_vres.xml",
            Include_Path     => "",
            Output_File_Name => "output_component_vres.xml",
            Output_Ref_File  => "output_component_vres.xml",
            Test_Name        => "Virtual Resources");

      --  Negative Tests:
      --  Vector in read-only path set to 'auto'
      declare
         Data         : Muxml.XML_Data_Type;
         Changed_Spec : constant String
           := "obj/component_vres_false_auto.xml";
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/component_vres.xml");
         Muxml.Utils.Set_Attribute
           (Doc => Data.Doc,
            XPath => "/component/requires/events/target/"
              & "event[@logical='et2']/inject_interrupt",
            Name  => "vector",
            Value => "auto");
         Muxml.Write
           (File => Changed_Spec,
            Kind => Muxml.None,
            Data => Data);

         Test (Input_Spec                => Changed_Spec,
               Input_Spec_Default_Folder => False,
               Include_Path              => "",
               Output_File_Name          => "output_component_vres.xml",
               Output_Ref_File           => "output_component_vres.xml",
               Test_Name                 => "False auto in vector");
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E: Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Invalid attribute value",
                    Message   => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
            Ada.Directories.Delete_File (Name => Changed_Spec);
      end;

      --  Id in read-only event set to auto
      declare
         Data         : Muxml.XML_Data_Type;
         Changed_Spec : constant String
           := "obj/component_vres_false_auto.xml";
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/component_vres.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/component/requires/events/source/"
              & "event[@logical='es2']",
            Name  => "id",
            Value => "auto");
         Muxml.Write
           (File => Changed_Spec,
            Kind => Muxml.None,
            Data => Data);

         Test (Input_Spec                => Changed_Spec,
               Input_Spec_Default_Folder => False,
               Include_Path              => "",
               Output_File_Name          => "output_component_vres.xml",
               Output_Ref_File           => "output_component_vres.xml",
               Test_Name                 => "False auto in event");
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E: Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Invalid attribute value",
                    Message   => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
            Ada.Directories.Delete_File (Name => Changed_Spec);
      end;

      --  Not enough space for virtual addresses
      declare
         use type Interfaces.Unsigned_64;

         Data               : Muxml.XML_Data_Type;
         Changed_Spec       : constant String
           := "obj/component_vres_va_space_full.xml";
         Start_Ml7          : constant Interfaces.Unsigned_64
           := 16#4000_0000#;
         Threshold          : constant Interfaces.Unsigned_64
           := Mutools.Vres_Alloc.Config.Default_Va_Space_Native.Last_Element;
         --  We want Start_Ml7 + Size_To_Fill_Space >=  Threshold
         Size_To_Fill_Space : constant Interfaces.Unsigned_64
           := (if Threshold > Start_Ml7 then (Threshold - Start_Ml7 + 1)
                                        else Interfaces.Unsigned_64'(1));
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/component_vres.xml");

         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/component/requires/memory/memory[@logical='ml7']",
            Name  => "size",
            Value => Mutools.Utils.To_Hex (Number => Size_To_Fill_Space));
         Muxml.Write
           (File => Changed_Spec,
            Kind => Muxml.None,
            Data => Data);

         Test (Input_Spec                => Changed_Spec,
               Input_Spec_Default_Folder => False,
               Include_Path              => "",
               Output_File_Name          => "output_component_vres.xml",
               Output_Ref_File           => "output_component_vres.xml",
               Test_Name                 => "Va space full");
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E: Mutools.Intervals.Out_Of_Space =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Cannot find free interval of size "
                      & "'16#1000_1000#'",
                    Message  => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
            Ada.Directories.Delete_File (Name => Changed_Spec);
      end;

      --  Not enough space for event ids
      declare
         use type Interfaces.Unsigned_64;

         Data          : Muxml.XML_Data_Type;
         Changed_Spec  : constant String
           := "obj/component_vres_event_space_full.xml";
         Parent, Child : DOM.Core.Node;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/component_vres.xml");

         Parent := Muxml.Utils.Get_Element
           (Doc => Data.Doc,
            XPath => "/component/requires/channels");
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
               Name  => "size",
               Value => "16#1000#");
            Child := DOM.Core.Nodes.Append_Child (N         => Parent,
                                                  New_Child => Child);
         end loop;
         Muxml.Write
           (File => Changed_Spec,
            Kind => Muxml.None,
            Data => Data);

         Test (Input_Spec                => Changed_Spec,
               Input_Spec_Default_Folder => False,
               Include_Path              => "",
               Output_File_Name          => "output_component_vres.xml",
               Output_Ref_File           => "output_component_vres.xml",
               Test_Name                 => "Event space full");
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E: Mutools.Intervals.Out_Of_Space =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "Cannot find free interval of size "
                      & "'16#0001#'",
                    Message   => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
            Ada.Directories.Delete_File (Name => Changed_Spec);
      end;

      --  Library: Run must not change anything in a library component
      --  Hence, there must be a validation error if autoallocation is
      --  requested.
      declare
         use type Interfaces.Unsigned_64;

         Data         : Muxml.XML_Data_Type;
         Changed_Spec : constant String
           := "obj/library_debug_with_auto.xml";
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/library_debug.xml");

         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/library/requires/channels/reader[@logical='crl0']",
            Name  => "vector",
            Value => "auto");
         Muxml.Write
           (File => Changed_Spec,
            Kind => Muxml.None,
            Data => Data);

         Test (Input_Spec                => Changed_Spec,
               Input_Spec_Default_Folder => False,
               Include_Path              => "",
               Output_File_Name          => "output_component_vres.xml",
               Output_Ref_File           => "output_component_vres.xml",
               Test_Name                 => "Event space full");
         Assert (Condition => False,
                 Message   => "Exception expected");
      exception
         when E: Muxml.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                      = "XML validation error - "
                      & "obj/outdir/output_component_vres.xml:16:48: "
                      & "Invalid integer: ""auto""",
                    Message  => "Exception mismatch: "
                      & Ada.Exceptions.Exception_Message (X => E));
            Ada.Directories.Delete_File (Name => Changed_Spec);
      end;

      --  Alignment problems:
      declare
         Data         : Muxml.XML_Data_Type;
         Changed_Spec : constant String
           := "obj/component_vres_false_auto.xml";

         procedure Test_Alignment_Failure (XPath, Attr_Name, Value : String)
         is
         begin
            Muxml.Parse (Data => Data,
                         Kind => Muxml.None,
                         File => "data/component_vres.xml");
            Muxml.Utils.Set_Attribute
              (Doc   => Data.Doc,
               XPath => XPath,
               Name  => Attr_Name,
               Value => Value);
            Muxml.Write
              (File => Changed_Spec,
               Kind => Muxml.None,
               Data => Data);

            Test (Input_Spec                => Changed_Spec,
                  Input_Spec_Default_Folder => False,
                  Include_Path              => "",
                  Output_File_Name          => "output_component_vres.xml",
                  Output_Ref_File           => "output_component_vres.xml",
                  Test_Name                 => "Alignment Failure");
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
         --  An address is set but not aligned
         Test_Alignment_Failure
           (XPath     => "/component/requires/memory/"
              & "memory[@logical='ml1']",
            Attr_Name => "virtualAddress",
            Value     => "16#0002_0222#");

         --  The virtualAddressBase of an array is not aligned
         Test_Alignment_Failure
           (XPath     => "/component/requires/memory/"
              & "array[@logical='mal2']",
            Attr_Name => "virtualAddressBase",
            Value     => "16#3004_5100#");

         --  The elementSize in an array without baseAddress is not aligned
         Test_Alignment_Failure
           (XPath     => "/component/requires/memory/"
              & "array[@logical='mal3']",
            Attr_Name => "elementSize",
            Value     => "16#0010_100b#");
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
end Comp_Vres_Alloc.Test_Data.Tests;
