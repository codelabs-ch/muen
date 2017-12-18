--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Bin_Split.Spec.Test_Data.

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
package body Bin_Split.Spec.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Add_Fill_Entry (Gnattest_T : in out Test);
   procedure Test_Add_Fill_Entry_8b460f (Gnattest_T : in out Test) renames Test_Add_Fill_Entry;
--  id:2.2/8b460fbd51b295b2/Add_Fill_Entry/1/0/
   procedure Test_Add_Fill_Entry (Gnattest_T : in out Test) is
   --  bin_split-spec.ads:30:4:Add_Fill_Entry
--  end read only

      pragma Unreferenced (Gnattest_T);

      Spec : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Spec,
                   Kind => Muxml.Component,
                   File => "data/test_cspec.xml");

      Add_Fill_Entry
        (Spec            => Spec,
         Logical         => "section_name",
         Writable        => True,
         Executable      => False,
         Fill_Pattern    => 16#90#,
         Size            => 16#0700#,
         Virtual_Address => 16#0400#);

      Assert
        (Condition =>
           DOM.Core.Nodes.Length
             (DOM.Core.Documents.Get_Elements_By_Tag_Name
                (Doc => Spec.Doc,
                 Tag_Name => "fill"))
             > 0,
         Message   => "Fill entry not created");
--  begin read only
   end Test_Add_Fill_Entry;
--  end read only


--  begin read only
   procedure Test_Add_File_Entry (Gnattest_T : in out Test);
   procedure Test_Add_File_Entry_f30ecc (Gnattest_T : in out Test) renames Test_Add_File_Entry;
--  id:2.2/f30eccc8f250fb29/Add_File_Entry/1/0/
   procedure Test_Add_File_Entry (Gnattest_T : in out Test) is
   --  bin_split-spec.ads:42:4:Add_File_Entry
--  end read only

      pragma Unreferenced (Gnattest_T);

      Spec : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Spec,
                   Kind => Muxml.Component,
                   File => "data/test_cspec.xml");

      Add_File_Entry
        (Spec            => Spec,
         Logical         => "section_name",
         Writable        => True,
         Executable      => False,
         File_Name       => "test",
         Size            => 16#0700#,
         Virtual_Address => 16#0400#,
         Hash            => "3827eeabc");

      Assert
        (Condition =>
           DOM.Core.Nodes.Length
             (DOM.Core.Documents.Get_Elements_By_Tag_Name
                (Doc => Spec.Doc,
                 Tag_Name => "file")) > 0,
         Message   => "File entry not created");

      Assert
        (Condition =>
           DOM.Core.Nodes.Length
             (DOM.Core.Documents.Get_Elements_By_Tag_Name
                (Doc => Spec.Doc,
                 Tag_Name => "hash")) > 0,
         Message   => "Hash entry not created");
--  begin read only
   end Test_Add_File_Entry;
--  end read only


--  begin read only
   procedure Test_Create_Memory_Node (Gnattest_T : in out Test);
   procedure Test_Create_Memory_Node_8de05b (Gnattest_T : in out Test) renames Test_Create_Memory_Node;
--  id:2.2/8de05b00e8474ef3/Create_Memory_Node/1/0/
   procedure Test_Create_Memory_Node (Gnattest_T : in out Test) is
   --  bin_split-spec.ads:54:4:Create_Memory_Node
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Positive (Provides_Exists : Boolean)
      is
         use type DOM.Core.Node;

         Suffix : constant String
           := (if Provides_Exists then " (1)" else " (2)");

         Spec : Muxml.XML_Data_Type;
         Node : DOM.Core.Element;
      begin
         Muxml.Parse (Data => Spec,
                      Kind => Muxml.Component,
                      File => "data/test_cspec.xml");

         if not Provides_Exists then
            Muxml.Utils.Remove_Elements
              (Doc   => Spec.Doc,
               XPath => "/component/provides");
         end if;

         Node := Create_Memory_Node
           (Spec            => Spec,
            Logical         => "foobar",
            Writable        => True,
            Executable      => False,
            Size            => 16#abcd_0000#,
            Virtual_Address => 16#dead_beef#);

         Node := Muxml.Utils.Get_Element
           (Doc   => Spec.Doc,
            XPath => "/component/provides/memory[@logical='foobar']");

         Assert (Condition => Node /= null,
                 Message   => "Memory node not created" & Suffix);

         Assert (Condition => DOM.Core.Elements.Get_Attribute
                 (Elem => Node,
                  Name => "logical") = "foobar",
                 Message   => "Logical name mismatch" & Suffix);

         Assert (Condition => DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "writable") = "true",
                 Message   => "Writable mismatch" & Suffix);

         Assert (Condition => DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "executable") = "false",
                 Message   => "Executable mismatch" & Suffix);

         Assert (Condition => DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "size") = "16#abcd_0000#",
                 Message   => "Size mismatch" & Suffix);

         Assert (Condition => DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "virtualAddress") = "16#dead_beef#",
                 Message   => "Virtual address mismatch" & Suffix);

         Assert (Condition => DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "type") = "subject_binary",
                 Message   => "Type mismatch" & Suffix);

         Assert (Condition =>
                   DOM.Core.Nodes.Length
                     (Map => DOM.Core.Nodes.Attributes (N => Node)) = 6,
                 Message   => "Wrong number of attributes" & Suffix);
      end Positive;
   begin
      Positive (Provides_Exists => True);
      Positive (Provides_Exists => False);
--  begin read only
   end Test_Create_Memory_Node;
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
end Bin_Split.Spec.Test_Data.Tests;
