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
   procedure Test_Add_Fill_Entry_037de5 (Gnattest_T : in out Test) renames Test_Add_Fill_Entry;
--  id:2.2/037de52ddc72e8bd/Add_Fill_Entry/1/0/
   procedure Test_Add_Fill_Entry (Gnattest_T : in out Test) is
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
   procedure Test_Add_File_Entry_b74ecf (Gnattest_T : in out Test) renames Test_Add_File_Entry;
--  id:2.2/b74ecf0e5078d38a/Add_File_Entry/1/0/
   procedure Test_Add_File_Entry (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Add_Entry
      is
         Filename : constant String := "test";
         Hash     : constant String := "3827eeabc";

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
            File_Name       => Filename,
            Size            => 16#0700#,
            Virtual_Address => 16#0400#,
            Hash            => Hash);

         declare
            use type DOM.Core.Node;

            File_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Spec.Doc,
                 XPath => "/component/provides/memory"
                 & "[@logical='section_name']/file");
            Hash_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Spec.Doc,
                 XPath => "/component/provides/memory"
                 & "[@logical='section_name']/hash");
         begin
            Assert (Condition => File_Node /= null,
                    Message   => "File entry not created");
            Assert (Condition => DOM.Core.Elements.Get_Attribute
                    (Elem => File_Node,
                     Name => "filename") = Filename,
                    Message   => "Filename mismatch");
            Assert (Condition => DOM.Core.Elements.Get_Attribute
                    (Elem => File_Node,
                     Name => "offset") = "none",
                    Message   => "File offset mismatch");

            Assert (Condition => Hash_Node /= null,
                    Message   => "Hash entry not created");
            Assert (Condition => DOM.Core.Elements.Get_Attribute
                    (Elem => Hash_Node,
                     Name => "value") = Hash,
                    Message   => "Hash value mismatch");
         end;
      end Add_Entry;

      ----------------------------------------------------------------------

      procedure Add_Entry_Reloadable
      is
         Filename : constant String := "test";
         Hash     : constant String := "3827eeabc";
         Size     : constant := 16#7000#;
         VAddr    : constant := 16#0001_0000#;

         Spec : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Spec,
                      Kind => Muxml.Component,
                      File => "data/test_cspec.xml");

         Add_File_Entry
           (Spec            => Spec,
            Logical         => "reload",
            Writable        => True,
            Executable      => False,
            File_Name       => Filename,
            Size            => Size,
            Virtual_Address => VAddr,
            Hash            => Hash,
            Reloadable      => True);

         declare
            use type Interfaces.Unsigned_64;
            use type DOM.Core.Node;

            Src_Mem : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Spec.Doc,
                 XPath => "/component/provides/memory"
                 & "[@logical='reload_src']");
            Src_File_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Src_Mem,
                 XPath => "file");
            Src_Hash_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Src_Mem,
                 XPath => "hash");
            Dst_Mem       : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Spec.Doc,
                 XPath => "/component/provides/memory"
                 & "[@logical='reload']");
            Dst_File_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Dst_Mem,
                 XPath => "file");
            Dst_Hash_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Dst_Mem,
                 XPath => "hash");
         begin
            Assert (Condition => Src_Mem /= null,
                    Message   => "Source memory region not created");
            Assert (Condition => not Boolean'Value
                    (DOM.Core.Elements.Get_Attribute
                       (Elem => Src_Mem,
                        Name => "writable")),
                    Message   => "Source memory region is writable");
            Assert (Condition => Interfaces.Unsigned_64'Value
                    (DOM.Core.Elements.Get_Attribute
                       (Elem => Src_Mem,
                        Name => "size")) = Size,
                    Message   => "Source memory region size mismatch");
            Assert (Condition => Interfaces.Unsigned_64'Value
                    (DOM.Core.Elements.Get_Attribute
                       (Elem => Src_Mem,
                        Name => "virtualAddress"))
                    = VAddr + 16#ffff_8000_0000_0000#,
                    Message   => "Source memory region address mismatch");

            Assert (Condition => Src_File_Node /= null,
                    Message   => "Source file entry not created");
            Assert (Condition => DOM.Core.Elements.Get_Attribute
                    (Elem => Src_File_Node,
                     Name => "filename") = Filename,
                    Message   => "Source filename mismatch");
            Assert (Condition => DOM.Core.Elements.Get_Attribute
                    (Elem => Src_File_Node,
                     Name => "offset") = "none",
                    Message   => "Source file offset mismatch");
            Assert (Condition => Src_Hash_Node /= null,
                    Message   => "Source hash entry not created");
            Assert (Condition => DOM.Core.Elements.Get_Attribute
                    (Elem => Src_Hash_Node,
                     Name => "value") = Hash,
                    Message   => "Source hash value mismatch");

            Assert (Condition => Dst_Mem /= null,
                    Message   => "Dst memory region not created");
            Assert (Condition => Boolean'Value
                    (DOM.Core.Elements.Get_Attribute
                       (Elem => Dst_Mem,
                        Name => "writable")),
                    Message   => "Dst memory region is not writable");
            Assert (Condition => Interfaces.Unsigned_64'Value
                    (DOM.Core.Elements.Get_Attribute
                       (Elem => Dst_Mem,
                        Name => "size")) = Size,
                    Message   => "Dst memory region size mismatch");
            Assert (Condition => Interfaces.Unsigned_64'Value
                    (DOM.Core.Elements.Get_Attribute
                       (Elem => Dst_Mem,
                        Name => "virtualAddress")) = VAddr,
                    Message   => "Dst memory region address mismatch");
            Assert (Condition => Dst_File_Node = null,
                    Message   => "Dst file entry created");
            Assert (Condition => Dst_Hash_Node /= null,
                    Message   => "Dst hash entry not created");
            Assert (Condition => DOM.Core.Elements.Get_Attribute
                    (Elem => Dst_Hash_Node,
                     Name => "value") = Hash,
                    Message   => "Dst hash value mismatch");
         end;
      end Add_Entry_Reloadable;
   begin
      Add_Entry;
      Add_Entry_Reloadable;
--  begin read only
   end Test_Add_File_Entry;
--  end read only


--  begin read only
   procedure Test_Set_RIP (Gnattest_T : in out Test);
   procedure Test_Set_RIP_e793c2 (Gnattest_T : in out Test) renames Test_Set_RIP;
--  id:2.2/e793c22e421e5ffd/Set_RIP/1/0/
   procedure Test_Set_RIP (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      Ref_1 : constant Interfaces.Unsigned_64 := 16#cafe_beef#;
      Ref_2 : constant Interfaces.Unsigned_64 := 16#9090_4242#;
      Spec    : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Spec,
                   Kind => Muxml.Component,
                   File => "data/test_cspec.xml");
      Set_RIP (Spec        => Spec,
               Entry_Point => Ref_1);
      Assert (Condition => Interfaces.Unsigned_64'Value
              (Muxml.Utils.Get_Element_Value
                 (Doc   => Spec.Doc,
                  XPath => "/component/requires/vcpu/registers/gpr/rip"))
              = Ref_1,
              Message   => "RIP value mismatch (1)");

      --  Test setting RIP with existing XML elements.

      Set_RIP (Spec        => Spec,
               Entry_Point => Ref_2);
      Assert (Condition => Interfaces.Unsigned_64'Value
              (Muxml.Utils.Get_Element_Value
                 (Doc   => Spec.Doc,
                  XPath => "/component/requires/vcpu/registers/gpr/rip"))
              = Ref_2,
              Message   => "RIP value mismatch (2)");
--  begin read only
   end Test_Set_RIP;
--  end read only


--  begin read only
   procedure Test_Create_Memory_Node (Gnattest_T : in out Test);
   procedure Test_Create_Memory_Node_8de05b (Gnattest_T : in out Test) renames Test_Create_Memory_Node;
--  id:2.2/8de05b00e8474ef3/Create_Memory_Node/1/0/
   procedure Test_Create_Memory_Node (Gnattest_T : in out Test) is
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
