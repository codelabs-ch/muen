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
   --  bin_split-spec.ads:28:4:Add_Fill_Entry
--  end read only

      pragma Unreferenced (Gnattest_T);

      Spec : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Spec,
                   Kind => Muxml.None,
                   File => "data/test_cspec.xml");

      Add_Fill_Entry
        (Spec            => Spec,
         Logical         => "section_name",
         Writable        => True,
         Executable      => False,
         Fill_Pattern    => 16#9090#,
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
   --  bin_split-spec.ads:40:4:Add_File_Entry
--  end read only

      pragma Unreferenced (Gnattest_T);

      Spec : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Spec,
                   Kind => Muxml.None,
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
