--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Match.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Mutools.Match.Test_Data.Tests is


--  begin read only
   procedure Test_Is_Valid_Reference (Gnattest_T : in out Test);
   procedure Test_Is_Valid_Reference_f00842 (Gnattest_T : in out Test) renames Test_Is_Valid_Reference;
--  id:2.2/f008425ad8c5c86b/Is_Valid_Reference/1/0/
   procedure Test_Is_Valid_Reference (Gnattest_T : in out Test) is
   --  mutools-match.ads:26:4:Is_Valid_Reference
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
   procedure Test_Is_Valid_Reference_Lparent (Gnattest_T : in out Test);
   procedure Test_Is_Valid_Reference_Lparent_f5dc41 (Gnattest_T : in out Test) renames Test_Is_Valid_Reference_Lparent;
--  id:2.2/f5dc4151d251e127/Is_Valid_Reference_Lparent/1/0/
   procedure Test_Is_Valid_Reference_Lparent (Gnattest_T : in out Test) is
   --  mutools-match.ads:30:4:Is_Valid_Reference_Lparent
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data        : Muxml.XML_Data_Type;
      Impl        : DOM.Core.DOM_Implementation;
      Left, Right : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Left := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "parent");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Left,
         Name  => "physical",
         Value => "refname");
      Left := DOM.Core.Nodes.Append_Child
        (N         => Left,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "child"));

      Right := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "element");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Right,
         Name  => "name",
         Value => "refname");

      Assert (Condition => Is_Valid_Reference_Lparent
              (Left_Child => Left,
               Right      => Right),
              Message   => "Name does not match");

      DOM.Core.Elements.Set_Attribute
        (Elem  => Right,
         Name  => "name",
         Value => "nonexistent");
      Assert (Condition => not Is_Valid_Reference_Lparent
              (Left_Child => Left,
               Right      => Right),
              Message   => "Name matches");
--  begin read only
   end Test_Is_Valid_Reference_Lparent;
--  end read only

end Mutools.Match.Test_Data.Tests;
