--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Sinfo.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Sinfo.Utils.Test_Data.Tests is


--  begin read only
   procedure Test_Get_Bounds (Gnattest_T : in out Test);
   procedure Test_Get_Bounds_5acc2a (Gnattest_T : in out Test) renames Test_Get_Bounds;
--  id:2.2/5acc2ab604d2d8b2/Get_Bounds/1/0/
   procedure Test_Get_Bounds (Gnattest_T : in out Test) is
   --  sinfo-utils.ads:26:4:Get_Bounds
--  end read only

      pragma Unreferenced (Gnattest_T);

      Dom_Impl     : DOM.Core.DOM_Implementation;
      Policy       : Muxml.XML_Data_Type;
      Parent, Node : DOM.Core.Node;
      Lower, Upper : Integer;
   begin
      Policy.Doc := DOM.Core.Create_Document (Implementation => Dom_Impl);

      Parent := DOM.Core.Documents.Create_Element (Doc      => Policy.Doc,
                                                   Tag_Name => "parent");

      Node := DOM.Core.Documents.Create_Element (Doc      => Policy.Doc,
                                                 Tag_Name => "node");
      DOM.Core.Elements.Set_Attribute (Elem  => Node,
                                       Name  => "number",
                                       Value => "99");
      Node := DOM.Core.Nodes.Append_Child (N         => Parent,
                                           New_Child => Node);
      Node := DOM.Core.Documents.Create_Element (Doc      => Policy.Doc,
                                                 Tag_Name => "node");
      DOM.Core.Elements.Set_Attribute (Elem  => Node,
                                       Name  => "number",
                                       Value => "12");
      Node := DOM.Core.Nodes.Append_Child (N         => Parent,
                                           New_Child => Node);
      Node := DOM.Core.Documents.Create_Element (Doc      => Policy.Doc,
                                                 Tag_Name => "node");
      DOM.Core.Elements.Set_Attribute (Elem  => Node,
                                       Name  => "number",
                                       Value => "54");
      Node := DOM.Core.Nodes.Append_Child (N         => Parent,
                                           New_Child => Node);
      Node := DOM.Core.Documents.Create_Element (Doc      => Policy.Doc,
                                                 Tag_Name => "node");
      DOM.Core.Elements.Set_Attribute (Elem  => Node,
                                       Name  => "number",
                                       Value => "-20");
      Node := DOM.Core.Nodes.Append_Child (N         => Parent,
                                           New_Child => Node);

      Get_Bounds (Nodes     => DOM.Core.Elements.Get_Elements_By_Tag_Name
                  (Elem => Parent,
                   Name => "node"),
                  Attr_Name => "number",
                  Lower     => Lower,
                  Upper     => Upper);

      Assert (Condition => Lower = -20,
              Message   => "Lower bound mismatch:" & Lower'Img);
      Assert (Condition => Upper = 99,
              Message   => "Upper bound mismatch:" & Upper'Img);
--  begin read only
   end Test_Get_Bounds;
--  end read only

end Sinfo.Utils.Test_Data.Tests;