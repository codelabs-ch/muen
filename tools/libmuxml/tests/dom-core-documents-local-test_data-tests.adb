--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into DOM.Core.Documents.Local.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body DOM.Core.Documents.Local.Test_Data.Tests is


--  begin read only
   procedure Test_Adopt_Node (Gnattest_T : in out Test);
   procedure Test_Adopt_Node_6a4a84 (Gnattest_T : in out Test) renames Test_Adopt_Node;
--  id:2.2/6a4a8407a73d6d53/Adopt_Node/1/0/
   procedure Test_Adopt_Node (Gnattest_T : in out Test) is
   --  dom-core-documents-local.ads:31:4:Adopt_Node
--  end read only

      pragma Unreferenced (Gnattest_T);

      Doc_A, Doc_B   : DOM.Core.Document;
      Impl_A, Impl_B : DOM.Core.DOM_Implementation;
      Node           : DOM.Core.Node;
   begin
      Doc_A := DOM.Core.Create_Document (Implementation => Impl_A);
      Node  := DOM.Core.Documents.Create_Element
        (Doc      => Doc_A,
         Tag_Name => "A");

      Doc_B := DOM.Core.Create_Document (Implementation => Impl_B);

      Assert (Condition => DOM.Core.Nodes.Owner_Document (N => Node) = Doc_A,
              Message   => "Node does not belong to document A");

      Node := DOM.Core.Documents.Local.Adopt_Node
        (Doc    => Doc_B,
         Source => Node);
      Assert (Condition => DOM.Core.Nodes.Owner_Document (N => Node) = Doc_B,
              Message   => "Node not adopted");
--  begin read only
   end Test_Adopt_Node;
--  end read only

end DOM.Core.Documents.Local.Test_Data.Tests;
