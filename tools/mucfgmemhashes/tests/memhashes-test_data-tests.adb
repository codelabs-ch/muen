--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Memhashes.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Memhashes.Test_Data.Tests is


--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_e84213 (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/e84213e130018c54/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  memhashes.ads:25:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

      Fname  : constant String := "obj/test_policy.xml";
      Policy : Muxml.XML_Data_Type;

   begin
      Run (Policy_In  => "data/test_policy.xml",
           Policy_Out => Fname,
           Input_Dir  => "data");

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => Fname);

      declare
         Hash_Nodes : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Policy.Doc,
              XPath => "//hash");
      begin
         Assert (Condition => DOM.Core.Elements.Get_Attribute
                 (Elem => DOM.Core.Nodes.Item
                  (List  => Hash_Nodes,
                   Index => 0),
                  Name => "value") = "16#f8ca02c69621dd84cd1212ebfd7d6cdc9ba6a"
                 & "d658854f29567723531912d1a35#",
                 Message   => "Hash 1 mismatch");
         Assert (Condition => DOM.Core.Elements.Get_Attribute
                 (Elem => DOM.Core.Nodes.Item
                  (List  => Hash_Nodes,
                   Index => 1),
                  Name => "value") = "16#4fc47cd538682663dda78a790c92d25221231"
                 & "72914a05a89394a48f0216ad4c3#",
                 Message   => "Hash 2 mismatch");
      end;
--  begin read only
   end Test_Run;
--  end read only

end Memhashes.Test_Data.Tests;