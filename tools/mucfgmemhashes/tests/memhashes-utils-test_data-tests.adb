--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Memhashes.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with McKae.XML.XPath.XIA;
with Muxml.Utils;
--  begin read only
--  end read only
package body Memhashes.Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_SHA256_Digest (Gnattest_T : in out Test);
   procedure Test_SHA256_Digest_be9005 (Gnattest_T : in out Test) renames Test_SHA256_Digest;
--  id:2.2/be900524d3954a67/SHA256_Digest/1/0/
   procedure Test_SHA256_Digest (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
      Nodes  : DOM.Core.Node_List;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy_hashes.xml");
      Nodes := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/memory/memory/*[self::fill or self::file]/..");
      Assert (Condition => DOM.Core.Nodes.Length (List => Nodes) > 0,
              Message   => "No content regions in reference policy");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Cur_Mem : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Nodes,
               Index => I);
            Mem_Name : constant String
              := DOM.Core.Elements.Get_Attribute (Elem => Cur_Mem,
                                                  Name => "name");
            Ref_Hash : constant String
              := Muxml.Utils.Get_Attribute (Doc   => Cur_Mem,
                                            XPath => "hash",
                                            Name  => "value");
         begin
            if Ref_Hash'Length > 0 and Ref_Hash /= "none" then
               Assert (Condition => SHA256_Digest
                       (Node      => Cur_Mem,
                        Input_Dir => "data") = Ref_Hash,
                       Message   => "Hash mismatch of memory region '"
                       & Mem_Name & "'");
            end if;
         end;
      end loop;
--  begin read only
   end Test_SHA256_Digest;
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
end Memhashes.Utils.Test_Data.Tests;
