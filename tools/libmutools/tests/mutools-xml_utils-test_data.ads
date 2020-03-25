--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;

with DOM.Core.Elements;
with DOM.Core.Nodes;
with DOM.Core.Documents;
with DOM.Core.Append_Node;

with McKae.XML.XPath.XIA;

with Muxml.Utils;

with Test_Utils;

package Mutools.XML_Utils.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

   --  Create memory node with given arguments.
   function Create_Mem_Node
     (Doc     : DOM.Core.Document;
      Name    : String;
      Address : String;
      Size    : String)
      return DOM.Core.Node;

   --  Append MSR with given attributes to MSR list.
   procedure Append_MSR
     (Doc       : in out DOM.Core.Document;
      List      : in out DOM.Core.Node_List;
      MSR_Start :        String;
      MSR_End   :        String;
      Mode      :        String);

end Mutools.XML_Utils.Test_Data;
