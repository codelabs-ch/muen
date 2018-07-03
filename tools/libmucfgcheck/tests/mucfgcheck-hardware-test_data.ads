--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

with Ada.Exceptions;

with McKae.XML.XPath.XIA;

with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;

with Muxml.Utils;
with Mutools.XML_Utils;

package Mucfgcheck.Hardware.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

end Mucfgcheck.Hardware.Test_Data;
