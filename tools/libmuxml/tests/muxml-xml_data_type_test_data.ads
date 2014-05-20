--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

with GNATtest_Generated;

with Ada.Directories;
with Ada.Characters.Handling;
with Ada.Exceptions;

with DOM.Core.Nodes;

with Test_Utils;

package Muxml.XML_Data_Type_Test_Data is

   type XML_Data_Type_Access is access all GNATtest_Generated.GNATtest_Standard.Muxml.XML_Data_Type'Class;

--  begin read only
   type Test_XML_Data_Type is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with record
      Fixture : XML_Data_Type_Access;
   end record;

   procedure Set_Up (Gnattest_T : in out Test_XML_Data_Type);
   procedure Tear_Down (Gnattest_T : in out Test_XML_Data_Type);

end Muxml.XML_Data_Type_Test_Data;
