--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

with Ada.Directories;
with Ada.Exceptions;

with DOM.Core.Documents;

with Muxml.Utils;

with Test_Utils;

package Mutools.Expressions.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

   procedure Initialize_Node_Access_Testing
      (Data        :        Muxml.XML_Data_Type;
       Node_Access : in out Access_Hashmaps_Type);

end Mutools.Expressions.Test_Data;
