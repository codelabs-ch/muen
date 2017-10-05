--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with Ada.Unchecked_Conversion;
with Ada.Exceptions;
with Ada.Text_IO;

with Interfaces.C.Strings;

with Bfd.Files;

with AUnit.Test_Fixtures;

with Bin_Split.Types;

package Bin_Split.Binary.Sections.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

end Bin_Split.Binary.Sections.Test_Data;
