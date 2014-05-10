--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.


with AUnit.Test_Fixtures;

with GNATtest_Generated;

with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Test_Utils;

package Alloc.Map.Map_Type_Test_Data is

   type Map_Type_Access is access all GNATtest_Generated.GNATtest_Standard.Alloc.Map.Map_Type'Class;

--  begin read only
   type Test_Map_Type is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with record
      Fixture : Map_Type_Access;
   end record;

   procedure Set_Up (Gnattest_T : in out Test_Map_Type);
   procedure Tear_Down (Gnattest_T : in out Test_Map_Type);

   Output_File : Ada.Text_IO.File_Type;

   function U
     (S : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   procedure Write_Region (R : Alloc.Map.Region_Type);

end Alloc.Map.Map_Type_Test_Data;
