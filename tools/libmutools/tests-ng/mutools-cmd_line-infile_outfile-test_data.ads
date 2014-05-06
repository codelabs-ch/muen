--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

with Ada.Strings.Unbounded;

with GNAT.OS_Lib;

package Mutools.Cmd_Line.Infile_Outfile.Test_Data is

   use Ada.Strings.Unbounded;

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

   Infile  : constant Unbounded_String := To_Unbounded_String
     ("data/test_policy.xml");
   Outfile : constant Unbounded_String := To_Unbounded_String
     ("obj/test_policy.xml");

   Process_Counter : Positive := 1;
   Process_Error   : exception;

   procedure Immutable_Process
     (Input_Policy : Muxml.XML_Data_Type;
      Output_File  : String);

   procedure Mutable_Process
     (Policy      : in out Muxml.XML_Data_Type;
      Output_File :        String);

end Mutools.Cmd_Line.Infile_Outfile.Test_Data;
