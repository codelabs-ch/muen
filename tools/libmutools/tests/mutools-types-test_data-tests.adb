--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Types.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Mutools.Types.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Get_Max_ID (Gnattest_T : in out Test);
   procedure Test_Get_Max_ID_a65afa (Gnattest_T : in out Test) renames Test_Get_Max_ID;
--  id:2.2/a65afae2a79d6438/Get_Max_ID/1/0/
   procedure Test_Get_Max_ID (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => Get_Max_ID (Group => Vmx_Exit)
              = Constants.Max_Valid_VMX_Exit_ID,
              Message   => "Invalid VMX exit max ID");
      Assert (Condition => Get_Max_ID (Group => Vmcall)
              = Constants.Max_Valid_Vmcall_ID,
              Message   => "Invalid Vmcall max ID");
--  begin read only
   end Test_Get_Max_ID;
--  end read only


--  begin read only
   procedure Test_Is_Valid_Event_ID (Gnattest_T : in out Test);
   procedure Test_Is_Valid_Event_ID_2d339d (Gnattest_T : in out Test) renames Test_Is_Valid_Event_ID;
--  id:2.2/2d339dda9942d861/Is_Valid_Event_ID/1/0/
   procedure Test_Is_Valid_Event_ID (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use Mutools.Types;

      type ID_Test_Info is record
         Group : Event_Group_Type;
         ID    : Natural;
         Valid : Boolean;
      end record;

      Test_Data : constant array (Natural range <>) of ID_Test_Info
        := ((Group => Vmx_Exit,
             ID    => 0,
             Valid => True),
            (Group => Vmx_Exit,
             ID    => 1,
             Valid => False),
            (Group => Vmx_Exit,
             ID    => 7,
             Valid => False),
            (Group => Vmx_Exit,
             ID    => 34,
             Valid => True),
            (Group => Vmx_Exit,
             ID    => 35,
             Valid => False),
            (Group => Vmx_Exit,
             ID    => 38,
             Valid => False),
            (Group => Vmx_Exit,
             ID    => 41,
             Valid => False),
            (Group => Vmx_Exit,
             ID    => 42,
             Valid => False),
            (Group => Vmx_Exit,
             ID    => 52,
             Valid => False),
            (Group => Vmx_Exit,
             ID    => 59,
             Valid => True),
            (Group => Vmx_Exit,
             ID    => 60,
             Valid => False),
            (Group => Vmcall,
             ID    => 0,
             Valid => True),
            (Group => Vmcall,
             ID    => 31,
             Valid => True),
            (Group => Vmcall,
             ID    => 63,
             Valid => True),
            (Group => Vmcall,
             ID    => 64,
             Valid => False));
   begin
      for Data of Test_Data loop
         Assert (Condition => Is_Valid_Event_ID
                 (Group => Data.Group,
                  ID    => Data.ID) = Data.Valid,
                 Message   => "Unexpected result for ID" & Data.ID'Img
                 & " of group " & Data.Group'Img);
      end loop;
--  begin read only
   end Test_Is_Valid_Event_ID;
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
end Mutools.Types.Test_Data.Tests;
