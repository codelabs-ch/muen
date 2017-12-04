--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Stackcheck.Utils.Test_Data.

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
package body Stackcheck.Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Entity_To_Ada_Name (Gnattest_T : in out Test);
   procedure Test_Entity_To_Ada_Name_557e0f (Gnattest_T : in out Test) renames Test_Entity_To_Ada_Name;
--  id:2.2/557e0fbc48f38a65/Entity_To_Ada_Name/1/0/
   procedure Test_Entity_To_Ada_Name (Gnattest_T : in out Test) is
   --  stackcheck-utils.ads:23:4:Entity_To_Ada_Name
--  end read only

      pragma Unreferenced (Gnattest_T);

      type Str_Array is array (Natural range <>) of Unbounded_String;

      Src_Strs : constant Str_Array
        := (Null_Unbounded_String,
            To_Unbounded_String ("*handle_vmx_exit"),
            To_Unbounded_String ("exit_handlers__cpuid__process"),
            To_Unbounded_String ("musinfo__utils___elabs"),
            To_Unbounded_String ("debuglog__sink___elabb"));
      Ref_Strs : constant Str_Array
        := (Null_Unbounded_String,
            To_Unbounded_String ("Handle_Vmx_Exit"),
            To_Unbounded_String ("Exit_Handlers.Cpuid.Process"),
            To_Unbounded_String ("Musinfo.Utils'Elab_Spec"),
            To_Unbounded_String ("Debuglog.Sink'Elab_Body"));
   begin
      for I in Src_Strs'Range loop
         Assert (Condition => Entity_To_Ada_Name
                 (Str => To_String (Src_Strs (I))) = Ref_Strs (I),
                 Message   => "String mismatch: " & Entity_To_Ada_Name
                   (Str => To_String (Src_Strs (I)))
                 & " /= " & To_String (Ref_Strs (I)));
      end loop;
--  begin read only
   end Test_Entity_To_Ada_Name;
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
end Stackcheck.Utils.Test_Data.Tests;
