--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Cmd_Stream.Roots.Kernels.Test_Data.

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
package body Cmd_Stream.Roots.Kernels.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Create_Per_CPU_Kernel (Gnattest_T : in out Test);
   procedure Test_Create_Per_CPU_Kernel_886fd9 (Gnattest_T : in out Test) renames Test_Create_Per_CPU_Kernel;
--  id:2.2/886fd9d7e43af236/Create_Per_CPU_Kernel/1/0/
   procedure Test_Create_Per_CPU_Kernel (Gnattest_T : in out Test) is
   --  cmd_stream-roots-kernels.ads:25:4:Create_Per_CPU_Kernel
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Create_Per_CPU_Kernel;
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
end Cmd_Stream.Roots.Kernels.Test_Data.Tests;
