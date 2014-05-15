--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Iobm.IO_Ports.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Iobm.IO_Ports.Test_Data.Tests is


--  begin read only
   procedure Test_Allow_Ports (Gnattest_T : in out Test);
   procedure Test_Allow_Ports_744e03 (Gnattest_T : in out Test) renames Test_Allow_Ports;
--  id:2.2/744e03448184dde7/Allow_Ports/1/0/
   procedure Test_Allow_Ports (Gnattest_T : in out Test) is
   --  iobm-io_ports.ads:36:4:Allow_Ports
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Allow_Ports;
--  end read only


--  begin read only
   procedure Test_To_Stream (Gnattest_T : in out Test);
   procedure Test_To_Stream_0b7ee3 (Gnattest_T : in out Test) renames Test_To_Stream;
--  id:2.2/0b7ee3b84c90fcd1/To_Stream/1/0/
   procedure Test_To_Stream (Gnattest_T : in out Test) is
   --  iobm-io_ports.ads:47:4:To_Stream
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_To_Stream;
--  end read only

end Iobm.IO_Ports.Test_Data.Tests;
