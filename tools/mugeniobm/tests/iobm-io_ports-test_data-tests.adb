--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Iobm.IO_Ports.Test_Data.

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
package body Iobm.IO_Ports.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Allow_Ports (Gnattest_T : in out Test);
   procedure Test_Allow_Ports_744e03 (Gnattest_T : in out Test) renames Test_Allow_Ports;
--  id:2.2/744e03448184dde7/Allow_Ports/1/0/
   procedure Test_Allow_Ports (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Streams.Stream_Element_Array;

      B        : IO_Bitmap_Type            := Null_IO_Bitmap;
      Null_Ref : constant IO_Bitmap_Stream := (others => 16#ff#);
      S_Ref    : IO_Bitmap_Stream          := (2583   => 16#fe#,
                                               others => 16#ff#);
   begin
      Assert (Condition => To_Stream (B => B) = Null_Ref,
              Message   => "Null bitmap allows access");

      Allow_Ports (B          => B,
                   Start_Port => 16#50b0#,
                   End_Port   => 16#50b0#);
      Assert (Condition => To_Stream (B => B) = S_Ref,
              Message   => "Error allowing single port");

      Allow_Ports (B          => B,
                   Start_Port => 16#03d4#,
                   End_Port   => 16#03d5#);
      S_Ref (123) := 16#cf#;
      Assert (Condition => To_Stream (B => B) = S_Ref,
              Message   => "Error allowing ports");
--  begin read only
   end Test_Allow_Ports;
--  end read only


--  begin read only
   procedure Test_To_Stream (Gnattest_T : in out Test);
   procedure Test_To_Stream_0b7ee3 (Gnattest_T : in out Test) renames Test_To_Stream;
--  id:2.2/0b7ee3b84c90fcd1/To_Stream/1/0/
   procedure Test_To_Stream (Gnattest_T : in out Test) is
--  end read only

      use type Ada.Streams.Stream_Element_Array;

      IO_Bitmap  : IO_Bitmap_Type   := Null_IO_Bitmap;
      All_Denied : IO_Bitmap_Stream := (others => 16#ff#);
   begin
      Assert (Condition => To_Stream (B => IO_Bitmap) = All_Denied,
              Message   => "Not all ports denied");
--  begin read only
   end Test_To_Stream;
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
end Iobm.IO_Ports.Test_Data.Tests;
