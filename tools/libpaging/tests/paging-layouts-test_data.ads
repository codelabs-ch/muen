--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

with Ada.Streams.Stream_IO;

with Mutools.Files;

with Paging.EPT;
with Paging.IA32e;

with Test_Utils;

package Paging.Layouts.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

   --  Stream type that serializes to memory buffer.
   type Memory_Stream_Type is new Ada.Streams.Root_Stream_Type with record
      Buffer    : Ada.Streams.Stream_Element_Array (1 .. 1) := (others => 0);
      Write_Idx : Ada.Streams.Stream_Element_Offset         := 1;
      Read_Idx  : Ada.Streams.Stream_Element_Offset         := 1;
   end record;

   overriding
   procedure Read
     (Stream : in out Memory_Stream_Type;
      Item   :    out Ada.Streams.Stream_Element_Array;
      Last   :    out Ada.Streams.Stream_Element_Offset) is null;

   overriding
   procedure Write
     (Stream : in out Memory_Stream_Type;
      Item   :        Ada.Streams.Stream_Element_Array);

end Paging.Layouts.Test_Data;
