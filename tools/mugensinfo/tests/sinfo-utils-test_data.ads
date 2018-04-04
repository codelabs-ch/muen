--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

with Interfaces;

with Muxml.Utils;

with Sinfo.Constants;

package Sinfo.Utils.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

   Ref_Hash : Musinfo.Hash_Type
     := (16#e1#, 16#0e#, 16#6c#, 16#3b#, 16#5f#, 16#7e#, 16#8e#, 16#b8#,
         16#a5#, 16#51#, 16#0b#, 16#85#, 16#62#, 16#ed#, 16#44#, 16#91#,
         16#00#, 16#a8#, 16#56#, 16#e0#, 16#2a#, 16#31#, 16#a2#, 16#7d#,
         16#30#, 16#bf#, 16#5e#, 16#76#, 16#ef#, 16#d9#, 16#12#, 16#35#);

end Sinfo.Utils.Test_Data;
