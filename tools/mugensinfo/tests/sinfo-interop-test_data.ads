--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

with Interfaces.C;
use type Interfaces.C.int;

with Musinfo;

with Sinfo.Utils;
with Sinfo.Constants;

with C_Imports;

package Sinfo.Interop.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

   Ref_Name_Str : constant String (Musinfo.Name_Index_Type) := (others => 'a');
   Ref_Name     : constant Musinfo.Name_Type
     := Utils.Create_Name (Str => Ref_Name_Str);

   Ref_Dev : constant Musinfo.Resource_Type
     := (Kind     => Musinfo.Res_Device,
         Name     => Ref_Name,
         Dev_Data => (SID        => 16#abcd#,
                      IRTE_Start => 200,
                      IRQ_Start  => 12,
                      IR_Count   => 22,
                      Flags      => (MSI_Capable => True,
                                     Padding     => 0),
                      Padding    => (others => 0)),
        Padding   => (others => 0));

end Sinfo.Interop.Test_Data;
