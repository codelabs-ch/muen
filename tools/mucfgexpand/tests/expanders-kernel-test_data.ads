--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

with Expanders.Components;
with Expanders.Siblings;
with Expanders.Memory;
with Expanders.Subjects;

with Test_Utils.Expander;

package Expanders.Kernel.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

   --  Invoke kernel section skeleton and subject id expanders.
   procedure Pre_Subj_Mappings (Data : in out Muxml.XML_Data_Type);

   --  Prepare policy for MSR store mappings test.
   procedure Pre_Subj_MSR_Store_Mappings (Data : in out Muxml.XML_Data_Type);

   --  Prepare policy for scheduling group info mappings test.
   procedure Pre_Sched_Group_Info_Mappings (Data : in out Muxml.XML_Data_Type);

end Expanders.Kernel.Test_Data;
