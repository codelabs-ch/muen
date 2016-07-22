--  This package has been generated automatically by GNATtest.
--  Do not edit any part of it, see GNATtest documentation for more details.

--  begin read only
with Gnattest_Generated;

package Mucfgcheck.Kernel.Test_Data.Tests is

   type Test is new GNATtest_Generated.GNATtest_Standard.Mucfgcheck.Kernel.Test_Data.Test
   with null record;

   procedure Test_CPU_Store_Address_Equality_d15328 (Gnattest_T : in out Test);
   --  mucfgcheck-kernel.ads:25:4:CPU_Store_Address_Equality

   procedure Test_Stack_Address_Equality_61fb48 (Gnattest_T : in out Test);
   --  mucfgcheck-kernel.ads:28:4:Stack_Address_Equality

   procedure Test_IOMMU_Consecutiveness_fc88d4 (Gnattest_T : in out Test);
   --  mucfgcheck-kernel.ads:31:4:IOMMU_Consecutiveness

   procedure Test_CPU_Memory_Section_Count_14dd51 (Gnattest_T : in out Test);
   --  mucfgcheck-kernel.ads:34:4:CPU_Memory_Section_Count

   procedure Test_Virtual_Memory_Overlap_7973e4 (Gnattest_T : in out Test);
   --  mucfgcheck-kernel.ads:37:4:Virtual_Memory_Overlap

   procedure Test_System_Board_Reference_9057a6 (Gnattest_T : in out Test);
   --  mucfgcheck-kernel.ads:41:4:System_Board_Reference

   procedure Test_VTd_IRT_Region_Mapping_725e0c (Gnattest_T : in out Test);
   --  mucfgcheck-kernel.ads:45:4:VTd_IRT_Region_Mapping

end Mucfgcheck.Kernel.Test_Data.Tests;
--  end read only
