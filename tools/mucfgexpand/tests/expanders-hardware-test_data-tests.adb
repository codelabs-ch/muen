--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Hardware.Test_Data.

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
package body Expanders.Hardware.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Add_IOMMU_Default_Caps (Gnattest_T : in out Test);
   procedure Test_Add_IOMMU_Default_Caps_44ac7d (Gnattest_T : in out Test) renames Test_Add_IOMMU_Default_Caps;
--  id:2.2/44ac7dcb3bcef9b3/Add_IOMMU_Default_Caps/1/0/
   procedure Test_Add_IOMMU_Default_Caps (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/hardware_iommu_caps.xml",
         Ref_Diff => "data/hardware_iommu_caps.xml.diff",
         Expander => Add_IOMMU_Default_Caps'Access);
--  begin read only
   end Test_Add_IOMMU_Default_Caps;
--  end read only


--  begin read only
   procedure Test_Add_MSI_IRQ_Numbers (Gnattest_T : in out Test);
   procedure Test_Add_MSI_IRQ_Numbers_27d5e8 (Gnattest_T : in out Test) renames Test_Add_MSI_IRQ_Numbers;
--  id:2.2/27d5e822b86e188e/Add_MSI_IRQ_Numbers/1/0/
   procedure Test_Add_MSI_IRQ_Numbers (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/hardware_msi_irqs.xml",
         Ref_Diff => "data/hardware_msi_irqs.xml.diff",
         Expander => Add_MSI_IRQ_Numbers'Access);
--  begin read only
   end Test_Add_MSI_IRQ_Numbers;
--  end read only


--  begin read only
   procedure Test_Add_PCI_Device_MSI_IRQs (Gnattest_T : in out Test);
   procedure Test_Add_PCI_Device_MSI_IRQs_52e298 (Gnattest_T : in out Test) renames Test_Add_PCI_Device_MSI_IRQs;
--  id:2.2/52e298aa73e29584/Add_PCI_Device_MSI_IRQs/1/0/
   procedure Test_Add_PCI_Device_MSI_IRQs (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
        Test_Utils.Expander.Run_Test
          (Filename => "obj/hardware_pci_msi.xml",
           Ref_Diff => "data/hardware_pci_msi.xml.diff",
           Pre      => Pre_PCI_Device_MSI'Access,
           Expander => Add_PCI_Device_MSI_IRQs'Access);
--  begin read only
   end Test_Add_PCI_Device_MSI_IRQs;
--  end read only


--  begin read only
   procedure Test_Add_Reserved_Memory_Blocks (Gnattest_T : in out Test);
   procedure Test_Add_Reserved_Memory_Blocks_ea03f6 (Gnattest_T : in out Test) renames Test_Add_Reserved_Memory_Blocks;
--  id:2.2/ea03f6c6ec01e163/Add_Reserved_Memory_Blocks/1/0/
   procedure Test_Add_Reserved_Memory_Blocks (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/hardware_reserved_memory_blocks.xml",
         Ref_Diff => "data/hardware_reserved_memory_blocks.xml.diff",
         Expander => Add_Reserved_Memory_Blocks'Access);
--  begin read only
   end Test_Add_Reserved_Memory_Blocks;
--  end read only


--  begin read only
   procedure Test_Add_Processor_CPU_IDs (Gnattest_T : in out Test);
   procedure Test_Add_Processor_CPU_IDs_d44965 (Gnattest_T : in out Test) renames Test_Add_Processor_CPU_IDs;
--  id:2.2/d449658550ed6525/Add_Processor_CPU_IDs/1/0/
   procedure Test_Add_Processor_CPU_IDs (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/processor_cpu_ids.xml",
         Ref_Diff => "data/processor_cpu_ids.xml.diff",
         Expander => Add_Processor_CPU_IDs'Access);
--  begin read only
   end Test_Add_Processor_CPU_IDs;
--  end read only


--  begin read only
   procedure Test_Remove_Reserved_Mem_Regions (Gnattest_T : in out Test);
   procedure Test_Remove_Reserved_Mem_Regions_30c1ec (Gnattest_T : in out Test) renames Test_Remove_Reserved_Mem_Regions;
--  id:2.2/30c1ec4a7af39fe3/Remove_Reserved_Mem_Regions/1/0/
   procedure Test_Remove_Reserved_Mem_Regions (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/hardware_reserved_memory.xml",
         Ref_Diff => "data/hardware_reserved_memory.xml.diff",
         Expander => Remove_Reserved_Mem_Regions'Access);
--  begin read only
   end Test_Remove_Reserved_Mem_Regions;
--  end read only


--  begin read only
   procedure Test_Remove_Reserved_Mem_References (Gnattest_T : in out Test);
   procedure Test_Remove_Reserved_Mem_References_6529e6 (Gnattest_T : in out Test) renames Test_Remove_Reserved_Mem_References;
--  id:2.2/6529e6a3b72406a9/Remove_Reserved_Mem_References/1/0/
   procedure Test_Remove_Reserved_Mem_References (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/hardware_reserved_memory_references.xml",
         Ref_Diff => "data/hardware_reserved_memory_references.xml.diff",
         Expander => Remove_Reserved_Mem_References'Access);
--  begin read only
   end Test_Remove_Reserved_Mem_References;
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
end Expanders.Hardware.Test_Data.Tests;
