package Skp.Hardware
is

   Debugconsole_Port : constant := 16#50b0#;
   Ioapic_1_Mmio : constant := 16#001f_c000#;
   Ioapic_1_Gsi_Base : constant := 16#0000#;
   Ioapic_1_Max_Redirection_Entry : constant := 16#0017#;
   Iommu_1_Mmio : constant := 16#001f_d000#;
   Iommu_1_Fr_Offset : constant := 16#0200#;
   Iommu_1_Iotlb_Invalidate_Offset : constant := 16#0108#;
   Iommu_2_Mmio : constant := 16#001f_e000#;
   Iommu_2_Fr_Offset : constant := 16#0202#;
   Iommu_2_Iotlb_Invalidate_Offset : constant := 16#0500#;
   System_Board_Reset_Port : constant := 16#0cf9#;
   System_Board_Pm1a_Cnt_Slp_Typ : constant := 16#1c00#;

end Skp.Hardware;
