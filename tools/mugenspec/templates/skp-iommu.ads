package Skp.IOMMU
is

   Root_Table_Address    : constant := __root_table_addr__;
   Base_Address          : constant := __base_addr__;

   IR_Table_Phys_Address : constant := __ir_table_phys_addr__;
   IR_Table_Virt_Address : constant := __ir_table_virt_addr__;
   IR_Table_Size         : constant := 7;

   Cap_AGAW_Bit          : constant := __cap_agaw_bit__;

   type IOMMU_Device_Range is range __iommu_device_range__;

end Skp.IOMMU;
