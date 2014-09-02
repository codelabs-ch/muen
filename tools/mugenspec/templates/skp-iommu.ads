package Skp.IOMMU
is

   Root_Table_Address : constant := __root_table_addr__;
   Base_Address       : constant := __base_addr__;

   IR_Table_Address   : constant := __ir_table_addr__;

   type IOMMU_Device_Range is range __iommu_device_range__;

end Skp.IOMMU;
