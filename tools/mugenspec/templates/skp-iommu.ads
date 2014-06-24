with SK;

package Skp.IOMMU
is

   Root_Table_Address : constant := __root_table_addr__;

   type IOMMU_Device_Range is range __iommu_device_range__;

   type IOMMU_Device_Array is array (IOMMU_Device_Range) of SK.Word64;

   IOMMUs : constant IOMMU_Device_Array := IOMMU_Device_Array'(
__iommu_devices__);

end Skp.IOMMU;
