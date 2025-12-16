with Storage_Drv_Nvme_Component;
with Storage_Drv_Nvme_Component.Devices;
with Storage_Drv_Nvme_Component.Channel_Arrays;
with Storage_Drv_Nvme_Component.Memory;
with Storage_Drv_Nvme_Component.Memory_Arrays;

package Storage_Drv_Cspecs_Wrapper is
   package Storage_Drv_Component renames Storage_Drv_Nvme_Component;
   package Devices renames Storage_Drv_Nvme_Component.Devices;
   package Channel_Arrays renames Storage_Drv_Nvme_Component.Channel_Arrays;
   package Memory renames Storage_Drv_Nvme_Component.Memory;
   package Memory_Arrays renames Storage_Drv_Nvme_Component.Memory_Arrays;
end Storage_Drv_Cspecs_Wrapper;
