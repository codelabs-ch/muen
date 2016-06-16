package Libdebug_Component.Devices
is

   Hsu_Irq : constant := 15;

   Hsu_Mmio1_Address    : constant := 16#a000_3000#;
   Hsu_Mmio1_Size       : constant := 16#1000#;
   Hsu_Mmio1_Executable : constant Boolean := False;
   Hsu_Mmio1_Writable   : constant Boolean := True;

end Libdebug_Component.Devices;
