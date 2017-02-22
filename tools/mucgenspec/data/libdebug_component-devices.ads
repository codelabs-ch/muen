pragma Style_Checks (Off);

package Libdebug_Component.Devices
is

   Hsu_Irq : constant := 15;

   Hsu_Mmio1_Address    : constant := 16#a000_3000#;
   Hsu_Mmio1_Size       : constant := 16#1000#;
   Hsu_Mmio1_Executable : constant Boolean := False;
   Hsu_Mmio1_Writable   : constant Boolean := True;

   Hsu_Port_Start : constant := 16#cafa#;
   Hsu_Port_End   : constant := 16#cafe#;

end Libdebug_Component.Devices;
