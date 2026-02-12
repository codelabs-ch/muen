with Ports_Config;

package Ahci_Log is

   procedure Dump_Cmd_Table
     (ID  : Ports_Config.Port_Range;
      Len : Integer);

   procedure Dump_Cmd_List
     (ID  : Ports_Config.Port_Range;
      Len : Integer);

   procedure Dump_Port_Regs (ID : Ports_Config.Port_Range);

   procedure Print_Port_Error (ID : Ports_Config.Port_Range);

   --  Output PCI device capabilities and their index.
   procedure Print_PCI_Capabilities;

end Ahci_Log;
