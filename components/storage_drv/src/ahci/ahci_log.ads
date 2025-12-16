with Storage_Interface; use Storage_Interface;

package Ahci_Log is

   procedure Dump_Cmd_Table
     (ID  : PConf.Port_Range;
      Len : Integer);

   procedure Dump_Cmd_List
     (ID  : PConf.Port_Range;
      Len : Integer);

   procedure Dump_Port_Regs (ID : PConf.Port_Range);

   procedure Print_Port_Error (ID : PConf.Port_Range);

   --  Output PCI device capabilities and their index.
   procedure Print_PCI_Capabilities;

end Ahci_Log;