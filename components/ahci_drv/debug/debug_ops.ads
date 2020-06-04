--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Interfaces;

with Ahci;
with Mbr;
with Muenblock;

package Debug_Ops
is
   procedure Dump_Cmd_Table
     (ID  : Ahci.Port_Range;
      Len : Integer);

   procedure Dump_Cmd_List
     (ID  : Ahci.Port_Range;
      Len : Integer);

   procedure Dump_Port_Regs (ID : Ahci.Port_Range);

   procedure Print_Port_Error (ID : Ahci.Port_Range);

   procedure Print_MBR_Partition_Table (Table : Mbr.Partition_Table_Type);

   procedure Print_Request (Request : Muenblock.Block_Request_Type);

   --  Initialize debug log.
   procedure Init (Epoch : Interfaces.Unsigned_64);

   --  Output given string.
   procedure Put_String (Item : String);

   --  Output given string and append a new line.
   procedure Put_Line (Item : String);

   --  Output a given Bit_Array
   procedure Put_Bit_Array (Item : Ahci.Bit_Array);

   --  Output PCI device information.
   procedure Print_PCI_Device_Info;

   --  Output PCI device capabilities and their index.
   procedure Print_PCI_Capabilities;

   --  Output HBA memory registers.
   procedure Print_HBA_Memory_Regs;

end Debug_Ops;
