--
--  Copyright (C) 2014-2021  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014-2021  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Storage_Interface; use Storage_Interface;
with Gpt;
with Partitions;
with Muenblock;

package Log
is

   procedure Print_MBR_Partition_Table (Table : Partitions.Partition_Table_Type);

   subtype Bool_Image is String (1 .. 5);

   function Boolean_Image (B : Boolean) return Bool_Image;

   procedure Print_GPT_Partition_Table_Entry (Partition : Gpt.Partition_Entry_Type; Index : Partitions.Partition_Array_Length);

   procedure Print_GPT_Header (Header : Gpt.GPT_Header_Type);

   procedure Print_Request (Request : Muenblock.Block_Request_Type);

   --  Initialize debug log.
   procedure Init (Epoch : Interfaces.Unsigned_64);

   --  Output given string.
   procedure Put_String (Item : String);

   -- Output new line
   procedure New_Line;

   --  Output given string and append a new line.
   procedure Put_Line (Item : String);

   procedure Write_Character (Item : Character);

   --  Output a given Bit_Array
   procedure Put_Bit_Array (Item : Bit_Array);

   --  Output PCI device information.
   procedure Print_PCI_Device_Info;

end Log;
