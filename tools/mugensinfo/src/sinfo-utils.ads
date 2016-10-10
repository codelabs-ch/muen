--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with DOM.Core;

with Musinfo;

package Sinfo.Utils
is

   use type Musinfo.Resource_Count_Type;

   --  Create name from given string.
   function Create_Name (Str : String) return Musinfo.Name_Type
     with
       Pre => Str'Length in Musinfo.Name_Index_Type;

   --  Create memory region with given parameters.
   function Create_Memregion
     (Content    : Musinfo.Content_Type;
      Address    : Interfaces.Unsigned_64;
      Size       : Interfaces.Unsigned_64;
      Hash       : Musinfo.Hash_Type    := Musinfo.No_Hash;
      Pattern    : Musinfo.Pattern_Type := Musinfo.No_Pattern;
      Writable   : Boolean;
      Executable : Boolean)
      return Musinfo.Memregion_Type;

   --  Create channel information with given parameters.
   function Create_Channel_Info
     (Has_Event  : Boolean;
      Has_Vector : Boolean;
      Event      : Musinfo.Event_Number_Range;
      Vector     : Musinfo.Vector_Range)
      return Musinfo.Channel_Info_Type;

   --  Create resource with given parameters.
   function Create_Resource
     (Name               : Musinfo.Name_Type;
      Memregion_Index    : Musinfo.Resource_Count_Type;
      Channel_Info_Index : Musinfo.Resource_Count_Type)
      return Musinfo.Resource_Type;

   --  Create dev info with given parameters.
   function Create_Dev_Info
     (SID         : Interfaces.Unsigned_16;
      IRTE_Start  : Interfaces.Unsigned_16;
      IRQ_Start   : Interfaces.Unsigned_8;
      IR_Count    : Interfaces.Unsigned_8;
      MSI_Capable : Boolean)
      return Musinfo.Dev_Info_Type;

   --  Append memory region to given subject info.
   procedure Append_Memregion
     (Info   : in out Musinfo.Subject_Info_Type;
      Name   :        Musinfo.Name_Type;
      Region :        Musinfo.Memregion_Type)
     with
       Pre =>
         Info.Resource_Count < Musinfo.Resource_Count_Type'Last and
         Info.Memregion_Count < Musinfo.Resource_Count_Type'Last;

   --  Append channel with specified parameters to given subject info.
   procedure Append_Channel
     (Info       : in out Musinfo.Subject_Info_Type;
      Name       :        Musinfo.Name_Type;
      Memregion  :        Musinfo.Memregion_Type;
      Has_Event  :        Boolean;
      Has_Vector :        Boolean;
      Event      :        Musinfo.Event_Number_Range;
      Vector     :        Musinfo.Vector_Range)
     with
       Pre =>
         not Memregion.Flags.Executable and
         Info.Resource_Count < Musinfo.Resource_Count_Type'Last and
         Info.Memregion_Count < Musinfo.Resource_Count_Type'Last and
         Info.Channel_Info_Count < Musinfo.Resource_Count_Type'Last;

   --  Append device data to given subject info record.
   procedure Append_Dev
     (Info        : in out Musinfo.Subject_Info_Type;
      SID         :        Interfaces.Unsigned_16;
      IRTE_Start  :        Interfaces.Unsigned_16;
      IRQ_Start   :        Interfaces.Unsigned_8;
      IR_Count    :        Interfaces.Unsigned_8;
      MSI_Capable :        Boolean)
     with
       Pre =>
         Info.Dev_Info_Count < Musinfo.Resource_Count_Type'Last;

   --  Convert given hex string to hash array.
   function To_Hash (Hex : String) return Musinfo.Hash_Type
   with
      Pre => Hex'Length = 64 + 4
         and then Hex (Hex'First .. Hex'First + 2) = "16#"
         and then Hex (Hex'Last) = '#';

   --  Get memory region information from given virtual and physical memory
   --  nodes.
   function Get_Memory_Info
     (Virt_Mem_Node : DOM.Core.Node;
      Phys_Mem_Node : DOM.Core.Node)
      return Musinfo.Memregion_Type;

end Sinfo.Utils;
