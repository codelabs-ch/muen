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

with DOM.Core;

with Musinfo;

package Sinfo.Utils
is

   --  Create name from given string.
   function Create_Name (Str : String) return Musinfo.Name_Type
     with
       Pre => Str'Length in Musinfo.Name_Index_Type;

   --  Append new resource to sinfo. Raises exception if available sinfo
   --  resource space is exhausted.
   procedure Append_Resource
     (Info     : in out Musinfo.Subject_Info_Type;
      Resource :        Musinfo.Resource_Type);

   Sinfo_Full : exception;

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
