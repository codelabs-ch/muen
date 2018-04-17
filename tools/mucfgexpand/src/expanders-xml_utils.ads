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

with DOM.Core;

with Muxml;
with Paging;

package Expanders.XML_Utils
is

   --  Create subject source event node with given parameters.
   function Create_Source_Event_Node
     (Policy        : in out Muxml.XML_Data_Type;
      ID            :        String;
      Logical_Name  :        String;
      Physical_Name :        String)
      return DOM.Core.Node;

   --  Create subject target event node with given parameters.
   function Create_Target_Event_Node
     (Policy        : in out Muxml.XML_Data_Type;
      Logical_Name  :        String;
      Physical_Name :        String;
      Vector        :        String)
      return DOM.Core.Node;

   --  Create subject device node with given parameters.
   function Create_Logical_Device_Node
     (Policy        : in out Muxml.XML_Data_Type;
      Logical_Name  :        String;
      Physical_Name :        String)
      return DOM.Core.Node;

   --  Returns the size of the paging structures needed to map the virtual
   --  memory regions given by XPath expressions given the specified paging
   --  levels and large page mappings. Dev_Virt_Mem_XPath identifies the
   --  virtual device memory regions and Virt_Mem_XPath the virtual memory
   --  mappings.
   function Calculate_PT_Size
     (Policy             : Muxml.XML_Data_Type;
      Paging_Levels      : Paging.Paging_Level;
      Large_Pages        : Boolean;
      Dev_Virt_Mem_XPath : String;
      Virt_Mem_XPath     : String)
      return Interfaces.Unsigned_64;

   --  Allocate a region of given region size in the address space with the
   --  specified size and reserved (fixed and device) memory regions. Return
   --  the virtual start address of the new region.
   function Calculate_Region_Address
     (Policy             : Muxml.XML_Data_Type;
      Fixed_Memory       : DOM.Core.Node_List;
      Device_Memory      : DOM.Core.Node_List;
      Address_Space_Size : Interfaces.Unsigned_64;
      Region_Size        : Interfaces.Unsigned_64)
      return Interfaces.Unsigned_64;

   --  Returns True if the subject memory region specified by virtual address
   --  and size is free to map, i.e. no overlapping, existing mapping in the
   --  address space of the given subject is present
   function Is_Free_To_Map
     (Subject         : DOM.Core.Node;
      Virtual_Address : Interfaces.Unsigned_64;
      Region_Size     : Interfaces.Unsigned_64)
      return Boolean;

end Expanders.XML_Utils;
