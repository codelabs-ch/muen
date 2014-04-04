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

package Expanders.XML_Utils
is

   --  Append new child node to given node.
   procedure Append_Child
     (Node      : DOM.Core.Node;
      New_Child : DOM.Core.Node);

   --  Remove child element node with given name. All children of the specified
   --  child node are removed as well. An exception is raised if no child with
   --  the given name exists.
   procedure Remove_Child
     (Node       : DOM.Core.Node;
      Child_Name : String);

   --  Add physical memory region element with given parameters to policy.
   procedure Add_Memory_Region
     (Policy    : in out Muxml.XML_Data_Type;
      Name      :        String;
      Address   :        String;
      Size      :        String;
      Caching   :        String;
      Alignment :        String);

   --  Add file-backed physical memory region element with given parameters to
   --  policy.
   procedure Add_Memory_Region
     (Policy      : in out Muxml.XML_Data_Type;
      Name        :        String;
      Address     :        String;
      Size        :        String;
      Caching     :        String;
      Alignment   :        String;
      File_Name   :        String;
      File_Format :        String;
      File_Offset :        String);

   --  Create virtual memory node with given parameters.
   function Create_Virtual_Memory_Node
     (Policy        : in out Muxml.XML_Data_Type;
      Logical_Name  :        String;
      Physical_Name :        String;
      Address       :        String;
      Writable      :        Boolean;
      Executable    :        Boolean)
      return DOM.Core.Node;

   --  Returns the size of the paging structures needed to map the virtual
   --  memory regions given by XPath expressions. Dev_Virt_Mem_XPath identifies
   --  the virtual device memory regions and Virt_Mem_XPath the virtual memory
   --  mappings.
   function Calculate_PT_Size
     (Policy             : Muxml.XML_Data_Type;
      Dev_Virt_Mem_XPath : String;
      Virt_Mem_XPath     : String)
      return Interfaces.Unsigned_64;

   XML_Error : exception;

end Expanders.XML_Utils;
