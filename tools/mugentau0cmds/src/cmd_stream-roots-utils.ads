--
--  Copyright (C) 2019  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2019  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Paging;

with Muxml;

with Cmd_Stream.XML_Utils;

package Cmd_Stream.Roots.Utils
is

   --  Generate command stream to assign given logical memory and device
   --  memory to specified root object.
   procedure Assign_Memory
     (Stream_Doc    : Muxml.XML_Data_Type;
      Physical_Mem  : DOM.Core.Node_List;
      Physical_Devs : DOM.Core.Node_List;
      Logical_Mem   : DOM.Core.Node_List;
      Logical_Devs  : DOM.Core.Node_List;
      Object_Attr   : Cmd_Stream.XML_Utils.Attribute_Type;
      Object_Kind   : String;
      Entity_Name   : String;
      Paging_Levels : Paging.Paging_Level);

end Cmd_Stream.Roots.Utils;
