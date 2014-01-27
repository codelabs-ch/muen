--
--  Copyright (C) 2014  Alexander Senier <mail@senier.net>
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

with DOM.Core.Nodes;
with DOM.Core.Elements;
with McKae.XML.XPath.XIA;
with Alloc.Map;
with Interfaces;

package body Alloc.Allocator
is

   Not_Implemented : exception;

   procedure Write
     (Output_File : String;
      Policy      : Muxml.XML_Data_Type)
   is
      Physical_Map           : DOM.Core.Node_List;
      Map                    : Alloc.Map.Map_Type;
      Physical_Address, Size : Interfaces.Unsigned_64;

      use DOM.Core.Elements;
      use DOM.Core.Nodes;
      use type Interfaces.Unsigned_64;
   begin
      Physical_Map := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/platform/memory/memoryBlock");

      if Length (Physical_Map) = 0 then
         raise Internal_Error with "No physical memory found";
      end if;

      for I in 0 .. DOM.Core.Nodes.Length (List => Physical_Map) - 1
      loop
         Physical_Address := Interfaces.Unsigned_64'Value
            (Get_Attribute (Item (Physical_Map, I), "physicalAddress"));
         Size := Interfaces.Unsigned_64'Value
            (Get_Attribute (Item (Physical_Map, I), "size"));
         Map.Insert_Empty_Region
            (First_Address => Physical_Address,
             Last_Address  => Physical_Address + Size - 1);
      end loop;

      Muxml.Write
         (File => Output_File,
          Data => Policy);

      raise Not_Implemented;
   exception
      when Alloc.Map.Overlapping_Empty_Region =>
         raise Overlapping_Physical_Memory;
   end Write;

end Alloc.Allocator;
