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

with Ada.Strings.Unbounded;

with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Cspec.Utils;

package body Cspec.Generators
is

   use Ada.Strings.Unbounded;

   type Specific_String_Getter is not null access function
     (Node : DOM.Core.Node)
   return String;

   --  Use given function to convert nodes specified by XPath query to string
   --  representation.
   function Get_Str
     (Policy : Muxml.XML_Data_Type;
      Func   : Specific_String_Getter;
      XPath  : String)
      return String;

   -------------------------------------------------------------------------

   function Get_Channels_Str
     (Policy    : Muxml.XML_Data_Type;
      Comp_Name : String)
      return String
   is
   begin
      return Get_Str
        (Policy => Policy,
         Func   => Utils.To_Channel_Str'Access,
         XPath  => "/system/components/*[@name='" & Comp_Name
         & "']/channels/*");
   end Get_Channels_Str;

   -------------------------------------------------------------------------

   function Get_Devices_Str
     (Policy    : Muxml.XML_Data_Type;
      Comp_Name : String)
      return String
   is
   begin
      return Get_Str
        (Policy => Policy,
         Func   => Utils.To_Device_Str'Access,
         XPath  => "/system/components/*[@name='" & Comp_Name
         & "']/devices/*");
   end Get_Devices_Str;

   -------------------------------------------------------------------------

   function Get_Memory_Str
     (Policy    : Muxml.XML_Data_Type;
      Comp_Name : String)
      return String
   is
   begin
      return Get_Str
        (Policy => Policy,
         Func   => Utils.To_Memory_Str'Access,
         XPath  => "/system/components/*[@name='" & Comp_Name
         & "']/memory/memory");
   end Get_Memory_Str;

   -------------------------------------------------------------------------

   function Get_Str
     (Policy : Muxml.XML_Data_Type;
      Func   : Specific_String_Getter;
      XPath  : String)
      return String
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => XPath);
      Count : constant Natural := DOM.Core.Nodes.Length (List => Nodes);
      Res   : Unbounded_String;
   begin
      for I in 0 .. Count - 1 loop
         Res := Res & Func (Node => DOM.Core.Nodes.Item
                            (List  => Nodes,
                             Index => I));

         if I /= Count - 1 then
            Res := Res & ASCII.LF & ASCII.LF;
         end if;
      end loop;

      return To_String (Res);
   end Get_Str;

end Cspec.Generators;
