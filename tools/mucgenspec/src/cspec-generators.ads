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

with Muxml;

package Cspec.Generators
is

   --  Convert memory elements of given component to string representation. An
   --  empty string is returned if the component specifies no memory resources.
   function Get_Memory_Str (Spec : Muxml.XML_Data_Type) return String;

   --  Convert channel elements of given component to string representation. An
   --  empty string is returned if the component specifies no channel
   --  resources.
   function Get_Channels_Str (Spec : Muxml.XML_Data_Type) return String;

   --  Convert device elements of given component to string representation. An
   --  empty string is returned if the component specifies no device resources.
   function Get_Devices_Str (Spec : Muxml.XML_Data_Type) return String;

   --  Convert event elements of given component to string representation. An
   --  empty string is returned if the component specifies no event resources.
   function Get_Event_Str (Spec : Muxml.XML_Data_Type) return String;

   --  Convert memory array elements of given component to string
   --  representation. An empty string is returned if the component specifies
   --  no memory array resources.
   function Get_Memory_Arrays_Str (Spec : Muxml.XML_Data_Type) return String;

   --  Convert channel array elements of given component to string
   --  representation. An empty string is returned if the component specifies
   --  no channel array resources.
   function Get_Channel_Arrays_Str (Spec : Muxml.XML_Data_Type) return String;

   --  Convert config variables of given component to string
   --  representation. An empty string is returned if the component has no
   --  config variables.
   function Get_Config_Str (Spec : Muxml.XML_Data_Type) return String;

end Cspec.Generators;
