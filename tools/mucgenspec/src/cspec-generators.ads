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

   --  Convert memory elements of component given by name to string
   --  representation. An empty string is returned if the component specifies
   --  no memory resources.
   function Get_Memory_Str
     (Policy    : Muxml.XML_Data_Type;
      Comp_Name : String)
      return String;

   --  Convert channel elements of component given by name to string
   --  representation. An empty string is returned if the component specifies
   --  no channel resources.
   function Get_Channels_Str
     (Policy    : Muxml.XML_Data_Type;
      Comp_Name : String)
      return String;

end Cspec.Generators;
