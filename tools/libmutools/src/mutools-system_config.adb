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

with Muxml.Utils;

package body Mutools.System_Config
is

   -------------------------------------------------------------------------

   function Get_Value
     (Data : Muxml.XML_Data_Type;
      Name : String)
      return Boolean
   is
      Val_Str : constant String
        := Muxml.Utils.Get_Attribute
          (Doc   => Data.Doc,
           XPath => "/system/config/boolean[@name='" & Name & "']",
           Name  => "value");
   begin
      if Val_Str'Length = 0 then
         raise Not_Found with "No boolean config option '" & Name & "' found";
      end if;

      return Boolean'Value (Val_Str);
   end Get_Value;

end Mutools.System_Config;
