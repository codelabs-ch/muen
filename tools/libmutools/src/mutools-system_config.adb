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

with Muxml.Utils;

package body Mutools.System_Config
is

   use type DOM.Core.Node;

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

   -------------------------------------------------------------------------

   function Has_Boolean
     (Data : Muxml.XML_Data_Type;
      Name : String)
      return Boolean
   is
      Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/config/boolean[@name='" & Name & "']");
   begin
      return Node /= null;
   end Has_Boolean;

   -------------------------------------------------------------------------

   function Has_Integer
     (Data : Muxml.XML_Data_Type;
      Name : String)
      return Boolean
   is
      Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/config/integer[@name='" & Name & "']");
   begin
      return Node /= null;
   end Has_Integer;

end Mutools.System_Config;
