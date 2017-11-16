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
     (Spec  : Muxml.XML_Data_Type;
      Func  : Specific_String_Getter;
      XPath : String)
      return String;

   -------------------------------------------------------------------------

   function Get_Channel_Arrays_Str (Spec : Muxml.XML_Data_Type) return String
   is
   begin
      return Get_Str
        (Spec  => Spec,
         Func  => Utils.To_Channel_Array_Str'Access,
         XPath => "*[self::component or self::library]/requires/"
         & "channels/array");
   end Get_Channel_Arrays_Str;

   -------------------------------------------------------------------------

   function Get_Channels_Str (Spec : Muxml.XML_Data_Type) return String
   is
   begin
      return Get_Str
        (Spec  => Spec,
         Func  => Utils.To_Channel_Str'Access,
         XPath => "*[self::component or self::library]/requires/channels/*"
         & "[self::reader or self::writer]");
   end Get_Channels_Str;

   -------------------------------------------------------------------------

   function Get_Config_Str (Spec : Muxml.XML_Data_Type) return String
   is
   begin
      return Get_Str
        (Spec  => Spec,
         Func  => Utils.To_Config_Variable_Str'Access,
         XPath => "*[self::component or self::library]/config/*");
   end Get_Config_Str;

   -------------------------------------------------------------------------

   function Get_Devices_Str (Spec : Muxml.XML_Data_Type) return String
   is
   begin
      return Get_Str
        (Spec  => Spec,
         Func  => Utils.To_Device_Str'Access,
         XPath => "*[self::component or self::library]/requires/devices/*");
   end Get_Devices_Str;

   -------------------------------------------------------------------------

   function Get_Memory_Arrays_Str (Spec : Muxml.XML_Data_Type) return String
   is
   begin
      return Get_Str
        (Spec  => Spec,
         Func  => Utils.To_Memory_Array_Str'Access,
         XPath => "*[self::component or self::library]/requires/memory/array");
   end Get_Memory_Arrays_Str;

   -------------------------------------------------------------------------

   function Get_Memory_Str (Spec : Muxml.XML_Data_Type) return String
   is
   begin
      return Get_Str
        (Spec  => Spec,
         Func  => Utils.To_Memory_Str'Access,
         XPath => "*[self::component or self::library]/requires/"
         & "memory/memory");
   end Get_Memory_Str;

   -------------------------------------------------------------------------

   function Get_Str
     (Spec  : Muxml.XML_Data_Type;
      Func  : Specific_String_Getter;
      XPath : String)
      return String
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Spec.Doc,
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
