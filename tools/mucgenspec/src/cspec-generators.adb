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

   -------------------------------------------------------------------------

   function Get_Channels_Str
     (Policy    : Muxml.XML_Data_Type;
      Comp_Name : String)
      return String
   is
      Res      : Unbounded_String;
      Channels : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/components/component[@name='" & Comp_Name
           & "']/channels/*");
      Count    : constant Natural := DOM.Core.Nodes.Length (List => Channels);
   begin
      for I in 0 .. Count - 1 loop
         Res := Res & Utils.To_Channel_Str
           (Channel => DOM.Core.Nodes.Item
              (List  => Channels,
               Index => I));

         if I /= Count - 1 then
            Res := Res & ASCII.LF & ASCII.LF;
         end if;
      end loop;

      return To_String (Res);
   end Get_Channels_Str;

end Cspec.Generators;
