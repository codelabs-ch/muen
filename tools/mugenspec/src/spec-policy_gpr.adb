--
--  Copyright (C) 2014-2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014-2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Mutools.Templates;

with String_Templates;

package body Spec.Policy_Gpr
is

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Features : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/features/*");
      Configs  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/config/*");

      --  Create string representation of system config.
      function Get_Config return String;

      --  Create features string.
      function Get_Features return String;

      ----------------------------------------------------------------------

      function Get_Config return String
      is
         use Ada.Strings.Unbounded;

         Result : Unbounded_String;
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Configs) - 1 loop
            declare
               Cfg_Node : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item
                   (List  => Configs,
                    Index => I);
               Cfg_Type : constant String
                 := Mutools.Utils.Capitalize
                   (Str => DOM.Core.Nodes.Node_Name (N => Cfg_Node));
               Name     : constant String
                 := Mutools.Utils.To_Ada_Identifier
                   (Str => DOM.Core.Elements.Get_Attribute
                      (Elem => Cfg_Node,
                       Name => "name"));
               Value    : constant String
                 := DOM.Core.Elements.Get_Attribute
                     (Elem => Cfg_Node,
                      Name => "value");
            begin
               Result := Result & Mutools.Utils.Indent & Name
                 & " : " & Cfg_Type & " := """ & Value & """;" & ASCII.LF;
            end;
         end loop;

         return To_String (Result);
      end Get_Config;

      ----------------------------------------------------------------------

      function Get_Features return String
      is
         use Ada.Strings.Unbounded;

         Result : Unbounded_String;
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => Features) - 1 loop
            declare
               Feature : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item
                   (List  => Features,
                    Index => I);
               Name    : constant String
                 := Mutools.Utils.Capitalize
                   (Str => DOM.Core.Nodes.Node_Name (N => Feature));
               Status  : constant String
                 := (if DOM.Core.Elements.Get_Attribute
                     (Elem => Feature,
                      Name => "enabled") = "true"
                     then "enabled" else "disabled");
            begin
               Result := Result & Mutools.Utils.Indent & Name
                 & "_Support : Feature_Type := """ & Status & """;"
                 & ASCII.LF;
            end;
         end loop;

         return To_String (Result);
      end Get_Features;

      Filename : constant String := Output_Dir & "/" & "policy.gpr";
      Tmpl     : Mutools.Templates.Template_Type;
   begin
      Mulog.Log (Msg => "Writing policy project file to '" & Filename & "'");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.policy_gpr);

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__features__",
         Content  => Get_Features);

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__config__",
         Content  => Get_Config);

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Filename);
   end Write;

end Spec.Policy_Gpr;
