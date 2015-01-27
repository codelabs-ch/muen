--
--  Copyright (C) 2014, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Spec.Kernel
is

   -------------------------------------------------------------------------

   procedure Write_Project_File
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Features : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/features/*");

      --  Create features string.
      function Get_Features return String;

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

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Filename);
   end Write_Project_File;

end Spec.Kernel;
