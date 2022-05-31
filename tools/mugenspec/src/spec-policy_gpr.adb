--
--  Copyright (C) 2014-2020  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014-2020  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Characters.Handling;
with Ada.Strings.Unbounded;

with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Muxml.Utils;

with Mulog;
with Mutools.Templates;
with Mutools.Types;

with String_Templates;

package body Spec.Policy_Gpr
is

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Filename : constant String := Output_Dir & "/" & "policy.gpr";
      Tmpl     : Mutools.Templates.Template_Type;

      --  Returns the configured debug device type as string.
      function Get_Debug_Device_Type return String;

      --  Returns all valid diagnostics device kinds as string.
      function Get_Diagnostics_Kind return String;

      --  Returns the string "True" if the scheduling plan has multiple major
      --  frames and "False" otherwise.
      function Has_Multiple_Major_Frames return String;

      ----------------------------------------------------------------------

      function Get_Debug_Device_Type return String
      is
         Prefix : constant String := "Debug_Type : Diagnostics_Kind := """;
         Diagnostics_Type : constant String
           := Muxml.Utils.Get_Attribute
             (Doc   => Policy.Doc,
              XPath => "/system/platform/kernelDiagnostics",
              Name  => "type");
      begin
         return Indent & Prefix & Ada.Characters.Handling.To_Lower
           (Item => Diagnostics_Type) & """;" & ASCII.LF;
      end Get_Debug_Device_Type;

      ----------------------------------------------------------------------

      function Get_Diagnostics_Kind return String
      is
         use Ada.Strings.Unbounded;
         use type Mutools.Types.Kernel_Diagnostics_Kind;

         Buf : Unbounded_String;
      begin
         for K in Mutools.Types.Kernel_Diagnostics_Kind loop
            if K /= Mutools.Types.Kernel_Diagnostics_Kind'First then
               Buf := Buf & ",";
            end if;
            Buf := Buf & """" & Ada.Characters.Handling.To_Lower (K'Img)
              & """";
         end loop;

         return To_String (Source => Buf);
      end Get_Diagnostics_Kind;

      ----------------------------------------------------------------------

      function Has_Multiple_Major_Frames return String
      is
         Major_Frames : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Policy.Doc,
              XPath => "/system/scheduling/majorFrame");
         Multiple_Major_Frames_Present : constant Boolean
           := DOM.Core.Nodes.Length (List => Major_Frames) > 1;
      begin
         return Mutools.Utils.To_Ada_Identifier
           (Str => Multiple_Major_Frames_Present'Img);
      end Has_Multiple_Major_Frames;
   begin
      Mulog.Log (Msg => "Writing policy project file to '" & Filename & "'");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.policy_gpr);

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__diagnostics_kind__",
         Content  => Get_Diagnostics_Kind);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__debug_device_type__",
         Content  => Get_Debug_Device_Type);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__multiple_major_frames__",
         Content  => Has_Multiple_Major_Frames);
      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Filename);
   end Write;

end Spec.Policy_Gpr;
