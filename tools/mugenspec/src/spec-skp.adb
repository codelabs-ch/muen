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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.XML_Utils;
with Mutools.Templates;

with String_Templates;

package body Spec.Skp
is

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      use Ada.Strings.Unbounded;
      use Interfaces;

      S_Count    : constant Natural := DOM.Core.Nodes.Length
        (List => McKae.XML.XPath.XIA.XPath_Query
           (N     => Policy.Doc,
            XPath => "/system/subjects/subject"));
      CPU_Count  : constant Natural
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => Policy);
      CPU_Nodes  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/hardware/processor/cpu");
      VMXON_Addr : constant Unsigned_64 := Unsigned_64'Value
        (Muxml.Utils.Get_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/memory/memory[@type='system_vmxon' and "
            & "contains(string(@name),'kernel_0')]",
            Name  => "physicalAddress"));

      Tmpl : Mutools.Templates.Template_Type;
   begin
      Mulog.Log (Msg => "Writing system spec to '" & Output_Dir & "/skp.ads'");

      Tmpl := Mutools.Templates.Create (Content => String_Templates.skp_ads);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__cpu_count__",
         Content  => Ada.Strings.Fixed.Trim
           (Source => CPU_Count'Img,
            Side   => Ada.Strings.Left));
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__subj_range__",
         Content  => "0 .."  & Positive'Image (S_Count - 1));
      Mutools.Templates.Replace (Template => Tmpl,
                                 Pattern  => "__vmxon_addr__",
                                 Content  => Mutools.Utils.To_Hex
                                   (Number => VMXON_Addr));

      declare
         Mapping   : array (0 .. CPU_Count - 1) of Natural;
         Predicate : Unbounded_String;
         Array_Str : Unbounded_String := To_Unbounded_String
           (Indent (N => 2));
      begin
         for I in 0 .. CPU_Count - 1 loop
            declare
               Node    : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item
                   (List  => CPU_Nodes,
                    Index => I);
               APIC_ID : constant Natural
                 := Natural'Value
                   (DOM.Core.Elements.Get_Attribute
                      (Elem => Node,
                       Name => "apicId"));
               CPU_ID : constant Natural
                 := Natural'Value
                   (DOM.Core.Elements.Get_Attribute
                      (Elem => Node,
                       Name => "cpuId"));
            begin
               Mapping (CPU_ID) := APIC_ID;
            end;
         end loop;

         for I in Mapping'Range loop
            declare
               APIC_ID_Str : constant String
                 := Ada.Strings.Fixed.Trim
                   (Source => Mapping (I)'Img,
                    Side   => Ada.Strings.Left);
            begin
               Predicate := Predicate & APIC_ID_Str;
               Array_Str := Array_Str & APIC_ID_Str;
               if I < CPU_Count - 1 then
                  Predicate := Predicate & " | ";
                  Array_Str := Array_Str & ", ";
               end if;
            end;
         end loop;

         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__valid_apic_ids__",
            Content  => To_String (Predicate));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__cpu_to_apic_id__",
            Content  => To_String (Array_Str));
      end;

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Dir & "/skp.ads");
   end Write;

end Spec.Skp;
