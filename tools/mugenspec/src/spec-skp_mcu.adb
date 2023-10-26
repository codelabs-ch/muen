--
--  Copyright (C) 2023  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2023  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

with DOM.Core.Elements;

with Mulog;
with Muxml.Utils;
with Mutools.Templates;

with String_Templates;

package body Spec.Skp_MCU
is

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Ucode_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Policy.Doc,
           XPath => "/system/kernel/memory/cpu[@id='0']/"
           & "memory[@logical='microcode']");
      Ucode_Addr : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64'Value
          (DOM.Core.Elements.Get_Attribute
             (Elem => Ucode_Node,
              Name => "virtualAddress"));

      Tmpl : Mutools.Templates.Template_Type;
      Pkg  : constant String := Output_Dir & "/skp-mcu.ads";
   begin
      Mulog.Log (Msg => "Writing MCU spec to '" & Pkg & "'");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_mcu_ads);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__ucode_addr__",
         Content  => Mutools.Utils.To_Hex (Number => Ucode_Addr));

      Mutools. Templates.Write
        (Template => Tmpl,
         Filename => Pkg);
   end Write;

end Spec.Skp_MCU;
