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

with Interfaces;

with DOM.Core.Elements;

with Mulog;
with Muxml.Utils;
with Mutools.Templates;

with String_Templates;

package body Spec.Skp_Hardware
is

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Tmpl : Mutools.Templates.Template_Type;

      MMConf_Base_Addr_Str : constant String
        := Muxml.Utils.Get_Attribute
          (Doc   => Policy.Doc,
           XPath => "/system/hardware/devices",
           Name  => "pciConfigAddress");

      --  Write I/O port constant for debug console.
      procedure Write_Debugconsole;

      ----------------------------------------------------------------------

      procedure Write_Debugconsole
      is
         Logical_Dev    : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Policy.Doc,
              XPath => "/system/kernel/devices/device"
              & "[@logical='debugconsole']");
         Phys_Dev_Name  : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Logical_Dev,
              Name => "physical");
         Phys_Port_Name : constant String
           := Muxml.Utils.Get_Attribute
             (Doc   => Logical_Dev,
              XPath => "ioPort",
              Name  => "physical");
         Phys_Address   : constant String
           := Muxml.Utils.Get_Attribute
             (Doc   => Policy.Doc,
              XPath => "/system/hardware/devices/device"
              & "[@name='" & Phys_Dev_Name & "']/ioPort[@name='"
              & Phys_Port_Name & "']",
              Name  => "start");
      begin
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__debug_console_port__",
            Content  => Phys_Address);
      end Write_Debugconsole;
   begin
      Mulog.Log (Msg => "Writing hardware spec to '"
                 & Output_Dir & "/skp-hardware.ads'");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_hardware_ads);

      Write_Debugconsole;

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__mmconf_base_addr__",
         Content  =>
           (if MMConf_Base_Addr_Str'Length > 0 then MMConf_Base_Addr_Str
            else Mutools.Utils.To_Hex
              (Number => Interfaces.Unsigned_64'Last)));

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Dir & "/skp-hardware.ads");
   end Write;

end Spec.Skp_Hardware;
