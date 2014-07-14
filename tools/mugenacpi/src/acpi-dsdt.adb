--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Mutools.Utils;
with Mutools.Templates;

with Muxml.Utils;

with String_Templates;

package body Acpi.DSDT
is

   -------------------------------------------------------------------------

   procedure Write
     (Policy   : Muxml.XML_Data_Type;
      Subject  : DOM.Core.Node;
      Filename : String)
   is
      pragma Unreferenced (Subject);

      Dsl_File : String := Filename;
      Tmpl     : Mutools.Templates.Template_Type;
   begin
      Dsl_File (Dsl_File'Last - 3 .. Dsl_File'Last) := ".dsl";

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.linux_dsdt_dsl);

      Set_PCI_Cfg_Space_Address :
      declare
         PCI_Cfg_Addr_Str : constant String := Muxml.Utils.Get_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/platform/devices",
            Name  => "pciConfigAddress");
         PCI_Cfg_Addr     : Interfaces.Unsigned_64 := 0;
      begin
         if PCI_Cfg_Addr_Str'Length > 0 then
            PCI_Cfg_Addr := Interfaces.Unsigned_64'Value (PCI_Cfg_Addr_Str);
         end if;

         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__config_base_address__",
            Content  => Mutools.Utils.To_Hex
              (Number     => PCI_Cfg_Addr,
               Normalize  => False));
      end Set_PCI_Cfg_Space_Address;

      Mutools.Templates.Write (Template => Tmpl,
                               Filename => Dsl_File);
   end Write;

end Acpi.DSDT;
