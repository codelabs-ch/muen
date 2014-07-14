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

with Mutools.Templates;

with String_Templates;

package body Acpi.DSDT
is

   -------------------------------------------------------------------------

   procedure Write
     (Policy   : Muxml.XML_Data_Type;
      Subject  : DOM.Core.Node;
      Filename : String)
   is
      pragma Unreferenced (Policy, Subject);

      Dsl_File : String := Filename;
      Tmpl     : Mutools.Templates.Template_Type;
   begin
      Dsl_File (Dsl_File'Last - 3 .. Dsl_File'Last) := ".dsl";

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.linux_dsdt_dsl);

      Mutools.Templates.Write (Template => Tmpl,
                               Filename => Dsl_File);
   end Write;

end Acpi.DSDT;
