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

with Ada.Directories;
with Ada.Characters.Handling;

with Mulog;
with Mutools.Utils;
with Mutools.Templates;

with String_Templates;

package body Cspec
is

   -------------------------------------------------------------------------

   procedure Run
     (Policy_File      : String;
      Component_Name   : String;
      Output_Directory : String)
   is
      pragma Unreferenced (Policy_File);

      Tmpl : Mutools.Templates.Template_Type;
   begin
      Mulog.Log (Msg => "Generating '" & Component_Name & "' component specs "
                 & "in '" & Output_Directory & "' directory");

      if not Ada.Directories.Exists (Name => Output_Directory) then
         Ada.Directories.Create_Path (New_Directory => Output_Directory);
      end if;

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.component_name_component_ads);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__component_name__",
         Content  => Mutools.Utils.Capitalize (Str => Component_Name));
      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Directory & "/"
         & Ada.Characters.Handling.To_Lower (Item => Component_Name)
         & "_component.ads");
   end Run;

end Cspec;
