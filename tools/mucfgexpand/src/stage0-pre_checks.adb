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

with Mucfgcheck.Platform;
with Mucfgcheck.Validation_Errors;

with Cfgchecks;

package body Stage0.Pre_Checks
is

   package MP renames Mucfgcheck.Platform;

   -------------------------------------------------------------------------

   procedure Clear
   is
   begin
      Check_Procs.Clear;
   end Clear;

   -------------------------------------------------------------------------

   function Get_Count return Natural renames Check_Procs.Get_Count;

   -------------------------------------------------------------------------

   procedure Register_All (Data : Muxml.XML_Data_Type)
   is
      use Cfgchecks;

      pragma Unreferenced (Data);
   begin
      Check_Procs.Register
        (Process => Library_Name_Uniqueness'Access);
      Check_Procs.Register
        (Process => Component_Library_References'Access);
      Check_Procs.Register
        (Process => Component_Library_Cyclic_References'Access);
      Check_Procs.Register
        (Process => MP.Alias_Physical_Device_References'Access);
      Check_Procs.Register
        (Process => MP.Alias_Physical_Device_Resource_References'Access);
      Check_Procs.Register
        (Process => MP.Class_Physical_Device_References'Access);
      Check_Procs.Register
        (Process => MP.Subject_Alias_Resource_References'Access);
   end Register_All;

   -------------------------------------------------------------------------

   procedure Run (Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Procs.Run (Data => Data);
      Mucfgcheck.Validation_Errors.Check;
   end Run;

end Stage0.Pre_Checks;
