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

with Mucfgcheck.Config;

with Cfgchecks;

package body Stage1.Pre_Checks
is

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
        (Process => Mucfgcheck.Config.Name_Uniqueness'Access);
      Check_Procs.Register
        (Process => Hardware_IRQ_MSI_Name_Uniqueness'Access);
      Check_Procs.Register
        (Process => Hardware_Reserved_Memory_Region_Name_Uniqueness'Access);
      Check_Procs.Register
        (Process => Hardware_Reserved_Memory_Region_References'Access);
      Check_Procs.Register
        (Process => Device_RMRR_Domain_Assignment'Access);
      Check_Procs.Register
        (Process => Subject_Component_References'Access);
      Check_Procs.Register
        (Process => Component_Name_Uniqueness'Access);
      Check_Procs.Register
        (Process => Component_Channel_Name_Uniqueness'Access);
      Check_Procs.Register
        (Process => Subject_IRQ_MSI_Name_Uniqueness'Access);
      Check_Procs.Register
        (Process => Subject_IRQ_MSI_References'Access);
      Check_Procs.Register
        (Process => Subject_Resource_Maps_Logical_Uniqueness'Access);
      Check_Procs.Register
        (Process => Subject_Component_Resource_Mappings'Access);
      Check_Procs.Register
        (Process => Subject_Channel_Exports'Access);
      Check_Procs.Register
        (Process => Subject_Memory_Exports'Access);
      Check_Procs.Register
        (Process => Subject_Device_Exports'Access);
      Check_Procs.Register
        (Process => Subject_Event_Exports'Access);
      Check_Procs.Register
        (Process => Component_Channel_Size'Access);
      Check_Procs.Register
        (Process => Component_Memory_Size'Access);
      Check_Procs.Register
        (Process => Component_Device_Memory_Size'Access);
      Check_Procs.Register
        (Process => Component_Device_IO_Port_Range'Access);
      Check_Procs.Register
        (Process => Subject_Sibling_References'Access);
   end Register_All;

   -------------------------------------------------------------------------

   procedure Run (Data : Muxml.XML_Data_Type) renames Check_Procs.Run;

end Stage1.Pre_Checks;
