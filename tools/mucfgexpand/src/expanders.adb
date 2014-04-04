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

with Mutools.Processors;

with Expanders.Memory;
with Expanders.Kernel;
with Expanders.Subjects;

pragma Elaborate_All (Mutools.Processors);

package body Expanders
is

   package Procs is new Mutools.Processors
     (Param_Type => Muxml.XML_Data_Type);

   -------------------------------------------------------------------------

   function Get_Count return Natural renames Procs.Get_Count;

   -------------------------------------------------------------------------

   procedure Register_All
   is
   begin
      Procs.Register (Process => Memory.Add_Alignment'Access);
      Procs.Register (Process => Memory.Add_Kernel_Binary'Access);
      Procs.Register (Process => Memory.Add_Stack_Store'Access);
      Procs.Register (Process => Memory.Add_Subject_States'Access);
      Procs.Register (Process => Memory.Add_Tau0_Interface'Access);
      Procs.Register (Process => Memory.Add_AP_Trampoline'Access);
      Procs.Register (Process => Memory.Add_VMXON_Regions'Access);
      Procs.Register (Process => Memory.Add_VMCS_Regions'Access);
      Procs.Register (Process => Memory.Add_Subject_Bitmaps'Access);
      Procs.Register (Process => Kernel.Add_Section_Skeleton'Access);
      Procs.Register (Process => Kernel.Add_Binary_Mappings'Access);
      Procs.Register (Process => Kernel.Add_Subj_State_Mappings'Access);
      Procs.Register (Process => Kernel.Map_Tau0_Interface'Access);
      Procs.Register (Process => Kernel.Add_Devices'Access);
      Procs.Register (Process => Subjects.Add_Binaries'Access);

      --  All kernel/subject memory regions and mappings must exist and specify
      --  and alignment to add PTs.

      Procs.Register (Process => Memory.Add_Kernel_PTs'Access);
      Procs.Register (Process => Memory.Add_Subject_PTs'Access);
   end Register_All;

   -------------------------------------------------------------------------

   procedure Run (Data : in out Muxml.XML_Data_Type) renames Procs.Run;

end Expanders;
