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

package Expanders.Config
is

   Kernel_Text_Section_Addr         : constant := 16#0010_0000#;
   Kernel_Text_Section_Size         : constant := 16#0001_1000#;
   Kernel_Data_Section_Addr         : constant := 16#0012_0000#;
   Kernel_Data_Section_Size         : constant := 16#0000_2000#;
   Kernel_BSS_Section_Addr          : constant := 16#0012_2000#;
   Kernel_BSS_Section_Size          : constant := 16#0000_4000#;
   Kernel_Stack_Addr                : constant := 16#0012_7000#;
   Kernel_Stack_Size                : constant := 16#0000_1000#;
   Kernel_Interrupt_Stack_Addr      : constant := 16#0012_9000#;
   Kernel_Interrupt_Stack_Size      : constant := 16#0000_1000#;
   Kernel_Global_Data_Section_Addr  : constant := 16#0012_b000#;
   Kernel_Global_Data_Section_Size  : constant := 16#0000_1000#;
   Kernel_RO_Section_Addr           : constant := 16#0012_f000#;
   Kernel_RO_Section_Size           : constant := 16#002c_0000#;
   Tau0_Interface_Virtual_Addr      : constant := 16#003f_f000#;
   Crash_Audit_Virtual_Addr         : constant := 16#0040_0000#;
   Kernel_Devices_Virtual_Addr      : constant := 16#0050_0000#;
   Subject_States_Virtual_Addr      : constant := 16#0060_0000#;
   Subject_Timed_Event_Virtual_Addr : constant := 16#0070_0000#;
   Subject_Interrupts_Virtual_Addr  : constant := 16#0080_0000#;
   Subject_MSR_Store_Virtual_Addr   : constant := 16#0090_0000#;
   Subject_VMCS_Virtual_Addr        : constant := 16#00a0_0000#;
   Sched_Group_Info_Virtual_Addr    : constant := 16#00b0_0000#;
   Subject_FPU_States_Virtual_Addr  : constant := 16#00c0_0000#;

   Subject_Sinfo_Region_Size        : constant := 16#8000#;
   Sched_Group_Info_Region_Size     : constant := 16#1000#;

   Subject_Loader_Source_Base_Addr  : constant := 16#7000_0000_0000#;

end Expanders.Config;
