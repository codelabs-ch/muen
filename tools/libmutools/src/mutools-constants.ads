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

package Mutools.Constants
is

   --  Size of one memory page in bytes.
   Page_Size : constant := 4096;

   --  Model-Specific Registers, see Intel SDM Vol. 4, "Chapter 2
   --  Model-Specific Registers (MSRs)".
   IA32_SYSENTER_CS          : constant := 16#0000_0174#;
   IA32_SYSENTER_ESP         : constant := 16#0000_0175#;
   IA32_SYSENTER_EIP         : constant := 16#0000_0176#;
   IA32_DEBUGCTL             : constant := 16#0000_01d9#;
   IA32_PAT                  : constant := 16#0000_0277#;
   IA32_PERF_GLOBAL_CTRL     : constant := 16#0000_038f#;
   IA32_EFER                 : constant := 16#c000_0080#;
   IA32_STAR                 : constant := 16#c000_0081#;
   IA32_LSTAR                : constant := 16#c000_0082#;
   IA32_CSTAR                : constant := 16#c000_0083#;
   IA32_FMASK                : constant := 16#c000_0084#;
   IA32_FS_BASE              : constant := 16#c000_0100#;
   IA32_GS_BASE              : constant := 16#c000_0101#;
   IA32_KERNEL_GS_BASE       : constant := 16#c000_0102#;

   --  Power/Thermal MSRs

   MSR_PLATFORM_INFO         : constant := 16#0000_00ce#;
   IA32_THERM_STATUS         : constant := 16#0000_019c#;
   IA32_TEMPERATURE_TARGET   : constant := 16#0000_01a2#;
   IA32_PACKAGE_THERM_STATUS : constant := 16#0000_01b1#;
   MSR_RAPL_POWER_UNIT       : constant := 16#0000_0606#;
   MSR_PKG_POWER_LIMIT       : constant := 16#0000_0610#;
   MSR_PKG_ENERGY_STATUS     : constant := 16#0000_0611#;
   MSR_DRAM_ENERGY_STATUS    : constant := 16#0000_0619#;
   MSR_PP1_ENERGY_STATUS     : constant := 16#0000_0641#;
   MSR_CONFIG_TDP_CONTROL    : constant := 16#0000_064b#;
   IA32_PM_ENABLE            : constant := 16#0000_0770#;
   IA32_HWP_CAPABILITIES     : constant := 16#0000_0771#;
   IA32_HWP_REQUEST          : constant := 16#0000_0774#;

   --  Size of a single MSR-Store entry in bytes, see Intel SDM Vol. 3C,
   --  "24.7.2 VM-Exit Controls for MSRs".
   MSR_Store_Entry_Size : constant := 16;

   --  Device IRQ to host vector remapping offset.
   Host_IRQ_Remap_Offset : constant := 32;

   --  Largest valid hardware IRQ number.
   Hardware_Max_IRQ_Number : constant := 220;

   --  Muen kernel entry point address.
   Kernel_Entry_Point : constant := 16#0010_0000#;

   --  Base address of subject PCI config space.
   Subject_PCI_Config_Space_Addr : constant := 16#f800_0000#;

   --  Number of bits for the subject event range. The total number of
   --  available events is given by 2 ** Event_Bits.
   Event_Bits            : constant := 6;
   Max_Valid_Vmcall_ID   : constant := 2 ** Event_Bits - 1;
   Max_Valid_VMX_Exit_ID : constant := 59;

end Mutools.Constants;
