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

   --  Model-Specific Registers, see Intel SDM Vol. 3C, chapter 35.
   IA32_SYSENTER_CS      : constant := 16#0000_0174#;
   IA32_SYSENTER_ESP     : constant := 16#0000_0175#;
   IA32_SYSENTER_EIP     : constant := 16#0000_0176#;
   IA32_DEBUGCTL         : constant := 16#0000_01d9#;
   IA32_PAT              : constant := 16#0000_0277#;
   IA32_PERF_GLOBAL_CTRL : constant := 16#0000_038f#;
   IA32_EFER             : constant := 16#c000_0080#;
   IA32_STAR             : constant := 16#c000_0081#;
   IA32_LSTAR            : constant := 16#c000_0082#;
   IA32_FMASK            : constant := 16#c000_0084#;
   IA32_FS_BASE          : constant := 16#c000_0100#;
   IA32_GS_BASE          : constant := 16#c000_0101#;
   IA32_KERNEL_GS_BASE   : constant := 16#c000_0102#;

   --  Size of a single MSR-Store entry in bytes, see Intel SDM Vol. 3C,
   --  table 24-11.
   MSR_Store_Entry_Size : constant := 16;

   --  Device IRQ to host vector remapping offset.
   Host_IRQ_Remap_Offset : constant := 32;

   --  Base address of subject PCI config space.
   Subject_PCI_Config_Space_Addr : constant := 16#f800_0000#;

end Mutools.Constants;
