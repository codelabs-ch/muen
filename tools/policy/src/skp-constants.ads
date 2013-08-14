--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Skp.Constants
is

   --  Pin-Base VM-Execution Controls

   VM_CTRL_EXT_INT_EXITING : constant := 16#0000_0001#;
   VM_CTRL_NMI_EXITING     : constant := 16#0000_0008#;
   VM_CTRL_PREEMPT_TIMER   : constant := 16#0000_0040#;

   --  Primary Processor-Based VM-Execution Controls

   VM_CTRL_EXIT_HLT        : constant := 16#0000_0080#;
   VM_CTRL_EXIT_INVLPG     : constant := 16#0000_0200#;
   VM_CTRL_EXIT_MWAIT      : constant := 16#0000_0400#;
   VM_CTRL_EXIT_RDPMC      : constant := 16#0000_0800#;
   VM_CTRL_EXIT_RDTSC      : constant := 16#0000_1000#;
   VM_CTRL_EXIT_CR3_LOAD   : constant := 16#0000_8000#;
   VM_CTRL_EXIT_CR3_STORE  : constant := 16#0001_0000#;
   VM_CTRL_EXIT_CR8_LOAD   : constant := 16#0008_0000#;
   VM_CTRL_EXIT_CR8_STORE  : constant := 16#0010_0000#;
   VM_CTRL_EXIT_MOV_DR     : constant := 16#0080_0000#;
   VM_CTRL_IO_BITMAPS      : constant := 16#0200_0000#;
   VM_CTRL_MSR_BITMAPS     : constant := 16#1000_0000#;
   VM_CTRL_EXIT_MONITOR    : constant := 16#2000_0000#;
   VM_CTRL_SECONDARY_PROC  : constant := 16#8000_0000#;

   --  Secondary Processor-Based VM-Execution Controls

   VM_CTRL_ENABLE_EPT      : constant := 16#0000_0002#;
   VM_CTRL_EXIT_DT         : constant := 16#0000_0004#;
   VM_CTRL_EXIT_WBINVD     : constant := 16#0000_0040#;
   VM_CTRL_UNRESTRICTED    : constant := 16#0000_0080#;

   --  VM-Exit Controls

   VM_CTRL_EXIT_IA32E_MODE : constant := 16#0000_0200#;
   VM_CTRL_EXIT_ACK_INT    : constant := 16#0000_8000#;
   VM_CTRL_EXIT_SAVE_EFER  : constant := 16#0010_0000#;
   VM_CTRL_EXIT_LOAD_EFER  : constant := 16#0020_0000#;
   VM_CTRL_EXIT_SAVE_TIMER : constant := 16#0040_0000#;

   --  VM-Entry Controls

   VM_CTRL_ENTR_IA32E_MODE : constant := 16#0000_0200#;
   VM_CTRL_ENTR_LOAD_EFER  : constant := 16#0000_8000#;

end Skp.Constants;
