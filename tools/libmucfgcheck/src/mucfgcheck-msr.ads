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

with Muxml;

package Mucfgcheck.MSR
is

   --  Validate that all MSR start addresses are smaller than end addresses.
   procedure Start_Smaller_End (XML_Data : Muxml.XML_Data_Type);

   --  Validate that all MSR start and addresses are either low or high.
   procedure Low_Or_High (XML_Data : Muxml.XML_Data_Type);

   --  Validate that subject MSRs are in the allowed list:
   --  - IA32_SYSENTER_CS/ESP/EIP
   --  - IA32_DEBUGCTL
   --  - IA32_EFER/STAR/LSTAR/CSTAR/FMASK
   --  - IA32_FS_BASE/GS_BASE/KERNEL_GS_BASE
   procedure Check_Whitelist (XML_Data : Muxml.XML_Data_Type);

end Mucfgcheck.MSR;
