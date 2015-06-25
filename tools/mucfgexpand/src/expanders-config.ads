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

package Expanders.Config
is

   Kernel_Data_Section_Size    : constant := 16#0000_2000#;
   Kernel_Data_Section_Addr    : constant := 16#0011_0000#;
   Kernel_BSS_Section_Size     : constant := 16#0000_1000#;
   Kernel_BSS_Section_Addr     : constant := 16#0011_8000#;
   Kernel_RO_Section_Size      : constant := 16#0002_1000#;
   Kernel_RO_Section_Addr      : constant := 16#0011_f000#;
   Kernel_Stack_Size           : constant := 16#0000_2000#;
   Kernel_Stack_Addr           : constant := 16#0011_3000#;
   Kernel_Store_Size           : constant := 16#0000_1000#;
   Kernel_Store_Addr           : constant := 16#0011_6000#;
   VTd_IRT_Virtual_Addr        : constant := 16#001f_e000#;
   Tau0_Interface_Virtual_Addr : constant := 16#001f_f000#;
   Kernel_Devices_Virtual_Addr : constant := 16#0020_0000#;
   Subject_States_Virtual_Addr : constant := 16#0030_0000#;
   Subject_Timers_Virtual_Addr : constant := 16#0040_0000#;

end Expanders.Config;
