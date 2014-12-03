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

package Expanders.Kernel
is

   --  Add kernel section skeleton.
   procedure Add_Section_Skeleton (Data : in out Muxml.XML_Data_Type);

   --  Add kernel memory mappings (binary, stack and store).
   procedure Add_Binary_Mappings (Data : in out Muxml.XML_Data_Type);

   --  Add subject state memory mappings.
   procedure Add_Subj_State_Mappings (Data : in out Muxml.XML_Data_Type);

   --  Add subject timer page mappings.
   procedure Add_Subj_Timer_Mappings (Data : in out Muxml.XML_Data_Type);

   --  Map tau0 interface memory region on BSP.
   procedure Map_Tau0_Interface (Data : in out Muxml.XML_Data_Type);

   --  Add devices to kernel section.
   procedure Add_Devices (Data : in out Muxml.XML_Data_Type);

end Expanders.Kernel;
