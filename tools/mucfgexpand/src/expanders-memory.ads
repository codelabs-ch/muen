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

package Expanders.Memory
is

   --  Add kernel binary memory elements.
   procedure Add_Kernel_Binary (Data : in out Muxml.XML_Data_Type);

   --  Add kernel pagetable memory elements.
   procedure Add_Kernel_PTs (Data : in out Muxml.XML_Data_Type);

   --  Add subject pagetable memory elements.
   procedure Add_Subject_PTs (Data : in out Muxml.XML_Data_Type);

   --  Add kernel stack and store memory elements.
   procedure Add_Stack_Store (Data : in out Muxml.XML_Data_Type);

   --  Add subject state memory elements.
   procedure Add_Subject_States (Data : in out Muxml.XML_Data_Type);

   --  Add subject timer page memory elements.
   procedure Add_Subject_Timer_Pages (Data : in out Muxml.XML_Data_Type);

   --  Add tau0 interface memory element.
   procedure Add_Tau0_Interface (Data : in out Muxml.XML_Data_Type);

   --  Add memory region for AP trampoline.
   procedure Add_AP_Trampoline (Data : in out Muxml.XML_Data_Type);

   --  Add VMXON regions.
   procedure Add_VMXON_Regions (Data : in out Muxml.XML_Data_Type);

   --  Add VMCS regions.
   procedure Add_VMCS_Regions (Data : in out Muxml.XML_Data_Type);

   --  Add alignment/type attribute with default value where missing.
   procedure Add_Missing_Attributes (Data : in out Muxml.XML_Data_Type);

   --  Add subject I/O and MSR bitmap memory regions.
   procedure Add_Subject_Bitmaps (Data : in out Muxml.XML_Data_Type);

   --  Add subject MSR store memory regions.
   procedure Add_Subject_MSR_Store (Data : in out Muxml.XML_Data_Type);

end Expanders.Memory;
