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

package Expanders.Device_Domains
is

   --  Add section skeleton.
   procedure Add_Section_Skeleton (Data : in out Muxml.XML_Data_Type);

   --  Expand domain IDs (DIDs).
   procedure Add_Domain_IDs (Data : in out Muxml.XML_Data_Type);

   --  Add VT-d tables (root, context, address translation and IR tables).
   procedure Add_Tables (Data : in out Muxml.XML_Data_Type);

   --  Map memory of subjects referenced by map subject memory directive.
   procedure Map_Subject_Memory (Data : in out Muxml.XML_Data_Type);

   --  Add mappings for reserved memory regions of referenced devices.
   procedure Add_Reserved_Memory_Region_Mappings
     (Data : in out Muxml.XML_Data_Type);

   --  Remove map reserved memory region attribute from policy.
   procedure Remove_Map_Reserved_Mem_Attribute
     (Data : in out Muxml.XML_Data_Type);

end Expanders.Device_Domains;
