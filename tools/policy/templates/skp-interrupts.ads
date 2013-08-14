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

with SK;

--# inherit SK, Skp;
package Skp.Interrupts is

   subtype Remapped_Vector_Type is SK.Byte range 32 .. 255;

   type IRQ_Route_Type is record
      CPU    : Skp.CPU_Range;
      IRQ    : SK.Byte;
      Vector : Remapped_Vector_Type;
   end record;

   Null_IRQ_Route : constant IRQ_Route_Type := IRQ_Route_Type'
     (CPU    => 0,
      IRQ    => 0,
      Vector => 255);

   type Routing_Range is range __routing_range__;

   type IRQ_Routing_Array is array (Routing_Range) of IRQ_Route_Type;

   IRQ_Routing : constant IRQ_Routing_Array := IRQ_Routing_Array'(
__irq_routing_table__);

   type Vector_Routing_Array is array (Remapped_Vector_Type)
     of Skp.Dst_Subject_Type;

   Vector_Routing : constant Vector_Routing_Array := Vector_Routing_Array'(
__vector_routing_table__);

end Skp.Interrupts;
