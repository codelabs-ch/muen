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

with Skp.Interrupts;

with SK.Dump;

package body SK.VTd.Interrupts
is

   --  Position of the destination ID in an I/O APIC RTE, see Intel IOAPIC
   --  specification, section 3.2.4.

   Dest_ID_Shiftpos : constant Natural := 56;

   -------------------------------------------------------------------------

   procedure Setup_IRQ_Routing
   is
      use type Skp.Dst_Vector_Range;

      Route   : Skp.Interrupts.IRQ_Route_Type;
      APIC_ID : SK.Byte;
   begin
      for I in Skp.Interrupts.Routing_Range loop
         Route   := Skp.Interrupts.IRQ_Routing (I);
         APIC_ID := SK.Byte (Route.APIC_ID);

         pragma Debug (Dump.Print_IRQ_Routing
                       (RTE_Idx => Route.RTE_Idx,
                        IRQ     => Route.IRQ,
                        Vector  => SK.Byte (Route.Vector),
                        APIC_ID => APIC_ID));

         if Route.Vector /= Skp.Invalid_Vector then
            IO_Apic.Route_IRQ
              (RTE_Index      => Route.RTE_Idx,
               Vector         => SK.Byte (Route.Vector),
               Trigger_Mode   => Route.IRQ_Mode,
               Trigger_Level  => Route.IRQ_Level,
               Destination_ID => SK.Word64 (APIC_ID) * 2 ** Dest_ID_Shiftpos);
         end if;
      end loop;
   end Setup_IRQ_Routing;

end SK.VTd.Interrupts;
