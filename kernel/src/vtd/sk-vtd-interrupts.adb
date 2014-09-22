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

with Skp.Interrupts;

with SK.Dump;

package body SK.VTd.Interrupts
is

   -------------------------------------------------------------------------

   procedure Setup_IRQ_Routing (VTd_Enabled : Boolean)
   is
      use type Skp.Dst_Vector_Range;

      Route    : Skp.Interrupts.IRQ_Route_Type;
      Dest_ID  : SK.Byte;
      Shiftpos : Natural;
   begin
      for I in Skp.Interrupts.Routing_Range loop
         Route := Skp.Interrupts.IRQ_Routing (I);

         if VTd_Enabled then

            --  See Intel VT-d specification, section 5.5.1.

            Shiftpos := 49;
            Dest_ID  := Route.IRQ;
         else

            --  See Intel IOAPIC specification, section 3.2.4.

            Shiftpos := 56;
            Dest_ID  := CPU_Registry.Get_APIC_ID (CPU_ID => Route.CPU);
         end if;

         pragma Debug (Dump.Print_IRQ_Routing
                       (IRQ         => Route.IRQ,
                        Vector      => SK.Byte (Route.Vector),
                        CPU_ID      => SK.Byte (Route.CPU),
                        Dest_ID     => Dest_ID,
                        VTd_Enabled => VTd_Enabled));

         if Route.Vector /= Skp.Invalid_Vector then
            IO_Apic.Route_IRQ
              (IRQ            => Route.IRQ,
               Vector         => SK.Byte (Route.Vector),
               Trigger_Mode   => Route.IRQ_Mode,
               Trigger_Level  => Route.IRQ_Level,
               Destination_Id => SK.Word64 (Dest_ID) * 2 ** Shiftpos);
         end if;
      end loop;
   end Setup_IRQ_Routing;

end SK.VTd.Interrupts;
