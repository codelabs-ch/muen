--
--  Copyright (C) 2013, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package SK.IO_Apic
with
   Abstract_State =>
     (State with External => (Async_Writers, Async_Readers, Effective_Writes)),
   Initializes    => State
is

   --  Setup RTE with specified vector to given destination. The destination ID
   --  is either an APIC ID or a VT-d IR IRTE index. It must be calculated by
   --  the client and is used as-is.
   procedure Route_IRQ
     (RTE_Index      : Skp.Interrupts.RTE_Index_Type;
      Vector         : SK.Byte;
      Trigger_Mode   : Skp.Interrupts.IRQ_Mode_Type;
      Trigger_Level  : Skp.Interrupts.IRQ_Level_Type;
      Destination_ID : SK.Word64)
   with
      Global  => (In_Out => State),
      Depends => (State  =>+ (Destination_ID, RTE_Index, Trigger_Mode,
                              Trigger_Level, Vector));

   --  Mask IRQ with given redirection table index.
   procedure Mask_IRQ (RTE_Index : Skp.Interrupts.RTE_Index_Type)
   with
      Global  => (In_Out => State),
      Depends => (State =>+ RTE_Index);

   --  Unmask IRQ with given redirection table entry.
   procedure Unmask_IRQ (RTE_Index : Skp.Interrupts.RTE_Index_Type)
   with
      Global  => (In_Out => State),
      Depends => (State =>+ RTE_Index);

end SK.IO_Apic;
