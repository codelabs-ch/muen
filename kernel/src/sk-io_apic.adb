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

with System;

with Skp.Kernel;

with SK.Bitops;

package body SK.IO_Apic
with
   Refined_State => (State => (Window, Register_Select))
is

   --  I/O APIC register offsets relative to I/O Apic address, see 82093AA I/O
   --  Advanced Programmable Interrupt Controller (IOAPIC) specification,
   --  section 3.2.4.

   IO_APIC_IND  : constant := 16#00#;
   IO_APIC_DAT  : constant := 16#10#;

   IO_APIC_REDTBL : constant := 16#10#;

   RED_INTPOL       : constant := 13;
   RED_TRIGGER_MODE : constant := 15;
   RED_INTFORMAT    : constant := 48;

   Register_Select : SK.Word32
   with
      Volatile,
      Async_Writers,  --  XXX Can the chosen register change behind out back?
      Async_Readers,
      Effective_Writes,
      Address => System'To_Address (Skp.Kernel.IO_Apic_Address + IO_APIC_IND);

   Window : SK.Word32
   with
      Volatile,
      Async_Writers,  --  XXX Can the chosen register change behind out back?
      Async_Readers,
      Effective_Writes,
      Address => System'To_Address (Skp.Kernel.IO_Apic_Address + IO_APIC_DAT);

   -------------------------------------------------------------------------

   --  Create redirection entry from specified parameters, see 82093AA I/O
   --  Advanced Programmable Interrupt Controller (IOAPIC) specification,
   --  section 3.2.4.
   procedure Create_Redirection_Entry
     (Redir_Entry    : out SK.Word64;
      Vector         :     SK.Byte;
      Trigger_Mode   :     Skp.Interrupts.IRQ_Mode_Type;
      Trigger_Level  :     Skp.Interrupts.IRQ_Level_Type;
      Destination_ID :     SK.Word64)
   with
      Global  => null,
      Depends => (Redir_Entry => (Destination_ID, Trigger_Mode, Trigger_Level,
                                  Vector))
   is
      use type Skp.Interrupts.IRQ_Mode_Type;
      use type Skp.Interrupts.IRQ_Level_Type;
   begin
      Redir_Entry := Word64 (Vector);

      Redir_Entry := Bitops.Bit_Set (Value => Redir_Entry,
                                     Pos   => RED_INTFORMAT);

      if Trigger_Mode = Skp.Interrupts.Level then
         Redir_Entry := Bitops.Bit_Set (Value => Redir_Entry,
                                        Pos   => RED_TRIGGER_MODE);
      end if;

      if Trigger_Level = Skp.Interrupts.Low then
         Redir_Entry := Bitops.Bit_Set (Value => Redir_Entry,
                                        Pos   => RED_INTPOL);
      end if;

      Redir_Entry := Redir_Entry + Destination_ID;
   end Create_Redirection_Entry;

   -------------------------------------------------------------------------

   procedure Route_IRQ
     (RTE_Index      : Skp.Interrupts.RTE_Index_Type;
      Vector         : SK.Byte;
      Trigger_Mode   : Skp.Interrupts.IRQ_Mode_Type;
      Trigger_Level  : Skp.Interrupts.IRQ_Level_Type;
      Destination_ID : SK.Word64)
   with
      --  XXX Data flow does not represent properties of registers
      Refined_Global  => (Output => (Window, Register_Select)),
      Refined_Depends =>
        (Window          => (Destination_ID, Trigger_Mode, Trigger_Level,
                             Vector),
         Register_Select => RTE_Index)
   is
      Redir_Entry : SK.Word64;
   begin
      Create_Redirection_Entry (Redir_Entry    => Redir_Entry,
                                Vector         => Vector,
                                Trigger_Mode   => Trigger_Mode,
                                Trigger_Level  => Trigger_Level,
                                Destination_ID => Destination_ID);

      Register_Select := IO_APIC_REDTBL + SK.Word32 (RTE_Index) * 2;
      Window          := SK.Word32'Mod (Redir_Entry);

      Register_Select := IO_APIC_REDTBL + SK.Word32 (RTE_Index) * 2 + 1;
      Window          := SK.Word32 (Redir_Entry / 2 ** 32);
   end Route_IRQ;

end SK.IO_Apic;
