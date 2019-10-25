--
--  Copyright (C) 2013-2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.Dump;
with SK.IO;
with SK.CPU;

package body SK.Interrupts
is

   Pic_Cmd_Master  : constant := 16#20#;
   Pic_Cmd_Slave   : constant := 16#a0#;
   Pic_Data_Master : constant := 16#21#;
   Pic_Data_Slave  : constant := 16#a1#;

   Pit_Ch0_Data : constant := 16#40#;
   Pit_Mode     : constant := 16#43#;

   MCE_Vector : constant := 18;

   -------------------------------------------------------------------------

   procedure Disable_Legacy_PIC
   is
   begin

      --  Start initialization sequence in cascade mode

      IO.Outb (Port  => Pic_Cmd_Master,
               Value => 16#11#);
      IO.Outb (Port  => Pic_Cmd_Slave,
               Value => 16#11#);

      --  ICW2: Master PIC vector offset (32)

      IO.Outb (Port  => Pic_Data_Master,
               Value => 16#20#);

      --  ICW2: Slave PIC vector offset (40)

      IO.Outb (Port  => Pic_Data_Slave,
               Value => 16#28#);

      --  ICW3: Tell Master PIC that there is a slave PIC at IRQ2

      IO.Outb (Port  => Pic_Data_Master,
               Value => 16#04#);

      --  ICW3: Tell Slave PIC its cascade identity

      IO.Outb (Port  => Pic_Data_Slave,
               Value => 16#02#);

      --  ICW4: Enable 8086 mode

      IO.Outb (Port  => Pic_Data_Master,
               Value => 16#01#);
      IO.Outb (Port  => Pic_Data_Slave,
               Value => 16#01#);

      --  Disable slave.

      IO.Outb (Port  => Pic_Data_Slave,
               Value => 16#ff#);

      --  Disable master.

      IO.Outb (Port  => Pic_Data_Master,
               Value => 16#ff#);
   end Disable_Legacy_PIC;

   -------------------------------------------------------------------------

   procedure Disable_Legacy_PIT
   is
   begin
      IO.Outb (Port  => Pit_Mode,
               Value => 16#30#);
      IO.Outb (Port  => Pit_Ch0_Data,
               Value => 0);
      IO.Outb (Port  => Pit_Ch0_Data,
               Value => 0);
   end Disable_Legacy_PIT;

   -------------------------------------------------------------------------

   procedure Dispatch_Exception (Context : Crash_Audit_Types.Isr_Context_Type)
   is
      A : Crash_Audit.Entry_Type;
      E : Crash_Audit_Types.Exception_Context_Type;
      M : Crash_Audit_Types.MCE_Context_Type;
   begin
      E.ISR_Ctx := Context;
      E.CR0     := CPU.Get_CR0;
      E.CR3     := CPU.Get_CR3;
      E.CR4     := CPU.Get_CR4;

      pragma Debug (Dump.Print_ISR_State (Context => E));

      Crash_Audit.Allocate (Audit => A);
      Crash_Audit.Set_Reason
        (Audit  => A,
         Reason => Crash_Audit_Types.Hardware_Exception);
      Crash_Audit.Set_Exception_Context
        (Audit   => A,
         Context => E);

      if Context.Vector = MCE_Vector then
         MCE.Create_Context (Ctx => M);
         Crash_Audit.Set_MCE_Context
           (Audit   => A,
            Context => M);
         pragma Debug (Dump.Print_MCE_State (Context => M));
      end if;

      Crash_Audit.Finalize (Audit => A);
   end Dispatch_Exception;

end SK.Interrupts;
