--
--  Copyright (C) 2013-2022  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2022  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.CPU;
with SK.Strings;

with Exceptions;

with Log;

package body Interrupt_Handler
is

   -------------------------------------------------------------------------

   procedure Dispatch_Exception (Context : SK.Exceptions.Isr_Context_Type)
   is
      use type SK.Byte;

      Vector : constant SK.Byte := SK.Byte'Mod (Context.Vector);
   begin
      if Vector > 31 then
         Handle_Interrupt (Vector => Vector);
      elsif Vector = 3 then
         Log.Put_Line (Item => "#BP exception @ RIP "
                       & SK.Strings.Img (Item => Context.RIP));
         Log.Put_Line (Item => "                RSP "
                       & SK.Strings.Img (Item => Context.RSP));
         Log.Put_Line (Item => "                RBP "
                       & SK.Strings.Img (Item => Context.Regs.RBP));
         Log.Put_Line (Item => "         Error Code "
                       & SK.Strings.Img (Item => Context.Error_Code));
         Exceptions.Print_Backtrace (RIP => Context.RIP,
                                     RBP => Context.Regs.RBP);
         Exceptions.BP_Triggered := True;

         --  Hardware stores the RIP pointing to the instruction after int3 on
         --  the interrupt stack so we can simply return to resume execution.

      else
         Log.Put_Line (Item => "Halting due to unexpected exception "
                       & SK.Strings.Img (Item => Vector));
         Log.Put_Line (Item => " Error Code: "
                       & SK.Strings.Img (Item => Context.Error_Code));
         Log.Put_Line (Item => " RIP       : "
                       & SK.Strings.Img (Item => Context.RIP));
         Log.Put_Line (Item => " RSP       :"
                       & SK.Strings.Img (Item => Context.RSP));
         Log.Put_Line (Item => " CS        : "
                       & SK.Strings.Img (Item => SK.Byte'Mod (Context.CS)));
         Log.Put_Line (Item => " SS        : "
                       & SK.Strings.Img (Item => SK.Byte'Mod (Context.SS)));
         Log.Put_Line (Item => " RFLAGS    : "
                       & SK.Strings.Img (Item => Context.RFLAGS));

         SK.CPU.Stop;
      end if;
   end Dispatch_Exception;

   -------------------------------------------------------------------------

   procedure Handle_Interrupt (Vector : SK.Byte)
   is
   begin
      Log.Put_Line (Item => "Received vector " & SK.Strings.Img
                    (Item => Vector));
   end Handle_Interrupt;

end Interrupt_Handler;
