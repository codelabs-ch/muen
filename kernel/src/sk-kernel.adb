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

with SK.KC;
with SK.Version;
with SK.Interrupts;
with SK.System_State;
with SK.VMX;
with SK.Scheduler;
with SK.Apic;
with SK.MP;
with SK.CPU_Global;
with SK.CPU;

package body SK.Kernel
is

   -------------------------------------------------------------------------

   procedure Initialize
   is
      Success, Is_Bsp : Boolean;
      Regs            : SK.CPU_Registers_Type;
   begin
      Is_Bsp := Apic.Is_BSP;

      if Is_Bsp then
         Interrupts.Init;
      end if;
      Interrupts.Load;

      pragma Debug (Is_Bsp, KC.Init);
      pragma Debug (Is_Bsp, KC.Put_Line
                    (Item => "Booting Muen kernel "
                     & SK.Version.Version_String & " ("
                     & Standard'Compiler_Version & ")"));
      Success := System_State.Is_Valid;
      if Success then

         Apic.Enable;
         CPU_Global.Init;

         if Is_Bsp then

            --  BSP

            Interrupts.Disable_Legacy_PIC;
            Interrupts.Setup_IRQ_Routing;
            Apic.Start_AP_Processors;
         end if;

         VMX.Enable;
         Scheduler.Init;

         --  Synchronize all logical CPUs.

         MP.Wait_For_All;
         VMX.Restore_Guest_Regs
           (Subject_Id => CPU_Global.Get_Current_Minor_Frame.Subject_Id,
            Regs       => Regs);

         --  TODO: Refactor

         CPU.Restore_Registers (Regs => Regs);
         CPU.VMLAUNCH;
         VMX.VMX_Error;
      else
         pragma Debug (KC.Put_Line (Item => "System initialisation error"));
         CPU.Stop;
      end if;
   end Initialize;

end SK.Kernel;
