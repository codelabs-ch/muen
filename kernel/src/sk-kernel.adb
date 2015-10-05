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

with SK.Apic;
with SK.CPU;
with SK.FPU;
with SK.KC;
with SK.Version;
with SK.Scheduler;
with SK.System_State;
with SK.VMX;
with SK.VTd.Interrupts;

package body SK.Kernel
is

   -------------------------------------------------------------------------

   procedure Initialize (Subject_Registers : out SK.CPU_Registers_Type)
   is
      Success, Is_Bsp : Boolean;
   begin
      Is_Bsp := CPU_Global.Is_BSP;

      if Is_Bsp then
         Interrupts.Init;
      end if;
      Interrupts.Load;

      pragma Debug (Is_Bsp, KC.Init);
      pragma Debug (Is_Bsp, KC.Put_Line
                    (Item => "Booting Muen kernel "
                     & SK.Version.Version_String & " ("
                     & Standard'Compiler_Version & ")"));
      Success := System_State.Is_Valid and FPU.Has_Valid_State;

      if not Success then
         pragma Debug (KC.Put_Line (Item => "System initialisation error"));
         loop
            CPU.Cli;
            CPU.Hlt;
         end loop;
      end if;

      FPU.Enable;
      Apic.Enable;
      CPU_Global.Init;

      --  Register CPU ID -> local APIC ID mapping and make sure all CPUs
      --  are registered before programming the IRQ routing.

      CPU_Registry.Register (CPU_ID  => CPU_Global.CPU_ID,
                             APIC_ID => Apic.Get_ID);

      if Is_Bsp then
         Apic.Start_AP_Processors;
      end if;

      MP.Wait_For_All;

      if Is_Bsp then
         Interrupts.Disable_Legacy_PIT;
         Interrupts.Disable_Legacy_PIC;
         VTd.Interrupts.Setup_IRQ_Routing;
         VTd.Initialize;
      end if;

      System_State.Enable_VMX_Feature;

      VMX.Enter_Root_Mode;
      Scheduler.Init;

      --  Synchronize all logical CPUs.

      MP.Wait_For_All;

      Scheduler.Set_VMX_Exit_Timer;
      Subjects.Restore_State
        (Id   => CPU_Global.Get_Current_Subject_ID,
         Regs => Subject_Registers);

   end Initialize;

end SK.Kernel;
