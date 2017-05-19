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
with SK.KC;
with SK.MCE;
with SK.Version;
with SK.Scheduler;
with SK.System_State;
with SK.VTd.Interrupts;
with SK.Interrupts;

package body SK.Kernel
is

   -------------------------------------------------------------------------

   procedure Initialize (Subject_Registers : out SK.CPU_Registers_Type)
   is
      Success, Is_Bsp : Boolean;
   begin
      CPU_Global.Init;
      Is_Bsp := CPU_Global.Is_BSP;

      pragma Debug (Is_Bsp, KC.Init);
      pragma Debug (Is_Bsp, KC.Put_Line
                    (Item => "Booting Muen kernel "
                     & SK.Version.Version_String & " ("
                     & Standard'Compiler_Version & ")"));
      declare
         Valid_Sys_State : constant Boolean := System_State.Is_Valid;
         Valid_FPU_State : constant Boolean := FPU.Has_Valid_State;
         Valid_MCE_State : constant Boolean := MCE.Is_Valid;
      begin
         Success := Valid_Sys_State and Valid_FPU_State and Valid_MCE_State;

         if not Success then
            pragma Debug (KC.Put_Line (Item => "System initialisation error"));
            loop
               CPU.Cli;
               CPU.Hlt;
            end loop;
         end if;

         FPU.Enable;
         Apic.Enable;
         MCE.Enable;

         if Is_Bsp then
            MP.Initialize_All_Barrier;
            Apic.Start_AP_Processors;
         end if;

         MP.Wait_For_All;

         if Is_Bsp then
            Interrupts.Disable_Legacy_PIT;
            Interrupts.Disable_Legacy_PIC;
            VTd.Interrupts.Setup_IRQ_Routing;
            VTd.Initialize;
            Subjects_Events.Initialize;
         end if;

         System_State.Enable_VMX_Feature;

         VMX.Enter_Root_Mode;
         Scheduler.Init;

         --  Synchronize all logical CPUs.

         MP.Wait_For_All;

         Scheduler.Set_VMX_Exit_Timer;

         declare
            Current_Subject : constant Skp.Subject_Id_Type
              := CPU_Global.Get_Current_Subject_ID;
         begin
            Subjects.Filter_State (ID => Current_Subject);
            Subjects.Restore_State
              (ID   => Current_Subject,
               Regs => Subject_Registers);
         end;
      end;
   end Initialize;

end SK.Kernel;
