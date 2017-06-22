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

with Skp.Kernel;

with SK.Apic;
with SK.CPU;
with SK.KC;
with SK.MCE;
with SK.Version;
with SK.System_State;
with SK.VTd.Interrupts;
with SK.Interrupts;
with SK.Crash_Audit_Types;

package body SK.Kernel
is

   -------------------------------------------------------------------------

   procedure Initialize (Subject_Registers : out SK.CPU_Registers_Type)
   is
      Success : Boolean;
   begin
      Interrupt_Tables.Initialize
        (Stack_Addr => Skp.Kernel.Intr_Stack_Address);

      pragma Debug (CPU_Info.Is_BSP, KC.Init);
      pragma Debug (CPU_Info.Is_BSP, KC.Put_Line
                    (Item => "Booting Muen kernel "
                     & SK.Version.Version_String & " ("
                     & Standard'Compiler_Version & ")"));
      declare
         Init_Ctx : Crash_Audit_Types.Init_Context_Type;

         Valid_Sys_State, Valid_FPU_State, Valid_MCE_State : Boolean;
      begin
         pragma Warnings (GNATprove, Off, "unused assignment");
         System_State.Check_State
           (Is_Valid => Valid_Sys_State,
            Ctx      => Init_Ctx.Sys_Ctx);
         FPU.Check_State
           (Is_Valid => Valid_FPU_State,
            Ctx      => Init_Ctx.FPU_Ctx);
         MCE.Check_State
           (Is_Valid => Valid_MCE_State,
            Ctx      => Init_Ctx.MCE_Ctx);
         pragma Warnings (GNATprove, On, "unused assignment");

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

         if CPU_Info.Is_BSP then
            Crash_Audit.Init;
            MP.Initialize_All_Barrier;
            Apic.Start_AP_Processors;
            Interrupts.Disable_Legacy_PIT;
            Interrupts.Disable_Legacy_PIC;
            VTd.Initialize;
            VTd.Interrupts.Setup_IRQ_Routing;
            Subjects_Events.Initialize;
         end if;

         MP.Wait_For_All;

         System_State.Enable_VMX_Feature;
         VMX.Enter_Root_Mode;
         Scheduler.Init;

         --  Synchronize all logical CPUs.

         MP.Wait_For_All;

         Scheduler.Set_VMX_Exit_Timer;

         declare
            Current_Subject : constant Skp.Subject_Id_Type
              := Scheduler.Get_Current_Subject_ID;
         begin
            Subjects.Filter_State (ID => Current_Subject);
            Subjects.Restore_State
              (ID   => Current_Subject,
               Regs => Subject_Registers);
         end;
      end;
   end Initialize;

end SK.Kernel;
