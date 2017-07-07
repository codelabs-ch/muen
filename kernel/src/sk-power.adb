--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.IO;
with SK.CPU;
with SK.Dump;
with SK.Strings;
with SK.Constants;
pragma $Release_Warnings (Off, "unit * is not referenced");
with SK.CPU_Info;
pragma $Release_Warnings (On, "unit * is not referenced");

with Skp.Hardware;

package body SK.Power
is

   procedure Dump_State
   is
   begin
      pragma Debug (Dump.Print_Message
                    (Msg => "APIC ID " & Strings.Img (Byte (CPU_Info.APIC_ID))
                     & " PWR IA32_PERF_STATUS "
                     & Strings.Img (CPU.Get_MSR64
                       (Register => Constants.IA32_PERF_STATUS))));
      pragma Debug (Dump.Print_Message
                    (Msg => "APIC ID " & Strings.Img (Byte (CPU_Info.APIC_ID))
                     & " PWR IA32_PERF_CTL "
                     & Strings.Img (CPU.Get_MSR64
                       (Register => Constants.IA32_PERF_CTL))));
   end Dump_State;

   -------------------------------------------------------------------------

   procedure Turbo
   is
      Val : Word64;
   begin

      --  See Intel SDM Vol. 3B, 14.3.2.2.
      --  Assumption: Intel IDA is on, otherwise the system will restart.
      --              Use CPUID.06H:EAX[1] to check.

      pragma Debug
        (Dump.Print_Message
           (Msg => "PWR MSR_TURBO_RATIO_LIMIT "
            & Strings.Img (CPU.Get_MSR64
              (Register => Constants.MSR_TURBO_RATIO_LIMIT))));
      pragma Debug
        (Dump.Print_Message
           (Msg => "PWR IA32_MISC_ENABLE "
            & Strings.Img (CPU.Get_MSR64
              (Register => Constants.IA32_MISC_ENABLE))));
      pragma Debug
        (Dump.Print_Message
           (Msg => "PWR MSR_PLATFORM_INFO "
            & Strings.Img (CPU.Get_MSR64
              (Register => Constants.MSR_PLATFORM_INFO))));
      pragma Debug
        (Dump.Print_Message
           (Msg => "PWR IA32_PERF_STATUS(INIT) "
            & Strings.Img (CPU.Get_MSR64
              (Register => Constants.IA32_PERF_STATUS))));

      --  Enable turbo by read-modify-write of largest possible value, hardware
      --  will pick max. transition target possible.

      Val := CPU.Get_MSR64 (Register => Constants.IA32_PERF_CTL);
      Val := Val or (16#ff# * 2 ** 8);
      CPU.Write_MSR64 (Register => Constants.IA32_PERF_CTL,
                       Value    => Val);
   end Turbo;

   -------------------------------------------------------------------------

   procedure Shutdown
   is
      --  Enable S5 soft-off (1 << 13).
      ACPI_PM1_CNT_SLP_EN : constant := 16#2000#;
   begin
      IO.Outw (Port  => Skp.Hardware.System_Board_Poweroff_Port,
               Value => Skp.Hardware.System_Board_Pm1a_Cnt_Slp_Typ
               or ACPI_PM1_CNT_SLP_EN);

      --  Somehow we survived, stop the CPU.

      CPU.Stop;
   end Shutdown;

   -------------------------------------------------------------------------

   procedure Reboot (Power_Cycle : Boolean)
   is
      FULL_RST : constant := 2#1000#; --  Power cycle.
      RST_CPU  : constant := 2#0100#; --  Do the actual reset.
      SYS_RST  : constant := 2#0010#; --  CPU soft (0) or hard (1) reset.

      Code : Byte := RST_CPU or SYS_RST;
   begin
      if Power_Cycle then
         Code := Code or FULL_RST;
      end if;

      IO.Outb (Port  => Skp.Hardware.System_Board_Reset_Port,
               Value => Code);

      --  Somehow we survived, stop the CPU.

      CPU.Stop;
   end Reboot;

end SK.Power;
