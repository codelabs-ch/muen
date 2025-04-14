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

with SK.Bitops;
with SK.CPU;
with SK.Constants;
with SK.Dump;
with SK.IO;
with SK.Strings;

with Skp.Hardware;

package body SK.Power
is

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

   -------------------------------------------------------------------------

   --  Returns true if Enhanced Intel SpeedStep is supported by the CPU.
   function Has_Intel_SpeedStep_Support return Boolean
   with
      Volatile_Function,
      Global => (Input => X86_64.State)
   is
      Unused_EAX, Unused_EBX, ECX, Unused_EDX : Word32;
   begin
      Unused_EAX := 1;
      ECX := 0;

      CPU.CPUID
        (EAX => Unused_EAX,
         EBX => Unused_EBX,
         ECX => ECX,
         EDX => Unused_EDX);

      --  Intel SDM Vol. 3B, "14.1 Enhanced Intel Speedstep Technology".

      return Bitops.Bit_Test
        (Value => Word64 (ECX),
         Pos   => Constants.CPUID_FEATURE_SPEEDSTEP);
   end Has_Intel_SpeedStep_Support;

   -------------------------------------------------------------------------

   --  Returns true if the Opportunistic CPU operation is supported by the CPU.
   function Has_Opportunistic_CPU_Operation_Support return Boolean
   with
      Volatile_Function,
      Global => (Input => X86_64.State)
   is
      EAX, Unused_EBX, Unused_ECX, Unused_EDX : Word32;
   begin
      EAX := 6;
      Unused_ECX := 0;

      CPU.CPUID
        (EAX => EAX,
         EBX => Unused_EBX,
         ECX => Unused_ECX,
         EDX => Unused_EDX);

      --  Intel SDM Vol. 3B, "14.3.2.1 Discover Hardware Support and Enabling of
      --  Opportunistic Processor Performance Operation".

      return Bitops.Bit_Test
        (Value => Word64 (EAX),
         Pos   => Constants.CPUID_FEATURE_TURBO_FLAG);
   end Has_Opportunistic_CPU_Operation_Support;

   -------------------------------------------------------------------------

   procedure Set_CPU_Base_Frequency
   is
      --  Intel SDM Vol. 3B, "14.3.2.2 OS Control of Opportunistic Processor
      --  Performance Operation".
      TURBO_DISENGAGE_BIT : constant := 32;
      TARGET_MASK         : constant Word64 := 16#ffff_ffff#;
      MSR_Platform_Info   : constant Word64 := SK.CPU.Get_MSR64
        (Register => Constants.MSR_PLATFORM_INFO);
      --  Target performance state value, MSR_PLATFORM_INFO[15:0], see Intel SDM
      --  Vol. 4, "2.1 Architectural MSRs", Table 2-2.
      Tgt_Perf_State_Value  : constant Word64 := MSR_Platform_Info and 16#ffff#;
      Has_SpeedStep_Support : constant Boolean
        := Has_Intel_SpeedStep_Support;
      Has_Turbo_Support     : constant Boolean
        := Has_Opportunistic_CPU_Operation_Support;

      Value : Word64;
   begin
      pragma Debug (Dump.Print_Message
                    (Msg => "PWR: IA32_MISC_ENABLE        "
                     & Strings.Img (CPU.Get_MSR64
                       (Register => Constants.IA32_MISC_ENABLE))));
      pragma Debug (Dump.Print_Message
                    (Msg => "PWR: MSR_PLATFORM_INFO       "
                     & Strings.Img (MSR_Platform_Info)));
      pragma Debug (Dump.Print_Message
                    (Msg => "PWR: IA32_PERF_STATUS (INIT) "
                     & Strings.Img (CPU.Get_MSR64
                       (Register => Constants.IA32_PERF_STATUS))));
      pragma Debug (not Has_SpeedStep_Support, Dump.Print_Message
                    (Msg => "PWR: No support for Enhanced Intel SpeedStep "));
      pragma Debug (not Has_Turbo_Support, Dump.Print_Message
                    (Msg => "PWR: No support for Opportunistic CPU Operation"));

      if Has_SpeedStep_Support then
         Value := CPU.Get_MSR64 (Register => Constants.IA32_PERF_CTL);

         --  Mask current Enhanced Intel Speedstep Transition Target.

         Value := (Value and not TARGET_MASK) or Tgt_Perf_State_Value;

         if Has_Turbo_Support then
            Value := Bitops.Bit_Set (Value => Value,
                                     Pos   => TURBO_DISENGAGE_BIT);
         end if;

         CPU.Write_MSR64 (Register => Constants.IA32_PERF_CTL,
                          Value    => Value);
         pragma Debug (Dump.Print_Message
                       (Msg => "PWR: IA32_PERF_STATUS (POST) "
                        & Strings.Img (CPU.Get_MSR64
                          (Register => Constants.IA32_PERF_STATUS))));
         pragma Debug (Dump.Print_Message
                       (Msg => "PWR: IA32_PERF_CTL           "
                        & Strings.Img (CPU.Get_MSR64
                          (Register => Constants.IA32_PERF_CTL))));
      end if;
   end Set_CPU_Base_Frequency;

end SK.Power;
