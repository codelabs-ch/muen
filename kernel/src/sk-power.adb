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
with SK.Bitops;
with SK.Constants;
with SK.Strings;
with SK.Dump;

pragma $Release_Warnings (Off, "unit * is not referenced");
with SK.CPU_Info;
pragma $Release_Warnings (On, "unit * is not referenced");

with Skp.Hardware;

package body SK.Power
is

   -------------------------------------------------------------------------

   procedure Enable_HWP
   is
      HWP_Available_Bit    : constant := 7;
      HWP_EPP_Ctl_Hint_Bit : constant := 10;

      --  Energy Performance Preference hint, 16#80# is the default. Use 16#ff#
      --  for energy efficiency preference, 0 for performance preference.
      HWP_EPP_Hint : constant := 16#80#;

      EAX, Unused_EBX, Unused_ECX, Unused_EDX : Word32;
   begin
      --  TODO: Should we do this on the package level only on BSP?
      --  TODO: Check HWP-availability bits in tau0, fail if not present.

      EAX := 6;
      Unused_ECX := 0;
      CPU.CPUID
        (EAX => EAX,
         EBX => Unused_EBX,
         ECX => Unused_ECX,
         EDX => Unused_EDX);

      if Bitops.Bit_Test
        (Value => Word64 (EAX),
         Pos   => HWP_Available_Bit)
        and then Bitops.Bit_Test
          (Value => Word64 (EAX),
           Pos   => HWP_EPP_Ctl_Hint_Bit)
      then
         pragma Debug
           (CPU_Info.Is_BSP,
            Dump.Print_Message (Msg => "Intel HWP available"));

         CPU.Write_MSR64 (Register => Constants.IA32_PM_ENABLE,
                          Value    => 1);

         declare
            HWP_Caps : constant Word64
              := CPU.Get_MSR64 (Register => Constants.IA32_HWP_CAPABILITIES);
            Highest_Performance, Lowest_Performance : Byte;
            Request : Word64;
         begin
            pragma Debug
              (CPU_Info.Is_BSP,
               Dump.Print_Message (Msg => "HW_Caps "
                                   & SK.Strings.Img (HWP_Caps)));

            Highest_Performance := Byte (HWP_Caps and 16#ff#);
            Lowest_Performance  := Byte ((HWP_Caps / 2 ** 24) and 16#ff#);

            pragma Debug
              (CPU_Info.Is_BSP,
               Dump.Print_Message
                 (Msg => "Highest performance "
                  & SK.Strings.Img (Word64 (Highest_Performance))));
            pragma Debug
              (CPU_Info.Is_BSP,
               Dump.Print_Message
                 (Msg => "Lowest performance "
                  & SK.Strings.Img (Word64 (Lowest_Performance))));
            pragma Debug
              (CPU_Info.Is_BSP,
               Dump.Print_Message
                 (Msg => "EPP hint "
                  & SK.Strings.Img (Word64 (HWP_EPP_Hint))));

            --  While HWP works by just enabling it, follow Intel SDM
            --  Vol. 3B, "14.4.10 Recommendations for OS use of HWP Controls".

            Request := Word64 (Lowest_Performance);
            Request := Request or Word64 (Highest_Performance) * 2 ** 8;
            Request := Request or HWP_EPP_Hint * 2 ** 24;

            pragma Debug
              (CPU_Info.Is_BSP,
               Dump.Print_Message (Msg => "Request->HWP "
                                   & SK.Strings.Img (Request)));

            CPU.Write_MSR64 (Register => Constants.IA32_HWP_REQUEST,
                             Value    => Request);
         end;
      end if;
   end Enable_HWP;

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
