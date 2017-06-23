--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with SK.Bitops;
with SK.Constants;
with SK.KC;
with SK.Dump;
with SK.Strings;

package body SK.MCE
is

   -------------------------------------------------------------------------

   procedure Check_State
     (Is_Valid : out Boolean;
      Ctx      : out Crash_Audit_Types.MCE_Init_Context_Type)
   is
      Unused_EAX, Unused_EBX, Unused_ECX, EDX : Word32;
   begin
      Ctx        := Crash_Audit_Types.Null_MCE_Init_Context;
      Unused_EAX := 1;
      Unused_ECX := 0;

      pragma Warnings
        (GNATprove, Off, "unused assignment to ""Unused_E*X""",
         Reason => "Only parts of the CPUID result is needed");
      CPU.CPUID
        (EAX => Unused_EAX,
         EBX => Unused_EBX,
         ECX => Unused_ECX,
         EDX => EDX);
      pragma Warnings (GNATprove, On, "unused assignment to ""Unused_E*X""");

      Ctx.MCE_Support := Bitops.Bit_Test
        (Value => Word64 (EDX),
         Pos   => Constants.CPUID_FEATURE_MCE);
      Ctx.MCA_Support := Bitops.Bit_Test
        (Value => Word64 (EDX),
         Pos   => Constants.CPUID_FEATURE_MCA);
      pragma Debug (not Ctx.MCE_Support,
                    KC.Put_Line (Item => "Init: No MCE support"));
      pragma Debug (not Ctx.MCA_Support,
                    KC.Put_Line (Item => "Init: No MCA support"));

      Is_Valid := Ctx.MCE_Support and Ctx.MCA_Support;
   end Check_State;

   -------------------------------------------------------------------------

   procedure Enable
   is
      Bank_Count : Byte;
      CR4, Value : Word64;
   begin
      Value := CPU.Get_MSR64 (Register => Constants.IA32_MCG_CAP);
      pragma Debug (Dump.Print_Message
                    (Msg => "MCE: IA32_MCG_CAP "
                     & Strings.Img (Word32'Mod (Value))));
      Bank_Count := Byte (Value and 16#ff#);

      if Bitops.Bit_Test
        (Value => Value,
         Pos   => Constants.MCG_CTL_P_FLAG)
      then
         pragma Debug
           (Dump.Print_Message
              (Msg => "MCE: IA32_MCG_CTL present, "
               & "enabling all MCA features"));
         CPU.Write_MSR64
           (Register => Constants.IA32_MCG_CTL,
            Value    => Word64'Last);
      end if;

      for I in Integer range 0 .. Integer (Bank_Count) - 1 loop
         CPU.Write_MSR64
           (Register => Word32 (Constants.IA32_MC0_CTL + I * 4),
            Value    => Word64'Last);
      end loop;

      for I in Integer range 0 .. Integer (Bank_Count) - 1 loop
         CPU.Write_MSR64
           (Register => Word32 (Constants.IA32_MC0_STATUS + I * 4),
            Value    => 0);
      end loop;

      CR4 := CPU.Get_CR4;
      CR4 := Bitops.Bit_Set (Value => CR4,
                             Pos   => Constants.CR4_MCE_FLAG);
      CPU.Set_CR4 (Value => CR4);
   end Enable;

end SK.MCE;
