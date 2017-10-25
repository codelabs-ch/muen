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
with
   Refined_State => (State => Bank_Count)
is

   -------------------------------------------------------------------------

   procedure Check_State
     (Is_Valid : out Boolean;
      Ctx      : out Crash_Audit_Types.MCE_Init_Context_Type)
   is
      EDX   : Word32;
   begin
      Ctx := Crash_Audit_Types.Null_MCE_Init_Context;

      declare
         Unused_EAX, Unused_EBX, Unused_ECX : Word32;
      begin
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
         pragma Warnings (GNATprove, On,
                          "unused assignment to ""Unused_E*X""");
      end;

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
      pragma Debug
        (Dump.Print_Message
           (Msg => "MCE: IA32_MCG_CAP "
            & Strings.Img
              (Word32'Mod
                   (CPU.Get_MSR64 (Register => Constants.IA32_MCG_CAP)))));

      Ctx.Bank_Count_OK := Bank_Count > 0;
      pragma Debug (not Ctx.Bank_Count_OK,
                    KC.Put_Line
                      (Item => "Init: Unsupported number of MCE banks"));

      Is_Valid := Ctx.Bank_Count_OK and Ctx.MCE_Support and Ctx.MCA_Support;
   end Check_State;

   -------------------------------------------------------------------------

   procedure Create_Context (Ctx : out Crash_Audit_Types.MCE_Context_Type)
   is
      Value : Word64;
   begin
      Ctx := Crash_Audit_Types.Null_MCE_Context;
      Ctx.Bank_Count := Bank_Count;

      Ctx.MCG_Status := CPU.Get_MSR64 (Register => Constants.IA32_MCG_STATUS);

      for I in 1 .. Bank_Count loop
         Value := CPU.Get_MSR64
           (Register =>
              Word32 (Constants.IA32_MC0_STATUS + (Integer (I) - 1) * 4));
         if Bitops.Bit_Test
           (Value => Value,
            Pos   => Constants.MCi_STATUS_Bit_Valid)
         then
            Ctx.MCi_Status (I) := Value;
            if Bitops.Bit_Test
              (Value => Value,
               Pos   => Constants.MCi_STATUS_Bit_Addrv)
            then
               Ctx.MCi_Addr (I) := CPU.Get_MSR64
                 (Register =>
                    Word32 (Constants.IA32_MC0_ADDR + (Integer (I) - 1) * 4));
            end if;
            if Bitops.Bit_Test
              (Value => Value,
               Pos   => Constants.MCi_STATUS_Bit_Miscv)
            then
               Ctx.MCi_Misc (I) := CPU.Get_MSR64
                 (Register =>
                    Word32 (Constants.IA32_MC0_MISC + (Integer (I) - 1) * 4));
            end if;
         end if;
      end loop;
   end Create_Context;

   -------------------------------------------------------------------------

   procedure Enable
   is
      CR4, Value : Word64;
   begin
      Value := CPU.Get_MSR64 (Register => Constants.IA32_MCG_CAP);

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

begin
   declare
      Mcg_Cap       : constant Word64 :=
        CPU.Get_MSR64 (Register => Constants.IA32_MCG_CAP);
      Mcg_Cap_Count : constant Word64 := (Mcg_Cap and 16#ff#);
   begin
      if Mcg_Cap_Count < Crash_Audit_Types.MCE_Max_Banks then
         Bank_Count :=
           Crash_Audit_Types.Bank_Index_Ext_Range (Mcg_Cap_Count);
      else
         Bank_Count := 0;
      end if;
   end;

end SK.MCE;
