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
with SK.Dump;

pragma $Release_Warnings
  (Off, "unit * is not referenced", Reason => "Only used for debug output");
with SK.Strings;
pragma $Release_Warnings (On, "unit * is not referenced");

package body SK.MCE
with
   Refined_State => (State => Bank_Count)
is

   -------------------------------------------------------------------------

   procedure Check_State
     (Is_Valid : out Boolean;
      Ctx      : out Crash_Audit_Types.MCE_Init_Context_Type)
   is
      EDX : Word32;
   begin
      Ctx := Crash_Audit_Types.Null_MCE_Init_Context;

      declare
         Unused_EAX, Unused_EBX, Unused_ECX : Word32;
      begin
         Unused_EAX := 1;
         Unused_ECX := 0;

         CPU.CPUID
           (EAX => Unused_EAX,
            EBX => Unused_EBX,
            ECX => Unused_ECX,
            EDX => EDX);
      end;

      Ctx.MCE_Support := Bitops.Bit_Test
        (Value => Word64 (EDX),
         Pos   => Constants.CPUID_FEATURE_MCE);
      Ctx.MCA_Support := Bitops.Bit_Test
        (Value => Word64 (EDX),
         Pos   => Constants.CPUID_FEATURE_MCA);
      pragma Debug (not Ctx.MCE_Support,
                    Dump.Print_Message (Msg => "Init: No MCE support"));
      pragma Debug (not Ctx.MCA_Support,
                    Dump.Print_Message (Msg => "Init: No MCA support"));
      pragma Debug
        (Dump.Print_Message
           (Msg => "MCE: IA32_MCG_CAP "
            & Strings.Img
              (Word32'Mod
                   (CPU.Get_MSR64 (Register => Constants.IA32_MCG_CAP)))));

      Ctx.Bank_Count_OK := Bank_Count <= Crash_Audit_Types.MCE_Max_HW_Banks;
      pragma Debug
        (not Ctx.Bank_Count_OK,
         Dump.Print_Message (Msg => "Init: Unsupported number of MCE banks "
                             & Strings.Img (Bank_Count)));

      Is_Valid := Ctx.Bank_Count_OK and Ctx.MCE_Support and Ctx.MCA_Support;
   end Check_State;

   -------------------------------------------------------------------------

   procedure Create_Context (Ctx : out Crash_Audit_Types.MCE_Context_Type)
   is
      use Crash_Audit_Types;

      Value      : Word64;
      Reg_Offset : Word32;
      Count      : MCE_Record_Count_Type := 0;
   begin
      Ctx := Null_MCE_Context;
      Ctx.Bank_Count := MCE_Bank_Count_Type (Bank_Count);

      Ctx.MCG_Status := CPU.Get_MSR64 (Register => Constants.IA32_MCG_STATUS);

      --  Uncorrected errors first, then the remaining valid banks.

      for Uncorrected in reverse Boolean loop
         for I in MCE_Bank_Index_Type loop
            exit when I >= MCE_Bank_Count_Type (Bank_Count);
            Reg_Offset := Word32 (I) * 4;
            Value := CPU.Get_MSR64
              (Register => Constants.IA32_MC0_STATUS + Reg_Offset);
            if Bitops.Bit_Test
              (Value => Value,
               Pos   => Constants.MCi_STATUS_Bit_Valid)
              and then
                (if Uncorrected
                 then Bitops.Bit_Test
                   (Value => Value,
                    Pos   => Constants.MCi_STATUS_Bit_UC)
                 else not Ctx.Valid_Banks (I))
            then
               Ctx.Valid_Banks (I) := True;
               if Count < MCE_Max_Bank_Records then
                  Count := Count + 1;
                  Ctx.Banks (Count).Bank       := I;
                  Ctx.Banks (Count).MCi_Status := Value;
                  if Bitops.Bit_Test
                    (Value => Value,
                     Pos   => Constants.MCi_STATUS_Bit_Addrv)
                  then
                     Ctx.Banks (Count).MCi_Addr := CPU.Get_MSR64
                       (Register => Constants.IA32_MC0_ADDR + Reg_Offset);
                  end if;
                  if Bitops.Bit_Test
                    (Value => Value,
                     Pos   => Constants.MCi_STATUS_Bit_Miscv)
                  then
                     Ctx.Banks (Count).MCi_Misc := CPU.Get_MSR64
                       (Register => Constants.IA32_MC0_MISC + Reg_Offset);
                  end if;
               end if;
            end if;
         end loop;
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
              (Msg => "MCE: IA32_MCG_CTL present, enabling all MCA features"));
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
      Mcg_Cap : constant Word64
        := CPU.Get_MSR64 (Register => Constants.IA32_MCG_CAP);
   begin
      Bank_Count := Byte (Mcg_Cap and 16#ff#);
   end;

end SK.MCE;
