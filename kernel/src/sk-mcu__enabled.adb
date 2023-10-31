--
--  Copyright (C) 2023  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2023  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with System;

with SK.Strings;
with SK.CPU;
with SK.Dump;

pragma $Release_Warnings
  (Off, "unit * is not referenced", Reason => "Only used for debug output");
with SK.CPU_Info;
pragma $Release_Warnings (On, "unit * is not referenced");

with Skp.MCU;

package body SK.MCU
with
   Refined_State => (State => UCH)
is

   IA32_BIOS_UPDT_TRIG : constant := 16#79#;
   IA32_BIOS_SIGN_ID   : constant := 16#8b#;

   Header_Size : constant := 48;

   type Padding_Type is array (1 .. 3) of Word32;

   --  Microcode update header.
   --  See Intel SDM Vol. 3A, "9.11.1 Microcode Update"
   type Header_Type is record
      Header_Version      : Word32;
      Update_Revision     : Word32;
      Date                : Word32;
      Processor_Signature : Word32;
      Checksum            : Word32;
      Loader_Revision     : Word32;
      Processor_Flags     : Word32;
      Data_Size           : Word32;
      Total_Size          : Word32;
      Reserved            : Padding_Type;
   end record
   with
      Pack,
      Size        => Header_Size * 8,
      Object_Size => Header_Size * 8;

   pragma Warnings
     (GNATprove, Off,
      "indirect writes to * through a potential alias are ignored",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   UCH : constant Header_Type
   with
      Import,
      Address => System'To_Address (Skp.MCU.Ucode_Address);
   pragma Warnings
     (GNATprove, On,
      "indirect writes to * through a potential alias are ignored");

   --  Return update ID, see Intel SDM Vol. 3A, "9.11.7 Update Signature and
   --  Verification".
   procedure Get_Update_ID (Value : out Word32);

   --  Perform actual MCU.
   procedure Perform_Update;

   -------------------------------------------------------------------------

   procedure Get_Update_ID (Value : out Word32)
   is
      Unused_EAX, Unused_EBX, Unused_ECX, Unused_EDX : Word32;
   begin
      CPU.Write_MSR64
        (Register => IA32_BIOS_SIGN_ID,
         Value    => 0);
      Unused_EAX := 1;
      Unused_ECX := 0;
      pragma Warnings
        (GNATProve, Off, "statement has no effect",
         Reason => "The effect is to trigger the sign ID update");
      CPU.CPUID
        (EAX => Unused_EAX,
         EBX => Unused_EBX,
         ECX => Unused_ECX,
         EDX => Unused_EDX);
      pragma Warnings
        (GNATProve, On, "statement has no effect");
      CPU.Get_MSR
        (Register => IA32_BIOS_SIGN_ID,
         Low      => Unused_EAX,
         High     => Value);
   end Get_Update_ID;

   -------------------------------------------------------------------------

   procedure Perform_Update
   is
      Ucode_Addr : constant Word64 := Skp.MCU.Ucode_Address + Header_Size;
   begin
      CPU.Write_MSR
        (Register => IA32_BIOS_UPDT_TRIG,
         Low      => Word32'Mod (Ucode_Addr),
         High     => Word32'Mod (Ucode_Addr / 2 ** 32));
   end Perform_Update;

   -------------------------------------------------------------------------

   --  See Intel SDM Vol. 3A, "9.11.6 Microcode Update Loader" and Intel SDM
   --  Vol. 3A, "9.11.7.2 Authenticating the Update".
   procedure Process
   is
      My_Sig : Word32 := 1;
      Unused_Rev, Unused_EBX, Unused_ECX, Unused_EDX : Word32;
   begin
      Unused_ECX := 0;
      CPU.CPUID
        (EAX => My_Sig,
         EBX => Unused_EBX,
         ECX => Unused_ECX,
         EDX => Unused_EDX);
      Get_Update_ID (Value => Unused_Rev);

      --  Common info is only logged on BSP.

      pragma Debug
        (CPU_Info.Is_BSP,
         Dump.Print_Message
           (Msg    => "MCU: Updating CPU with signature "
            & Strings.Img (My_Sig) & " and revision "
            & Strings.Img (Unused_Rev)));

      if UCH.Header_Version /= 1 then
         pragma Debug
           (CPU_Info.Is_BSP,
            Dump.Print_Message
              (Msg    =>
                 "MCU: ERROR - Unknown header version " &
                 Strings.Img (UCH.Header_Version)));
         return;
      end if;

      pragma Debug
        (CPU_Info.Is_BSP,
         Dump.Print_Message
           (Msg    =>
              "MCU: Ucode update @ "
              & Strings.Img (Word64'(Skp.MCU.Ucode_Address))
              & ASCII.LF & "MCU: Header version      : "
              & Strings.Img (UCH.Header_Version)
              & ASCII.LF & "MCU: Update revision     : "
              & Strings.Img (UCH.Update_Revision)
              & ASCII.LF & "MCU: Update date         : "
              & Strings.Img (UCH.Date)
              & ASCII.LF & "MCU: Processor signature : "
              & Strings.Img (UCH.Processor_Signature)
              & ASCII.LF & "MCU: Loader revision     : "
              & Strings.Img (UCH.Loader_Revision)
              & ASCII.LF & "MCU: Processor flags     : "
              & Strings.Img (UCH.Processor_Flags)));

      if My_Sig /= UCH.Processor_Signature then
         pragma Debug
           (Dump.Print_Message
              (Msg =>
                 "MCU" & Strings.Img_Nobase (Byte (CPU_Info.CPU_ID))
                 & ": ERROR - Processor signature mismatch"));
         return;
      end if;
      if Unused_Rev >= UCH.Update_Revision then
         pragma Debug
           (Dump.Print_Message
              (Msg =>
                 "MCU" & Strings.Img_Nobase (Byte (CPU_Info.CPU_ID))
                 & ": Skipping update, ucode not newer than"
                 & " active revision"));
         return;
      end if;

      Perform_Update;
      Get_Update_ID (Value => Unused_Rev);
      pragma Debug
        (Unused_Rev = UCH.Update_Revision,
         Dump.Print_Message
           (Msg => "MCU" & Strings.Img_Nobase (Byte (CPU_Info.CPU_ID))
            & ": Update to revision " & Strings.Img (Unused_Rev)
            & " successful"));
      pragma Debug
        (Unused_Rev /= UCH.Update_Revision,
         Dump.Print_Message
           (Msg => "MCU" & Strings.Img_Nobase (Byte (CPU_Info.CPU_ID))
            & ": ERROR - Update failed, revision still"
            & Strings.Img (Unused_Rev)));
   end Process;

end SK.MCU;
