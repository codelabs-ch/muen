--
--  Copyright (C) 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.KC;
with SK.CPU;
with SK.Bitops;
with SK.Constants;
with SK.Dump;
with SK.Strings;

package body SK.FPU
with
   Refined_State => (State => (Subject_FPU_States, Active_XCR0_Features,
                               Current_XCR0))
is

   Null_FPU_State : constant FPU_State_Type
     := (XCR0       => 0,
         Padding    => (others => 0),
         XSAVE_Area => (Legacy_Header    => Null_XSAVE_Legacy_Header,
                        Legacy_Registers => (others => 0),
                        Legacy_Reserved  => (others => 0),
                        XSAVE_Header     => Null_XSAVE_Header,
                        Extended_Region  => (others => 0)));

   -------------------------------------------------------------------------

   procedure Enable
   with
      Refined_Global  => (In_Out => (Current_XCR0, X86_64.State),
                          Output => Active_XCR0_Features),
      Refined_Depends => (Active_XCR0_Features => X86_64.State,
                          (Current_XCR0,
                           X86_64.State)       => (Current_XCR0, X86_64.State))
   is
      CR4 : Word64;
      EAX, Unused_EBX, Unused_ECX, EDX : Word32;
   begin
      CR4 := CPU.Get_CR4;
      CR4 := Bitops.Bit_Set (Value => CR4,
                             Pos   => Constants.CR4_OSFXSR_FLAG);
      CR4 := Bitops.Bit_Set (Value => CR4,
                             Pos   => Constants.CR4_XSAVE_FLAG);
      CPU.Set_CR4 (Value => CR4);

      EAX := 16#d#;
      Unused_ECX := 0;

      CPU.CPUID
        (EAX => EAX,
         EBX => Unused_EBX,
         ECX => Unused_ECX,
         EDX => EDX);

      Active_XCR0_Features := Word64 (EAX) + Word64 (EDX) * 2 ** 32;
      Active_XCR0_Features
        := Active_XCR0_Features and Constants.XCR0_Supported_Features_Mask;
      pragma Debug (Dump.Print_Message
                    (Msg  => "XCR0: " & Strings.Img (Active_XCR0_Features)));
      Write_XCR0 (Value => Active_XCR0_Features);
   end Enable;

   -------------------------------------------------------------------------

   procedure Get_Registers
     (ID   :     Skp.Global_Subject_ID_Type;
      Regs : out XSAVE_Legacy_Header_Type)
   with
      Refined_Global  => (Input => Subject_FPU_States),
      Refined_Depends => (Regs  => (ID, Subject_FPU_States)),
      Refined_Post    => Regs = Subject_FPU_States (ID).XSAVE_Area.Legacy_Header
   is
   begin
      Regs := Subject_FPU_States (ID).XSAVE_Area.Legacy_Header;
   end Get_Registers;

   -------------------------------------------------------------------------

   procedure Reset_State (ID : Skp.Global_Subject_ID_Type)
   with
      Refined_Global  => (Input  => Active_XCR0_Features,
                          In_Out => Subject_FPU_States),
      Refined_Depends => (Subject_FPU_States =>+ (ID, Active_XCR0_Features,
                                                   Subject_FPU_States))
   is
   begin
      --D @Interface
      --D Set FPU state of subject with specified ID to
      --D \texttt{Null\_FPU\_State} and set XCR0, FCW and MXCSR fields to their
      --D initial values, see Intel SDM Vol. 1,
      --D "13.6 Processor Tracking of XSAVE-Managed State" and Intel SDM Vol.
      --D 3A, "9.1.1 Processor State After Reset".
      Subject_FPU_States (ID) := Null_FPU_State;
      Subject_FPU_States (ID).XCR0 := Active_XCR0_Features;
      Subject_FPU_States (ID).XSAVE_Area.Legacy_Header.FCW
        := Constants.FCW_Default_Value;
      Subject_FPU_States (ID).XSAVE_Area.Legacy_Header.MXCSR
        := Constants.MXCSR_Default_Value;
      Subject_FPU_States (ID).XSAVE_Area.Legacy_Header.MXCSR_Mask
        := Constants.MXCSR_Mask_Default_Value;
   end Reset_State;

   -------------------------------------------------------------------------

   procedure Restore_State (ID : Skp.Global_Subject_ID_Type)
   with
     Refined_Global  => (Input  => (Subject_FPU_States, Active_XCR0_Features),
                         In_Out => (Current_XCR0, X86_64.State)),
     Refined_Depends => ((Current_XCR0,
                          X86_64.State) =>+ (ID, Subject_FPU_States,
                                             Current_XCR0,
                                             Active_XCR0_Features))
   is
   begin
      Write_XCR0 (Value => Active_XCR0_Features);
      CPU.XRSTOR (Source => Subject_FPU_States (ID).XSAVE_Area,
                  State  => Active_XCR0_Features);
      Write_XCR0 (Value => Subject_FPU_States (ID).XCR0);
   end Restore_State;

   -------------------------------------------------------------------------

   procedure Save_State (ID : Skp.Global_Subject_ID_Type)
   with
      Refined_Global  => (Input  => Active_XCR0_Features,
                          In_Out => (Current_XCR0, Subject_FPU_States,
                                     X86_64.State)),
      Refined_Depends => (Subject_FPU_States =>+ (ID, Active_XCR0_Features,
                                                  Current_XCR0, X86_64.State),
                          (Current_XCR0,
                           X86_64.State)     =>+ (Active_XCR0_Features,
                                                  Current_XCR0))
   is
   begin
      Write_XCR0 (Value => Active_XCR0_Features);
      CPU.XSAVE (Target => Subject_FPU_States (ID).XSAVE_Area,
                 State  => Active_XCR0_Features);
   end Save_State;

   -------------------------------------------------------------------------

   --  Sets Features_Present to True if XSAVE has support for FPU, SSE and AVX
   --  state handling. Save_Area_Size is set to True if the FPU state save area
   --  is larger than the reported maximum XSAVE area size.
   procedure Query_XSAVE
     (Features_Present : out Boolean;
      Save_Area_Size   : out Boolean)
   with
      Global  => (Input => X86_64.State),
      Depends => ((Features_Present, Save_Area_Size) => X86_64.State)
   is
      EAX, Unused_EBX, ECX, Unused_EDX : Word32;
   begin
      EAX := 16#d#;
      ECX := 0;

      CPU.CPUID
        (EAX => EAX,
         EBX => Unused_EBX,
         ECX => ECX,
         EDX => Unused_EDX);

      Features_Present := Bitops.Bit_Test
        (Value => SK.Word64 (EAX),
         Pos   => Constants.XCR0_FPU_STATE_FLAG);
      Save_Area_Size := ECX <= SK.XSAVE_Area_Size;
   end Query_XSAVE;

   -------------------------------------------------------------------------

   procedure Check_State
     (Is_Valid : out Boolean;
      Ctx      : out Crash_Audit_Types.FPU_Init_Context_Type)
   is
   begin
      Ctx := Crash_Audit_Types.Null_FPU_Init_Context;

      Query_XSAVE (Features_Present => Ctx.XSAVE_Support,
                   Save_Area_Size   => Ctx.Area_Size);

      pragma Debug
        (not Ctx.XSAVE_Support,
         KC.Put_Line (Item => "Init: FPU XSAVE feature missing"));
      pragma Debug
        (not Ctx.Area_Size,
         KC.Put_Line (Item => "Init: FPU state save area too small"));

      Is_Valid := Ctx.XSAVE_Support and Ctx.Area_Size;
   end Check_State;

   -------------------------------------------------------------------------

   procedure Set_XCR0
     (ID    : Skp.Global_Subject_ID_Type;
      Value : Word64)
   with
      Refined_Global  => (In_Out => Subject_FPU_States),
      Refined_Depends => (Subject_FPU_States  =>+ (ID, Value)),
      Refined_Post    => Subject_FPU_States (ID).XCR0 = Value
   is
      XSTATE_BV    : constant Word64
        := Subject_FPU_States (ID).XSAVE_Area.XSAVE_Header.XSTATE_BV;
      Cleared_Bits : constant Word64 := (Value xor XSTATE_BV) and XSTATE_BV;
   begin
      Subject_FPU_States (ID).XCR0 := Value;
      if Cleared_Bits > 0 then

         --  To ensure that extended processor state that is being disabled does
         --  not incur a performance penalty, make sure the associated XSAVE
         --  state components are set to their initial value upon the next
         --  XRSTOR, see Intel SDM Vol. 3A, "13.5.3 Enable the Use Of XSAVE
         --  Feature Set And XSAVE State Components".

         Subject_FPU_States (ID).XSAVE_Area.XSAVE_Header.XSTATE_BV
           := XSTATE_BV - Cleared_Bits;
      end if;
   end Set_XCR0;

   -------------------------------------------------------------------------

   procedure Write_XCR0 (Value : Word64)
   with
      Refined_Global  => (In_Out => (Current_XCR0, X86_64.State)),
      Refined_Depends => ((Current_XCR0,
                           X86_64.State) =>+ (Current_XCR0, Value))
   is
   begin
      if Current_XCR0 /= Value then
         Current_XCR0 := Value;
         CPU.XSETBV (Register => 0,
                     Value    => Current_XCR0);
      end if;
   end Write_XCR0;

end SK.FPU;
