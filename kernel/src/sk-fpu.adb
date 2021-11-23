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
   Refined_State => (State => (Subject_FPU_States, Active_XCR0_Features))
is

   Null_FPU_State : constant FPU_State_Type
     := (XCR0       => 0,
         Padding    => (others => 0),
         XSAVE_Area => (Legacy_Header   => Null_XSAVE_Legacy_Header,
                        Extended_Region => (others => 0)));

   -------------------------------------------------------------------------

   procedure Enable
   with
      Refined_Global  => (In_Out => X86_64.State,
                          Output => Active_XCR0_Features),
      Refined_Depends => ((Active_XCR0_Features, X86_64.State) => X86_64.State)
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
      CPU.XSETBV (Register => 0,
                  Value    => Active_XCR0_Features);
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
                          In_Out => (Subject_FPU_States, X86_64.State)),
      Refined_Depends => ((Subject_FPU_States,
                           X86_64.State)       => (ID, Active_XCR0_Features,
                                                   X86_64.State,
                                                   Subject_FPU_States))
   is
   begin
      --D @Interface
      --D Set FPU state of subject with specified ID to
      --D \texttt{Null\_FPU\_State}.
      Subject_FPU_States (ID) := Null_FPU_State;
      Subject_FPU_States (ID).XCR0 := Active_XCR0_Features;
      Restore_State (ID => ID);
      CPU.Fninit;
      CPU.Ldmxcsr (Value => Constants.MXCSR_Default_Value);
      Save_State (ID => ID);
   end Reset_State;

   -------------------------------------------------------------------------

   procedure Restore_State (ID : Skp.Global_Subject_ID_Type)
   with
     Refined_Global  => (Input  => (Subject_FPU_States, Active_XCR0_Features),
                         In_Out => X86_64.State),
     Refined_Depends => (X86_64.State =>+ (ID, Subject_FPU_States,
                                           Active_XCR0_Features))
   is
   begin
      CPU.XRSTOR (Source => Subject_FPU_States (ID).XSAVE_Area,
                  State  => Active_XCR0_Features);
   end Restore_State;

   -------------------------------------------------------------------------

   procedure Save_State (ID : Skp.Global_Subject_ID_Type)
   with
      Refined_Global  => (Input  => (X86_64.State, Active_XCR0_Features),
                          In_Out => Subject_FPU_States),
      Refined_Depends => (Subject_FPU_States =>+ (ID, Active_XCR0_Features,
                          X86_64.State))
   is
   begin
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

end SK.FPU;
