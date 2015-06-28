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
with SK.Constants;

package body SK.FPU
with
   Refined_State => (State => Subject_FPU_States)
is

   type Subject_FPU_State_Array is array
     (Skp.Subject_Id_Type) of SK.FPU_Save_Area_Type;

   Subject_FPU_States : Subject_FPU_State_Array
     := (others => FPU_Save_Area_Type'(others => 0));

   -------------------------------------------------------------------------

   procedure Enable
   is
   begin
      null;
   end Enable;

   -------------------------------------------------------------------------

   procedure Restore_State (ID : Skp.Subject_Id_Type)
   with
     Refined_Global  => (Input  => Subject_FPU_States,
                         In_Out => X86_64.State),
     Refined_Depends => (X86_64.State =>+ (ID, Subject_FPU_States))
   is
   begin
      CPU.Fxrstor (Source => Subject_FPU_States (ID));
   end Restore_State;

   -------------------------------------------------------------------------

   procedure Save_State (ID : Skp.Subject_Id_Type)
   with
      Refined_Global  => (Input  => X86_64.State,
                          In_Out => Subject_FPU_States),
      Refined_Depends => (Subject_FPU_States =>+ (ID, X86_64.State))
   is
   begin
      CPU.Fxsave (Target => Subject_FPU_States (ID));
   end Save_State;

   -------------------------------------------------------------------------

   --  Returns True if SSE/SSE2/SSE3/SSSE3 extensions and support for the
   --  FXSAVE and FXRSTOR instructions are present, see Intel SDM Vol. 3A,
   --  chapter 9.6.
   function Has_Valid_State return Boolean
   is
      Valid_State : Boolean;
      Unused_EAX, Unused_EBX, ECX, EDX : SK.Word32;
   begin
      Unused_EAX := 1;
      ECX        := 0;

      pragma Warnings
        (GNATprove, Off, "unused assignment to ""Unused_E*X""",
         Reason => "Only parts of the CPUID result is needed");
      CPU.CPUID
        (EAX => Unused_EAX,
         EBX => Unused_EBX,
         ECX => ECX,
         EDX => EDX);
      pragma Warnings (GNATprove, On, "unused assignment to ""Unused_E*X""");

      Valid_State := Bit_Test
        (Value => Word64 (EDX),
         Pos   => Constants.CPUID_FEATURE_FXSR) and then
        Bit_Test
          (Value => Word64 (EDX),
           Pos   => Constants.CPUID_FEATURE_SSE) and then
        Bit_Test
          (Value => Word64 (EDX),
           Pos   => Constants.CPUID_FEATURE_SSE2) and then
        Bit_Test
          (Value => Word64 (ECX),
           Pos   => Constants.CPUID_FEATURE_SSE3) and then
        Bit_Test
          (Value => Word64 (ECX),
           Pos   => Constants.CPUID_FEATURE_SSSE3);

      pragma Debug (not Valid_State,
                    KC.Put_Line (Item => "FXSAVE features missing"));

      return Valid_State;
   end Has_Valid_State;

end SK.FPU;
