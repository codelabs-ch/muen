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
is

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
      EAX, Unused_EBX, ECX, EDX : SK.Word32;
   begin
      EAX := 16#d#;
      ECX := 0;

      pragma Warnings (GNATprove, Off, "unused assignment to ""Unused_EBX""",
                       Reason => "Only parts of the CPUID result is needed");
      CPU.CPUID
        (EAX => EAX,
         EBX => Unused_EBX,
         ECX => ECX,
         EDX => EDX);
      pragma Warnings (GNATprove, On, "unused assignment to ""Unused_EBX""");

      Features_Present := Bit_Test (Value => SK.Word64 (EAX),
                                    Pos   => Constants.XCR0_FPU_STATE_FLAG);
      Features_Present := Features_Present and
        Bit_Test (Value => SK.Word64 (EAX),
                  Pos   => Constants.XCR0_SSE_STATE_FLAG);
      Features_Present := Features_Present and
        Bit_Test (Value => SK.Word64 (EAX),
                  Pos   => Constants.XCR0_AVX_STATE_FLAG);
      Features_Present := Features_Present and EDX = 0;

      Save_Area_Size := ECX <= SK.XSAVE_Area_Size;
   end Query_XSAVE;

   -------------------------------------------------------------------------

   function Has_Valid_State return Boolean
   is
      XSAVE_Support : Boolean;
      FPU_Area_Size : Boolean;
   begin
      Query_XSAVE (Features_Present => XSAVE_Support,
                   Save_Area_Size   => FPU_Area_Size);

      pragma Debug (not XSAVE_Support,
                    KC.Put_Line (Item => "XSAVE features missing"));
      pragma Debug (not FPU_Area_Size,
                    KC.Put_Line (Item => "FPU state save area too small"));

      return XSAVE_Support and FPU_Area_Size;
   end Has_Valid_State;

end SK.FPU;
