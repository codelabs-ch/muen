--
--  Copyright (C) 2024  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2024  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with System.Machine_Code;

with SK.Constants;
with SK.CPU;
with SK.Dumper;
with SK.Strings;

with Debuglog.Client;

with Log;

package body FPU
is

   XCR0  : SK.Word64;
   State : SK.XSAVE_Area_Type;

   --  Read current state of FPU and store it in the XCR0 and State variables.
   procedure Read_FPU_State;

   --  Returns the current FCW value from the FPU.
   function Read_FCW return SK.Word16;

   --  Write the give value to the FCW FPU register.
   procedure Write_FCW (Value : SK.Word16);

   --  Log given FPU register values.
   procedure Put_Register (Regs : SK.XSAVE_Legacy_Header_Type);

   -------------------------------------------------------------------------

   procedure Do_Tests
   is
      use type SK.Word16;

      Value : SK.Word16 := 16#0040#;
   begin
      if State.Legacy_Header.FCW = Value then
         Log.Put_Line (Item => "FCW already contains value to be written");
      end if;

      --  Write FCW value to FPU register and read back the state to the XSAVE
      --  area to show it has the value just written.

      Write_FCW (Value => Value);
      Read_FPU_State;
      if State.Legacy_Header.FCW = Value then
         Log.Put_Line (Item => "Setting FCW successful");
      else
         Log.Put_Line (Item => "Setting FCW to value failed: "
                       & SK.Strings.Img (Value) & " /= "
                       & SK.Strings.Img (State.Legacy_Header.FCW));
      end if;

      --  Clear XSTATE_BV state-component bitmap in XSAVE area XSAVE header and
      --  restore state to FPU. The xrstor instruction will restore the initial
      --  configuration for all state-components except MXCSR.

      State.XSAVE_Header.XSTATE_BV := 0;
      SK.CPU.XRSTOR (Source => State,
                     State  => XCR0);
      Value := Read_FCW;
      if Value = SK.Constants.FCW_Default_Value then
         Log.Put_Line (Item => "Resetting FCW with xrstor successful");
      else
         Log.Put_Line
           (Item => "Resetting FCW with xrstor failed: "
            & SK.Strings.Img (Value) & " /= "
            & SK.Strings.Img (SK.Word16 (SK.Constants.FCW_Default_Value)));
      end if;
   end Do_Tests;

   -------------------------------------------------------------------------

   procedure Log_State (Title : String)
   is
   begin
      Log.Put_Line (Item => Title);
      Log.Put_Line
        (Item => "XCR0 : " & SK.Strings.Img (XCR0)
         & ", XSTATE_BV " & SK.Strings.Img (State.XSAVE_Header.XSTATE_BV)
         & ", XCOMP_BV " & SK.Strings.Img (State.XSAVE_Header.XCOMP_BV));
      Put_Register (Regs => State.Legacy_Header);

      Log.Put_Line (Item => "## SSE Registers");
      declare
         use SK.Strings;

         type MMX_Register_Range is range 0 .. 23;

         type Reg_Name_Array is array (MMX_Register_Range) of String (1 .. 5);

         Reg_Name : constant Reg_Name_Array
           := ("ST0  ", "ST1  ", "ST2  ", "ST3  ", "ST4  ", "ST5  ", "ST6  ",
               "ST7  ", "XMM0 ", "XMM1 ", "XMM2 ", "XMM3 ", "XMM4 ", "XMM5 ",
               "XMM6 ", "XMM7 ", "XMM8 ", "XMM9 ", "XMM10", "XMM11", "XMM12",
               "XMM13", "XMM14", "XMM15");

         MMX : SK.XSAVE_Legacy_Registers_Type renames State.Legacy_Registers;
         Idx : Natural;
      begin
         for I in MMX_Register_Range loop
            Idx := Natural (I) * 16;

            --  Shift index by one since MMX registers uses 1-based array range.

            Idx := Idx + 1;

            Log.Put_Line
              (Item => " " & Reg_Name (I) & ": "
               & Img_Nobase (MMX (Idx)) & Img_Nobase (MMX (Idx + 1))
               & Img_Nobase (MMX (Idx + 2)) & Img_Nobase (MMX (Idx + 3))
               & Img_Nobase (MMX (Idx + 4)) & Img_Nobase (MMX (Idx + 5))
               & Img_Nobase (MMX (Idx + 6)) & Img_Nobase (MMX (Idx + 7))
               & Img_Nobase (MMX (Idx + 8)) & Img_Nobase (MMX (Idx + 9))
               & Img_Nobase (MMX (Idx + 10)) & Img_Nobase (MMX (Idx + 11))
               & Img_Nobase (MMX (Idx + 12)) & Img_Nobase (MMX (Idx + 13))
               & Img_Nobase (MMX (Idx + 14)) & Img_Nobase (MMX (Idx + 15)));
         end loop;
      end;
   end Log_State;

   -------------------------------------------------------------------------

   procedure Put_Register (Regs : SK.XSAVE_Legacy_Header_Type)
   with SPARK_Mode => Off
   is
      package D is new SK.Dumper
        (New_Line   => Debuglog.Client.New_Line,
         Put_Line   => Log.Put_Line,
         Put_String => Debuglog.Client.Put);
   begin
      D.Output_FPU_Registers (Regs => Regs);
   end Put_Register;

   -------------------------------------------------------------------------

   function Read_FCW return SK.Word16
   with SPARK_Mode => Off
   is
      Result : SK.Word16;
   begin
      System.Machine_Code.Asm
        (Template => "fnstcw %0",
         Outputs  => (SK.Word16'Asm_Output ("=m", Result)),
         Volatile => True);
      return Result;
   end Read_FCW;

   -------------------------------------------------------------------------

   procedure Read_FPU_State
   is
   begin
      SK.CPU.XGETBV
        (Register => 0,
         Value    => XCR0);
      SK.CPU.XSAVE (Target => State,
                    State  => XCR0);
   end Read_FPU_State;

   -------------------------------------------------------------------------

   procedure Write_FCW (Value : SK.Word16)
   with SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "fldcw %0",
         Inputs   => (SK.Word16'Asm_Input ("m", Value)),
         Volatile => True);
   end Write_FCW;

   -------------------------------------------------------------------------

begin

   --  Save the initial FPU state prior to execution of other example code.

   Read_FPU_State;
end FPU;
