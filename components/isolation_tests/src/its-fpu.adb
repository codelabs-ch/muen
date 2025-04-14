--
--  Copyright (C) 2025  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2025  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

with SK.CPU;
with SK.Strings;

with ITS.Log_Buffer;
with ITS.Results;

package body ITS.FPU
is

   XCR0  : Interfaces.Unsigned_64;
   State : SK.XSAVE_Area_Type;

   Testsuite_Name : constant String := Enclosing_Entity;

   --  Read current state of FPU and store it in the XCR0 and State variables.
   procedure Read_FPU_State;

   -------------------------------------------------------------------------

   procedure Verify_Initial_State
   is
      use type Interfaces.Unsigned_16;
      use type SK.XSAVE_Legacy_Registers_Type;
      use type SK.XSAVE_Extended_Region_Type;

      Title           : constant String
        := "Verify initial FPU State";
      Description     : constant String
        := "This test verifies the initial subject-visible FPU state.";
      Expected_Result : constant String
        := "The FPU legacy and XSAVE header values contain the expected "
        & "values and the FPU legacy registers and extended region are null.";
      Log_ID          : Log_Buffer.Ext_Log_Entries_Range;
      Success         : Boolean;
      Unused_Byte     : Interfaces.Unsigned_8;
      Start, Stop     : Interfaces.Unsigned_64;
      Src_Info        : constant String
        := Enclosing_Entity & ", " & Source_Location;

      Null_Legacy_Registers : constant SK.XSAVE_Legacy_Registers_Type
        := (others => 0);
      Null_Extended_Region  : constant SK.XSAVE_Extended_Region_Type
        := (others => 0);

      XCR0_Ref       : constant Interfaces.Unsigned_64 := 7;
      XSTATE_BV_Ref  : constant Interfaces.Unsigned_64 := 7;
      XCOMP_BV_Ref   : constant Interfaces.Unsigned_64 := 0;
      MXCSR_Ref      : constant Interfaces.Unsigned_32 := 16#1f80#;
      MXCSR_Mask_Ref : constant Interfaces.Unsigned_32 := 16#ffff#;
      FCW_Ref        : constant Interfaces.Unsigned_16 := 16#037f#;
      FSW_Ref        : constant Interfaces.Unsigned_16 := 0;
      FTW_Ref        : constant Interfaces.Unsigned_16 := 16#00ff#;
      FOP_Ref        : constant Interfaces.Unsigned_16 := 0;
      FIP_Ref        : constant Interfaces.Unsigned_64 := 0;
      FDP_Ref        : constant Interfaces.Unsigned_64 := 0;
   begin
      Start := Musinfo.Instance.TSC_Schedule_Start;

      Log_Buffer.Start_Entry (ID => Log_ID);
      Log_Buffer.Put_Line (Str => "Verifying FPU state in memory");
      Log_Buffer.New_Line;

      Log_Buffer.Put_Line (Str => ".:[ Assertions ]:.");

      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> XCR0");
      Log_Buffer.Put_Line  (Str => "  Expected : "
         & SK.Strings.Img
           (Item => SK.Word64 (XCR0_Ref)));
      Log_Buffer.Put_Line (Str => "  Result   : "
                           & SK.Strings.Img (Item => XCR0));

      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> XSTATE_BV");
      Log_Buffer.Put_Line
        (Str => "  Expected : " & SK.Strings.Img (XSTATE_BV_Ref));
      Log_Buffer.Put_Line
        (Str => "  Result   : "
         & SK.Strings.Img (Item => State.XSAVE_Header.XSTATE_BV));

      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> XCOMP_BV");
      Log_Buffer.Put_Line
        (Str => "  Expected : " & SK.Strings.Img (XCOMP_BV_Ref));
      Log_Buffer.Put_Line
        (Str => "  Result   : "
         & SK.Strings.Img (Item => State.XSAVE_Header.XCOMP_BV));

      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> MXCSR");
      Log_Buffer.Put_Line
        (Str => "  Expected : " & SK.Strings.Img (MXCSR_Ref));
      Log_Buffer.Put_Line
        (Str => "  Result   : "
         & SK.Strings.Img (Item => State.Legacy_Header.MXCSR));

      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> MXCSR mask");
      Log_Buffer.Put_Line
        (Str => "  Expected : " & SK.Strings.Img (MXCSR_Mask_Ref));
      Log_Buffer.Put_Line
        (Str => "  Result   : "
         & SK.Strings.Img (Item => State.Legacy_Header.MXCSR_Mask));

      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> FCW");
      Log_Buffer.Put_Line
        (Str => "  Expected : " & SK.Strings.Img (FCW_Ref));
      Log_Buffer.Put_Line
        (Str => "  Result   : "
         & SK.Strings.Img (Item => State.Legacy_Header.FCW));

      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> FSW");
      Log_Buffer.Put_Line
        (Str => "  Expected : " & SK.Strings.Img (FSW_Ref));
      Log_Buffer.Put_Line
        (Str => "  Result   : "
         & SK.Strings.Img (Item => State.Legacy_Header.FSW));

      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> FTW");
      Log_Buffer.Put_Line
        (Str => "  Expected : " & SK.Strings.Img (FTW_Ref));
      Log_Buffer.Put_Line
        (Str => "  Result   : "
         & SK.Strings.Img (Item => State.Legacy_Header.FTW));

      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> FOP");
      Log_Buffer.Put_Line
        (Str => "  Expected : " & SK.Strings.Img (FOP_Ref));
      Log_Buffer.Put_Line
        (Str => "  Result   : "
         & SK.Strings.Img (Item => State.Legacy_Header.FOP));

      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> FIP");
      Log_Buffer.Put_Line
        (Str => "  Expected : " & SK.Strings.Img (FIP_Ref));
      Log_Buffer.Put_Line
        (Str => "  Result   : "
         & SK.Strings.Img (Item => State.Legacy_Header.FIP));

      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> FDP");
      Log_Buffer.Put_Line
        (Str => "  Expected : " & SK.Strings.Img (FDP_Ref));
      Log_Buffer.Put_Line
        (Str => "  Result   : "
         & SK.Strings.Img (Item => State.Legacy_Header.FDP));

      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> Legacy registers");
      Log_Buffer.Put_Line (Str => "  Expected : all-0");
      Log_Buffer.Put (Str => "  Result   : ");
      if State.Legacy_Registers = Null_Legacy_Registers then
         Log_Buffer.Put_Line (Str => "True");
      else
         Log_Buffer.Put_Line (Str => "False");
      end if;

      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> Extended region");
      Log_Buffer.Put_Line (Str => "  Expected : all-0");
      Log_Buffer.Put (Str => "  Result   : ");
      if State.Extended_Region = Null_Extended_Region then
         Log_Buffer.Put_Line (Str => "True");
      else
         Log_Buffer.Put_Line (Str => "False");
      end if;

      Success := XCR0 = XCR0_Ref
         and State.XSAVE_Header.XSTATE_BV = XSTATE_BV_Ref
         and State.XSAVE_Header.XCOMP_BV = XCOMP_BV_Ref
         and State.Legacy_Header.MXCSR = MXCSR_Ref
         and State.Legacy_Header.MXCSR_Mask = MXCSR_Mask_Ref
         and State.Legacy_Header.FCW = FCW_Ref
         and State.Legacy_Header.FSW = FSW_Ref
         and State.Legacy_Header.FTW = FTW_Ref
         and State.Legacy_Header.FOP = FOP_Ref
         and State.Legacy_Header.FIP = FIP_Ref
         and State.Legacy_Header.FDP = FDP_Ref
         and State.Legacy_Registers = Null_Legacy_Registers
         and State.Extended_Region = Null_Extended_Region;

      Stop := Musinfo.Instance.TSC_Schedule_End;
      Results.Append
        (Title           => Title,
         Description     => Description,
         Expected        => Expected_Result,
         Source_Info     => Src_Info,
         Testsuite       => Testsuite_Name,
         Success         => Success,
         Start_Timestamp => Start,
         End_Timestamp   => Stop,
         Log_Entry       => Log_ID);
   end Verify_Initial_State;

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

begin

   --  Save the initial FPU state.

   Read_FPU_State;
end ITS.FPU;
