--
--  Copyright (C) 2021  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2021  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

with SK.Constants;
with SK.CPU;
with SK.Strings;

with ITS.Log_Buffer;
with ITS.Results;
with ITS.Subject_State;

package body ITS.Instructions
is

   ------------------------------------------------------------------------

   procedure Execute_RDTSC
   is
      use type Interfaces.Unsigned_32;

      Title           : constant String
        := "Execute RDTSC Instruction";
      Description     : constant String
        := "This test verifies that an attempted execution of the ``rdtsc`` "
        & "instruction is prohibited and results in a trap indicating "
        & "RDTSC execution.";
      Expected_Result : constant String
        := "VM-Exit with reason 'RDTSC' (16).";
      Log_ID          : Log_Buffer.Ext_Log_Entries_Range;
      Result          : SK.Subject_State_Type;
      Success         : Boolean;
      Start, Stop     : Interfaces.Unsigned_64;
      Unused_Value    : Interfaces.Unsigned_64;
      Src_Info        : constant String
        := Enclosing_Entity & ", " & Source_Location;
   begin
      Start := Musinfo.Instance.TSC_Schedule_Start;
      ITS.Subject_State.Result_State := SK.Null_Subject_State;

      Log_Buffer.Start_Entry (ID => Log_ID);
      Log_Buffer.Put_Line (Str => "Executing rdtsc.");
      Log_Buffer.New_Line;
      Unused_Value := SK.CPU.RDTSC;
      Result := ITS.Subject_State.Result_State;

      Log_Buffer.Put_Line (Str => ".:[ Assertions ]:.");
      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> Exit Reason");
      Log_Buffer.Put_Line
        (Str => "  Expected : " & SK.Strings.Img
           (Item => Interfaces.Unsigned_32 (SK.Constants.EXIT_REASON_RDTSC)));
      Log_Buffer.Put_Line
        (Str => "  Result   : " & SK.Strings.Img (Item => Result.Exit_Reason));

      Success := Result.Exit_Reason = SK.Constants.EXIT_REASON_RDTSC;

      Stop := Musinfo.Instance.TSC_Schedule_End;
      Results.Append
        (Title           => Title,
         Description     => Description,
         Expected        => Expected_Result,
         Source_Info     => Src_Info,
         Success         => Success,
         Start_Timestamp => Start,
         End_Timestamp   => Stop,
         Log_Entry       => Log_ID);
   end Execute_RDTSC;

   ------------------------------------------------------------------------

   procedure Execute_VMXOFF
   is
      use type Interfaces.Unsigned_32;

      EXIT_REASON_VMXOFF : constant Interfaces.Unsigned_32 := 26;

      Title           : constant String
        := "Execute VMXOFF Instruction";
      Description     : constant String
        := "This test verifies that an attempted execution of the ``vmxoff`` "
        & "instruction is prohibited and results in a trap indicating "
        & "VMXOFF execution.";
      Expected_Result : constant String
        := "VM-Exit with reason 'VMXOFF' (26).";
      Log_ID          : Log_Buffer.Ext_Log_Entries_Range;
      Result          : SK.Subject_State_Type;
      Success         : Boolean;
      Start, Stop     : Interfaces.Unsigned_64;
      Unused_Value    : Interfaces.Unsigned_64;
      Src_Info        : constant String
        := Enclosing_Entity & ", " & Source_Location;
   begin
      Start := Musinfo.Instance.TSC_Schedule_Start;
      ITS.Subject_State.Result_State := SK.Null_Subject_State;

      Log_Buffer.Start_Entry (ID => Log_ID);
      Log_Buffer.Put_Line (Str => "Executing vmxoff.");
      Log_Buffer.New_Line;

      System.Machine_Code.Asm
        (Template => "vmxoff",
         Clobber  => "cc",
         Volatile => True);

      Result := ITS.Subject_State.Result_State;

      Log_Buffer.Put_Line (Str => ".:[ Assertions ]:.");
      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> Exit Reason");
      Log_Buffer.Put_Line
        (Str => "  Expected : " & SK.Strings.Img
           (Item => EXIT_REASON_VMXOFF));
      Log_Buffer.Put_Line
        (Str => "  Result   : " & SK.Strings.Img (Item => Result.Exit_Reason));

      Success := Result.Exit_Reason = EXIT_REASON_VMXOFF;

      Stop := Musinfo.Instance.TSC_Schedule_End;
      Results.Append
        (Title           => Title,
         Description     => Description,
         Expected        => Expected_Result,
         Source_Info     => Src_Info,
         Success         => Success,
         Start_Timestamp => Start,
         End_Timestamp   => Stop,
         Log_Entry       => Log_ID);
   end Execute_VMXOFF;

   -------------------------------------------------------------------------

   procedure Write_To_CR0
   is
      use type Interfaces.Unsigned_32;
      use type Interfaces.Unsigned_64;

      CR0_Value       : constant Interfaces.Unsigned_64 := 16#0001_0033#;
      Title           : constant String
        := "Write to CR0";
      Description     : constant String
        := "This test verifies that an attempted write to the CR0 control "
        & "register is prohibited and results in a trap indicating "
        & "control-register access.";
      Expected_Result : constant String
        := "VM-Exit with reason 'Control-register accesses' (28) and "
        & "qualification (0) designating a write to CR0, see "
        & "Intel SDM Vol. 3C, '27.2.1 Basic VM-Exit Information', table 27-3.";
      Ref_Quali       : constant Interfaces.Unsigned_64 := 16#0000#;
      Log_ID          : Log_Buffer.Ext_Log_Entries_Range;
      Result          : SK.Subject_State_Type;
      Success         : Boolean;
      Start, Stop     : Interfaces.Unsigned_64;
      Src_Info        : constant String
        := Enclosing_Entity & ", " & Source_Location;
   begin
      Start := Musinfo.Instance.TSC_Schedule_Start;
      ITS.Subject_State.Result_State := SK.Null_Subject_State;

      Log_Buffer.Start_Entry (ID => Log_ID);
      Log_Buffer.Put_Line (Str => "Executing mov to cr0.");
      Log_Buffer.New_Line;
      System.Machine_Code.Asm
        (Template => "movq %0, %%cr0",
         Inputs   => (Interfaces.Unsigned_64'Asm_Input ("r", CR0_Value)),
         Volatile => True);

      Result := ITS.Subject_State.Result_State;

      Log_Buffer.Put_Line (Str => ".:[ Assertions ]:.");
      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> Exit Reason");
      Log_Buffer.Put_Line
        (Str => "  Expected : " & SK.Strings.Img
           (Item => Interfaces.Unsigned_32
                (SK.Constants.EXIT_REASON_CR_ACCESS)));
      Log_Buffer.Put_Line
        (Str => "  Result   : " & SK.Strings.Img (Item => Result.Exit_Reason));
      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> Exit Qualification");
      Log_Buffer.Put_Line (Str => "  Expected : "
                           & SK.Strings.Img (Item => Ref_Quali));
      Log_Buffer.Put_Line
        (Str => "  Result   : "
         & SK.Strings.Img (Item => Result.Exit_Qualification));
      Success := Result.Exit_Reason = SK.Constants.EXIT_REASON_CR_ACCESS
        and then Result.Exit_Qualification = Ref_Quali;

      Stop := Musinfo.Instance.TSC_Schedule_End;
      Results.Append
        (Title           => Title,
         Description     => Description,
         Expected        => Expected_Result,
         Source_Info     => Src_Info,
         Success         => Success,
         Start_Timestamp => Start,
         End_Timestamp   => Stop,
         Log_Entry       => Log_ID);
   end Write_To_CR0;

   -------------------------------------------------------------------------

   procedure Write_To_CR3
   is
      use type Interfaces.Unsigned_32;
      use type Interfaces.Unsigned_64;

      CR3_Value       : constant Interfaces.Unsigned_64 := 16#fefe_0000#;

      Title           : constant String
        := "Write to CR3";
      Description     : constant String
        := "This test verifies that an attempted write to the CR3 control "
        & "register is prohibited and results in a trap indicating "
        & "control-register access.";
      Expected_Result : constant String
        := "VM-Exit with reason 'Control-register accesses' (28) and "
        & "qualification (3) designating a write to CR3, see "
        & "Intel SDM Vol. 3C, '27.2.1 Basic VM-Exit Information', table 27-3.";
      Ref_Quali       : constant Interfaces.Unsigned_64 := 16#0003#;
      Log_ID          : Log_Buffer.Ext_Log_Entries_Range;
      Result          : SK.Subject_State_Type;
      Success         : Boolean;
      Start, Stop     : Interfaces.Unsigned_64;
      Src_Info        : constant String
        := Enclosing_Entity & ", " & Source_Location;
   begin
      Start := Musinfo.Instance.TSC_Schedule_Start;
      ITS.Subject_State.Result_State := SK.Null_Subject_State;

      Log_Buffer.Start_Entry (ID => Log_ID);
      Log_Buffer.Put_Line (Str => "Executing mov to cr3.");
      Log_Buffer.New_Line;
      System.Machine_Code.Asm
        (Template => "movq %0, %%cr3",
         Inputs   => (Interfaces.Unsigned_64'Asm_Input ("r", CR3_Value)),
         Volatile => True);

      Result := ITS.Subject_State.Result_State;

      Log_Buffer.Put_Line (Str => ".:[ Assertions ]:.");
      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> Exit Reason");
      Log_Buffer.Put_Line
        (Str => "  Expected : " & SK.Strings.Img
           (Item => Interfaces.Unsigned_32
                (SK.Constants.EXIT_REASON_CR_ACCESS)));
      Log_Buffer.Put_Line
        (Str => "  Result   : " & SK.Strings.Img (Item => Result.Exit_Reason));
      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> Exit Qualification");
      Log_Buffer.Put_Line (Str => "  Expected : "
                           & SK.Strings.Img (Item => Ref_Quali));
      Log_Buffer.Put_Line
        (Str => "  Result   : "
         & SK.Strings.Img (Item => Result.Exit_Qualification));
      Success := Result.Exit_Reason = SK.Constants.EXIT_REASON_CR_ACCESS
        and then Result.Exit_Qualification = Ref_Quali;

      Stop := Musinfo.Instance.TSC_Schedule_End;
      Results.Append
        (Title           => Title,
         Description     => Description,
         Expected        => Expected_Result,
         Source_Info     => Src_Info,
         Success         => Success,
         Start_Timestamp => Start,
         End_Timestamp   => Stop,
         Log_Entry       => Log_ID);
   end Write_To_CR3;

end ITS.Instructions;
