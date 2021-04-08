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

with Interfaces;

with SK.CPU;
with SK.Constants;
with SK.Strings;

with ITS.Log_Buffer;
with ITS.Results;
with ITS.Subject_State;

package body ITS.MSRs
is

   -------------------------------------------------------------------------

   procedure Read_From_Disallowed_Register
   is
      use type Interfaces.Unsigned_32;

      MSR_Address     : constant Interfaces.Unsigned_32 := 16#0010#;
      Title           : constant String
        := "Read Access to disallowed MSR";
      Description     : constant String
        := "This test verifies that an attempted read access to a disallowed "
        & "Model-Specific Register is prohibited and results in a trap "
        & "indicating a RDMSR.";
      Expected_Result : constant String
        := "VM-Exit with reason 'RDMSR' (31).";
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
      Log_Buffer.Put_Line (Str => "Reading from MSR "
                           & SK.Strings.Img (Item => MSR_Address) & ".");
      Log_Buffer.New_Line;

      Unused_Value := SK.CPU.Get_MSR64 (Register => MSR_Address);
      Result := ITS.Subject_State.Result_State;

      Log_Buffer.Put_Line (Str => ".:[ Assertions ]:.");
      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> Exit Reason");
      Log_Buffer.Put_Line
        (Str => "  Expected : "
         & SK.Strings.Img
           (Item => SK.Word32 (SK.Constants.EXIT_REASON_RDMSR)));
      Log_Buffer.Put_Line
        (Str => "  Result   : " & SK.Strings.Img (Item => Result.Exit_Reason));

      Success := Result.Exit_Reason = SK.Constants.EXIT_REASON_RDMSR;

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
   end Read_From_Disallowed_Register;

   -------------------------------------------------------------------------

   procedure Write_To_Disallowed_Register
   is
      use type Interfaces.Unsigned_32;

      MSR_Address     : constant Interfaces.Unsigned_32 := 16#c000_1000#;
      Title           : constant String
        := "Write Access to disallowed MSR";
      Description     : constant String
        := "This test verifies that an attempted write access to a disallowed "
        & "Model-Specific Register is prohibited and results in a trap "
        & "indicating a WRMSR.";
      Expected_Result : constant String
        := "VM-Exit with reason 'WRMSR' (32).";
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
      Log_Buffer.Put_Line (Str => "Writing 0xfa to MSR "
                           & SK.Strings.Img (Item => MSR_Address) & ".");
      Log_Buffer.New_Line;
      SK.CPU.Write_MSR64 (Register => MSR_Address,
                          Value    => 16#fa#);
      Result := ITS.Subject_State.Result_State;

      Log_Buffer.Put_Line (Str => ".:[ Assertions ]:.");
      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> Exit Reason");
      Log_Buffer.Put_Line
        (Str => "  Expected : "
         & SK.Strings.Img
           (Item => SK.Word32 (SK.Constants.EXIT_REASON_WRMSR)));
      Log_Buffer.Put_Line
        (Str => "  Result   : " & SK.Strings.Img (Item => Result.Exit_Reason));

      Success := Result.Exit_Reason = SK.Constants.EXIT_REASON_WRMSR;

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
   end Write_To_Disallowed_Register;

   -------------------------------------------------------------------------

   procedure Write_To_Read_Only_Register
   is
      use type Interfaces.Unsigned_32;

      MSR_Address     : constant Interfaces.Unsigned_32 := 16#0174#;
      Title           : constant String
        := "Write Access to read-only MSR";
      Description     : constant String
        := "This test verifies that an attempted write access to a read-only "
        & "Model-Specific Register is prohibited and results in a trap "
        & "indicating a WRMSR.";
      Expected_Result : constant String
        := "VM-Exit with reason 'WRMSR' (32).";
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
      Log_Buffer.Put_Line (Str => "Writing 0x0 to MSR "
                           & SK.Strings.Img (Item => MSR_Address) & ".");
      Log_Buffer.New_Line;
      SK.CPU.Write_MSR64 (Register => MSR_Address,
                          Value    => 0);
      Result := ITS.Subject_State.Result_State;

      Log_Buffer.Put_Line (Str => ".:[ Assertions ]:.");
      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> Exit Reason");
      Log_Buffer.Put_Line
        (Str => "  Expected : "
         & SK.Strings.Img
           (Item => SK.Word32 (SK.Constants.EXIT_REASON_WRMSR)));
      Log_Buffer.Put_Line
        (Str => "  Result   : " & SK.Strings.Img (Item => Result.Exit_Reason));

      Success := Result.Exit_Reason = SK.Constants.EXIT_REASON_WRMSR;

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
   end Write_To_Read_Only_Register;

end ITS.MSRs;
