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

with SK.Constants;
with SK.IO;
with SK.Strings;

with ITS.Log_Buffer;
with ITS.Results;
with ITS.Subject_State;

package body ITS.IO_Ports
is

   Testsuite_Name : constant String := Enclosing_Entity;

   -------------------------------------------------------------------------

   procedure Read_From_Disallowed_IO_Port
   is
      IO_Port         : constant Interfaces.Unsigned_16 := 16#ffff#;
      Title           : constant String
        := "Read Access to disallowed I/O Port";
      Description     : constant String
        := "This test verifies that an attempted read access to a disallowed "
        & "I/O port is prohibited and results in a trap indicating an "
        & "I/O instructon.";
      Expected_Result : constant String
        := "VM-Exit with reason 'I/O instruction' (30) and qualification "
        & "(0xffff0008) designating a byte-sized read from port "
        & SK.Strings.Img (Item => IO_Port)
        & " where the read access was attempted, see "
        & "Intel SDM Vol. 3C, '27.2.1 Basic VM-Exit Information', table 27-5.";
      Ref_Quali       : constant Interfaces.Unsigned_64 := 16#ffff_0008#;
      Log_ID          : Log_Buffer.Ext_Log_Entries_Range;
      Result          : SK.Subject_State_Type;
      Success         : Boolean;
      Start, Stop     : Interfaces.Unsigned_64;
      Unused_Value    : Interfaces.Unsigned_8;
      Src_Info        : constant String
        := Enclosing_Entity & ", " & Source_Location;
   begin
      Start := Musinfo.Instance.TSC_Schedule_Start;
      ITS.Subject_State.Result_State := SK.Null_Subject_State;

      Log_Buffer.Start_Entry (ID => Log_ID);
      Log_Buffer.Put_Line (Str => "Reading from I/O port "
                           & SK.Strings.Img (Item => IO_Port) & ".");
      Log_Buffer.New_Line;
      SK.IO.Inb (Port  => IO_Port,
                 Value => Unused_Value);
      Result := ITS.Subject_State.Result_State;

      Log_Buffer.Put_Line (Str => ".:[ Assertions ]:.");
      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> Exit Reason");
      Log_Buffer.Put_Line
        (Str => "  Expected : "
         & SK.Strings.Img
           (Item => SK.Word32 (SK.Constants.EXIT_REASON_IO_INSTRUCTION)));
      Log_Buffer.Put_Line
        (Str => "  Result   : " & SK.Strings.Img (Item => Result.Exit_Reason));
      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> Exit Qualification");
      Log_Buffer.Put_Line (Str => "  Expected : "
                           & SK.Strings.Img (Item => Ref_Quali));
      Log_Buffer.Put_Line
        (Str => "  Result   : "
         & SK.Strings.Img (Item => Result.Exit_Qualification));
      Success := Result.Exit_Reason = SK.Constants.EXIT_REASON_IO_INSTRUCTION
        and then Result.Exit_Qualification = Ref_Quali;

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
   end Read_From_Disallowed_IO_Port;

   -------------------------------------------------------------------------

   procedure Write_To_Disallowed_IO_Port
   is
      IO_Port         : constant Interfaces.Unsigned_16 := 16#64#;
      Title           : constant String
        := "Write Access to disallowed I/O Port";
      Description     : constant String
        := "This test verifies that an attempted write access to a disallowed "
        & "I/O port is prohibited and results in a trap indicating an "
        & "I/O instructon.";
      Expected_Result : constant String
        := "VM-Exit with reason 'I/O instruction' (30) and qualification "
        & "(0x640000) designating a byte-sized write to port "
        & SK.Strings.Img (Item => IO_Port)
        & " where the write access was attempted, see "
        & "Intel SDM Vol. 3C, '27.2.1 Basic VM-Exit Information', table 27-5.";
      Ref_Quali       : constant Interfaces.Unsigned_64 := 16#64_0000#;
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
      Log_Buffer.Put_Line (Str => "Writing 0xfa to I/O port "
                           & SK.Strings.Img (Item => IO_Port) & ".");
      Log_Buffer.New_Line;
      SK.IO.Outb (Port  => IO_Port,
                  Value => 16#fa#);
      Result := ITS.Subject_State.Result_State;

      Log_Buffer.Put_Line (Str => ".:[ Assertions ]:.");
      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> Exit Reason");
      Log_Buffer.Put_Line
        (Str => "  Expected : "
         & SK.Strings.Img
           (Item => SK.Word32 (SK.Constants.EXIT_REASON_IO_INSTRUCTION)));
      Log_Buffer.Put_Line
        (Str => "  Result   : " & SK.Strings.Img (Item => Result.Exit_Reason));
      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> Exit Qualification");
      Log_Buffer.Put_Line (Str => "  Expected : "
                           & SK.Strings.Img (Item => Ref_Quali));
      Log_Buffer.Put_Line
        (Str => "  Result   : "
         & SK.Strings.Img (Item => Result.Exit_Qualification));
      Success := Result.Exit_Reason = SK.Constants.EXIT_REASON_IO_INSTRUCTION
        and then Result.Exit_Qualification = Ref_Quali;

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
   end Write_To_Disallowed_IO_Port;

end ITS.IO_Ports;
