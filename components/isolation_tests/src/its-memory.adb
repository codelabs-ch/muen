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
with SK.Strings;

with ITS.Log_Buffer;
with ITS.Results;
with ITS.Subject_State;
with ITS.Utils;

with Isolation_Tests_Component.Memory;

package body ITS.Memory
is

   package Cspecs renames Isolation_Tests_Component.Memory;

   -------------------------------------------------------------------------

   procedure Write_To_Read_Only_Region
   is
      use type Interfaces.Unsigned_32;
      use type Interfaces.Unsigned_64;

      Memory_Address  : constant Interfaces.Unsigned_64
        := Cspecs.Read_Only_Address;
      Title           : constant String
        := "Write Access to read-only Memory";
      Description     : constant String
        := "This test verifies that an attempted write access to a read-only "
        & "memory region is prohibited and results in a trap indicating an "
        & "exception.";
      Expected_Result : constant String
        := "VM-Exit with reason 'Exception or non-maskable interrupt (NMI)' "
        & "(0) and qualification containing the memory address "
        & SK.Strings.Img (Item => Memory_Address)
        & " where the write access was attempted.";
      Log_ID          : Log_Buffer.Ext_Log_Entries_Range;
      Result          : SK.Subject_State_Type;
      Success         : Boolean;
   begin
      ITS.Subject_State.Result_State := SK.Null_Subject_State;

      Log_Buffer.Start_Entry (ID => Log_ID);
      Log_Buffer.Put_Line (Str => "Writing 0x0 to memory address "
                           & SK.Strings.Img (Item => Memory_Address) & ".");
      Log_Buffer.New_Line;
      ITS.Utils.Write_Byte (Address => Memory_Address,
                            Value   => 0);
      Result := ITS.Subject_State.Result_State;

      Log_Buffer.Put_Line (Str => ".:[ Assertions ]:.");
      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> Exit Reason");
      Log_Buffer.Put_Line
        (Str => "  Expected : "
         & SK.Strings.Img
           (Item => SK.Word32 (SK.Constants.EXIT_REASON_EXCEPTION_NMI)));
      Log_Buffer.Put_Line
        (Str => "  Result   : " & SK.Strings.Img (Item => Result.Exit_Reason));
      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> Exit Qualification");
      Log_Buffer.Put_Line (Str => "  Expected : "
                           & SK.Strings.Img (Item => Memory_Address));
      Log_Buffer.Put_Line
        (Str => "  Result   : "
         & SK.Strings.Img (Item => Result.Exit_Qualification));
      Success := Result.Exit_Reason = SK.Constants.EXIT_REASON_EXCEPTION_NMI
        and then Result.Exit_Qualification = Memory_Address;

      Results.Append (Title       => Title,
                      Description => Description,
                      Expected    => Expected_Result,
                      Success     => Success,
                      Log_Entry   => Log_ID);
   end Write_To_Read_Only_Region;

   -------------------------------------------------------------------------

   procedure Write_To_Unmapped_Region
   is
      use type Interfaces.Unsigned_32;
      use type Interfaces.Unsigned_64;

      Memory_Address  : constant Interfaces.Unsigned_64 := 0;
      Title           : constant String
        := "Write Access to unmapped Memory";
      Description     : constant String
        := "This test verifies that an attempted write access to an unmapped "
        & "memory region is prohibited and results in a trap indicating an "
        & "exception.";
      Expected_Result : constant String
        := "VM-Exit with reason 'Exception or non-maskable interrupt (NMI)' "
        & "(0) and qualification containing the memory address "
        & SK.Strings.Img (Item => Memory_Address)
        & " where the write access was attempted.";
      Log_ID          : Log_Buffer.Ext_Log_Entries_Range;
      Result          : SK.Subject_State_Type;
      Success         : Boolean;
   begin
      ITS.Subject_State.Result_State := SK.Null_Subject_State;

      Log_Buffer.Start_Entry (ID => Log_ID);
      Log_Buffer.Put_Line (Str => "Writing 0xfa to memory address "
                           & SK.Strings.Img (Item => Memory_Address) & ".");
      ITS.Utils.Write_Byte (Address => Memory_Address,
                            Value   => 16#fa#);
      Log_Buffer.New_Line;
      Result := ITS.Subject_State.Result_State;

      Log_Buffer.Put_Line (Str => ".:[ Assertions ]:.");
      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> Exit Reason");
      Log_Buffer.Put_Line
        (Str => "  Expected : "
         & SK.Strings.Img
           (Item => SK.Word32 (SK.Constants.EXIT_REASON_EXCEPTION_NMI)));
      Log_Buffer.Put_Line (Str => "  Result   : "
                           & SK.Strings.Img (Item => Result.Exit_Reason));
      Log_Buffer.New_Line;
      Log_Buffer.Put_Line (Str => "> Exit Qualification");
      Log_Buffer.Put_Line
        (Str => "  Expected : " & SK.Strings.Img (Item => Memory_Address));
      Log_Buffer.Put_Line
        (Str => "  Result   : "
         & SK.Strings.Img (Item => Result.Exit_Qualification));
      Success := Result.Exit_Reason = SK.Constants.EXIT_REASON_EXCEPTION_NMI
        and then Result.Exit_Qualification = Memory_Address;

      Results.Append (Title       => Title,
                      Description => Description,
                      Expected    => Expected_Result,
                      Success     => Success,
                      Log_Entry   => Log_ID);
   end Write_To_Unmapped_Region;

end ITS.Memory;
