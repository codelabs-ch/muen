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

with Debuglog.Client;

package body ITS.Log_Buffer
is

   subtype Ext_Buffer_Index_Range is Natural range 0 .. Cspec.Log_Buffer_Size;

   subtype Buffer_Index_Range is Ext_Buffer_Index_Range range
     1 .. Ext_Buffer_Index_Range'Last;

   Null_Index : constant Ext_Buffer_Index_Range := 0;

   type Log_Entry is record
      Start_Idx : Buffer_Index_Range;
      End_Idx   : Ext_Buffer_Index_Range;
   end record;

   Null_Entry : constant Log_Entry
     := (Start_Idx => Buffer_Index_Range'First,
         End_Idx   => Null_Index);

   type Log_Entries_Array is array (Log_Entries_Range) of Log_Entry;

   --  Log buffer storage area.
   Buffer : String (Buffer_Index_Range);

   Log_Entries : Log_Entries_Array;

   --  Currently active log entry.
   Current_Entry : Ext_Log_Entries_Range;

   -------------------------------------------------------------------------

   procedure Clear
   is
   begin
      for C of Buffer loop
         C := ASCII.NUL;
      end loop;
      Log_Entries := (others => Null_Entry);
      Current_Entry := Ext_Log_Entries_Range'First;
   end Clear;

   -------------------------------------------------------------------------

   procedure New_Line
   is
   begin
      Put (Str => (1 => ASCII.LF));
   end New_Line;

   -------------------------------------------------------------------------

   procedure Print_Entry
     (ID     : Log_Entries_Range;
      Prefix : String := "")
   is
      Cur_Entry : constant Log_Entry := Log_Entries (ID);
   begin
      if Cur_Entry.End_Idx >= Cur_Entry.Start_Idx then
         Debuglog.Client.Put (Item => Prefix);
      end if;

      for I in Natural range Cur_Entry.Start_Idx .. Cur_Entry.End_Idx loop
         Debuglog.Client.Put (Item => Buffer (I));
         if Buffer (I) = ASCII.LF then
            Debuglog.Client.Put (Item => Prefix);
         end if;
      end loop;
   end Print_Entry;

   -------------------------------------------------------------------------

   procedure Put (Str : String)
   is
      Cur_End : constant Ext_Buffer_Index_Range
        := Log_Entries (Current_Entry).End_Idx;
      New_End : constant Natural
        := Cur_End + Str'Length;
   begin
      if New_End <= Buffer_Index_Range'Last then
         for I in Str'Range loop
            Buffer (Cur_End + I) := Str (I);
         end loop;
         Log_Entries (Current_Entry).End_Idx := New_End;
      end if;
   end Put;

   -------------------------------------------------------------------------

   procedure Put_Line (Str : String)
   is
   begin
      Put (Str => Str);
      New_Line;
   end Put_Line;

   -------------------------------------------------------------------------

   procedure Start_Entry (ID : out Ext_Log_Entries_Range)
   is
      Last_Entry : constant Ext_Log_Entries_Range := Current_Entry;
   begin
      ID := Null_Entry_Index;
      if Last_Entry < Log_Entries_Range'Last then
         Current_Entry := Last_Entry + 1;
         ID := Current_Entry;
         if Last_Entry in Log_Entries_Range then
            if Log_Entries (Last_Entry).End_Idx < Buffer_Index_Range'Last then
               Log_Entries (Current_Entry).Start_Idx
                 := Log_Entries (Last_Entry).End_Idx + 1;
               Log_Entries (Current_Entry).End_Idx
                 := Log_Entries (Current_Entry).Start_Idx - 1;
            else
               ID := Null_Entry_Index;
            end if;
         end if;
      end if;
   end Start_Entry;

   -------------------------------------------------------------------------

begin
   Clear;
end ITS.Log_Buffer;
