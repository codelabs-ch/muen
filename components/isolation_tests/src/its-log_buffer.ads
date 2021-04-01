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

with Isolation_Tests_Component.Config;

package ITS.Log_Buffer
is

   package Cspec renames Isolation_Tests_Component.Config;

   type Ext_Log_Entries_Range is range 0 .. Cspec.Log_Entry_Max;

   subtype Log_Entries_Range is Ext_Log_Entries_Range range
     1 .. Ext_Log_Entries_Range'Last;

   Null_Entry_Index : constant Ext_Log_Entries_Range
     := Ext_Log_Entries_Range'First;

   --  Initialize log buffer.
   procedure Clear;

   --  Start new log entry with given ID. The preceding log entry will be
   --  closed. Null_Index is returned if the buffer is full.
   procedure Start_Entry (ID : out Ext_Log_Entries_Range);

   --  Append string to current log entry.
   procedure Put (Str : String);
   procedure Put_Line (Str : String);
   procedure New_Line;

   --  Print entry specified by ID to debuglog.
   procedure Print_Entry (ID : Log_Entries_Range);

end ITS.Log_Buffer;
