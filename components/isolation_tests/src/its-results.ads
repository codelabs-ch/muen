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

with Musinfo.Instance;

with ITS.Log_Buffer;

package ITS.Results
is

   use type Interfaces.Unsigned_64;

   --  Append new test result with given parameters. The last reported result
   --  will be overwritten if the result storage is full.
   procedure Append
     (Title           : String;
      Description     : String;
      Expected        : String;
      Source_Info     : String;
      Success         : Boolean;
      Start_Timestamp : Interfaces.Unsigned_64;
      End_Timestamp   : Interfaces.Unsigned_64;
      Log_Entry       : Log_Buffer.Ext_Log_Entries_Range)
   with
      Pre =>
        Title'Length in Bounded_String'Range and
        Description'Length in Bounded_String'Range and
        Expected'Length in Bounded_String'Range and
        Source_Info'Length in Bounded_String'Range and
        End_Timestamp > Start_Timestamp;

   --  Output test results to log.
   procedure Report
   with
      Pre => Musinfo.Instance.Is_Valid;

end ITS.Results;
