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

with ITS.Log_Buffer;

package ITS.Results
is

   --  Append new test result with given parameters. The last reported result
   --  will be overwritten if the result storage is full.
   procedure Append
     (Title       : String;
      Description : String;
      Expected    : String;
      Success     : Boolean;
      Log_Entry   : Log_Buffer.Ext_Log_Entries_Range)
   with
      Pre =>
        Title'Length in Bounded_String'Range and
        Description'Length in Bounded_String'Range and
        Expected'Length in Bounded_String'Range;

   --  Output test results to log.
   procedure Report;

end ITS.Results;
