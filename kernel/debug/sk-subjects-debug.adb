--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.KC;
with SK.Locks;
with SK.Dumper;

package body SK.Subjects.Debug
with
   SPARK_Mode => Off
is

   package D is new Dumper
     (New_Line   => KC.New_Line,
      Put_Line   => KC.Put_Line,
      Put_String => KC.Put_String);

   -------------------------------------------------------------------------

   procedure Print_State (S : Crash_Audit_Types.Subj_Context_Type)
   is
   begin
      Locks.Acquire;
      D.Output_Subj_State (Context => S);
      Locks.Release;
   end Print_State;

end SK.Subjects.Debug;
