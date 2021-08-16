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

with SK.Crash_Audit_Types;

--D @Interface
--D Utility package providing helper function for printing subject state debug
--D information.
--D Note: implementation is only present in debug builds. In release versions
--D this package is empty.
package SK.Subjects.Debug
with
   SPARK_Mode => Off
is

   --  Print state information of given subject.
   procedure Print_State (S : Crash_Audit_Types.Subj_Context_Type);

end SK.Subjects.Debug;
