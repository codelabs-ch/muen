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

package body SK.Subjects_MSR_Store
with
   Refined_State => (State => MSR_Storage)
is

   -------------------------------------------------------------------------

   procedure Clear_MSRs (Subject : Skp.Global_Subject_ID_Type)
   with
      Refined_Global  => (In_Out => MSR_Storage),
      Refined_Depends => (MSR_Storage =>+ Subject)
   is
   begin
      for I in MSR_Entry_Range loop
         MSR_Storage (Subject).MSRs (I).Data := 0;
      end loop;
      MSR_Storage (Subject).Padding := (others => 0);
   end Clear_MSRs;

end SK.Subjects_MSR_Store;
