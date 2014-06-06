--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Msrstore.Tables
is

   -------------------------------------------------------------------------

   procedure Append_Entry
     (Store : in out MSR_Store_Type;
      Index :        Interfaces.Unsigned_32;
      Data  :        Interfaces.Unsigned_64)
   is
      Cur_Idx : constant MSR_Store_Size := MSR_Store_Size (Store.Next_Idx);
   begin
      Store.Data (Cur_Idx).Index := Index;
      Store.Data (Cur_Idx).Data  := Data;
      Store.Next_Idx             := Store.Next_Idx + 1;
   end Append_Entry;

end Msrstore.Tables;
