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

package body VTd.Tables.IR
is

   -------------------------------------------------------------------------

   procedure Add_Entry
     (IRT    : in out IR_Table_Type;
      Index  :        Index_Range;
      Vector :        Interfaces.Unsigned_8;
      DST    :        Interfaces.Unsigned_32;
      SID    :        Interfaces.Unsigned_16)
   is
      E : constant IR_Entry_Type
        := (Present => 1,
            V       => Vector,
            DST     => DST,
            SID     => SID,
            others  => <>);
   begin
      IRT.Entries (Index) := E;
   end Add_Entry;

end VTd.Tables.IR;
