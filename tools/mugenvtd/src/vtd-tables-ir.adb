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

with Ada.Unchecked_Conversion;

package body VTd.Tables.IR
is

   -------------------------------------------------------------------------

   procedure Add_Entry
     (IRT    : in out IR_Table_Type;
      Index  :        Index_Range;
      Vector :        Interfaces.Unsigned_8;
      DST    :        Interfaces.Unsigned_32;
      SID    :        Interfaces.Unsigned_16;
      TM     :        Bit_Type)
   is
      E : constant IR_Entry_Type
        := (Present => 1,
            V       => Vector,
            DST     => DST,
            SID     => SID,
            TM      => TM,
            SVT     => SID_SQ_Verification,
            others  => <>);
   begin
      IRT.Entries (Index) := E;
   end Add_Entry;

   -------------------------------------------------------------------------

   procedure Serialize
     (IRT      : IR_Table_Type;
      Filename : String)
   is
      use type Ada.Streams.Stream_Element_Offset;

      subtype Table_Stream is Ada.Streams.Stream_Element_Array
        (1 .. IRT'Size / 8);

      --  IR_Table_Type'Write adds additional output so manual conversion to
      --  stream array is necessary.
      function Convert is new Ada.Unchecked_Conversion
        (Source => IR_Table_Type,
         Target => Table_Stream);
   begin
      Write (Stream   => Convert (S => IRT),
             Filename => Filename);
   end Serialize;

end VTd.Tables.IR;
