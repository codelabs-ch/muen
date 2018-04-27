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

with Interfaces;

generic

   type Index_Range is new IR_Entry_Range;

package VTd.Tables.IR
is

   --  Interrupt Remapping Table, see Intel VT-d specification, section
   --  5.3.2.1.
   type IR_Table_Type is private;

   --  Add an entry with given index and values to Interrupt Remapping Table.
   procedure Add_Entry
     (IRT    : in out IR_Table_Type;
      Index  :        Index_Range;
      Vector :        Interfaces.Unsigned_8;
      DST    :        Interfaces.Unsigned_32;
      SID    :        Interfaces.Unsigned_16;
      TM     :        Bit_Type);

   --  Serialize given Interrupt Remapping Table to file with specified
   --  filename.
   procedure Serialize
     (IRT      : IR_Table_Type;
      Filename : String);

private

   --  SVT (Source Validation Type) value of 01b: Verify requester-id in
   --  interrupt request using SID and SQ fields in the IRTE.
   --
   --  The SQ field is 00b: Verify the interrupt request by comparing all
   --  16-bits of SID field with the 16-bit requester-id of the interrupt
   --  request.
   SID_SQ_Verification : constant Bit_Array (1 .. 2) := (1 => 1, 2 => 0);

   --  Interrupt Remapping Table Entry (IRTE), see Intel VT-d specification,
   --  section 9.10.
   type IR_Entry_Type is record
      Present    : Bit_Type               := 0;
      FPD        : Bit_Type               := 0;
      DM         : Bit_Type               := 0;
      RH         : Bit_Type               := 0;
      TM         : Bit_Type               := 0;
      DLM        : Bit_Array (1 .. 3)     := (others => 0);
      AVAIL      : Bit_Array (1 .. 4)     := (others => 0);
      Reserved_1 : Bit_Array (1 .. 4)     := (others => 0);
      V          : Interfaces.Unsigned_8  := 0;
      Reserved_2 : Interfaces.Unsigned_8  := 0;
      DST        : Interfaces.Unsigned_32 := 0;
      SID        : Interfaces.Unsigned_16 := 0;
      SQ         : Bit_Array (1 .. 2)     := (others => 0);
      SVT        : Bit_Array (1 .. 2)     := (others => 0);
      Reserved_3 : Bit_Array (1 .. 44)    := (others => 0);
   end record
     with Size => 128;

   for IR_Entry_Type use record
      Present    at 0 range  0 .. 0;
      FPD        at 0 range  1 .. 1;
      DM         at 0 range  2 .. 2;
      RH         at 0 range  3 .. 3;
      TM         at 0 range  4 .. 4;
      DLM        at 0 range  5 .. 7;
      AVAIL      at 0 range  8 .. 11;
      Reserved_1 at 0 range 12 .. 15;
      V          at 0 range 16 .. 23;
      Reserved_2 at 0 range 24 .. 31;
      DST        at 0 range 32 .. 63;
      SID        at 0 range 64 .. 79;
      SQ         at 0 range 80 .. 81;
      SVT        at 0 range 82 .. 83;
      Reserved_3 at 0 range 84 .. 127;
   end record;

   type IR_Entry_Array is array (Index_Range) of IR_Entry_Type
     with
       Pack;

   type IR_Table_Type is record
      Entries : IR_Entry_Array;
   end record
     with
       Pack;

end VTd.Tables.IR;
