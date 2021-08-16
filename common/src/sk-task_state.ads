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

--D @Interface
--D This package implements types and subprograms to manage Task-State Segments.
package SK.Task_State
is

   --  Task-State Segment (TSS), see Intel SDM Vol. 3A, "7.7
   --  Task Management in 64-bit Mode".
   type TSS_Type is private;

   Null_TSS : constant TSS_Type;

   --  Interrupt stack table index.
   type IST_Index_Type is new Positive range 1 .. 7;

   --  Set interrupt stack table (IST) pointer with given index in TSS.
   procedure Set_IST_Entry
     (TSS_Data : in out TSS_Type;
      Index    :        IST_Index_Type;
      Address  :        SK.Word64)
   with
      Depends => (TSS_Data =>+ (Index, Address));

private

   type TSS_Entry_Type is record
      Low  : SK.Word32;
      High : SK.Word32;
   end record
     with
       Size => 64;

   for TSS_Entry_Type use record
      Low  at 0 range 0 .. 31;
      High at 4 range 0 .. 31;
   end record;

   Null_TSS_Entry : constant TSS_Entry_Type := TSS_Entry_Type'
     (Low  => 0,
      High => 0);

   --  RSP privilege levels present in TSS.
   type RSP_Privilege_Level is new Natural range 0 .. 2;

   type RSP_Array is array (RSP_Privilege_Level) of TSS_Entry_Type
     with Pack,
     Size => 3 * 8 * 8;

   type IST_Array is array (IST_Index_Type) of TSS_Entry_Type
     with Pack,
     Size => 7 * 8 * 8;

   type TSS_Type is record
      Reserved_1  : SK.Word32;
      RSPs        : RSP_Array;
      Reserved_2  : SK.Word32;
      Reserved_3  : SK.Word32;
      IST         : IST_Array;
      Reserved_4  : SK.Word32;
      Reserved_5  : SK.Word32;
      Reserved_6  : SK.Word16;
      IO_Map_Base : SK.Word16;
   end record
     with
       Size => 104 * 8;

   for TSS_Type use record
      Reserved_1  at   0 range 0 .. 31;
      RSPs        at   4 range 0 .. 191;
      Reserved_2  at  28 range 0 .. 31;
      Reserved_3  at  32 range 0 .. 31;
      IST         at  36 range 0 .. 447;
      Reserved_4  at  92 range 0 .. 31;
      Reserved_5  at  96 range 0 .. 31;
      Reserved_6  at 100 range 0 .. 15;
      IO_Map_Base at 102 range 0 .. 15;
   end record;

   Null_TSS : constant TSS_Type := TSS_Type'
     (Reserved_1  => 0,
      RSPs        => RSP_Array'(others => Null_TSS_Entry),
      Reserved_2  => 0,
      Reserved_3  => 0,
      IST         => IST_Array'(others => Null_TSS_Entry),
      Reserved_4  => 0,
      Reserved_5  => 0,
      Reserved_6  => 0,
      IO_Map_Base => 0);

end SK.Task_State;
