--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Skp;

use type Skp.Vector_Range;

package SK.Descriptors
is

   --  Pseudo Descriptor type, see Intel SDM Vol. 3A, chapter 3.5.1.
   type Pseudo_Descriptor_Type is record
      Limit : SK.Word16;
      Base  : SK.Word64;
   end record;

   --  Interrupt/Trap gate descriptor, see Intel SDM Vol. 3A, chapter 6.14.1.
   type Gate_Type is record
      Offset_15_00     : SK.Word16;
      Segment_Selector : SK.Word16;
      Flags            : SK.Word16;
      Offset_31_16     : SK.Word16;
      Offset_63_32     : SK.Word32;
      Reserved         : SK.Word32;
   end record;

   Null_Gate : constant Gate_Type;

   --  Range of descriptor table entries.
   type Descriptor_Table_Range is range 1 .. 256;

   --  The ISR array type stores addresses of Interrupt Service Routines.
   type ISR_Array is array (Skp.Vector_Range range <>) of SK.Word64;

   --  Interrupt descriptor table, see Intel SDM Vol. 3A, chapter 6.10.
   type IDT_Type is array (Skp.Vector_Range range <>) of Gate_Type;

   --  Setup IDT using the given ISR addresses.
   procedure Setup_IDT
     (ISRs :        ISR_Array;
      IDT  : in out IDT_Type)
   with
      Depends => (IDT =>+ ISRs),
      Pre     => ISRs'First = IDT'First and ISRs'Last = IDT'Last;

   --  Create pseudo-descriptor from given descriptor table address and length.
   function Create_Descriptor
     (Table_Address : SK.Word64;
      Table_Length  : Descriptor_Table_Range)
      return Pseudo_Descriptor_Type;

private

   for Pseudo_Descriptor_Type use record
      Limit at 0 range 0 .. 15;
      Base  at 2 range 0 .. 63;
   end record;
   for Pseudo_Descriptor_Type'Size use 80;

   for Gate_Type use record
      Offset_15_00     at  0 range 0 .. 15;
      Segment_Selector at  2 range 0 .. 15;
      Flags            at  4 range 0 .. 15;
      Offset_31_16     at  6 range 0 .. 15;
      Offset_63_32     at  8 range 0 .. 31;
      Reserved         at 12 range 0 .. 31;
   end record;
   for Gate_Type'Size use 16 * 8;

   Null_Gate : constant Gate_Type := Gate_Type'
     (Offset_15_00     => 0,
      Segment_Selector => 0,
      Flags            => 0,
      Offset_31_16     => 0,
      Offset_63_32     => 0,
      Reserved         => 0);

   for IDT_Type'Alignment use 8;

end SK.Descriptors;
