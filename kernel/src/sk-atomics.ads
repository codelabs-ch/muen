--
--  Copyright (C) 2022  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2022  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
--D This package provides atomic types and subprograms to operate on those
--D types. They are suitable for concurrent access, e.g. from different CPU
--D cores.
package SK.Atomics
is

   subtype Bit_Pos is Byte range 0 .. 63;

   --D @Interface
   --D 64-bit atomic type, which represents a bitmap with 64 elements.
   type Atomic64_Type is private;

   --  Initialize given atomic instance.
   procedure Init (Atomic : out Atomic64_Type);

   --  Atomically clear bit at specified position of given atomic bitmap.
   procedure Clear
     (Atomic : in out Atomic64_Type;
      Bit    :        Bit_Pos);

   --  Set bit at specified position of given atomic bitmap.
   procedure Set
     (Atomic : in out Atomic64_Type;
      Bit    :        Bit_Pos);

   --  Find highest bit set in given atomic bitmap. Found is set to False, if
   --  no bit is set.
   procedure Find_Highest_Bit_Set
     (Atomic :     Atomic64_Type;
      Found  : out Boolean;
      Bit    : out Bit_Pos);

private

   type Atomic64_Type is record
      --D @Interface
      --D 64-bits accessible atomically to enable concurrent access.
      Bits : Word64 with Atomic;
   end record
   with
      Atomic,
      Size      => 64,
      Alignment => 8;

end SK.Atomics;
