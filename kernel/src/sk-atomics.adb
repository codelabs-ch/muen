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

with System.Machine_Code;

with SK.Bitops;

package body SK.Atomics
is

   -------------------------------------------------------------------------

   function Bit_Test
     (Atomic : Atomic64_Type;
      Bit    : Bit_Pos)
     return Boolean
   is
      Bits : constant Word64 := Atomic.Bits;
   begin
      return SK.Bitops.Bit_Test (Value => Bits,
                                 Pos   => SK.Bitops.Word64_Pos (Bit));
   end Bit_Test;

   -------------------------------------------------------------------------

   procedure Clear
     (Atomic : in out Atomic64_Type;
      Bit    :        Bit_Pos)
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "lock btr %0, (%1)",
         Inputs   => (Word32'Asm_Input ("r", Word32 (Bit)),
                      System.Address'Asm_Input
                        ("r", Atomic'Address)),
         Clobber  => "memory",
         Volatile => True);
   end Clear;

   -------------------------------------------------------------------------

   procedure Find_Highest_Bit_Set
     (Atomic :     Atomic64_Type;
      Found  : out Boolean;
      Bit    : out Bit_Pos)
   is
      subtype Bit_Search_Range is Bitops.Word64_Pos
      range 0 .. Word64 (Bit_Pos'Last);
      procedure Find_Highest_Bit_Set is new Bitops.Find_Highest_Bit_Set
        (Search_Range => Bit_Search_Range);

      Bits : Word64;
      Pos  : Bit_Search_Range;
   begin
      Bits := Atomic.Bits;
      Find_Highest_Bit_Set
        (Field => Bits,
         Found => Found,
         Pos   => Pos);
      Bit := Bit_Pos (Pos);
   end Find_Highest_Bit_Set;

   -------------------------------------------------------------------------

   function Has_Bit_Set (Atomic : Atomic64_Type) return Boolean
   is
      Bits : constant Word64 := Atomic.Bits;
   begin
      return Bits /= 0;
   end Has_Bit_Set;

   -------------------------------------------------------------------------

   procedure Init (Atomic : out Atomic64_Type)
   is
   begin
      Atomic := (Bits => 0);
   end Init;

   -------------------------------------------------------------------------

   procedure Set
     (Atomic : in out Atomic64_Type;
      Bit    :        Bit_Pos)
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "lock bts %0, (%1)",
         Inputs   => (Word32'Asm_Input ("r", Word32 (Bit)),
                      System.Address'Asm_Input
                        ("r", Atomic'Address)),
         Clobber  => "memory",
         Volatile => True);
   end Set;

end SK.Atomics;
