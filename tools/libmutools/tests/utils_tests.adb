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

with Mutools.Utils;

package body Utils_Tests
is

   use Ahven;
   use Mutools;

   -------------------------------------------------------------------------

   procedure Bit_Ops
   is
      use type Interfaces.Unsigned_64;

      Ref_Num_1 : constant Interfaces.Unsigned_64 := 16#800000#;
      Ref_Num_2 : constant Interfaces.Unsigned_64 := 16#A000800000#;
      Number    : Interfaces.Unsigned_64 := 0;
   begin
      for I in Utils.Unsigned_64_Pos'Range loop
         Assert (Condition => not Utils.Bit_Test (Value => Number,
                                              Pos   => I),
                 Message   => "Bit" & I'Img & " set");
      end loop;

      Number := Utils.Bit_Set (Value => Number,
                               Pos   => 23);
      Assert (Condition => Number = Ref_Num_1,
              Message   => "Number mismatch (1)");
      Assert (Condition => Utils.Bit_Test (Value => Number,
                                           Pos   => 23),
              Message   => "Bit 23 not set");

      Number := Utils.Bit_Set (Value => Number,
                               Pos   => 37);
      Number := Utils.Bit_Set (Value => Number,
                               Pos   => 39);
      Assert (Condition => Number = Ref_Num_2,
              Message   => "Number mismatch (2)");
   end Bit_Ops;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Utility function tests");
      T.Add_Test_Routine
        (Routine => To_Hex'Access,
         Name    => "Unsigned_64 to Hex conversion");
      T.Add_Test_Routine
        (Routine => Bit_Ops'Access,
         Name    => "Unsigned_64 bit set/test operations");
   end Initialize;

   -------------------------------------------------------------------------

   procedure To_Hex
   is
      Ref_First  : constant String                 := "16#0#";
      Ref_Last   : constant String                 := "16#FFFFFFFFFFFFFFFF#";
      Ref_Number : constant String                 := "DEADCAFEBEEFBEEF";
      Number     : constant Interfaces.Unsigned_64 := 16#deadcafebeefbeef#;
   begin
      Assert (Condition => Utils.To_Hex
              (Number => Interfaces.Unsigned_64'First) = Ref_First,
              Message   => "Unsigned_64'First hex string mismatch");
      Assert (Condition => Utils.To_Hex
              (Number => Interfaces.Unsigned_64'Last) = Ref_Last,
              Message   => "Unsigned_64'Last hex string mismatch");
      Assert (Condition => Utils.To_Hex
              (Number => Number,
               Prefix => False) = Ref_Number,
              Message   => "Hex string without prefix mismatch");
   end To_Hex;

end Utils_Tests;
