--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--  Copyright (C) 2014  Alexander Senier <mail@senier.net>
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
      Ref_Num_3 : constant Interfaces.Unsigned_64 := 16#A000000000#;
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

      Number := Utils.Bit_Clear (Value => Number,
                                 Pos   => 23);
      Assert (Condition => Number = Ref_Num_3,
              Message   => "Number mismatch (3)");
   end Bit_Ops;

   -------------------------------------------------------------------------

   procedure Decode_Entity_Name
   is
      Str : constant String := "linux|zp";
   begin
      Assert (Condition => Utils.Decode_Entity_Name
              (Encoded_Str => Str) = "linux",
              Message   => "Entity name mismatch");
   end Decode_Entity_Name;

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
         Name    => "Unsigned_64 bit operations");
      T.Add_Test_Routine
        (Routine => Decode_Entity_Name'Access,
         Name    => "Decode entity name");
   end Initialize;

   -------------------------------------------------------------------------

   procedure To_Hex
   is
      Ref_First  : constant String := "16#0#";
      Ref_Last   : constant String := "16#ffffffffffffffff#";
      Ref_Number : constant String := "deadcafebeefbeef";
      Norm_First : constant String := "16#0000#";
      Norm_Last  : constant String := "16#ffff_ffff_ffff_ffff#";
      Norm_Num   : constant String := "16#dead_cafe_beef_beef#";
      Norm_Num2  : constant String := "16#00de_adbe_efbe#";
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
      Assert (Condition => Utils.To_Hex
              (Number    => Interfaces.Unsigned_64'First,
               Prefix    => True,
               Normalize => True) = Norm_First,
              Message => "Normalized Unsigned_64'First hex string mismatch");
      Assert (Condition => Utils.To_Hex
              (Number    => Interfaces.Unsigned_64'Last,
               Prefix    => True,
               Normalize => True) = Norm_Last,
              Message => "Normalized Unsigned_64'Last hex string mismatch");
      Assert (Condition => Utils.To_Hex
              (Number    => Number,
               Prefix    => True,
               Normalize => True) = Norm_Num,
              Message => "Normalized " & Norm_Num & " hex string mismatch");
      Assert (Condition => Utils.To_Hex
              (Number    => 16#de_adbe_efbe#,
               Prefix    => True,
               Normalize => True) = Norm_Num2,
              Message => "Normalized " & Norm_Num2 & " hex string mismatch");
   end To_Hex;

end Utils_Tests;
