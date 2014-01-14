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

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Utility function tests");
      T.Add_Test_Routine
        (Routine => To_Hex'Access,
         Name    => "Unsigned_64 to Hex conversion");
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
