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

with Musinfo.Writer;

with Test_Utils;

package body Writer_Tests
is

   use Ahven;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Writer tests");
      T.Add_Test_Routine
        (Routine => Serialize'Access,
         Name    => "Subject info serialization");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Serialize
   is
   begin
      Musinfo.Writer.Serialize
        (Info     => Musinfo.Null_Subject_Info,
         Filename => "obj/null_info");

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/null_info",
               Filename2 => "obj/null_info"),
              Message   => "Null info mismatch");
   end Serialize;

end Writer_Tests;
