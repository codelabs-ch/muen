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

with Muxml;
with Mutools.XML_Utils;

with Pack.Utils;

package body Util_Tests
is

   use Ahven;
   use Pack;

   -------------------------------------------------------------------------

   procedure Image_Size
   is
      use type Interfaces.Unsigned_64;

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => "linux|bin",
         Address     => "16#0011_4000#",
         Size        => "16#0001_3000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "subject_binary",
         File_Name   => "obj1.o",
         File_Format => "bin_raw",
         File_Offset => "none");

      Assert (Condition => Utils.Get_Image_Size (Policy => Data) = 16#127000#,
              Message   => "Image size mismatch");
   end Image_Size;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Util tests");
      T.Add_Test_Routine
        (Routine => Image_Size'Access,
         Name    => "Calculate image size");
   end Initialize;

end Util_Tests;
